/*
** Object de/serialization.
** Copyright (C) 2005-2021 Mike Pall. See Copyright Notice in luajit.h
*/

#define lj_serialize_c
#define LUA_CORE

#include "lj_obj.h"

#if LJ_HASBUFFER
#include "lj_err.h"
#include "lj_buf.h"
#include "lj_str.h"
#include "lj_tab.h"
#include "lj_udata.h"
#include "lj_ctype.h"
#include "lj_cdata.h"
#include "lj_ir.h"
#include "lj_serialize.h"

/* Tags for internal serialization format. */
enum {
  SER_TAG_NIL,		/* 0x00 */
  SER_TAG_FALSE,
  SER_TAG_TRUE,
  SER_TAG_NULL,
  SER_TAG_LIGHTUD32,
  SER_TAG_LIGHTUD64,
  SER_TAG_INT,
  SER_TAG_NUM,
  SER_TAG_TAB,		/* 0x08 */
  SER_TAG_0x0e = SER_TAG_TAB+6,
  SER_TAG_DICT,
  SER_TAG_INT64,	/* 0x10 */
  SER_TAG_UINT64,
  SER_TAG_COMPLEX,
  SER_TAG_0x13,
  SER_TAG_0x14,
  SER_TAG_0x15,
  SER_TAG_0x16,
  SER_TAG_0x17,
  SER_TAG_0x18,		/* 0x18 */
  SER_TAG_0x19,
  SER_TAG_0x1a,
  SER_TAG_0x1b,
  SER_TAG_0x1c,
  SER_TAG_0x1d,
  SER_TAG_0x1e,
  SER_TAG_0x1f,
  SER_TAG_STR,		/* 0x20 + str->len */
};
LJ_STATIC_ASSERT((SER_TAG_TAB & 7) == 0);

/* -- Helper functions ---------------------------------------------------- */

static LJ_AINLINE char *serialize_more(char *w, SBufExt *sbx, MSize sz)
{
  if (LJ_UNLIKELY(sz > (MSize)(sbx->e - w))) {
    sbx->w = w;
    w = lj_buf_more2((SBuf *)sbx, sz);
  }
  return w;
}

/* Write U124 to buffer. */
static LJ_NOINLINE char *serialize_wu124_(char *w, uint32_t v)
{
  if (v < 0x1fe0) {
    v -= 0xe0;
    *w++ = (char)(0xe0 | (v >> 8)); *w++ = (char)v;
  } else {
    *w++ = (char)0xff;
    memcpy(w, &v, 4); w += 4;
  }
  return w;
}

static LJ_AINLINE char *serialize_wu124(char *w, uint32_t v)
{
  if (LJ_LIKELY(v < 0xe0)) {
    *w++ = (char)v;
    return w;
  } else {
    return serialize_wu124_(w, v);
  }
}

static LJ_NOINLINE char *serialize_ru124_(char *r, char *w, uint32_t *pv)
{
  uint32_t v = *pv;
  if (v != 0xff) {
    if (r >= w) return NULL;
    v = ((v & 0x1f) << 8) + *(uint8_t *)r + 0xe0; r++;
  } else {
    if (r + 4 > w) return NULL;
    v = lj_getu32(r); r += 4;
  }
  *pv = v;
  return r;
}

static LJ_AINLINE char *serialize_ru124(char *r, char *w, uint32_t *pv)
{
  if (LJ_LIKELY(r < w)) {
    uint32_t v = *(uint8_t *)r; r++;
    *pv = v;
    if (LJ_UNLIKELY(v >= 0xe0)) {
      r = serialize_ru124_(r, w, pv);
    }
    return r;
  }
  return NULL;
}

/* Prepare string dictionary for use (once). */
void lj_serialize_dict_prep(lua_State *L, GCtab *dict)
{
  if (!dict->hmask) {  /* No hash part means not prepared, yet. */
    MSize i, len = lj_tab_len(dict);
    if (!len) return;
    lj_tab_resize(L, dict, dict->asize, hsize2hbits(len));
    for (i = 1; i <= len && i < dict->asize; i++) {
      cTValue *o = arrayslot(dict, i);
      if (tvisstr(o)) {
	if (!lj_tab_getstr(dict, strV(o))) {  /* Ignore dups. */
	  lj_tab_newkey(L, dict, o)->u64 = (uint64_t)(i-1);
	}
      } else if (!tvisfalse(o)) {
	lj_err_caller(L, LJ_ERR_BUFFER_BADOPT);
      }
    }
  }
}

/* -- Internal serializer ------------------------------------------------- */

/* Put serialized object into buffer. */
static char *serialize_put(char *w, SBufExt *sbx, cTValue *o)
{
  if (LJ_LIKELY(tvisstr(o))) {
    const GCstr *str = strV(o);
    MSize len = str->len;
    w = serialize_more(w, sbx, 5+len);
    w = serialize_wu124(w, SER_TAG_STR + len);
    w = lj_buf_wmem(w, strdata(str), len);
  } else if (tvisnum(o)) {
    uint64_t x = o->u64;
    w = serialize_more(w, sbx, 1+sizeof(lua_Number));
    *w++ = SER_TAG_NUM; memcpy(w, &x, 8); w += 8;
  } else if (tvispri(o)) {
    w = serialize_more(w, sbx, 1);
    *w++ = (char)(SER_TAG_NIL + ~itype(o));
  } else if (tvistab(o)) {
    const GCtab *t = tabV(o);
    uint32_t narray = 0, nhash = 0, one = 2;
    if (sbx->depth <= 0) lj_err_caller(sbufL(sbx), LJ_ERR_BUFFER_DEPTH);
    sbx->depth--;
    if (t->asize > 0) {  /* Determine max. length of array part. */
      ptrdiff_t i;
      TValue *array = tvref(t->array);
      for (i = (ptrdiff_t)t->asize-1; i >= 0; i--)
	if (!tvisnil(&array[i]))
	  break;
      narray = (uint32_t)(i+1);
      if (narray && tvisnil(&array[0])) one = 4;
    }
    if (t->hmask > 0) {  /* Count number of used hash slots. */
      uint32_t i, hmask = t->hmask;
      Node *node = noderef(t->node);
      for (i = 0; i <= hmask; i++)
	nhash += !tvisnil(&node[i].val);
    }
    /* Write number of array slots and hash slots. */
    w = serialize_more(w, sbx, 1+2*5);
    *w++ = (char)(SER_TAG_TAB + (nhash ? 1 : 0) + (narray ? one : 0));
    if (narray) w = serialize_wu124(w, narray);
    if (nhash) w = serialize_wu124(w, nhash);
    if (narray) {  /* Write array entries. */
      cTValue *oa = tvref(t->array) + (one >> 2);
      cTValue *oe = tvref(t->array) + narray;
      while (oa < oe) w = serialize_put(w, sbx, oa++);
    }
    if (nhash) {  /* Write hash entries. */
      const Node *node = noderef(t->node) + t->hmask;
      GCtab *dict = tabref(sbx->dict);
      if (LJ_UNLIKELY(dict)) {
	for (;; node--)
	  if (!tvisnil(&node->val)) {
	    if (LJ_LIKELY(tvisstr(&node->key))) {
	      /* Inlined lj_tab_getstr is 30% faster. */
	      const GCstr *str = strV(&node->key);
	      Node *n = hashstr(dict, str);
	      do {
		if (tvisstr(&n->key) && strV(&n->key) == str) {
		  uint32_t idx = n->val.u32.lo;
		  w = serialize_more(w, sbx, 1+5);
		  *w++ = SER_TAG_DICT;
		  w = serialize_wu124(w, idx);
		  break;
		}
		n = nextnode(n);
		if (!n) {
		  MSize len = str->len;
		  w = serialize_more(w, sbx, 5+len);
		  w = serialize_wu124(w, SER_TAG_STR + len);
		  w = lj_buf_wmem(w, strdata(str), len);
		  break;
		}
	      } while (1);
	    } else {
	      w = serialize_put(w, sbx, &node->key);
	    }
	    w = serialize_put(w, sbx, &node->val);
	    if (--nhash == 0) break;
	  }
      } else {
	for (;; node--)
	  if (!tvisnil(&node->val)) {
	    w = serialize_put(w, sbx, &node->key);
	    w = serialize_put(w, sbx, &node->val);
	    if (--nhash == 0) break;
	  }
      }
    }
    sbx->depth++;
  } else if (tviscdata(o)) {
    CTState *cts = ctype_cts(sbufL(sbx));
    CType *s = ctype_raw(cts, cdataV(o)->ctypeid);
    uint8_t *sp = cdataptr(cdataV(o));
    if (ctype_isinteger(s->info) && s->size == 8) {
      w = serialize_more(w, sbx, 1+8);
      *w++ = (s->info & CTF_UNSIGNED) ? SER_TAG_UINT64 : SER_TAG_INT64;
      memcpy(w, sp, 8);
      w += 8;
    } else if (ctype_iscomplex(s->info) && s->size == 16) {
      w = serialize_more(w, sbx, 1+16);
      *w++ = SER_TAG_COMPLEX;
      memcpy(w, sp, 16);
      w += 16;
    } else {
      goto badenc;  /* NYI other cdata */
    }
  } else if (tvislightud(o)) {
    uintptr_t ud = (uintptr_t)lightudV(G(sbufL(sbx)), o);
    w = serialize_more(w, sbx, 1+sizeof(ud));
    if (ud == 0) {
      *w++ = SER_TAG_NULL;
    } else if (checku32(ud)) {
      *w++ = SER_TAG_LIGHTUD32; memcpy(w, &ud, 4); w += 4;
    } else {
      *w++ = SER_TAG_LIGHTUD64; memcpy(w, &ud, 8); w += 8;
    }
  } else {
    /* NYI userdata */
  badenc:
    lj_err_callerv(sbufL(sbx), LJ_ERR_BUFFER_BADENC, lj_typename(o));
  }
  return w;
}

/* Get serialized object from buffer. */
static char *serialize_get(char *r, SBufExt *sbx, TValue *o)
{
  char *w = sbx->w;
  uint32_t tp;
  r = serialize_ru124(r, w, &tp); if (LJ_UNLIKELY(!r)) goto eob;
  if (LJ_LIKELY(tp >= SER_TAG_STR)) {
    uint32_t len = tp - SER_TAG_STR;
    if (LJ_UNLIKELY(len > (uint32_t)(w - r))) goto eob;
    setstrV(sbufL(sbx), o, lj_str_new(sbufL(sbx), r, len));
    r += len;
  } else if (tp == SER_TAG_INT) {
    if (LJ_UNLIKELY(r + 4 > w)) goto eob;
    setintV(o, (int32_t)(lj_getu32(r)));
    r += 4;
  } else if (tp == SER_TAG_NUM) {
    if (LJ_UNLIKELY(r + 8 > w)) goto eob;
    memcpy(o, r, 8); r += 8;
    if (!tvisnum(o)) setnanV(o);
  } else if (tp <= SER_TAG_TRUE) {
    setpriV(o, ~tp);
  } else if (tp == SER_TAG_DICT) {
    GCtab *dict;
    uint32_t idx;
    r = serialize_ru124(r, w, &idx);
    idx++;
    dict = tabref(sbx->dict);
    if (dict && idx < dict->asize && tvisstr(arrayslot(dict, idx)))
      copyTV(sbufL(sbx), o, arrayslot(dict, idx));
    else
      lj_err_callerv(sbufL(sbx), LJ_ERR_BUFFER_BADDICTX, idx);
  } else if (tp >= SER_TAG_TAB && tp < SER_TAG_TAB+6) {
    uint32_t narray = 0, nhash = 0;
    GCtab *t;
    if (tp >= SER_TAG_TAB+2) {
      r = serialize_ru124(r, w, &narray); if (LJ_UNLIKELY(!r)) goto eob;
    }
    if ((tp & 1)) {
      r = serialize_ru124(r, w, &nhash); if (LJ_UNLIKELY(!r)) goto eob;
    }
    t = lj_tab_new(sbufL(sbx), narray, hsize2hbits(nhash));
    settabV(sbufL(sbx), o, t);
    if (narray) {
      TValue *oa = tvref(t->array) + (tp >= SER_TAG_TAB+4);
      TValue *oe = tvref(t->array) + narray;
      while (oa < oe) r = serialize_get(r, sbx, oa++);
    }
    if (nhash) {
      do {
	TValue k, *v;
	r = serialize_get(r, sbx, &k);
	v = lj_tab_set(sbufL(sbx), t, &k);
	if (LJ_UNLIKELY(!tvisnil(v)))
	  lj_err_caller(sbufL(sbx), LJ_ERR_BUFFER_DUPKEY);
	r = serialize_get(r, sbx, v);
      } while (--nhash);
    }
  } else if (tp >= SER_TAG_INT64 &&  tp <= SER_TAG_COMPLEX) {
    uint32_t sz = tp == SER_TAG_COMPLEX ? 16 : 8;
    GCcdata *cd;
    if (LJ_UNLIKELY(r + sz > w)) goto eob;
    cd = lj_cdata_new_(sbufL(sbx),
	   tp == SER_TAG_INT64 ? CTID_INT64 :
	   tp == SER_TAG_UINT64 ? CTID_UINT64 : CTID_COMPLEX_DOUBLE,
	   sz);
    memcpy(cdataptr(cd), r, sz); r += sz;
    setcdataV(sbufL(sbx), o, cd);
  } else if (tp <= SER_TAG_LIGHTUD64) {
    uintptr_t ud = 0;
    if (tp == SER_TAG_LIGHTUD32) {
      if (LJ_UNLIKELY(r + 4 > w)) goto eob;
      ud = (uintptr_t)(lj_getu32(r));
      r += 4;
    }
    else if (tp == SER_TAG_LIGHTUD64) {
      if (LJ_UNLIKELY(r + 8 > w)) goto eob;
      memcpy(&ud, r, 8); r += 8;
    }
    setrawlightudV(o, lj_lightud_intern(sbufL(sbx), (void *)ud));
  } else {
    lj_err_callerv(sbufL(sbx), LJ_ERR_BUFFER_BADDEC, tp);
  }
  return r;
eob:
  lj_err_caller(sbufL(sbx), LJ_ERR_BUFFER_EOB);
  return NULL;
}

/* -- External serialization API ------------------------------------------ */

/* Encode to buffer. */
SBufExt * lj_serialize_put(SBufExt *sbx, cTValue *o)
{
  sbx->depth = LJ_SERIALIZE_DEPTH;
  sbx->w = serialize_put(sbx->w, sbx, o);
  return sbx;
}

/* Decode from buffer. */
char * lj_serialize_get(SBufExt *sbx, TValue *o)
{
  return serialize_get(sbx->r, sbx, o);
}

/* Stand-alone encoding, borrowing from global temporary buffer. */
GCstr * lj_serialize_encode(lua_State *L, cTValue *o)
{
  SBufExt sbx;
  char *w;
  memset(&sbx, 0, sizeof(SBufExt));
  lj_bufx_set_borrow(L, &sbx, &G(L)->tmpbuf);
  sbx.depth = LJ_SERIALIZE_DEPTH;
  w = serialize_put(sbx.w, &sbx, o);
  return lj_str_new(L, sbx.b, (size_t)(w - sbx.b));
}

/* Stand-alone decoding, copy-on-write from string. */
void lj_serialize_decode(lua_State *L, TValue *o, GCstr *str)
{
  SBufExt sbx;
  char *r;
  memset(&sbx, 0, sizeof(SBufExt));
  lj_bufx_set_cow(L, &sbx, strdata(str), str->len);
  /* No need to set sbx.cowref here. */
  r = lj_serialize_get(&sbx, o);
  if (r != sbx.w) lj_err_caller(L, LJ_ERR_BUFFER_LEFTOV);
}

/* Peek into buffer to find the result IRType for specialization purposes. */
LJ_FUNC MSize lj_serialize_peektype(SBufExt *sbx)
{
  uint32_t tp;
  if (serialize_ru124(sbx->r, sbx->w, &tp)) {
    /* This must match the handling of all tags in the decoder above. */
    switch (tp) {
    case SER_TAG_NIL: return IRT_NIL;
    case SER_TAG_FALSE: return IRT_FALSE;
    case SER_TAG_TRUE: return IRT_TRUE;
    case SER_TAG_NULL: case SER_TAG_LIGHTUD32: case SER_TAG_LIGHTUD64:
      return IRT_LIGHTUD;
    case SER_TAG_INT: return IRT_NUM;
    case SER_TAG_NUM: return IRT_NUM;
    case SER_TAG_TAB: case SER_TAG_TAB+1: case SER_TAG_TAB+2:
    case SER_TAG_TAB+3: case SER_TAG_TAB+4: case SER_TAG_TAB+5:
      return IRT_TAB;
    case SER_TAG_INT64: case SER_TAG_UINT64: case SER_TAG_COMPLEX:
      return IRT_CDATA;
    case SER_TAG_DICT:
    default:
      return IRT_STR;
    }
  }
  return IRT_NIL;  /* Will fail on actual decode. */
}

#endif
