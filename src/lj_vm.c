/*
** RaptorJIT virtual machine bytecode interpreter.
*/

#include <assert.h>
#include <stdint.h>
#include <setjmp.h>
#include <stdio.h>

#include "lj_bc.h"
#include "lj_ccall.h"
#include "lj_dispatch.h"
#include "lj_frame.h"
#include "lj_lib.h"
#include "lj_obj.h"
#include "lj_state.h"
#include "lj_vm.h"
#include "lj_tab.h"
#include "lj_meta.h"
#include "lj_func.h"
#include "lj_buf.h"

#define TRACE(name) printf("%-6s A=%-3d B=%-3d C=%-3d D=%-5d stackdepth=%ld\n", \
                           name, A, B, C, D, TOP-BASE)
#define TRACEFF(name) printf("ASMFF  OP=%-3x %-10s\n", OP, name)

#define neg(n) (-1 - (n))
#define max(a,b) ((a)>(b) ? (a) : (b))
#define min(a,b) ((a)<(b) ? (a) : (b))

/* Forward declarations. */
static int vm_return(lua_State *L, uint64_t link, int resultofs, int nresults);
void fff_fallback(lua_State *L);

/* Simple debug utility. */
#if 0
static void printstack(lua_State *L)
{
  int i;
  for (i = -2; i < L->top - L->base; i++) {
    TValue *v = L->base + i;
    printf("[%3d] %p 0x%lx %s\n", i, v, v->u64, lj_typename(v));
    fflush(stdout);
  }
}
static void printupvalues(GCfuncL *parent)
{
  int i;
  for (i = 0; i < parent->nupvalues; i++) {
    TValue *v = parent->uvptr[i]->uv.v;
    printf("[%3d] %p 0x%lx %s\n", i, v, v->u64, lj_typename(v));
    fflush(stdout);
  }
}
#endif

/*
 * Random notes:
 *
 * This interpreter keeps the value of L->top consistent between
 * bytecodes. This is helpful for debugging. The assembler VM only
 * updates L->top when calling external code that might use it.
 */


/* -- Virtual machine registers ------------------------------------------- */

/* The "registers" in the virtual machine are simply some important
 * values that frequently instructions manipulate. We give these
 * values short names so that it is easier to talk about what each
 * instruction does in terms of the registers that it manipulates.
 * 
 * We provide macros to abstract over the way each register is
 * represented. We refer to each register by a NAME whether it is
 * represented by a global variable, a slot in the lua_State struct,
 * etc.
 */

/* Backing variables for certain registers. */
static int multres;
static int nargs;
static void *kbase;
static const BCIns *pc;

/* Program counter (PC) register stores the address of the next
 * instruction to run after the current instruction finishes.
 *
 * Most instructions simply increment the program counter. Branch and
 * function call instructions load new values to make a jump.
 */
#define PC pc

/* BASE (base stack slot) is the first stack slot for use by the
 * current stack frame.
 *
 * BASE[N>=0] holds the Nth local value accessible in the current
 * function call. These slots include function arguments, local
 * variables, and temporary values.
 *
 * BASE[-1] encodes the state needed to return to the previous frame.
 * BASE[-2] holds a reference to the currently running function.
 *
 * See lj_frame.h for more details of how call frames are linked.
 */
#define BASE (L->base)

/* TOP (top stack frame) is the last valid stack slot.
 *
 * The values BASE..TOP are the local values within the stack frame
 * that are typically referenced as instruction operands. 
 */
#define TOP (L->top)

/* NARGS (number of arguments) register specifies the number of fixed
 * arguments in a function call.
 *
 * NARGS is set by function calling instructions (e.g. CALL) and then
 * read by function header instructions (e.g. FUNCF).
 */
#define NARGS nargs

/* MULTRES (multiple results) register specifies the number of values
 * provided by a multiple-valued instruction. The multiple values are
 * counted separately from (in addition to) any fixed values.
 *
 * MULTRES is read by multiple value call instructions (e.g.
 * CALLM) and set by multiple value return instructions (e.g. RETM, FUNCC).
 */
/* XXX Rename to e.g. "MULTVAL" since this is used for both results and arguments? */
#define MULTRES multres

/* KBASE (constant base) register specifies the base address of the
 * array of constants that can be referenced by instructions. The
 * constants are specific to each function definition (prototype.) 
 *
 * The array is divided into two parts: pointer constants (GCobj*) at
 * negative indices and tagged-value (TValue) constants at
 * non-negative indecies. Specifically,
 *
 * KBASE[N>=0] holds the Nth TValue constant.
 * KBASE[N<0] holds the ~Nth (bitwise complement of N) GCptr* constant.
 */
#define KBASE kbase

/* STATE describes what kind of code the virtual machine is running.
 *
 * STATE=<0 is a complemented value from the LJ_VMST enum e.g.
 * ~LJ_VMST_INTERP or ~LJ_VMST_GC or ~LJ_VMST_C.
 *
 * STATE>0 is the number of the trace whose machine code is running.
 */
#define STATE (G(L)->vmstate)

/* Registers OP, A, B, C, and D are loaded with the opcode and
 * operands of the current instruction. Exactly which of these
 * registers contains a valid value varies between different
 * instructions.
 *
 * Note: Changing PC does not automatically update these registers.
 */
#define OP bc_op(curins)
#define A  bc_a(curins)
#define B  bc_b(curins)
#define C  bc_c(curins)
#define D  bc_d(curins)


/* -- Utility functions --------------------------------------------------- */

/* Copy values from 'src' to 'dst' and fill missing values with nil.
 * Return pointer to element after the last one filled in dst.
 */
static TValue *copyTVs(lua_State *L, TValue *dst, TValue *src,
                       int need, int have)
{
  int ncopy = min(need, have);
  int npad  = max(0, need - have);
  lua_assert(need>=0);
  lua_assert(have>=0);
  while (ncopy-- > 0) copyTV(L, dst++, src++);
  while (npad--  > 0) setnilV(dst++);
  return dst;
}

/* Return the nth constant TValue. */
static inline TValue* ktv(int n)
{
  return (TValue*)KBASE + n;
}

/* Return the nth constant GC object. */
static inline const GCobj* kgc(int n)
{
  return *((const GCobj**)KBASE-1-n);
}

static inline void branchPC(int offset)
{
  PC += offset - BCBIAS_J;
}

/* Reference the nth constant GC object with known type. */
#define kgcref(n, type) ((type *)kgc(n))


/* Execute virtual machine instructions in a tail-recursive loop. */
void execute(lua_State *L) {
  BCIns curins;
 execute:
  curins = *PC++;
  switch (OP) {
  case BC_ISLT:   assert(0 && "NYI BYTECODE: ISLT");
  case BC_ISGE:   assert(0 && "NYI BYTECODE: ISGE");
  case BC_ISLE:   assert(0 && "NYI BYTECODE: ISLE");
  case BC_ISGT:
    /* ISGT: Take following JMP instruction if A > D. */
    TRACE("ISGT");
    {
      int flag;
      if (tvisnum(BASE+A) && tvisnum(BASE+D)) {
        /* Compare two floats. */
        flag = (BASE+A)->n > (BASE+D)->n;
      } else {
        /* Fall back to meta-comparison. */
        TValue *res = lj_meta_comp(L, BASE+A, BASE+D, OP);
        if ((intptr_t)res > 1)
          assert(0 && "NYI: ISGT metamethod");
        else
          flag = (intptr_t)res == 1;
      }
      /* Advance to jump instruction. */
      curins = *PC++;
      if (flag) branchPC(D);
    }
    break;
  case BC_ISEQV:
    /* ISEQV: Take following JMP instruction if A is equal to D. */
    TRACE("ISEQV");
  case BC_ISNEV:
    /* ISNEV: Take following JMP instruction if A is not equal to D. */
    if (OP == BC_ISNEV)
      TRACE("ISNEV");
    {
      TValue *x = BASE+A; TValue *y = BASE+D;
      int flag = (OP == BC_ISNEV); // Invert flag on ISNEV.
      if (tvisnum(x) && tvisnum(y))
        flag ^= (x->n == y->n);
      else if (tviscdata(x) || tviscdata(y))
        assert(0 && "NYI: ISEQV/ISNEV on cdata.");
      else if (x->u64 == y->u64)
        // Same GCobjs or pvalues?
        flag ^= 1;
      else if (itype(x) != itype(y))
        // Not the same type?
        flag = flag;
      else if (itype(x) <= LJ_TISTABUD)
        // Different tables or userdatas. Need to check __eq metamethod.
        assert(0 && "NYI: ISEQV/ISNEV on tables/userdatas.");
      curins = *PC++;
      if (flag) branchPC(D);
    }
    break;
  case BC_ISEQS:
    /* ISEQS: Take following JMP instruction if A is equal to string D. */
    TRACE("ISEQS");
  case BC_ISNES:
    /* ISNES: Take following JMP instruction if A is not equal to string D. */
    if (OP == BC_ISNES)
      TRACE("ISNES");
    {
      int flag = (OP == BC_ISNES); // Invert flag on ISNES.
      if (tvisstr(BASE+A))
        flag ^= (strV(BASE+A) == kgcref(D, GCstr));
      else if (tviscdata(BASE+A))
        assert(0 && "NYI: ISEQS/ISNES on cdata.");
      curins = *PC++;
      if (flag) branchPC(D);
    }
    break;
  case BC_ISEQN:
    /* ISEQN: Take following JMP if A is equal to number constant D. */
    TRACE("ISEQN");
  case BC_ISNEN:
    /* ISNEN: Take following JMP if A is not equal to number constant D. */
    if (OP == BC_ISNEN)
      TRACE("ISNEN");
    {
      int flag = (OP == BC_ISNEN); // Invert flag on ISNEN.
      if (tvisnum(BASE+A))
        flag ^= numV(BASE+A) == numV(ktv(D));
      else if (tviscdata(BASE+A))
        assert(0 && "NYI: ISEQN/ISNEN on cdata.");
      curins = *PC++;
      if (flag) branchPC(D);
    }
    break;
  case BC_ISEQP:  assert(0 && "NYI BYTECODE: ISEQP");
  case BC_ISNEP:  assert(0 && "NYI BYTECODE: ISNEP");
  case BC_ISTC:
    /* ISTC: Copy D to A and take following JMP instruction if D is true. */
    TRACE("ISTC");
  case BC_ISFC:
    /* ISFC: Copy D to A and take following JMP instruction if D is false. */
    if (OP == BC_ISFC)
      TRACE("ISFC");
    {
      int flag = (OP == BC_ISFC); // Invert flag on ISFC.
      BASE[A] = BASE[D];
      flag ^= tvistruecond(BASE+D);
      curins = *PC++;
      if (flag) branchPC(D);
    }
    break;
  case BC_IST:
    /* IST: Take following JMP instruction if D is true. */
    TRACE("IST");
    {
      int flag = tvistruecond(BASE+D);
      /* Advance to jump instruction. */
      curins = *PC++;
      if (flag) branchPC(D);
    }
    break;
  case BC_ISF:
    TRACE("ISF");
    {
      int flag = !tvistruecond(BASE+D);
      /* Advance to jump instruction. */
      curins = *PC++;
      if (flag) branchPC(D);
    }
    break;
  case BC_ISTYPE: assert(0 && "NYI BYTECODE: ISTYPE");
  case BC_ISNUM:  assert(0 && "NYI BYTECODE: ISNUM");
  case BC_MOV:
    TRACE("MOV");
    /* MOV: A = dst; D = src */
    copyTV(L, BASE+A, BASE+D);
    break;
  case BC_NOT:    assert(0 && "NYI BYTECODE: NOT");
  case BC_UNM:    assert(0 && "NYI BYTECODE: UNM");
  case BC_LEN:
    /* LEN: Set A to #D (object length). */
    TRACE("LEN");
    {
      TValue *dst = BASE+A;
      TValue *o = BASE+D;
      if (tvisstr(o)) {
        setnumV(dst, strV(o)->len);
      } else if (tvistab(o)) {
        GCtab *t = tabV(o);
        assert(!t->metatable && "NYI: LEN on meta table");
        setnumV(dst, lj_tab_len(t));
      } else {
        assert(0 && "NYI: LEN on cdata");
      }
    }
    break;
  case BC_ADDVN:
    /* ADDVN: Add number constant C to B and store the result in A. */
    TRACE("ADDVN");
    {
      assert(tvisnum(BASE+B) && "NYI: ADDVN with meta method");
      setnumV(BASE+A, numV(BASE+B) + numV(ktv(C)));
    }
    break;
  case BC_SUBVN:
    /* SUBVN: Subtract number constant C from  B and store the result in A. */
    TRACE("SUBVN");
    {
      assert(tvisnum(BASE+B) && "NYI: SUBVN with meta method");
      setnumV(BASE+A, numV(BASE+B) - numV(ktv(C)));
    }
    break;
  case BC_MULVN:  assert(0 && "NYI BYTECODE: MULVN");
  case BC_DIVVN:
    /* DIVVN: Divide B by number constant C and store the result in A. */
    TRACE("DIVVN");
    {
      assert(tvisnum(BASE+B) && "NYI: DIVVN with meta method");
      setnumV(BASE+A, numV(BASE+B) / numV(ktv(C)));
    }
    break;
  case BC_MODVN:  assert(0 && "NYI BYTECODE: MODVN");
  case BC_ADDNV:  assert(0 && "NYI BYTECODE: ADDNV");
  case BC_SUBNV:  assert(0 && "NYI BYTECODE: SUBNV");
  case BC_MULNV:  assert(0 && "NYI BYTECODE: MULNV");
  case BC_DIVNV:  assert(0 && "NYI BYTECODE: DIVNV");
  case BC_MODNV:  assert(0 && "NYI BYTECODE: MODNV");
  case BC_ADDVV:  assert(0 && "NYI BYTECODE: ADDVV");
  case BC_SUBVV:  assert(0 && "NYI BYTECODE: SUBVV");
  case BC_MULVV:  assert(0 && "NYI BYTECODE: MULVV");
  case BC_DIVVV:  assert(0 && "NYI BYTECODE: DIVVV");
  case BC_MODVV:  assert(0 && "NYI BYTECODE: MODVV");
  case BC_POW:    assert(0 && "NYI BYTECODE: POW");
  case BC_CAT:
    TRACE("CAT");
    {
      TValue *res = lj_meta_cat(L, BASE + C, C-B);
      assert(res == NULL && "NYI: CAT metamethod");
      copyTV(L, BASE+A, BASE+B);
    }
    break;
  case BC_KSTR:
    TRACE("KSTR");
    setgcVraw(BASE+A, kgcref(D, GCobj), LJ_TSTR);
    break;
  case BC_KCDATA: assert(0 && "NYI BYTECODE: KCDATA");
  case BC_KSHORT:
    TRACE("KSHORT");
    /* BASE[A] = D */
    setnumV(BASE+A, (int16_t) D); // D is a signed int16 literal.
    break;
  case BC_KNUM:   assert(0 && "NYI BYTECODE: KNUM");
  case BC_KPRI:
    TRACE("KPRI");
    {
      assert(D <= 2 && "NYI PRIMITIVE != 0/1/2");
      if (D) setboolV(BASE+A, D-1);
      else   setnilV(BASE+A);
    }
    break;
  case BC_KNIL:   assert(0 && "NYI BYTECODE: KNIL");
  case BC_UGET:
    TRACE("UGET");
    {
      GCfuncL *parent = &(funcV(BASE-2)->l);
      BASE[A] = *mref(parent->uvptr[D]->uv.v, TValue);
    }
    break;
  case BC_USETV:
    /* USETV: Set upvalue A to D. */
    TRACE("USETV");
    {
      GCfuncL *parent = &(funcV(BASE-2)->l);
      GCupval *uv = &parent->uvptr[A]->uv;
      TValue *v = (TValue *)uv->v;
      copyTV(L, v, BASE+D);
      // Upvalue closed, marked black, and new value is collectable and white?
      if (uv->closed && (uv->marked & LJ_GC_BLACK)
          && tvisgcv(v) && iswhite(gcval(v)))
        // Crossed a write barrier. Move the barrier forward.
        lj_gc_barrieruv(G(L), v);
    }
    break;
  case BC_USETS:
    /* USETS: Set upvalue A to string constant D. */
    TRACE("USETS");
    {
      GCfuncL *parent = &(funcV(BASE-2)->l);
      GCupval *uv = &parent->uvptr[A]->uv;
      TValue *v = (TValue *)uv->v;
      GCobj *o = kgcref(D, GCobj);
      setgcVraw(v, o, LJ_TSTR);
      // Upvalue closed, marked black, and new value is white?
      if (uv->closed && (uv->marked & LJ_GC_BLACK) && iswhite(o))
        // Crossed a write barrier. Move the barrier forward.
        lj_gc_barrieruv(G(L), v);
    }
    break;
  case BC_USETN:  assert(0 && "NYI BYTECODE: USETN");
  case BC_USETP:  assert(0 && "NYI BYTECODE: USETP");
  case BC_UCLO:
    /* UCLO: Close upvalues for slots ≥ rbase and jump to target D. */
    TRACE("UCLO");
    if (L->openupval > 0)
      lj_func_closeuv(L, BASE+A);
    branchPC(D);
    break;
  case BC_FNEW:
    TRACE("FNEW");
    {
      GCproto *pt = kgcref(D, GCproto);
      GCfuncL *parent = &(funcV(BASE-2)->l);
      GCfunc *fn = lj_func_newL_gc(L, pt, parent);
      setgcVraw(BASE+A, (GCobj*)fn, LJ_TFUNC);
    }
    break;
  case BC_TNEW:
    TRACE("TNEW");
    if (G(L)->gc.total > G(L)->gc.threshold) {
      lj_gc_step_fixtop(L);
    }
    {
      uint32_t asize = D & ((1<<11)-1);
      uint32_t hbits = D >> 11;
      GCtab *tab = lj_tab_new(L, asize, hbits);
      setgcVraw(BASE+A, (GCobj*)tab, LJ_TTAB);
    }
    break;
  case BC_TDUP:
    TRACE("TDUP");
    if (G(L)->gc.total > G(L)->gc.threshold) {
      lj_gc_step_fixtop(L);
    }
    {
      GCtab *tab = lj_tab_dup(L, kgcref(D, GCtab));
      setgcVraw(BASE+A, (GCobj*)tab, LJ_TTAB);
    }
    break;
  case BC_GGET:
    TRACE("GGET");
    /* A = _G[D] */
    {
      GCfunc *fn = funcV(BASE-2);
      GCtab *env = tabref(fn->l.env);
      GCstr *key = kgcref(D, GCstr);
      cTValue *tv = lj_tab_getstr(env, key);
      assert(tv && !tvisnil(tv));
      copyTV(L, BASE+A, tv);
      break;
    }
  case BC_GSET:   assert(0 && "NYI BYTECODE: GSET");
  case BC_TGETV:
    /* TGETV: A = B[C] */
    TRACE("TGETV");
    {
      TValue *o = BASE+B;
      cTValue *res;
      if (tvistab(o)) {
        GCtab *tab = tabV(o);
        res = lj_tab_get(L, tab, BASE+C);
      } else {
        res = lj_meta_tget(L, o, BASE+C);
        assert(res != NULL && "NYI: lj_meta_tget unreachable");
      }
      if (res)
        copyTV(L, BASE+A, res);
      else
        setnilV(BASE+A);
      break;
    }
  case BC_TGETS:
    TRACE("TGETS");
    {
      TValue *o = BASE+B;
      cTValue *res;
      GCstr *key = kgcref(C, GCstr);
      if (tvistab(o)) {
        GCtab *tab = tabV(o);
        res = lj_tab_getstr(tab, key);
      } else {
        TValue tvkey;
        /* Convert key to tagged value. */
        setgcVraw(&tvkey, obj2gco(key), LJ_TSTR);
        // XXX SAVE_L
        // XXX SAVE_PC
        res = lj_meta_tget(L, o, &tvkey);
        assert(res != NULL && "NYI: lj_meta_tget unreachable");
      }
      if (res)
        copyTV(L, BASE+A, res);
      else
        setnilV(BASE+A);
      break;
    }
  case BC_TGETB:  assert(0 && "NYI BYTECODE: TGETB");
  case BC_TGETR:  assert(0 && "NYI BYTECODE: TGETR");
  case BC_TSETV:
    TRACE("TSETV");
    {
      TValue *o = BASE+B;
      if (tvistab(o)) {
        GCtab *tab = tabV(o);
        copyTV(L, lj_tab_set(L, tab, BASE+C), BASE+A);
        lj_gc_anybarriert(L, tab);
      } else {
        TValue *v = lj_meta_tset(L, o, BASE+C);
        assert(v != NULL && "NYI: TSETV __newindex");
        /* NOBARRIER: lj_meta_tset ensures the table is not black. */
        copyTV(L, v, BASE+A);
      }
    }
    break;
  case BC_TSETS:
    TRACE("TSETS");
    {
      TValue *o = BASE+B;
      GCstr *key = kgcref(C, GCstr);
      if (tvistab(o)) {
        GCtab *tab = tabV(o);
        copyTV(L, lj_tab_setstr(L, tab, key), BASE+A);
        lj_gc_anybarriert(L, tab);
      } else {
        TValue tvkey;
        /* Convert key to tagged value. */
        setgcVraw(&tvkey, obj2gco(key), LJ_TSTR);
        // XXX SAVE_L
        // XXX SAVE_PC
        TValue *v = lj_meta_tset(L, o, &tvkey);
        assert(v != NULL && "NYI: TSETS __newindex");
        /* NOBARRIER: lj_meta_tset ensures the table is not black. */
        copyTV(L, v, BASE+A);
      }
      break;
    }
  case BC_TSETB:
    /* TSETB: B[C] = A where C is an unsigned literal. */
    TRACE("TSETB");
    {
      TValue *o = BASE+B;
      TValue key;
      key.n = C;
      if (tvistab(o)) {
        GCtab *tab = tabV(o);
        copyTV(L, lj_tab_set(L, tab, &key), BASE+A);
        lj_gc_anybarriert(L, tab);
      } else {
        TValue *v = lj_meta_tset(L, o, &key);
        assert(v != NULL && "NYI: TSETB __newindex");
        /* NOBARRIER: lj_meta_tset ensures the table is not black. */
        copyTV(L, v, BASE+A);
      }
    }
    break;
  case BC_TSETM:
    TRACE("TSETM");
    {
      int i = 0, ix = ktv(D)->u32.lo;
      TValue *o = BASE+A-1;
      GCtab *tab = tabV(o);
      if (isblack((GCobj*)tab))
        lj_gc_barrierback(G(L), tab);
      if (tab->asize < ix+NARGS)
        lj_tab_reasize(L, tab, ix + NARGS);
      for (i = 0; i < NARGS; i++)
        copyTV(L, BASE+A+i, BASE+D+i);
    }
    break;
  case BC_TSETR:  assert(0 && "NYI BYTECODE: TSETR");
  case BC_CALLM: case BC_CALL:
    if (OP == BC_CALLM) {
      /* CALLM: A = newbase, B = nresults+1, C = extra_nargs */
      TRACE("CALLM");
      NARGS = C+MULTRES; /* nargs in MULTRES */
    } else if (OP == BC_CALL) {
      /* CALL: A = newbase, B = nresults+1, C = nargs+1 */
      TRACE("CALL");
      NARGS = C-1;
    }
    {
      TValue *f;
      /* Setup new base for callee frame. */
      BASE += 2 + A;
      f = BASE-2;
      assert(tvisfunc(f) && "NYI: CALL to non-function");
      /* Notes:
       *
       * PC is 32-bit aligned and so the low bits are always 00 which
       * corresponds to the FRAME_LUA tag value.
       *
       * CALL does not have to record the number of expected results
       * in the frame data. The callee's RET bytecode will locate this
       * CALL and read the value from the B operand. */
      BASE[-1].u64 = (intptr_t)PC;
      PC = mref(funcV(f)->l.pc, BCIns);
    }
    break;
  case BC_CALLMT: assert(0 && "NYI BYTECODE: CALLMT");
  case BC_CALLT:
    /* CALLT: Tailcall A(A+1, ..., A+D-1). */
    TRACE("CALLT");
    {
      NARGS = D-1;
      MULTRES = NARGS;
      TValue *callbase = BASE + A+2;
      TValue *f = callbase-2;
      assert(tvisfunc(f) && "NYI: CALLT to non-function");
      assert((BASE[-1].u64 & FRAME_TYPE) == FRAME_LUA
             && "NYI: CALLT from vararg function");
      // Copy function and arguments down into parent frame.
      BASE[-2] = *f;
      copyTVs(L, BASE, callbase, NARGS, NARGS);
      assert(funcV(f)->l.ffid <= FF_C && "NYI: CALLT to ASM fast function");
      PC = mref(funcV(f)->l.pc, BCIns);
    }
    break;
  case BC_ITERC:
    TRACE("ITERC");
    {
      TValue *fb = BASE+A+2;
      fb[0] = fb[-4]; // Copy state.
      fb[1] = fb[-3]; // Copy control var.
      fb[-2] = fb[-5]; // Copy callable.
      NARGS = 2; // Handle like a regular 2-arg call.
      TValue *f = fb-2;
      if (!tvisfunc(f)) {
        lj_meta_call(L, f, fb+NARGS);
        NARGS += 1;
        assert(BASE != KBASE && "NYI: ITERC tail call");
      }
      BASE = fb;
      BASE[-1].u64 = (intptr_t)PC;
      PC = mref(funcV(f)->l.pc, BCIns);
    }
    break;
  case BC_ITERN:
    /* ITERN: Specialized ITERC, if iterator function A-3 is next(). */
    TRACE("ITERN");
    {
      // NYI: add hotloop, record BC_ITERN.
      GCtab *tab = tabV(BASE + A-2);
      TValue *state = BASE + A-1;
      TValue *key = BASE + A+0;
      TValue *val = BASE + A+1;
      int i = state->i;
      /* Advance to ITERL instruction. */
      curins = *PC++;
      /* Traverse array part. */
      while (i < tab->asize) {
        cTValue *entry = arrayslot(tab, i);
        if (tvisnil(entry) && ++i) continue; // Skip holes in array part.
        /* Return array index as a numeric key. */
        setnumV(key, i);
        /* Copy array slot to returned value. */
        *val = *entry;
        /* Update control var. */
        state->i = i+1;
        goto itern_next;
      }
      /* Traverse hash part. */
      i -= tab->asize;
      while (i <= tab->hmask) {
        Node *n = &noderef(tab->node)[i];
        if (tvisnil(&n->val) && ++i) continue; // Skip holes in hash part.
        /* Copy key and value from hash slot. */
        *key = n->key;
        *val = n->val;
        /* Update control var. */
        state->i = tab->asize + i+1;
        goto itern_next;
      }
      goto itern_end;
    itern_next:
      /* Iterate: branch to target from ITERL. */
      branchPC(D);
    itern_end:
      /* End of iteration: advance to ITERL+1. */
      break;
    }
  case BC_VARG:
    TRACE("VARG");
    {
      int delta = BASE[-1].u64 >> 3;
      MULTRES = delta;
      copyTVs(L, BASE+A, BASE-2-delta+C, B>0 ? B : MULTRES, MULTRES);
    }
    break;
  case BC_ISNEXT:
    /* ISNEXT: Verify ITERN specialization and jump. */
    TRACE("ISNEXT");
    {
      TValue *fn = BASE + A-3;
      TValue *tab = BASE + A-2;
      TValue *nil = BASE + A-1;
      branchPC(D);
      if (tvisfunc(fn)
          && funcV(fn)->c.ffid == FF_next_N
          && tvistab(tab)
          && tvisnil(nil))
        BASE[A-1].u64 = U64x(fffe7fff, 00000000); // Initialize control var.
      else
        /* Despecialize bytecode if any of the checks fail. */
        setbc_op(PC, BC_ITERC);
    }
    break;
  case BC_RETM:
    TRACE("RETM");
    if (vm_return(L, BASE[-1].u64, A, D+MULTRES)) return;
    break;
  case BC_RET:
    TRACE("RET");
    if (vm_return(L, BASE[-1].u64, A, D-1)) return;
    break;
  case BC_RET0:
    TRACE("RET0");
    if (vm_return(L, BASE[-1].u64, A, 0)) return;
    break;
  case BC_RET1:
    TRACE("RET1");
    if (vm_return(L, BASE[-1].u64, A, 1)) return;
    break;
  case BC_FORL:
    TRACE("FORL");
    break;        /* XXX hotloop */
  case BC_JFORI:  assert(0 && "NYI BYTECODE: JFORI");
  case BC_FORI:
    TRACE("FORI");
    {
      TValue *state = BASE + A;
      TValue *idx = state, *stop = state+1, *step = state+2, *ext = state+3;
      assert(tvisnum(idx)  && "NYI: non-number loop index");
      assert(tvisnum(stop) && "NYI: non-number loop stop");
      assert(tvisnum(step) && "NYI: non-number loop step");
      setnumV(ext, idx->n);
      /* Check for termination */
      if ((step->n >= 0 && idx->n >= stop->n) ||
          (step->n <  0 && stop->n >= idx->n)) {
        pc += bc_j(D);
      }
    }
    break;
  case BC_IFORL:  assert(0 && "NYI BYTECODE: IFORL");
  case BC_JFORL:  assert(0 && "NYI BYTECODE: JFORL");
  case BC_ITERL:
    TRACE("ITERL");
    /* XXX hotloop */
  case BC_IITERL:
    if (OP == BC_IITERL) TRACE("IITERL");
    if (!tvisnil(BASE+A)) {
      /* Save control var and branch. */
      branchPC(D);
      BASE[A-1] = *(BASE+A);
    }
  case BC_JITERL:
    if (OP == BC_JITERL) assert(0 && "NYI BYTECODE: JITERL");
    break;
  case BC_LOOP:   assert(0 && "NYI BYTECODE: LOOP");
  case BC_ILOOP:  assert(0 && "NYI BYTECODE: ILOOP");
  case BC_JLOOP:  assert(0 && "NYI BYTECODE: JLOOP");
  case BC_JMP:
    TRACE("JMP");
    branchPC(D);
    break;
  case BC_FUNCF:
    TRACE("FUNCF");
    {
      GCproto *pt = (GCproto*)((intptr_t)(PC-1) - sizeof(GCproto));
      KBASE = mref(pt->k, void);
      /* Fill missing args with nil. */
      if (A > NARGS) copyTVs(L, BASE+NARGS, NULL, A-NARGS, 0);
    }
    break;
  case BC_IFUNCF: assert(0 && "NYI BYTECODE: IFUNCF");
  case BC_JFUNCF: assert(0 && "NYI BYTECODE: JFUNCF");
  case BC_FUNCV:
    TRACE("FUNCV");
    {
      GCproto *pt = (GCproto*)((intptr_t)(PC-1) - sizeof(GCproto));
      /* Save base of frame containing all parameters. */
      TValue *oldbase = BASE;
      /* Base for new frame containing only fixed parameters. */
      BASE += 2 + NARGS;
      copyTV(L, BASE-2, oldbase-2);
      BASE[-1].u64 = FRAME_VARG + ((BASE - oldbase) << 3);
      copyTVs(L, BASE, oldbase, pt->numparams, NARGS);
      TOP = BASE + pt->framesize;
      /* Set constant pool address. */
      KBASE = mref(pt->k, void);
    }      
    break;
  case BC_IFUNCV: assert(0 && "NYI BYTECODE: IFUNCV");
  case BC_JFUNCV: assert(0 && "NYI BYTECODE: JFUNCV");
  case BC_FUNCCW: assert(0 && "NYI BYTECODE: FUNCCW");
  case BC_FUNCC:
    TRACE("FUNCC");
    /* 
    ** Call C function.
    */
    {
      int nresults, resultofs;
      lua_CFunction *f = &funcV(BASE-2)->c.f; /* C function pointer */
      assert(TOP+LUA_MINSTACK <= mref(L->maxstack, TValue));
      assert(OP == BC_FUNCC); /* XXX */
      TOP = BASE + NARGS;
      STATE = ~LJ_VMST_C;
      nresults = (*f)(L);
      STATE = ~LJ_VMST_INTERP;
      resultofs = TOP - (BASE + nresults);
      if (vm_return(L, BASE[-1].u64, resultofs, nresults)) return;
    }
    break;
  default:
    /*
      XXX - handle ASM fast functions.
      FIXME: need symbols for pseudo opcodes.
    */
    switch ((uint32_t)OP) {
    case 0x61:
      TRACEFF("assert");
      {
        if (tvistruecond(BASE))
          vm_return(L, BASE[-1].u64, 0, NARGS);
        else
          fff_fallback(L);
      }
      break;
    case 0x62:
      TRACEFF("type");
      {
        uint32_t type = itype(BASE);
        GCfuncC *f = &funcV(BASE-2)->c;
        if (type < LJ_TISNUM)
          type = LJ_TISNUM;
        type = ~type;
        BASE[-2] = f->upvalue[type];
        vm_return(L, BASE[-1].u64, -2, 1);
      }
      break;
    case 0x64:
      TRACEFF("pairs");
      /* XXX - punt to fallback. */
      fff_fallback(L);
      break;
    case 0x65:
      TRACEFF("ipairs_aux");
      {
        TValue *tab = BASE;
        TValue *i = BASE+1;
        const TValue *v;
        if (!tvistab(tab) || !tvisnum(i)) {
          fff_fallback(L);
          break;
        }
        uint64_t link = BASE[-1].u64;
        /* Increment index. */
        uint32_t n = numV(i) + 1;
        setnumV(BASE-2, n);
        /* Try to load value from table (if this fails the iterator ends.)  */
        if (n < tabV(tab)->asize) {
          /* Value is in array part of tab. */
          v = arrayslot(tabV(tab), n);
          if (tvisnil(v)) goto ipairs_end;
        } else {
          if (!tabV(tab)->hmask) goto ipairs_end;
          v = lj_tab_getinth(tabV(tab), n);
          if (!v) goto ipairs_end;
        }
        BASE[-1] = *v; /* Copy array slot. */
        vm_return(L, link, -2, 2); /* Iterate: return (i, value). */
        break;
        /* End of interator: return no values. */
        ipairs_end: vm_return(L, link, -2, 0);
      }
      break;
    case 0x66:
      TRACEFF("ipairs");
      /* XXX - punt to fallback. */
      fff_fallback(L);
      break;
    case 0x67:
      TRACEFF("getmetatable");
      {
        GCtab *mt;
        if (tvistab(BASE))
          mt = tabref(tabV(BASE)->metatable);
        else if (tvisudata(BASE))
          mt = tabref(udataV(BASE)->metatable);
        else
          mt = tabref(basemt_obj(G(L), BASE));
        if (mt) {
          cTValue *mo = lj_tab_getstr(mt, mmname_str(G(L), MM_metatable));
          setgcVraw(BASE-2, (GCobj*)(mo ? (GCtab*)mo : mt), LJ_TTAB);
        } else {
          setnilV(BASE-2);
        }
        vm_return(L, BASE[-1].u64, -2, 1);
      }
      break;
    case 0x68:
      TRACEFF("setmetatable");
      /* XXX - punt to fallback. */
      fff_fallback(L);
      break;
    case 0x6a:
      TRACEFF("tonumber");
      {
        if (NARGS != 1 || !tvisnumber(BASE))
          fff_fallback(L);
        else
          vm_return(L, BASE[-1].u64, 0, 1);
      }
      break;
    case 0x6c:
      TRACEFF("pcall");
      {
        /* First argument is the function to call, consume it. */
        TValue *f = BASE;
        NARGS -= 1;
        /* Push pcall frame. */
        BASE += 2;
        copyTVs(L, BASE, BASE-1, NARGS, NARGS); /* Copy function arguments. */
        BASE[-1].u64 = 16 + FRAME_PCALL + (hook_active(G(L)) ? 1 : 0);
        /* Call protected function. */
        assert(tvisfunc(f) && "NYI: pcall to non-function");
        PC = mref(funcV(f)->l.pc, BCIns);
      }
      break;
    case 0x96:
      TRACEFF("sub");
      {
        if (G(L)->gc.total > G(L)->gc.threshold)
          lj_gc_step_fixtop(L);
        if (NARGS < 2 || !tvisstr(BASE) || !tvisnum(BASE+1)
            || (NARGS > 2 && !tvisnum(BASE+2))) {
          fff_fallback(L);
          break;
        }
        GCstr *str = strV(BASE);
        int start = numV(BASE+1);
        int end = NARGS > 2 ? numV(BASE+2) : -1;
        if (start < 0)
          start = max(start + str->len+1, 1);
        else
          start = max(min(start, str->len), 1);
        if (end < 0)
          end = max(end + str->len+1, 0);
        else
          end = min(end, str->len);
        str = lj_str_new(L, strdata(str)+start-1, max(1+end-start, 0));
        setgcVraw(BASE-2, (GCobj *)str, LJ_TSTR);
        vm_return(L, BASE[-1].u64, -2, 1);
      }
      break;
    case 0x98:
      /* Fast function string operations. */
      {
        if (G(L)->gc.total >= G(L)->gc.threshold)
          lj_gc_step(L);
        if (!tvisstr(BASE))
          fff_fallback(L);
        GCstr *str = strV(BASE);
        SBuf *buf = &G(L)->tmpbuf;
        buf->L = L;
        buf->p = buf->b;
        switch ((uint32_t)OP) {
        case 0x98:
          TRACEFF("string_lower");
          lj_buf_putstr_lower(buf, str);
          break;
        default: assert(0 && "NYI: fast string operation");
        }
        setgcVraw(BASE, (GCobj *)lj_buf_tostr(buf), LJ_TSTR);
        vm_return(L, BASE[-1].u64, 0, 1);
      }
      break;
    default: assert(0 && "INVALID BYTECODE");
    }
  }
  /* Tail recursion. */
  goto execute;
}


/* -- Return handling ----------------------------------------------------- */

/* Return to the previous frame.
 *
 * Ensures that...
 *   BASE is restored to the base frame of the previous stack frame.
 *   Return values are copied to BASE..BASE+NRESULTS+MULTRES.
 *
 * Returns true if the virtual machine should 'return' on the C stack
 * i.e. if we are returning from this invocation of the bytecode interpreter.
 */
static int vm_return(lua_State *L, uint64_t link, int resultofs, int nresults) {
  switch (link & FRAME_TYPE) {
  case FRAME_C:
    /* Returning from the VM to C code. */
    {
      CFrame *cf = (CFrame*)L->cframe;
      int delta = link>>3;
      int nexpected = cf->nresults;
      TValue *dst = BASE + resultofs;
      STATE = ~LJ_VMST_C;
      /* Push TRUE for successful return from a pcall.  */
      if (link & FRAME_P) {
        setboolV(--dst, 1);
        nresults += 1;
      }
      if (nexpected < 0) // Return all results.
        MULTRES = nexpected = nresults;
      /* Copy results into caller frame */
      dst = copyTVs(L, BASE-2, dst, nexpected, nresults);
      TOP = dst; // When returning from C frames, last result is the new TOP.
      BASE -= delta;
      return 1;
    }
    break;
  case FRAME_LUA:
    PC = (BCIns*)link;
    {
      /* Find details in caller's CALL instruction operands. */
      int delta = bc_a(*(PC-1));
      int nexpected = bc_b(*(PC-1)) - 1;
      GCproto *pt;
      if (nexpected < 0) // Return all results.
        MULTRES = nexpected = nresults;
      copyTVs(L, BASE-2, BASE+resultofs, nexpected, nresults);
      BASE -= 2 + delta;
      pt = funcproto(funcV(BASE-2));
      TOP = BASE + pt->framesize;
      KBASE = mref(pt->k, void);
      return 0;
    }
    break;
  case FRAME_VARG:
    /* Return from vararg function: relocate BASE down and resultofs up. */
    {
      int delta = link >> 3;
      BASE -= delta;
      return vm_return(L, BASE[-1].u64, resultofs + delta, nresults);
    }
    break;
  }
  switch (link & FRAME_TYPEP) {
  case FRAME_PCALL:
  case FRAME_PCALLH:
    {
      /* Pop pcall frame, and adjust resultofs accordingly.
       *
       * Decrement resultofs by one, and increment nresults by one.
       * Push TRUE for successful return from a pcall in front of results.
       * (We know there is space because we freed two slots from the pcall
       * frame.)
       *
       * Return from call frame with the adjusted resultofs/nresults.
       */
      BASE -= 2; resultofs += 2;
      resultofs--; nresults++; setboolV(BASE+resultofs, 1);
      return vm_return(L, BASE[-1].u64, resultofs, nresults);
    }
    break;
  }
  assert(0 && "NYI: Unsupported case in vm_return");
  return 0;
}


/* -- Fast ASM functions--------------------------------------------------- */

/* Fallback to ASM fast function handler
 *
 * Call fallback handler for ASM fast functions (relics from the ASM VM) and
 * massage VM state to return to caller.
 *
 * This can not just use vm_return and needs a special handler because ASK fast
 * functions are peculiar in multiple ways:
 *  - they use the stack space BASE-2..BASE+NARGS
 *  - they can yield to retries and tailcalls (NYI)
 *
 */
void fff_fallback(lua_State *L) {
  uint64_t link = BASE[-1].u64;
  TOP = BASE + NARGS;
  assert(TOP+1+LUA_MINSTACK <= mref(L->maxstack, TValue));
  lua_CFunction *f = &funcV(BASE-2)->c.f; /* C function pointer */
  int res = (*f)(L);
  switch (res) {
  case -1: assert(0 && "NYI: fff_fallback tailcall");
  case  0: assert(0 && "NYI: fff_fallback retry");
  default: /* FFH_RES(n) */
    vm_return(L, link, -2, res-1);
  }
}


/* -- API functions ------------------------------------------------------- */

/* Call a Lua function from C. */
int luacall(lua_State *L, int p, TValue *newbase, int nres, ptrdiff_t ef)
{
  int res;
  GCfunc *func;
  /* Add new CFrame to the chain. */
  CFrame cf = { L->cframe, L, nres };
  //assert(nres >= 0 && "NYI: LUA_MULTRET");
  L->cframe = &cf;
  /* Reference the now-current lua_State. */
  setgcref(G(L)->cur_L, obj2gco(L));
  /* Set return frame link. */
  newbase[-1].u64 = (p ? FRAME_CP : FRAME_C) + ((newbase - BASE) << 3);
  /* Setup VM state for callee. */
  STATE = ~LJ_VMST_INTERP;
  NARGS = (TOP - newbase);
  BASE = newbase;
  TOP = BASE + NARGS;
  /* Branch and execute callee. */
  func = funcV(newbase-2);
  PC = mref(func->l.pc, BCIns);
  /* Setup "catch" jump buffer for a protected call. */
  if ((res = _setjmp(cf.jb)) == 0) {
    /* Try */
    execute(L);
  } else {
    /* Catch */
    assert(0 && "NYI: Catch exception from Lua call");
  }
  /* Unlink C frame. */
  L->cframe = cf.previous;
  /* XXX */
  return LUA_OK;
}

/* Call a Lua function object. */
void lj_vm_call(lua_State *L, TValue *newbase, int nres) {
  luacall(L, 0, newbase, nres-1, 0); /* -1 to compensate for lj_api +1 */
}

/* Call a Lua function object with a protected (error-handling) stack
 * frame.
 */
int lj_vm_pcall(lua_State *L, TValue *newbase, int nres, ptrdiff_t ef)  {
  return luacall(L, 1, newbase, nres-1, ef);
}

/* Call a C function with a protected (error-handling) stack frame. */
int lj_vm_cpcall(lua_State *L, lua_CFunction f, void *ud, lua_CPFunction cp) { 
  int res;
  /* "Neg. delta means cframe w/o frame." */
  int nresults = -savestack(L, L->top) / sizeof(TValue);
  /* Add to CFrame chain. */
  CFrame cf = { L->cframe, L, nresults };
  L->cframe = &cf;
  /* Reference the now-current lua_State. */
  setgcref(G(L)->cur_L, obj2gco(L));
  if ((res = _setjmp(cf.jb)) == 0) {
    /* Try */
    TValue *newbase = cp(L, f, ud);
    if (newbase)
      res = luacall(L, 0, newbase, nresults, 0);
  } else {
    /* Catch */
    assert(0 && "NYI cpcall error");
  }
  /* Unlink C frame. */
  L->cframe = cf.previous;
  return res;
}

int lj_vm_resume(lua_State *L, TValue *base, int nres1, ptrdiff_t ef) { assert(0 && "NYI"); }
void lj_vm_unwind_c(void *cframe, int errcode) { assert(0 && "NYI"); }
void lj_vm_unwind_ff(CFrame *cframe) {
  lua_State *L = cframe->L;
  uint64_t link = BASE[-1].u64;
  setboolV(BASE-1, 0); /* Push FALSE for unsuccessful return from a pcall.  */
  vm_return(L, link, -1, 2);
  execute(L);
  exit(EXIT_FAILURE); /* Unreachable. */
}
void lj_vm_unwind_c_eh(void)                   { assert(0 && "NYI"); }
void lj_vm_unwind_ff_eh(void)                  { assert(0 && "NYI"); }
void lj_vm_unwind_rethrow(void)                { assert(0 && "NYI"); }
void lj_vm_ffi_callback()                      { assert(0 && "NYI"); }
void lj_vm_ffi_call(CCallState *cc)            { assert(0 && "NYI"); }

/* Miscellaneous functions. */
int lj_vm_cpuid(uint32_t f, uint32_t res[4])       {
  asm volatile("cpuid":"=a"(*res),"=b"(*(res+1)),
               "=c"(*(res+2)),"=d"(*(res+3)):"a"(f));
  return (int)res[0];
}

/* Dispatch targets for recording and hooks. */
void lj_vm_record(void)   { assert(0 && "NYI"); }
void lj_vm_inshook(void)  { assert(0 && "NYI"); }
void lj_vm_rethook(void)  { assert(0 && "NYI"); }
void lj_vm_callhook(void) { assert(0 && "NYI"); }

/* Trace exit handling. */
void lj_vm_exit_handler(void) { assert(0 && "NYI"); }
void lj_vm_exit_interp(void)  { assert(0 && "NYI"); }
void lj_vm_exit_interp_notrack(void) { assert(0 && "NYI"); }

/* Internal math helper functions. */
double lj_vm_floor(double a)             { assert(0 && "NYI"); }
double lj_vm_ceil(double a)              { assert(0 && "NYI"); }

void lj_vm_floor_sse(void)   { assert(0 && "NYI"); }
void lj_vm_ceil_sse(void)    { assert(0 && "NYI"); }
void lj_vm_trunc_sse(void)   { assert(0 && "NYI"); }
void lj_vm_powi_sse(void)    { assert(0 && "NYI"); }
double lj_vm_trunc(double d) { assert(0 && "NYI"); }

/* Continuations for metamethods. */
void lj_cont_cat(void)	  { assert(0 && "NYI"); }
void lj_cont_ra(void)	  { assert(0 && "NYI"); }
void lj_cont_nop(void)	  { assert(0 && "NYI"); }
void lj_cont_condt(void)  { assert(0 && "NYI"); }
void lj_cont_condf(void)  { assert(0 && "NYI"); }
void lj_cont_hook(void)	  { assert(0 && "NYI"); }
void lj_cont_stitch(void) { assert(0 && "NYI"); }

char lj_vm_asm_begin[0];
