/*
** Assembler VM interface definitions.
** Copyright (C) 2005-2017 Mike Pall. See Copyright Notice in luajit.h
*/

#ifndef _LJ_VM_H
#define _LJ_VM_H

#include "lj_obj.h"
#include "lj_ir.h"
#include <math.h>
#include <setjmp.h>

/* Entry points for VM. */
LJ_ASMF void lj_vm_call(lua_State *L, TValue *base, int nres1);
LJ_ASMF int lj_vm_pcall(lua_State *L, TValue *base, int nres1, ptrdiff_t ef);
typedef TValue *(*lua_CPFunction)(lua_State *L, lua_CFunction func, void *ud);
LJ_ASMF int lj_vm_cpcall(lua_State *L, lua_CFunction func, void *ud,
			 lua_CPFunction cp);
LJ_ASMF int lj_vm_resume(lua_State *L, TValue *base, int nres1, ptrdiff_t ef);
LJ_ASMF_NORET void lj_vm_unwind_c(void *cframe, int errcode);
LJ_ASMF_NORET void lj_vm_unwind_ff(void *cframe);
LJ_ASMF void lj_vm_unwind_c_eh(void);
LJ_ASMF void lj_vm_unwind_ff_eh(void);
LJ_ASMF void lj_vm_unwind_rethrow(void);

/* Miscellaneous functions. */
LJ_ASMF int lj_vm_cpuid(uint32_t f, uint32_t res[4]);
LJ_ASMF double lj_vm_foldfpm(double x, int op);
#if !LJ_ARCH_HASFPU
/* Declared in lj_obj.h: LJ_ASMF int32_t lj_vm_tobit(double x); */
#endif

/* Dispatch targets for recording and hooks. */
LJ_ASMF void lj_vm_record(void);
LJ_ASMF void lj_vm_inshook(void);
LJ_ASMF void lj_vm_rethook(void);
LJ_ASMF void lj_vm_callhook(void);

/* Trace entry and exit handling. See lj_vm_trace_call_*.asm */
LJ_ASMF void lj_vm_trace_call(void *tcs, void *mcode);
LJ_ASMF void lj_vm_exit_handler(void);
LJ_ASMF void lj_vm_exit_interp(void);
LJ_ASMF void lj_vm_exit_interp_notrack(void);

/* Internal math helper functions. */
static inline double lj_vm_floor(double a) { return floor(a); }
static inline double lj_vm_ceil(double a)  { return ceil(a); }
static inline double lj_vm_foldarith(double x, double y, int op) {
  switch (op) {
  case IR_ADD - IR_ADD: return x+y; break;
  case IR_SUB - IR_ADD: return x-y; break;
  case IR_MUL - IR_ADD: return x*y; break;
  case IR_DIV - IR_ADD: return x/y; break;
  case IR_MOD - IR_ADD: return x-lj_vm_floor(x/y)*y; break;
  case IR_POW - IR_ADD: return pow(x, y); break;
  case IR_NEG - IR_ADD: return -x; break;
  case IR_ABS - IR_ADD: return fabs(x); break;
  case IR_ATAN2 - IR_ADD: return atan2(x, y); break;
  case IR_LDEXP - IR_ADD: return ldexp(x, (int)y); break;
  case IR_MIN - IR_ADD: return x > y ? y : x; break;
  case IR_MAX - IR_ADD: return x < y ? y : x; break;
  default: return x;
  }
}
#ifdef LUAJIT_NO_LOG2
LJ_ASMF double lj_vm_log2(double);
#else
#define lj_vm_log2	log2
#endif
LJ_ASMF int32_t lj_vm_modi(int32_t, int32_t);

LJ_ASMF void lj_vm_floor_sse(void);
LJ_ASMF void lj_vm_ceil_sse(void);
LJ_ASMF void lj_vm_trunc_sse(void);
LJ_ASMF void lj_vm_powi_sse(void);
#define lj_vm_powi	NULL
LJ_ASMF double lj_vm_trunc(double);
#ifdef LUAJIT_NO_EXP2
LJ_ASMF double lj_vm_exp2(double);
#else
#define lj_vm_exp2	exp2
#endif
LJ_ASMF int lj_vm_errno(void);

/* Start of the ASM code. */
LJ_ASMF char lj_vm_text_begin[];

/* Bytecode offsets are relative to lj_vm_text_begin. */
#define makeasmfunc(ofs)	((ASMFunction)(lj_vm_text_begin + (ofs)))

/* VM registers and calling convention. */
#define LJ_VMF __attribute__((preserve_none))

struct lj_vm_fn_tag;
#define LJ_VM_FN_PARAM \
  lua_State *L, TValue *base, const void *kbase, \
  BCIns bc, const BCIns *pc, const struct lj_vm_fn_tag *vm

#define LJ_VM_FN_ARGS L, base, kbase, bc, pc, vm

typedef LJ_VMF void (*lj_vm_fn_t)(LJ_VM_FN_PARAM);

struct lj_vm_fn_tag { lj_vm_fn_t fn; };

#define lj_vm_fn(name) lj_vm_fn__##name
#define lj_vm_fn_declare(name) LJ_ASMF LJ_VMF void lj_vm_fn(name)(LJ_VM_FN_PARAM)
#define lj_vm_fn_call_f(fn) fn(LJ_VM_FN_ARGS)
#define lj_vm_fn_call(name) lj_vm_fn_call_f(lj_vm_fn(name))

/* Continuations for metamethods. */
lj_vm_fn_declare(cont_cat);  /* Continue with concatenation. */
lj_vm_fn_declare(cont_ra);  /* Store result in RA from instruction. */
lj_vm_fn_declare(cont_nop);  /* Do nothing, just continue execution. */
lj_vm_fn_declare(cont_condt);  /* Branch if result is true. */
lj_vm_fn_declare(cont_condf);  /* Branch if result is false. */
lj_vm_fn_declare(cont_hook);  /* Continue from hook yield. */
lj_vm_fn_declare(cont_stitch);  /* Trace stitching. */

/* Bytecode declarations. */
lj_vm_fn_declare(ISLT);
lj_vm_fn_declare(ISGE);
lj_vm_fn_declare(ISLE);
lj_vm_fn_declare(ISGT);
lj_vm_fn_declare(ISEQV);
lj_vm_fn_declare(ISNEV);
lj_vm_fn_declare(ISEQS);
lj_vm_fn_declare(ISNES);
lj_vm_fn_declare(ISEQN);
lj_vm_fn_declare(ISNEN);
lj_vm_fn_declare(ISEQP);
lj_vm_fn_declare(ISNEP);

lj_vm_fn_declare(ISTC);
lj_vm_fn_declare(ISFC);
lj_vm_fn_declare(IST);
lj_vm_fn_declare(ISF);
lj_vm_fn_declare(ISTYPE);
lj_vm_fn_declare(ISNUM);

lj_vm_fn_declare(MOV);
lj_vm_fn_declare(NOT);
lj_vm_fn_declare(UNM);
lj_vm_fn_declare(LEN);

lj_vm_fn_declare(ADDVN);
lj_vm_fn_declare(SUBVN);
lj_vm_fn_declare(MULVN);
lj_vm_fn_declare(DIVVN);
lj_vm_fn_declare(MODVN);

lj_vm_fn_declare(ADDNV);
lj_vm_fn_declare(SUBNV);
lj_vm_fn_declare(MULNV);
lj_vm_fn_declare(DIVNV);
lj_vm_fn_declare(MODNV);

lj_vm_fn_declare(ADDVV);
lj_vm_fn_declare(SUBVV);
lj_vm_fn_declare(MULVV);
lj_vm_fn_declare(DIVVV);
lj_vm_fn_declare(MODVV);
lj_vm_fn_declare(POW);

lj_vm_fn_declare(CAT);

lj_vm_fn_declare(KSTR);
lj_vm_fn_declare(KCDATA);
lj_vm_fn_declare(KSHORT);
lj_vm_fn_declare(KNUM);
lj_vm_fn_declare(KPRI);
lj_vm_fn_declare(KNIL);

lj_vm_fn_declare(UGET);
lj_vm_fn_declare(USETV);
lj_vm_fn_declare(USETS);
lj_vm_fn_declare(USETN);
lj_vm_fn_declare(USETP);
lj_vm_fn_declare(UCLO);
lj_vm_fn_declare(FNEW);

lj_vm_fn_declare(TNEW);
lj_vm_fn_declare(TDUP);
lj_vm_fn_declare(GGET);
lj_vm_fn_declare(GSET);
lj_vm_fn_declare(TGETV);
lj_vm_fn_declare(TGETS);
lj_vm_fn_declare(TGETB);
lj_vm_fn_declare(TGETR);
lj_vm_fn_declare(TSETV);
lj_vm_fn_declare(TSETS);
lj_vm_fn_declare(TSETB);
lj_vm_fn_declare(TSETM);
lj_vm_fn_declare(TSETR);

lj_vm_fn_declare(CALL);
lj_vm_fn_declare(CALLT);
lj_vm_fn_declare(ITERC);
lj_vm_fn_declare(ITERN);
lj_vm_fn_declare(VARG);
lj_vm_fn_declare(ISNEXT);

lj_vm_fn_declare(RETM);
lj_vm_fn_declare(RET);
lj_vm_fn_declare(RET0);
lj_vm_fn_declare(RET1);

lj_vm_fn_declare(FORI);
lj_vm_fn_declare(FORL);

lj_vm_fn_declare(ITERL);
lj_vm_fn_declare(JITERL);

lj_vm_fn_declare(LOOP);

lj_vm_fn_declare(JMP);

lj_vm_fn_declare(FUNCF);
lj_vm_fn_declare(FUNCV);
lj_vm_fn_declare(FUNCC);

/* Fast paths. */
lj_vm_fn_declare(assert);
lj_vm_fn_declare(type);
lj_vm_fn_declare(next);
lj_vm_fn_declare(pairs);
lj_vm_fn_declare(ipairs_aux);
lj_vm_fn_declare(ipairs);
lj_vm_fn_declare(getmetatable);
lj_vm_fn_declare(setmetatable);
lj_vm_fn_declare(rawget);
lj_vm_fn_declare(tonumber);
lj_vm_fn_declare(tostring);
lj_vm_fn_declare(pcall);
lj_vm_fn_declare(xpcall);

lj_vm_fn_declare(coroutine_yield);
lj_vm_fn_declare(coroutine_resume);

lj_vm_fn_declare(math_abs);
lj_vm_fn_declare(math_floor);
lj_vm_fn_declare(math_ceil);
lj_vm_fn_declare(math_sqrt);
lj_vm_fn_declare(math_log10); 
lj_vm_fn_declare(math_exp);
lj_vm_fn_declare(math_sin);
lj_vm_fn_declare(math_cos);
lj_vm_fn_declare(math_tan);

lj_vm_fn_declare(math_frexp);
lj_vm_fn_declare(math_modf);
lj_vm_fn_declare(math_log);
lj_vm_fn_declare(math_atan);

lj_vm_fn_declare(math_ldexp);
lj_vm_fn_declare(math_min);
lj_vm_fn_declare(math_max);

lj_vm_fn_declare(bit_tobit);
lj_vm_fn_declare(bit_bnot);
lj_vm_fn_declare(bit_bswap);
lj_vm_fn_declare(bit_lshift);
lj_vm_fn_declare(bit_rshift);
lj_vm_fn_declare(bit_arshift);
lj_vm_fn_declare(bit_rol);
lj_vm_fn_declare(bit_ror);
lj_vm_fn_declare(bit_band);
lj_vm_fn_declare(bit_bor);
lj_vm_fn_declare(bit_bxor);
lj_vm_fn_declare(string_byte);
lj_vm_fn_declare(string_char);
lj_vm_fn_declare(string_sub);
lj_vm_fn_declare(string_op);

lj_vm_fn_declare(NYI);

static lj_vm_fn_t lj_vm_dispatch[] = {

  /* Comparison ops. ORDER OPR. */
  lj_vm_fn(ISLT),
  lj_vm_fn(ISGE),
  lj_vm_fn(ISLE),
  lj_vm_fn(ISGT),
  lj_vm_fn(ISEQV),
  lj_vm_fn(ISNEV),
  lj_vm_fn(ISEQS),
  lj_vm_fn(ISNES),
  lj_vm_fn(ISEQN),
  lj_vm_fn(ISNEN),
  lj_vm_fn(ISEQP),
  lj_vm_fn(ISNEP),

  /* Unary test and copy ops. */
  lj_vm_fn(ISTC),
  lj_vm_fn(ISFC),
  lj_vm_fn(IST),
  lj_vm_fn(ISF),
  lj_vm_fn(ISTYPE),
  lj_vm_fn(ISNUM),

  /* Unary ops. */
  lj_vm_fn(MOV),
  lj_vm_fn(NOT),
  lj_vm_fn(UNM),
  lj_vm_fn(LEN),

  /* Binary ops. ORDER OPR. VV last, POW must be next. */
  lj_vm_fn(ADDVN),
  lj_vm_fn(SUBVN),
  lj_vm_fn(MULVN),
  lj_vm_fn(DIVVN),
  lj_vm_fn(MODVN),

  lj_vm_fn(ADDNV),
  lj_vm_fn(SUBNV),
  lj_vm_fn(MULNV),
  lj_vm_fn(DIVNV),
  lj_vm_fn(MODNV),
  
  lj_vm_fn(ADDVV),
  lj_vm_fn(SUBVV),
  lj_vm_fn(MULVV),
  lj_vm_fn(DIVVV),
  lj_vm_fn(MODVV),
  lj_vm_fn(POW),

  lj_vm_fn(CAT),

  /* Constant ops. */
  lj_vm_fn(KSTR),
  lj_vm_fn(KCDATA),
  lj_vm_fn(KSHORT),
  lj_vm_fn(KNUM),
  lj_vm_fn(KPRI),
  lj_vm_fn(KNIL),

  /* Upvalue and function ops. */
  lj_vm_fn(UGET),
  lj_vm_fn(USETV),
  lj_vm_fn(USETS),
  lj_vm_fn(USETN),
  lj_vm_fn(USETP),
  lj_vm_fn(UCLO),
  lj_vm_fn(FNEW),

  /* Table ops. */
  lj_vm_fn(TNEW),
  lj_vm_fn(TDUP),
  lj_vm_fn(GGET),
  lj_vm_fn(GSET),
  lj_vm_fn(TGETV),
  lj_vm_fn(TGETS),
  lj_vm_fn(TGETB),
  lj_vm_fn(TGETR),
  lj_vm_fn(TSETV),
  lj_vm_fn(TSETS),
  lj_vm_fn(TSETB),
  lj_vm_fn(TSETM),
  lj_vm_fn(TSETR),

  /* Calls and vararg handling. T = tail call. */
  lj_vm_fn(CALL),
  lj_vm_fn(CALL),
  lj_vm_fn(CALLT),
  lj_vm_fn(CALLT),
  lj_vm_fn(ITERC),
  lj_vm_fn(ITERN),
  lj_vm_fn(VARG),
  lj_vm_fn(ISNEXT),

  /* Returns. */
  lj_vm_fn(RETM),
  lj_vm_fn(RET),
  lj_vm_fn(RET0),
  lj_vm_fn(RET1),

  /* Loops and branches. I/J = interp/JIT, I/C/L = init/call/loop. */
  lj_vm_fn(FORI),
  lj_vm_fn(FORI),

  lj_vm_fn(FORL),
  lj_vm_fn(FORL),
  lj_vm_fn(FORL),

  lj_vm_fn(ITERL),
  lj_vm_fn(ITERL),
  lj_vm_fn(JITERL),

  lj_vm_fn(LOOP),
  lj_vm_fn(LOOP),
  lj_vm_fn(LOOP),

  lj_vm_fn(JMP),

  /* Function headers. I/J = interp/JIT, F/V/C = fixarg/vararg/C func. */
  lj_vm_fn(FUNCF),
  lj_vm_fn(FUNCF),
  lj_vm_fn(FUNCF),
  lj_vm_fn(FUNCV),
  lj_vm_fn(FUNCV),
  lj_vm_fn(FUNCV),
  lj_vm_fn(FUNCC),
  lj_vm_fn(FUNCC),

  /* Fast-path pseudo ops. */
  lj_vm_fn(assert),
  lj_vm_fn(type),
  lj_vm_fn(next),
  lj_vm_fn(pairs),
  lj_vm_fn(ipairs_aux),
  lj_vm_fn(ipairs),
  lj_vm_fn(getmetatable),
  lj_vm_fn(setmetatable),
  lj_vm_fn(rawget),
  lj_vm_fn(tonumber),
  lj_vm_fn(tostring),
  lj_vm_fn(pcall),
  lj_vm_fn(xpcall),

  lj_vm_fn(coroutine_yield),
  lj_vm_fn(coroutine_resume),
  lj_vm_fn(coroutine_resume),

  lj_vm_fn(math_abs),
  lj_vm_fn(math_floor),
  lj_vm_fn(math_ceil),
  lj_vm_fn(math_sqrt),
  lj_vm_fn(math_log10), 
  lj_vm_fn(math_exp),
  lj_vm_fn(math_sin),
  lj_vm_fn(math_cos),
  lj_vm_fn(math_tan),
  lj_vm_fn(NYI),
  lj_vm_fn(NYI),
  lj_vm_fn(NYI),
  lj_vm_fn(NYI),
  lj_vm_fn(NYI),
  lj_vm_fn(NYI),

  lj_vm_fn(math_frexp),
  lj_vm_fn(math_modf),
  lj_vm_fn(math_log),
  lj_vm_fn(math_atan),
  lj_vm_fn(NYI),
  lj_vm_fn(NYI),
  lj_vm_fn(math_ldexp),
  lj_vm_fn(math_min),
  lj_vm_fn(math_max),

  lj_vm_fn(bit_tobit),
  lj_vm_fn(bit_bnot),
  lj_vm_fn(bit_bswap),
  lj_vm_fn(bit_lshift),
  lj_vm_fn(bit_rshift),
  lj_vm_fn(bit_arshift),
  lj_vm_fn(bit_rol),
  lj_vm_fn(bit_ror),
  lj_vm_fn(bit_band),
  lj_vm_fn(bit_bor),
  lj_vm_fn(bit_bxor),
  lj_vm_fn(string_byte),
  lj_vm_fn(string_char),
  lj_vm_fn(string_sub),
  lj_vm_fn(string_op),
  lj_vm_fn(string_op),
  lj_vm_fn(string_op),
  lj_vm_fn(NYI)
};

#endif
