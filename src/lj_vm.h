/*
** Assembler VM interface definitions.
** Copyright (C) 2005-2017 Mike Pall. See Copyright Notice in luajit.h
*/

#ifndef _LJ_VM_H
#define _LJ_VM_H

#include "lj_obj.h"
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
LJ_ASMF double lj_vm_foldarith(double x, double y, int op);
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
LJ_ASMF double lj_vm_floor(double);
LJ_ASMF double lj_vm_ceil(double);
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

/* Continuations for metamethods. */
LJ_ASMF void lj_cont_cat(void);  /* Continue with concatenation. */
LJ_ASMF void lj_cont_ra(void);  /* Store result in RA from instruction. */
LJ_ASMF void lj_cont_nop(void);  /* Do nothing, just continue execution. */
LJ_ASMF void lj_cont_condt(void);  /* Branch if result is true. */
LJ_ASMF void lj_cont_condf(void);  /* Branch if result is false. */
LJ_ASMF void lj_cont_hook(void);  /* Continue from hook yield. */
LJ_ASMF void lj_cont_stitch(void);  /* Trace stitching. */

/* Start of the ASM code. */
LJ_ASMF char lj_vm_asm_begin[];

/* Bytecode offsets are relative to lj_vm_asm_begin. */
#define makeasmfunc(ofs)	((ASMFunction)(lj_vm_asm_begin + (ofs)))

#define VM_FUNC_PARAMS \
	BCIns curins, const BCIns *pc, void *disp, \
	lua_State *L, \
	unsigned int multres, unsigned int nargs, \
	void *kbase, \
	lua_State *cont_L, TValue *cont_base, ptrdiff_t cont_ofs

#define VM_FUNC_ARGS curins, pc, disp, L, multres, nargs, kbase, cont_L, cont_base, cont_ofs

typedef void (*lj_vm_func)(VM_FUNC_PARAMS);

#define VM_FUNC_(name) lj_vm_func_##name
#define VM_FUNC(name) void VM_FUNC_(name) (VM_FUNC_PARAMS)

VM_FUNC(IScc);
VM_FUNC(ISccV);
VM_FUNC(ISccS);
VM_FUNC(ISccN);
VM_FUNC(ISccP);

VM_FUNC(ISbC);
VM_FUNC(IST);
VM_FUNC(ISF);
VM_FUNC(ISTYPE);
VM_FUNC(ISNUM);

VM_FUNC(MOV);
VM_FUNC(NOT);
VM_FUNC(UNM);
VM_FUNC(LEN);

VM_FUNC(arithVN);
VM_FUNC(arithNV);
VM_FUNC(arithVV);
VM_FUNC(CAT);

VM_FUNC(KSTR);
VM_FUNC(KCDATA);
VM_FUNC(KSHORT);
VM_FUNC(KNUM);
VM_FUNC(KPRI);
VM_FUNC(KNIL);

VM_FUNC(UGET);
VM_FUNC(USETV);
VM_FUNC(USETS);
VM_FUNC(USETN);
VM_FUNC(USETP);
VM_FUNC(UCLO);
VM_FUNC(FNEW);

VM_FUNC(TNEW);
VM_FUNC(TDUP);
VM_FUNC(GGET);
VM_FUNC(GSET);
VM_FUNC(TGETV);
VM_FUNC(TGETS);
VM_FUNC(TGETB);
VM_FUNC(TGETR);
VM_FUNC(TSETV);
VM_FUNC(TSETS);
VM_FUNC(TSETB);
VM_FUNC(TSETM);
VM_FUNC(TSETR);

VM_FUNC(CALL);
VM_FUNC(CALLT);
VM_FUNC(ITERC);
VM_FUNC(ITERN);
VM_FUNC(VARG);
VM_FUNC(ISNEXT);

VM_FUNC(RETM);
VM_FUNC(RET);
VM_FUNC(RET0);
VM_FUNC(RET1);

VM_FUNC(FORI);
VM_FUNC(FORL);

VM_FUNC(ITERL);
VM_FUNC(JITERL);

VM_FUNC(LOOP);

VM_FUNC(JMP);

VM_FUNC(FUNCF);
VM_FUNC(FUNCV);
VM_FUNC(FUNCC);

VM_FUNC(assert);
VM_FUNC(type);
VM_FUNC(next);
VM_FUNC(pairs);
VM_FUNC(ipairs_aux);
VM_FUNC(ipairs);
VM_FUNC(getmetatable);
VM_FUNC(setmetatable);
VM_FUNC(rawget);
VM_FUNC(tonumber);
VM_FUNC(tostring);
VM_FUNC(pcall);
VM_FUNC(xpcall);

VM_FUNC(coroutine_yield);
VM_FUNC(coroutine_resume);

VM_FUNC(math_abs);
VM_FUNC(math_floor);
VM_FUNC(math_ceil);
VM_FUNC(math_sqrt);
VM_FUNC(math_log10); 
VM_FUNC(math_exp);
VM_FUNC(math_sin);
VM_FUNC(math_cos);
VM_FUNC(math_tan);

VM_FUNC(math_frexp);
VM_FUNC(math_modf);
VM_FUNC(math_log);
VM_FUNC(math_atan);

VM_FUNC(math_ldexp);
VM_FUNC(math_min);
VM_FUNC(math_max);

VM_FUNC(bit_tobit);
VM_FUNC(bit_bnot);
VM_FUNC(bit_bswap);
VM_FUNC(bit_lshift);
VM_FUNC(bit_rshift);
VM_FUNC(bit_arshift);
VM_FUNC(bit_rol);
VM_FUNC(bit_ror);
VM_FUNC(bit_band);
VM_FUNC(bit_bor);
VM_FUNC(bit_bxor);
VM_FUNC(string_byte);
VM_FUNC(string_char);
VM_FUNC(string_sub);
VM_FUNC(string_op);

VM_FUNC(NYI);

static lj_vm_func disp[] = {

  /* Comparison ops. ORDER OPR. */
  VM_FUNC_(IScc),
  VM_FUNC_(IScc),
  VM_FUNC_(IScc),
  VM_FUNC_(IScc),
  VM_FUNC_(ISccV),
  VM_FUNC_(ISccV),
  VM_FUNC_(ISccS),
  VM_FUNC_(ISccS),
  VM_FUNC_(ISccN),
  VM_FUNC_(ISccN),
  VM_FUNC_(ISccP),
  VM_FUNC_(ISccP),

  /* Unary test and copy ops. */
  VM_FUNC_(ISbC),
  VM_FUNC_(ISbC),
  VM_FUNC_(IST),
  VM_FUNC_(ISF),
  VM_FUNC_(ISTYPE),
  VM_FUNC_(ISNUM),

  /* Unary ops. */
  VM_FUNC_(MOV),
  VM_FUNC_(NOT),
  VM_FUNC_(UNM),
  VM_FUNC_(LEN),

  /* Binary ops. ORDER OPR. VV last, POW must be next. */
  VM_FUNC_(arithVN),
  VM_FUNC_(arithVN),
  VM_FUNC_(arithVN),
  VM_FUNC_(arithVN),
  VM_FUNC_(arithVN),

  VM_FUNC_(arithNV),
  VM_FUNC_(arithNV),
  VM_FUNC_(arithNV),
  VM_FUNC_(arithNV),
  VM_FUNC_(arithNV),
  
  VM_FUNC_(arithVV),
  VM_FUNC_(arithVV),
  VM_FUNC_(arithVV),
  VM_FUNC_(arithVV),
  VM_FUNC_(arithVV),
  
  VM_FUNC_(arithVV), // POW
  VM_FUNC_(CAT),

  /* Constant ops. */
  VM_FUNC_(KSTR),
  VM_FUNC_(KCDATA),
  VM_FUNC_(KSHORT),
  VM_FUNC_(KNUM),
  VM_FUNC_(KPRI),
  VM_FUNC_(KNIL),

  /* Upvalue and function ops. */
  VM_FUNC_(UGET),
  VM_FUNC_(USETV),
  VM_FUNC_(USETS),
  VM_FUNC_(USETN),
  VM_FUNC_(USETP),
  VM_FUNC_(UCLO),
  VM_FUNC_(FNEW),

  /* Table ops. */
  VM_FUNC_(TNEW),
  VM_FUNC_(TDUP),
  VM_FUNC_(GGET),
  VM_FUNC_(GSET),
  VM_FUNC_(TGETV),
  VM_FUNC_(TGETS),
  VM_FUNC_(TGETB),
  VM_FUNC_(TGETR),
  VM_FUNC_(TSETV),
  VM_FUNC_(TSETS),
  VM_FUNC_(TSETB),
  VM_FUNC_(TSETM),
  VM_FUNC_(TSETR),

  /* Calls and vararg handling. T = tail call. */
  VM_FUNC_(CALL),
  VM_FUNC_(CALL),
  VM_FUNC_(CALLT),
  VM_FUNC_(CALLT),
  VM_FUNC_(ITERC),
  VM_FUNC_(ITERN),
  VM_FUNC_(VARG),
  VM_FUNC_(ISNEXT),

  /* Returns. */
  VM_FUNC_(RETM),
  VM_FUNC_(RET),
  VM_FUNC_(RET0),
  VM_FUNC_(RET1),

  /* Loops and branches. I/J = interp/JIT, I/C/L = init/call/loop. */
  VM_FUNC_(FORI),
  VM_FUNC_(FORI),

  VM_FUNC_(FORL),
  VM_FUNC_(FORL),
  VM_FUNC_(FORL),

  VM_FUNC_(ITERL),
  VM_FUNC_(ITERL),
  VM_FUNC_(JITERL),

  VM_FUNC_(LOOP),
  VM_FUNC_(LOOP),
  VM_FUNC_(LOOP),

  VM_FUNC_(JMP),

  /* Function headers. I/J = interp/JIT, F/V/C = fixarg/vararg/C func. */
  VM_FUNC_(FUNCF),
  VM_FUNC_(FUNCF),
  VM_FUNC_(FUNCF),
  VM_FUNC_(FUNCV),
  VM_FUNC_(FUNCV),
  VM_FUNC_(FUNCV),
  VM_FUNC_(FUNCC),
  VM_FUNC_(FUNCC),

  /* Fast function pseudo ops. */
  VM_FUNC_(assert),
  VM_FUNC_(type),
  VM_FUNC_(next),
  VM_FUNC_(pairs),
  VM_FUNC_(ipairs_aux),
  VM_FUNC_(ipairs),
  VM_FUNC_(getmetatable),
  VM_FUNC_(setmetatable),
  VM_FUNC_(rawget),
  VM_FUNC_(tonumber),
  VM_FUNC_(tostring),
  VM_FUNC_(pcall),
  VM_FUNC_(xpcall),

  VM_FUNC_(coroutine_yield),
  VM_FUNC_(coroutine_resume),
  VM_FUNC_(coroutine_resume),

  VM_FUNC_(math_abs),
  VM_FUNC_(math_floor),
  VM_FUNC_(math_ceil),
  VM_FUNC_(math_sqrt),
  VM_FUNC_(math_log10), 
  VM_FUNC_(math_exp),
  VM_FUNC_(math_sin),
  VM_FUNC_(math_cos),
  VM_FUNC_(math_tan),
  VM_FUNC_(NYI),
  VM_FUNC_(NYI),
  VM_FUNC_(NYI),
  VM_FUNC_(NYI),
  VM_FUNC_(NYI),
  VM_FUNC_(NYI),

  VM_FUNC_(math_frexp),
  VM_FUNC_(math_modf),
  VM_FUNC_(math_log),
  VM_FUNC_(math_atan),
  VM_FUNC_(NYI),
  VM_FUNC_(NYI),
  VM_FUNC_(math_ldexp),
  VM_FUNC_(math_min),
  VM_FUNC_(math_max),

  VM_FUNC_(bit_tobit),
  VM_FUNC_(bit_bnot),
  VM_FUNC_(bit_bswap),
  VM_FUNC_(bit_lshift),
  VM_FUNC_(bit_rshift),
  VM_FUNC_(bit_arshift),
  VM_FUNC_(bit_rol),
  VM_FUNC_(bit_ror),
  VM_FUNC_(bit_band),
  VM_FUNC_(bit_bor),
  VM_FUNC_(bit_bxor),
  VM_FUNC_(string_byte),
  VM_FUNC_(string_char),
  VM_FUNC_(string_sub),
  VM_FUNC_(string_op),
  VM_FUNC_(string_op),
  VM_FUNC_(string_op),
  VM_FUNC_(NYI)
};

#endif
