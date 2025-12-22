/*
** RaptorJIT virtual machine bytecode interpreter.
** Copyright (C) 2018-2019 Luke Gorrie, Max Rottenkolber
** See Copyright Notice in luajit.h
*/

#include <assert.h>
#include <stdint.h>
#include <setjmp.h>
#include <stdio.h>
#include <math.h>
#include <byteswap.h>

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
#include "lj_trace.h"
#include "lj_err.h"
#include "lj_vm_tcs.h"

#define neg(n) (-1 - (n))
#define max(a,b) ((a)>(b) ? (a) : (b))
#define min(a,b) ((a)<(b) ? (a) : (b))

/* From lib_base.c: */
void lj_ffh_coroutine_wrap_err(lua_State *L, lua_State *co);


/* -- Virtual machine registers ------------------------------------------- */

/* The "registers" in the virtual machine are simply some important
 * values that instructions frequently  manipulate. This is a 
 * tail-calling interpreter so we keep the most important registers
 * loaded as the parameters of interpreter routines.
 * See LJ_VM_FN_PARAM and LJ_VM_FN_ARGS.
 * 
 * We provide macros to abstract over the way each register is
 * represented. We refer to each register by a NAME whether it is
 * represented by a parameter, or slot in the lua_State struct L,
 * etc.
 */

/* The dispatch table (VM) register points to a table of functions
 * pointers, one for each interpreter routine. VM+OP points to the
 * implementation of the current instructions bytecode operand.
 */
#define VM vm

/* Program counter (PC) register stores the address of the next
 * instruction to run after the current instruction finishes.
 *
 * Most instructions simply increment the program counter. Branch and
 * function call instructions load new values to make a jump.
 */
#define PC pc

/* Current bytecode instruction (BC) holds the currently executing
 * instruction. 
 */
#define BC bc

/* OP, A, B, C, and D refer to the opcode and operands of the current
 * instruction (BC). Exactly which of these registers contains a valid
 * value varies between different instructions.
 *
 * Note: Changing PC does not automatically update these registers.
 */
#define OP bc_op(BC)
#define A  bc_a(BC)
#define B  bc_b(BC)
#define C  bc_c(BC)
#define D  bc_d(BC)

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
#define BASE base

/* TOP (top stack frame) is the last valid stack slot.
 *
 * The values BASE..TOP are the local values within the stack frame
 * that are typically referenced as instruction operands. 
 */
#define TOP (L->top)

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
#define MULTRES multres

/* STATE describes what kind of code the virtual machine is running.
 *
 * STATE=<0 is a complemented value from the LJ_VMST enum e.g.
 * ~LJ_VMST_INTERP or ~LJ_VMST_GC or ~LJ_VMST_C.
 *
 * STATE>0 is the number of the trace whose machine code is running.
 */
#define STATE (G(L)->vmstate)

/* Register CONT_BASE holds the stack base of the meta-method to continue from.
 */
#define CONT_BASE (L->cont_base)


/* -- Utility functions --------------------------------------------------- */

/* Copy values from 'src' to 'dst' and fill missing values with nil.
 * Return pointer to element after the last one filled in dst.
 */
static inline TValue *copyTVs (lua_State *L, TValue *dst, TValue *src,
                               int need, int have) {
  int ncopy = min(need, have);
  int npad  = max(0, need - have);
  lj_assertL(need>=0, "need cannot be negative");
  lj_assertL(have>=0, "have cannot be negative");
  while (ncopy-- > 0) copyTV(L, dst++, src++);
  while (npad--  > 0) setnilV(dst++);
  return dst;
}

/* Return the nth constant TValue. */
#define ktv(n) ((TValue*)KBASE + n)

/* Return the nth constant GC object. */
#define kgc(n) *((const GCobj**)KBASE-1-n)

/* Reference the nth constant GC object with known type. */
#define kgcref(n, type) ((type *)kgc(n))

/* Branch to JMP:J. */
#define branchPC(offset) { PC += (int)offset - BCBIAS_J; }


/* -- Debugging ----------------------------------------------------------- */

// #define LUA_VM_DEBUG 1

#ifdef LUA_VM_DEBUG
/* Count of executed instructions for debugger prosperity. */
static volatile uint64_t insctr;

/* Offset for when to start tracing (as in "logging", this has nothing to do
 * with the tracing JIT whatsoever) bytecode execution when debugging the
 * interpreter.
 *
 * You can read the value of `insctr' at moments of interest from GDB and set
 * this variable accordingly to log the executed bytecodes from that point
 * onward.
 */
static volatile uint64_t insctr_tracefrom = UINT64_MAX;
static volatile uint64_t insctr_traceto = UINT64_MAX;

/* Bytecode names. */
const char *const bc_names[] = {
#define BCENUM(name, ma, mb, mc, mt)	#name,
BCDEF(BCENUM)
#undef BCENUM
  NULL
};

/* Debug utilities. */

static inline void printins (lua_State *L, const BCIns *PC) {
  BCIns BC = PC[0];
  printf("%-6lu %p %-6s OP=%-3x A=%-3d B=%-3d C=%-3d D=%-5d stackdepth=%-3ld%s\n",
    insctr, PC, OP < BC__MAX ? bc_names[OP] : "FF",
    OP, A, B, C, D, TOP - L->base,
    (G(L)->dispatchmode & DISPMODE_REC) ? " [rec]" : "");
}

void printstack(lua_State *L)
{
  int i;
  for (i = -2; i < L->top - L->base; i++) {
    TValue *v = L->base + i;
    printf("[%3d] %p 0x%lx %s\n", i, v, v->u64, lj_typename(v));
    fflush(stdout);
  }
}
void printupvalues(GCfuncL *parent)
{
  int i;
  for (i = 0; i < parent->nupvalues; i++) {
    TValue *v = parent->uvptr[i]->uv.v;
    printf("[%3d] %p 0x%lx %s\n", i, v, v->u64, lj_typename(v));
    fflush(stdout);
  }
}
void printsource(lua_State *L)
{
  lua_Debug ar;
  lua_getstack(L, 0, &ar);
  lua_getinfo(L, "nSl", &ar);
  printf("%s %s:%d\n", ar.what, ar.source, ar.currentline);
}

void printv(TValue *o)
{
  if (tvisnil(o))
    printf("nil\n");
  if (tvistrue(o))
    printf("true\n");
  if (tvisfalse(o))
    printf("false\n");
  if (tvisnumber(o))
    printf("%f\n", numV(o));
  if (tvisstr(o))
    printf("%s\n", strVdata(o));
}
#endif


/* Execute virtual machine instructions in a tail-recursive loop. */

#define routine __attribute__((noinline)) lj_vm_fn_declare
#define routine_inline __attribute__((always_inline)) lj_vm_fn_declare
#define tailcall __attribute__((musttail)) return
#define next(fn) lj_vm_fn_call(fn)
#define next_ptr(fn) lj_vm_fn_call_f(fn)

routine_inline(dispatch) {
#ifdef LUA_VM_DEBUG
  insctr++;
  if (insctr >= insctr_tracefrom && insctr <= insctr_traceto)
    printins(L, PC);
#endif
  BC = *PC++;
  tailcall next_ptr(VM[OP].fn);
}


/* -- Various auxiliary VM functions -------------------------------------- */

/* Save a pc to the active CFrame.
 *
 * Note: this needs to be called before calling out to external functions that
 * can throw Lua errors in order for them to be able to produce error messages.
 */
static inline void vm_savepc(lua_State *L, const BCIns *pc) {
  setcframe_pc(cframe_raw(L->cframe), pc);
}

/* Helper tobit function for bitops.
 *
 * Takes a Lua number `n' and produces a signed integer in the 32-bit result
 * range.
 */
static inline int32_t tobit(TValue *n) {
  static union {lua_Number n; uint64_t b;} bn;
  bn.n = n->n + 6755399441055744.0; /* 2^52+2^51 */
  return (int32_t)bn.b;
}


/* -- Call handling ------------------------------------------------------- */

static inline TValue *frame_callee(TValue *base) {
  return &base[-2];
}

static inline const BCIns **frame_link(TValue *base) {
  return (const BCIns **)&base[-1];
}

static inline const BCIns *frame(int ftp, ptrdiff_t delta) {
  return (const BCIns *)((delta << 3) + ftp);
}

static inline ptrdiff_t link_delta(const BCIns *link) {
  return (ptrdiff_t)link >> 3;
}

static inline intptr_t link_type(const BCIns *link) {
  return (intptr_t)link & FRAME_TYPE;
}

static inline intptr_t link_typep(const BCIns *link) {
  return (intptr_t)link & FRAME_TYPEP;
}

/* Call Lua function or callable object.
 *
 * Setup new BASE for callee frame, set NARGS, and construct frame link
 * according to frame type. Set PC to beginning of function or __call
 * metamethod. In the latter case, the "function" is inserted as the first
 * argument.
 *
 * Note: when the frame type (`ftp') is FRAME_LUA then the current PC is used
 * as the return bytecode. For other frame types a delta link is computed based
 * on the offset between BASE and `callbase'.
 */
routine(call_meta);

routine_inline(call) {
  TValue *f = frame_callee(BASE);
  if (!tvisfunc(f))
    tailcall next(call_meta);
  L->base = BASE;
  TOP = BASE + NARGS;
  PC = mref(funcV(f)->l.pc, BCIns);
  tailcall next(dispatch);
}

routine(call_meta) {
  vm_savepc(L, PC);
  lj_meta_call(L, frame_callee(BASE), BASE + NARGS);
  NARGS += 1;
  tailcall next(call);
}

/* Perform a tailcall.
 *
 * Copies function and arguments at offset into current (parent) frame and
 * performs call via vm_call.
 */

routine_inline(callt) {
  const BCIns *link = *frame_link(L->base); // Parent link
  if (link_typep(link) == FRAME_VARG) {
    /* Frame below is a VARG frame, relocate BASE. */
    L->base -= link_delta(link);
    link = *frame_link(L->base);
  }
  // Copy function and arguments down into parent frame.
  *frame_callee(L->base) = *frame_callee(BASE);
  copyTVs(L, L->base, BASE, NARGS, NARGS);
  BASE = L->base;
  *frame_link(BASE) = link;
  tailcall next(call);
}


/* -- Return handling ----------------------------------------------------- */

/* Return to the previous frame.
 *
 * PC is the frame link (which in case of FRAME_LUA is the return PC).
 * BASE points to return values.
 * MULTRES is the number of values to return.
 *
 * Ensures that...
 *   BASE is restored to the base frame of the previous stack frame.
 *   Return values are copied to BASE..BASE+NRESULTS+MULTRES.
 *
 * Returns true if the virtual machine should 'return' on the C stack
 * i.e. if we are returning from this invocation of the bytecode interpreter.
 */

routine_inline(return);

routine(return_protected) {
  switch (link_typep(PC)) {
  case FRAME_PCALL:
  case FRAME_PCALLH:
    /* Return from protected call: signal success to caller.
       (See lj_unwind_ff for unwinding from failed pcalls.)
     
       Pop pcall frame, and adjust L->base accordingly.

       Decrement BASE by one, and increment MULTRES by one.
       Push TRUE for successful return from a pcall in front of results.
       (We know there is space because we freed at least two slots from
       the pcall frame.)

       Return from call frame with the adjusted BASE/MULTRES. */
    L->base -= link_delta(PC);
    PC = *frame_link(L->base); /* Might be clobbered. */
    BASE--; MULTRES++; setboolV(BASE, 1);
    tailcall next(return);
  default:
    assert(0 && "NYI");
  }
}

routine(return_continuation) {
  if (link_typep(PC) != FRAME_CONT)
    tailcall next(return_protected);
  /* Return from metamethod continuation frame: restore PC and caller frame.
     The results (if any) of the continuation start at BASE.
     The MULTRES register is used to pass the number of available results.
      
     Note the FRAME_CONT layout:
                                              L->base
          -4       -3         -2         -1         0       1
      | <cont> |  <pc>  |<metamethod>| <link> | <arg1> |   ...
  */
  GCproto *pt;
  int delta = link_delta(PC);
  lj_vm_fn_t cont = contptr(L->base[-4].u64);
  PC = mref(L->base[-3].u64, BCIns);
  L->base -= delta;
  if ((intptr_t)cont == LJ_CONT_TAILCALL) {
    /* Tail call from C function. */
    BASE = L->base;
    tailcall next(callt);
  } else if ((intptr_t)cont == LJ_CONT_FFI_CALLBACK) {
    /* Return from FFI callback. */
    assert(0 && "NYI: vm_return from FFI callback.");
  } else {
    /* Call continuation. */
    CONT_BASE = L->base + delta;
    pt = funcproto(funcV(frame_callee(L->base)));
    TOP = L->base + pt->framesize;
    KBASE = mref(pt->k, void);
    tailcall next_ptr((*cont));
  }
}

routine(return_cframe) {
  if (link_type(PC) != FRAME_C)
    tailcall next(return_continuation);
  /* Returning from the VM to C code. */
  CFrame *cf = cframe_raw(L->cframe);
  int nexpected = cf->nresults;
  STATE = ~LJ_VMST_C;
  if (nexpected < 0) // Return all results.
    nexpected = MULTRES;
  /* Copy results into caller frame */
  // When returning from C frames, last result is the new TOP.
  TOP = copyTVs(L, frame_callee(L->base), BASE, nexpected, MULTRES);
  BASE = L->base -= link_delta(PC);
  return;
}

routine(return_varg) {
  if (link_type(PC) != FRAME_VARG)
    tailcall next(return_cframe);
  /* Return from vararg function: relocate L->base down. */
  L->base -= link_delta(PC);
  PC = *frame_link(L->base);
  tailcall next(return);
}

routine_inline(return) {
  if (link_type(PC) != FRAME_LUA)
    tailcall next(return_varg);
  /* Return from a Lua function. */
  // Find details in caller's CALL instruction operands.
  int delta = bc_a(PC[-1]);
  int nexpected = bc_b(PC[-1]) - 1;
  GCproto *pt;
  if (nexpected < 0) // Return all results.
    nexpected = MULTRES;
  copyTVs(L, frame_callee(L->base), BASE, nexpected, MULTRES);
  BASE = L->base = frame_callee(L->base) - delta;
  pt = funcproto(funcV(frame_callee(BASE)));
  TOP = BASE + pt->framesize;
  KBASE = mref(pt->k, void);
  tailcall next(dispatch);
}


/* -- Calling metamethod continuations ------------------------------------ */

/* Push continuation frame and call metamethod at `newbase'.
 * See vm_return for handling of FRAME_CONT.
 */
routine(continuation) {
  *frame_link(frame_callee(BASE)) = PC;
  *frame_link(BASE) = frame(FRAME_CONT, BASE - L->base);
  tailcall next(call);
}


/* -- JIT trace recorder -------------------------------------------------- */

/* Re-dispatch to static instruction. */
routine_inline(redispatch) {
  // PC points to next instruction, and current instruction might
  // have been patched, so reload BC.
  BC = PC[-1];
  // Re-dispatch to static ins.
  tailcall next_ptr(VM[OP+GG_LEN_DDISP].fn);
}

/* Dispatch target for recording phase. */
routine(record) {
  // XXX - handle HOOK_VMEVENT, HOOK_ACTIVE
  /* NB: cframe->multres is used by lj_dispatch_ins. */
  ((CFrame *) cframe_raw(L->cframe))->multres = MULTRES + 1;
  lj_dispatch_ins(L, PC);
  BASE = L->base;
  tailcall next(redispatch);
}

/* Hot loop detection. */
static inline int hotloop(lua_State *L, const BCIns *pc) {
  HotCount old_count = hotcount_get(L2GG(L), pc);
  HotCount new_count = hotcount_set(L2GG(L), pc, old_count - HOTCOUNT_LOOP);
  return new_count > old_count; /* Hot loop counter underflow. */
}

/* Invoke trace recorder for hot loop body. */
routine(hotloop) {
  vm_savepc(L, PC);
  jit_State *J = L2J(L);
  J->L = L;
  lj_trace_hot(J, PC);
  BASE = L->base; // needed?
  tailcall next(redispatch);
}

/* Dispatch to call. */
routine_inline(dispatch_call) {
  ASMFunction fn = lj_dispatch_call(L, PC);
  vm_savepc(L, 0); // Invalidate for subsequent line hook.
  PC = (BCIns *)((uintptr_t)PC & -2); // Strip hot call marker.
  BASE = L->base;
  NARGS = L->top - L->base; // needed?
  tailcall next_ptr(((lj_vm_fn_t)fn));
}

/* Dispatch target for call hooks. */
routine(hook_call) {
  vm_savepc(L, PC);
  tailcall next(dispatch_call);
}

/* Hot call detection. */
static inline int hotcall(lua_State *L, const BCIns *PC) {
  HotCount old_count = hotcount_get(L2GG(L), PC);
  HotCount new_count = hotcount_set(L2GG(L), PC, old_count - HOTCOUNT_CALL);
  return new_count > old_count; /* Hot loop counter underflow. */
}

/* Invoke trace recorder for hot function. */
routine(hotcall) {
  vm_savepc(L, PC);
  TOP = BASE + NARGS;
  PC = (BCIns *)((uintptr_t)PC | 1); /* LSB set: marker for hot call. */
  tailcall next(dispatch_call);
}

/* Execute a JIT compiled machine code trace.
 *
 * Sets up the global context state needed for trace execution, and
 * synchronizes trace machine code and bytecode interpreter VM via
 * TraceCallState.
 *
 * Can either rethrow an error returned from the trace, or continue
 * interpreter execution with new PC/BASE.
 */
routine(exec_trace) {
  BCReg traceno = D;
  jit_State *J = L2J(L);
  GCtrace *trace = gcrefp(J->trace[traceno], GCtrace);
  TraceCallState tcs = {};
  int status;
  /* Setup trace context. */
  J2G(J)->jit_base = BASE;
  J2G(J)->tmpbuf.L = L;
  J2GG(J)->tcs = &tcs;
  tcs.state.gpr[GPR_DISPATCH] = (intptr_t)VM;
  tcs.state.gpr[GPR_BASE] = (intptr_t)BASE;
  /* Call JIT compiled trace with call state. */
  lj_vm_trace_call(&tcs, trace->mcode);
  /* Handle trace exit. */
  if (tcs.handler != TRACE_EXIT_INTERP_NOTRACK) {
    /* Record which trace exited to the interpreter. */
    J2G(J)->lasttrace = STATE;
  }
  /* Restore interpreter state. */
  if (tcs.handler == TRACE_EXIT) {
    J->L = L;
    J->parent = STATE;
    J->exitno = tcs.exitno;
    STATE = ~LJ_VMST_EXIT;
    L->base = BASE = J2G(J)->jit_base;
    J2G(J)->jit_base = NULL;
    status = lj_trace_exit(J, &tcs.state);
    PC = cframe_pc(cframe_raw(L->cframe)); // set by lj_trace_exit
  } else {
    L->base = BASE = (TValue*)tcs.state.gpr[GPR_BASE];
    status = (int)tcs.state.gpr[GPR_RET];
    PC = (BCIns*)tcs.state.gpr[GPR_PC];
  }
  /* Restore NARGS, MULTRES. */
  if (status > 0) {
    /* Status is MULTRES+1. */
    NARGS = MULTRES = status-1;
  } else if (status < 0) {
    /* Error returned from trace, rethrow from the right C frame. */
    lj_err_throw(L, -status);
  }
  /* Restore KBASE. */
  GCfunc *fn = funcV(frame_callee(BASE));
  if (isluafunc(fn)) {
    KBASE = mref(funcproto(fn)->k, void);
  } else {
    const BCIns *link = *frame_link(BASE);
    if (link_type(link) == FRAME_LUA) {
      /* Set KBASE for Lua function below. */
      ptrdiff_t delta = bc_a(link[-1]);
      fn = funcV(frame_callee(frame_callee(BASE)-delta));
      KBASE = mref(funcproto(fn)->k, void);
    } else {
      /* Trace stitching continuation. */
      assert(link_typep(link) == FRAME_CONT);
    }
  }
  /* Return to interpreter. */
  J2G(J)->jit_base = NULL;
  STATE = ~LJ_VMST_INTERP;
  tailcall next(dispatch);
}

/* Trace stitching continuation. */
routine(cont_stitch) {
  jit_State *J = L2J(L);
  BC = PC[-1];
  GCtrace *prev = (GCtrace *)gcV(CONT_BASE-5);
  TValue *callbase = L->base + A;
  /* Copy results. */
  copyTVs(L, callbase, BASE, B-1, MULTRES);
  BASE = L->base;
  /* Have a trace, and it is not blacklisted? */
  if (prev && prev->traceno != prev->link) {
    if (prev->link) {
      /* Jump to stitched trace. */
      BC = BCINS_AD(OP, A, prev->link);
      tailcall next(exec_trace);
    } else {
      /* Stitch a new trace to the previous trace. */
      J->L = L;
      J->exitno = prev->traceno;
      /* NB: cframe->multres is used by lj_dispatch_stitch. */
      ((CFrame *) cframe_raw(L->cframe))->multres = MULTRES + 1;
      lj_dispatch_stitch(J, PC);
      BASE = L->base; // needed?
    }
  }
  tailcall next(dispatch);
}


/* -- Continuations for metamethods ---------------------------------------- */

routine(cont_cat) {
  /* Continue with concatenation. */
  BC = PC[-1];
  int left = (CONT_BASE-4) - (L->base+B);
  if (left > 0) {
    /* CAT has remaining arguments, concatenate. */
    copyTVs(L, CONT_BASE-4, BASE, 1, MULTRES);
    TValue *mbase = lj_meta_cat(L, CONT_BASE-4, left);
    if (mbase) {
      NARGS = 2;
      BASE = mbase;
      tailcall next(continuation);
    } else {
      BASE = L->base;
      copyTV(L, BASE+A, BASE+B);
      tailcall next(dispatch);
    }
  } else {
    /* CAT is complete, store result. */
    tailcall next(cont_ra);
  }
}

routine(cont_ra) {
  /* Store result in A from invoking instruction. */
  BC = PC[-1];
  copyTVs(L, L->base+A, BASE, 1, MULTRES);
  BASE = L->base;
  tailcall next(dispatch);
}

routine(cont_nop) {
  /* Do nothing, just continue execution. */
  BASE = L->base;
  tailcall next(dispatch);
}

routine(cont_condt) {
  /* Branch if result is true. */
  int flag = MULTRES && tvistruecond(BASE);
  BASE = L->base;
  BC = *PC++;
  if (flag) branchPC(D);
  tailcall next(dispatch);
}

routine(cont_condf) {
  /* Branch if result is false. */
  int flag = !(MULTRES && tvistruecond(BASE));
  BASE = L->base;
  BC = *PC++;
  if (flag) branchPC(D);
  tailcall next(dispatch);
}

routine(cont_hook) {
  assert(0 && "NYI");
}


/* -- Bytecode implementations. --------------------------------------------*/

static inline int lj_vm_compare (double x, double y, int op) {
  /* Compare two floats.
   *
   * Note: to preserve NaN semantics GE/GT branch on unordered, but LT/LE
   * don't.
   */
  switch (op) {
    case BC_ISLT: return x < y;
    case BC_ISGE: return x >= y || isnan(x) || isnan(y);
    case BC_ISLE: return x <= y;
    case BC_ISGT: return x > y || isnan(x) || isnan(y);
    default: assert(0 && "NYI");
  }
}

routine(meta_compare) {
  vm_savepc(L, PC);
  TValue *res = lj_meta_comp(L, BASE+A, BASE+D, OP);
  if ((intptr_t)res > 1) {
    NARGS = 2;
    BASE = res;
    tailcall next(continuation);
  }
  BC = *PC++; /* Advance to jump instruction. */
  if (res) branchPC(D);
  tailcall next(dispatch);
}

routine(ISLT) {
  /* ISLT: Take following JMP instruction if A < D. */
  if (!tvisnum(BASE+A) || !tvisnum(BASE+D))
    tailcall next(meta_compare);
  double x = BASE[A].n, y = BASE[D].n;
  BC = *PC++; /* Advance to jump instruction. */
  if (lj_vm_compare(x, y, BC_ISLT))
    branchPC(D);
  tailcall next(dispatch);
}

routine(ISGE) {
  /* ISGE: Take following JMP instruction if A >= D. */
  if (!tvisnum(BASE+A) || !tvisnum(BASE+D))
    tailcall next(meta_compare);
  double x = BASE[A].n, y = BASE[D].n;
  BC = *PC++; /* Advance to jump instruction. */
  if (lj_vm_compare(x, y, BC_ISGE))
    branchPC(D);
  tailcall next(dispatch);
}

routine(ISLE) {
  /* ISLE: Take following JMP instruction if A <= D. */
  if (!tvisnum(BASE+A) || !tvisnum(BASE+D))
    tailcall next(meta_compare);
  double x = BASE[A].n, y = BASE[D].n;
  BC = *PC++; /* Advance to jump instruction. */
  if (lj_vm_compare(x, y, BC_ISLE))
    branchPC(D);
  tailcall next(dispatch);
}

routine(ISGT) {
  /* ISGT: Take following JMP instruction if A > D. */
  if (!tvisnum(BASE+A) || !tvisnum(BASE+D))
    tailcall next(meta_compare);
  double x = BASE[A].n, y = BASE[D].n;
  BC = *PC++; /* Advance to jump instruction. */
  if (lj_vm_compare(x, y, BC_ISGT))
    branchPC(D);
  tailcall next(dispatch);
}

routine(meta_equal) {
  vm_savepc(L, (BCIns*)((intptr_t)PC-4));
  TValue *res = lj_meta_equal(L, gcval(BASE+A), gcval(BASE+D), OP == BC_ISNEV);
  if ((intptr_t)res > 1) {
    NARGS = 2;
    BASE = res;
    tailcall next(continuation);
  }
  BC = *PC++; /* Advance to jump instruction. */
  if (res) branchPC(D);
  tailcall next(dispatch);
}

routine(meta_equal_cd) {
  vm_savepc(L, (BCIns*)((intptr_t)PC-4));
  TValue *res = lj_meta_equal_cd(L, BC);
  if ((intptr_t)res > 1) {
    NARGS = 2;
    BASE = res;
    tailcall next(continuation);
  }
  BC = *PC++; /* Advance to jump instruction. */
  if (res) branchPC(D);
  tailcall next(dispatch);
}

routine(equal_distinct) {
  if (itype(BASE+A) <= LJ_TISTABUD)
    // Different tables or userdatas. Need to check __eq metamethod.
    tailcall next(meta_equal);
  // Distinct objects
  int flag = (OP == BC_ISNEV);
  BC = *PC++; /* Advance to jump instruction. */
  if (flag) branchPC(D);
  tailcall next(dispatch);
}

routine(equal_diverse) {
  if (itype(BASE+A) == itype(BASE+D))
    tailcall next(equal_distinct);
  // Not the same type
  int flag = (OP == BC_ISNEV);
  BC = *PC++; /* Advance to jump instruction. */
  if (flag) branchPC(D);
  tailcall next(dispatch);
}

routine(equal_identical) {
  if (BASE[A].u64 != BASE[D].u64)
    tailcall next(equal_diverse);
  // Same GCobjs or pvalues
  int flag = (OP == BC_ISEQV);
  BC = *PC++; /* Advance to jump instruction. */
  if (flag) branchPC(D);
  tailcall next(dispatch);
}

routine(equal_object) {
  if (!tviscdata(BASE+A) && !tviscdata(BASE+D))
    tailcall next(equal_identical);
  // Either object is cdata.
  tailcall next(meta_equal_cd);
}

routine(ISEQV) {
  /* ISEQV: Take following JMP instruction if A is equal to D. */
  const TValue *x = BASE+A, *y = BASE+D;
  if (!tvisnum(x) || !tvisnum(y))
    tailcall next(equal_object);
  // Both are numbers.
  BC = *PC++; /* Advance to jump instruction. */
  if (x->n == y->n) branchPC(D);
  tailcall next(dispatch);
}

routine(ISNEV) {
  /* ISNEV: Take following JMP instruction if A is not equal to D. */
  const TValue *x = BASE+A, *y = BASE+D;
  if (!tvisnum(x) || !tvisnum(y))
    tailcall next(equal_object);
  // Both are numbers.
  BC = *PC++; /* Advance to jump instruction. */
  if (x->n != y->n) branchPC(D);
  tailcall next(dispatch);
}

routine(equal_cdata) {
  if (tviscdata(BASE+A))
    tailcall next(meta_equal_cd);
  // A is not cdata, not a number, and hence not equal to D.
  int flag = OP & 1;
  BC = *PC++; /* Advance to jump instruction. */
  if (flag) branchPC(D);
  tailcall next(dispatch);
}

routine(ISEQS) {
  /* ISEQS: Take following JMP instruction if A is equal to string D. */
  if (!tvisstr(BASE+A))
    tailcall next(equal_cdata);
  // A is a string
  int flag = (strV(BASE+A) == kgcref(D, GCstr));
  BC = *PC++; /* Advance to jump instruction. */
  if (flag) branchPC(D);
  tailcall next(dispatch);
}

routine(ISNES) {
  /* ISNES: Take following JMP instruction if A is not equal to string D. */
  if (!tvisstr(BASE+A))
    tailcall next(equal_cdata);
  // A is a string
  int flag = (strV(BASE+A) != kgcref(D, GCstr));
  BC = *PC++; /* Advance to jump instruction. */
  if (flag) branchPC(D);
  tailcall next(dispatch);
}

routine(ISEQN) {
  /* ISEQN: Take following JMP if A is equal to number constant D. */
  if (!tvisnum(BASE+A))
    tailcall next(equal_cdata);
  // A is a number
  int flag = (numV(BASE+A) == numV(ktv(D)));
  BC = *PC++; /* Advance to jump instruction. */
  if (flag) branchPC(D);
  tailcall next(dispatch);
}

routine(ISNEN) {
  /* ISNEN: Take following JMP if A is not equal to number constant D. */
  if (!tvisnum(BASE+A))
    tailcall next(equal_cdata);
  // A is a number
  int flag = (numV(BASE+A) != numV(ktv(D)));
  BC = *PC++; /* Advance to jump instruction. */
  if (flag) branchPC(D);
  tailcall next(dispatch);
}

routine(ISEQP) {
  /* ISEQP: Take following JMP if A is equal to primtive D.*/
  if (itype(BASE+A) != ~D)
    tailcall next(equal_cdata);
  // Type of A matches D whch is one of nil/false/true
  BC = *PC++; /* Advance to jump instruction. */
  branchPC(D);
  tailcall next(dispatch);
}

routine(ISNEP) {
  /* ISNEP: Take following JMP unless A is equal to primtive D.*/
  if (itype(BASE+A) != ~D)
    tailcall next(equal_cdata);
  // Type of A matches D whch is one of nil/false/true
  PC++; /* Skip jump instruction. */
  tailcall next(dispatch);
}

routine(ISTC) {
  /* ISFC: Copy D to A and take following JMP instruction if D is false. */
  BASE[A] = BASE[D];
  int flag = tvistruecond(BASE+D);
  BC = *PC++;
  if (flag) branchPC(D);
  tailcall next(dispatch);
}

routine(ISFC) {
  /* ISFC: Copy D to A and take following JMP instruction if D is false. */
  BASE[A] = BASE[D];
  int flag = !tvistruecond(BASE+D);
  BC = *PC++;
  if (flag) branchPC(D);
  tailcall next(dispatch);
}

routine(IST) {
  /* IST: Take following JMP instruction if D is true. */
  int flag = tvistruecond(BASE+D);
  /* Advance to jump instruction. */
  BC = *PC++;
  if (flag) branchPC(D);
  tailcall next(dispatch);
}

routine(ISF) {
  /* ISF: Take following JMP instruction if D is false. */
  int flag = !tvistruecond(BASE+D);
  /* Advance to jump instruction. */
  BC = *PC++;
  if (flag) branchPC(D);
  tailcall next(dispatch);
}

routine(meta_istype) {
  vm_savepc(L, PC);
  lj_meta_istype(L, A, D);
  tailcall next(dispatch);
}

routine(ISTYPE) {
  /* ISTYPE: assert A is of type -D. */
  if (itype(BASE+A) != -D)
    tailcall next(meta_istype);
  tailcall next(dispatch);
}

routine(ISNUM) {
  /* ISNUM: assert A is a number. */
  if (!tvisnum(BASE+A))
    tailcall next(meta_istype);
  tailcall next(dispatch);
}

routine(MOV) {
  /* MOV: A = dst; D = src */
  copyTV(L, BASE+A, BASE+D);
  tailcall next(dispatch);
}

routine(NOT) {
  /* NOT: Set A to boolean not of D. */
  setboolV(BASE+A, !tvistruecond(BASE+D));
  tailcall next(dispatch);
}

routine(meta_arith_V) {
  vm_savepc(L, PC);
  TValue *mbase = lj_meta_arith(L, BASE+A, BASE+D, BASE+D, OP);
  if (!mbase)
    tailcall next(dispatch);
  NARGS = 2;
  BASE = mbase;
  tailcall next(continuation);
}

routine(UNM) {
  /* UNM: Set A to -D (unary minus). */
  if (!tvisnum(BASE+D))
    tailcall next(meta_arith_V);
  BASE[A].n = lj_vm_foldarith(BASE[D].n, BASE[D].n, MM_unm-MM_add);
  tailcall next(dispatch);
}

routine(meta_len) {
    BASE = lj_meta_len(L, BASE+D);
    NARGS = 1;
    tailcall next(continuation);
}

routine(table_len) {
     setnumV(BASE+A, lj_tab_len(tabV(BASE+D)));
      tailcall next(dispatch);
}

routine(LEN) {
  /* LEN: Set A to #D (object length). */
  if (tvisstr(BASE+D)) {
    setnumV(BASE+A, strV(BASE+D)->len);
    tailcall next(dispatch);
  }
  if (tvistab(BASE+D)) {
    if (!LJ_52)
      /* Lua 5.1 does not support __len on tables. */
      tailcall next(table_len);
    if (!tabV(BASE+D)->metatable || (tabV(BASE+D)->nomm & MM_len))
      tailcall next(table_len);
  }
  tailcall next(meta_len);
}

routine(meta_arith_VN) {
  vm_savepc(L, PC);
  TValue *mbase = lj_meta_arith(L, BASE+A, BASE+B, ktv(C), OP);
  if (!mbase)
    tailcall next(dispatch);
  NARGS = 2;
  BASE = mbase;
  tailcall next(continuation);
}

routine(ADDVN) {
  /* ADDVN: Add number constant C to B and store the result in A. */
  if (!tvisnum(BASE+B))
    tailcall next(meta_arith_VN);
  BASE[A].n = lj_vm_foldarith(BASE[B].n, ktv(C)->n, MM_add-MM_add);
  tailcall next(dispatch);
}

routine(SUBVN) {
  /* SUBVN: Subtract number constant C from B and store the result in A. */
  if (!tvisnum(BASE+B))
    tailcall next(meta_arith_VN);
  BASE[A].n = lj_vm_foldarith(BASE[B].n, ktv(C)->n, MM_sub-MM_add);
  tailcall next(dispatch);
}

routine(MULVN) {
  /* MULVN: Multiply B by number constant C and store the result in A. */
  if (!tvisnum(BASE+B))
    tailcall next(meta_arith_VN);
  BASE[A].n = lj_vm_foldarith(BASE[B].n, ktv(C)->n, MM_mul-MM_add);
  tailcall next(dispatch);
}

routine(DIVVN) {
  /* DIVVN: Divide B by number constant C and store the result in A. */
  if (!tvisnum(BASE+B))
    tailcall next(meta_arith_VN);
  BASE[A].n = lj_vm_foldarith(BASE[B].n, ktv(C)->n, MM_div-MM_add);
  tailcall next(dispatch);
}

routine(MODVN) {
  /* MODVN: Calculate B modulo number constant C and store the result in A. */
  if (!tvisnum(BASE+B))
    tailcall next(meta_arith_VN);
  BASE[A].n = lj_vm_foldarith(BASE[B].n, ktv(C)->n, MM_mod-MM_add);
  tailcall next(dispatch);
}

routine(meta_arith_NV) {
  vm_savepc(L, PC);
  TValue *mbase = lj_meta_arith(L, BASE+A, ktv(C), BASE+B, OP);
  if (!mbase)
    tailcall next(dispatch);
  NARGS = 2;
  BASE = mbase;
  tailcall next(continuation);
}

routine(ADDNV) {
  /* ADDNV: Add B to number constant C and store the result in A. */
  if (!tvisnum(BASE+B))
    tailcall next(meta_arith_NV);
  BASE[A].n = lj_vm_foldarith(ktv(C)->n, BASE[B].n, MM_add-MM_add);
  tailcall next(dispatch);
}

routine(SUBNV) {
  /* SUBNV: Subtract B from number constant C and store the result in A. */
  if (!tvisnum(BASE+B))
    tailcall next(meta_arith_NV);
  BASE[A].n = lj_vm_foldarith(ktv(C)->n, BASE[B].n, MM_sub-MM_add);
  tailcall next(dispatch);
}

routine(MULNV) {
  /* MULNV: Multiply number constant C by B and store the result in A. */
  if (!tvisnum(BASE+B))
    tailcall next(meta_arith_NV);
  BASE[A].n = lj_vm_foldarith(ktv(C)->n, BASE[B].n, MM_mul-MM_add);
  tailcall next(dispatch);
}

routine(DIVNV) {
  /* DIVNV: Divide number constant C by B and store the result in A. */
  if (!tvisnum(BASE+B))
    tailcall next(meta_arith_NV);
  BASE[A].n = lj_vm_foldarith(ktv(C)->n, BASE[B].n, MM_div-MM_add);
  tailcall next(dispatch);
}

routine(MODNV) {
  /* MODNV: Calculate number constant C modulo B and store the result in A. */
  if (!tvisnum(BASE+B))
    tailcall next(meta_arith_NV);
  BASE[A].n = lj_vm_foldarith(ktv(C)->n, BASE[B].n, MM_mod-MM_add);
  tailcall next(dispatch);
}

routine(meta_arith_VV) {
  vm_savepc(L, PC);
  TValue *mbase = lj_meta_arith(L, BASE+A, BASE+B, BASE+C, OP);
  if (!mbase)
    tailcall next(dispatch);
  NARGS = 2;
  BASE = mbase;
  tailcall next(continuation);
}

routine(ADDVV) {
  /* ADDVV: Add C to B and store the result in A. */
  if (!tvisnum(BASE+B) || !tvisnum(BASE+C))
    tailcall next(meta_arith_VV);
  BASE[A].n = lj_vm_foldarith(BASE[B].n, BASE[C].n, MM_add-MM_add);
  tailcall next(dispatch);
}

routine(SUBVV) {
  /* SUBVV: Subtract C from B and store the result in A. */
  if (!tvisnum(BASE+B) || !tvisnum(BASE+C))
    tailcall next(meta_arith_VV);
  BASE[A].n = lj_vm_foldarith(BASE[B].n, BASE[C].n, MM_sub-MM_add);
  tailcall next(dispatch);
}

routine(MULVV) {
  /* MULVV: Multiply B by C and store the result in A. */
  if (!tvisnum(BASE+B) || !tvisnum(BASE+C))
    tailcall next(meta_arith_VV);
  BASE[A].n = lj_vm_foldarith(BASE[B].n, BASE[C].n, MM_mul-MM_add);
  tailcall next(dispatch);
}

routine(DIVVV) {
  /* DIVVV: Divide B by C and store the result in A. */
  if (!tvisnum(BASE+B) || !tvisnum(BASE+C))
    tailcall next(meta_arith_VV);
  BASE[A].n = lj_vm_foldarith(BASE[B].n, BASE[C].n, MM_div-MM_add);
  tailcall next(dispatch);
}

routine(MODVV) {
  /* MODVV: Calculate B modulo C and store the result in A. */
  if (!tvisnum(BASE+B) || !tvisnum(BASE+C))
    tailcall next(meta_arith_VV);
  BASE[A].n = lj_vm_foldarith(BASE[B].n, BASE[C].n, MM_mod-MM_add);
  tailcall next(dispatch);
}

routine(POW) {
  /* POW: Calculate power C of B and store the result in A. */
  if (!tvisnum(BASE+B) || !tvisnum(BASE+C))
    tailcall next(meta_arith_VV);
  BASE[A].n = lj_vm_foldarith(BASE[B].n, BASE[C].n, MM_pow-MM_add);
  tailcall next(dispatch);
}

routine(CAT) {
  /* CAT: Concatenate all values in variable slots B to C inclusive. */
  vm_savepc(L, PC);
  TValue *mbase = lj_meta_cat(L, BASE+C, C-B);
  if (mbase) {
    NARGS = 2;
    BASE = mbase;
    tailcall next(continuation);
  } else copyTV(L, BASE+A, BASE+B);
  tailcall next(dispatch);
}

routine(KSTR) {
  setgcVraw(BASE+A, kgcref(D, GCobj), LJ_TSTR);
  tailcall next(dispatch);
}

routine(KCDATA) {
  /* KCDATA: Set A to cdata constant D. */
  setcdataV(L, BASE+A, kgcref(D, GCcdata));
  tailcall next(dispatch);
}

routine(KSHORT) {
  /* BASE[A] = D */
  setnumV(BASE+A, (int16_t) D); // D is a signed int16 literal.
  tailcall next(dispatch);
}

routine(KNUM) {
  /* KNUM: Set slot A to number constant D. */
  setnumV(BASE+A, ktv(D)->n);
  tailcall next(dispatch);
}

routine(KPRI) {
  /* KPRI: Set A to primitive D. */
  setpriV(BASE+A, ~D); // D is 0/1/2 for nil/false/true.
  tailcall next(dispatch);
}

routine(KNIL) {
  /* KNIL: Set slots A to D to nil. */
  copyTVs(L, BASE+A, NULL, 1+D - A, 0);
  tailcall next(dispatch);
}

routine(UGET) {
  /* UGET: 	Set A to upvalue D. */
  GCfuncL *parent = &(funcV(frame_callee(BASE))->l);
  BASE[A] = *mref(parent->uvptr[D]->uv.v, TValue);
  tailcall next(dispatch);
}

routine(USETV) {
  /* USETV: Set upvalue A to D. */
  GCfuncL *parent = &(funcV(frame_callee(BASE))->l);
  GCupval *uv = &parent->uvptr[A]->uv;
  TValue *v = (TValue *)uv->v;
  copyTV(L, v, BASE+D);
  // Upvalue closed, marked black, and new value is collectable and white?
  if (uv->closed && (uv->marked & LJ_GC_BLACK)
      && tvisgcv(v) && iswhite(gcval(v)))
    // Crossed a write barrier. Move the barrier forward.
    lj_gc_barrieruv(G(L), v);
  tailcall next(dispatch);
}

routine(USETS) {
  /* USETS: Set upvalue A to string constant D. */
  GCfuncL *parent = &(funcV(frame_callee(BASE))->l);
  GCupval *uv = &parent->uvptr[A]->uv;
  TValue *v = (TValue *)uv->v;
  GCobj *o = kgcref(D, GCobj);
  setgcVraw(v, o, LJ_TSTR);
  // Upvalue closed, marked black, and new value is white?
  if (uv->closed && (uv->marked & LJ_GC_BLACK) && iswhite(o))
    // Crossed a write barrier. Move the barrier forward.
    lj_gc_barrieruv(G(L), v);
  tailcall next(dispatch);
}

routine(USETN) {
  /* USETN: Set upvalue A to number constant D. */
  GCfuncL *parent = &(funcV(frame_callee(BASE))->l);
  GCupval *uv = &parent->uvptr[A]->uv;
  TValue *v = (TValue *)uv->v;
  setnumV(v, numV(ktv(D)));
  tailcall next(dispatch);
}

routine(USETP) {
  /* USETP: Set upvalue A to primitive D. */
  GCfuncL *parent = &(funcV(frame_callee(BASE))->l);
  GCupval *uv = &parent->uvptr[A]->uv;
  TValue *v = (TValue *)uv->v;
  setpriV(v, ~D);
  tailcall next(dispatch);
}

routine(UCLO) {
  /* UCLO: Close upvalues for slots ≥ rbase and jump to target D. */
  if (L->openupval > (GCRef)0)
    lj_func_closeuv(L, BASE+A);
  branchPC(D);
  tailcall next(dispatch);
}

routine(FNEW) {
  /* FNEW: Create new closure from prototype D and store it in A. */
  vm_savepc(L, PC);
  GCproto *pt = kgcref(D, GCproto);
  GCfuncL *parent = &(funcV(frame_callee(BASE))->l);
  GCfunc *fn = lj_func_newL_gc(L, pt, parent);
  setgcVraw(BASE+A, (GCobj*)fn, LJ_TFUNC);
  tailcall next(dispatch);
}

routine(TNEW) {
  /* TNEW: Set A to new table with size D. */
  vm_savepc(L, PC);
  lj_gc_check(L);
  uint32_t asize = D & ((1<<11)-1);
  uint32_t hbits = D >> 11;
  GCtab *tab = lj_tab_new(L, asize, hbits);
  setgcVraw(BASE+A, (GCobj*)tab, LJ_TTAB);
  tailcall next(dispatch);
}

routine(TDUP) {
  /* TDUP: 	Set A to duplicated template table D. */
  vm_savepc(L, PC);
  lj_gc_check(L);
  GCtab *tab = lj_tab_dup(L, kgcref(D, GCtab));
  setgcVraw(BASE+A, (GCobj*)tab, LJ_TTAB);
  tailcall next(dispatch);
}

routine(GGET) {
  /* GGET: A = _G[D] */
  vm_savepc(L, PC);
  GCfunc *fn = funcV(frame_callee(BASE));
  TValue e, k;
  const TValue *v;
  setgcVraw(&e, fn->l.env, LJ_TTAB);
  setgcVraw(&k, kgcref(D, GCobj), LJ_TSTR);
  v = lj_meta_tget(L, &e, &k);
  if (v)
    copyTV(L, BASE+A, v);
  else {
    NARGS = 2;
    BASE = TOP;
    tailcall next(continuation);
  }
  tailcall next(dispatch);
}

routine(GSET) {
  /* GSET: _G[D] = A */
  vm_savepc(L, PC);
  GCfunc *fn = funcV(frame_callee(BASE));
  TValue e, k, *v;
  setgcVraw(&e, fn->l.env, LJ_TTAB);
  setgcVraw(&k, kgcref(D, GCobj), LJ_TSTR);
  v = lj_meta_tset(L, &e, &k);
  if (v) {
    copyTV(L, v, BASE+A);
  } else {
    copyTV(L, TOP+2, BASE+A); /* Copy value to third argument. */
    NARGS = 3;
    BASE = TOP;
    tailcall next(continuation);
  }
  tailcall next(dispatch);
}

routine(TGETV) {
  /* TGETV: A = B[C] */
  vm_savepc(L, PC);
  const TValue *v = lj_meta_tget(L, BASE+B, BASE+C);
  if (v)
    copyTV(L, BASE+A, v);
  else {
    NARGS = 2;
    BASE = TOP;
    tailcall next(continuation);
  }
  tailcall next(dispatch);
}

routine(TGETS) {
  /* TGETS: A = B[C] where C is a string constant. */
  vm_savepc(L, PC);
  TValue k;
  const TValue *v;
  setgcVraw(&k, kgcref(C, GCobj), LJ_TSTR);
  v = lj_meta_tget(L, BASE+B, &k);
  if (v)
    copyTV(L, BASE+A, v);
  else {
    NARGS = 2;
    BASE = TOP;
    tailcall next(continuation);
  }
  tailcall next(dispatch);
}

routine(TGETB) {
  /* TGETB: A = B[C] where C is a byte literal. */
  vm_savepc(L, PC);
  TValue k;
  const TValue *v;
  k.n = C;
  v = lj_meta_tget(L, BASE+B, &k);
  if (v)
    copyTV(L, BASE+A, v);
  else {
    NARGS = 2;
    BASE = TOP;
    tailcall next(continuation);
  }
  tailcall next(dispatch);
}

routine(TGETR) {
  /* TGETR: A = B[C]  (__index is ignored). */
  copyTV(L, BASE+A, lj_tab_getint(tabV(BASE+B), (int32_t)numV(BASE+C)));
  tailcall next(dispatch);
}

routine(TSETV) {
  /* TSETV: B[C] = A */
  vm_savepc(L, PC);
  TValue *v = lj_meta_tset(L, BASE+B, BASE+C);
  if (v) {
    copyTV(L, v, BASE+A);
  } else {
    copyTV(L, TOP+2, BASE+A); /* Copy value to third argument. */
    NARGS = 3;
    BASE = TOP;
    tailcall next(continuation);
  }
  tailcall next(dispatch);
}

routine(TSETS) {
  /* TSETS: B[C] = A */
  vm_savepc(L, PC);
  TValue k, *v;
  setgcVraw(&k, kgcref(C, GCobj), LJ_TSTR);
  v = lj_meta_tset(L, BASE+B, &k);
  if (v) {
    copyTV(L, v, BASE+A);
  } else {
    copyTV(L, TOP+2, BASE+A); /* Copy value to third argument. */
    NARGS = 3;
    BASE = TOP;
    tailcall next(continuation);
  }
  tailcall next(dispatch);
}

routine(TSETB) {
  /* TSETB: B[C] = A where C is an unsigned literal. */
  vm_savepc(L, PC);
  TValue k, *v;
  k.n = C;
  v = lj_meta_tset(L, BASE+B, &k);
  if (v) {
    copyTV(L, v, BASE+A);
  } else {
    copyTV(L, TOP+2, BASE+A); /* Copy value to third argument. */
    NARGS = 3;
    BASE = TOP;
    tailcall next(continuation);
  }
  tailcall next(dispatch);
}

routine(TSETM) {
  /* TSETM: (A-1)[D], (A-1)[D+1], ... = A, A+1, ... */
  vm_savepc(L, PC);
  unsigned int i = 0, ix = ktv(D)->u32.lo;
  TValue *o = BASE+A-1;
  GCtab *tab = tabV(o);
  if (tab->asize < ix+MULTRES)
    lj_tab_reasize(L, tab, ix + MULTRES);
  for (i = 0; i < MULTRES; i++)
    *arrayslot(tab, ix+i) = BASE[A+i];
  lj_gc_anybarriert(L, tab);
  tailcall next(dispatch);
}

routine(TSETR) {
  /* TSETR: B[C] = A (__newindex is ignored.) */
  vm_savepc(L, PC);
  copyTV(L, lj_tab_setint(L, tabV(BASE+B), (int32_t)numV(BASE+C)), BASE+A);
  tailcall next(dispatch);
}

routine(CALL) {
  /* CALLM: Call A(A+1, ..., A+C+MULTRES) */
  /* CALL: Call A(A+1, ..., A+C-1 */
  NARGS = OP == BC_CALL ? C-1 : C+MULTRES; /* nargs in MULTRES */
  /* Notes:
   *
   * PC is 32-bit aligned and so the low bits are always 00 which
   * corresponds to the FRAME_LUA tag value.
   *
   * CALL does not have to record the number of expected results
   * in the frame data. The callee's RET bytecode will locate this
   * CALL and read the value from the B operand. */
  L->base = BASE += 2+A;
  *frame_link(BASE) = PC;
  tailcall next(call);
}

routine(CALLT) {
  /* CALLMT: Tailcall A(A+1, ..., A+D+MULTRES) */
  /* CALLT: Tailcall A(A+1, ..., A+D-1). */
  NARGS = OP == BC_CALLT ? D-1 : D+MULTRES; /* nargs in MULTRES */
  MULTRES = NARGS;
  BASE += 2+A;
  tailcall next(callt);
}

routine(ITERC) {
  /* ITERC: Call iterator: A, A+1, A+2 = A-3, A-2, A-1; A, ..., A+B-2 = A(A+1, A+2). */
  NARGS = 2;
  L->base = BASE += 2+A;
  *frame_link(BASE) = PC;
  BASE[0] = BASE[-4]; // Copy state.
  BASE[1] = BASE[-3]; // Copy control var.
  *frame_callee(BASE) = BASE[-5]; // Copy callable.
  tailcall next(call);
}

routine(ITERN) {
  /* ITERN: Specialized ITERC, if iterator function A-3 is next(). */
  // NYI: add hotloop, record BC_ITERN.
  GCtab *tab = tabV(BASE + A-2);
  TValue *state = BASE + A-1;
  TValue *key = BASE + A+0;
  TValue *val = BASE + A+1;
  unsigned int i = state->i;
  /* Advance to ITERL instruction. */
  BC = *PC++;
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
  { tailcall next(dispatch); }
}

routine(IITERN) {
  // TODO: once we have hot countine ITERN, add interpreting version here.
  tailcall next(ITERN);
}

routine(VARG) {
  /* VARG: Vararg: A, ..., A+B-2 = ... */
  int delta = link_delta(*frame_link(BASE));
  MULTRES = max(delta-2-(int)C, 0);
  copyTVs(L, BASE+A, BASE-delta+C, B>0 ? B-1 : MULTRES, MULTRES);
  tailcall next(dispatch);
}

routine(ISNEXT) {
  /* ISNEXT: Verify ITERN specialization and jump. */
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
  tailcall next(dispatch);
}

routine(RETM) {
  /* RETM: return A, ..., A+D+MULTRES-1 */
  PC = *frame_link(BASE);
  BASE += A;
  MULTRES += D;
  tailcall next(return);
}

routine(RET) {
  /* RET: return A, ..., A+D-2 */
  PC = *frame_link(BASE);
  BASE += A;
  MULTRES = D-1;
  tailcall next(return);
}

routine(RET0) {
  /* RET0: return */
  PC = *frame_link(BASE);
  BASE += A;
  MULTRES = 0;
  tailcall next(return);
}

routine(RET1) {
  /* RET1: return A */
  PC = *frame_link(BASE);
  BASE += A;
  MULTRES = 1;
  tailcall next(return);
}

static inline TValue *for_idx(TValue *state) { return &state[0]; }
static inline TValue *for_stop(TValue *state) { return &state[1]; }
static inline TValue *for_step(TValue *state) { return &state[2]; }
static inline TValue *for_ext(TValue *state) { return &state[3]; }

static inline int for_init(TValue* state) {
  double idx = numV(for_idx(state));
  double stop = numV(for_stop(state));
  double step = numV(for_step(state));
  /* Copy loop index to stack. */
  setnumV(for_ext(state), idx);
  /* Check for termination */
  return (step >= 0 && idx <= stop) || (step < 0 && stop <= idx);
}

static inline int for_next(TValue* state) {
  double idx = numV(for_idx(state));
  double stop = numV(for_stop(state));
  double step = numV(for_step(state));
  /* Update loop index. */
  double next = idx + step;
  setnumV(for_idx(state), next);
  /* Copy loop index to stack. */
  setnumV(for_ext(state), next);
  /* Check for termination */
  return (step >= 0 && next <= stop) || (step < 0 && stop <= next);
}

routine(FORI) {
  /* FORI: Numeric 'for' loop init. */
  vm_savepc(L, PC);
  /* Initialize loop parameters. */
  lj_meta_for(L, BASE+A); 
  if (!for_init(BASE+A))
    branchPC(D);
  tailcall next(dispatch);
}

routine(JFORI) {
  /* JFORI: Numeric 'for' loop init, JIT-compiled. */
  vm_savepc(L, PC);
  /* Initialize loop parameters. */
  lj_meta_for(L, BASE+A); 
  /* Always branch in JFORI. */
  branchPC(D);
  if (!for_init(BASE+A))
    tailcall next(dispatch);
  /* Continue with trace in found in JFORL bytecode. */
  BC = PC[-1];
  tailcall next(exec_trace);
}

routine_inline(for_loop) {
  if (!tvisnum(for_idx(BASE+A))  ||
      !tvisnum(for_stop(BASE+A)) ||
      !tvisnum(for_step(BASE+A))) {
    vm_savepc(L, PC);
    lj_meta_for(L, BASE+A);
  }
  if (for_next(BASE+A))
    branchPC(D);
  tailcall next(dispatch);
}

routine(FORL) {
  /* FORL: Numeric 'for' loop */
  if (hotloop(L, PC))
    tailcall next(hotloop);
  tailcall next(for_loop);
}

routine(IFORL) {
  /* IFORL: Numeric 'for' loop, force interpreter */
  tailcall next(for_loop);
}

routine(JFORL) {
  /* JFORL: Numeric 'for' loop, jitted */
  if (for_next(BASE+A))
    tailcall next(exec_trace);
  tailcall next(dispatch);
}

static inline int iter_next(TValue *state) {
  if (!tvisnil(state)) {
    /* Save control var. */
    state[-1] = *(state);
    return 1;
  }
  return 0;
}

routine(ITERL) {
  /* ITERL: Iterator 'for' loop. */
  if (hotloop(L, PC))
    tailcall next(hotloop);
  if (iter_next(BASE+A))
    branchPC(D);
  tailcall next(dispatch);
}

routine(IITERL) {
  /* IITERL: Iterator 'for' loop, force interpreter. */
  if (iter_next(BASE+A))
    branchPC(D);
  tailcall next(dispatch);
}

routine(JITERL) {
  /* JITERL: Iterator 'for' loop, JIT-compiled. */
  if (iter_next(BASE+A))
    tailcall next(exec_trace);
  tailcall next(dispatch);
}

routine(LOOP) {
  /* LOOP: Generic loop */
  if (hotloop(L, PC))
    tailcall next(hotloop);
  tailcall next(dispatch);
}

routine(ILOOP) {
  /* ILOOP: Generic loop, force interpreter */
  tailcall next(dispatch); // nop
}

routine(JLOOP) {
  /* JLOOP: Generic loop, JIT-compiled */
  tailcall next(exec_trace);
}

routine(JMP) {
  /* JMP: Jump */
  branchPC(D);
  tailcall next(dispatch);
}

static inline GCproto *func_proto(const BCIns *pc) {
  return (GCproto *)((intptr_t)(pc-1) - sizeof(GCproto));
}

static inline TValue *func_top(
  lua_State *L, TValue *base, unsigned int delta, unsigned int nargs
) {
  assert((L->top + delta + LUA_MINSTACK) <= mref(L->maxstack, TValue));
  /* Fill missing args with nil. */
  if (delta > nargs) copyTVs(L, base+nargs, NULL, delta-nargs, 0);
  return base + delta;
}

routine(FUNCF) {
  /* FUNCF: Fixed-arg Lua function */
  if (hotcall(L, PC))
    tailcall next(hotcall);
  KBASE = mref(func_proto(PC)->k, void);
  TOP = func_top(L, BASE, A, NARGS);
  tailcall next(dispatch);
}

routine(IFUNCF) {
  /* IFUNCF: Fixed-arg Lua function, force interpreter */
  KBASE = mref(func_proto(PC)->k, void);
  TOP = func_top(L, BASE, A, NARGS);
  tailcall next(dispatch);
}

routine(JFUNCF) {
  /* JFUNCF: Fixed-arg Lua function, JIT-compiled */
  KBASE = mref(func_proto(PC)->k, void);
  TOP = func_top(L, BASE, A, NARGS);
  tailcall next(exec_trace);
}

routine(FUNCV) {
  /* FUNCV: Vararg Lua function */
  GCproto *pt = func_proto(PC);
  KBASE = mref(pt->k, void);
  TOP = BASE + A;
  assert(TOP+LUA_MINSTACK <= mref(L->maxstack, TValue));
  /* Save base of frame containing all parameters. */
  TValue *oldbase = BASE;
  /* Base for new frame containing only fixed parameters. */
  L->base = BASE += 2+NARGS;
  copyTV(L, frame_callee(BASE), frame_callee(oldbase));
  *frame_link(BASE) = frame(FRAME_VARG, BASE - oldbase);
  copyTVs(L, BASE, oldbase, pt->numparams, NARGS);
  /* Fill moved args with nil. */
  copyTVs(L, oldbase, NULL, min(pt->numparams, NARGS), 0);
  tailcall next(dispatch);
}

/* NYI: IFUNCV: Vararg Lua function, force interpreter */
/* NYI: JFUNCV: Vararg Lua function, JIT-compiled */

routine(FUNCC) {
  /* FUNCC: Pseudo-header for C functions */
  /* 
  ** Call C function.
  */
  int nresults, resultofs;
  lua_CFunction *f = &funcV(frame_callee(BASE))->c.f; /* C function pointer */
  TOP = BASE + NARGS;
  assert(TOP+LUA_MINSTACK <= mref(L->maxstack, TValue));
  STATE = ~LJ_VMST_C;
  nresults = (*f)(L);
  STATE = ~LJ_VMST_INTERP;
  BASE = L->base;
  resultofs = TOP - (BASE + nresults);
  PC = *frame_link(BASE);
  BASE += resultofs;
  MULTRES = nresults;
  tailcall next(return);
}

/* NYI: FUNCCW */


/* -- Fast-paths --------------------------------------------------------- */

/* Slow-path fallback handler
 *
 * Call fallback handler for fast-paths (relics from the ASM VM) and
 * massage VM state to return to caller.
 *
 * Slow-paths ae peculiar in multiple ways:
 *  - they use the stack space BASE-2..BASE+NARGS
 *  - they can yield to retries and tailcalls
 */
routine(slowpath) {
  const BCIns *link = *frame_link(BASE);
  TOP = BASE + NARGS;
  assert(TOP+1+LUA_MINSTACK <= mref(L->maxstack, TValue));
  lua_CFunction *f = &funcV(frame_callee(BASE))->c.f; /* C function pointer */
  vm_savepc(L, link);
  int res = (*f)(L);
  switch (res) {
  case -1: { /* FFH_TAILCALL */
    PC = link; /* Reset PC for debug_framepc(). */
    if (link_type(link) == FRAME_LUA) {
      int delta = bc_a(PC[-1]);
      L->base -= delta+2;
    } else {
      L->base -= link_delta(link);
    }
    *frame_link(BASE) = PC;
    tailcall next(call);
  }
  case  0: /* FFH_RETRY */
    PC--; /* Retry the operation. */
    tailcall next(dispatch);
  default: /* FFH_RES(n) */
    PC = link;
    BASE -= 2;
    MULTRES = res-1; /* res is number of results + 1 */
    tailcall next(return);
  }
}

/* Fast-path implementations */

#define BC_assert (BC__MAX+0x00)
routine(assert) {
  if (NARGS < 1 || !tvistruecond(BASE))
    tailcall next(slowpath);

  PC = *frame_link(BASE);
  MULTRES = NARGS;
  tailcall next(return);
}

#define BC_type (BC__MAX+0x01)
routine(type) {
  if (NARGS < 1)
    tailcall next(slowpath);
  
  uint32_t type = itype(BASE);
  GCfuncC *f = &funcV(frame_callee(BASE))->c;
  if (type < LJ_TISNUM)
    type = LJ_TISNUM;
  type = ~type;
  *frame_callee(BASE) = f->upvalue[type];
  PC = *frame_link(BASE);
  BASE -= 2;
  MULTRES = 1;
  tailcall next(return);
}

#define BC_next (BC__MAX+0x02)
routine(next) {
  if (NARGS < 1 || !tvistab(BASE))
    tailcall next(slowpath);

  if (NARGS < 2) setnilV(BASE+1);
  TOP = BASE;
  vm_savepc(L, *frame_link(BASE));
  int more = lj_tab_next(tabV(BASE), BASE+1, BASE+1);
  if (more > 0) {
    /* Copy key and value to results. */
    PC = *frame_link(BASE);
    BASE += 1;
    MULTRES = 2;
    tailcall next(return);
  } else if (!more) {
    /* End of traversal: return nil. */
    setnilV(frame_callee(BASE));
    PC = *frame_link(BASE);
    BASE -= 2;
    MULTRES = 1;
    tailcall next(return);
  } else
    /* Invalid key: throw from slow path. */
    tailcall next(slowpath);
}

#define BC_pairs (BC__MAX+0x03)
routine(pairs) {
  /* XXX - punt to fallback. */
  tailcall next(slowpath);
}

#define BC_ipairs_aux (BC__MAX+0x04)
routine(ipairs_aux) {
  if (NARGS < 2 || !tvistab(BASE) || !tvisnum(BASE+1))
    tailcall next(slowpath);

  TValue *tab = BASE;
  TValue *i = BASE+1;
  const TValue *v;
  PC = *frame_link(BASE);
  BASE -= 2;
  /* Increment index. */
  uint32_t n = numV(i) + 1;
  setnumV(BASE, n);
  /* Try to load value from table (if this fails the iterator ends.)  */
  if (n < tabV(tab)->asize) {
    /* Value is in array part of tab. */
    v = arrayslot(tabV(tab), n);
    if (tvisnil(v)) goto stop;
  } else {
    if (!tabV(tab)->hmask) goto stop;
    v = lj_tab_getinth(tabV(tab), n);
    if (!v) goto stop;
  }
  /* Copy array slot. */
  BASE[1] = *v; 
  /* Iterate: return (i, value). */
  MULTRES = 2;
  tailcall next(return); 
stop:
  /* End of interator: return no values. */
  MULTRES = 0;
  tailcall next(return);
}

#define BC_ipairs (BC__MAX+0x05)
routine(ipairs) {
  /* XXX - punt to fallback. */
  tailcall next(slowpath);
}

#define BC_getmetatable (BC__MAX+0x06)
routine(getmetatable) {
  if (NARGS <= 0)
    tailcall next(slowpath);

  GCtab *mt;
  if (tvistab(BASE))
    mt = tabref(tabV(BASE)->metatable);
  else if (tvisudata(BASE))
    mt = tabref(udataV(BASE)->metatable);
  else
    mt = tabref(basemt_obj(G(L), BASE));
  PC = *frame_link(BASE);
  BASE -= 2;
  MULTRES = 1;
  if (mt) {
    cTValue *mo = lj_tab_getstr(mt, mmname_str(G(L), MM_metatable));
    if (mo)
      copyTV(L, BASE, mo);
    else
      setgcVraw(BASE, (GCobj*)mt, LJ_TTAB);
  } else {
    setnilV(BASE);
  }
  tailcall next(return);
}

#define BC_setmetatable (BC__MAX+0x07)
routine(setmetatable) {
  /* XXX - punt to fallback. */
  tailcall next(slowpath);
}

#define BC_rawget (BC__MAX+0x08)
routine(rawget) { // 0x69
  if (NARGS < 2 || !tvistab(BASE))
    tailcall next(slowpath);

  copyTV(L, BASE, lj_tab_get(L, tabV(BASE), BASE+1));
  PC = *frame_link(BASE);
  MULTRES = 1;
  tailcall next(return);
}

#define BC_tonumber (BC__MAX+0x09)
routine(tonumber) { 
  if (NARGS != 1 || !tvisnumber(BASE))
    tailcall next(slowpath);

  PC = *frame_link(BASE);
  MULTRES = 1;
  tailcall next(return);
}

#define BC_tostring (BC__MAX+0x0a)
routine(tostring) {
  /* XXX - punt to fallback. */
  tailcall next(slowpath);
}

#define BC_pcall (BC__MAX+0x0b)
routine(pcall) {
  if (NARGS < 1)
    tailcall next(slowpath);

  /* First argument (BASE) is the function to call. */
  int delta = 2;
  L->base = BASE += delta;
  /* Copy remaining function arguments (from top to avoid clobberin'). */
  int copyargs = NARGS--;
  while (copyargs--)
    copyTV(L, BASE+copyargs, BASE-delta+1+copyargs);
  int hookflag = hook_active(G(L)) ? 1 : 0;
  *frame_link(BASE) = frame(FRAME_PCALL + hookflag, delta);
  tailcall next(call);
}

#define BC_xpcall (BC__MAX+0x0c)
routine(xpcall) {
  if (NARGS < 2 || !tvisfunc(BASE+1))
    tailcall next(slowpath);

  /* First argument (BASE) is the function to call, second argument (BASE+1)
     is the handler. */
  int delta = 3;
  L->base = BASE += delta;
  /* Swap function and handler. */
  TValue f;
  f = BASE[-delta]; BASE[-delta] = BASE[1-delta]; BASE[1-delta] = f;
  /* Copy remaining function arguments (from top to avoid clobberin'). */
  int copyargs = (NARGS -= 2);
  while (copyargs--)
    copyTV(L, BASE+copyargs, BASE-delta+2+copyargs);
  int hookflag = hook_active(G(L)) ? 1 : 0;
  *frame_link(BASE) = frame(FRAME_PCALL + hookflag, delta);
  tailcall next(call);
}

/*
 * -- Resuming and yielding from coroutines ---------------------------
 *
 * Coroutines (from an implementation standpoint) are implemented as
 * separate execution contexts (lua_State), notably with dedicated
 * stacks, that execute a given function interleaved with the execution
 * of the parent lua_State.
 *
 * An instance of an execution state (represented as a lua_State) is
 * also called a "thread". A thread combined with an "initial function"
 * forms what we call a coroutine.
 *
 * Resuming a coroutine for the first time is like calling the initial
 * function inside the coroutine's thread with the arguments provided
 * (which have to be copied to the coroutine’s stack.)
 *
 * A resumed coroutine can yield result values, or throw an error. A
 * coroutine that has yielded can be resumed again to continue
 * execution from the point where it yielded.
 *
 * Resuming a coroutine again after a yield is like returning to the
 * caller of yield in the coroutine’s thread with the arguments to
 * resume being the result values.
 */

#define BC_coroutine_yield (BC__MAX+0x0d)
routine(coroutine_yield) {
  /* Yielding from a coroutine means unlinking its CFrame and setting its
   * thread status to LUA_YIELD before returning to lj_vm_resume.
   */
  if ((intptr_t)L->cframe & CFRAME_RESUME) {
    TOP = BASE+NARGS;
    L->cframe = 0;
    L->status = LUA_YIELD;
    return;
  } else {
    tailcall next(slowpath);
  }
}

#define BC_coroutine_resume (BC__MAX+0x0e)
#define BC_coroutine_wrap (BC__MAX+0x0f)
routine(coroutine_resume) {
  /* The code for the resume and the wrap_aux fast functions are similar
   * enough to share most of their code. They differ as follows:
   *
   *   - resume behaves like pcall, catching errors during coroutine
   *     execution and prepending a boolean status value to the results
   *
   *   - wrap_aux behaves like a regular function call, it returns the
   *     results as is, but throws an error if the coroutine fails
   *
   * The code below prepares the stack frame of coroutine `co'. The frame
   * starts at `rbase' and ends at `rtop'. See lj_vm_resume for how the
   * call/return behavior of resume is implemented.
   *
   * On yield resume will return `true' followed by the coroutine’s
   * results. On error resume returns `false' and the error message
   * produced by the coroutine.
   */
  lua_State *co;
  TValue *rbase, *rtop;
  if (OP == BC_coroutine_resume) {
    /* resume: first argument must be a thread (the coroutine object). */
    if (NARGS >= 1 && tvisthread(BASE))
      co = threadV(BASE);
    else
      tailcall next(slowpath);
  } else {
    /* wrap_aux: get thread from caller upvalue. */
    co = threadV(&funcV(frame_callee(BASE))->c.upvalue[0]);
  }
  /* The thread must not have a CFrame attached. */
  if (co->cframe)
    tailcall next(slowpath);
  /* The thread's status must be either LUA_OK or LUA_YIELD. */
  if (co->status > LUA_YIELD)
    tailcall next(slowpath);
  /* If the coroutine is resumed for the first time then we expect the
      initial function at the top of its stack. */
  if (co->status == LUA_OK && co->base == co->top)
    tailcall next(slowpath);
  /* Prepare frame at coroutine’s TOP. (When resumed for the first time,
      make room for frame link.) */
  rbase = co->top + (co->status == LUA_OK);
  /* Extend coroutine frame to hold remaining arguments. */
  rtop = rbase + NARGS - (OP == BC_coroutine_resume); /* resume: -1 for `co' */
  /* Make sure we don't exceed the coroutine’s stack space. */
  if (rtop > mref(co->maxstack, TValue))
    tailcall next(slowpath);
  else
    co->top = rtop;
  /* Save caller PC. */
  vm_savepc(L, *frame_link(BASE));
  /* Clear arguments from stack. */
  TOP = BASE;
  if (OP == BC_coroutine_resume)
    /* resume: keep resumed thread in parent stack for GC. */
    TOP += 1;
  /* Copy arguments. resume: -1 for `co' */
  copyTVs(L, rbase, TOP, rtop-rbase, NARGS - (OP == BC_coroutine_resume));
  /* Resume coroutine at rbase. */
  lj_vm_resume(co, rbase, 0, 0);
  /* Reference the now-current lua_State. */
  setgcref(G(L)->cur_L, obj2gco(L));
  /* Handle result depending on co->status. */
  if (co->status <= LUA_YIELD) {
    /* Coroutine yielded with results. */
    int nresults = co->top - co->base;
    /* Clear coroutine stack. */
    co->top = co->base;
    /* Ensure we have stack space for coroutine results. */
    assert(TOP+nresults <= mref(L->maxstack, TValue));
    /* Copy coroutine results */
    copyTVs(L, TOP, co->base, nresults, nresults);
    if (OP==BC_coroutine_resume) {
      /* resume: prepend true to results. */
      setboolV(BASE, 1);
      nresults += 1;
    }
    PC = *frame_link(BASE);    
    MULTRES = nresults;
    tailcall next(return);
  } else {
    /* Coroutine returned with error (at co->top-1). */
    if (OP == BC_coroutine_wrap) {
      /* wrap_aux: throw error. */
      lj_ffh_coroutine_wrap_err(L, co);
    } else {
      /* resume: catch the error. */
      co->top -= 1; /* Clear error from coroutine stack. */
      /* Return (false, <error>) */
      setboolV(BASE, 0);
      copyTV(L, BASE+1, co->top);
      PC = *frame_link(BASE);      
      MULTRES = 2;
      tailcall next(return);
    }
  }
  tailcall next(dispatch);
}

#define BC_math_abs (BC__MAX+0x10)
routine(math_abs) {
  if (NARGS < 1 || !tvisnum(BASE))
    tailcall next(slowpath);

  double n = numV(BASE);
  PC = *frame_link(BASE);
  BASE -= 2;
  MULTRES = 1;
  setnumV(BASE, n < 0 ? -1*n : n);
  tailcall next(return);
}

#define BC_math_floor (BC__MAX+0x11)
routine(math_floor) {
  if (NARGS < 1 || !tvisnum(BASE))
    tailcall next(slowpath);

  double n = numV(BASE);
  PC = *frame_link(BASE);
  BASE -= 2;
  MULTRES = 1;
  setnumV(BASE, lj_vm_floor(n));
  tailcall next(return);
}

#define BC_math_ceil (BC__MAX+0x12)
routine(math_ceil) {
  if (NARGS < 1 || !tvisnum(BASE))
    tailcall next(slowpath);

  double n = numV(BASE);
  PC = *frame_link(BASE);
  BASE -= 2;
  MULTRES = 1;
  setnumV(BASE, lj_vm_ceil(n));
  tailcall next(return);
}

#define BC_math_sqrt (BC__MAX+0x13)
routine(math_sqrt) {
  if (NARGS < 1 || !tvisnum(BASE))
    tailcall next(slowpath);

  double n = numV(BASE);
  PC = *frame_link(BASE);
  BASE -= 2;
  MULTRES = 1;
  setnumV(BASE, sqrt(n));
  tailcall next(return);
}

#define BC_math_log10 (BC__MAX+0x14)
routine(math_log10) {
  if (NARGS < 1 || !tvisnum(BASE))
    tailcall next(slowpath);

  double n = numV(BASE);
  PC = *frame_link(BASE);
  BASE -= 2;
  MULTRES = 1;
  setnumV(BASE, log10(n));
  tailcall next(return);
}

#define BC_math_exp (BC__MAX+0x15)
routine(math_exp) {
  if (NARGS < 1 || !tvisnum(BASE))
    tailcall next(slowpath);

  double n = numV(BASE);
  PC = *frame_link(BASE);
  BASE -= 2;
  MULTRES = 1;
  setnumV(BASE, exp(n));
  tailcall next(return);
}

#define BC_math_sin (BC__MAX+0x16)
routine(math_sin) {
  if (NARGS < 1 || !tvisnum(BASE))
    tailcall next(slowpath);

  double n = numV(BASE);
  PC = *frame_link(BASE);
  BASE -= 2;
  MULTRES = 1;
  setnumV(BASE, sin(n));
  tailcall next(return);
}

#define BC_math_cos (BC__MAX+0x17)
routine(math_cos) {
  if (NARGS < 1 || !tvisnum(BASE))
    tailcall next(slowpath);
  
  double n = numV(BASE);
  PC = *frame_link(BASE);
  BASE -= 2;
  MULTRES = 1;
  setnumV(BASE, cos(n));
  tailcall next(return);
}

#define BC_math_tan (BC__MAX+0x18)
routine(math_tan) {
  if (NARGS < 1 || !tvisnum(BASE))
    tailcall next(slowpath);

  double n = numV(BASE);
  PC = *frame_link(BASE);
  BASE -= 2;
  MULTRES = 1;
  setnumV(BASE, tan(n));
  tailcall next(return);
}

#define BC_math_frexp (BC__MAX+0x1F)
routine(math_frexp) {
  if (NARGS < 1 || !tvisnum(BASE))
    tailcall next(slowpath);

  int n;
  setnumV(BASE, frexp(numV(BASE), &n));
  setnumV(BASE+1, n);
  PC = *frame_link(BASE);    
  MULTRES = 2;
  tailcall next(return);
}

#define BC_math_modf (BC__MAX+0x20)
routine(math_modf) {
  if (NARGS < 1 || !tvisnum(BASE))
    tailcall next(slowpath);

  double n;
  setnumV(BASE, modf(numV(BASE), &n));
  setnumV(BASE+1, n);
  PC = *frame_link(BASE);    
  MULTRES = 2;
  tailcall next(return);
}

#define BC_math_log (BC__MAX+0x21)
routine(math_log) {
  if (NARGS < 1 || !tvisnum(BASE))
    tailcall next(slowpath);
  
  double n = numV(BASE);
  PC = *frame_link(BASE);
  BASE -= 2;
  MULTRES = 1;
  setnumV(BASE, log(n));
  tailcall next(return);
}

#define BC_math_atan (BC__MAX+0x22)
routine(math_atan) {
  if (NARGS < 2 || !tvisnum(BASE) || !tvisnum(BASE+1))
    tailcall next(slowpath);

  double y = numV(BASE);
  double x = numV(BASE+1);
  PC = *frame_link(BASE);
  BASE -= 2;
  MULTRES = 1;
  setnumV(BASE, atan2(y, x));
  tailcall next(return);
}

#define BC_math_ldexp (BC__MAX+0x25)
routine(math_ldexp) {
  if (NARGS < 2 || !tvisnum(BASE) || !tvisnum(BASE+1))
    tailcall next(slowpath);

  double x = numV(BASE);
  double exp = numV(BASE+1);
  PC = *frame_link(BASE);
  BASE -= 2;
  MULTRES = 1;
  setnumV(BASE, ldexp(x, exp));
  tailcall next(return);
}

#define BC_math_min (BC__MAX+0x26)
routine(math_min) {
  if (NARGS < 1 || !tvisnum(BASE))
    tailcall next(slowpath);
  while (NARGS-- > 1) {
    if (!tvisnum(BASE+NARGS))
      tailcall next(slowpath);
    setnumV(BASE, min(numV(BASE), numV(BASE+NARGS)));
  }
  PC = *frame_link(BASE);  
  MULTRES = 1;
  tailcall next(return);
}

#define BC_math_max (BC__MAX+0x27)
routine(math_max) {
  if (NARGS < 1 || !tvisnum(BASE))
    tailcall next(slowpath);
  while (NARGS-- > 1) {
    if (!tvisnum(BASE+NARGS))
      tailcall next(slowpath);
    setnumV(BASE, max(numV(BASE), numV(BASE+NARGS)));
  }
  PC = *frame_link(BASE);  
  MULTRES = 1;
  tailcall next(return);
}

#define BC_bit_tobit (BC__MAX+0x28)
routine(bit_tobit) {
  if (NARGS < 1 || !tvisnum(BASE))
    tailcall next(slowpath);

  BASE->n = tobit(BASE);
  PC = *frame_link(BASE);    
  MULTRES = 1;
  tailcall next(return);
}

#define BC_bit_bnot (BC__MAX+0x29)
routine(bit_bnot) {
  if (NARGS < 1 || !tvisnum(BASE))
    tailcall next(slowpath);

  BASE->n = ~tobit(BASE);
  PC = *frame_link(BASE);    
  MULTRES = 1;
  tailcall next(return);
}

#define BC_bit_bswap (BC__MAX+0x2a)
routine(bit_bswap) {
  if (NARGS < 1 || !tvisnum(BASE))
    tailcall next(slowpath);

  BASE->n = (int32_t)bswap_32((uint32_t)tobit(BASE));
  PC = *frame_link(BASE);    
  MULTRES = 1;
  tailcall next(return);
}

#define BC_bit_lshift (BC__MAX+0x2b)
routine(bit_lshift) {
  if (NARGS < 2 || !tvisnum(BASE) || !tvisnum(BASE+1))
    tailcall next(slowpath);

  BASE->n = tobit(BASE) << tobit(BASE+1);
  PC = *frame_link(BASE); 
  MULTRES = 1;
  tailcall next(return);
}

#define BC_bit_rshift (BC__MAX+0x2c)
routine(bit_rshift) {
  if (NARGS < 2 || !tvisnum(BASE) || !tvisnum(BASE+1))
    tailcall next(slowpath);

  BASE->n = (int32_t)((uint32_t)tobit(BASE) >> tobit(BASE+1));
  PC = *frame_link(BASE);
  MULTRES = 1;
  tailcall next(return);
}

#define BC_bit_arshift (BC__MAX+0x2d)
routine(bit_arshift) {
  if (NARGS < 2 || !tvisnum(BASE) || !tvisnum(BASE+1))
    tailcall next(slowpath);

  BASE->n = tobit(BASE) >> tobit(BASE+1);
  PC = *frame_link(BASE);
  MULTRES = 1;
  tailcall next(return);
}

#define BC_bit_rol (BC__MAX+0x2e)
routine(bit_rol) {
  if (NARGS < 2 || !tvisnum(BASE) || !tvisnum(BASE+1))
    tailcall next(slowpath);

  uint32_t b = tobit(BASE), n = (uint32_t)tobit(BASE+1) & 31;
  BASE->n = (int32_t)((b << n) | (b >> (32-n)));
  PC = *frame_link(BASE);
  MULTRES = 1;
  tailcall next(return);
}

#define BC_bit_ror (BC__MAX+0x2f)
routine(bit_ror) {
  if (NARGS < 2 || !tvisnum(BASE) || !tvisnum(BASE+1))
    tailcall next(slowpath);

  uint32_t b = tobit(BASE), n = (uint32_t)tobit(BASE+1) & 31;
  BASE->n = (int32_t)((b << (32-n)) | (b >> n));
  PC = *frame_link(BASE);
  MULTRES = 1;
  tailcall next(return);
}

#define BC_bit_band (BC__MAX+0x30)
routine(bit_band) {
  if (NARGS < 1 || !tvisnum(BASE))
    tailcall next(slowpath);
  int32_t res = tobit(BASE);
  while (NARGS-- > 1) {
    if (!tvisnum(BASE+NARGS))
      tailcall next(slowpath);
    res &= tobit(BASE+NARGS);
  }
  BASE->n = res;
  PC = *frame_link(BASE);
  MULTRES = 1;
  tailcall next(return);
}

#define BC_bit_bor (BC__MAX+0x31)
routine(bit_bor) {
  if (NARGS < 1 || !tvisnum(BASE))
    tailcall next(slowpath);
  int32_t res = tobit(BASE);
  while (NARGS-- > 1) {
    if (!tvisnum(BASE+NARGS))
      tailcall next(slowpath);
    res |= tobit(BASE+NARGS);
  }
  BASE->n = res;
  PC = *frame_link(BASE);
  MULTRES = 1;
  tailcall next(return);
}

#define BC_bit_bxor (BC__MAX+0x32)
routine(bit_bxor) {
  if (NARGS < 1 || !tvisnum(BASE))
    tailcall next(slowpath);
  int32_t res = tobit(BASE);
  while (NARGS-- > 1) {
    if (!tvisnum(BASE+NARGS))
      tailcall next(slowpath);
    res ^= tobit(BASE+NARGS);
  }
  BASE->n = res;
  PC = *frame_link(BASE);
  MULTRES = 1;
  tailcall next(return);
}

#define BC_string_byte (BC__MAX+0x33)
routine(string_byte) {
  if (NARGS < 1 || !tvisstr(BASE)
      || (NARGS > 1 && !tvisnum(BASE+1))
      || (NARGS > 2 && !tvisnum(BASE+2)))
    tailcall next(slowpath);

  GCstr *str = strV(BASE);
  int start = NARGS >= 2 ? numV(BASE+1) : 1;
  int end = NARGS >= 3 ? numV(BASE+2) : start;
  int i, nresults;
  if (start < 0)
    start = max(start + (int)str->len+1, 1);
  else
    start = max(min(start, (int)str->len+1), 1);
  if (end < 0)
    end = max(end + (int)str->len+1, 0);
  else
    end = min(end, (int)str->len);
  nresults = max(1+end-start, 0);
  assert(BASE+nresults <= mref(L->maxstack, TValue));
  for (i=0; i+start <= end; i++)
    setnumV(BASE+i, (uint8_t)(strdata(str)[i+start-1]));
  PC = *frame_link(BASE);
  MULTRES = nresults;
  tailcall next(return);
}

#define BC_string_char (BC__MAX+0x34)
routine(string_char) {
  /* XXX - punt to fallback. */
  tailcall next(slowpath);
}

#define BC_string_sub (BC__MAX+0x35)
routine(string_sub) {
  if (NARGS < 2 || !tvisstr(BASE) || !tvisnum(BASE+1)
      || (NARGS > 2 && !tvisnum(BASE+2)))
    tailcall next(slowpath);

  vm_savepc(L, PC);
  lj_gc_check(L);
  GCstr *str = strV(BASE);
  int start = numV(BASE+1);
  int end = NARGS > 2 ? numV(BASE+2) : -1;
  PC = *frame_link(BASE);
  BASE -= 2;
  MULTRES = 1;
  if (start < 0)
    start = max(start + (int)str->len+1, 1);
  else
    start = max(min(start, (int)str->len+1), 1);
  if (end < 0)
    end = max(end + (int)str->len+1, 0);
  else
    end = min(end, (int)str->len);
  str = lj_str_new(L, strdata(str)+start-1, max(1+end-start, 0));
  setgcVraw(BASE, (GCobj *)str, LJ_TSTR);
  tailcall next(return);
}

#define BC_string_reverse (BC__MAX+0x36)
#define BC_string_lower (BC__MAX+0x37)
#define BC_string_upper (BC__MAX+0x38)
routine(string_op) {
  if (NARGS < 1 || !tvisstr(BASE))
    tailcall next(slowpath);

  /* Fast function string operations. */
  vm_savepc(L, PC);
  lj_gc_check(L);
  GCstr *str = strV(BASE);
  SBuf *buf = lj_buf_tmp_(L);
  switch ((uint32_t)OP) {
  case BC_string_reverse:
    lj_buf_putstr_reverse(buf, str);
    break;
  case BC_string_lower:
    lj_buf_putstr_lower(buf, str);
    break;
  case BC_string_upper:
    lj_buf_putstr_upper(buf, str);
    break;
  default: assert(0 && "NYI: fast string operation");
  }
  setgcVraw(BASE, (GCobj *)lj_buf_tostr(buf), LJ_TSTR);
  PC = *frame_link(BASE);
  MULTRES = 1;
  tailcall next(return);
}

/* Trap to pad unused dispatch table slots with. */
routine(NYI) {
  assert(0 && "INVALID or NYI BYTECODE");
}


/* -- VM entry and re-entry ---------------------------------------------- */

static inline void vm_enter(lua_State *L, int ftp, TValue *BASE) {
  const void *KBASE = (0);
  BCIns BC = (0);
  const BCIns *PC = (0);
  const struct lj_vm_fn_tag *VM = (const struct lj_vm_fn_tag *)L2GG(L)->dispatch;
  unsigned int NARGS = TOP - BASE;
  unsigned int MULTRES = 0;
  *frame_link(BASE) = frame(ftp, BASE - L->base);
  lj_vm_fn_call(call);
}

static inline void vm_reenter(lua_State *L, TValue *BASE) {
  const void *KBASE = (0);
  BCIns BC = (0);
  const BCIns *PC = *frame_link(L->base);
  const struct lj_vm_fn_tag *VM = (const struct lj_vm_fn_tag *)L2GG(L)->dispatch;
  unsigned int NARGS = 0;
  unsigned int MULTRES = TOP-BASE;
  lj_vm_fn_call(return);
}

static inline void vm_unwind(lua_State *L) {
  TValue *BASE = L->base - 1;
  const void *KBASE = (0);
  BCIns BC = (0);
  const BCIns *PC = *frame_link(L->base);
  const struct lj_vm_fn_tag *VM = (const struct lj_vm_fn_tag *)L2GG(L)->dispatch;
  unsigned int NARGS = 0;
  unsigned int MULTRES = 2;
  setboolV(BASE, 0); /* Push FALSE for unsuccessful return from a pcall.  */
  lj_vm_fn_call(return);
}


/* -- API functions ------------------------------------------------------- */

/* Call a Lua function from C. */
int luacall(lua_State *L, int p, TValue *newbase, int nres, ptrdiff_t ef)
{
  int res;
  /* Add new CFrame to the chain. */
  CFrame cf = { L->cframe, L, nres };
  L->cframe = &cf;
  /* Reference the now-current lua_State. */
  setgcref(G(L)->cur_L, obj2gco(L));
  L2J(L)->L = L;
  /* Setup VM state for callee. */
  STATE = ~LJ_VMST_INTERP;
  /* Setup "catch" jump buffer for a protected call. */
  res = _setjmp(cf.jb);
  if (res < 0) {
    /* -1 signals to continue execution from pcall, xpcall. */
    vm_unwind(L);
  } else if (res == 0) {
    /* Try */
    vm_enter(L, p ? FRAME_CP : FRAME_C, newbase);
  } else {
    /* Catch */
    return res;
  }
  /* Unlink C frame. */
  L->cframe = cf.previous;
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
  TValue *newbase = NULL;
  /* "Neg. delta means cframe w/o frame." */
  int nresults = -savestack(L, L->top);
  /* Add to CFrame chain. */
  CFrame cf = { L->cframe, L, nresults };
  L->cframe = &cf;
  /* Reference the now-current lua_State. */
  setgcref(G(L)->cur_L, obj2gco(L));
  L2J(L)->L = L;
  /* Setup "catch" jump buffer for a protected call. */
  res = _setjmp(cf.jb);
  if (res < 0) {
    /* -1 signals to continue execution from pcall, xpcall. */
    assert(0 && "NYI");
  } else if (res == 0) {
    /* Try */
    newbase = cp(L, f, ud);
    if (newbase) {
      /* Setup VM state for callee. */
      STATE = ~LJ_VMST_INTERP;
      vm_enter(L, FRAME_CP, newbase);
    }
  } else {
    /* Catch */
    return res;
  }
  /* Unlink C frame. */
  L->cframe = cf.previous;
  return LUA_OK;
}

/* Resume coroutine, see fast-paths resume and yield.
 *
 * Note: nres1 is ignored, ef NYI.
 */
int lj_vm_resume(lua_State *L, TValue *newbase, int nres1, ptrdiff_t ef) {
  int res;
  /* Set CFrame. (Note: this frame is unlinked by yield.) */
  CFrame cf = { 0, L, -1 };
  setmref(L->cframe, (intptr_t)&cf + CFRAME_RESUME);
  /* Reference the now-current lua_State. */
  setgcref(G(L)->cur_L, obj2gco(L));
  L2J(L)->L = L;
  /* Setup VM state for callee. */
  STATE = ~LJ_VMST_INTERP;
  /* Setup "catch" jump buffer for a protected call. */
  res = _setjmp(cf.jb);
  if (res < 0) {
    /* -1 signals to continue from pcall, xpcall. */
    assert(0 && "NYI");
  } else if (res == 0) {
    /* Try */
    if (L->status == LUA_OK) {
      /* Initial resume (like a call). */
      vm_enter(L, FRAME_CP, newbase);
    } else {
      /* Resume after yield (like a return). */
      L->status = LUA_OK;
      vm_reenter(L, newbase);
    }
  } else {
    /* Catch */
    L->status = res;
  }
  /* Restore PC. */
  return L->status;
}

/* Unwind from CFrame, longjmp with errcode. */
void lj_vm_unwind_c(void *cframe, int errcode) {
  longjmp(mref(cframe_raw(cframe), CFrame)->jb, errcode);
}

/* Unwind from protected Lua frame, see fast functions pcall and xpcall. */
void lj_vm_unwind_ff(void *cframe) {
  lj_vm_unwind_c(cframe, -1); /* -1 < LUA_OK signals luacall to continue. */
}

void lj_vm_unwind_c_eh(void)                   { assert(0 && "NYI"); }
void lj_vm_unwind_ff_eh(void)                  { assert(0 && "NYI"); }
void lj_vm_unwind_rethrow(void)                { assert(0 && "NYI"); }
void lj_vm_ffi_callback()                      { assert(0 && "NYI"); }

/* Miscellaneous functions. */
int lj_vm_cpuid(uint32_t f, uint32_t res[4])       {
  asm volatile("cpuid":"=a"(*res),"=b"(*(res+1)),
               "=c"(*(res+2)),"=d"(*(res+3)):"a"(f));
  return (int)res[0];
}

/* Dispatch targets for recording and hooks. */
void lj_vm_inshook(void)  { assert(0 && "NYI"); }
void lj_vm_rethook(void)  { assert(0 && "NYI"); }

void lj_vm_floor_sse(void)   { assert(0 && "NYI"); }
void lj_vm_ceil_sse(void)    { assert(0 && "NYI"); }
void lj_vm_trunc_sse(void)   { assert(0 && "NYI"); }
void lj_vm_powi_sse(void)    { assert(0 && "NYI"); }
double lj_vm_trunc(double d) { assert(0 && "NYI"); }

LJ_ASMF TValue *lj_vm_next(GCtab *t, uint32_t idx) { assert(0 && "NYI"); }
