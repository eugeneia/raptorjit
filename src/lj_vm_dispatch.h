#ifndef _LJ_VM_DISPATCH_H
#define _LJ_VM_DISPATCH_H

#include "lj_bc.h"
#include "lj_vm.h"

static lj_vm_fn_t lj_vm_dispatch[] = {

  /* Bytecode. */
#define CALLM CALL
#define CALLMT CALLT
#define IFUNCV FUNCV
#define JFUNCV NYI
#define FUNCCW NYI

#define BCDISPATCH(name, ma, mb, mc, mt) lj_vm_fn(name),
  BCDEF(BCDISPATCH)

#undef CALLM
#undef CALLMT
#undef IFUNCV
#undef JFUNCV
#undef FUNCCW

  /* Fast-path pseudo ops. */
#define coroutine_wrap_aux coroutine_resume
#define math_asin NYI
#define math_acos NYI
#define math_atan NYI
#define math_sinh NYI
#define math_cosh NYI
#define math_tanh NYI
#define math_pow NYI
#define math_fmod NYI
#define string_reverse string_op
#define string_lower string_op
#define string_upper string_op

#define VMFFDEF(name)	lj_vm_fn(name),
#include "lj_vmffdef.h"

#undef coroutine_wrap_aux
#undef math_asin
#undef math_acos
#undef math_atan
#undef math_sinh
#undef math_cosh
#undef math_tanh
#undef math_pow
#undef math_fmod
#undef string_reverse
#undef string_lower
#undef string_upper

  lj_vm_fn(NYI)

};

#endif
