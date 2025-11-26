/*
** Math helper functions for assembler VM.
** Copyright (C) 2005-2023 Mike Pall. See Copyright Notice in luajit.h
*/

#define lj_vmmath_c
#define LUA_CORE

#include <errno.h>
#include <math.h>

#include "lj_obj.h"
#include "lj_ir.h"
#include "lj_vm.h"

/* -- Wrapper functions --------------------------------------------------- */

int32_t lj_vm_modi(int32_t a, int32_t b)
{
  uint32_t y, ua, ub;
  lj_assertX(b != 0, "modulo with zero divisor");
  ua = a < 0 ? ~(uint32_t)a+1u : (uint32_t)a;
  ub = b < 0 ? ~(uint32_t)b+1u : (uint32_t)b;
  y = ua % ub;
  if (y != 0 && (a^b) < 0) y = y - ub;
  if (((int32_t)y^b) < 0) y = ~y+1u;
  return (int32_t)y;
}


#ifdef LUAJIT_NO_LOG2
double lj_vm_log2(double a)
{
  return log(a) * 1.4426950408889634074;
}
#endif

/* Computes fpm(x) for extended math functions. */
double lj_vm_foldfpm(double x, int fpm)
{
  switch (fpm) {
  case IRFPM_FLOOR: return lj_vm_floor(x);
  case IRFPM_CEIL: return lj_vm_ceil(x);
  case IRFPM_TRUNC: return lj_vm_trunc(x);
  case IRFPM_SQRT: return sqrt(x);
  case IRFPM_LOG: return log(x);
  case IRFPM_LOG2: return lj_vm_log2(x);
  default: lj_assertX(0, "bad fpm %d", fpm);
  }
  return 0;
}

int lj_vm_errno(void)
{
  return errno;
}

