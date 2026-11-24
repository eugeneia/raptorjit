;;; Various SSE math functions
;;; Copyright (C) 2026 Max Rottenkolber. See Copyright Notice in luajit.h

global lj_vm_num2int_check
global lj_vm_num2i64
global lj_vm_num2u64
global lj_vm_tobit
global lj_vm_floor
global lj_vm_ceil
global lj_vm_trunc

section .data

section .text

lj_vm_num2int_check:
        cvttsd2si eax, xmm0
        cvtsi2sd xmm1, eax
        ucomisd xmm1, xmm0
        jp num2int_error
        jne num2int_error
        ret
num2int_error:
        mov rax, 0x8000000080000000
        ret

lj_vm_num2i64:
        cvttsd2si rax, xmm0
        ret

lj_vm_num2u64:
        cvttsd2si rax, xmm0             ; Convert [-2^63..2^63) range
        cmp rax, 1                      ; Indefinite result -0x8000000000000000LL - 1 sets overflow
        jo num2u64_bias
        ret
num2u64_bias:
        mov rax, 0xc3f0000000000000     ; -0x1p64 (double)
        movq xmm1, rax
        addsd xmm0, xmm1
        cvttsd2si rax, xmm0             ; Convert [2^63..2^64+2^63) range
        ret

lj_vm_tobit:
        mov rax, 0x4338000000000000 ; 2^52 + 2^51
        movq xmm1, rax
        addsd xmm0, xmm1
        movd eax, xmm0

lj_vm_floor:
        roundsd xmm0, xmm0, 0b1001
        ret

lj_vm_ceil:
        roundsd xmm0, xmm0, 0b1010
        ret

lj_vm_trunc:
        roundsd xmm0, xmm0, 0b1011
        ret
