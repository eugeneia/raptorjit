;;; Calling trace machine code.
;;; Copyright (C) 2019 Max Rottenkolber. See Copyright Notice in luajit.h

global lj_vm_trace_call
global lj_vm_exit_interp
global lj_vm_exit_handler

section .data

section .text

;;; Execute the machine code sequence of a trace. Accepts pointers to a
;;; TraceCallState (see lj_vm_tcs.h) and the machine code as its arguments.
;;;
;;; Code generated by the JIT is rather deeply integrated into the former
;;; structure of the assembly bytecode VM. I.e., it expects certain former VM
;;; registers to preserve across interpreter/trace boundaries.
;;;
;;; Traced machine code does not honor the System V Calling Convention, we have
;;; to save and restore callee-save registers before entering trace machine
;;; code.
;;;
;;; We setup the the former DISPATCH and BASE registers with values from
;;; our TraceCallState (tcs) before calling the JITed code. The TraceCallState
;;; tcs is itself referenced by a pointer stored relatively close to DISPATCH
;;; and can thus be retrieved by trace exit handlers.
;;;
;;; The JITed code does not actually return by itself. At its end it will call
;;; a trace exit handler (our chance to copy back the execution's results into
;;; tcs) which will in turn return to lj_vm_trace_call.
;;;
lj_vm_trace_call:
;;; Save callee-save registers.
        push rbx
        push rbp
        push r12
        push r13
        push r14
        push r15

;;; Setup register state (DISPATCH, BASE) for use by JITed code from tcs.
        mov r14, rdi
        mov rax, [rdi-8]        ;tcs relative to dispatch
        mov rdx, [rax+0]        ;tcs->base

;;; Save rsp minus offset for return address (call) into tcs.
        lea rcx, [rsp-8]
        mov [rax+8], rcx        ;tcs->rsp = rsp-8

;;; Call JITed code, which will jump to exit handler, which in turn will
;;; return here.
        call rsi                ;mcode

;;; Restore callee-save registers.
        pop r15
        pop r14
        pop r13
        pop r12
        pop rbp
        pop rbx

;;; Return to interpreter.
        ret


;;; Exit handlers for JITed machine code.

;;; Called from an exit stub with empty stack.
lj_vm_exit_interp:
        push rax
        mov rax, [r14-8]        ;tcs relative to dispatch
        mov dword [rax+16], 1   ;tcs->handler = TRACE_EXIT_INTERP
        pop rax
        jmp lj_vm_exit

;;; Called from an exit stub with the exit number on the stack.
;;; The 16 bit exit number is stored with two (sign-extended) push imm8.
lj_vm_exit_handler:
        push rax
        mov rax, [r14-8]        ;tcs relative to dispatch
        mov dword [rax+16], 0   ;tcs->handler = TRACE_EXIT

;;; Reconstruct exit number.
        push rcx
        mov ecx, dword [rsp+24] ;lower half of exit no
        mov ch, byte [rsp+16]   ;upper half of exit no
        mov dword [rax+20], ecx ;tcs->exitno (actually a uint32_t)

        pop rcx
        pop rax
        lea rsp, [rsp+16]

        jmp lj_vm_exit

;;; Build tcs->exitstate and restore stack for return to lj_vm_trace_call.
lj_vm_exit:
;;; Copy register state.
        push rax
        mov rax, [r14-8]        ;tcs relative to dispatch
        lea rax, [rax+24]       ;tcs->exitstate
        mov [rax+248], r15
        mov [rax+240], r14
        mov [rax+232], r13
        mov [rax+216], r12
        mov [rax+208], r11
        mov [rax+200], r10
        mov [rax+192], r9
        mov [rax+184], r8
        mov [rax+176], rdi
        mov [rax+168], rbp
        mov [rax+160], rsp
        mov [rax+152], rbx
        mov [rax+144], rdx
        mov [rax+136], rcx
        pop rcx                 ;rax
        mov [rax+128], rcx
        movsd qword [rax+120], xmm15
        movsd qword [rax+112], xmm14
        movsd qword [rax+104], xmm13
        movsd qword [rax+96], xmm12
        movsd qword [rax+88], xmm11
        movsd qword [rax+80], xmm10
        movsd qword [rax+72], xmm9
        movsd qword [rax+64], xmm8
        movsd qword [rax+56], xmm7
        movsd qword [rax+48], xmm6
        movsd qword [rax+40], xmm5
        movsd qword [rax+32], xmm4
        movsd qword [rax+24], xmm3
        movsd qword [rax+16], xmm2
        movsd qword [rax+8], xmm1
        movsd qword [rax+0], xmm0

;;; Copy spill slots.
        lea rax, [rax+256]      ;exitstate->spill
        mov rdx, [r14-8]        ;tcs relative to dispatch
        mov rdx, [rdx+8]        ;tcs->rsp
        mov rbx, rsp
        sub rbx, rdx
        mov rcx, 0
copy_spill:
        cmp rbx, rbx
        jz return
        mov rax, [rdx+rcx]
        mov [rax+rcx], rax
        sub rbx, 8
        add rcx, 8
        jmp copy_spill

;;; Return to lj_vm_trace_call
return:
        mov rsp, rdx
        ret
