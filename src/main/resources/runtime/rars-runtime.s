
        .text
        
        .globl	print_integer
print_integer:
        li      a7, 1
        ecall
        ret        
       
        .globl	read_integer
read_integer:
        li      a7, 5
        ecall
        ret        

        .globl	read_string
read_string:
        li      a7, 8
        ecall
        ret        

        .globl	print_character
print_character:
        li      a7, 11
        ecall
        ret


        .globl	print_string
print_string:
        li      a7, 4
        ecall
        ret        

        .globl	get_time
get_time:
        li      a7, 30
        ecall
        ret

        .globl	system_sleep
system_sleep:
        li      a7, 32
        ecall
        ret

        .globl	read_character
read_character:
        li      a7, 12
        ecall
        ret

        .globl	clear_screen
clear_screen:
        li      a7, 39
        ecall
        ret

        .globl	exit
exit:
        li      a7, 93
        ecall
        ret

        .globl	random_int
random_int:     
        li      a7, 41
        ecall
        ret

        .globl	random_int_range
random_int_range:
        li      a7, 42
        ecall
        ret

       	.eqv    KEYBOARD_0_BASE, 0xffff0000 # Comentar esta línea para ensamblar en RIPES

        .globl	keyio_poll_key
keyio_poll_key:
        li      a0, 0
       	li      t0, KEYBOARD_0_BASE
        lb      a1, 0(t0)                   # carga registro de control del receptor
        andi    a2, a1, 0x00000001          # mira el bit 0
        beqz    a2, keyio_poll_key_return   # si no hay nada disponible devuelve 0.
        lb      a0, 4(t0)                   # lee carácter del registro de datos del receptor
        andi    a2, a1, 0xfffffffe
        sb      a2, 0(t0)                   # pone a 0 el bit 0 del registro de control
keyio_poll_key_return:
        ret

	.globl	memcpy
memcpy:
        mv      a3, a0
memcpy_loop:
        beqz    a2, memcpy_return
        lbu     t0, 0(a1)
        sb      t0, 0(a3)
        addi    a3, a3, 1
        addi    a1, a1, 1
        addi    a2, a2, -1
        j       memcpy_loop
memcpy_return:
        ret

	.globl	memset
memset:
        mv      t0, a0
memset_loop:
        beqz    a2, memset_return
        sb      a1, 0(t0)
        addi    t0, t0, 1
        addi    a2, a2, -1
        j       memset_loop
memset_return:
        ret
        
