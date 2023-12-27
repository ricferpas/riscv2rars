

        .text
        
print_integer:
        li      a7, 1
        ecall
        ret        
       
read_integer:
        li      a7, 5
        ecall
        ret        

#read_string:
#        li      a7, 8
#        ecall
#        ret        

print_character:
        li      a7, 11
        ecall
        ret


print_string:
        li      a7, 4
        ecall
        ret        

get_time:
        li      a7, 30
        ecall
        ret

system_sleep:
        li      a7, 32
        ecall
        ret

read_character:
        li      a7, 12
        ecall
        ret

clear_screen:
        li      a7, 39
        ecall
        ret

rars_exit:
        li      a7, 93
        ecall
        ret

random_int:     
        li      a7, 41
        ecall
        ret

random_int_range:
        li      a7, 42
        ecall
        ret

        
keyio_poll_key:
        li      a0, 0
        lb      a1, 0xffff0000                 # carga registro de control #del receptor
        andi    a2, a1, 0x00000001            # mira el bit 0
        beqz    a2, keyio_poll_key_return      # si no hay nada disponible, #devuelve 0.
        lb      a0, 0xffff0004                 # lee carácter del registro de #datos del receptor
        andi    a2, a1, 0xfffffffe
        sb      a2, 0xffff0000, a3              # pone a 0 el bit 0 del registro de control
keyio_poll_key_return:
        ret

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

#memset:
#        move    $v0, $a0
#memset_loop:
#        beqz    $a2, memset_return
#        sb      $a1, 0($a0)
#        addiu   $a0, $a0, 1
#        addiu   $a2, $a2, -1
#        j       memset_loop
#memset_return:
#        ret
#        
