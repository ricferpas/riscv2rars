

        .text
        
print_integer:
        li      $v0, 1
        syscall
        jr      $ra        
        nop
        
read_string:
        li      $v0, 8
        syscall
        jr      $ra        
        nop

print_character:
        li      $v0, 11
        syscall
        jr      $ra        
        nop

print_string:
        li      $v0, 4
        syscall
        jr      $ra        
        nop

get_time:
        li      $v0, 30
        syscall
        move    $v0, $a0
        move    $v1, $a1
        jr      $ra
        nop
        
read_character:
        li      $v0, 12
        syscall
        jr      $ra
        nop

clear_screen:
        li      $v0, 39
        syscall
        jr      $ra
        nop

mips_exit:
        li      $v0, 17
        syscall
        jr      $ra
        nop

random_int:     
        li      $v0, 41
        syscall
        move    $v0, $a0
        jr      $ra
        nop

random_int_range:
        li      $v0, 42
        syscall
        move    $v0, $a0
        jr      $ra
        nop

keyio_poll_key:
        li      $v0, 0
        lb      $t0, 0xffff0000                 # carga registro de control del receptor
        andi    $t0, $t0, 0x00000001            # mira el bit 0
        beqz    $t0, keyio_poll_key_return      # si no hay nada disponible, devuelve 0.
        nop
        lb      $v0, 0xffff0004                 # lee carácter del registro de datos del receptor
keyio_poll_key_return:
        jr      $ra
        nop

        
