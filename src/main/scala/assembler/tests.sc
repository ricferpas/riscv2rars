package assembler

object tests {
  2 + 2                                           //> res0: Int(4) = 4
  val p = ast.Program.fromFile("/tmp/t.s")        //> p  : assembler.ast.Program = Program(List(Directive(section,List(LabelRef(.md
                                                  //| ebug.abi32))), Directive(previous,List()), Directive(file,List(StringConst(te
                                                  //| tris.c))), Directive(section,List(LabelRef(.debug_info), StringConst(), Label
                                                  //| Ref(@progbits))), Label($section_info), Directive(section,List(LabelRef(.debu
                                                  //| g_abbrev), StringConst(), LabelRef(@progbits))), Label($section_abbrev), Dire
                                                  //| ctive(section,List(LabelRef(.debug_aranges), StringConst(), LabelRef(@progbit
                                                  //| s))), Directive(section,List(LabelRef(.debug_macinfo), StringConst(), LabelRe
                                                  //| f(@progbits))), Directive(section,List(LabelRef(.debug_line), StringConst(), 
                                                  //| LabelRef(@progbits))), Label($section_line), Directive(section,List(LabelRef(
                                                  //| .debug_loc), StringConst(), LabelRef(@progbits))), Directive(section,List(Lab
                                                  //| elRef(.debug_pubtypes), StringConst(), LabelRef(@progbits))), Directive(secti
                                                  //| on,List(LabelRef(.debug_str), StringConst(MS), LabelRef(@progbits), IntegerCo
                                                  //| nst(1))), Label($info_str
                                                  //| Output exceeds cutoff limit.
  printer.format(p)                               //> res1: String = .section .mdebug.abi32
                                                  //| .previous 
                                                  //| .file 	"tetris.c"
                                                  //| .section .debug_info,	"",@progbits
                                                  //| $section_info:
                                                  //| .section .debug_abbrev,	"",@progbits
                                                  //| $section_abbrev:
                                                  //| .section .debug_aranges,	"",@progbits
                                                  //| .section .debug_macinfo,	"",@progbits
                                                  //| .section .debug_line,	"",@progbits
                                                  //| $section_line:
                                                  //| .section .debug_loc,	"",@progbits
                                                  //| .section .debug_pubtypes,	"",@progbits
                                                  //| .section .debug_str,	"MS",@progbits,1
                                                  //| $info_string:
                                                  //| .section .debug_ranges,	"",@progbits
                                                  //| $debug_range:
                                                  //| .section .debug_loc,	"",@progbits
                                                  //| $section_debug_loc:
                                                  //| .text 
                                                  //| $text_begin:
                                                  //| .data 
                                                  //| .file 1,	"tetris.c"
                                                  //| .text 
                                                  //| .globl imagen_pixel_addr
                                                  //| .align 2
                                                  //| .type imagen_pixel_addr,@function
                                                  //| .set nomips16
                                                  //| .ent imagen_pixel_addr
                                                  //| imagen_pixel_addr:
                                                  //| .cfi_startproc 
                                                  //| $func_begin0:
                                                  //| .loc 1,18,0
                                                  //| .frame $sp,0,$ra
                                                  //| .mask 0,0
                                                  //| .fmask 0,0
                                                  //| .set noreorder
                                                  //| .set nomacro
                                                  //| .set noat
                                                  //| .loc 1,19,0,prolo
                                                  //| Output exceeds cutoff limit.
}