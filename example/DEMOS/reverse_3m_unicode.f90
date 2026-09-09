     program demo_reverse
     use iso_fortran_env, only : stdout => output_unit
     use M_unicode,       only : reverse, ch=>character
     use M_unicode,       only : unicode_type, assignment(=)
     use M_unicode,       only : ut => unicode_type, operator(==)
     implicit none
     character(len=*),parameter :: g='(g0)'
     type(unicode_type)         :: original(3)
        original(1)='abcde'
        original(2)='한국말'
        original(3)='五十七'
        write(stdout,g)ch(original)
        write(stdout,*)
        write(stdout,g)ch(reverse(original))
     end program demo_reverse
