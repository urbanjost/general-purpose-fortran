     program demo_lower
     use iso_fortran_env, only : stdout => output_unit
     use M_unicode,       only : lower, unicode_type, assignment(=), trim
     use M_unicode,       only : ut => unicode_type, operator(==)
     implicit none
     character(len=*),parameter :: g='(*(g0))'
     type(unicode_type) :: pangram
     type(unicode_type) :: diacritics
     type(unicode_type) :: expected
       !
       ! a sentence containing every letter of the English alphabet
       pangram="THE QUICK BROWN FOX JUMPS OVER THE LAZY DOG"
       expected="the quick brown fox jumps over the lazy dog"
       call test(pangram,expected)
       !
       ! Slovak pangram
       PANGRAM    = 'VYPÄTÁ DCÉRA GRÓFA MAXWELLA S IQ NIŽŠÍM AKO &
       &KÔŇ NÚTI ČEĽAĎ HRÝZŤ HŔBU JABĹK.'
       expected   = 'vypätá dcéra grófa maxwella s iq nižším ako &
       &kôň núti čeľaď hrýzť hŕbu jabĺk.'
       call test(pangram,expected)
       !
       ! contains each special Czech letter with diacritics exactly once
       DIACRITICS='PŘÍLIŠ ŽLUŤOUČKÝ KŮŇ ÚPĚL ĎÁBELSKÉ ÓDY.'
       expected ='příliš žluťoučký kůň úpěl ďábelské ódy.'
       print g,'("A horse that was too yellow-ish moaned devilish odes")'
       call test(diacritics,expected)
     contains
     subroutine test(in,expected)
     type(unicode_type),intent(in) :: in
     type(unicode_type),intent(in) :: expected
     type(unicode_type)            :: lowercase
     character(len=*),parameter    :: nl=new_line('A')
         write(stdout,g)in%character()
         lowercase=lower(in)
         write(stdout,g)lowercase%character()
         write(stdout,g)merge('PASSED','FAILED',lowercase == expected ),nl
     end subroutine test
     end program demo_lower
