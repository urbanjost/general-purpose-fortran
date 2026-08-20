     program demo_upper
     use iso_fortran_env, only : stdout => output_unit
     use M_unicode,       only : upper, unicode_type, assignment(=)
     use M_unicode,       only : ut => unicode_type, operator(==)
     implicit none
     character(len=*),parameter :: g='(*(g0))'
     type(unicode_type)         :: pangram
     type(unicode_type)         :: diacritics
     type(unicode_type)         :: expected
        !
        ! a sentence containing every letter of the English alphabet
        ! often used to test telegraphs since the advent of the 19th century
        ! and as an exercise repetitively generated in typing classes
        pangram  = "The quick brown fox jumps over the lazy dog."
        expected = "THE QUICK BROWN FOX JUMPS OVER THE LAZY DOG."
        call test(pangram,expected)
        !
        ! Slovak pangram
        pangram    = 'Vypätá dcéra grófa Maxwella s IQ nižším ako &
        &kôň núti čeľaď hrýzť hŕbu jabĺk.'
        expected   = 'VYPÄTÁ DCÉRA GRÓFA MAXWELLA S IQ NIŽŠÍM AKO &
        &KÔŇ NÚTI ČEĽAĎ HRÝZŤ HŔBU JABĹK.'
        call test(pangram,expected)
        !
        ! contains each special Czech letter with diacritics exactly once
        print g,'("A horse that was too yellow-ish moaned devilish odes")'
        diacritics = 'Příliš žluťoučký kůň úpěl ďábelské ódy.'
        expected   = 'PŘÍLIŠ ŽLUŤOUČKÝ KŮŇ ÚPĚL ĎÁBELSKÉ ÓDY.'
        call test(diacritics,expected)
     contains
     subroutine test(in,expected)
     type(unicode_type),intent(in) :: in
     type(unicode_type),intent(in) :: expected
     type(unicode_type)            :: uppercase
     character(len=*),parameter    :: nl=new_line('A')
        write(stdout,g)in%character()
        uppercase=upper(in)
        write(stdout,g)uppercase%character()
        write(stdout,g)merge('PASSED','FAILED',uppercase == expected ),nl
     end subroutine test
     end program demo_upper
