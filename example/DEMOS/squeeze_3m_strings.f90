     program demo_squeeze
     use M_strings, only : squeeze
     implicit none
     character(len=:),allocatable :: strings(:)

     strings=[ character(len=72) :: &
     &'', &
     &'"If I were two-faced,&
     &would I be wearing this one?" --- Abraham Lincoln',  &
     &'..1111111111111111111&
     &111111111111111111111111111111111111111111117777888', &
     &'I never give ''em hell,&
     &I just tell the truth, and they think it''s hell.',&
     &'                                                  &
     & --- Harry S Truman'    &
     &]
        call printme( trim(strings(1)), ' ' )
        call printme( strings(2:4),     ['-','7','.'] )
        call printme( strings(5),       [' ','-','r'] )
     contains
     impure elemental subroutine printme(str,chr)
     character(len=*),intent(in) :: str
     character(len=1),intent(in) :: chr
     character(len=:),allocatable :: answer
        write(*,'(a)')repeat('=',11)
        write(*,'("IN:   <<<",g0,">>>")')str
        answer=squeeze(str,chr)
        write(*,'("OUT:  <<<",g0,">>>")')answer
        write(*,'("LENS: ",*(g0,1x))')"from",len(str),"to",len(answer), &
                & "for a change of",len(str)-len(answer)
        write(*,'("CHAR: ",g0)')chr
     end subroutine printme
     end program demo_squeeze
