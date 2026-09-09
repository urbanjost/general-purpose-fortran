       program demo_squeeze
       use M_strings, only : squeeze
       implicit none
          call printme( '', ' ' )
          call printme('1111  1111   111 111  1117777888',['1','7','X'] )
          call printme(' Mary had a lllittllle lllamb','l')
       contains
       impure elemental subroutine printme(str,chr)
       character(len=*),intent(in) :: str
       character(len=1),intent(in) :: chr
       character(len=:),allocatable :: answer
          write(*,'(a)')repeat('=',42)
          write(*,'("IN:   ",g0)')str
          answer=squeeze(str,chr)
          write(*,'("OUT:  ",g0)')answer
          write(*,'("LENS: ",*(g0,1x))')"from",len(str),"to",len(answer), &
                  & "for a change of",len(str)-len(answer)
          write(*,'("CHAR: ",g0)')chr
       end subroutine printme
       end program demo_squeeze
