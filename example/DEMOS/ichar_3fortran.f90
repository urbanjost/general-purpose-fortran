          program demo_ichar
          implicit none
          integer i
             do i=0,127
                call printme()
             enddo
          contains
          subroutine printme()
          character(len=1) :: letter
          letter=char(i)
             select case(i)
              case (:31,127:)
                write(*,'(1x,i0.3,1x,"HEX=",z2.2,1x,i0)')i,letter,ichar(letter)
              case default
                write(*,'(1x,i0.3,1x,a,1x,i0)')i,letter,ichar(letter)
             end select
          end subroutine printme
          end program demo_ichar
