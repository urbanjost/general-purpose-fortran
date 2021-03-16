          program demo_codebase
          use M_strings, only : codebase
          implicit none
          character(len=20) :: answer
          integer           :: i, j
          logical           :: ierr
          do j=1,100
             do i=2,36
                ierr=codebase(j,i,answer)
                write(*,*)'VALUE=',j,' BASE=',i,' ANSWER=',answer
             enddo
          enddo
          end program demo_codebase
