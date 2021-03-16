          ! program demo_lbound
          module m_bounds
          implicit none
           contains
              subroutine msub(arr)
                 !!integer,intent(in) :: arr(*)  ! cannot be assumed-size array
                 integer,intent(in) :: arr(:)
                 write(*,*)'MSUB: LOWER=',lbound(arr), &
                 & 'UPPER=',ubound(arr), &
                 & 'SIZE=',size(arr)
              end subroutine msub
           end module m_bounds

           use m_bounds, only : msub
           implicit none
           interface
              subroutine esub(arr)
              integer,intent(in) :: arr(:)
              end subroutine esub
           end interface
           integer :: arr(-10:10)
              write(*,*)'MAIN: LOWER=',lbound(arr), &
              & 'UPPER=',ubound(arr), &
              & 'SIZE=',size(arr)
              call csub()
              call msub(arr)
              call esub(arr)
           contains
              subroutine csub
                 write(*,*)'CSUB: LOWER=',lbound(arr), &
                 & 'UPPER=',ubound(arr), &
                 & 'SIZE=',size(arr)
              end subroutine csub
           end

           subroutine esub(arr)
           implicit none
           integer,intent(in) :: arr(:)
              ! WARNING: IF CALLED WITHOUT AN EXPLICIT INTERFACE
              ! THIS WILL GIVE UNDEFINED ANSWERS (like 0,0,0)
              write(*,*)'ESUB: LOWER=',lbound(arr), &
              & 'UPPER=',ubound(arr), &
              & 'SIZE=',size(arr)
           end subroutine esub
          !end program demo_lbound
