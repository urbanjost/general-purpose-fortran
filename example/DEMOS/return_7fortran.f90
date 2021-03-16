          program demo_return
             call tryreturn(1)
             call tryreturn(10)
          contains
             subroutine tryreturn(i)
                integer,intent(in) :: i
                select case(i)
                 case(1)
                   write(*,*)'*one*'
                   return
                 case(2)
                   write(*,*)'*two*'
                   return
                 case default
                   write(*,*)'*default*'
                   return
                end select
                write(*,*)'*cannot get here*'
                return
             end subroutine tryreturn
          end program demo_return
