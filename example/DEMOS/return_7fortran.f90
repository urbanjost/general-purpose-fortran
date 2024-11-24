          program demo_return
             call tryreturn(1)
             write(*,*)'back at main program:1'
             call tryreturn(10)
             write(*,*)'back at main program:10'
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
                write(*,*)'*unexpected value*'
             end select
             write(*,*)'*<ERROR> should not get here*'
          end subroutine tryreturn
          end program demo_return
