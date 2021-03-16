          program demo_system_getkey
          use M_getkey, only : system_getkey
          character :: A
          integer   :: icount=0
          character(len=1),parameter :: null=char(0)
             call clear()
             write(*,*)'begin striking keys to demonstrate interactive raw I/O mode'
             call menu()
             do
                A=system_getkey()
                icount=icount+1
                select case(A)
                case('a':'e')
                   write(*,*)'You entered a valid menu item ',A,'=>',ichar(A),icount
                case(null)
                   if(icount.gt.40000000)then
                      write(*,*)'limit of 40 000 000, calls reached'
                      stop
                   endif
                case('q')
                   stop
                case default
                   call clear()
                   write(*,*)'unknown menu option'
                   write(*,*)'you entered key=',A,'->',ichar(A),icount
                   call menu()
                end select
             enddo
             contains

             subroutine clear()
             ! ANSI VT102 screen clear sequence.
             ! May not work in all terminal emulators
             write(*,'(a,"[2J")',advance='no')char(27)
             flush(6)
             write(*,*)
             end subroutine clear

             subroutine menu()
             write(*,"(3x,'a)  first choice   ')")
             write(*,"(3x,'b)  second choice  ')")
             write(*,"(3x,'c)  third choice   ')")
             write(*,"(3x,'d)  fourth choice  ')")
             write(*,"(3x,'e)  fifth choice   ')")
             write(*,"('enter choice (q to quit):')")
             end subroutine menu

          end program demo_system_getkey
