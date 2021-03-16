          program demo_free_source_form
          use,intrinsic :: iso_fortran_env, only : ERROR_UNIT, &
                                                   INPUT_UNIT, &
                                                   OUTPUT_UNIT ! access computing environment
          use iso_fortran_env, only : int8, int32, int64
          implicit none
          integer,parameter  :: dp=kind(0.0d0)
          real(kind=dp),save :: x(10)=0.0_dp
          character(len=255) :: filename
          logical            :: lval
          integer            :: ier
          integer            :: i_myloop
          integer            :: i, j, k
          character(len=*),parameter    :: VERSION='1.0'
          character(len=:), allocatable :: mystring
             call usage()
             filename='my file'
             print *, "filename=", trim(filename)
          !-----------------------------------------------------------------------
             MYLOOP: do I_MYLOOP=1,10  ! DO loop
                cycle MYLOOP           ! start next pass of loop
                exit  MYLOOP           ! go to next statement after corresponding ENDDO
             enddo MYLOOP
          !-----------------------------------------------------------------------
             block
                character(len=1) :: c
                mystring=trim(filename)
                do i=1,len(mystring)
                   c=mystring(i:i)
                   select case (c)
                    CASE ('a' : 'j');            WRITE(*,*)c, ' :One of the first ten letters'
                    CASE ('l' : 'p', 'u' : 'y'); WRITE(*,*)c, ' :One of l, m, n, o, p, u, v, w, x, y'
                    CASE ('z', 'q' : 't');       WRITE(*,*)c, ' :One of z, q, r, s, t'
                    CASE default
                      WRITE(ERROR_UNIT,*)c, ' :Other characters, which may not be letters'
                   end select
                enddo
                write(*,*)signum([10,20,0,-100])
                ! mine(3f) is a function that does I/O. Do not use in an I/O statement
                x=mine(100.40)
             endblock
          !-----------------------------------------------------------------------
          contains
          ! An integer signum function:
          elemental integer function signum (n)
          integer,intent(in) :: n
            select case (n)
             case (:-1); signum =    1
             case (0);   signum = 0
             case (1:);  signum = 1
            end select
          end function signum
          !-------------------------------------------------------------------------------
          function mine(xx) result(yy) ! note when RESULT used, function name has no type
          implicit none
          real :: xx
          real :: yy
            write(*,*)'VALUE=',xx
            yy=xx
          end function mine
          !-------------------------------------------------------------------------------
          subroutine usage()
          character(len=80),allocatable :: help_text(:)
          integer                       :: i
            help_text=[ &
            &'12345678901234567890123456789012345678901234567890123456789012345678901234567890',&
            &'This is example help text for the example program                               ',&
            &'                                                                                ',&
            &'                                                                                ',&
            &'                                                                                ']
            WRITE(*,'(a)')(help_text(i),i=1,size(help_text))
          end subroutine usage
          !-------------------------------------------------------------------------------
          end program demo_free_source_form
