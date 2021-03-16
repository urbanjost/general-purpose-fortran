          program demo_fortran
          implicit none
          character(len=*),parameter :: ident="@(#)example(1f): example program"
          integer :: bb
          integer :: i_myloop
             call sub1(10,bb)
             write(*,*)'BB=',bb
          !-----------------------------------------------------------------------
          MYLOOP: do I_MYLOOP=1,10  ! DO loop
             cycle MYLOOP           ! start next pass of loop
             exit  MYLOOP           ! go to next statement after corresponding ENDDO
          enddo MYLOOP
          !-----------------------------------------------------------------------
          block
          character(LEN=1) :: c
          select case (c)
          case ('a' : 'j')
             write(*,*)  'One of the first ten letters'
          case ('l' : 'p', 'u' : 'y')
             write(*,*)  'One of l, m, n, o, p, u, v, w, x, y'
          case ('z', 'q' : 't')
             WRITE(*,*)  'One of z, q, r, s, t'
          case default
             write(*,*)  'Other characters, which may not be letters'
          endselect
          endblock
          !-----------------------------------------------------------------------
          CHAR: block
          ! ALLOCATABLE CHARACTER ARRAY
          character(len=80),allocatable :: help_text(:)
          integer                       :: i
          help_text=[ &
          &'12345678901234567890123456789012345678901234567890123456789012345678901234567890',&
          &'This is example help text for the example program                               ',&
          &'The example program uses kracken(3f) for command line parsing.                  ',&
          &'                                                                                ',&
          &'                                                                                ',&
          &'                                                                                ',&
          &'                                                                                ']
          WRITE(*,'(a)')(help_text(i),i=1,size(help_text))
          endblock CHAR
          !-----------------------------------------------------------------------

            contains

            subroutine sub1(a,b,c)
            integer,intent(in)                                 :: a
            integer,intent(out)                                :: b
            character(len=:),intent(out),allocatable,optional  :: c
               b=2*a
               if(present(c))then
                  c='Hello World'
               endif
            end subroutine sub1

            integer function signum (n)
            !An integer signum function
            integer,intent(in) :: n
            select case (n)
            case  (:-1)  ;  signum  =  -1
            case  (0)    ;  signum  =   0
            case  (1:)   ;  signum  =   1
            end select
            end function signum

              end program demo_fortran
