      program demo_command_argument_count
      implicit none
      integer :: count
      integer :: i

      ! basics:
         count = command_argument_count()
         print *
         print '(*(g0))', 'argument count=',count
         ! usually used in conjunction with get_command_argument()
         do i=1,command_argument_count()
            ! call function wrapper around get_command_argument()
            write(*,*)i,get_arg(i)
         enddo

      ! more:
         ! test the function extensively
         call testit()

      contains
      ! a test done by recursively calling the program
      ! NO PLACE FOR BEGINNERS!
      subroutine testit()
      integer,parameter             :: sz=10
      character(len=:),allocatable  :: self
      character(len=:),allocatable  :: cmd
      character(len=sz),allocatable :: first
      type test
         character(len=sz)            :: testname
         character(len=:),allocatable :: cmd
         integer                      :: answer
      end type
      type(test),allocatable        :: tests(:)
      integer                       :: i
      integer                       :: indx
         ! a set of tests (note a test cannot be null):
         tests=[ &
          & test('singular','',0), &
          & test('multiple','orange wolf C',3), &
          & test('quotes','a "Quoted String"',2), &
          & test('spaces',' one two three four    five  six  ',6), &
         ! note many characters might be special to a shell
          & test('special','"<>$@# " "$%&*()_-"',2), &
          & test('many',&
          & 'a b c d e f g h i j k l m n o p q r s t u v w x y z',26)]
         ! get name of program
         self=get_arg(0)
         ! if no options call all the test cases
         if(count.eq.0)then
            do i=1,size(tests)
               call execute_command_line&
                & (self//' '//tests(i)%testname//' '//tests(i)%cmd)
            enddo
          else
            ! assumed to be a test call of the program so compare
            ! test and results and report
            cmd=get_cmd()
            first=get_arg(1)
            indx=findloc(tests%testname,first,dim=1)
            if(indx.gt.0)then
               write(*,'(*(g0,1x))')'testing ',first,tests(indx)%cmd
               write(*,'(*(g0,1x))')'got     ',cmd
               write(*,'(a,a,tl20,a,t26,a)')'case ',repeat('.',20), &
                & trim(first),merge(' PASSED',' FAILED',&
                & count.eq.tests(indx)%answer+1)
            else
               write(*,*)'<ERROR>Unexpected command line ',cmd
            endif
         endif
      end subroutine testit

      function get_cmd() result(command_line)
      ! return entire command line
      integer                      :: command_line_length
      character(len=:),allocatable :: command_line
         ! get command line length
         call get_command(length=command_line_length)
         ! allocate string big enough to hold command line
         allocate(character(len=command_line_length) :: command_line)
         ! get command line as a string
         call get_command(command=command_line)
         ! trim leading spaces just in case
         command_line=adjustl(command_line)
      end function get_cmd

      function get_arg(n,status) result(arg)
      ! get nth argument from command line
      integer,intent(in)           :: n
      integer,intent(out),optional :: status
      integer                      :: argument_length, istat
      character(len=:),allocatable :: arg
         call get_command_argument( number=n, length=argument_length )
         if(allocated(arg))deallocate( arg )
         allocate(character(len=argument_length) :: arg )
         call get_command_argument(n, arg, status=istat )
         if(present(status)) status=istat
      end function get_arg

      end program demo_command_argument_count
