program basic
!! STACK OF TESTS -- some systems will have a limit on how much process spawning is allowed
use M_CLI2,  only : set_args, get_args
use M_CLI2,  only : get_args_fixed_length, get_args_fixed_size
implicit none

integer                      :: casen=0

integer                      :: x,y,z      
integer                      :: ithree(3)
integer,allocatable          :: ints(:)

real                         :: r_x,r_y,r_z      
real                         :: rfour(4)
real,allocatable             :: reals(:)

logical                      :: l_x,l_y
logical                      :: lfive(5)
logical,allocatable          :: logicals(:)

character(len=30)            :: string     

complex                      :: c_x, c_y, c_z   ! scalars
complex,allocatable          :: c_aarr(:)       ! allocatable array
complex                      :: c_three(3)      ! fixed-size array

character(len=:),allocatable :: command
character(len=4096)          :: cmd
integer                      :: e
integer                      :: i
character(len=*),parameter   :: gen='(*(g0,1x))'

   call get_command_argument(0,cmd) ! get name of this executable
   e=len_trim(cmd)+1
   
   command=' &
   & --ints 11,22,33 &
   & -x 10 -y 20 -z 10#30 &
   & --ithree -1,-2,-3 &
   ! character
   & -string "My string,""again" &
   ! real
   & -r_x -8 -r_y -88 -r_z -888 &
   & --reals 1.2,2.3,3.4,4.5,5.6,6.7,7.8 &
   & --rfour 1.1,2.2,3.3,4.4 &
   ! logical
   & --logicals T:.true:.TRUE.:.false.:F:FALSE:TRUE -l_x T -l_y F -lfive T,F,T,F,T & 
   ! complex
   & --c_three -999,-999,-999,-999,-999,-999 &
   & -c_x -999,-999 -c_y -999,-999 -c_z -999,-999 &
   & --c_aarr -999:-999::-999:-999 &
   ! case number
   & -casen 0 &
   &'
   call readcli()                              ! assume initially called with no parameters so that parameters are default values
   select case(casen)
   case(0)
      write(6,*)'COMMAND:',command
      write(6,*)'check defaults'

      write(6,*)'default integers'
      call printit(all([x,y,z,ints,ithree] == [10,20,30,11,22,33,-1,-2,-3]))

      write(6,*)'default reals scalar'
      call printit(all([r_x,r_y,r_z] == [-8.0,-88.0,-888.0]))
      write(6,*)'default reals fixed array'
      call printit(all(rfour == [1.1,2.2,3.3,4.4])) 
      write(6,*)'default allocatable array'
      write(*,*)reals
      write(*,*)[1.2,2.3,3.4,4.5,5.6,6.7,7.8]
      call printit(all(reals == [1.2,2.3,3.4,4.5,5.6,6.7,7.8]))

      write(6,*)'default logicals'
      call printit(all([l_x ,l_y,lfive,logicals].eqv. &
      & [.true., .false., .true., .false., .true., .false., .true., .true., .true., .true., .false., .false., .false., .true.]))

      write(6,*)'default complex'
      call printit(all([c_x,c_y,c_z,c_three,c_aarr] ==  [(cmplx(-999,-999),i=1,8)]))

      call runit('-x 4 -y 5 -z 6 -casen 1')       ! now call itself with some values specified
   case(1)
      write(6,*)'scalar ints'
      call printit(all([x,y,z] == [4,5,6]))       ! options set on command line in previous case
      call runit('-r_x 40 -r_y 50 -r_z 60 -casen 2')
   case(2)
      write(6,*)'scalar reals'
      call printit(all([r_x,r_y,r_z] == [40.0,50.0,60.0]))
      call runit('-x 400 -y 500 -z 600 --ints -1,-2,-3 --ithree -11,-22,-33 -casen 3')
   case(3)
      call printit(all([x,y,z,ints,ithree] == [400,500,600,-1,-2,-3,-11,-22,-33]))
      call runit('-c_x "(1,2)" -c_y 10,20 -c_z "(2#111,16#-AB)" -c_three 1,2,3,4,5,6 -c_aarr 111::222,333::444 -casen 4')
   case(4)
      ! test results for case 4

      write(6,gen)'CASE4 EXPECTED:',&
      [cmplx(1.0,2.0),cmplx(10.0,20.0),cmplx(7,-171),cmplx(1,2),cmplx(3,4),cmplx(5,6),cmplx(111,222),cmplx(333,444)]
         
      write(6,gen)'CASE4 RESULTS:',[c_x,c_y,c_z,c_three,c_aarr]

      write(6,gen)'CASE4 TESTS:',[c_x,c_y,c_z,c_three,c_aarr] == &
      & [cmplx(1.0,2.0),cmplx(10.0,20.0),cmplx(7,-171),cmplx(1,2),cmplx(3,4),cmplx(5,6),cmplx(111,222),cmplx(333,444)]

      call printit(all([c_x,c_y,c_z,c_three,c_aarr] == &
      & [cmplx(1.0,2.0),cmplx(10.0,20.0),cmplx(7,-171),cmplx(1,2),cmplx(3,4),cmplx(5,6),cmplx(111,222),cmplx(333,444)]))

      flush(unit=6)

      ! run next case
      call runit('-x 400 -y 500 -z 600 --ints -1,-2,-3 -casen 900')
   case(5)
   case(6)
   case(7)
   case(8)
   case(9)
   case(900)
      write(6,*)'USAGE'
      flush(unit=6)
      call runit('--casen 901 --usage')
      call runit('--casen 901')
   case(901)
      write(6,*)'HELP'
      flush(unit=6)
      call runit('--casen 902 --help')
      call runit('--casen 902')
   case(902)
      write(6,*)'VERSION'
      flush(unit=6)
      call runit('--casen 999 --version')
      call runit('--casen 999')
   case(999)
   case default
      write(6,'(a)')'default - should not get here'
      flush(unit=6)
      stop
   end select

contains

subroutine runit(string)
character(len=*),intent(in) :: string
      write(6,*)'RUN:',string
      flush(unit=6)
      call execute_command_line(cmd(:e)//string)
end subroutine runit

subroutine printit(testit)
logical testit
   write(6,'(*(g0,1x))',advance='no')'CASE ',casen,merge('PASSED:','FAILED:',testit)
   write(6,'(/,a)')repeat('=',132)
   flush(unit=6)
end subroutine printit

subroutine readcli()
   flush(unit=6)
   call set_args(command)
   ! integer
   call get_args('x',x)
   call get_args('y',y)
   call get_args('z',z)
   call get_args('ints',ints)
   call get_args_fixed_size('ithree',ithree)
   ! logical
   call get_args('l_x',l_x)
   call get_args('l_y',l_y)
   call get_args('logicals',logicals)
   call get_args_fixed_size('lfive',lfive)
   ! real
   call get_args('r_x',r_x)
   call get_args('r_y',r_y)
   call get_args('r_z',r_z)
   call get_args('reals',reals)
   call get_args_fixed_size('rfour',rfour)
   ! character
   call get_args_fixed_length('string',string)
   ! complex
   call get_args('c_x',c_x)
   call get_args('c_y',c_y)
   call get_args('c_z',c_z)
   call get_args('c_aarr',c_aarr)
   call get_args_fixed_size('c_three',c_three)
   !
   call get_args('casen',casen)
end subroutine readcli

end program basic
