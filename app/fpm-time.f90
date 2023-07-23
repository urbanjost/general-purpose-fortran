program fpm_time
use M_CLI2,    only : set_args, sget, sgets, iget, lget, unnamed
use M_strings, only : join
character(len=:),allocatable :: subcommand
character(len=:),allocatable :: flag
character(len=:),allocatable :: command_gprof, command_fpm, command_base
character(len=:),allocatable :: which(:)
character(len=*),parameter   :: gprof=       "'gprof --demangle --flat-profile '"
character(len=*),parameter   :: gprof_start= "'gprof --demangle --no-flat-profile --sum'"
character(len=*),parameter   :: gprof_quiet= "'gprof --demangle --no-flat-profile --sum' -- gmon.out gmon.sum"
character(len=*),parameter   :: gprof_sum=   "'gprof --demangle --flat-profile ' -- gmon.out gmon.sum"
character(len=:),allocatable :: opts
character(len=:),allocatable :: help_text(:)
character(len=:),allocatable :: version_text(:)
integer                      :: repeat
integer                      :: i
logical                      :: dryrun
   ! set help and version text
   call setup()
   ! process command argments
   call set_args('--target:T "*" --repeat:R 1 --flag:F:: " " --dryrun:D F',help_text,version_text)
   ! process command arguments
   if(size(unnamed).eq.0)then
      unnamed=['test']
   endif
   subcommand=trim(unnamed(1))
   if(subcommand.eq.'example')then
      subcommand='run --example'
   endif
   which=sgets('target')
   if(size(unnamed).gt.1)then
      which=[character(len=max(len(which),len(unnamed))) :: which,unnamed(2:)]
   endif
   repeat=max(1,iget('repeat'))
   opts= "'-g -O0 -pg -fPIC "//sget('flag')//"'"
   dryrun=lget('dryrun')
   ! get rid of any previous gprof(1) output files
   call delete_file('gmon.sum')
   call delete_file('gmon.out')

   ! build and run the specified program with profiling turned on
   command_fpm=                                 &
    & join(which,                               &
    & start='fpm '//subcommand//' ',            &
    & left="'",right="'",sep=' ',               &
    & end=' --verbose --compiler gfortran --flag '&
    & //opts)
   command_base=                                  &
    & 'fpm '//subcommand//' '//                   &
    & join(which,left="'",right="'",sep=' ')//    &
    & ' --verbose --compiler gfortran --flag '//  &
    & opts//' --runner '

   if(repeat.gt.1)then

      command_gprof=command_base//gprof_start
      call run()

      command_gprof=command_base//gprof_quiet
      do i=2,repeat-1
         call run()
      enddo

      command_gprof=command_base//gprof_sum
      call run()

      call delete_file('gmon.out')

   else ! regular single execution
      command_gprof=command_base//gprof
      call run()
   endif

contains

subroutine run()
   ! run gprof with each program name
   write(*,'("+",a)')trim(command_fpm)
   if(.not.dryrun) call execute_command_line(command_fpm)
   write(*,'("+",a)')trim(command_gprof)
   if(.not.dryrun) call execute_command_line(command_gprof)
end subroutine

!> delete a file by filename
subroutine delete_file(file)
    character(len=*), intent(in) :: file
    logical :: exist
    integer :: unit
    integer :: ios
    inquire(file=file, exist=exist,iostat=ios)
    if (exist) then
        open(file=file, newunit=unit,iostat=ios)
        close(unit, status="delete",iostat=ios)
    endif
end subroutine delete_file

subroutine setup()
help_text=[ CHARACTER(LEN=128) :: &
'help_text=[ CHARACTER(LEN=128) :: &',&
'NAME                               ',&
'  fpm-time(1) - call fpm(1) with gprof(1) to generata a flat timing profile',&
'SYNOPIS                                                                    ',&
'  fpm-time [subcommand] [--target] targets                                 ',&
'DESCRIPTION                                                                ',&
'  Run the fpm(1) command with the gfortran(1) compiler and compiler flags  ',&
'  required to build instrumented programs which will generate gprof(1)     ',&
'  output files. Run the program and then run a basic gprof(1) command      ',&
'  on each output.                                                          ',&
'                                                                           ',&
'  IMPORTANT: ONE target program should be selected if multiple targets exist.',&
'                                                                             ',&
'  NOTE: 2021-03-21                                                           ',&
'                                                                             ',&
'     This is a prototype plug-in for fpm(1), which is currently in alpha     ',&
'     release. It may require changes at any time as a result.                ',&
'                                                                             ',&
'OPTIONS                                                                      ',&
'   subcommand  fpm(1) subcommand used to run a program (test,run). If        ',&
'               no options are specified the default is "test".               ',&
'               The name "example" will be converted to "run --example"       ',&
'               internally.                                                   ',&
'   --targets   which targets to run. The default is "*". ONE target should   ',&
'               be tested                                                     ',&
'   --flag      ADDITIONAL flags to add to the compile                        ',&
'   --repeat,R  number of times to execute the program. Typically, this helps ',&
'               reduce the effects of I/O buffering and other factors that can',&
'               skew results. Defaults to one execution.                      ',&
'   --help      display this help and exit                                    ',&
'   --version   output version information and exit                           ',&
'                                                                             ',&
'EXAMPLE                                                                      ',&
'   # in the parent directory of the fpm(1) project                           ',&
'   # (where "fpm.toml" resides).                                             ',&
'                                                                             ',&
'    fpm-time                                                                 ',&
'    fpm-time run demo1 demo2                                                 ',&
'                                                                             ',&
'SEE ALSO                                                                     ',&
'    gprof(1), gcov(1)                                                        ',&
'']

version_text=[ CHARACTER(LEN=128) :: &
'version_text=[ CHARACTER(LEN=128) :: &',&
'@(#)PRODUCT:         GPF (General Purpose Fortran) utilities and examples',&
'@(#)PROGRAM:         fpm-time(1)                                         ',&
'@(#)DESCRIPTION:     Run gprof(1) in an fpm(1) package                   ',&
'@(#)VERSION:         1.0.0, 20210321                                     ',&
'@(#)AUTHOR:          John S. Urban                                       ',&
'@(#)HOME PAGE:       https://github.com/urbanjost?tab=repositories       ',&
'@(#)LICENSE:         MIT License                                         ',&
'']

end subroutine setup
end program fpm_time
