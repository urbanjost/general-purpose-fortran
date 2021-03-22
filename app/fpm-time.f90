program fpm_time
use M_CLI2,    only : set_args, sget, sgets, unnamed
use M_strings, only : join
character(len=:),allocatable :: subcommand
character(len=:),allocatable :: command
character(len=:),allocatable :: which(:)
character(len=*),parameter   :: gprof="'gprof --demangle --flat-profile'"
character(len=*),parameter   :: opts="'-O0 -pg -fPIC -fcoarray=single'"
character(len=:),allocatable :: help_text(:)
character(len=:),allocatable :: version_text(:)
   call setup()
   call set_args('--target "*"',help_text,version_text)
   if(size(unnamed).eq.0)then
      unnamed=['test']
   endif
   subcommand=trim(unnamed(1))
   which=sgets('target')
   if(size(unnamed).gt.1)then
      which=[character(len=max(len(which),len(unnamed))) :: which,unnamed(2:)]
   endif

   ! build and run the specified programs with profiling turned on
   command=                                     &
    & join(which,                               &
    & start='fpm '//subcommand//' ',            &
    & left="'",right="'",sep=' ',               &
    & end=' --compiler gfortran --flag '//opts)

   write(*,'("+",a)')trim(command)
   call execute_command_line(command)

   ! run gprof with each program name
   command=                                     &
    & 'fpm '//subcommand//' '//                 &
    & join(which,left="'",right="'",sep=' ')//  &
    & ' --compiler gfortran --flag '//          &
    & opts//                                    &
    & ' --runner '//                            &
    & gprof

   write(*,'("+",a)')trim(command)
   call execute_command_line(command)

contains
subroutine setup()
help_text=[ CHARACTER(LEN=128) :: &
'NAME                                                                            ',&
'  fpm-time(1) - call fpm(1) with gprof(1) to generata a flat timing profile     ',&
'SYNOPIS                                                                         ',&
'  fpm-time [subcommand] [--target] targets                                      ',&
'DESCRIPTION                                                                     ',&
'  Run the fpm(1) command with the gfortran(1) compiler and compiler flags       ',&
'  required to build instrumented programs which will generate gprof(1)          ',&
'  output files. Run the programs and then run a basic gprof(1) command          ',&
'  on each output.                                                               ',&
'                                                                                ',&
'OPTIONS                                                                         ',&
'   subcommand  fpm(1) subcommand used to run a program (test,run). If           ',&
'               no options are specified the default is "test".                  ',&
'   targets     which targets to run. The default is "*".                        ',&
'                                                                                ',&
'EXAMPLE                                                                         ',&
'   # in the parent directory of the fpm(1) project                              ',&
'   # (where "fpm.toml" resides).                                                ',&
'                                                                                ',&
'    fpm-time                                                                    ',&
'    fpm-time run demo1 demo2                                                    ',&
'                                                                                ',&
'SEE ALSO                                                                        ',&
'    gprof(1), gcov(1)                                                           ',&
'']
!>
!!##NAME
!!   fpm-time(1) - call fpm(1) with gprof(1) to generata a flat timing profile
!!##SYNOPIS
!!   fpm-time [subcommand] [--target] targets
!!##DESCRIPTION
!!   Run the fpm(1) command with the gfortran(1) compiler and compiler flags
!!   required to build instrumented programs which will generate gprof(1)
!!   output files. Run the programs and then run a basic gprof(1) command
!!   on each output.
!!
!!##OPTIONS
!!    subcommand  fpm(1) subcommand used to run a program (test,run). If
!!                no options are specified the default is "test".
!!    targets     which targets to run. The default is "*".
!!
!!##EXAMPLE
!!
!!    # in the parent directory of the fpm(1) project
!!    # (where "fpm.toml" resides).
!!
!!     fpm-time
!!     fpm-time run demo1 demo2
!!
!!##SEE ALSO
!!     gprof(1), gcov(1)

version_text=[ CHARACTER(LEN=128) :: &
'@(#)PRODUCT:         GPF (General Purpose Fortran) utilities and examples       ',&
'@(#)PROGRAM:         fpm-time(1)                                                ',&
'@(#)DESCRIPTION:     Run gprof(1) in an fpm(1) package                          ',&
'@(#)VERSION:         1.0.0, 20210321                                            ',&
'@(#)AUTHOR:          John S. Urban                                              ',&
'@(#)HOME PAGE:       https://github.com/urbanjost?tab=repositories              ',&
'@(#)LICENSE:         MIT License                                                ',&
'']

end subroutine setup
end program fpm_time
