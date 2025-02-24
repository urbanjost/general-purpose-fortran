program gdb
use M_CLI2, only : set_args, sgets, sget, iget, rget, dget, lget, leafs=>unnamed, specified
use M_process ,ONLY: process_open_read, process_readline, streampointer, process_close
implicit none
logical                      :: verbose
character(len=:),allocatable :: help(:),version(:)
character(len=:),allocatable :: cmd
character(len=:),allocatable :: dir
character(len=:),allocatable :: cmd_gdb
character(len=:),allocatable :: sub_command
integer :: wide
integer :: i
type(streampointer) :: fp ! C file pointer returned by process_open()
character(len=4096) :: line
integer ierr
character(len=*),parameter :: common_options='&
 & --example F&
 & --test F&
 & --no-prune F&
 & --link-flag " "&
 & --flag " "&
 & --directory:C " "&
 & --compiler "gfortran"&
 & --c-flag " "&
 & --c-compiler " "&
 & --archiver " "&
 & --gdb " "'
character(len=:),allocatable :: options
   ! process command-line options
   call setup()
   call set_args(' --wide:w F '//common_options,help,version)
   verbose=lget('verbose')
   cmd_gdb=sget('gdb')
   options=''
   if( specified('no-prune')  )options=options//' --noprune '
   if( specified('directory') )options=options//' --directory '//sget('directory')
   if( specified('compiler')  )options=options//' --compiler '//sget('compiler')
   if( specified('link-flag') )options=options//' --link-flag "'//sget('link-flag')//'"'
   if( specified('flag')      )options=options//' --flag "'//sget('flag')//'"'
   if( specified('c-flag')    )options=options//' --c-flag "'//sget('c-flag')//'"'
   if( specified('archiver')  )options=options//' --archiver "'//sget('archiver')//'"'
   if( lget('test') )then
      sub_command='test'
   else
      sub_command='run'
      if( specified('example')   )options=options//' --example '
   endif

   if( size(leafs) == 0 )leafs=['']
   do i = 1, size(leafs)

      ! need to make a call to the fpm API to get the program cleanly.
      cmd = 'fpm '//sub_command//' '//trim(leafs(i) )//options//" --profile debug --runner|grep '^build'"
      ! note the process_* procedures are currently available only on POSIX platforms
      ! could use system commands based on OS or possibly get M_process to support
      ! more platforms
      call process_open_read(cmd,fp,ierr) ! open process to read from
      if( ierr == 0 )then
        call process_readline(line,fp,ierr) ! read a line from the process
        if( ierr /= 0 ) stop 'error processing command'//trim(cmd)
        call process_close(fp,ierr)
      endif
      cmd='fpm '//sub_command//' '//trim(leafs(i) )//options//' --profile debug --runner "'
      cmd=cmd//"vim -c 'set mouse=a' "
      if( lget('wide') )then
          cmd=cmd//" -c 'let g:termdebug_wide=1'"
      endif
      cmd=cmd//" -c 'packadd termdebug' "
      cmd=cmd//" -c 'resize +10' "
      cmd=cmd//" -c ':tnoremap <F1> <C-W>N' "
      cmd=cmd//" -c 'Termdebug "//trim(line)//"' "
      if( cmd_gdb /= '' )then
         cmd = cmd//"-c \""call TermDebugSendCommand('"//cmd_gdb//"')\"" "
      endif
      ! assuming in app/ and that .f90 or .F90 file; could leave this off or generalize
      if( sub_command == 'test' )then
              dir='test'
      elseif( specified('example') )then
              dir='example'
      else
              dir='app'
      endif
      if( leafs(i) == '' )then
         cmd=cmd//' '//dir//'/*.[fF]90"'
      else
         cmd=cmd//' '//dir//'/'//leafs(i)//'.[fF]90"'
      endif
      if( lget('verbose') )then
         write(*,*)trim(cmd)
      endif
      call execute_command_line(cmd)
   enddo
contains
subroutine setup()
help=[ CHARACTER(LEN=128) :: &
'NAME',&
'   fpm-gdb(1f) - [FUNIX:FILESYSTEM] launch gdb(1) in vim(1) from fpm(1)',&
'   (LICENSE:MIT)                                                       ',&
'                                                                       ',&
'SYNOPSIS                                                               ',&
'    fpm-gdb [PROGRAM][OPTIONS][ --help|--version]                      ',&
'                                                                       ',&
'DESCRIPTION                                                            ',&
'   fpm-gdb(1f) is an fpm(1) plugin that starts up gdb(1) in the vim(1) ',&
'   editor.                                                             ',&
'                                                                       ',&
'   It uses the vim(1) terminal feature. The terminal feature is optional.',&
'   Enter this in vim(1) to check if your version has it:                 ',&
'                                                                         ',&
'       :echo has(''terminal'')                                           ',&
'                                                                         ',&
'   If the result is "1" you have it.                                     ',&
'                                                                         ',&
'                                                                         ',&
'OPTIONS                                                                  ',&
'    PROGRAM       if more than one application is built by the package   ',&
'                  the name must be specified. Unlike with the "fpm run"  ',&
'                  command wildcards are not permitted.                   ',&
'    --gdb CMDS    pass initial commands to gdb(1)                        ',&
'    -wide,-w      assume a wide screen width. Wide mode places the       ',&
'                  code in a window on the left of the screen. <C-W>      ',&
'                  followed by one of {RHKLJ} can change the window       ',&
'                  layout.                                                ',&
'    --test        use subcommand "test" instead of the default "run"     ',&
'                  in order to select test programs. If --test is specified',&
'                  --example is ignored.                                   ',&
'                                                                          ',&
'    --verbose,-V  verbose mode                                            ',&
'    --version,-v  Print version information on standard output then       ',&
'                  exit successfully.                                      ',&
'    --help,-h     Print usage information on standard output then         ',&
'                  exit successfully.                                      ',&
'                                                                          ',&
'   In addition, the following options from the fpm(1) "run" subcommand    ',&
'   are supported, noting that "--profile debug" is always specified as    ',&
'   well ...                                                               ',&
'                                                                          ',&
'       --example    --no-prune  --link-flag  --flag                       ',&
'       --directory  --compiler  --c-flag     --c-compiler                 ',&
'       --archiver                                                         ',&
'                                                                          ',&
'GETTING STARTED                                                           ',&
'Lets start in a terminal at least 132 characters wide and enter           ',&
'                                                                          ',&
'     fpm gdb --wide                                                       ',&
'                                                                          ',&
'and then in the gdb(1) command window enter                               ',&
'                                                                          ',&
'    b main                                                                ',&
'    info sources                                                          ',&
'    info functions                                                        ',&
'    b file.F90:routine                                                    ',&
'    run                                                                   ',&
'                                                                          ',&
'This will set a breakpoint at the beginning of the program, list the files',&
'in scope for debugging, list the procedures, set some other breakpoint    ',&
'in file "file.F90" at top of procedure "routine" and then start running   ',&
'the program (with optional arguments).  Clicking on "next" would take     ',&
'you to the next breakpoint.                                               ',&
'                                                                          ',&
'For some compilers "b 1"(e.g. Intel) might be required instead of "b      ',&
'main"(e.g gfortran).                                                      ',&
'                                                                          ',&
'Many other commands exist.  Assuming you are at your next breakpoint,     ',&
'you can ask where you are, list local variables and print the value of    ',&
'some variable, like "i"                                                   ',&
'                                                                          ',&
'    where                                                                 ',&
'    info locals                                                           ',&
'    print i                                                               ',&
'                                                                          ',&
'USING THE MOUSE                                                           ',&
'                                                                          ',&
'Assuming your terminal window supports vim(1) mouse mode, you can use     ',&
'the mouse in various ways. For example                                    ',&
'                                                                          ',&
'   TO SET BREAK POINTS                                                    ',&
'                                                                          ',&
'   If you click the right mouse in the code file you should get an option ',&
'   menu for setting and clearing breakpoints.                             ',&
'                                                                          ',&
'   TO EVALATE VARIABLES                                                   ',&
'                                                                          ',&
'   click on variables to highlight them and click the [eval] icon.        ',&
'                                                                          ',&
'SCROLLING                                                                 ',&
'                                                                          ',&
'The gdb and output windows will not be in Normal mode and so              ',&
'will not scroll by default. When focus is on the window that will not     ',&
'scroll enter "ctrl-W N" to go to scrollable, and enter "i" to return      ',&
'to the original mode.                                                     ',&
'                                                                          ',&
'On some platforms instead of "ctrl-W" followed by capital "N" you can     ',&
'define which key goes to Normal mode. For example, to define F1 to        ',&
'switch to Terminal-Normal mode:                                           ',&
'                                                                          ',&
'     :tnoremap <F1> <C-W>N                                                ',&
'                                                                          ',&
'In the gdb window in particular, you probably want to toggle between      ',&
'the modes, because when scrolling is on command recall is not.            ',&
'                                                                          ',&
'When in Normal mode your interaction with the program is suspended,       ',&
'so you want to return to the original mode or you cannot enter commands   ',&
'in the gdb pane and cannot see new output or enter input in Normal mode.  ',&
'To leave scrollable mode (enter "i") in the pane.                         ',&
'                                                                          ',&
'Check out :help window-moving for more information on changing the        ',&
'window layout.                                                            ',&
'                                                                          ',&
'MORE INFO                                                                 ',&
'                                                                          ',&
'General gdb instructions are beyond the scope of this discussion, but     ',&
'"help" in the gdb pane can get you started. At least look at watch,       ',&
'display, list, break, info, where, print, set, next, step, and info after ',&
'you get started with the basics.                                          ',&
'                                                                          ',&
'For the vim(1) terminal help go to the rightmost vim(1) window and enter  ',&
'":help terminal-debug".                                                   ',&
'                                                                          ',&
'EXAMPLES                                                                  ',&
'     fpm gdb                                                              ',&
'                                                                          ',&
'     fpm gdb --compiler gfortran                                          ',&
'                                                                          ',&
'     # run with initial gdb(1) commands in a file                         ',&
'     fpm gdb -wide -gdb ''source mycmds.gdb''                             ',&
'                                                                          ',&
'     fpm gdb --example demo1                                              ',&
'                                                                          ',&
'SEE ALSO                                                                  ',&
'    gdb(1), fpm(1), vim(1)                                                ',&
'                                                                          ',&
'    + fpm run --runner ddd                                                ',&
'    + fpm run --runner gdbgui                                             ',&
'    + fpm run --runner gdbtui                                             ',&
'    + fpm run --runner gdb                                                ',&
'']
version=[ CHARACTER(LEN=128) :: &
'PRODUCT:        GPF (General Purpose Fortran) utilities and examples',&
'PROGRAM:        fpm-gdb(1f)                                         ',&
'DESCRIPTION:    fpm(1) plugin that launches gdb(1)                  ',&
'VERSION:        2.0, 2022-04-29                                     ',&
'AUTHOR:         John S. Urban                                       ',&
'LICENSE:        MIT                                                 ',&
'']
end subroutine setup
end program gdb
