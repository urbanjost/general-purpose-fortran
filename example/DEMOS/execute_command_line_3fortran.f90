      program demo_execute_command_line
      implicit none
      integer :: exitstat, cmdstat
      character(len=256) :: cmdmsg

         call execute_command_line( &
         &  command  = "external_prog.exe", &
         &  exitstat = exitstat,            &
         &  cmdstat  = cmdstat,             &
         &  cmdmsg   = cmdmsg)
         print *, "Exit status of external_prog.exe was ", exitstat
         if(cmdstat.ne.0)then
            print *, '<ERROR>'//trim(cmdmsg)
         endif

         ! if asynchronous exitstat and cmdstat may not be relied on
         call execute_command_line("reindex_files.exe", wait=.false.)
         print *, "Now hopefully reindexing files in the background"

         if(cmd('dir'))then
            write(*,*)'OK'
         else
            stop 4
         endif

         ! might short-circuit or not if a command fails
         if(all(cmd([character(len=80) :: 'date','time myprg','date'])))then
             write(*,*)'good time'
         else
             write(*,*)'bad time'
         endif

         stop 'end of program'
      contains

      elemental impure function cmd(command)
      ! a functional interface for calling system commands
      use, intrinsic :: iso_fortran_env, only : &
      & stderr=>ERROR_UNIT, stdout=>OUTPUT_UNIT
      character(len=*),intent(in) :: command
      logical                     :: cmd
      logical                     :: wait
      integer                     :: exitstat
      integer                     :: cmdstat
      character(len=256)          :: cmdmsg
         wait=.false.
         exitstat=0
         cmdstat=0
         call execute_command_line(command=command,wait=wait, &
         & exitstat=exitstat,cmdstat=cmdstat,cmdmsg=cmdmsg)
         if(cmdstat.ne.0)then
            flush(stdout)
            write(stderr,'(a)')trim(cmdmsg)
            flush(stderr)
         endif
         if(exitstat.ne.0)then
            flush(stdout)
            write(stderr,'(*(g0))')'exitstat=',exitstat,':',trim(command)
            flush(stderr)
         endif
         cmd=merge(.true.,.false.,exitstat==0)
      end function cmd

      end program demo_execute_command_line
