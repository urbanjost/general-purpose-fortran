program fcmd
use, intrinsic :: iso_fortran_env, only : stderr=>ERROR_UNIT
use M_CLI2, only : set_args, sgets, sget, lget, leafs=>unnamed, specified
use M_strings, only : split, lower
use M_system, only  : system_access, R_OK, W_OK, X_OK, F_OK, system_getenv, system_dir, system_isdir
use M_io,      only : joinpath, basename, rd
implicit none
character(len=:),allocatable    :: searchpath
character(len=:),allocatable    :: directories(:)
character(len=:),allocatable    :: pathnames(:)
character(len=:),allocatable    :: pathname
character(len=:),allocatable    :: cmd
character(len=:),allocatable    :: cmds(:)
character(len=:),allocatable    :: name
integer                         :: path_line_length
integer                         :: i, j, k, m
integer                         :: icount
logical                         :: all
logical                         :: verbose
logical                         :: wild
logical                         :: long
logical                         :: interactive
logical                         :: tstfor
logical                         :: ignorecase
character(len=:),allocatable    :: help(:),version(:)
character(len=:),allocatable    :: command
integer                         :: exitstat
integer                         :: cmdstat
character(len=256)              :: cmdmsg
   ! process command-line options
   call setup()
   call set_args('fcmd --vi F --first:f F --cmd:c " " --ignorecase:i F --wild:w F --ok F --test:t F --long:l',help,version)
   all=.not.lget('first')
   wild=lget('wild')
   interactive=lget('ok')
   tstfor=lget('test')
   ignorecase=lget('ignorecase')
   verbose=lget('verbose')

   cmd=''
   if(specified('vi')) cmd=system_getenv('FCEDIT',system_getenv('EDITOR',system_getenv('VISUAL','vi')))
   if(specified('cmd'))then
      cmd=cmd//':'//sget('cmd')
      if(sget('cmd')==':') cmd=system_getenv('FCEDIT',system_getenv('EDITOR',system_getenv('VISUAL','vi')))
   endif
   if(lget('long')) cmd=cmd//':ls -l'

   call get_environment_variable(name="PATH", length=path_line_length)  ! get length of $PATH
   allocate(character(len=path_line_length) :: searchpath)              ! make a string variable long enough to hold $PATH
   call get_environment_variable(name="PATH", value=searchpath)         ! get value of environment variable $PATH
   if(index(searchpath,';').eq.0)then
      call split(searchpath,directories,':')                               ! create array of directory names in $PATH
   else
      call split(searchpath,directories,';')                               ! create array of directory names in $PATH
   endif

   if(size(leafs).eq.0)then
       !write(*,'(i3," ",a)')(i,trim(directories(i)),i=1,size(directories))
       write(*,'(a)')(trim(directories(i)),i=1,size(directories))
       stop
   endif
   call split(cmd,cmds,';:')
   if(size(cmds).eq.0)cmds=['']
   if(verbose)then
      write(stderr,'("cmds>>",g0)')cmds
      write(stderr,'("leafs>>",g0)')leafs
      write(stderr,'("all>>",g0)')all
   endif
   icount=0
   NAMESLOOP: do j=1,size(leafs)                                        ! try name appended to each directory name
      if(wild)then
         name='*'//trim(leafs(j))//'*'
      else
         name=trim(leafs(j))
      endif
      DIRLOOP: do i=1,size(directories)
         if(.not.system_isdir(trim(directories(i))))cycle
         pathnames=system_dir(trim(directories(i)),name,ignorecase=ignorecase)
         do m=1,size(pathnames)
            pathname=trim(joinpath(directories(i),pathnames(m)))
            if(system_isdir(pathname))cycle
            select case(basename(pathname))
            case('.','..','')
            case default
               if(system_access(pathname,X_OK))then
                  do k=1,size(cmds)
                     if(cmds(k).eq.'')then
                        if(tstfor)then
                           write(*,'(a)')basename(pathname)
                           stop
                        else
                           write(*,'(a)')pathname
                        endif
                     else
                        command=cmds(k) // ' ' // pathname
                        if(interactive)then
                           if(rd('execute "'//command//'" ?',.false.))then
                              call execute_command_line(command, exitstat=exitstat, cmdstat=cmdstat, cmdmsg=cmdmsg)
                              if(exitstat.ne.0.or.cmdstat.ne.0)then
                                 write(*,'(*(g0))')trim(cmdmsg)
                              endif
                           endif
                        else
                           call execute_command_line(command, exitstat=exitstat, cmdstat=cmdstat, cmdmsg=cmdmsg)
                           if(exitstat.ne.0.or.cmdstat.ne.0)then
                              write(*,'(*(g0))')trim(cmdmsg)
                           endif
                        endif
                     endif
                  enddo
                  if(.not.all)exit DIRLOOP
               endif
            end select
         enddo
      enddo DIRLOOP
   enddo NAMESLOOP
   if(tstfor)stop 1
contains
subroutine setup()
help=[ CHARACTER(LEN=128) :: &
'NAME',&
'   fcmd(1f) - [FUNIX:FILESYSTEM] find the pathname of commands and',&
'              optionally perform commands on them.',&
'   (LICENSE:MIT)',&
'',&
'SYNOPSIS',&
'    fcmd [commands(s) [--wild] [ --first][--ignorecase][--test]',&
'    [ --cmd COMMAND;COMMAND,COMMAND;... ]|[--long]|[--vi]',&
'    [ --help|--version]',&
'',&
'DESCRIPTION',&
'   fcmd(1f) takes one or more command names. For each of its arguments',&
'   by default it prints to stdout the path of the executables that',&
'   would have been executed when this argument had been entered at the',&
'   shell prompt. It does this by searching for an executable or script',&
'   in the directories listed in the environment variable PATH.',&
'',&
'   Optionally, commands can be specified to act on the path names found.',&
'',&
'OPTIONS',&
'   If no options are supplied the current search path is displayed one directory',&
'   per line.',&
'',&
'    command(s)      names of commands to locate. simple globbing with *',&
'                    and ? is allowed if the names are quoted.',&
'    --ignorecase,i  ignore case of input command(s)',&
'    --first,-f  locate first match of each executable name expression, not all.',&
'    --cmd,-c    invoke the command on the files found. If present with',&
'                no parameter the desired command is assumed to be',&
'                the default editor (useful for finding and looking',&
'                at scripts). The editor command is looked for in the',&
'                environment variables FCEDIT, EDITOR and then VISUAL.',&
'                if not found, "vi" is used.',&
'',&
'                Multiple commands delimited by a semi-colon and/or a colon',&
'                may be used.',&
'',&
'                Abbreviations for common --cmd options:',&
'',&
'                --long,l   abbreviation for "--cmd ''ls -l''"',&
'                --vi       abbreviation for "--cmd ''vim''"',&
'',&
'    --ok        Prompt for a y/n answer before executing the list of',&
'                commands on each file found.',&
'    --test,-t   print first command found and stop',&
'    --wild,-w   add asterisk as a suffix and prefix to all command names',&
'                being searched for.',&
'    --version,-v  Print version information on standard output then',&
'                  exit successfully.',&
'    --help,-h   Print usage information on standard output then',&
'                exit successfully.',&
'EXAMPLE',&
'   Sample commands',&
'',&
'    fcmd ls           # find path to ls(1) command',&
'    fcmd ''*sum*''      # find all commands containing "sum"',&
'    fcmd sum -w       # also find all commands containing "sum"',&
'    fcmd ''*''          # list all commands in search path',&
'    fcmd gunzip -c    # edit the script gunzip(1)',&
'    fcmd ls dir       # find both commands',&
'',&
'    # find all commands in path and a man-page if they have one',&
'    fcmd ''*'' -c whereis',&
'',&
'    # find a command and use the commands file(1) and stat(1) on the',&
'    # pathnames found.',&
'    fcmd pwd -c ''file;stat''',&
'',&
'    Common commands to use are "cat -vet", "ls -l", "strings", "what",',&
'    "sum", "whereis", "stat", "wc", "ldd", and "file".',&
'',&
'    #!/bin/bash',&
'    #@(#)  find which command is available and view file',&
'    FILE="$1"',&
'    case "$(fcmd -t w3m lynx links)" in',&
'    w3m) w3m $FILE;;',&
'    lynx) lynx $FILE;;',&
'    links) links $FILE;;',&
'    *) echo ''no browser found'';exit;;',&
'    esac',&
'',&
'SEE ALSO',&
'    which(1), xargs(1)',&
'']
version=[ CHARACTER(LEN=128) :: &
'PRODUCT:        GPF (General Purpose Fortran) utilities and examples',&
'PROGRAM:        fcmd(1f)',&
'DESCRIPTION:    list pathnames of command names that are executable in $PATH',&
'VERSION:        1.1, 2023-01-21',&
'AUTHOR:         John S. Urban',&
'LICENSE:        MIT',&
'']
end subroutine setup
end program fcmd
