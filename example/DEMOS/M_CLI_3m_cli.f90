          program demo_M_CLI
          !-! FULL EXAMPLE ADDING HELP AND VERSION TEXT AND INTERACTIVE EXAMPLE
          use M_CLI,  only : commandline, check_commandline, unnamed
          implicit none
          integer                      :: i
          character(len=:),allocatable :: status
          character(len=255)           :: message ! use for I/O error messages
          character(len=:),allocatable :: readme  ! stores updated namelist
          character(len=:),allocatable :: help_text(:), version_text(:)
          integer                      :: ios

          real               :: x, y, z  ; namelist /args/ x, y, z
          real               :: point(3) ; namelist /args/ point
          character(len=80)  :: title    ; namelist /args/ title
          logical            :: l, l_    ; namelist /args/ l, l_
          character(len=*),parameter :: cmd=&
             ' -x 1 -y 2 -z 3 --point -1,-2,-3 --title "my title" -l F -L F '

             !-! PARSING SECTION : SHOULD NOT HAVE TO CHANGE
             call set() !-! set text values for help
             readme=commandline(cmd)
             read(readme,nml=args,iostat=ios,iomsg=message)
             call check_commandline(ios,message,help_text,version_text)
             do
                call readargs(status) ! interactively change NAMELIST group
                if(status.eq.'stop')exit
                call dosomething() ! use the NAMELIST values
             enddo
             !-! END PARSING SECTION

             !-! ALL DONE CRACKING THE COMMAND LINE.
             !-! USE THE VALUES IN YOUR PROGRAM!

             !-! THE OPTIONAL UNNAMED VALUES ON THE COMMAND LINE ARE
             !-! ACCUMULATED IN THE CHARACTER ARRAY "UNNAMED"
             if(size(unnamed).gt.0)then
                write(*,'(a)')'files:'
                write(*,'(i6.6,3a)')(i,'[',unnamed(i),']',i=1,size(unnamed))
             endif

          contains
          subroutine set()
             help_text=[character(len=80) :: &
                'NAME                                                    ', &
                '   myprocedure(1) - make all things possible            ', &
                'SYNOPSIS                                                ', &
                '   function myprocedure(stuff)                          ', &
                '   class(*) :: stuff                                    ', &
                'DESCRIPTION                                             ', &
                '   myprocedure(1) makes all things possible given STUFF ', &
                'OPTIONS                                                 ', &
                '   STUFF  things to do things to                        ', &
                'RETURNS                                                 ', &
                '   MYPROCEDURE  the answers you want                    ', &
                'EXAMPLE                                                 ', &
                '' ]

             version_text=[character(len=80) :: &
                '@(#)PROGRAM:     demo5            >', &
                '@(#)DESCRIPTION: My demo program  >', &
                '@(#)VERSION:     1.0 20200115     >', &
                '@(#)AUTHOR:      me, myself, and I>', &
                '@(#)LICENSE:     Public Domain    >', &
                '' ]
          end subroutine set
          subroutine readargs(status)
          character(len=:),intent(out),allocatable :: status
          character(len=256) :: line
          character(len=256) :: answer
          integer            :: lun
          integer            :: ios
             status=''
             write(*,'(a)')'args>> "." to run, "stop" to end,&
             & "show" to show keywords, "read","write","sh"'
             do
                write(*,'(a)',advance='no')'args>>'
                read(*,'(a)')line
                if(line(1:1).eq.'!')cycle
                select case(line)
                 case('.')
                   exit
                 case('show')
                   write(*,*)'SO FAR'
                   write(*,nml=args)
                   !-! something where you could restrict nml output to just
                   !-! listed names would be nice
                   !-!write(*,nml=args)['A','H']
                   !-!write(*,nml=*NML)args['A','H']
                 case('help')
                 write(*,'(a)')[character(len=80) :: &
                 ' You are in interactive mode where you can display and change&
                 & your values using', &
                 ' NAMELIST syntax:', &
                 '  KEYWORD=VALUE(S) - change a variable value', &
                 '  show             - show current values', &
                 '  stop             - stop program', &
                 '  .                - return to program and run', &
                 '  write FILENAME   - write NAMELIST group to specified file',&
                 '  read  FILENAME   - read NAMELIST input file', &
                 '  sh               - start shell process', &
                 '', &
                '' ]
                 case('stop')
                   status='stop'
                   exit
                 case('sh')
                   call execute_command_line('bash')
                 case('read')
                   write(*,'(a)',advance='no')'filename:'
                   read(*,'(a)',iostat=ios)answer
                   if(ios.ne.0)exit
                   open(file=answer,iostat=ios,newunit=lun)
                   if(ios.ne.0)exit
                   read(lun,args,iostat=ios)
                   close(unit=lun,iostat=ios)
                 case('write')
                   write(*,'(a)',advance='no')'filename:'
                   read(*,'(a)',iostat=ios)answer
                   if(ios.ne.0)exit
                   open(file=answer,iostat=ios,newunit=lun)
                   if(ios.ne.0)exit
                   write(lun,args,iostat=ios)
                   close(unit=lun,iostat=ios)
                 case default
                   UPDATE: block
                      character(len=:),allocatable :: intmp
                      character(len=256)  :: message
                      integer :: ios
                      intmp='&ARGS '//trim(line)//'/'
                      read(intmp,nml=args,iostat=ios,iomsg=message)
                      if(ios.ne.0)then
                         write(*,*)'ERROR:',trim(message)
                      endif
                   endblock UPDATE
                end select
             enddo
          end subroutine readargs
          subroutine dosomething()
             ! placeholder
             write(*,*)'USE ALL THOSE VALUES'
          end subroutine dosomething

          end program demo_M_CLI
