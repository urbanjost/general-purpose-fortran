          program demo_get_subcommand
          !*! SUBCOMMANDS
          !*! For a command with subcommands like git(1)
          !*! you can make separate namelists for each subcommand.
          !*! You can call this program which has two subcommands (run, test),
          !*! like this:
          !*!    demo_get_subcommand --help
          !*!    demo_get_subcommand run -x -y -z -title -l -L
          !*!    demo_get_subcommand test -title -l -L -testname
          !*!    demo_get_subcommand run --help
             implicit none
          !*! DEFINE VALUES TO USE AS ARGUMENTS WITH INITIAL VALUES
             real               :: x=-999.0,y=-999.0,z=-999.0
             character(len=80)  :: title="not set"
             logical            :: l=.false.
             logical            :: l_=.false.
             character(len=80)  :: testname="not set"
             character(len=20)  :: name
             call parse(name) !*! DEFINE AND PARSE COMMAND LINE
             !*! ALL DONE CRACKING THE COMMAND LINE.
             !*! USE THE VALUES IN YOUR PROGRAM.
             write(*,*)'command was ',name
             write(*,*)'x,y,z .... ',x,y,z
             write(*,*)'title .... ',title
             write(*,*)'l,l_ ..... ',l,l_
             write(*,*)'testname . ',testname
          contains
          subroutine parse(name)
          !*! PUT EVERYTHING TO DO WITH COMMAND PARSING HERE FOR CLARITY
          use M_CLI2, only : set_args, get_args, get_args_fixed_length
          use M_CLI2, only : get_subcommand
          use M_CLI2, only : CLI_RESPONSE_FILE
          character(len=*)              :: name    ! the subcommand name
          character(len=:),allocatable  :: help_text(:), version_text(:)
             CLI_RESPONSE_FILE=.true.
          ! define version text
             version_text=[character(len=80) :: &
                '@(#)PROGRAM:     demo_get_subcommand            >', &
                '@(#)DESCRIPTION: My demo program  >', &
                '@(#)VERSION:     1.0 20200715     >', &
                '@(#)AUTHOR:      me, myself, and I>', &
                '@(#)LICENSE:     Public Domain    >', &
                '' ]
              ! general help for "demo_get_subcommand --help"
              help_text=[character(len=80) :: &
               ' allowed subcommands are          ', &
               '   * run  -l -L -title -x -y -z   ', &
               '   * test -l -L -title            ', &
               '' ]
             ! find the subcommand name by looking for first word on command
             ! not starting with dash
             name = get_subcommand()
             select case(name)
             case('run')
              help_text=[character(len=80) :: &
               '                                  ', &
               ' Help for subcommand "run"        ', &
               '                                  ', &
               '' ]
              call set_args( &
              & '-x 1 -y 2 -z 3 --title "my title" -l F -L F',&
              & help_text,version_text)
              call get_args('x',x)
              call get_args('y',y)
              call get_args('z',z)
              call get_args_fixed_length('title',title)
              call get_args('l',l)
              call get_args('L',l_)
             case('test')
              help_text=[character(len=80) :: &
               '                                  ', &
               ' Help for subcommand "test"       ', &
               '                                  ', &
               '' ]
              call set_args(&
              & '--title "my title" -l F -L F --testname "Test"',&
              & help_text,version_text)
              call get_args_fixed_length('title',title)
              call get_args('l',l)
              call get_args('L',l_)
              call get_args_fixed_length('testname',testname)
             case default
              ! process help and version
              call set_args(' ',help_text,version_text)
              write(*,'(*(a))')'unknown or missing subcommand [',trim(name),']'
              write(*,'(a)')[character(len=80) ::  &
              ' allowed subcommands are          ', &
              '   * run  -l -L -title -x -y -z   ', &
              '   * test -l -L -title            ', &
              '' ]
              stop
             end select
          end subroutine parse
          end program demo_get_subcommand
