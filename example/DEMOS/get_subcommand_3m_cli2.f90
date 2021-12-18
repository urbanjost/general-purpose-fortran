     program demo_get_subcommand
     !x! SUBCOMMANDS
     !x! For a command with subcommands like git(1)
     !x! you can make separate namelists for each subcommand.
     !x! You can call this program which has two subcommands (run, test),
     !x! like this:
     !x!    demo_get_subcommand --help
     !x!    demo_get_subcommand run -x -y -z -title -l -L
     !x!    demo_get_subcommand test -title -l -L -testname
     !x!    demo_get_subcommand run --help
        implicit none
     !x! DEFINE VALUES TO USE AS ARGUMENTS WITH INITIAL VALUES
        real               :: x=-999.0,y=-999.0,z=-999.0
        character(len=80)  :: title="not set"
        logical            :: l=.false.
        logical            :: l_=.false.
        character(len=80)  :: testname="not set"
        character(len=20)  :: name
        call parse(name) !x! DEFINE AND PARSE COMMAND LINE
        !x! ALL DONE CRACKING THE COMMAND LINE.
        !x! USE THE VALUES IN YOUR PROGRAM.
        write(*,*)'command was ',name
        write(*,*)'x,y,z .... ',x,y,z
        write(*,*)'title .... ',title
        write(*,*)'l,l_ ..... ',l,l_
        write(*,*)'testname . ',testname
     contains
     subroutine parse(name)
     !x! PUT EVERYTHING TO DO WITH COMMAND PARSING HERE FOR CLARITY
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
