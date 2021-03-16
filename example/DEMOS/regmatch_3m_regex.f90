          program demo_regmatch
          ! read regular expression from command line and look for it in lines read from stdin.
          use M_regex, only: regex_type, regcomp, regexec, regmatch, regfree
          implicit none
          integer                      :: command_argument_length
          character(len=:),allocatable :: command_argument
          character(len=1024)          :: input_line
          type(regex_type)             :: regex
          logical                      :: match
          integer,parameter            :: max_subexpressions=10
          integer                      :: matches(2,max_subexpressions)
          integer                      :: ios
             !find length of command argument
             call get_command_argument(number=1,length=command_argument_length)
             ! allocate a string long enough to hold the argument
             allocate(character(len=command_argument_length) :: command_argument)
             ! get the command argument
             call get_command_argument(1, command_argument)

             ! compile up regular expression
             call regcomp(regex,command_argument,'x')

             ! read lines and look for match to expression
             INFINITE: do
                read(*,'(a)',iostat=ios)input_line
                if(ios.ne.0)exit INFINITE
                ! look for a match in (remaining) string
                match=regexec(regex,input_line,matches)
                ! if no match found go for next line
                if(.not.match)cycle INFINITE
                ! show line with match
                write(*,'(a)') trim(input_line)
             enddo INFINITE

             ! free memory used for compiled regular expression
             call regfree(regex)

          end program demo_regmatch
