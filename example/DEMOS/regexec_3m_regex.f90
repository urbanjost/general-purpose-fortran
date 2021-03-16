          program demo_regexec
          ! read regular expression from command line and look for it in lines read from stdin.
          use M_regex, only: regex_type, regcomp, regexec, regfree
          implicit none
          integer                      :: command_argument_length
          character(len=:),allocatable :: command_argument
          character(len=1024)          :: input_line
          type(regex_type)             :: regex
          logical                      :: match
          integer                      :: ios
             call get_command_argument(number=1,length=command_argument_length)
             allocate(character(len=command_argument_length) :: command_argument)
             call get_command_argument(1, command_argument)
             call regcomp(regex,command_argument,'xn') ! compile up regular expression
             INFINITE: do
                read(*,'(a)',iostat=ios)input_line
                if(ios.ne.0)exit INFINITE
                match=regexec(regex,input_line) ! look for a match in (remaining) string
                if(.not.match)cycle INFINITE    ! if no match found go for next line
                write(*,'(a)') trim(input_line) ! show line with match
             enddo INFINITE
             call regfree(regex)                ! free memory used for compiled regular expression
          end program demo_regexec
