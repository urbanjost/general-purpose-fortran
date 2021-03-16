          program demo_regcomp
          use M_regex, only: regex_type, regcomp, regexec, regfree
          use M_regex, only: regmatch
          implicit none
          type(regex_type)             :: regex
          integer                      :: matches(2,1)
          character(len=:),allocatable :: input_line
          character(len=:),allocatable :: output_line
          character(len=:),allocatable :: expression
          logical                      :: match
          integer                      :: ipass
          integer                      :: istart
             expression= "[-0-9.*/+]+"                              ! define extended regular expression
             input_line= "30*0 250*1 5 AND 6 7:and some text8 999"  ! define an input line to search for the expression
             call regcomp(regex,expression,'x')                     ! compile the regex
             ipass=1                                                ! initialize pass counter
             INFINITE: do                     ! find match, then look again in remainder of line till all matches found
                match=regexec(regex,input_line,matches)        ! look for a match in (remaining) string
                if(.not.match)exit INFINITE                    ! if no match found exit
                output_line=regmatch(1,input_line,matches)     ! use bounds in MATCHES to select the matching substring
                write(*,'(8x,*(a))') 'match="',output_line,'"' ! show match
                istart=matches(2,1)+1                          ! find beginning of remainder of string
                if(istart.gt.len(input_line))exit INFINITE     ! reached end of string
                input_line=input_line(istart:)                 ! reduce string by any previous match
                ipass=ipass+1                                  ! increment count of passes made
             enddo INFINITE                                    ! free memory used for compiled regular expression
             call regfree(regex)
          end program demo_regcomp
