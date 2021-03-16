          program demo_regsub
          use M_regex, only: regex_type, regcomp, regexec, regerror, regmatch, regfree, regsub
          use M_strings, only : replace
          implicit none
          type(regex_type)             :: regex
          integer,parameter            :: maxmatch=10
          integer                      :: matches(2,maxmatch)
          character(len=:),allocatable :: input_line(:)
          character(len=:),allocatable :: output_line
          character(len=:),allocatable :: expression
          logical                      :: match
          integer                      :: stat
          integer                      :: i
          logical                      :: BRE
             ! The /etc/passwd file is a colon-separated file that contains the following information:
             !
             !     User name.
             !     Encrypted password.
             !     User ID number (UID)
             !     User's group ID number (GID)
             !     Full name of the user (GECOS)
             !     User home directory.
             !     Login shell.
             !
             BRE=.true.
             if(BRE)then  ! BRE (Basic Regular Expression)
                expression= '\([^:]*\):*\([^:]*\):*\([^:]*\):*\([^:]*\):*\([^:]*\):*\([^:]*\):*\([^:]*\):*.*'
                call regcomp(regex,expression,status=stat)
             else         ! ERE (Extended Regular Expression)
                expression= '([^:]*):*([^:]*):*([^:]*):*([^:]*):*([^:]*):*([^:]*):*([^:]*):*.*'
                call regcomp(regex,expression,'x',status=stat)
             endif

             if(stat.ne.0)then                                        ! if RE did not compile report error
                write(*,*)'*regcomp* ERROR:',regerror(regex,stat)
                stop 1
             endif

             ! simulate /etc/password file in array, with common aberrant input lines included
             input_line= [character(len=128) ::                                     &
             "doeqj:xxxxx:1001:200:John Q. Doe:/home/doeqj:/bin/tcsh: and more",    &
             "doeqj:xxxxx:1001:200:John Q. Doe:/home/doeqj:/bin/tcsh: and more:",   &
             "doeqj: :1001: :John Q. Doe:/home/doeqj:/bin/tcsh:",                   &
             "doeqj:xxxxx:1001:200:John Q. Doe:/home/doeqj:/bin/tcsh:",             &
             "doeqj:xxxxx:1001:200:John Q. Doe:/home/doeqj:/bin/tcsh",              &
             "doeqj:xxxxx:1001:200:John Q. Doe:/home/doeqj",                        &
             "doeqj:xxxxx:",                                                        &
             "doeqj:xxxxx",                                                         &
             "doeqj:",                                                              &
             "doeqj",                                                               &
             ":::::::::::::::",                                                     &
             ":::",                                                                 &
             "" ]

             do i=1,size(input_line)
                input_line=replace(input_line(i),'::',': :')          ! the RE shown needs the field to have at least one character
                match=regexec(regex,input_line(i),matches)            ! generate the matches array using the compiled RE

                write(*,'(a)')repeat('-',80)                          ! put out a number line
                write(*,'(a)') input_line(i)
                                                                      ! replace \n lines
                call regsub(input_line(i),matches,'username="\1" &
                &password="\2" UID="\3" GID="\4" name="\5" &
                &home="\6" shell="\7" ',output_line)
                write(*,'(a)') output_line

                if(i.eq.1)then                                        ! show other examples of formatting for first entry
                   call regsub(input_line(i),matches,&
                   & 'The username for \5 is "\1"',output_line)
                   write(*,'(a)') output_line

                   call regsub(input_line(i),matches,&
                   & 'username "\1" has UID=\3, GID=\4 &
                   & and default shell "\7"',output_line)
                   write(*,'(a)') output_line
                endif
             enddo

             call regfree(regex)

          end program demo_regsub
