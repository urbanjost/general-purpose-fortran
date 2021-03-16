           program demo_match
           use :: M_BRE, only : getpat, match
           use :: M_BRE, only : MAXPAT, MAXARG, MAXLINE, YES, ERR
           implicit none
           ! find _ find patterns in text
           integer                      :: pat(MAXPAT)
           character(len=MAXARG-1)      :: argument
           integer                      :: stat
           integer                      :: ios
           integer                      :: len_arg
           character(len=MAXLINE-2)     :: line
           call get_command_argument(1, argument,status=stat,length=len_arg)
           if(stat.ne.0.or.argument.eq.'')then
              write(*,*)"usage: find pattern."
           elseif(getpat(argument(:len_arg), pat) .eq. ERR) then
              write(*,*)"illegal pattern."
           else
              INFINITE: do
                 read(*,'(a)',iostat=ios)line
                 if(ios.ne.0)exit
                 if(match(trim(line), pat) .eq. YES) then
                    write(*,'(*(a))')trim(line)
                 endif
              enddo INFINITE
           endif
           end program demo_match
