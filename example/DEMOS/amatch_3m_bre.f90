           program demo_amatch
           use :: M_BRE, only : getpat, amatch
           use :: M_BRE, only : MAXPAT, MAXARG, MAXLINE, MAXTAGS, YES, ERR
           implicit none
           ! find _ find patterns in text
           integer                      :: pat(MAXPAT)
           character(len=MAXARG-1)      :: argument
           integer                      :: stat
           integer                      :: ios
           integer                      :: len_arg
           integer                      :: loc
           integer                      :: ii
           character(len=MAXLINE-2)     :: line
           integer                      :: tagbeg(MAXTAGS),tagend(MAXTAGS)
           call get_command_argument(1, argument,status=stat,length=len_arg)
           if(stat.ne.0.or.argument.eq.'')then
              write(*,*)"usage: find pattern."
           elseif(getpat(argument(:len_arg), pat) .eq. ERR) then
              write(*,*)"illegal pattern."
           else
              INFINITE: do
                 read(*,'(a)',iostat=ios)line
                 tagbeg=-9999;tagend=-9999
                 if(ios.ne.0)exit
                 loc = amatch(trim(line), 1, pat, tagbeg, tagend) ! returns location/0
                 if(loc.gt.0)then ! matched; if no match, loc is returned as 0
                    write(*,'(*(a))')trim(line)
                    ! (element "i + 1" returns start or end, respectively, of "i"th tagged subpattern)
                    write(*,'(*(i0,1x,i0,1x,i0,/))')(ii,tagbeg(ii),tagend(ii),ii=1,size(tagbeg))
                 endif
              enddo INFINITE
           endif
           end program demo_amatch
