      program demo_amatch
      use :: M_match, only : getpat, amatch
      use :: M_match, only : MAXPAT, MAXARG, MAXLINE, MAXTAGS, YES, ERR
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
      call get_command_argument(1, argument,status=stat,length=len_arg)
      if(stat.ne.0.or.argument.eq.'')then
         write(*,*)"usage: find pattern."
      elseif(getpat(argument(:len_arg), pat) .eq. ERR) then
         write(*,*)"illegal pattern."
      else
         INFINITE: do
            read(*,'(a)',iostat=ios)line
            if(ios.ne.0)exit
            loc = amatch(trim(line), 1, pat) ! returns location/0
            if(loc.gt.0)then ! matched; if no match, loc is returned as 0
               write(*,'(*(a))')trim(line)
            endif
         enddo INFINITE
      endif
      end program demo_amatch
