          program demo_regerror
          use M_regex, only: regex_type, regcomp, regexec, regmatch, regfree, regerror
          type(regex_type)             :: regex
          integer,parameter            :: maxmatch=10
          integer                      :: matches(2,maxmatch)

          character(len=:),allocatable :: input_line
          character(len=:),allocatable :: expression
          logical                      :: match

             expression= "([0-9\.\-\*\/]+)+"
             expression= "([0-9\.\-\*\/+)+"  ! intentionally bad RE (Regular Expression)
             input_line= "30*0 250*1 5 6 7"
             call regcomp(regex,expression,'x',status=istat)
             if (istat/=0) then
               write(*,'("Regex runtime error in regcomp(3f):",a,", expression=",a)') regerror(regex,istat),expression
               stop 1
             endif
             match=regexec(regex,input_line,matches,status=istat)
             if (istat/=0) then
               write(*,'("Regex runtime error in regexec:(3f)",a)') regerror(regex,istat)
               stop 2
             endif
             if(match)then
                do i=1,maxmatch
                   if(matches(1,i).le.0)exit
                   write(*,*) 'match="',regmatch(i,input_line,matches),'"'
                enddo
             endif
             call regfree(regex)

              end program demo_regerror
