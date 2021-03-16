          program demo_regfree
          use M_regex, only: regex_type, regcomp, regexec, regmatch, regfree, regerror
          type(regex_type)             :: regex
          character(len=:),allocatable :: expression
             expression= "([0-9\.\-\*\/]+)+"
             call regcomp(regex,expression,'x')
             if (istat/=0) then
               stop 'Regex runtime error: regcomp failed.'
             endif
             call regfree(regex)
          end program demo_regfree
