          program demo_sget
          use M_kracken, only: kracken, sget
          implicit none
          character(len=:),allocatable :: string, a, b
            ! define command arguments and parse user command
            call kracken('demo','-string This is the default -a A default -b B default' )
            ! get any values specified on command line for -truth
            string=sget('demo_string')
            a=sget('demo_a')
            b=sget('demo_b')
            write(*,'("string is ",a)')trim(string)
            write(*,'("a is ",a)')trim(a)
            write(*,'("b is ",a)')trim(b)
          end program demo_sget
