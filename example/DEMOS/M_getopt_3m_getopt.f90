          program demo_getopts
          use M_getopt, only : getopt,option_s,optarg,optopt
          implicit none

          character(len=*),parameter :: OPTIONS='ab:c'
          type(option_s):: opts(2)
             opts(1) = option_s( "alpha", .false., 'a' )
             opts(2) = option_s( "beta",  .true.,  'b' )
             do
                PARSE: select case( getopt( OPTIONS, opts ))
                case( char(0))
                   exit PARSE
                case( 'a' )
                   print *, 'option alpha/a', optarg
                case( 'b' )
                   print *, 'option beta/b=', optarg
                case( '?' )
                   print *, 'unknown option ', optopt,' not in ',OPTIONS
                   stop
                case default
                   print *, 'unhandled option c ', optopt, ' (an intentional bug)'
                end select PARSE
             end do
          end program demo_getopts
