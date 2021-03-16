          program demo_prime_factors
          use M_factor, only : prime_factors
          implicit none
             integer  :: number
             integer  :: iexp(10), iprm(10), nprm
             logical  :: verbose=.true.
             integer  :: ios
             do
               write(*,'(a)', advance='no') ' Enter number to be factored: '
               read(*,*,iostat=ios,end=999) number
               if(ios.eq.0)then
                  call prime_factors(number, nprm, iprm, iexp, verbose)
               endif
             enddo
          999 continue
          end program demo_prime_factors
