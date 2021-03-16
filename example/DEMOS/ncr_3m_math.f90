          program demo_ncr
          use m_math, only : ncr
          implicit none
          integer, parameter  :: dp = selected_real_kind(12, 60)
          integer             :: n, r, ier
          real (dp)           :: result
          do
             write(*, '(a)', advance='no') ' Enter n, r : '
             read(*, *) n, r
             call ncr(n, r, result, ier)
             if (ier /= 0) then
                write(*, *) ' Error, IER = ', ier
                if (ier == 4) write(*, '(a, f12.5)') ' ln(ncr) = ', result
             else
                write(*, '(a, g16.8)') ' ncr = ', result
             endif
          enddo
          end program demo_ncr
