          program demo_M_display
          use M_display
          implicit none
          integer, parameter :: rk = selected_real_kind(6), n = 3
          real(rk) :: a(n,n), b(n,n), x
          integer i, j, k(5)
            call disp_set(advance = 'double')
            forall(i=1:n, j=1:n)
              a(i,j) = exp(real(i+j-1, rk))
              b(i,j) = exp(real(i**j, rk))
            end forall
            call disp('A = ', a)
            call disp(b)
            call disp(a(1:2,:),'f0.5')
            call disp('MATRIX', a, style='UNDERLINE & NUMBER', unit=-3, digmax=4)
            k = [-3,0,12,14,0]
            call disp('K', k, style='pad', orient='row', sep=' ', zeroas='.')
            x = 1.5
            call disp('The square of '//tostring(x)//' is '//tostring(x*x))
            call disp_set(matsep = ' | ')
            call disp([11,12,13], advance='no')
            call disp([.true., .false., .true.], advance='no')
            call disp(['A','B','C'])
          end program demo_M_display
