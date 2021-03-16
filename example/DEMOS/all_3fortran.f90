           program demo_all
           implicit none
           logical l
              l = all([.true., .true., .true.])
              print *, l
              call section
           contains
              subroutine section
              integer a(2,3), b(2,3)
                 a = 1
                 b = 1
                 b(2,2) = 2
                 print *, all(a .eq. b, 1)
                 print *, all(a .eq. b, 2)
              end subroutine section
           end program demo_all
