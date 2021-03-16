           program demo_any
           implicit none
           logical l
              l = any([.true., .true., .true.])
              print *, l
              call section
              contains
                subroutine section
                integer a(2,3), b(2,3)
                  a = 1
                  b = 1
                  b(2,2) = 2
                  print *, any(a .eq. b, 1)
                  print *, any(a .eq. b, 2)
                end subroutine section
           end program demo_any
