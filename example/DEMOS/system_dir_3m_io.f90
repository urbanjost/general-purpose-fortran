          program demo_system_dir
          use M_system, only : system_dir
          implicit none
             write(*, '(a)')system_dir(pattern='*.f90')
          end program demo_system_dir
