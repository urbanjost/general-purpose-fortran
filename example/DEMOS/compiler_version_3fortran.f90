          program demo_compiler_version
          use, intrinsic :: iso_fortran_env, only : compiler_version, compiler_options
          implicit none
             print '(4a)', &
                'This file was compiled by ', &
                compiler_version(),           &
                ' using the options ',        &
                compiler_options()
          end program demo_compiler_version
