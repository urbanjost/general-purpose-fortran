          program demo_wrt
          use, intrinsic :: iso_fortran_env, only : &
           & stdin=>input_unit, &
           & stdout=>output_unit, &
           & stderr=>error_unit
          use M_msg, only: wrt
          implicit none
          integer,allocatable :: luns(:)
          integer :: iostat=0
          ! a null list allows for turning off verbose or debug mode output
          luns=[integer ::]
          call wrt(luns,'NULL LIST:',huge(0),'PI=',asin(1.0d0)*2.0d0,iostat=iostat)
          write(*,*)'IOSTAT=',iostat
          ! multiple files can be used to create a log file
          luns=[stderr,stdout]
          call wrt(luns,'TWO FILES:',huge(0),'PI=',asin(1.0d0)*2.0d0,iostat=iostat)
          write(*,*)'IOSTAT=',iostat
          ! unlike direct use of WRITE a function can be used that returns
          ! an INTEGER array.
          end program demo_wrt
