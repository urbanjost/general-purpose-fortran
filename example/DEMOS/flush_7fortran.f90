          program demo_flush
          use, intrinsic :: iso_fortran_env, only : &
          & stderr=>ERROR_UNIT, &
          & stdin=>INPUT_UNIT,  &
          & stdout=>OUTPUT_UNIT
          implicit none
          integer :: iostat
          character(len=255) :: iomsg
             flush (stderr, iostat=iostat, iomsg=iomsg)
             if(iostat.ne.0)then
                write(*,*)'ERROR:'//trim(iomsg)
                error stop 1
             endif
             flush (stdout, err = 999 )
             stop
             999 continue
             stop 10
          end program demo_flush
