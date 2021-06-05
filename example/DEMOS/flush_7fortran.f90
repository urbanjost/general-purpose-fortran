          program demo_flush
          implicit none
          character(len=256) :: msg
          integer :: ios, lun
             lun=10
             flush (unit=lun, iostat=ios, iomsg=msg)
             if(ios.ne.0)then
                write(*,'(a)')'<ERROR>*flush*:'//trim(msg)
             endif
          end program demo_flush
