          program demo_close
          implicit none
          character(len=256) :: message
          integer            :: ios
             open (10, file='employee.names', action='read', iostat=ios,iomsg=message)
             if (ios < 0) then
                ! perform end-of-file processing on the file connected to unit 10.

                close (10, status='keep',iostat=ios,iomsg=message)
                if(ios.ne.0)then
                   write(*,'(*(a))')'*demo_close* close error: ',trim(message)
                   stop 1
                endif
             elseif (ios > 0) then
                ! perform error processing on open
                write(*,'(*(a))')'*demo_close* open error: ',trim(message)
                stop 2
             endif
          end program demo_close
