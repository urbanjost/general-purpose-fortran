          program demo_backspace
          implicit none
          character(len=256) :: line
          character(len=256) :: mssge
          integer            :: i
          integer            :: ios
             open(10,file='demo_backspace.txt') ! open a file
             do i=1,100                         ! write lines to it
                write(10,'(a,i0)') 'line ',i
             enddo
             do i=1,10                          ! back up several lines
                backspace(10, iostat=ios,iomsg=mssge)
                if(ios.ne.0)then
                        write(*,'(*(a))') '*demo_backspace* ERROR:',mssge
                endif
             enddo
             read(10,'(a)')line
             write(*,*)'back at a previous record !'
             write(*,'(1x,a)')line
             !! writing new line will truncate file to current record position
             close(10,status='delete')
          end program demo_backspace
