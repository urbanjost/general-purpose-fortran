         program demo_backspace
         implicit none
         character(len=256) :: line
         character(len=256) :: mssge
         integer            :: i
         integer            :: j
         integer            :: ios
         integer,allocatable :: iarr(:)

            ! create a basic sequential file
            open(10,file='dem_backspace.txt',action='readwrite') ! open a file
            do i=1,30                         ! write lines to it
               write(10,'(a,i3,*(i3))') 'line ',i, (j,j=1,i)
            enddo

            ! back up several lines
            do i=1,14
               backspace(10, iostat=ios,iomsg=mssge)
               if(ios.ne.0)then
                       write(*,'(*(a))') '*dem_backspace* ERROR:',mssge
               endif
            enddo
            read(10,'(a)')line
            write(*,*)'back at a previous record !'

            ! read line as a string
            write(*,'("string=",a)')trim(line)

            ! backspace so can read again as numbers
            backspace(10)
            ! read part of a line numerically to get size of array to read
            read(10,'(5x,i3)')i
            allocate(iarr(i))

            ! reread line just reading array
            backspace(10)
            read(10,'(8x,*(i3))')iarr
            write(*,'(*(g0,1x))')'size=',i,'array=',iarr

            !! Note: writing a new line will truncate file
            !!       to current record position

            close(10,status='delete')

         end program demo_backspace
