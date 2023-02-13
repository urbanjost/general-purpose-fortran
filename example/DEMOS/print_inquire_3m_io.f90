     program demo_print_inquire
     use M_io, only : print_inquire, fileopen
     implicit none
     character(len=4096)  :: filename
     character(len=20)    :: mode
     integer              :: ios
     character(len=256)   :: message
     integer              :: lun
        do
           write(*,'(a)',advance='no')'enter filename>'
           read(*,'(a)',iostat=ios)filename
           if(ios /= 0)exit
           write(*,'(a)',advance='no')'enter mode ([rwa][bt][+]>'
           read(*,'(a)',iostat=ios)mode
           if(ios /= 0)exit
           lun=fileopen(filename,mode,ios)
           if(ios == 0)then
              write(*,*)'OPENED'
           else
              write(*,*)'ERROR: IOS=',ios
           endif
           if(lun /= -1)then
              call print_inquire(lun,'')
              close(lun,iostat=ios,iomsg=message)
              if(ios /= 0)then
                 write(*,'(a)')trim(message)
              endif
           endif
        enddo
     end program demo_print_inquire
