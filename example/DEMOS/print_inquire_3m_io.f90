     program demo_print_inquire
     use M_io, only : print_inquire, fileopen
     implicit none
     character(len=4096)  :: filename
     character(len=20)    :: mode
     integer              :: iostat
     character(len=256)   :: message
     integer              :: lun
        do
           write(*,'(a)',advance='no')'enter filename>'
           read(*,'(a)',iostat=iostat)filename
           if(iostat /= 0)exit
           write(*,'(a)',advance='no')'enter mode ([rwa][bt][+]>'
           read(*,'(a)',iostat=iostat)mode
           if(iostat /= 0)exit
           lun=fileopen(filename,mode,iostat)
           if(iostat == 0)then
              write(*,*)'OPENED'
           else
              write(*,*)'ERROR: iostat=',iostat
           endif
           if(lun /= -1)then
              call print_inquire(lun,'')
              close(lun,iostat=iostat,iomsg=message)
              if(iostat /= 0)then
                 write(*,'(a)')trim(message)
              endif
           endif
        enddo
     end program demo_print_inquire
