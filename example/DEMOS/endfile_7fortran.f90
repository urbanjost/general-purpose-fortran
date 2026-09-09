          program demo_endfile
          implicit none
          integer :: lun, i, j, iostat
          integer,parameter:: isz=10
             !
             ! create a little scratch file
             open(newunit=lun,file='_scr.txt',  &
             & form='formatted',                &
             & action='readwrite')
             write(lun,'(i0)')(100+i,i=1,isz)
             !
             ! write end of file after reading half of file
             rewind(lun)
             write(*,*)'rewind and read',isz/2,'lines'
             read(lun,*)(j,i=1,isz/2)
             endfile lun ! will truncate line at current position
             !
             ! NOTE: backspace before writing any addition lines
             !       once an ENDFILE(7) statement is executed
             ! backspace(lun)
             !
             ! rewind and echo remaining file
             rewind(lun)
             j=0
             do i=1,huge(0)-1
                read(lun,*,iostat=iostat)j
                if(iostat.ne.0)exit
                write(*,*)i,j
             enddo
             write(*,*)'number of lines in file was ',isz,', is now ',i-1
             close(unit=lun,status='delete')
          end program demo_endfile
