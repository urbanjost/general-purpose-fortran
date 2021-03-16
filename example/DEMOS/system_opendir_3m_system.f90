          program demo_system_opendir
          use M_system, only : system_opendir,system_readdir
          use M_system, only : system_closedir
          use iso_c_binding
          implicit none
          type(c_ptr)                  :: dir
          character(len=:),allocatable :: filename
          integer                      :: ierr
          !--- open directory stream to read from
          call system_opendir('.',dir,ierr)
          if(ierr.eq.0)then
             !--- read directory stream
             do
                call system_readdir(dir,filename,ierr)
                if(filename.eq.' ')exit
                write(*,*)filename
             enddo
          endif
          !--- close directory stream
          call system_closedir(dir,ierr)
          end program demo_system_opendir
