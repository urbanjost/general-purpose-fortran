          program demo_system_readdir
          use M_system, only : system_opendir,system_readdir
          use M_system, only : system_rewinddir,system_closedir
          use iso_c_binding
          implicit none

          type(c_ptr)                  :: dir
          character(len=:),allocatable :: filename
          integer                      :: i, ierr
          !--- open directory stream to read from
          call system_opendir('.',dir,ierr)
          if(ierr.eq.0)then
             !--- read directory stream twice
             do i=1,2
                write(*,'(a,i0)')'PASS ',i
                do
                   call system_readdir(dir,filename,ierr)
                   if(filename.eq.' ')exit
                   write(*,*)filename
                enddo
                call system_rewinddir(dir)
             enddo
          endif
          !--- close directory stream
          call system_closedir(dir,ierr)

          end program demo_system_readdir
