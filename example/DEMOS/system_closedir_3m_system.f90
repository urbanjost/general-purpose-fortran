     program demo_system_closedir
     use M_system, only : system_opendir,system_readdir
     use M_system, only : system_closedir, system_rewinddir
     use iso_c_binding, only : c_ptr
     implicit none
     type(c_ptr)                  :: dir
     character(len=:),allocatable :: filename
     integer                      :: ierr
     !--- open directory stream to read from
     call system_opendir('.',dir,ierr)
     !--- read directory stream
     do
        call system_readdir(dir,filename,ierr)
        if(filename.eq.' ')exit
        write(*,*)filename
     enddo
     call system_rewinddir(dir)
     !--- close directory stream
     call system_closedir(dir,ierr)
     end program demo_system_closedir
