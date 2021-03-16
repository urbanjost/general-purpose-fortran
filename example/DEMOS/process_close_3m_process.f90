         program demo_process_close
         use M_process ,ONLY: process_open_read, process_open_write
         use M_process ,ONLY: streampointer, process_close
         implicit none
         type(streampointer) :: fp
         integer             :: ierr
           ! open process to read from
           call process_open_read('ls -l',fp,ierr)
           write(*,*)'CLOSE   : process is opened with status ',ierr
           call process_close(fp,ierr)
           write(*,*)'CLOSE   : process closed with status ',ierr
         end program demo_process_close
