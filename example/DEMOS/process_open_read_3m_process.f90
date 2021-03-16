         program demo_process_open_read
         use M_process ,ONLY: process_open_read, process_readline
         use M_process ,ONLY: streampointer, process_close
         implicit none
         type(streampointer) :: fp
         ! line of data to read (assumed long enough to hold any output line)
         character(len=4096) :: line
         integer             :: ierr
           ! open process to read from
           call process_open_read('ls -l',fp,ierr)
           write(*,*)'READTEST: process is opened with status ',ierr
           ierr=0
           do while(ierr .eq. 0)
             ! read a line from the process
             call process_readline(line,fp,ierr)
             if(ierr.ne.0)then
               write(*,*)'READTEST: ierr is ',ierr
               exit
             endif
             write(*,*)'READTEST: ',trim(line)
           enddo
           call process_close(fp,ierr)
           write(*,*)'READTEST: process closed with status ',ierr
         end program demo_process_open_read
