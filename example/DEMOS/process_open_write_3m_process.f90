         program demo_process_open_write
         use, intrinsic :: iso_fortran_env, only : &
         & stdin=>input_unit, &
         & stdout=>output_unit, &
         & stderr=>error_unit
         use M_process ,ONLY: process_open_write, process_writeline
         use M_process ,ONLY: streampointer, process_close
         implicit none
         type(streampointer) :: fp
         ! line of data to write
         character(len=4096) :: line
         integer             :: ierr
         integer             :: i
           ! open process to write to
           call process_open_write('cat -n',fp,ierr)
           write(stdout,*)'OPENWTEST: process is opened with status ',ierr
           ! remember C and Fortran I/O are often independent of each other
           flush(stdout)
           ierr=0
           line='xxxxxxxxxxxxxxxxxxxxxxxxxxx'
           do i=1,10
             ! write a line to the process
             call process_writeline(trim(line),fp,ierr)
             if(ierr.lt.0)then
               write(stdout,*)'OPENWTEST: ierr is ',ierr
               exit
             endif
           enddo
           call process_close(fp,ierr)
           write(stdout,*)'OPENWTEST: process closed with status ',ierr
         end program demo_process_open_write
