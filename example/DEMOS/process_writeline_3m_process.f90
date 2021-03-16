         program demo_process_writeline
         use, intrinsic :: iso_fortran_env, only : &
            & stdin=>input_unit, &
            & stdout=>output_unit, &
            & stderr=>error_unit
         use m_process ,only: process_open_write, process_writeline
         use m_process ,only: streampointer, process_close
         implicit none
         type(streampointer) :: fp
         ! line of data to write
         character(len=4096) :: line
         integer             :: ierr
         integer             :: i
           ! open process to write to
           call process_open_write('cat -n',fp,ierr)
           write(*,*)'WRITETEST: process is opened with status ',ierr
           ! remember C and Fortran I/O are often independent of each other
           flush(stdout)
           ierr=0
           line='xxxxxxxxxxxxxxxxxxxxxxxxxxx'
           do i=1,10
             ! write a line to the process
             call process_writeline(trim(line),fp,ierr)
             if(ierr.lt.0)then
               write(*,*)'WRITETEST: ierr is ',ierr
               exit
             endif
           enddo
           call process_close(fp,ierr)
           write(*,*)'WRITETEST: process closed with status ',ierr
         end program demo_process_writeline
