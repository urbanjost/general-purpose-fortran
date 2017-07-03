[UP]

-----------------------------------------------------------------------------------------------------------------------------------
                                              Manual Reference Pages  - M_process (3)
-----------------------------------------------------------------------------------------------------------------------------------
                                                                 
NAME

    M_process - [M_process]Fortran Module for calling process-related C functions from Fortran

CONTENTS

    Synopsis
    Description
    Options
    Examples
    See Also

SYNOPSIS

    use M_process, only : process_open_read, process_open_write, process_close
    use M_process, only : process_readline, process_readall, process_writeline
    use M_process, only : streampointer, process_debug

DESCRIPTION

    Module M_process(3f) lets Fortran code read/write lines from/to processes.

    These Fortran procedures use the ISO_C_BINDING interface to define Fortran-callable versions of the C procedures popen(3c)/
    pclose(3c) and fgets(3c)/fputs(3c). A set of record-oriented wrapper routines are then used to create a simple Fortran-callable
    interface.

    Basically, you

    o    Open a process for either reading from or writing to using formatted sequential text records (eg. "lines"); much like with
         a regular file.

    o    pass a CHARACTER variable to/from the process that represents a record.

    o    Use internal READs and internal WRITEs or parsing routines to create or interpret the lines.

    o    when done close the process much like closing a file.

    The procedures defined are:

        ! open process to read from
        subroutine process_open_read(cmd,fp,ierr)


        ! open process to write to
        subroutine process_open_write(cmd,fp,ierr)


        ! read line from process
        subroutine process_readline(string,fp,ierr)
        ! read all of process output into a string string
        function process_readall(cmd,ierr) result (string)


        ! write line to process
        subroutine process_writeline(string,fp,ierr)


        ! close process
        subroutine process_close(fp,ierr)



    where the variable types are

          character(len=*)    :: cmd
          type(streampointer) :: fp
          character(len=*)    :: string
          integer             :: ierr



OPTIONS

         cmd command passed to system to start process

         fp C file pointer returned by process_open_*()

         string data line to send or receive from process

         ierr error flag returned. Non-zero indicates an error

    maximum character value length is currently 4096

EXAMPLES

    An example that places all the output of a command into a single string variable (see process_readall(3) for an even simpler
    way to do this) ...

          program read_ex
          use M_process ,only: process_open_read, process_readline
          use M_process ,only: streampointer, process_close
          implicit none
          ! C file pointer returned by process_open()
          type(streampointer) :: fp
          ! check status of calls to process module routines
          integer :: ierr
          ! hold results, assuming sufficient memory is available
          character(len=:),allocatable :: string
          ! long enough to hold any expected line
          character(len=4096) :: line


             string=  


             !###! open process to read from
             call process_open_read( ls ,fp,ierr)


             !###! read output of process till end
             do
                call process_readline(line,fp,ierr)
                if(ierr.ne.0)exit
                !###! append output lines together
                string=string//trim(line)//   
                write(*,*) [ //string// ] 
             enddo


             write(*,*)trim(string)


             !###! Wrap up
             call process_close(fp,ierr)



        end program read_ex

    An example program that calls the M_process module to start a plotting program called gnuplot(1) and give it enough commands to
    generate a plot. It then lets you interactively interact with the program or continue on in the program.

           program gnuExample
           ! @(#)  Example of Fortran writing GNUPLOT command and data file.
           use M_process ,only: process_open_write, process_writeline
           use M_process ,only: streampointer, process_close
           implicit none
           !*! line of data to write (assumed long enough to hold any command line)
           character(len=4096) :: line
           !*! C file pointer returned by process_open()
           type(streampointer) :: fp
           !*! check status of calls to process module routines
           integer :: ierr
           !*! DO loop counter
           integer :: i


           !*! data file to create and put X,Y values into
           character(len=60)   :: fidmap= SAMPLE.MAP 
           !*! number of points to put into curve to be plotted
           integer,parameter   :: n=50
           !*! arrays to fill with curve data to be plotted
           real                :: x(n),y(n)
           integer             :: ios


           !*! open data file to hold X,Y values to be plotted
           open(70,file=fidmap)
           !*! Define sample X,Y array.
           do i=1,n
           !*! set X() values as whole numbers 1 to N
              x(i)=i
           !*!
              y(i)=(x(i)+0.5)**2
           enddo


           !*! Write the X,Y array as coordinates to be plotted.
           write(70, (2f10.3) )(x(i),y(i),i=1,n)
           !*! if not closed or flushed, subprocess may not be able to read
           flush(70, iostat = ios)


           !*! Write the GnuPlot commands
           !*! open process to write to (ie. start gnuplot(1) program)
           call process_open_write( gnuplot ,fp,ierr)
           call process_writeline &
           & ( set title " Example of GNUPlot data and command file generation" ,fp,ierr)
           !*! write a command to the process
           call process_writeline( set nokey ,fp,ierr)


           !*! build a command with a formatted write
           write(line,101) trim(fidmap)
           101 format( plot " ,A, " with lines )
           !*! write a command to the process
           call process_writeline(trim(line),fp,ierr)




           !*! Additional gnuplot commands; in this case interactively entered
           write(*, (a) ) enter gnuplot commands or "." to exit 
           do
              write(*, (a) ,advance= no ) gnu>> 
              read(*, (a) ,iostat=ios)line
              if(line.eq. . )exit
              call process_writeline(trim(line),fp,ierr)
           enddo


           !*! Wrap up
           call process_close(fp,ierr)
           write(*,*) CLOSED THE PROCESS. RETURNING TO PROGRAM 
           !*! delete the data file
           close(70,status= delete ,iostat=ios)
           end program gnuExample



SEE ALSO

    o  PIPES: pipe(3c), popen(3c), pclose(3c), fflush(3c)

    o  NAMED PIPES: mkfifo(3c), mknod(3c)

    o  SUBPROCESSES: fork(3c)

    o  OTHER: fflush(3c)

-----------------------------------------------------------------------------------------------------------------------------------

                                                           M_process (3)                                              July 02, 2017

Generated by manServer 1.08 from c579f644-bc11-4cc1-9e14-f43f8ae887eb using man macros.
                                                            [M_process]
