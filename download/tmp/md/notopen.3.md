[UP]

-----------------------------------------------------------------------------------------------------------------------------------
                                               Manual Reference Pages  - notopen (3)
-----------------------------------------------------------------------------------------------------------------------------------
                                                                 
NAME

    notopen(3f) - [M_io] Find a FUN/LUN (Fortran-unit-number) that is not in use

CONTENTS

    Synopsis
    Description
    Options
    Notes
    Example
    Authors

SYNOPSIS

    Usage


          integer function notopen(start,end,err)
          integer,optional,intent(in)  :: start
          integer,optional,intent(in)  :: end
          integer,optional,intent(out) :: err

DESCRIPTION

    A free FORTRAN unit number is needed to OPEN a file. NOTOPEN() returns a FORTRAN unit number from START to END not currently
    associated with an I/O unit. START and END are expected to be positive integers where END .ge. START.

    If NOTOPEN() returns -1, then no free FORTRAN unit could be found in the specified range.

    Otherwise, NOTOPEN() returns an integer representing a free FORTRAN logical unit number. Note that NOTOPEN() assumes the
    following unit numbers defined by the Fortran 2008 ISO_FORTRAN_ENV module

          ERROR_UNIT,INPUT_UNIT,OUTPUT_UNIT



    are special, and will never return those values.

OPTIONS

             start optional logical unit number to start scan at, defaults to 10.

             end optional logical unit number to stop scan at, defaults to 99.

             err optional error flag returned. ERR will be non-zero if no errors. If not present and an error occurs the program
             will stop instead of returning.

NOTES

    Why are the default START and END limits from 10 to 99? the Fortran 77 standard did not specify a specific limit on the upper
    range limit, but the LUN range of 1 to 99 was almost always supported in conventional programming environments. Additionally,
    units in the range 0-10 have often been the units used for pre-assigned files. Occasionally 100, 101 and 102 are reserved (for
    files such as standard input, standard output, standard error, ...). Therefore, the defaults for START and END were selected to
    be 10 and 99. And most programs do not need more than 90 files simultaneously open, so the defaults work well in practice with
    many versions/vintages of Fortran.

    Note that an environment may impose a limit on the number of simultaneously open files (which some compilers work around).

    Beginning with f2008, you can probably use OPEN(NEWUNIT=...) instead.

EXAMPLE

    Sample program:

        program demo_notopen ! test the NOTOPEN(3f) function
        use m_io, only: notopen
        implicit none
        integer :: ii, ierr, igot


        write(*,*) check for preassigned files from unit 0 to unit 1000 
        write(*,*) (5 and 6 always return -1) 


        do ii=0,1000
           if(notopen(ii,ii,ierr) .ne. ii)then
              write(*,*) INUSE: ,ii, notopen(ii,ii,ierr)
           endif
        enddo


        ! open all files from UNIT=10 to UNIT=30 so have used units
        do ii=10,30,1
          open(unit=ii,status="scratch")
        enddo
        ! close UNIT=25
        close(25)


        ! find open file in range 10 to 30
        write(*,*) Should get 25 for this .. ,notopen(10,30,ierr)


        close(18)
        do ii=10,32
          igot=notopen(ii,ii,ierr)
          write(*,*) For unit  ,ii,  I got  ,igot,  with ERR= ,ierr
        enddo


        end program demo_notopen



    Expected output(can vary with each programming environment):

          check for preassigned files from unit 0 to unit 1000
          (5 and 6 always return -1)
          INUSE:    0    -1
          INUSE:    5    -1
          INUSE:    6    -1
          Should get 25 for this .. 25
          For  unit  10  I  got  -1  with  ERR=  -1
          For  unit  11  I  got  -1  with  ERR=  -1
          For  unit  12  I  got  -1  with  ERR=  -1
          For  unit  13  I  got  -1  with  ERR=  -1
          For  unit  14  I  got  -1  with  ERR=  -1
          For  unit  15  I  got  -1  with  ERR=  -1
          For  unit  16  I  got  -1  with  ERR=  -1
          For  unit  17  I  got  -1  with  ERR=  -1
          For  unit  18  I  got  18  with  ERR=   0
          For  unit  19  I  got  -1  with  ERR=  -1
          For  unit  20  I  got  -1  with  ERR=  -1
          For  unit  21  I  got  -1  with  ERR=  -1
          For  unit  22  I  got  -1  with  ERR=  -1
          For  unit  23  I  got  -1  with  ERR=  -1
          For  unit  24  I  got  -1  with  ERR=  -1
          For  unit  25  I  got  25  with  ERR=   0
          For  unit  26  I  got  -1  with  ERR=  -1
          For  unit  27  I  got  -1  with  ERR=  -1
          For  unit  28  I  got  -1  with  ERR=  -1
          For  unit  29  I  got  -1  with  ERR=  -1
          For  unit  30  I  got  -1  with  ERR=  -1
          For  unit  31  I  got  31  with  ERR=   0
          For  unit  32  I  got  32  with  ERR=   0



AUTHORS

    John S. Urban

-----------------------------------------------------------------------------------------------------------------------------------

                                                            notopen (3)                                               July 02, 2017

Generated by manServer 1.08 from 7cc2c0f5-a256-4e8b-9630-500f0b99fa0e using man macros.
                                                             [notopen]
