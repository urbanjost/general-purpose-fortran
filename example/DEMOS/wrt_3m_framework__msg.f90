     program demo_wrt
     use, intrinsic :: iso_fortran_env, only : &
      & stdin=>input_unit, &
      & stdout=>output_unit, &
      & stderr=>error_unit
     use M_framework__msg, only: wrt, fmt
     implicit none
     integer,allocatable :: luns(:)
     integer :: iostat=0
     integer,parameter :: ints(3)=[1,2,3]

     ! a null list allows for turning off verbose or debug mode output
     luns=[integer ::]
     call wrt(luns,'NULL LIST:',huge(0),'PI=',asin(1.0d0)*2.0d0,iostat=iostat)
     write(*,*)'IOSTAT=',iostat

     ! multiple files can be used to create a log file, for example
     luns=[stderr,stdout]
     call wrt(luns,'TWO FILES:',huge(0),'PI=',asin(1.0d0)*2.0d0,iostat=iostat)
     write(*,*)'IOSTAT=',iostat

     ! using fmt
     call wrt([stdout,stdout,stdout],'USING FMT :', &
      & huge(0),'PI=',asin(1.0d0)*2.0d0,fmt(ints(2),'i0.4'),iostat=iostat)

     end program demo_wrt
