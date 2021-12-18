     program demo_msg
     use M_msg, only : str
     implicit none
     character(len=:),allocatable :: pr
     character(len=:),allocatable :: frmt
     integer                      :: biggest

     pr=str('HUGE(3f) integers',huge(0),&
     &'and real',huge(0.0),'and double',huge(0.0d0))
     write(*,'(a)')pr
     pr=str('real            :',huge(0.0),0.0,12345.6789,tiny(0.0) )
     write(*,'(a)')pr
     pr=str('doubleprecision :',huge(0.0d0),0.0d0,12345.6789d0,tiny(0.0d0) )
     write(*,'(a)')pr
     pr=str('complex         :',cmplx(huge(0.0),tiny(0.0)) )
     write(*,'(a)')pr

     ! create a format on the fly
     biggest=huge(0)
     frmt=str('(*(i',int(log10(real(biggest))),':,1x))',sep='')
     write(*,*)'format=',frmt

     ! although it will often work, using str(3f)
     ! in an I/O statement is not recommended
     ! because if an error occurs str(3f) will try
     ! to write while part of an I/O statement
     ! which not all compilers can handle and is currently non-standard
     write(*,*)str('program will now stop')

     end program demo_msg
