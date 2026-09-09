     program demo_anyscalar_to_string
     use M_anything, only : anyscalar_to_string
     implicit none
     character(len=:),allocatable :: pr
     character(len=:),allocatable :: frmt
     integer                      :: biggest

     pr=anyscalar_to_string('HUGE(3f) integers',huge(0),&
     &'and real',huge(0.0),'and double',huge(0.0d0))
     write(*,'(a)')pr
     pr=anyscalar_to_string('real            :',huge(0.0),0.0,12345.6789,tiny(0.0) )
     write(*,'(a)')pr
     pr=anyscalar_to_string('doubleprecision :',huge(0.0d0),0.0d0,12345.6789d0,tiny(0.0d0) )
     write(*,'(a)')pr
     pr=anyscalar_to_string('complex         :',cmplx(huge(0.0),tiny(0.0)) )
     write(*,'(a)')pr

     ! create a format on the fly
     biggest=huge(0)
     frmt=anyscalar_to_string('(*(i',int(log10(real(biggest))),':,1x))',sep='')
     write(*,*)'format=',frmt

     ! although it will often work, using anyscalar_to_string(3f)
     ! in an I/O statement is not recommended
     ! because if an error occurs anyscalar_to_string(3f) will try
     ! to write while part of an I/O statement
     ! which not all compilers can handle and is currently non-standard
     write(*,*)anyscalar_to_string('program will now stop')

     end program demo_anyscalar_to_string
