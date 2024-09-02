         program demo_str
         use M_strings, only : str, quote
         implicit none
         character(len=:),allocatable :: pr
         character(len=:),allocatable :: frmt
         integer                      :: biggest

         pr=str('HUGE(3f) integers',huge(0),&
         & 'and real',huge(0.0),'and double',huge(0.0d0))
         write(*,'(a)')pr
         pr=str('real            :',&
          & huge(0.0),0.0,12345.6789,tiny(0.0) )
         write(*,'(a)')pr
         pr=str('doubleprecision :',&
          & huge(0.0d0),0.0d0,12345.6789d0,tiny(0.0d0) )
         write(*,'(a)')pr
         pr=str('complex         :',&
          & cmplx(huge(0.0),tiny(0.0)) )
         write(*,'(a)')pr

         ! create a format on the fly
         biggest=huge(0)
         ! +0 for gfortran-11 bug
         frmt=str('(*(i',int(log10(real(biggest)))+0,':,1x))',sep='')
         write(*,*)'format=',frmt

         ! compound output
         pr=str(10,100.0,"string",(11.0,22.0),.false.)
         write(*,'(a)')pr
         ! a separator and also use of quote(3f)
         pr=str(10,100.0,quote("string"),(11.0,22.0),.false.,sep=';')
         write(*,'(a)')pr
         ! CSV mode
         pr=str(10,100.0,"string",(11.0,22.0),.false.,csv=.true.)
         write(*,'(a)')pr
         ! everything a vector instead of a scalar
         pr=str([10,20,30],["string"],[(11.0,22.0)],[.false.,.true.])
         write(*,'(a)')pr
         pr=str([10,20,30],["string"],[(11.0,22.0)],[.false.,.true.],sep='|')
         write(*,'(a)')pr
         pr=str([10,20,30],["string"],[(11.0,22.0)],[.false.,.true.],csv=.true.)
         write(*,'(a)')pr

         ! although it will often work, using str(3f) in an I/O statement
         ! is not recommended
         write(*,*)str('program will now attempt to stop')

         end program demo_str
