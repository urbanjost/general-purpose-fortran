              program demo_msg
              use M_strings, only : msg
              implicit none
              character(len=:),allocatable :: pr
              character(len=:),allocatable :: frmt
              integer                      :: biggest

              pr=msg('HUGE(3f) integers',huge(0),&
              & 'and real',huge(0.0),'and double',huge(0.0d0))
              write(*,'(a)')pr
              pr=msg('real            :',&
               & huge(0.0),0.0,12345.6789,tiny(0.0) )
              write(*,'(a)')pr
              pr=msg('doubleprecision :',&
               & huge(0.0d0),0.0d0,12345.6789d0,tiny(0.0d0) )
              write(*,'(a)')pr
              pr=msg('complex         :',&
               & cmplx(huge(0.0),tiny(0.0)) )
              write(*,'(a)')pr

              ! create a format on the fly
              biggest=huge(0)
              frmt=msg('(*(i',int(log10(real(biggest))),':,1x))',nospace=.true.)
              write(*,*)'format=',frmt

              ! although it will often work, using msg(3f) in an I/O statement
              ! is not recommended
              write(*,*)msg('program will now stop')

              end program demo_msg
