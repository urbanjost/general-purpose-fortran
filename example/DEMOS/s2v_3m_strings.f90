          program demo_s2v

           use M_strings, only: s2v, int, real, dble
           implicit none
           character(len=8)              :: s=' 10.345 '
           integer                       :: i
           character(len=14),allocatable :: strings(:)
           doubleprecision               :: dv
           integer                       :: errnum

           ! different strings representing INTEGER, REAL, and DOUBLEPRECISION
           strings=[&
           &' 10.345       ',&
           &'+10           ',&
           &'    -3        ',&
           &'    -4.94e-2  ',&
           &'0.1           ',&
           &'12345.678910d0',&
           &'              ',& ! Note: will return zero without an error message
           &'1 2 1 2 1 . 0 ',& ! Note: spaces will be ignored
           &'WHAT?         ']  ! Note: error messages will appear, zero returned

           ! a numeric value is returned,
           ! so it can be used in numeric expression
           write(*,*) '1/2 value of string is ',s2v(s)/2.0d0
           write(*,*)
           write(*,*)' STRING            VALUE                    ERROR_NUMBER'
           do i=1,size(strings)
              ! Note: not a good idea to use s2v(3f) in a WRITE(3f) statement,
              ! as it does I/O when errors occur, so called on a separate line
              dv=s2v(strings(i),errnum)
              write(*,*) strings(i)//'=',dv,errnum
           enddo
           write(*,*)"Extended intrinsics"
           write(*,*)'given inputs:',s,strings(:8)
           write(*,*)'INT(3f):',int(s),int(strings(:8))
           write(*,*)'REAL(3f):',real(s),real(strings(:8))
           write(*,*)'DBLE(3f):',dble(s),dble(strings(:8))
           write(*,*)"That's all folks!"

           end program demo_s2v
