            program demo_abs
            integer :: i = -1, iout
            real :: x = -1.e0, xout, zout
            complex :: z = (-3.e0,4.e0)
            doubleprecision :: r8 = 45.78D+00, dout
               write(*,*)'INPUTS:',i,x,z,r8
               iout = abs(i)
               xout = abs(x)
               zout = abs(z)
               dout = abs(r8)
               write(*,*)'OUTPUTS:',iout,xout,zout,dout
               write ( *, '(a,f12.4,12x,f12.4)' ) ' Double precision  ', -r8, abs(r8)
               ! COMPLEX
               ! 3 - 4 -5 right triangle test :
               write(*,*)'The abs() of (3.0,4.0) should be 5.0',abs((3.0,4.0))
               ! ELEMENTAL
               write(*,*)'abs is ELEMENTAL: ',abs([-10, 20, 0, -1, -3, 100])
            end program demo_abs
