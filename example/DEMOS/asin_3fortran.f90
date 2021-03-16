           program demo_asin
           use, intrinsic :: iso_fortran_env, only : dp=>real64
           implicit none
           ! value to convert degrees to radians
           real(kind=dp),parameter :: D2R=acos(-1.0_dp)/180.0_dp
           real(kind=dp)           :: angle, rise, run
             ! given sine(Θ) = 1.25 miles/50 miles (opposite/hypotenuse)
             ! then taking the arcsine of both sides of the equality yields
             ! Θ = arcsine(1.25 miles/50 miles) ie. arcsine(opposite/hypotenuse)
             rise=1.250_dp
             run=50.00_dp
             angle = asin(rise/run)
             write(*,*)'angle of incline(radians) = ', angle
             angle = angle/D2R
             write(*,*)'angle of incline(degrees) = ', angle

             write(*,*)'percent grade=',rise/run*100.0_dp
           end program demo_asin
