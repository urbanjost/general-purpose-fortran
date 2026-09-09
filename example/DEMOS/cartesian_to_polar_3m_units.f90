     program demo_cartesian_to_polar
     use M_units, only : cartesian_to_polar
     ! basic cardinal directions
        call  printme( +1.0, +0.0)
        call  printme( +0.0, +1.0)
        call  printme( -1.0, +0.0)
        call  printme( +0.0, -1.0)
     ! the 3-4-5 right triangle
        call  printme( +4.0, +3.0)
        call  printme( +3.0, +4.0)
        call  printme( -3.0, +4.0)
        call  printme( -4.0, +3.0)
        call  printme( -4.0, -3.0)
        call  printme( -3.0, -4.0)
        call  printme( +3.0, -4.0)
        call  printme( +4.0, -3.0)
        write(*,'(*(g0))') 'cases where input is too large:'
        call  printme( huge(0.0),huge(0.0))
        stopit: block
           real :: radius, inclination
           call cartesian_to_polar( huge(0.0), huge(0.0), radius, inclination)
        endblock stopit
     contains
     subroutine printme(x,y)
     real,intent(in):: x,y
     real :: radius,inclination
     integer :: ierr
        call cartesian_to_polar(x,y,radius,inclination,ierr)
        write(*,*)ierr, x,y,radius,inclination,inclination*180/acos(-1.0)
     end subroutine printme
     end program demo_cartesian_to_polar
