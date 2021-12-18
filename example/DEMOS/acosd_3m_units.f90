     program demo_acosd
     use M_units, only :  acosd, cosd
     implicit none
        write(*, *)       cosd(0.0),cosd(45.0),cosd(120.0),cosd(180.0),cosd(720.0)
        write(*, *)acosd([cosd(0.0),cosd(45.0),cosd(120.0),cosd(180.0),cosd(720.0) ])
     end program demo_acosd
