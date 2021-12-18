     program demo_atand
     use M_units, only :  atand, tand
     implicit none
        write(*, *)       tand(0.0),tand(45.0),tand(120.0),tand(180.0),tand(720.0)
        write(*, *)atand([tand(0.0),tand(45.0),tand(120.0),tand(180.0),tand(720.0) ])
     end program demo_atand
