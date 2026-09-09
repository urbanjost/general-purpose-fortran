     program demo_pixel
     use :: M_pixel
     implicit none
        call prefsize(10,10) ! set up drawing surface
        call mapcolor(0,255,255,255)
        call mapcolor(1,255,000,000)
        call mapcolor(2,255,255,000)
        call mapcolor(3,255,000,255)
        call mapcolor(4,000,255,255)
        call mapcolor(5,000,255,000)
        call mapcolor(6,000,000,255)
        call mapcolor(7,000,000,000)
        call vinit()
        call color(0)
        call clear()
        call color(1)
        call pixel(1,1)
        call color(3)
        call pixel(3,3)
        call pixel(5,5,5)
        call print_ascii()
        call vexit()
     end program demo_pixel
