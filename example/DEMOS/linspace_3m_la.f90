     program demo_linspace
     use M_LA,  only : linspace
     implicit none
     character(len=*), parameter :: gen='(*(g0, 1x))'
        write( *, gen ) linspace(  0,      9,    10 )
        write( *, gen ) linspace( 10.0,   20.0,  11 )
        write( *, gen ) linspace( 11.1d0, 12.1d0, 5 )
        write( *, gen ) linspace( 11.1,   12.1,   5 )
     end program demo_linspace
