       program demo_cpad
        use M_strings, only : cpad
        implicit none
           write(*,'("[",a,"]")') cpad( 'my string', 20)
           write(*,'("[",a,"]")') cpad( 'my string   ', 20)
           write(*,'("[",a,"]")') cpad( '   my string', 20)
           write(*,'("[",a,"]")') cpad( '   my string   ', 20)
           write(*,'("[",a,"]")') cpad( valuein=42 , length=7)
           write(*,'("[",a,"]")') cpad( valuein=1.0/9.0 , length=20)
       end program demo_cpad
