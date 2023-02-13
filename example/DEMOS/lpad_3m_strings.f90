       program demo_lpad
        use M_strings, only : lpad
        implicit none
           write(*,'("[",a,"]")') lpad( 'my string', 20)
           write(*,'("[",a,"]")') lpad( 'my string   ', 20)
           write(*,'("[",a,"]")') lpad( '   my string', 20)
           write(*,'("[",a,"]")') lpad( '   my string   ', 20)
           write(*,'("[",a,"]")') lpad( valuein=42 , length=7)
           write(*,'("[",a,"]")') lpad( valuein=1.0/9.0 , length=20)
       end program demo_lpad
