       program demo_rpad
        use M_strings, only : rpad
        implicit none
           write(*,'("[",a,"]")') rpad( 'my string', 20)
           write(*,'("[",a,"]")') rpad( 'my string   ', 20)
           write(*,'("[",a,"]")') rpad( '   my string', 20)
           write(*,'("[",a,"]")') rpad( '   my string   ', 20)
           write(*,'("[",a,"]")') rpad( valuein=42 , length=7)
           write(*,'("[",a,"]")') rpad( valuein=1.0/9.0 , length=20)
       end program demo_rpad
