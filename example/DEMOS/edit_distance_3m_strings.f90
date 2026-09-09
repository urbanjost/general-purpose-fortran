     program demo_edit_distance
     use M_strings, only : edit_distance
        write(*,*)edit_distance('kittens','sitting')==3
        write(*,*)edit_distance('geek','gesek')==1
        write(*,*)edit_distance('Saturday','Sunday')==3
     end program demo_edit_distance
