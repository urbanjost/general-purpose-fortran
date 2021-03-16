           program demo_new_line
             implicit none
             write(*,'(A)') 'This is record 1.'//NEW_LINE('A')//'This is record 2.'
           end program demo_new_line
