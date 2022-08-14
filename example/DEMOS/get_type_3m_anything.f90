     program demo_get_type
     use M_anything,      only : get_type
     implicit none
     integer :: i
        write(*,*)get_type([(i*i,i=1,10)])
        write(*,*)get_type([11.11,22.22,33.33])
        write(*,*)get_type('This is a string')
        write(*,*)get_type(30.0d0)
     end program demo_get_type
