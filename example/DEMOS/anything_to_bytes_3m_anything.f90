     program demo_anything_to_bytes
     use M_anything,      only : anything_to_bytes
     implicit none
     integer :: i
        write(*,'(/,4(1x,z2.2))')anything_to_bytes([(i*i,i=1,10)])
        write(*,'(/,4(1x,z2.2))')anything_to_bytes([11.11,22.22,33.33])
        write(*,'(/,4(1x,z2.2))')anything_to_bytes('This is a string')
     end program demo_anything_to_bytes
