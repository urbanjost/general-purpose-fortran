       program demo_anything_to_bytes
       use M_anything,      only : anything_to_bytes
       implicit none
       integer :: i
          write(*,"('select various types')")
          write(*,'(/,16(1x,z2.2))')anything_to_bytes([(i*i,i=1,10)])
          write(*,'(/,16(1x,z2.2))')anything_to_bytes([11.11,22.22,33.33])
          write(*,'(/,16(1x,z2.2))')anything_to_bytes('This is a string')

          write(*,"(/,'compare to TRANSFER(3f)')")
          write(*,'(/,16(1x,z2.2))') transfer([(i*i,i=1,10)],[' '])
          write(*,'(/,16(1x,z2.2))') transfer([11.11,22.22,33.33],[' '])
          write(*,'(/,16(1x,z2.2))') transfer('This is a string',[' '])
       end program demo_anything_to_bytes
