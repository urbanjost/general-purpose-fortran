       program demo_bytes_to_anything
       use M_anything,      only : bytes_to_anything
       use M_anything,      only : anything_to_bytes
       implicit none
       character(len=1),allocatable :: chars(:)
       integer :: ints(10)
       integer :: i
          chars=anything_to_bytes([(i*i,i=1,size(ints))])
          write(*,'(/,4(1x,z2.2))')chars
          call bytes_to_anything(chars,ints)
          write(*,'(*(g0,1x))')ints
       end program demo_bytes_to_anything
