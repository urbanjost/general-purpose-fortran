       program demo_loc
       use M_datapac, only : loc, label
       implicit none
       integer ::  i
       real, allocatable ::  x(:), y(:)
          call label('loc')
          y=[(real(i)/10.0,i=1,20000)]
          x=y**3.78-6*y**2.52+9*y**1.26
          call loc(y,size(y))
       end program demo_loc
