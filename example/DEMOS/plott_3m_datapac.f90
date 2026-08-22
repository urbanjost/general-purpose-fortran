       program demo_plott
       use M_datapac, only : plott, label
       implicit none
       integer ::  i
       integer,parameter :: dp=kind(0.0d0)
       real(kind=dp), allocatable ::  x(:), y(:)
          call label('plott')
          y=[(real(i)/10.0,i=1,30)]
          x=y**3.78-6*y**2.52+9*y**1.26
          call plott(x,y,size(x))
       end program demo_plott
