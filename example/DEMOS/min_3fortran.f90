      program demo_min
      implicit none
      integer :: i
      integer :: rectangle(3,4)=reshape([(-6+i,i=0,11)],[3,4])
          print *, 'basics'
          print *, min(10.0,11.0,30.0,-100.0)
          print *, min(-200.0,-1.0)
          print *, 'elemental'
          print *, min(1,[2,3,4])
          print *, min(5,[2,3,4])

          print *, 'box:'
          do i=1,size(rectangle,dim=1)
             write(*,'(*(i3,1x))')rectangle(i,:)
          enddo
          print *, 'make all values 0 or less:'
          do i=1,size(rectangle,dim=1)
             write(*,'(*(i3,1x))')min(rectangle(i,:),0)
          enddo
      end program demo_min
