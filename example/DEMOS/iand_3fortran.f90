      program demo_iand
      implicit none
      integer :: a, b
       data a / z'f' /, b / z'3' /
       write (*,*) 'a=',a,' b=',b,'iand(a,b)=',iand(a, b)
       write (*,'(b32.32)') a,b,iand(a,b)
      end program demo_iand
