          program demo_merge
          implicit none
          integer :: tsrc(2,3), fsrc(2,3), answer(2,3)
          logical :: mask(2,3)
          integer :: i
             tsrc(1,:)=[ 1,6,5 ]; fsrc(1,:)=[ 0,3,2 ]; &
             & mask(1,:)=[.true., .false.,.true.]
             tsrc(2,:)=[ 2,4,6 ]; fsrc(2,:)=[ 7,4,8 ]; &
             & mask(2,:)=[.false.,.false.,.true.]
             answer=merge(tsrc,fsrc,mask)
             write(*,'(3i2)')(answer(i,:),i=1,size(answer,dim=1))
          end program demo_merge
