     program demo_mat_magic
     use M_LA, only : mat_magic
     implicit none
     integer,parameter :: isize=10
     doubleprecision   :: arr(isize,isize)
     integer           :: i, j, k
        do k=1,isize
           write(*,'(*(g0,1x))')'K=',k
           call mat_magic(arr,size(arr,dim=1),k)
           do i=1,k
              write(*,'(i2,":",*(i5):)')i,&
               (nint(arr(i,j)),j=1,k),&
               nint(sum(arr(k,:k)))
           enddo
        enddo
     end program demo_mat_magic
