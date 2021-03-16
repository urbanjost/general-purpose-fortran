          program demo_swap
          use M_sort, only : swap
          integer             :: iarray(2)=[10,20]
          real                :: rarray(2)=[11.11,22.22]
          doubleprecision     :: darray(2)=[1234.56789d0,9876.54321d0]
          complex             :: carray(2)=[(1234,56789),(9876,54321)]
          logical             :: larray(2)=[.true.,.false.]
          character(len=16)   :: string(2)=["First string    ","The other string"]

          integer             :: one(13)=1
          integer             :: two(13)=2

          integer             :: one2(3,3)=1
          integer             :: two2(3,3)=2

             print *, "integers before swap ", iarray
             call swap (iarray(1), iarray(2))
             print *, "integers after swap  ", iarray

             print *, "reals before swap ", rarray
             call swap (rarray(1), rarray(2))
             print *, "reals after swap  ", rarray

             print *, "doubles before swap ", darray
             call swap (darray(1), darray(2))
             print *, "doubles after swap  ", darray

             print *, "complexes before swap ", carray
             call swap (carray(1), carray(2))
             print *, "complexes after swap  ", carray

             print *, "logicals before swap ", larray
             call swap (larray(1), larray(2))
             print *, "logicals after swap  ", larray

             print *, "strings before swap ", string
             call swap (string(1), string(2))
             print *, "strings after swap ", string

             write(*,*)'swap two vectors'
             write(*,'("one before: ",*(i0,:","))') one
             write(*,'("two before: ",*(i0,:","))') two
             call swap(one,two)
             write(*,'("one after: ",*(i0,:","))') one
             write(*,'("two after: ",*(i0,:","))') two

             write(*,*)'given these arrays initially each time '
             one2=1
             two2=2
             call printarrays()

             write(*,*)'swap two rows'
             one2=1
             two2=2
             call swap(one2(2,:),two2(3,:))
             call printarrays()

             write(*,*)'swap two columns'
             one2=1
             two2=2
             call swap(one2(:,2),two2(:,2))
             call printarrays()

             write(*,*)'swap two arrays with same number of elements'
             one2=1
             two2=2
             call swap(one2,two2)
             call printarrays()

             contains
             subroutine printarrays()
             integer :: i
             do i=1,size(one2(1,:))
                write(*,'(*(i0,:","))') one2(i,:)
             enddo
             write(*,*)
             do i=1,size(two2(1,:))
                write(*,'(*(i0,:","))') two2(i,:)
             enddo
             end subroutine printarrays

              end program demo_swap
