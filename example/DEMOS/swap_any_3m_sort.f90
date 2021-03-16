          program demo_swap_any
          use M_sort, only : swap_any
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

             print *, "integers before swap_any ", iarray
             call swap_any (iarray(1), iarray(2))
             print *, "integers after swap_any  ", iarray

             print *, "reals before swap_any ", rarray
             call swap_any (rarray(1), rarray(2))
             print *, "reals after swap_any  ", rarray

             print *, "doubles before swap_any ", darray
             call swap_any (darray(1), darray(2))
             print *, "doubles after swap_any  ", darray

             print *, "complexes before swap_any ", carray
             call swap_any (carray(1), carray(2))
             print *, "complexes after swap_any  ", carray

             print *, "logicals before swap_any ", larray
             call swap_any (larray(1), larray(2))
             print *, "logicals after swap_any  ", larray

             print *, "strings before swap_any ", string
             call swap_any (string(1), string(2))
             print *, "strings after swap_any ", string

             write(*,*)'swap_any two vectors'
             write(*,'("one before: ",*(i0,:","))') one
             write(*,'("two before: ",*(i0,:","))') two
             call swap_any(one,two)
             write(*,'("one after: ",*(i0,:","))') one
             write(*,'("two after: ",*(i0,:","))') two

             write(*,*)'given these arrays initially each time '
             one2=1
             two2=2
             call printarrays()

             write(*,*)'GETS THIS WRONG'
             write(*,*)'swap_any two rows'
             one2=1
             two2=2
             call swap_any(one2(2,:),two2(3,:))
             call printarrays()

             write(*,*)'GETS THIS WRONG'
             write(*,*)'swap_any two columns'
             one2=1
             two2=2
             call swap_any(one2(:,2),two2(:,2))
             call printarrays()

             write(*,*)'CANNOT DO MULTI-DIMENSIONAL ARRAYS YET'
             write(*,*)'swap_any two arrays with same number of elements'
             one2=1
             two2=2
             !call swap_any(one2,two2)
             !call printarrays()

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

              end program demo_swap_any
