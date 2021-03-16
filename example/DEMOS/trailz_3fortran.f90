           program demo_trailz
           use, intrinsic :: iso_fortran_env, only : integer_kinds, &
           & int8, int16, int32, int64
           implicit none
           integer(kind=int64) :: i, value
             write(*,*)'Default integer:'
             write(*,*)'bit_size=',bit_size(0)
             write(*,'(1x,i3,1x,i3,1x,b0)')-1,trailz(1),-1
             write(*,'(1x,i3,1x,i3,1x,b0)')0,trailz(0),0
             write(*,'(1x,i3,1x,i3,1x,b0)')1,trailz(1),1
             write(*,'(" huge(0)=",i0,1x,i0,1x,b0)')huge(0),trailz(huge(0)),huge(0)
             write(*,*)
             write(*,*)'integer(kind=int64):'
             do i=-1,62,5
                value=2**i
                write(*,'(i19,1x,i3,1x,b64.64)')value,trailz(value),value
             enddo
             value=huge(i)
             write(*,'(i19,1x,i3,1x,b64.64,"(huge(0_int64))")') &
             & value,trailz(value),value
           end program demo_trailz
