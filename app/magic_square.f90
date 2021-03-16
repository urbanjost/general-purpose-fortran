program demo_magic_square
!(LICENSE:PD)
use M_math, only : magic_square
implicit none

! ident_1="@(#)magic_square(1f):"

character(len=4096) :: arg
integer             :: ilen
integer,allocatable :: arr(:,:)
integer             :: i, j, k, ii
integer             :: itimes
integer             :: ios
   itimes=command_argument_count()
   if(itimes.eq.0)then
      do k=3,15
         call printit
         write(*,*)
      enddo

   else
      do ii=1,itimes
         call get_command_argument(ii,arg,length=ilen)
         read(arg,*,iostat=ios)k
         if(ios.eq.0)then
            call printit
         else
            write(*,*)trim(arg)
         endif
      enddo
   endif
contains
subroutine printit
   if(k.ne.2)then
   allocate(arr(k,k))
   call magic_square(arr(:k,:k))
   do i=1,k
      write(*,'(i2,":",*(i5):)')i,(int(arr(i,j)),j=1,k),sum(arr(k,:k))
   enddo
   deallocate(arr)
   else
      write(*,*)'cannot solve magic square for ',k
   endif
end subroutine printit
end program demo_magic_square
