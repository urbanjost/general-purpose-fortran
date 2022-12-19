      program demo_unpack
      implicit none
      logical,parameter :: T=.true., F=.false.

      integer :: vector(2)  = [1,1]

      ! mask and field must conform
      integer,parameter :: r=2, c=2
      logical :: mask(r,c)  = reshape([ T,F,F,T ],[2,2])
      integer :: field(r,c) = 0, unity(2,2)

         ! basic usage
         unity = unpack( vector, mask, field )
         call print_matrix_int('unity=', unity)

         ! if FIELD is a scalar it is used to fill all the elements
         ! not assigned to by the vector and mask.
         call print_matrix_int('scalar field',         &
         & unpack(                                     &
         & vector=[ 1, 2, 3, 4 ],                      &
         & mask=reshape([ T,F,T,F,F,F,T,F,T ], [3,3]), &
         & field=0) )

      contains

         subroutine print_matrix_int(title,arr)
         ! convenience routine:
         ! just prints small integer arrays in row-column format
         implicit none
         character(len=*),intent(in)  :: title
         integer,intent(in)           :: arr(:,:)
         integer                      :: i
         character(len=:),allocatable :: biggest

            write(*,*)trim(title)
            ! make buffer to write integer into
            biggest='           '
            ! find how many characters to use for integers
            write(biggest,'(i0)')ceiling(log10(real(maxval(abs(arr)))))+2
            ! use this format to write a row
            biggest='("  [",*(i'//trim(biggest)//':,","))'
            ! print one row of array at a time
            do i=1,size(arr,dim=1)
               write(*,fmt=biggest,advance='no')arr(i,:)
               write(*,'(" ]")')
            enddo
         end subroutine print_matrix_int

      end program demo_unpack
