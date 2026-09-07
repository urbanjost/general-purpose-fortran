         program demo_reduce
         implicit none
         character(len=*),parameter :: f='("[",*(g0,",",1x),"]")'
         integer,allocatable :: arr(:), b(:,:)

         ! Basic usage:
            ! the product of the elements of an array
            arr=[1, 2, 3, 4 ]
            write(*,*) arr
            write(*,*) 'product=', reduce(arr, my_mult)
            write(*,*) 'sum=', reduce(arr, my_sum)

         ! Examples of masking:
            ! the product of only the positive elements of an array
            arr=[1, -1, 2, -2, 3, -3 ]
            write(*,*)'positive value product=',reduce(arr, my_mult, mask=arr>0)
         ! sum values ignoring negative values
            write(*,*)'sum positive values=',reduce(arr, my_sum, mask=arr>0)

         ! a single-valued array returns the single value as the
         ! calls to the operator stop when only one element remains
            arr=[ 1234 ]
            write(*,*)'single value sum',reduce(arr, my_sum )
            write(*,*)'single value product',reduce(arr, my_mult )

         ! Example of operations along a dimension:
         !  If B is the array   1 3 5
         !                      2 4 6
            b=reshape([1,2,3,4,5,6],[2,3])
            write(*,f) REDUCE(B, MY_MULT),'should be [720]'
            write(*,f) REDUCE(B, MY_MULT, DIM=1),'should be [2,12,30]'
            write(*,f) REDUCE(B, MY_MULT, DIM=2),'should be [15, 48]'

         contains

         pure function my_mult(a,b) result(c)
         integer,intent(in) :: a, b
         integer            :: c
            c=a*b
         end function my_mult

         pure function my_sum(a,b) result(c)
         integer,intent(in) :: a, b
         integer            :: c
            c=a+b
         end function my_sum

         end program demo_reduce
