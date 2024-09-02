      program demo_trim
      implicit none
      character(len=:), allocatable :: str, strs(:)
      character(len=*),parameter :: brackets='( *("[",a,"]":,1x) )'
      integer :: i

         str='   trailing    '
         print brackets, str,trim(str) ! trims it

         str='   leading'
         print brackets, str,trim(str) ! no effect

         str='            '
         print brackets, str,trim(str) ! becomes zero length
         print *,  len(str), len(trim('               '))

        ! array elements are all the same length, so you often
        ! want to print them
         strs=[character(len=10) :: "Z"," a b c","ABC",""]

         write(*,*)'untrimmed:'
         ! everything prints as ten characters; nice for neat columns
         print brackets, (strs(i), i=1,size(strs))
         print brackets, (strs(i), i=size(strs),1,-1)
         write(*,*)'trimmed:'
         ! everything prints trimmed
         print brackets, (trim(strs(i)), i=1,size(strs))
         print brackets, (trim(strs(i)), i=size(strs),1,-1)

      end program demo_trim
