          program demo_pack
          implicit none
          integer, allocatable :: m(:)
          character(len=10) :: c(4)

           ! gathering nonzero elements from an array:
             m = [ 1, 0, 0, 0, 5, 0 ]
             write(*, fmt="(*(i0, ' '))") pack(m, m /= 0)

           ! Gathering nonzero elements from an array and appending elements
           ! from VECTOR till the size of the mask array (or array size if the
           ! mask is scalar):
             m = [ 1, 0, 0, 2 ]
             write(*, fmt="(*(i0, ' '))") pack(m, m /= 0, [ 0, 0, 3, 4 ])
             write(*, fmt="(*(i0, ' '))") pack(m, m /= 0 )

           ! select strings whose second character is "a"
             c = [ character(len=10) :: 'ape', 'bat', 'cat', 'dog']
             write(*, fmt="(*(g0, ' '))") pack(c, c(:)(2:2) == 'a' )

           ! creating a quicksort using PACK(3f)
             block
             intrinsic random_seed, random_number
             real :: x(10)
                call random_seed()
                call random_number(x)
                write (*,"(a10,*(1x,f0.3))") "initial",x
                write (*,"(a10,*(1x,f0.3))") "sorted",qsort(x)
             endblock
          contains
          !
          ! concise quicksort from @arjen and @beliavsky shows recursion,
          ! array sections, and vectorized comparisons.
          !
          pure recursive function qsort(values) result(sorted)
          intrinsic pack, size
          real, intent(in) :: values(:)
          real             :: sorted(size(values))
             if (size(values) > 1) then
                sorted = &
            & [qsort(pack(values(2:),values(2:)<values(1))), values(1), &
                & qsort(pack(values(2:),values(2:)>=values(1)))]
             else
                sorted = values
             endif
          end function qsort
          end program demo_pack
