           program demo_pack
           implicit none
           call test1()
           call test2()
           call test3()
           contains
           !
           subroutine test1()
           ! gathering nonzero elements from an array:
           integer :: m(6)
             m = [ 1, 0, 0, 0, 5, 0 ]
             write(*, fmt="(*(i0, ' '))") pack(m, m /= 0)  ! "1 5"
           end subroutine test1
           !
           subroutine test2()
           ! Gathering nonzero elements from an array and appending elements
           ! from VECTOR:
           integer :: m(4)
             m = [ 1, 0, 0, 2 ]
             write(*, fmt="(*(i0, ' '))") pack(m, m /= 0, [ 0, 0, 3, 4 ])  ! "1 2 3 4"
           end subroutine test2
           !
           subroutine test3()
           ! select strings whose second character is "a"
           character(len=10) :: m(4)
           m = [ character(len=10) :: 'ape', 'bat', 'cat', 'dog']
             write(*, fmt="(*(g0, ' '))") pack(m, m(:)(2:2) == 'a' )  ! "bat" "cat"
           end subroutine test3
           !
           end program demo_pack
