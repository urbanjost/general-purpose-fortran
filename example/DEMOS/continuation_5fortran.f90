      program demo_continuation
      implicit none
      integer :: point(3)
      character(len=:),allocatable :: string

      ! one statement using continuation:
      integer,save :: xx(3,5)= reshape([& ! define in row-column order
      !-------------------------!
       1,    2,   3,   4,   5,  &  ! row 1
       10,  20,  30,  40,  50,  &  ! row 2
       11,  22,  33,  44,  55   &  ! row 3
      !-------------------------!

      ],shape(xx),order=[2,1])

      ! print it in row-column order too
        call print_matrix_int('xx array:',xx)
        xx(3,5)= -1051
        call print_matrix_int('xx array:',xx)

      ! So this is OK:
         POINT=[&   ! define a Point <X,Y,Z>
         & 10, &    ! the X component
         & 20, &    ! the Y component
         & 30  ]    ! the Z component

      ! because you can have comments after the ampersand when it is not
      ! a string.
      ! But this is not OK:
      !   STRING='&    ! create a sentence
      !   & This&      ! first word
      !   & is&        ! second word
      !   & sentence&  ! third word
      !   & a'         ! forth word (a comment here is OK)
      !Because when continuing a string you cannot have a comment after the "&".
      !
      ! This is OK:
         STRING='&
         ! create a sentence
         & This&
         ! first word
         & is&
         ! second word
         & sentence&
         ! third word
         & a'        ! forth word (a comment here is OK)
      ! because comment LINES can go anywhere in Fortran source files

      ! Dusty corners
         call splitting_a_token()
         call longstring()
      contains

      subroutine splitting_a_token()

      ! Often denoted by "e" in honor of Euler,
      ! Napier's constant is the base of the natural logarithm system.
      real(kind=kind(0.0d0)),parameter :: &
      & Napier_constant = 2.71828182845904523d0

      ! without continuation
      write(*,*)napier_constant

      ! splitting a token the & is required
      write(*,*)napier_&
      &constant

      ! the left-hand ampersand is required when splitting constants to,
      ! including characters strings
      write(*,*)'Expecting &
                &the value',2.71828182&
                &845904523d0

      !NOT ALLOWED <<<<<<
      !write(*,*)napier_&
      !constant
      !>>>>>>>

      ! splitting a token is not recommended as it complicates identifying
      ! the use of a token name.

      end subroutine splitting_a_token
      Subroutine LongString()
      ! Long strings:

      Character (len=200) :: string1, String2
      character(len=:), allocatable :: a,b,c, big

         string1 = "A very long string that won't fit on a single &
                    &line can be made through proper continuation."

         ! alternatives to continuation lines
         string2 = "A very long string that won't fit on a single " // &
                   "line can be made through proper continuation " // &
                   "and concatenation of multiple strings."
         print *, "string1=",string1
         print *, "string2=",string2

         ! append multiple strings together to construct a long line
         a=repeat('A',100)
         b=repeat('B',100)
         big=a//b
         c=repeat('C',100)
         big=a//c
         big=big//"more at end"
         print *, "big=",big

      End Subroutine LongString

      subroutine print_matrix_int(title,arr)
      ! bonus points -- print an integer array in RC order with bells on.
      ! ie. It calculates the width needed for the longest variable and
      ! puts a frame around the array
      implicit none
      character(len=*),intent(in)  :: title
      integer,intent(in)           :: arr(:,:)
      integer                      :: i
      integer                      :: size_needed
      character(len=:),allocatable :: biggest
        write(*,*)trim(title)
        biggest='           '  ! make buffer to write integer into
        ! find how many characters to use for integers
        size_needed=ceiling(log10(real(maxval(abs(arr)))))+2
        write(biggest,'(i0)')size_needed
        ! use this format to write a row
        biggest='("   |",*(i'//trim(biggest)//':," |"))'
        ! print one row of array at a time
        write(*,'(*(g0))')&
        &'   #',(repeat('-',size_needed),'-#',i=1,size(arr,dim=2))
        do i=1,size(arr,dim=1)
           write(*,fmt=biggest,advance='no')arr(i,:)
           write(*,'(" |")')
        enddo
        write(*,'(*(g0))')&
        &'   #',(repeat('-',size_needed),'-#',i=1,size(arr,dim=2))
      end subroutine print_matrix_int
      end program demo_continuation
