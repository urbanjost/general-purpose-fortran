     program demo_base
     use M_strings, only: base
     implicit none
     integer                      :: ba, bd, i
     character(len=40)            :: x, y
     character(len=*), parameter  :: input(*) = [character(len=80) :: &
        '10 12345 10', &
        '2 10111 10', &
        '10 12345 20', &
        '10 abcdef 2', &
        '0 0 0']
     character(len=:),allocatable :: line
        print *, 'Base Conversion using base(3f)'
        do i = 1, size(input)
           line=input(i)
           read (line, *) bd, x, ba
           if (x == '0') exit
           if (base(x, bd, y, ba)) then
           else
              print *, 'Error in decoding/encoding numbers'
           end if
           write (*, '(a," in base ",i0," is ",a," in base ",i0)')&
           & trim(x),bd,trim(y),ba
        end do
     end program demo_base
