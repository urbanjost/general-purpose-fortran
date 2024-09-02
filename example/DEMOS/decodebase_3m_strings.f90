     program demo_decodebase
     use M_strings, only : codebase, decodebase
     implicit none
     integer           :: bd, i, r
     character(len=40) :: x
     character(len=*), parameter :: input(*) = [character(len=80) :: &
        '10  12345',   &
        '2   10111',   &
        '6   12345',   &
        '10  abcdef',  &
        '0   0']
     character(len=:),allocatable :: line
        print *, 'Base Conversion using decodebase(3f)'
        do i = 1, size(input)
           line=input(i)
           read (line, *) bd, x
           if (x == '0') exit
           if(.not.decodebase(x,bd,r)) then
             print *,'Error in decoding number.'
           endif
           write (*, '(a," in base ",i0," becomes ",i0," in base 10")')&
           & trim(x),bd,r
        end do
     end program demo_decodebase
