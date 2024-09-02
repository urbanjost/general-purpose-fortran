      program demo_leadz
      implicit none
      integer :: value, i
      character(len=80) :: f

        ! make a format statement for writing a value as a bit string
        write(f,'("(b",i0,".",i0,")")')bit_size(value),bit_size(value)

        ! show output for various integer values
        value=0
        do i=-150, 150, 50
           value=i
           write (*,'("LEADING ZERO BITS=",i3)',advance='no') leadz(value)
           write (*,'(" OF VALUE ")',advance='no')
           write(*,f,advance='no') value
           write(*,'(*(1x,g0))') "AKA",value
        enddo
        ! Notes:
        ! for two's-complements programming environments a negative non-zero
        ! integer value will always start with a 1 and a positive value with 0
        ! as the first bit is the sign bit. Such platforms are very common.
      end program demo_leadz
