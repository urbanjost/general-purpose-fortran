           program demo_leadz
           implicit none
           integer :: value, i
           character(len=80) :: f
             write(*,'(*(g0))')'BIT_SIZE=',bit_size(value)
             ! make a format statement for writing a value as a bit string
             write(f,'("(b",i0,".",i0,")")')bit_size(value),bit_size(value)
             ! show output for various integer values
             value=0
             do i=0,bit_size(value)-1
                write (*,'("LEADING ZERO BITS=",i3,1x)') leadz(value)
                write (*,'(" FOR VALUE ")',advance='no')
                write(*,f,advance='no') value
                write(*,'(*(1x,g0))') "OR",value
                value=value+2**(i)
             enddo
           end program demo_leadz
