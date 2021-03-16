           program demo_is_nan
           use M_units, only : is_nan
           real :: x
           character(len=*),parameter   :: linei='Infinity'
           character(len=*),parameter   :: line='NaN'
           character(len=:),allocatable :: readable
           real,parameter :: arr(*)=[-100.0,100.0,huge(0.0)]
              readable=linei
              read(readable,*)x
              write(*,*)is_nan(x),x   ! note Infinity is not a Nan
              write(*,*)is_nan(-x),-x
              readable=line
              read(readable,*)x
              write(*,*)is_nan(x),x
              write(*,*)x==x,x  ! note Nan is never equal to another value
              write(*,*)is_nan(arr),arr
           end program demo_is_nan
