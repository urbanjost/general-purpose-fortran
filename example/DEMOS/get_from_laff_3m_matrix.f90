          program demo_get_from_laff
          use M_matrix, only : laff, get_from_laff, put_into_laff
          implicit none
          doubleprecision,allocatable :: darr(:,:)
          real,allocatable            :: rarr(:,:)
          integer,allocatable         :: ivec(:)
          integer                     :: ierr
          integer                     :: i
          character(len=*),parameter  :: gen='(*(g0,1x))'

             ! create an array in LAFF so have something to get
             call laff('A=rand(4,5)*10.5,long,A')

             ! get the array as a REAL array
             call get_from_laff('A',rarr,ierr)
             write(*,gen)'in calling program RARR=',shape(rarr)
             write(*,gen)(rarr(i,:),new_line('A'),i=1,size(rarr,dim=1))

             ! get the array as a DOUBLEPRECISION  array
             call get_from_laff('A',darr,ierr)
             write(*,gen)'in calling program darr=',shape(darr)
             write(*,gen)(darr(i,:),new_line('A'),i=1,size(darr,dim=1))

             ! get the array as an INTEGER vector, much like the
             ! PUSH(3f) intrinsic
             call get_from_laff('A',ivec,ierr)
             write(*,gen)'in calling program ivec=',shape(ivec)
             write(*,gen)ivec

              end program demo_get_from_laff
