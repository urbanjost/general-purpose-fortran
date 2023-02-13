     program demo_get_from_lala
     use M_matrix, only : lala, get_from_lala, put_into_lala
     implicit none
     doubleprecision,allocatable :: darr(:,:)
     real,allocatable            :: rarr(:,:)
     integer,allocatable         :: ivec(:)
     integer                     :: ierr
     integer                     :: i
     character(len=*),parameter  :: gen='(*(g0,1x))'

        ! create an array in LALA so have something to get
        call lala('A=rand(4,5)*10.5,long,A')

        ! get the array as a REAL array
        call get_from_lala('A',rarr,ierr)
        write(*,gen)'in calling program RARR=',shape(rarr)
        write(*,gen)(rarr(i,:),new_line('A'),i=1,size(rarr,dim=1))

        ! get the array as a DOUBLEPRECISION  array
        call get_from_lala('A',darr,ierr)
        write(*,gen)'in calling program darr=',shape(darr)
        write(*,gen)(darr(i,:),new_line('A'),i=1,size(darr,dim=1))

        ! get the array as an INTEGER vector, much like the
        ! PUSH(3f) intrinsic
        call get_from_lala('A',ivec,ierr)
        write(*,gen)'in calling program ivec=',shape(ivec)
        write(*,gen)ivec

     end program demo_get_from_lala
