     program demo_rank
     ! create an index that can order an array in ascending order
     use M_orderpack, only : rank
     implicit none
     character(len=*),parameter :: g='(*(g0,1x))'
     integer,parameter             :: dp=kind(0.0d0)
     integer,parameter             :: isz=10000
     real(kind=dp)                 :: dd(isz)
     real(kind=dp)                 :: pp
     integer                       :: indx(isz)
     integer                       :: i,j,k
     character(len=:),allocatable  :: strings(:)
     integer,allocatable           :: cindx(:)
        ! make some random numbers
        call random_seed()
        call random_number(dd)
        dd=dd-0.50_dp
        k=int(log(huge(0.0_dp))/log(2.0_dp))-1
        do i=1,isz
           call random_number(pp)
           j=floor((k+1)*pp)
           dd(i)=dd(i)*(2.0_dp**j)
        enddo
        ! rank the numeric data
        call rank(dd,indx)
        ! check order
        do i=1,isz-1
           if(dd(indx(i)).gt.dd(indx(i+1)))then
              write(*,g)'ERROR: data not sorted i=',i,'index=',indx(i), &
              & 'values ',dd(indx(i)),dd(indx(i+1))
              stop 1
           endif
        enddo
        ! sort data using rank values
        dd=dd(indx)
        write(*,g)'sorted ',isz,'values'
        write(*,g)'from',dd(1),'to',dd(isz)
        write(*,*)minval(dd).eq.dd(1)
        write(*,*)maxval(dd).eq.dd(isz)
        write(*,*)minloc(dd).eq.1
        write(*,*)maxloc(dd).eq.isz
        ! do a character sort
        strings= [ character(len=20) ::                               &
        & 'red',    'green', 'blue', 'yellow', 'orange',   'black', &
        & 'white',  'brown', 'gray', 'cyan',   'magenta',           &
        & 'purple']
        if(allocated(cindx))deallocate(cindx);allocate(cindx(size(strings)))

        write(*,'(a,8(a:,","))')'BEFORE ',&
                & (trim(strings(i)),i=1,size(strings))

        call rank(strings,cindx)

        write(*,'(a,8(a:,","))')'SORTED ',&
                & (trim(strings(cindx(i))),i=1,size(strings))

        strings=strings(cindx) ! sort the array using the rank index

        do i=1,size(strings)-1
           if(strings(i).gt.strings(i+1))then
              write(*,*)'Error in sorting strings a-z'
           endif
        enddo
     end program demo_rank
