     program demo_specified
     use, intrinsic :: iso_fortran_env, only : &
     & stderr=>ERROR_UNIT, stdin=>INPUT_UNIT, stdout=>OUTPUT_UNIT
     use M_CLI2,  only : set_args, igets, rgets, specified, sget, lget
     implicit none

     ! Define args
     integer,allocatable  :: ints(:)
     real,allocatable     :: floats(:)
     logical              :: flag
     character(len=:),allocatable :: color
     character(len=:),allocatable :: list(:)
     integer :: i

      call set_args('&
         & --color:c "red"       &
         & --flag:f F            &
         & --ints:i 1,10,11      &
         & --floats:T 12.3, 4.56 &
         & ')
      ints=igets('ints')
      floats=rgets('floats')
      flag=lget('flag')
      color=sget('color')

      write(*,*)'color=',color
      write(*,*)'flag=',flag
      write(*,*)'ints=',ints
      write(*,*)'floats=',floats

      write(*,*)'was -flag specified?',specified('flag')

      ! elemental
      write(*,*)specified(['floats','ints  '])

      ! If you want to know if groups of parameters were specified use
      ! ANY(3) and ALL(3)
      write(*,*)'ANY:',any(specified(['floats','ints  ']))
      write(*,*)'ALL:',all(specified(['floats','ints  ']))

      ! For mutually exclusive
      if (all(specified(['floats','ints  '])))then
          write(*,*)'You specified both names --ints and --floats'
      endif

      ! For required parameter
      if (.not.any(specified(['floats','ints  '])))then
          write(*,*)'You must specify --ints or --floats'
      endif

     ! check if all values are in range from 10 to 30 and even
     write(*,*)'are all numbers good?',all([ints>=10,ints<= 30,(ints/2)*2==ints])

     ! perhaps you want to check one value at a time
     do i=1,size(ints)
        write(*,*)ints(i),[ints(i) >= 10,ints(i) <= 30,(ints(i)/2)*2 == ints(i)]
        if(all([ints(i) >= 10,ints(i) <= 30,(ints(i)/2)*2 == ints(i)]) )then
           write(*,*)ints(i),'is an even number from 10 to 30 inclusive'
        else
           write(*,*)ints(i),'is not an even number from 10 to 30 inclusive'
        endif
     enddo

     list = [character(len=10) :: 'red','white','blue']
     if( any(color == list) )then
        write(*,*)color,'matches a value in the list'
     else
        write(*,*)color,'not in the list'
     endif

     if(size(ints).eq.3)then
        write(*,*)'ints(:) has expected number of values'
     else
        write(*,*)'ints(:) does not have expected number of values'
     endif

     end program demo_specified
