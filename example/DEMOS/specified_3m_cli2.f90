     program demo_specified
     use M_CLI2,  only : set_args, get_args, specified
     implicit none
     ! DEFINE ARGS
     integer                 :: flag
     integer,allocatable     :: ints(:)
     real,allocatable        :: twonames(:)

     ! IT IS A BAD IDEA TO NOT HAVE THE SAME DEFAULT VALUE FOR ALIASED
     ! NAMES BUT CURRENTLY YOU STILL SPECIFY THEM
      call set_args('-flag 1 -f 1 -ints 1,2,3 -i 1,2,3 -twonames 11.3 -T 11.3')

     ! ASSIGN VALUES TO ELEMENTS CONDITIONALLY CALLING WITH SHORT NAME
      call get_args('flag',flag)
      if(specified('f'))call get_args('f',flag)
      call get_args('ints',ints)
      if(specified('i'))call get_args('i',ints)
      call get_args('twonames',twonames)
      if(specified('T'))call get_args('T',twonames)

      ! IF YOU WANT TO KNOW IF GROUPS OF PARAMETERS WERE SPECIFIED USE
      ! ANY(3f) and ALL(3f)
      write(*,*)specified(['twonames','T       '])
      write(*,*)'ANY:',any(specified(['twonames','T       ']))
      write(*,*)'ALL:',all(specified(['twonames','T       ']))

      ! FOR MUTUALLY EXCLUSIVE
      if (all(specified(['twonames','T       '])))then
          write(*,*)'You specified both names -T and -twonames'
      endif

      ! FOR REQUIRED PARAMETER
      if (.not.any(specified(['twonames','T       '])))then
          write(*,*)'You must specify -T or -twonames'
      endif
      ! USE VALUES
        write(*,*)'flag=',flag
        write(*,*)'ints=',ints
        write(*,*)'twonames=',twonames
      end program demo_specified
