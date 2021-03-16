           program demo_get_namelist
           use M_args,  only : unnamed
           implicit none
           integer :: i

           ! declare and initialize a namelist
           ! letter_ denotes an uppercase short command keyword
           ! all values should be allocated before calling get_args(3f)
           real              :: x=111.1, y=222.2, z=333.3
           real              :: point(3)=[10.0,20.0,30.0]
           character(len=80) :: title=" "
           logical           :: help=.false., version=.false.
           logical           :: l=.false., l_=.false., v=.false., h=.false.
           ! you can equivalence short and long options
           equivalence       (help,h),(version,v)
           ! just add a variable here and it is a new parameter !!
           namelist /args/ x,y,z,point,title,help,h,version,v,l,l_
           !
              call get_args()  ! crack command line options
              ! do stuff with your variables
              write(*,*)'VALUES ARE NOW'
              write(*,nml=args)
              if(size(unnamed).gt.0)then
                 write(*,'(a)')'UNNAMED:'
                 write(*,'(i6.6,3a)')(i,'[',unnamed(i),']',i=1,size(unnamed))
              endif
           contains
           subroutine get_args()
           ! The NAMELIST cannot be passed as an option to a routine so this
           ! routine must be in a contained routine or directly in the body of
           ! the routine that declares the NAMELIST. get_args(3f) should not
           ! need changed except for possibly the length of HOLD_NAMELIST
           use M_args,    only : get_namelist, print_dictionary, oneline
           !
           integer :: ios, i
           character(len=255) :: message ! use for I/O error messages
           character(len=:),allocatable :: readme  ! stores updated namelist
           ! make big enough for all of namelist
           character(len=10000) :: hold_namelist(60)
           ! the routine needs a copy of the options to determine what values
           ! are character and logical versus numeric
              write(hold_namelist,nml=args,iostat=ios,iomsg=message)
              if(ios.eq.0)then
                 ! pass in the namelist and get an updated copy that includes
                 ! values specified on the command line
                 readme=get_namelist(oneline(hold_namelist))
                 ! read the updated namelist to update the values
                 ! in the namelist group
                 read(readme,nml=args,iostat=ios,iomsg=message)
              endif
              if(ios.ne.0)then
                 write(*,'("ERROR:",i0,1x,a)')ios, trim(message)
                 call print_dictionary()
                 stop 1
              endif
              ! all done cracking the command line
           end subroutine get_args
           end program demo_get_namelist
