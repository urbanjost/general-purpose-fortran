           program demo_get_namelist
           use M_args,  only : unnamed, get_namelist, print_dictionary
           implicit none
           integer                      :: i
           character(len=255)           :: message ! use for I/O error messages
           character(len=:),allocatable :: readme  ! stores updated namelist
           integer                      :: ios
           real               :: x, y, z
           logical            :: help, h
           equivalence       (help,h)
           namelist /args/ x,y,z,help,h
           character(len=*),parameter :: cmd='&ARGS X=1 Y=2 Z=3 HELP=F H=F /'
           ! initialize namelist from string and then update from command line
           readme=cmd
           read(readme,nml=args,iostat=ios,iomsg=message)
           if(ios.eq.0)then
              ! update cmd with options from command line
              readme=get_namelist(cmd)
              read(readme,nml=args,iostat=ios,iomsg=message)
           endif
           if(ios.ne.0)then
              write(*,'("ERROR:",i0,1x,a)')ios, trim(message)
              call print_dictionary('OPTIONS:')
              stop 1
           endif
           ! all done cracking the command line
           ! use the values in your program.
           write(*,nml=args)
           ! the optional unnamed values on the command line are
           ! accumulated in the character array "UNNAMED"
           if(size(unnamed).gt.0)then
              write(*,'(a)')'files:'
              write(*,'(i6.6,3a)')(i,'[',unnamed(i),']',i=1,size(unnamed))
           endif
           end program demo_get_namelist
