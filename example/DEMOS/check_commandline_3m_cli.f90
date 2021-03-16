           program demo_check_commandline
           use M_CLI,  only : unnamed, commandline, check_commandline
           implicit none
           integer                      :: i
           character(len=255)           :: message ! use for I/O error messages
           character(len=:),allocatable :: readme  ! stores updated namelist
           character(len=:),allocatable :: version_text(:), help_text(:)
           integer                      :: ios

           real               :: x, y, z
           logical            :: help, h
           equivalence       (help,h)
           namelist /args/ x,y,z,help,h
           character(len=*),parameter :: cmd='-x 1 -y 2 -z 3 --help F -h F'

           ! initialize namelist from string and then update from command line
           readme=commandline(cmd)
           !write(*,*)'README=',readme
           read(readme,nml=args,iostat=ios,iomsg=message)
           version_text=[character(len=80) :: "version 1.0","author: me"]
           help_text=[character(len=80) ::      &
            & "wish I put instructions","here", &
            & "I suppose?"]
           call check_commandline(ios,message,help_text,version_text)

           ! all done cracking the command line
           ! use the values in your program.
           write(*,nml=args)
           ! the optional unnamed values on the command line are
           ! accumulated in the character array "UNNAMED"
           if(size(unnamed).gt.0)then
              write(*,'(a)')'files:'
              write(*,'(i6.6,3a)')(i,'[',unnamed(i),']',i=1,size(unnamed))
           endif
           end program demo_check_commandline
