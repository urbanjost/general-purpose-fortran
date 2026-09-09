     program demo_specified
     use M_CLI,  only : commandline, check_commandline, specified
     implicit none
     character(len=255)           :: message ! use for I/O error messages
     character(len=:),allocatable :: readme  ! stores updated namelist
     integer                      :: ios
     real                         :: x, y, z; namelist /args/ x, y, z
     character(len=*),parameter :: cmd='-x 1 -y 2 -z 3'
        ! initialize namelist from string and then update from command line
        readme=commandline(cmd)
        read(readme,nml=args,iostat=ios,iomsg=message)
        call check_commandline(ios,message)
        write(*,*)specified(['x','y','z'])
        ! ANY(3f) and ALL(3f) ARE USEFUL IF YOU WANT TO KNOW IF GROUPS
        ! OF PARAMETERS WERE SPECIFIED
        write(*,*)'ANY:',any(specified(['x','y','z']))
        write(*,*)'ALL:',all(specified(['x','y','z']))
        ! FOR MUTUALLY EXCLUSIVE
        if (all(specified(['x','y'])))then
            write(*,*)'You specified both names -x and -y'
        endif
        ! FOR REQUIRED PARAMETER
        if (.not.all(specified(['x','y','z'])))then
          write(*,*)'You must specify all three of -x,-y or -z'
        endif
        ! all done cracking the command line. Use the values in
        ! your program.
        write(*,nml=args)
     end program demo_specified
