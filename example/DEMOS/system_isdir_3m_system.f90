     program demo_system_isdir
     use M_system, only : system_isdir
     use M_system, only : access=>system_access, R_OK
     use M_system, only : system_dir
     implicit none
     character(len=1024),allocatable :: filenames(:) ! BUG: cannot use len=: in gfortran 8.3.1
     integer                         :: i
     character(len=80),parameter     :: names(*)=[ &
     & '/tmp            ', &
     & '/tmp/NOTTHERE   ', &
     & '/usr/local      ', &
     & '.               ', &
     & 'PROBABLY_NOT    ']
        !
        do i=1,size(names)
           write(*,*)' is ',trim(names(i)),' a directory? ', system_isdir(names(i))
        enddo
        !
        ! EXTENDED EXAMPLE: list readable non-hidden directories in current directory
        filenames=system_dir(pattern='*') ! list all files in current directory
        ! select readable directories
        filenames=pack(filenames,system_isdir(filenames).and.access(filenames,R_OK))
        filenames=pack(filenames,filenames(:)(1:1) .ne.'.') ! skip hidden directories
        do i=1,size(filenames)
           write(*,*)' ',trim(filenames(i)),' is a directory'
        enddo
        !
     end program demo_system_isdir
