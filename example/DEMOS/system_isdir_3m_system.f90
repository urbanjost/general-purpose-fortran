     program demo_system_isdir
     use M_system, only : system_isdir
     use M_system, only : access=>system_access, R_OK
     use M_system, only : system_dir
     implicit none
     character(len=:),allocatable :: fnames(:)
     integer                      :: i
     character(len=80),parameter  :: names(*)=[ &
     & '/tmp            ', &
     & '/tmp/NOTTHERE   ', &
     & '/usr/local      ', &
     & '.               ', &
     & 'PROBABLY_NOT    ']
        !
        do i=1,size(names)
           write(*,*)' is ',trim(names(i)),' a directory? ', &
           & system_isdir(names(i))
        enddo
        !
        ! EXTENDED EXAMPLE: list readable non-hidden directories in
        !                   current directory
        fnames=system_dir(pattern='*') ! list all files in current directory
        ! select readable directories
        fnames=pack(fnames,system_isdir(fnames).and.access(fnames,R_OK))
        fnames=pack(fnames,fnames(:)(1:1) .ne.'.') ! skip hidden directories
        do i=1,size(fnames)
           write(*,*)' ',trim(fnames(i)),' is a directory'
        enddo
        !
     end program demo_system_isdir
