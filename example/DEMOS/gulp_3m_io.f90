     program demo_gulp
     use M_io,      only : gulp
     implicit none
     character(len=4096)          :: FILENAME   ! file to read
     character(len=:),allocatable :: pageout(:) ! array to hold file in memory
     integer                      :: longest, lines, i
        ! get a filename
        call get_command_argument(1, FILENAME)
        ! allocate character array and copy file into it
        call gulp(FILENAME,pageout)
        if(.not.allocated(pageout))then
           write(*,*)'*demo_gulp* failed to load file '//FILENAME
        else
           ! write file from last line to first line
           longest=len(pageout)
           lines=size(pageout)
           write(*,*)'number of lines is ',lines
           write(*,*)'and length of lines is ',longest
           write(*,'(a)')repeat('%',longest+2)
           write(*,'("%",a,"%")')(trim(pageout(i)),i=lines,1,-1)
           write(*,'(a)')repeat('%',longest+2)
           deallocate(pageout)  ! release memory
        endif
     end program demo_gulp
