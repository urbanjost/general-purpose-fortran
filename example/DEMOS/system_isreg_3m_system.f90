     program demo_system_isreg
     use M_system, only : isreg=>system_isreg, islnk=>system_islnk
     use M_system, only : access=>system_access, R_OK
     use M_system, only : system_dir
     implicit none
     character(len=:),allocatable :: filenames(:)
     logical,allocatable :: mymask(:)
     integer                         :: i
     ! list readable non-hidden regular files and links in current directory
          ! make list of all files in current directory
          filenames=system_dir(pattern='*')
          ! select regular files and links
          mymask= isreg(filenames).or.islnk(filenames)
          ! skip hidden directories in those
          where(mymask) mymask=filenames(:)(1:1).ne.'.'
          ! select readable files in those
          where(mymask) mymask=access(filenames,R_OK)
          filenames=pack(filenames,mask=mymask)
          write(*,'(a)')(trim(filenames(i)),i=1,size(filenames))
     end program demo_system_isreg
