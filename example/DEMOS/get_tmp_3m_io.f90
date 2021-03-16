           program demo_get_tmp
           use M_io, only : get_tmp, uniq
           implicit none
           character(len=:),allocatable :: answer
              answer=get_tmp()
              write(*,*)'result is ',answer
              answer=get_tmp()//uniq('_scratch',create=.false.)
              write(*,*)'the file ',answer,' was a good scratch file name, at least a moment ago'
           end program demo_get_tmp
