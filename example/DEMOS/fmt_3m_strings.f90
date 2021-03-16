          program demo_fmt
          use M_strings, only : fmt
          implicit none
          character(len=:),allocatable :: paragraph(:)
          character(len=*),parameter    :: string= '&
           &one two three four five &
           &six seven eight &
           &nine ten eleven twelve &
           &thirteen fourteen fifteen sixteen &
           &seventeen'

          write(*,*)'LEN=',len(string)
          write(*,*)'INPUT:'
          write(*,*)string

          paragraph=fmt(string,40)
          write(*,*)'LEN=',len(paragraph),' SIZE=',size(paragraph)
          write(*,*)'OUTPUT:'
          write(*,'(a)')paragraph

          write(*,'(a)')fmt(string,0)
          write(*,'(3x,a)')fmt(string,47)

          end program demo_fmt
