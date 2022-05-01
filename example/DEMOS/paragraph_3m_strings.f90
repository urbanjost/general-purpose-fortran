     program demo_paragraph
     use M_strings, only : paragraph
     implicit none
     character(len=:),allocatable :: paragrph(:)
     character(len=*),parameter    :: string= '&
      &one two three four five &
      &six seven eight &
      &nine ten eleven twelve &
      &thirteen fourteen fifteen sixteen &
      &seventeen'

     write(*,*)'LEN=',len(string)
     write(*,*)'INPUT:'
     write(*,*)string

     paragrph=paragraph(string,40)
     write(*,*)'LEN=',len(paragrph),' SIZE=',size(paragrph)
     write(*,*)'OUTPUT:'
     write(*,'(a)')paragrph

     write(*,'(a)')paragraph(string,0)
     write(*,'(3x,a)')paragraph(string,47)

     end program demo_paragraph
