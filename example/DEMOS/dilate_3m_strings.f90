     program demo_dilate

     use M_strings, only : dilate, visible
     implicit none
     character(len=:),allocatable :: in
     integer                      :: i
        in='  this is my string  '
        ! change spaces to tabs to make a sample input
        do i=1,len(in)
           if(in(i:i) == ' ')in(i:i)=char(9)
        enddo
        write(*,'("[",a,"]")')visible(in)
        write(*,'("[",a,"]")')visible(dilate(in))
     end program demo_dilate
