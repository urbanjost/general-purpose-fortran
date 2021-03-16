          program demo_dilate

          !  test filter to remove tabs and trailing white space from input
          !  on files up to 1024 characters wide
          use M_strings, only : dilate
          implicit none
          character(len=:),allocatable :: in
          character(len=:),allocatable :: out
          integer                      :: i
             in='  this is my string  '
             ! change spaces to tabs to make a sample input
             do i=1,len(in)
                if(in(i:i).eq.' ')in(i:i)=char(9)
             enddo
             write(*,'(a)')in,dilate(in)
          end program demo_dilate
