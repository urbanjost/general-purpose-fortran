           program demo_s2c
           use M_strings, only : s2c
           implicit none
           character(len=*),parameter   :: string="single string"
           character(len=3),allocatable :: array(:)
              write(*,*)'INPUT STRING ',trim(string)
              ! put one character into each 3-character element of array
              array=s2c(string)
              ! write array with ASCII Decimal Equivalent below it except show
              ! unprintable characters like NULL as "XXX"
              write(*,'(1x,*("[",a3,"]":))')&
                   & merge('XXX',array,ichar(array(:)(1:1)).lt.32)
              write(*,'(1x,*("[",i3,"]":))')&
                   & ichar(array(:)(1:1))
           end program demo_s2c
