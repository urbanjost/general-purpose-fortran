        program demo_s2c
        use M_strings, only : s2c
        implicit none
        character(len=*),parameter   :: string="single string"
        character(len=*),parameter   :: g= '(1x,*("[",g3.3,"]":))'
        character(len=3),allocatable :: array(:)
           write(*,*)'INPUT STRING ',trim(string)
           ! put one character into each 3-character element of array
           array=s2c(string)
           ! write array with ASCII Decimal Equivalent below it except show
           ! unprintable characters like NULL as "XXX"
           write(*,g) merge('XXX',array,iachar(array(:)(1:1)) < 32)
           write(*,g) iachar(array(:)(1:1))
        end program demo_s2c
