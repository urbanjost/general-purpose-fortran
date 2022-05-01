     program demo_psort
     ! partially sort an array
     use M_orderpack, only : psort
     implicit none
     character(len=*),parameter :: g='(*(g0,1x))'
     integer :: nord

     int: block
        integer,allocatable :: ia(:)
        ia=[10,5,7,1,4,5,6,8,9,10,1]
        nord=5
        write(*,g)'Original.................:',ia
        call psort(ia,nord)
        write(*,g)'Number of indices to sort:',nord
        write(*,g)nord,'Lowest values..........:',ia(:nord)
        write(*,g)'Entire array.............:',ia
        write(*,g)
     endblock int
     char: block
        character(len=:),allocatable :: ca(:)
        integer :: i
        ca=[character(len=20) :: 'fan','a','car','be','egg','dam','gas']
        nord=3
        write(*,g)'Original.................:',(trim(ca(i)),i=1,size(ca))
        call psort(ca,nord)
        write(*,g)'Number of indices to sort:',nord
        write(*,g)nord,'Lowest values..........:',(trim(ca(i)),i=1,nord)
        write(*,g)'Entire array.............:',(trim(ca(i)),i=1,size(ca))
        write(*,g)
     endblock char

     end program demo_psort
