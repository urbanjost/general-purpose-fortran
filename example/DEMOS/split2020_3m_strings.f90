          program demo_sort2020
          use M_strings, only : split2020
          implicit none
          character(len=*),parameter :: gen='(*("[",g0,"]":,","))'

           ! Execution of TOKEN form
           block
             character (len=:), allocatable :: string
             character (len=:), allocatable :: tokens(:)
             character (len=*),parameter :: set = " ,"
             string = 'first,second,third'
             call split2020(string, set, tokens )
             write(*,gen)tokens

           ! assigns the value ['first ','second','third ' ]
           ! to TOKENS.
           endblock

           ! Execution of BOUNDS form

           block
             character (len=:), allocatable :: string
             character (len=*),parameter :: set = " ,"
             integer, allocatable        :: first(:), last(:)
             string =    'first,second,,forth'
             call split2020 (string, set, first, last)
             write(*,gen)first
             write(*,gen)last

           ! will assign the value [ 1, 7, 14, 15 ] to FIRST,
           ! and the value [ 5, 12, 13, 19 ] to LAST.
           endblock

           ! Execution of STEP form
           block
             character (len=:), allocatable :: string
             character (len=*),parameter :: set = " ,"
             integer :: p, istart, iend
             string = " one,   last  example  "
             do while (p < len(string))
               istart = p + 1
               call split2020 (string, set, p)
               iend=p-1
               if(iend.gt.istart)then
                  print '(t3,a,1x,i0,1x,i0)', string (istart:iend),istart,iend
               endif
             enddo
           endblock
       end program demo_sort2020
