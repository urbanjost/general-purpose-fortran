     program demo_uncouple
     use M_strings, only : uncouple
     character(len=*),parameter   :: string='This is a string'

      write(*,'(1x,*("[",a,"]":))') string
      ! converted to character array
      write(*,'(1x,*("[",a,"]":))') uncouple(string)

     end program demo_uncouple
