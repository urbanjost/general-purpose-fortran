           program demo_lenset
            use M_strings, only : lenset
            implicit none
            character(len=10)            :: string='abcdefghij'
            character(len=:),allocatable :: answer
               answer=lenset(string,5)
               write(*,'("[",a,"]")') answer
               answer=lenset(string,20)
               write(*,'("[",a,"]")') answer
           end program demo_lenset
