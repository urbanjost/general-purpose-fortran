     program demo_couple
     use M_strings, only : couple
     character(len=:),allocatable :: array(:)
      array=['T','h','i','s',' ','i','s',' ','a',' ','s','t','r','i','n','g']

      ! show the array
      write(*,'(1x,*("[",a,"]":))') array
      ! show the string
      write(*,'(1x,*("[",a,"]":))') couple(array)

     end program demo_couple
