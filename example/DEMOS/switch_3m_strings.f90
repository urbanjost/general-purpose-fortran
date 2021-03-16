          program demo_switch
          use M_strings, only : switch, isalpha, islower, nospace
          character(len=*),parameter :: &
          & dashes='-----------------------------------'
          character(len=*),parameter :: string='This is a string'
          character(len=1024)        :: line

          ! First, examples of standard Fortran features
          ! returns array [F,T,T,T,T,T]
          write(*,*)['A','=','=','=','=','='].eq.'='
          ! this would return T
          write(*,*)all(['=','=','=','=','=','='].eq.'=')
          ! this would return F
          write(*,*)all(['A','=','=','=','=','='].eq.'=')

          ! so to test if the string DASHES is all dashes
          ! using SWITCH(3f) is
          if(all(switch(dashes).eq.'-'))then
             write(*,*)'DASHES is all dashes'
          endif

          ! so to test is a string is all letters
          ! isalpha(3f) returns .true. only if character is a letter
          ! false because dashes are not a letter
          write(*,*) all(isalpha(switch(dashes)))
          ! false because of spaces
          write(*,*) all(isalpha(switch(string)))
          ! true because removed whitespace
          write(*,*) all(isalpha(switch(nospace(string))))

          ! to see if a string is all uppercase
          ! show the string
          write(*,*) string
          ! converted to character array
          write(*,'(1x,*("[",a,"]":))') switch(string)
          write(*,'(*(l3))') islower(switch(string))

          ! we need a string that is all letters
          line=nospace(string)
          write(*,*)'LINE=',trim(line)
          ! all true except first character
          write(*,*) islower(switch(nospace(string)))
          ! should be false
          write(*,*) all(islower(switch(nospace(string))))
          ! should be true
          write(*,*) all(islower(switch(nospace(string(2:)))))

          end program demo_switch
