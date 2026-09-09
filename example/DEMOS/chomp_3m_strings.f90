     program demo_chomp

     use M_strings, only : chomp
     implicit none
     character(len=100)            :: inline
     character(len=:),allocatable  :: token
     character(len=*),parameter    :: delimiters=' ;,'
     integer                       :: iostat
     integer                       :: icount
     integer                       :: itoken
        icount=0
        do        ! read lines from stdin until end-of-file or error
           read (unit=*,fmt="(a)",iostat=iostat) inline
           if(iostat /= 0)stop
           icount=icount+1
           itoken=0
           write(*,*)'INLINE ',trim(inline)
           do while ( chomp(inline,token,delimiters) >=  0)
              itoken=itoken+1
              print *, itoken,'TOKEN=['//trim(token)//']'
           enddo
        enddo

     end program demo_chomp
