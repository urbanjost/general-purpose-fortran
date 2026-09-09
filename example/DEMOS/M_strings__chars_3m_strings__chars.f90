         program demo_M_strings__chars
         use M_strings__chars,   only : toupper, tolower
         implicit none
         integer,parameter  :: bytes=80
         character          :: string*(bytes)
         character          :: lets(bytes)
         equivalence (string,lets)
            string='Do unto Others'
            write(*,*)toupper(lets)
            write(*,*)tolower(lets)
         end program demo_M_strings__chars
