         program demo_find_field
         use M_strings, only : find_field
         implicit none
         character(len=256)           :: string
         character(len=256)           :: field
         integer                      :: position
         character(len=:),allocatable :: delims
         character(len=1)             :: delim
         logical                      :: found

         delims='[,]'
         position=1
         found=.true.
         string='[a,b,[ccc,ddd],and more]'
         write(*,'(a)')trim(string)
         do
            call find_field(string,field,position,delims,delim,found=found)
            if(.not.found)exit
            write(*,'("<",a,">")')trim(field)
         enddo
         write(*,'(*(g0))')repeat('=',70)

         position=1
         found=.true.
         write(*,'(a)')trim(string)
         do
            call find_field(string,field,position,'[], ',delim,found=found)
            if(.not.found)exit
            write(*,'("<",a,">",i0,1x,a)')trim(field),position,delim
         enddo
         write(*,'(*(g0))')repeat('=',70)

         end program demo_find_field
