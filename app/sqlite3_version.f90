program demo_sqlite3_libversion
!(LICENSE:PD)

use M_sqlite, only : sqlite3_libversion
use M_sqlite, only : sqlite3_libversion_number
use M_sqlite, only : sqlite3_sourceid
implicit none
character(len=*),parameter   :: ident='@(#)sqlite3_version(3f): display version of libsqlite3 library'
character(len=:),allocatable :: message
integer                      :: ivalue

   message=sqlite3_libversion()
   write(*,*)'SQLITE3 LIBRARY VERSION=',message

   message = sqlite3_sourceid()
   write(*,*)'SQLITE3 SOURCEID=',message

   ivalue  = sqlite3_libversion_number()
   write(*,*)'SQLITE3 VERSION NUMBER=',ivalue

end program demo_sqlite3_libversion
