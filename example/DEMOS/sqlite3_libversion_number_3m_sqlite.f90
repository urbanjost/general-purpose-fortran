          program demo_sqlite3_libversion
          use M_sqlite, only : sqlite3_libversion
          use M_sqlite, only : sqlite3_libversion_number
          use M_sqlite, only : sqlite3_sourceid
          character(len=:),allocatable :: message

          message=sqlite3_libversion()
          write(*,*)'SQLITE3 LIBRARY VERSION=',message

          message = sqlite3_sourceid()
          write(*,*)'SQLITE3 SOURCEID=',message

          ivalue  = sqlite3_libversion_number()
          write(*,*)'SQLITE3 VERSION NUMBER=',ivalue

          end program demo_sqlite3_libversion
