          program demo_csvtable
          use M_sqlite
          implicit none
          type(SQLITE_DATABASE)             :: db
          type(SQLITE_STATEMENT)            :: stmt
          type(SQLITE_COLUMN), pointer      :: column(:)
          integer                           :: lun = 10
          integer                           :: ierr
          character(len=40), dimension(4)   :: name
          real                              :: salin
          real                              :: temp
          character(len=40)                 :: station
          character(len=40)                 :: date
          logical                           :: finished
          integer                           :: j
          character(len=40), pointer        :: result(:,:)
          character(len=:),allocatable      :: errmsg
          !--------------------------------------------------------------------
             call sqlite3_open('somedata.db', db)
          !--------------------------------------------------------------------
          ! The first part of the program simply defines the table:
             allocate(column(4))
             call sqlite3_column_props(column(1), name(1), SQLITE_CHAR, 10)
             call sqlite3_column_props(column(2), name(2), SQLITE_CHAR, 10)
             call sqlite3_column_props(column(3), name(3), SQLITE_REAL)
             call sqlite3_column_props(column(4), name(4), SQLITE_REAL)
             call sqlite3_create_table(db, 'measurements', column)
          !--------------------------------------------------------------------
          ! The second part reads a data file and stores the data in a table:
             call csvdata() ! create CSV file for demonstration
             ! Open a CSV file to feed data into the database
             open(lun, file = 'somedata.csv')
             !    To keep it simple:
             !    - The first line contains the names of the four columns
             read(lun, *) name
             !    - All lines after this contain the name of the station,
             !      the date and the two values.
          !--------------------------------------------------------------------
          ! Insert the values into the table.
          !
          ! Note that a transaction (via calls to sqlite3_begin and
          ! sqlite3_commit pair) is used, so that all the inserts can be
          ! done in one go. Inserting with autocommit is much slower,
          ! as the database file needs to be flushed every time.
          !
             call sqlite3_begin(db)
             do
                read(lun,*,iostat=ierr) station, date, salin, temp
                if(ierr .ne. 0) exit
                call sqlite3_set_column(column(1), station)
                call sqlite3_set_column(column(2), date   )
                call sqlite3_set_column(column(3), salin  )
                call sqlite3_set_column(column(4), temp   )
                call sqlite3_insert(db, 'measurements', column)
             enddo
             call sqlite3_commit(db)
             close(lun)
          !--------------------------------------------------------------------
          ! To check that it works, retrieve the average salinity and average
          ! temperature per station and print them sorted by station name
          !
          ! Retrieve the data by constructing an SQL query that will
          ! actually look like:
          !
          !  SELECT station, AVG(salinity), AVG(temperature) FROM measurements
          !         GROUP BY station ORDER BY station;
          !
          ! The routine sqlite3_prepare_select takes care of the actual
          ! construction of the above SQL query:
             !
             ! We want a simple report, the mean of salinity and temperature
             ! sorted by the station
             !
             deallocate(column)

             allocate(column(3))
             call sqlite3_column_query &
                & (column(1), 'station', SQLITE_CHAR)
             call sqlite3_column_query &
                & (column(2), name(3), SQLITE_REAL, function='avg')
             call sqlite3_column_query &
                & (column(3), name(4), SQLITE_REAL, function='avg')

             call sqlite3_prepare_select(db, 'measurements', column, stmt, &
                & 'group by station order by station')
          !--------------------------------------------------------------------
             write(*, '(3a20)')'Station', 'Mean salinity', 'Mean temperature'
             do
                call sqlite3_next_row(stmt, column, finished)
                if(finished) exit
                call sqlite3_get_column(column(1), station)
                call sqlite3_get_column(column(2), salin  )
                call sqlite3_get_column(column(3), temp   )
                write(*, '(a20,2f20.3)') station, salin, temp
             enddo
             !
             ! Get the entire table
             !
             call sqlite3_get_table(db, "select * from measurements", result, errmsg)

             if(associated(result))then
                write(*,*) 'Number of columns: ', size(result,1)
                write(*,*) 'Number of rows:    ', size(result,2)
                do j = 1,size(result,2)
                   write(*,'(10a20)') result(:,j)
                enddo
                deallocate(result)
             else
                write(*,*) 'Error: result table not allocated'
                write(*,*) 'Error: ', trim(errmsg)
             endif

             call sqlite3_close(db)
          !--------------------------------------------------------------------
          contains
          !--------------------------------------------------------------------
          subroutine csvdata()
          ! create fake datafile
          implicit none
          integer                         :: lun = 10
          real, dimension(5)              :: r
          character(len=100)              :: line
          character(len=40)               :: string
          character(len=20),dimension(6)  :: station = &
          & [character(len=20) :: 'NW1','NW2','OS30_LONGER_NAME','DH','DO','Ah111' ]
          integer                         :: i
          open(lun, file='somedata.csv')
          write(lun, '(a)') 'station,date,salinity,temperature'
          do i = 1,100
             call random_number(r)
             line = station(1+int(5.0*r(1)))
             write(string, '(i0,a,i0,a,i0)') &
             & 2005, '-', 1+int(12.0*r(2)), '-', 1+int(28.0*r(3))
             line = trim(line) // ',' // string
             write(string, '(f10.2)') 28.0+6.0*r(4)
             line = trim(line) // ',' // adjustl(string)
             write(string, '(f10.2)') 15.0+5.0*r(5)
             line = trim(line) // ',' // adjustl(string)
             write(lun, '(a)') trim(line)
          enddo
          close(lun)
          end subroutine csvdata
          !--------------------------------------------------------------------
          end program demo_csvtable
