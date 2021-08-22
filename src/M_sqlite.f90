











!>
!!##TODO
!!
!!     - Arbitrary length character strings in set_column and get_column
!!     - Think about finalize and reset: error code?
!!     - Support BLOBs
!!     - Support DATE
!!     - Support NULL
!!     - More elegant selection of functions of columns
!!
!!     - To avoid memory leaks, the application should invoke [sqlite3_free()]
!!
!!##IMPLEMENTATION NOTES
!!
!!     The handles to the database or prepared statements are stored in an array of two integers to take care of 64-bit platforms.
!!
!!     With the appropriate compilation options (if needed) the code should be thread-safe except for
!!     sqlite3_get_table(1), as no data are shared or saved in other routines.
!>
!!##NAME
!!     M_sqlite(3fm) - [M_sqlite] Fortran interface to SQLite3 API
!!
!!##SYNOPSIS
!!
!!
!!  Types
!!
!!    type(SQLITE_DATABASE)
!!    type(SQLITE_STATEMENT)
!!    type(SQLITE_COLUMN)
!!
!!  Routines
!!
!!   Extensions
!!    call sqlite3_create_table(db)
!!    call sqlite3_delete_table(db)
!!    call sqlite3_column_query(column,name,type,length,function)
!!    call sqlite3_prepare_select(db,tablename,columns,stmt,extra_clause)
!!    call sqlite3_get_column(column,value)
!!
!!   SQLite API
!!    call sqlite3_begin(db)
!!    call sqlite3_close(db)
!!    call sqlite3_column_props(column,name,type,length)
!!    call sqlite3_commit(db)
!!    call sqlite3_do(db,command)
!!    call sqlite3_finalize(stmt)
!!    call sqlite3_get_table(db,command,result,errmsg)
!!    call sqlite3_insert(db,tablename,columns)
!!    call sqlite3_next_row(stmt,columns,finished)
!!    call sqlite3_open(filename,db)
!!    call sqlite3_prepare(db,command,stmt,columns)
!!    call sqlite3_query_table(db,tablename,columns)
!!    call sqlite3_reset(stmt)
!!    call sqlite3_rollback(db)
!!    call sqlite3_set_column(column,value)
!!    call sqlite3_step(stmt,completion)
!!
!!    err = sqlite3_error(db)
!!    errmsg = sqlite3_errmsg(db)
!!
!!   Direct SQLite API (new)
!!    message = sqlite3_libversion()
!!    message = sqlite3_sourceid()
!!    ivalue  = sqlite3_libversion_number()
!!
!!    iresult = sqlite3_threadsafe()
!!    iresult = sqlite3_initialize()
!!    iresult = sqlite3_shutdown()
!!    iresult = sqlite3_os_init()
!!    iresult = sqlite3_os_end()
!!
!!##DESCRIPTION
!!
!!    THIS IS AN EXPERIMENTAL ATTEMPT TO BEGIN A FORTRAN SQLITE3 INTERFACE.
!!
!!    The M_sqlite(3f) module provides a high-level means for
!!    Fortran programmers to use the SQLite3 library by Richard Hipp
!!    (http://www.sqlite.org)
!!
!!    The interface has been implemented in such a way that you can use a
!!    few high-level routines for common tasks, such as inserting data into
!!    a database and querying the contents. To this end the module defines
!!    a set of routines and functions as well as several derived types to
!!    hide the low-level details.
!!
!!    The remaining majority of the routines merely prepare SQL statements
!!    or are a simple interface to the original C functions. Therefore, when
!!    in doubt consult the SQLite documentation at http://www.sqlite.org.
!!
!!    In its current form, M_sqlite(3f) does not provide a full Fortran
!!    API to all the functionality offered by SQLite, but it should be
!!    quite useable.
!!
!!##DATA TYPES
!!
!!    The following derived types are defined:
!!
!!    type(SQLITE_DATABASE)   Variables of this type are used to hold the connection to the database or
!!                            databases. They are created by the subroutine sqlite3_open(3f)
!!
!!                            The contents are valid until the database is closed (via sqlite3_close(3f)).
!!
!!    type(SQLITE_STATEMENT)  Variables of this type hold prepared statements, the common method for
!!                            database management systems to efficiently execute SQL statements.
!!
!!    type(SQLITE_COLUMN)     To provide easy communication with the database, SQLITE_COLUMN can hold
!!                            values of different types. This means you can use a single routine and
!!                            variable to transfer strings, integers or reals to and from the database.
!!
!!    The first two derived types are "opaque", that is they are used only to
!!    communicate between the application and the database library and there is
!!    information of interest to be gotten from them.
!!
!!    The third type is rather crucial to the working of the implementation: By
!!    setting the properties of an SQLITE_COLUMN variable you put data into the
!!    database or you can retrieve data from the database. See the example below for
!!    how this works.
!!
!!    There are a number of convenience routines that are meant to make this easier,
!!    which are marked below (with **). The remaining routines are calls to the C
!!    API for SQLite3.
!!
!!##ROUTINES
!!
!!   The M_sqlite(3fm) module currently provides the following functions:
!!
!! General
!!
!!    call sqlite3_open(filename, db)
!!                                    -- Open a database file and store the
!!                                       connection for later use.
!!    call sqlite3_close(db)
!!                                    -- Close the database connection. Simply an
!!                                       interface to the corresponding C function.
!!    err = sqlite3_error(db)
!!                                    -- Retrieve whether the previous command
!!                                       resulted in an error or not. Returns
!!                                       true if so, otherwise false.
!! Transactions
!!
!!    call sqlite3_begin(db)
!!                                    -- Start a transaction.
!!    call sqlite3_commit(db)
!!                                    -- Commit the changes made since the start
!!                                       of a transaction.
!!    call sqlite3_rollback(db)
!!                                    -- Undo changes made since the start of a
!!                                       transaction.
!! Create tables
!!
!!    **call sqlite3_column_props(column, name, type, length)
!!                                    -- Set the properties of a column
!!    **call sqlite3_set_column(column, value)
!!                                    -- Set the value of a column
!!    call sqlite3_insert(db, tablename, columns)
!!                                    -- Insert a complete new row into the table.
!!    **call sqlite3_create_table(db)
!!                                    -- Create a new table, based on the
!!                                       properties of the columns.
!!    **call sqlite3_delete_table(db)
!!                                    -- Delete an existing table by name.
!!
!! Query
!!
!!    **call sqlite3_column_query(column, name, type, length, function)
!!                                    -- Set properties of a column when
!!                                       constructing a SELECT query. The
!!                                       optional "function" is a string
!!                                       representing an SQL function
!!                                       like count or max.
!!    **call sqlite3_prepare_select(db,tablename,columns,stmt,extra_clause)
!!                                    -- Prepare a SELECT query.
!!    call sqlite3_next_row(stmt, columns, finished)
!!                                    -- Retrieve the next row of a SELECT query.
!!    **call sqlite3_get_column(column, value)
!!                                    -- Get the value of a column
!!    call sqlite3_get_table(db, command, result, errmsg)
!!                                    -- Get the result of a query in a
!!                                       single two-dimensional array
!!    call sqlite3_query_table(db, tablename, columns)
!!                                    -- Query the structure of the table
!!
!! General SQL command
!!
!!    call sqlite3_do(db, command)
!!                                    -- Run a single SQL command
!!    call sqlite3_prepare(db, command, stmt, columns)
!!                                    -- Prepare a general SQL statement
!!                                       for later actual execution.
!!    call sqlite3_step(stmt, completion)
!!                                    -- Run the prepared SQL statement
!!    call sqlite3_reset(stmt)
!!                                    -- Reset the prepared statement i
!!                                       so that it can be used again.
!!    call sqlite3_finalize(stmt)
!!                                    -- Free all resources associated with the
!!                                       prepared statement.
!!
!! Description
!!
!!    message=sqlite3_version()
!!                                    -- Get version number string
!!    message=sqlite3_sourceid()
!!                                    -- Get build identification string
!!    ival=sqlite3_libversion_number()
!!                                    -- Get library version number
!!##EXAMPLE
!!
!!
!!  To illustrate the usage of the library, here is a small example that is
!!  essentially the FLIBS example:
!!
!!    !--------------------------------------------------------------------
!!    ! csvtable.f90 --
!!    !    Program to read a simple CSV file and put it into a
!!    !    SQLite database, just to demonstrate how the Fortran
!!    !    interface works.
!!    program demo_csvtable
!!    use M_sqlite
!!    implicit none
!!    type(SQLITE_DATABASE)             :: db
!!    type(SQLITE_STATEMENT)            :: stmt
!!    type(SQLITE_COLUMN), pointer      :: column(:)
!!    integer                           :: lun = 10
!!    integer                           :: ierr
!!    character(len=40), dimension(4)   :: name
!!    real                              :: salin
!!    real                              :: temp
!!    character(len=40)                 :: station
!!    character(len=40)                 :: date
!!    logical                           :: finished
!!    integer                           :: j
!!    character(len=40), pointer        :: result(:,:)
!!    character(len=:),allocatable      :: errmsg
!!    !--------------------------------------------------------------------
!!       call sqlite3_open('somedata.db', db)
!!    !--------------------------------------------------------------------
!!    ! The first part of the program simply defines the table:
!!       allocate(column(4))
!!       call sqlite3_column_props(column(1), name(1), SQLITE_CHAR, 10)
!!       call sqlite3_column_props(column(2), name(2), SQLITE_CHAR, 10)
!!       call sqlite3_column_props(column(3), name(3), SQLITE_REAL)
!!       call sqlite3_column_props(column(4), name(4), SQLITE_REAL)
!!       call sqlite3_create_table(db, 'measurements', column)
!!    !--------------------------------------------------------------------
!!    ! The second part reads a data file and stores the data in a table:
!!       call csvdata() ! create CSV file for demonstration
!!       ! Open a CSV file to feed data into the database
!!       open(lun, file = 'somedata.csv')
!!       !    To keep it simple:
!!       !    - The first line contains the names of the four columns
!!       read(lun, *) name
!!       !    - All lines after this contain the name of the station,
!!       !      the date and the two values.
!!    !--------------------------------------------------------------------
!!    ! Insert the values into the table.
!!    !
!!    ! Note that a transaction (via calls to sqlite3_begin and
!!    ! sqlite3_commit pair) is used, so that all the inserts can be
!!    ! done in one go. Inserting with autocommit is much slower,
!!    ! as the database file needs to be flushed every time.
!!    !
!!       call sqlite3_begin(db)
!!       do
!!          read(lun,*,iostat=ierr) station, date, salin, temp
!!          if(ierr .ne. 0) exit
!!          call sqlite3_set_column(column(1), station)
!!          call sqlite3_set_column(column(2), date   )
!!          call sqlite3_set_column(column(3), salin  )
!!          call sqlite3_set_column(column(4), temp   )
!!          call sqlite3_insert(db, 'measurements', column)
!!       enddo
!!       call sqlite3_commit(db)
!!       close(lun)
!!    !--------------------------------------------------------------------
!!    ! To check that it works, retrieve the average salinity and average
!!    ! temperature per station and print them sorted by station name
!!    !
!!    ! Retrieve the data by constructing an SQL query that will
!!    ! actually look like:
!!    !
!!    !  SELECT station, AVG(salinity), AVG(temperature) FROM measurements
!!    !         GROUP BY station ORDER BY station;
!!    !
!!    ! The routine sqlite3_prepare_select takes care of the actual
!!    ! construction of the above SQL query:
!!       !
!!       ! We want a simple report, the mean of salinity and temperature
!!       ! sorted by the station
!!       !
!!       deallocate(column)
!!
!!       allocate(column(3))
!!       call sqlite3_column_query &
!!          & (column(1), 'station', SQLITE_CHAR)
!!       call sqlite3_column_query &
!!          & (column(2), name(3), SQLITE_REAL, function='avg')
!!       call sqlite3_column_query &
!!          & (column(3), name(4), SQLITE_REAL, function='avg')
!!
!!       call sqlite3_prepare_select(db, 'measurements', column, stmt, &
!!          & 'group by station order by station')
!!    !--------------------------------------------------------------------
!!       write(*, '(3a20)')'Station', 'Mean salinity', 'Mean temperature'
!!       do
!!          call sqlite3_next_row(stmt, column, finished)
!!          if(finished) exit
!!          call sqlite3_get_column(column(1), station)
!!          call sqlite3_get_column(column(2), salin  )
!!          call sqlite3_get_column(column(3), temp   )
!!          write(*, '(a20,2f20.3)') station, salin, temp
!!       enddo
!!       !
!!       ! Get the entire table
!!       !
!!       call sqlite3_get_table(db, "select * from measurements", result, errmsg)
!!
!!       if(associated(result))then
!!          write(*,*) 'Number of columns: ', size(result,1)
!!          write(*,*) 'Number of rows:    ', size(result,2)
!!          do j = 1,size(result,2)
!!             write(*,'(10a20)') result(:,j)
!!          enddo
!!          deallocate(result)
!!       else
!!          write(*,*) 'Error: result table not allocated'
!!          write(*,*) 'Error: ', trim(errmsg)
!!       endif
!!
!!       call sqlite3_close(db)
!!    !--------------------------------------------------------------------
!!    contains
!!    !--------------------------------------------------------------------
!!    subroutine csvdata()
!!    ! create fake datafile
!!    implicit none
!!    integer                         :: lun = 10
!!    real, dimension(5)              :: r
!!    character(len=100)              :: line
!!    character(len=40)               :: string
!!    character(len=20),dimension(6)  :: station = &
!!    & [character(len=20) :: 'NW1','NW2','OS30_LONGER_NAME','DH','DO','Ah111' ]
!!    integer                         :: i
!!    open(lun, file='somedata.csv')
!!    write(lun, '(a)') 'station,date,salinity,temperature'
!!    do i = 1,100
!!       call random_number(r)
!!       line = station(1+int(5.0*r(1)))
!!       write(string, '(i0,a,i0,a,i0)') &
!!       & 2005, '-', 1+int(12.0*r(2)), '-', 1+int(28.0*r(3))
!!       line = trim(line) // ',' // string
!!       write(string, '(f10.2)') 28.0+6.0*r(4)
!!       line = trim(line) // ',' // adjustl(string)
!!       write(string, '(f10.2)') 15.0+5.0*r(5)
!!       line = trim(line) // ',' // adjustl(string)
!!       write(lun, '(a)') trim(line)
!!    enddo
!!    close(lun)
!!    end subroutine csvdata
!!    !--------------------------------------------------------------------
!!    end program demo_csvtable
!!    !--------------------------------------------------------------------
!!
!!##LIMITATIONS
!!
!! The module is not complete yet:
!!
!!   * There is no support for blobs
!!
!!   * There is no support for NULL values or for DATE values.
!!
!!   * The SQLite API is not completely covered, though the subset should be
!!     useful for many applications.
!!
!!##IMPLEMENTATION NOTES
!!
!!   * The library has been designed with 64-bits platforms in mind: it should run
!!     on these platforms without any difficulties.
!!
!!##AUTHOR
!!
!!     This version (20180311), by John Urban, uses the ISO_C_BINDING
!!     interface to provide portability between PEs (Programming
!!     Environments). The extensions are modeled on modules by Arjen Markus
!!     <arjenmarkus@sourceforge.net> and the associated C code is inspired
!!     by work by Al Danial (http://danial.org).
module M_sqlite
use,intrinsic     :: iso_c_binding, only: c_float, c_int, c_char
use,intrinsic     :: iso_c_binding, only: c_ptr, c_f_pointer, c_null_char, c_null_ptr
use,intrinsic     :: iso_c_binding
implicit none
private

logical :: M_sqlite3_debug=.false.

integer,public,parameter         :: dp = kind(1.0d00)

integer,public,parameter         :: SQLITE_INT    = 1
integer,public,parameter         :: SQLITE_REAL   = 2
integer,public,parameter         :: SQLITE_DOUBLE = 3
integer,public,parameter         :: SQLITE_CHAR   = 4

integer,public,parameter         :: SQLITE_OK     = 0
integer,public,parameter         :: SQLITE_ERROR  = 1
integer,public,parameter         :: SQLITE_MISUSE = 21
integer,public,parameter         :: SQLITE_ROW    = 100
integer,public,parameter         :: SQLITE_DONE   = 101

type SQLITE_STATEMENT
   integer(kind=c_int)                 :: stmt_handle(2)
end type SQLITE_STATEMENT

type SQLITE_DATABASE
   integer(kind=c_int)                      :: db_handle(2)
   integer(kind=c_int)                      :: error
   character(len=:,kind=c_char),allocatable :: errmsg
end type SQLITE_DATABASE

type SQLITE_COLUMN
   character(len=40,kind=c_char)            :: name     = ' '
   character(len=40,kind=c_char)            :: type     = ' '
   character(len=40,kind=c_char)            :: function = ' '
   integer(kind=c_int)                      :: type_set
   integer(kind=c_int)                      :: int_value
   real(kind=dp)                            :: double_value
   character(len=:,kind=c_char),allocatable :: char_value
end type SQLITE_COLUMN

public SQLITE_STATEMENT, SQLITE_DATABASE, SQLITE_COLUMN
public test_suite_M_sqlite

private :: typename
private :: column_func

   private :: Cp2Fs_v1
   private :: Cp2Fs_v2

   private :: cp2fp_char

   private :: Ca2Fs_v1
   private :: Ca2Fs_v2

   private :: Fs2Ca
!
! Convenient interfaces
!
interface sqlite3_set_column
   module procedure sqlite3_set_column_int
   module procedure sqlite3_set_column_real
   module procedure sqlite3_set_column_double
   module procedure sqlite3_set_column_char
end interface

interface sqlite3_get_column
   module procedure sqlite3_get_column_int
   module procedure sqlite3_get_column_real
   module procedure sqlite3_get_column_double
   module procedure sqlite3_get_column_char
end interface


interface
   ! std C library function
   function strlen(s) bind(C, name='strlen')
      import c_ptr, c_size_t
      implicit none
      type(c_ptr), intent(in), value :: s
      integer(c_size_t)              :: strlen
   end function strlen
end interface

interface
   ! std C library function
   function free(s) bind(C, name='free')
      use, intrinsic :: iso_c_binding, only: c_ptr, c_int
      implicit none
      integer(kind=c_int) :: free
      type(c_ptr)         :: s
   end function free
end interface

public sqlite3_begin
public sqlite3_close
public sqlite3_column_props
public sqlite3_column_query
public sqlite3_commit
public sqlite3_create_table
public sqlite3_delete_table
public sqlite3_do
public sqlite3_errmsg
public sqlite3_error
public sqlite3_finalize
public sqlite3_get_column
public sqlite3_get_table
public sqlite3_insert
public sqlite3_next_row
public sqlite3_open
public sqlite3_prepare
public sqlite3_prepare_select
public sqlite3_query_table
public sqlite3_reset
public sqlite3_rollback
public sqlite3_set_column
public sqlite3_step

public sqlite3_sourceid
public sqlite3_libversion
public sqlite3_libversion_number
!===================================================================================================================================
!>
!!##NAME
!!    sqlite3_libversion_number(3f) - [M_sqlite] obtain library version number
!!
!!##SYNOPSIS
!!
!!    iversion=sqlite3_libversion_number()
!!
!!      character(len=:),allocatable :: version
!!      sqlite3_libversion_number   :: iversion
!!
!!##DESCRIPTION
!!      Get library version.
!!
!!##EXAMPLE
!!
!!  Sample example code
!!
!!    program demo_sqlite3_libversion
!!    use M_sqlite, only : sqlite3_libversion
!!    use M_sqlite, only : sqlite3_libversion_number
!!    use M_sqlite, only : sqlite3_sourceid
!!    character(len=:),allocatable :: message
!!
!!    message=sqlite3_libversion()
!!    write(*,*)'SQLITE3 LIBRARY VERSION=',message
!!
!!    message = sqlite3_sourceid()
!!    write(*,*)'SQLITE3 SOURCEID=',message
!!
!!    ivalue  = sqlite3_libversion_number()
!!    write(*,*)'SQLITE3 VERSION NUMBER=',ivalue
!!
!!    end program demo_sqlite3_libversion
!!
!!  Typical Results:
!!
!!    SQLITE3 LIBRARY VERSION=3.21.0
!!    SQLITE3 SOURCEID=2017-10-24 18:55:49 1a584e499906b5c87ec7d43d4abce641fdf017c42125b083109bc77c4de48827
!!    SQLITE3 VERSION NUMBER=     3021000
interface
   !! SQLITE_API int sqlite3_libversion_number(void);
   function sqlite3_libversion_number() result (iversion) bind(C, name='sqlite3_libversion_number')
      import c_int
      implicit none
      integer(kind=c_int) :: iversion
   end function sqlite3_libversion_number
end interface
!===================================================================================================================================
interface
   !! SQLITE_API int sqlite3_threadsafe(void);
   function sqlite3_threadsafe() result (ires) bind(C, name="sqlite3_threadsafe")
      import c_int
      implicit none
      integer(kind=c_int) :: ires
   end function sqlite3_threadsafe
   !! SQLITE_API int sqlite3_initialize(void);
   function sqlite3_initialize() result (ires) bind(C, name="sqlite3_initialize")
      import c_int
      implicit none
      integer(kind=c_int) :: ires
   end function sqlite3_initialize
   !! SQLITE_API int sqlite3_shutdown(void);
   function sqlite3_shutdown() result (ires) bind(C, name="sqlite3_shutdown")
      import c_int
      implicit none
      integer(kind=c_int) :: ires
   end function sqlite3_shutdown
   !! SQLITE_API int sqlite3_os_init(void);
   function sqlite3_os_init() result (ires) bind(C, name="sqlite3_os_init")
      import c_int
      implicit none
      integer(kind=c_int) :: ires
   end function sqlite3_os_init
   !! SQLITE_API int sqlite3_os_end(void);
   function sqlite3_os_end() result (ires) bind(C, name="sqlite3_os_end")
      import c_int
      implicit none
      integer(kind=c_int) :: ires
   end function sqlite3_os_end
end interface

contains
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
! typename --
!    Construct the type and attributes of a column in a new table
! Arguments:
!    column        Column information
!    primary       Name of the primary key
!
character(len=40) function typename(column, primary)
   type(SQLITE_COLUMN), intent(in) :: column
   character(len=*), intent(in)    :: primary

   call EGRESS('typename','start')
   if(column%name .ne. primary)then
      typename = column%type
   else
      !write(typename, '(2a)') trim(column%type), ' primary key'
      typename = trim(column%type) // ' primary key'
   endif
   call EGRESS('typename','finish')

end function typename
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
! column_func --
!    Construct the name and function of a column in a new table
! Arguments:
!    column        Column information
!
function column_func(column)
   type(SQLITE_COLUMN), intent(in) :: column
   character(len=:),allocatable    :: column_func

   call EGRESS('column_func','start')
   if(column%function .ne. ' ')then
      column_func = trim(column%function) // '(' // trim(column%name) // ')'
   else
      column_func = column%name
   endif
   call EGRESS('column_func','finish')

end function column_func
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!   sqlite3_column_props(3f) - [M_sqlite] Convenience routine to set the properties of a column
!!
!!##SYNOPSIS
!!
!!    subroutine sqlite3_column_props(column, name, type, length)
!!
!!        type(SQLITE_COLUMN) :: column
!!        character(len=*)    :: name
!!        integer             :: type
!!        integer, optional   :: length
!!
!!##DESCRIPTION
!!       Convenience routine to set the properties of a column
!!
!!       Side effects:
!!
!!           Fields in column filled
!!
!!##OPTIONS
!!        column    The structure that holds the information on the column
!!
!!        name      Name of the column in the table to which it belongs or will belong
!!
!!        type      Type of the column: one of SQLITE_INT, SQLITE_REAL,
!!                  SQLITE_DOUBLE or SQLITE_CHAR
!!
!!        length    Length of a character-valued column (defaults to 20 characters)
!!
!!##EXAMPLE
!!
subroutine sqlite3_column_props(column, name, type, length)
implicit none
type(SQLITE_COLUMN), intent(inout) :: column
character(len=*), intent(in)       :: name
integer, intent(in)                :: type
integer, intent(in), optional      :: length

   integer                            :: length_

   call EGRESS('sqlite3_column_props','start')
   length_ = 20
   if(present(length))then
      length_ = length
   endif

   column%name     = name
   column%type_set = type

   select case(type)
   case(SQLITE_INT)
      column%type = 'INT'
   case(SQLITE_REAL)
      column%type = 'FLOAT'
   case(SQLITE_DOUBLE)
      column%type = 'DOUBLE'
   case(SQLITE_CHAR)
      write(column%type, '(a,i0,a)') 'CHAR(', length_, ')'
   case default
      column%type = 'UNKNOWN!'
   end select
   call EGRESS('sqlite3_column_props','finish')

end subroutine sqlite3_column_props
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    sqlite3_column_query - [M_sqlite] Convenience routine to query a column or a function of that column
!!
!!##SYNOPSIS
!!
!!      subroutine sqlite3_column_query(column, name, type, length, function)
!!
!!        type(SQLITE_COLUMN) column
!!        character(len=*) name
!!        integer type
!!        integer, optional length
!!        character(len=*), optional function
!!
!!##DESCRIPTION
!!        Set the properties of a column when constructing a SELECT query. The
!!        "function" argument, if present, is a string representing an SQL function
!!        like count or max.
!!
!!        Side effects:
!!
!!           Fields in column filled
!!
!!##OPTIONS
!!        column    The variable that holds the information on the column
!!
!!        name      Name of the column in the table to which it belongs or will belong
!!
!!        type      Type of the column: one of SQLITE_INT, SQLITE_REAL, SQLITE_DOUBLE or
!!                  SQLITE_CHAR
!!
!!        length    Length of a character-valued column (defaults to 20 characters)
!!
!!        function  Name of the SQL function to perform on the values (if any).
subroutine sqlite3_column_query(column, name, type, length, function)
implicit none
type(SQLITE_COLUMN), intent(inout)     :: column
character(len=*), intent(in)           :: name
integer, intent(in)                    :: type
integer, intent(in), optional          :: length
character(len=*), intent(in), optional :: function

   call EGRESS('sqlite3_column_query','start')
   column%function = ' '
   if(present(function))then
      column%function = function
   endif
   if(present(length))then
      call sqlite3_column_props(column, name, type, length)
   else
      call sqlite3_column_props(column, name, type)
   endif
   call EGRESS('sqlite3_column_query','finish')

end subroutine sqlite3_column_query
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
! sqlite3_set_column_int    --
! sqlite3_set_column_real   --
! sqlite3_set_column_double --
! sqlite3_set_column_char   --

!>
!!##NAME
!!    sqlite3_set_column(3f)  - [M_sqlite] Convenience routines to set the value of a column
!!
!!##SYNOPSIS
!!
!!    subroutine sqlite3_set_column(column, value)
!!
!!        type(SQLITE_COLUMN) column
!!        any type value
!!
!!##DESCRIPTION
!!     Set the value of a column
!!
!!     Side effects:
!!
!!        Appropriate value field in column set
!!
!!##OPTIONS
!!
!!        column  The structure that holds the information on the column
!!
!!        value   The new value for the column. The type of the value that is passed can
!!                be integer, real, double precision real or character string.
!!
!!                Note: currently there is no conversion from the type of value that is
!!                stored to the type of the actual variable that is passed to the
!!                routine. If you ask for an integer and the column holds a real, then
!!                the result is undefined. Check the type with the value of the flag
!!                "type_set". (This is one of the things that should be improved)
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine sqlite3_set_column_int(column, value)
implicit none
type(SQLITE_COLUMN), intent(inout) :: column
integer, intent(in)                :: value

   call EGRESS('sqlite3_set_column_int','start')
   column%int_value = value
   column%type_set  = SQLITE_INT
   call EGRESS('sqlite3_set_column_int','finish')
end subroutine sqlite3_set_column_int
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine sqlite3_set_column_real(column, value)
implicit none
type(SQLITE_COLUMN), intent(inout) :: column
real, intent(in)                   :: value

   call EGRESS('sqlite3_set_column_real','start')
   column%double_value = value
   column%type_set  = SQLITE_DOUBLE
   call EGRESS('sqlite3_set_column_real','finish')
end subroutine sqlite3_set_column_real
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine sqlite3_set_column_double(column, value)
implicit none
type(SQLITE_COLUMN), intent(inout) :: column
real(kind=dp), intent(in)               :: value

   call EGRESS('sqlite3_set_column_double','start')
   column%double_value = value
   column%type_set  = SQLITE_DOUBLE
   call EGRESS('sqlite3_set_column_double','finish')
end subroutine sqlite3_set_column_double
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine sqlite3_set_column_char(column, value)
implicit none
type(SQLITE_COLUMN), intent(inout) :: column
character(len=*), intent(in)       :: value

   call EGRESS('sqlite3_set_column_char','start')
   column%char_value = value
   column%type_set  = SQLITE_CHAR
   call EGRESS('sqlite3_set_column_char','finish')
end subroutine sqlite3_set_column_char
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
! sqlite3_get_column_int    --
! sqlite3_get_column_real   --
! sqlite3_get_column_double --
! sqlite3_get_column_char   --

!>
!!##NAME
!!    sqlite3_get_column(3f) - [M_sqlite] Convenience routine to get the value of a column
!!
!!##SYNOPSIS
!!
!!    subroutine sqlite3_get_column(column, value)
!!
!!     type(SQLITE_COLUMN) :: column
!!     any type            :: value
!!
!!##DESCRIPTION
!!        Get the value of a column
!!
!!        Side effects:
!!
!!         Value argument will be set
!!
!!        Note:
!!
!!         No attempt is made to convert the value
!!         to the requested value. You will have to
!!         check this yourself
!!
!!##OPTIONS
!!        column  The structure that holds the information on the column
!!
!!        value   The value stored in the column. The type of the value that is passed
!!                can be integer, real, double precision real or character string.
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine sqlite3_get_column_int(column, value)
implicit none
type(SQLITE_COLUMN), intent(inout) :: column
integer, intent(out)               :: value

   call EGRESS('sqlite3_get_column_int','start')
   value = column%int_value
   call EGRESS('sqlite3_get_column_int','finish')
end subroutine sqlite3_get_column_int
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine sqlite3_get_column_real(column, value)
implicit none
type(SQLITE_COLUMN), intent(inout) :: column
real, intent(out)                  :: value

   call EGRESS('sqlite3_get_column_real','start')
   value = column%double_value
   call EGRESS('sqlite3_get_column_real','finish')
end subroutine sqlite3_get_column_real
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine sqlite3_get_column_double(column, value)
implicit none
type(SQLITE_COLUMN), intent(inout) :: column
real(kind=dp), intent(out)              :: value

   call EGRESS('sqlite3_get_column_double','start')
   value = column%double_value
   call EGRESS('sqlite3_get_column_double','finish')
end subroutine sqlite3_get_column_double
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine sqlite3_get_column_char(column, value)
implicit none
type(SQLITE_COLUMN), intent(inout) :: column
character(len=*), intent(out)      :: value

   call EGRESS('sqlite3_get_column_char','start')
   value = column%char_value
   call EGRESS('sqlite3_get_column_char','finish')
end subroutine sqlite3_get_column_char
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!     sqlite3_error(3f) - [M_sqlite] Return the last error code
!!
!!##SYNOPSIS
!!
!!    err = sqlite3_error(db)
!!
!!        type(SQLITE_DATABASE) db
!!
!!##DESCRIPTION
!!      Return last SQLite error code for this database
!!
!!##OPTIONS
!!      db   Structure variable identifying the database connection
!!
!!##RETURNS
!!      err  Retrieve whether the previous SQLite command resulted in an error or not. Returns
!!           true if so, otherwise false.
logical function sqlite3_error(db)
   type(SQLITE_DATABASE) :: db

   call EGRESS('sqlite3_error','start')
   sqlite3_error = db%error .ne. 0
end function sqlite3_error
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!   sqlite3_errmsg - [M_sqlite] Return the last error message
!!##SYNOPSIS
!!
!!   errmsg = sqlite3_errmsg(db)
!!
!!    type(SQLITE_DATABASE) db
!!
!!##DESCRIPTION
!!        Retrieve the last error message as a string
!!
!!##ARGUMENTS
!!      db      Variable identifying the database connection
!!
!!##RETURNS
!!      errmsg  Last SQLite error message for this database
function sqlite3_errmsg(db)
   type(SQLITE_DATABASE) :: db
   character(len=:),allocatable :: sqlite3_errmsg

   call EGRESS('sqlite3_errmsg','start')
   sqlite3_errmsg = db%errmsg
   call EGRESS('sqlite3_errmsg','finish')

end function sqlite3_errmsg
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!  sqlite3_open(3f) - [M_sqlite] Open a database file
!!
!!##SYNOPSIS
!!
!!    subroutine sqlite3_open(filename, db)
!!
!!        character(len=*) filename
!!        type(SQLITE_DATABASE) db
!!
!!##DESCRIPTION
!!        Open a database file and store the connection for later use.
!!
!!        Side effects:
!!
!!           The database file is opened and can be used via the db argument
!!
!!##OPTIONS
!!        filename  The name of the database file (it may also be ":mem" to get a
!!                  memory-based database instead of a file on disk)
!!
!!        db        Structure variable to identify the database connection
subroutine sqlite3_open(fname, db)
implicit none
character(len=*)      :: fname
type(SQLITE_DATABASE) :: db

   interface
      function sqlite3_open_f(fnamec, handle) bind(C,name="sqlite3_open_c")
         import c_char, c_int
         integer(kind=c_int)           :: sqlite3_open_c
         character(len=1,kind=c_char)  :: fnamec(*)
         integer(kind=c_int)           :: handle(*)
         integer(kind=c_int)           :: sqlite3_open_f
      end function sqlite3_open_f
   end interface
   call EGRESS('sqlite3_open','start')

   db%db_handle   = 0_c_int
   db%error       = 0_c_int
   db%errmsg      = ' '

   db%error = sqlite3_open_f(Fs2Ca(fname), db%db_handle)

   call EGRESS('sqlite3_open','finish')
end subroutine sqlite3_open
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!   sqlite3_close(3f) - [M_sqlite] Close a database file
!!
!!##SYNOPSIS
!!
!!    subroutine sqlite3_close(db)
!!
!!        type(SQLITE_DATABASE) db
!!
!!##DESCRIPTION
!!
!!    Close the database connection. Simply an interface to the corresponding C
!!    function.
!!
!!    Side effects:
!!
!!      The database file is closed and can no longer be accessed
!!
!!##OPTIONS
!!        db    Structure variable identifying the database connection
subroutine sqlite3_close(db)
implicit none
type(SQLITE_DATABASE) :: db

   interface
      function sqlite3_close_f(handle) bind(C,name="sqlite3_close_c")
         import c_int
         integer(kind=c_int) :: sqlite3_close_f
         integer(kind=c_int) :: handle(*)
      end function sqlite3_close_f
   end interface

   call EGRESS('sqlite3_close','start')
   db%error = sqlite3_close_f(db%db_handle)
   db%db_handle   = 0_c_int
   call EGRESS('sqlite3_close','finish')

end subroutine sqlite3_close
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!   sqlite3_do(3f) - [M_sqlite] Run a single SQL command
!!##SYNOPSIS
!!
!!    subroutine sqlite3_do(db, command)
!!
!!        type(SQLITE_DATABASE) db
!!        character(len=*) command
!!
!!##DESCRIPTION
!!   Run a single SQL command.
!!
!!   Side effects:
!!
!!      Whatever effects the command has.
!!
!!      Note
!!      that no output is reported back to the
!!      caller (except for error codes and
!!      messages if any).
!!
!!##OPTIONS
!!     db       Variable identifying the database connection
!!     command  String holding a complete SQL command
subroutine sqlite3_do(db, command)
implicit none
type(SQLITE_DATABASE)             :: db
character(len=*),intent(in)       :: command
type(c_ptr)                       :: errmsg_cptr

! ident_1="@(#)M_sqlite::sqlite3_do(3f): Run a single SQL command"

   interface
      function sqlite3_do_f(handle, command, errmsg_cptr_c) bind(C,name="sqlite3_do_c")
         import c_char, c_int, c_ptr
         integer(kind=c_int)                      :: sqlite3_do_f
         integer(kind=c_int),intent(in)           :: handle(*)
         character(len=1,kind=c_char),intent(in)  :: command(*)
         type(c_ptr),intent(out)                  :: errmsg_cptr_c
      end function sqlite3_do_f
   end interface

   call EGRESS('sqlite3_do','start')

   db%error  = sqlite3_do_f(db%db_handle, Fs2Ca(command), errmsg_cptr)

   db%errmsg = Cp2Fs_v1(errmsg_cptr)

   call EGRESS('sqlite3_do','finish')

end subroutine sqlite3_do
!==================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    sqlite3_begin(3f) - [M_sqlite] Start a transaction on the given database
!!
!!##SYNOPSIS
!!
!!   subroutine sqlite3_begin(db)
!!
!!    type(SQLITE_DATABASE) db
!!
!!##DESCRIPTION
!!    Start a transaction on the given database. When the corresponding
!!    routine sqlite3_commit is called, all changes will be made
!!    permanent. Use a transaction to gather lots of changes to the
!!    database - this is much faster than an automatic commission after
!!    each change.
!!
!!    Note:
!!
!!       Should be accompanied by a call to either
!!       sqlite3_commit or sqlite3_rollback
!!
!!##OPTIONS
!!      db    Structure variable identifying  the database
subroutine sqlite3_begin(db)
implicit none
type(SQLITE_DATABASE) :: db

   call EGRESS('sqlite3_begin','start')
   call sqlite3_do(db, "BEGIN TRANSACTION")
   call EGRESS('sqlite3_begin','finish')

end subroutine sqlite3_begin
!==================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!   sqlite3_commit(3f) - [M_sqlite] Commits a transaction on the given database
!!##SYNOPSIS
!!
!!    subroutine sqlite3_commit(db)
!!
!!     type(SQLITE_DATABASE) db
!!
!!##DESCRIPTION
!!    Commit the changes made since the start of a transaction. This makes the
!!    changes permanent.
!!
!!    Note:
!!
!!     Accompanies sqlite3_begin
!!
!!##OPTIONS
!!     db            Structure for the database
subroutine sqlite3_commit(db)
implicit none
type(SQLITE_DATABASE) :: db

   call EGRESS('sqlite3_commit','start')
   call sqlite3_do(db, "COMMIT TRANSACTION")
   call EGRESS('sqlite3_commit','finish')

end subroutine sqlite3_commit
!==================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    sqlite3_rollback(3f) - [M_sqlite] Rolls back any changes to the database since the last commit
!!
!!##SYNOPSIS
!!
!!    subroutine sqlite3_rollback(db)
!!
!!     type(SQLITE_DATABASE) :: db
!!
!!##DESCRIPTION
!!
!!    Undo the changes made since the start of a transaction. The database
!!    will be restored to the state it was in before the transaction was
!!    started.
!!
!!    Note:
!!
!!     Accompanies sqlite3_begin
!!
!!##OPTIONS
!!     db     Structure for the database
subroutine sqlite3_rollback(db)
implicit none
type(SQLITE_DATABASE) :: db

   call EGRESS('sqlite3_rollback','start')
   call EGRESS('sqlite3_rollback','finish')
   call sqlite3_do(db, "ROLLBACK")

end subroutine sqlite3_rollback
!==================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    sqlite3_delete_table(3f) - [M_sqlite] Delete a table
!!
!!##SYNOPSIS
!!
!!    subroutine sqlite3_delete_table(db)
!!
!!     type(SQLITE_DATABASE) db
!!     character(len=*) tablename
!!
!!##DESCRIPTION
!!    Delete an existing table by name. Convenience routine that constructs
!!    an SQL statement to do the actual job.
!!
!!    Note:
!!
!!     The table can not be recovered, unless this is part of a transaction
!!
!!##OPTIONS
!!    db            Structure for the database
!!    tablename     Name of the table to be deleted
subroutine sqlite3_delete_table(db, tablename)
implicit none
type(SQLITE_DATABASE)        :: db
character(len=*)             :: tablename

   character(len=:),allocatable :: command

   call EGRESS('sqlite3_delete_table','start')
   command="DELETE TABLE "//trim(tablename)
   call sqlite3_do(db, command)
   call EGRESS('sqlite3_delete_table','finish')

end subroutine sqlite3_delete_table
!==================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    sqlite3_create_table(3f) - [M_sqlite] Create a new table
!!
!!##SYNOPSIS
!!
!!    subroutine sqlite3_create_table(db)
!!
!!     type(SQLITE_DATABASE)      :: db
!!     character(len=*)           :: tablename
!!     type(SQLITE_COLUMN)        :: columns(:)
!!     character(len=*), optional :: primary
!!
!!##DESCRIPTION
!!    Create a new table, based on the properties of the columns. Convenience
!!    routine that constructs an SQL statement to do the actual job.
!!
!!    Side effects:
!!
!!     The new table is created
!!##OPTIONS
!!      db          Structure for the database
!!      tablename   Name of the table to be created
!!      columns     An array of the properties of the columns in the tables
!!                  name, type, ...)
!!      primary     Name of the column that acts as the primary key (if any).
!!                  This gets the SQL "UNIQUE" constraint.
subroutine sqlite3_create_table(db, tablename, columns, primary)
implicit none
type(SQLITE_DATABASE)              :: db
character(len=*)                   :: tablename
type(SQLITE_COLUMN), dimension(:)  :: columns
character(len=*), optional         :: primary

! ident_2="@(#)M_sqlite::sqlite3_create_table(3f): Create a new table"

   character(len=:),allocatable        :: command
   character(len=:),allocatable        :: primary_
   integer                             :: i
   integer                             :: ncols
   integer                             :: howbig

   call EGRESS('sqlite3_create_table','start')

   if(present(primary))then
      primary_ = primary
   else
      primary_ = ' '
   endif

   ncols = size(columns)

   INQUIRE(IOLENGTH=howbig) 'create table ', tablename, ' (', &                                      ! get length of command
      (trim(columns(i)%name), ' ', trim(typename(columns(i), primary_)), ', ', i = 1,ncols-1), &
      trim(columns(ncols)%name), ' ', trim(typename(columns(ncols),primary_)), ')'
   allocate(character(len=howbig) :: command)                                                        ! allocate command
   write(command, '(*(a))') 'create table ', tablename, ' (', &
      (trim(columns(i)%name), ' ', trim(typename(columns(i), primary_)), ', ', i = 1,ncols-1), &
      trim(columns(ncols)%name), ' ', trim(typename(columns(ncols),primary_)), ')'

   call sqlite3_do(db, command)

   call EGRESS('sqlite3_create_table','finish')
end subroutine sqlite3_create_table
!==================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    sqlite3_prepare_select(3f) - [M_sqlite] Prepare a selection of data from the database
!!
!!##SYNOPSIS
!!
!!    subroutine sqlite3_prepare_select(db, tablename, columns, stmt, extra_clause)
!!
!!     type(SQLITE_DATABASE)     :: db
!!     character(len=*)          :: tablename
!!     type(SQLITE_COLUMN)       :: columns(:)
!!     type(SQLITE_STATEMENT)    :: stmt
!!     character(len=*),optional :: extra_clause
!!
!!##DESCRIPTION
!!    Prepare a SELECT query. Convenience routine that creates the SQL query and
!!    "compiles" (prepares) it for later actual execution.
!!
!!    Side effects:
!!
!!     A new selection is prepared
!!
!!##OPTIONS
!!     db            Structure identifying the database connection
!!     tablename     Name of the table to be queried
!!     columns       An array of the properties of the columns to be returned
!!     stmt          A derived type used as a handle to the prepared statement (returned)
!!     extra_clause  Extra clause for SELECT statement (appended)
!!                   such as "SORT BY" or "GROUP BY"
subroutine sqlite3_prepare_select(db, tablename, columns, stmt, extra_clause)
implicit none
type(SQLITE_DATABASE)                       :: db
character(len=*)                            :: tablename
type(SQLITE_COLUMN), dimension(:), pointer  :: columns     ! On return: actual columns!
character(len=*), optional                  :: extra_clause
type(SQLITE_STATEMENT), intent(out)         :: stmt

   character(len=:),allocatable                :: command
   integer                                     :: nocols
   integer                                     :: i
   integer                                     :: cmdsize
   !
   ! Prepare the select statement for this table
   !
   ! TODO: expand the syntax!!
   !
   call EGRESS('sqlite3_prepare_select','start')
   nocols = size(columns)

   inquire(iolength=cmdsize)'select ', (trim(column_func(columns(i))), ',', i = 1,nocols-1), &
       trim(column_func(columns(nocols))), ' from ', trim(tablename)
   allocate(character(len=cmdsize) :: command)
   write(command, '(*(a))') 'select ', (trim(column_func(columns(i))), ',', i = 1,nocols-1), &
       trim(column_func(columns(nocols))), ' from ', trim(tablename)

   if(present(extra_clause))then
      command = trim(command) // ' ' // extra_clause
   endif

   call sqlite3_prepare(db, command, stmt, columns)
   call EGRESS('sqlite3_prepare_select','finish')

end subroutine sqlite3_prepare_select
!==================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
! Side effects:
!    A new row is written to the database
!
!>
!!##NAME
!!    sqlite3_insert(3f) - [M_sqlite] Insert a complete row into the given table
!!
!!##SYNOPSIS
!!
!!    subroutine sqlite3_insert(db, tablename, columns)
!!
!!     type(SQLITE_DATABASE) db
!!     character(len=*) tablename
!!     type(SQLITE_COLUMN), dimension(:) columns
!!
!!##DESCRIPTION
!!    Insert a complete new row into the table.
!!
!!##OPTIONS
!!    db            Structure identifying the database connection
!!    tablename     Name of the table into which the row must be inserted
!!    columns       An array of values for all columns to be inserted
subroutine sqlite3_insert(db, tablename, columns)
implicit none
type(SQLITE_DATABASE)                       :: db
character(len=*)                            :: tablename
type(SQLITE_COLUMN), dimension(:), target   :: columns
character(len=:),allocatable                :: command

   type(SQLITE_COLUMN), dimension(:), pointer  :: prepared_columns
   type(SQLITE_STATEMENT)                      :: stmt
   integer                                     :: i
   integer                                     :: rc
   type(c_ptr)                                 :: errmsg_cptr
   integer                                     :: howbig

   interface
      subroutine sqlite3_errmsg_f(handle, errmsg_cptr) bind(C,name="sqlite3_errmsg_c")
         import c_int,c_ptr
         integer(kind=c_int)   :: handle(*)
         type(c_ptr)           :: errmsg_cptr
      end subroutine sqlite3_errmsg_f
   end interface

   interface
      function sqlite3_bind_int_f(handle, colidx, value) bind(C,name="sqlite3_bind_int_c")
         import c_int
         integer(kind=c_int)   :: sqlite3_bind_int_f
         integer(kind=c_int)   :: handle(*)
         integer               :: colidx
         integer               :: value
      end function sqlite3_bind_int_f
   end interface

   interface
      function sqlite3_bind_double_f(handle, colidx, value) bind(C,name="sqlite3_bind_double_c")
         import c_int, dp
         integer(kind=c_int)   :: sqlite3_bind_double_f
         integer(kind=c_int)   :: handle(*)
         integer               :: colidx
         real(kind=dp)         :: value
      end function sqlite3_bind_double_f
   end interface

   interface
      integer function sqlite3_bind_text_f(handle, colidx, value) bind(C,name="sqlite3_bind_text_c")
         import c_int, c_char
         integer(kind=c_int)          :: handle(*)
         integer                      :: colidx
         character(len=1,kind=c_char) :: value(*)
      end function sqlite3_bind_text_f
   end interface

   ! Prepare the insert statement for this table

   call EGRESS('sqlite3_insert','start')

   inquire(IOLENGTH=howbig)   'insert into ', trim(tablename), ' values(', ('?,', i = 1,size(columns)-1), '?)'
   allocate(character(len=howbig) :: command)
   write(command, '(*(a))') 'insert into ', trim(tablename), ' values(', ('?,', i = 1,size(columns)-1), '?)'

   prepared_columns => columns
   call sqlite3_prepare(db, command, stmt, prepared_columns)

   ! Bind the values
   do i = 1,size(columns)
      select case(columns(i)%type_set)
      case(SQLITE_INT)
         rc = sqlite3_bind_int_f(stmt%stmt_handle, i, columns(i)%int_value)
      case(SQLITE_DOUBLE)
         rc = sqlite3_bind_double_f(stmt%stmt_handle, i, columns(i)%double_value)
      case(SQLITE_CHAR)
         rc = sqlite3_bind_text_f(stmt%stmt_handle, i, Fs2Ca(trim(columns(i)%char_value)))
      end select
      if(rc .ne. 0)then
         db%error = rc
         call sqlite3_errmsg_f(db%db_handle, errmsg_cptr)
         db%errmsg = Cp2Fs_v1(errmsg_cptr)
      endif
   enddo

   ! Actually perform the insert command
   call sqlite3_step(stmt, rc)
   call sqlite3_finalize(stmt)
   call EGRESS('sqlite3_insert','finish')

end subroutine sqlite3_insert
!==================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    sqlite3_next_row(3f) - [M_sqlite] Gets the next row of data from a selection
!!
!!##SYNOPSIS
!!
!!    subroutine sqlite3_next_row(stmt, columns, finished)
!!
!!     type(SQLITE_STATEMENT) :: stmt
!!     type(SQLITE_COLUMN)    :: columns(:)
!!     logical                :: finished
!!
!!##DESCRIPTION
!!    Retrieve the next row of a SELECT query. If the argument "finished" is set
!!    to true, the previous row was the last one.
!!
!!##OPTIONS
!!    stmt          Prepared statement. A derived type used as a handle
!!                  to the prepared statement
!!    columns       Columns to be returned
!!    finished      Indicates when there is no more data.
!!                  Set to .TRUE. if the last row was retrieved.
subroutine sqlite3_next_row(stmt, columns, finished)
implicit none
type(SQLITE_STATEMENT),intent(in) :: stmt
type(SQLITE_COLUMN)               :: columns(:)
logical,intent(out)               :: finished

   interface
      function sqlite3_column_int_f(handle, colidx) result ( value) bind(C,name="sqlite3_column_int_c")
         import c_int
         integer(kind=c_int)        :: handle(*)
         integer,value              :: colidx
         integer                    :: value
      end function sqlite3_column_int_f
   end interface

   !! SQLITE_API double sqlite3_column_double(sqlite3_stmt*, int iCol);
   interface
      function sqlite3_column_double_f(handle, colidx) result( value) bind(C,name="sqlite3_column_double_c")
         import c_int, dp
         integer(kind=c_int)        :: handle(*)
         integer,value              :: colidx
         real(kind=dp)              :: value
      end function sqlite3_column_double_f
   end interface

   !! SQLITE_API const unsigned char *sqlite3_column_text(sqlite3_stmt*, int iCol);
   interface
      function sqlite3_column_text_f(handle, colidx) result( value) bind(C,name="sqlite3_column_text_c")
         import c_int, c_ptr
         integer(kind=c_int)          :: handle(*)
         integer,value                :: colidx
         type(c_ptr)                  :: value
      end function sqlite3_column_text_f
   end interface

   integer                     :: rc
   integer                     :: idum
   integer                     :: i

   call EGRESS('sqlite3_next_row','start')

   call sqlite3_step(stmt, rc)

   if(rc .eq. SQLITE_ROW)then
      finished = .false.
      do i = 1,size(columns)                                                                ! Get the values
         select case(columns(i)%type_set)                                                  !! TODO: check validity of "type_set"
         case(SQLITE_INT)
            columns(i)%int_value = sqlite3_column_int_f(stmt%stmt_handle, i-1)
         case(SQLITE_REAL,SQLITE_DOUBLE)
            columns(i)%double_value = sqlite3_column_double_f(stmt%stmt_handle, i-1)
         case(SQLITE_CHAR)
            columns(i)%char_value = cp2fs_v1(sqlite3_column_text_f(stmt%stmt_handle, i-1))
         end select
      enddo
   else
      finished = .true.
   endif

   call EGRESS('sqlite3_next_row','finish')

end subroutine sqlite3_next_row
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    sqlite3_query_table(3f) - [M_sqlite] Retrieve the column names and types from a table
!!
!!##SYNOPSIS
!!
!!    subroutine sqlite3_query_table(db, tablename, columns)
!!
!!     type(SQLITE_DATABASE) db
!!     character(len=*) tablename
!!     type(SQLITE_COLUMN), dimension(:), pointer columns
!!
!!##DESCRIPTION
!!    Query the structure of the table
!!
!!    Side effects:
!!
!!       The columns array is allocated and filled
!!
!!    Note:
!!
!!       On entry the columns argument must not be
!!       associated. On exit, it will point to a
!!       freshly allocated array of column names/types
!!
!!##OPTIONS
!!     db           Structure variable identifying the database connection
!!     tablename    Name of the table to be inspected
!!     columns      An array with the properties of all columns. Deallocate
!!                  it when you are done.
subroutine sqlite3_query_table(db, tablename, columns)
implicit none
type(SQLITE_DATABASE)                       :: db
character(len=*),intent(in)                 :: tablename
type(SQLITE_COLUMN), dimension(:), pointer  :: columns     ! On return: actual columns!

   type(SQLITE_STATEMENT)                   :: stmt
   character(len=:),allocatable             :: command

   call EGRESS('sqlite3_query_table','start')

   if(associated(columns))then      ! we must free the columns, but can not be sure they are no longer used.
      nullify(columns)              ! So simply disassociate.
   endif

   command='select * from '//trim(tablename)
   call sqlite3_prepare(db, command, stmt, columns)
   call sqlite3_finalize(stmt)

   call EGRESS('sqlite3_query_table','finish')

end subroutine sqlite3_query_table
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    sqlite3_finalize(3f) - [M_sqlite] Finalize the prepared SQL statement
!!
!!##SYNOPSIS
!!
!!    subroutine sqlite3_finalize(stmt)
!!
!!     type(SQLITE_STATEMENT) stmt
!!
!!##DESCRIPTION
!!    Free all resources associated with the prepared statement.
!!
!!##OPTIONS
!!    stmt    A derived type used as a handle to the prepared statement
subroutine sqlite3_finalize(stmt)
implicit none
type(SQLITE_STATEMENT)                      :: stmt

   !! SQLITE_API int sqlite3_finalize(sqlite3_stmt *pStmt);
   interface
      subroutine sqlite3_finalize_f(stmt_handle) bind(C,name="sqlite3_finalize_c")
         import c_int
         integer(kind=c_int) :: stmt_handle(*)
      end subroutine sqlite3_finalize_f
   end interface

   call EGRESS('sqlite3_finalize','start')
   call sqlite3_finalize_f(stmt%stmt_handle)
   call EGRESS('sqlite3_finalize','finish')

end subroutine sqlite3_finalize
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    sqlite3_reset(3f) - [M_sqlite] Reset the prepared SQL statement so that it can be used again
!!
!!##SYNOPSIS
!!
!!    subroutine sqlite3_reset(stmt)
!!
!!     type(SQLITE_STATEMENT) stmt
!!
!!##DESCRIPTION
!!    Reset the prepared statement so that it can be used again.
!!
!!##OPTIONS
!!    stmt   A derived type used as a handle to the prepared statement
subroutine sqlite3_reset(stmt)
implicit none
type(SQLITE_STATEMENT)                      :: stmt

   interface
      subroutine sqlite3_reset_f(stmt_handle) bind(C,name="sqlite3_reset_c")
         import c_int
         integer(kind=c_int) :: stmt_handle(*)
      end subroutine sqlite3_reset_f
   end interface

   call EGRESS('sqlite3_reset','start')
   call sqlite3_reset_f(stmt%stmt_handle)
   call EGRESS('sqlite3_reset','finish')

end subroutine sqlite3_reset
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    sqlite3_step(3f) - [M_sqlite] Run the prepared SQL statement
!!
!!##SYNOPSIS
!!
!!    subroutine sqlite3_step(stmt, completion)
!!
!!     type(SQLITE_STATEMENT) stmt
!!     integer completion
!!
!!##DESCRIPTION
!!    Run the prepared SQL statement for one step. The code in completion
!!    will tell whether it was successful or not. Simply an interface to
!!    the equivalent C routine.
!!
!!##OPTIONS
!!    stmt        A derived type used as a handle to the prepared statement
!!    completion  Return code, indicating if the command is complete or
!!                not (SQLITE_DONE (success), SQLITE_MISUSE or SQLITE_ERROR)
subroutine sqlite3_step(stmt, completion)
implicit none
type(SQLITE_STATEMENT)                :: stmt
integer, intent(out)                  :: completion
integer(kind=c_int)                   :: completionc

   interface
      subroutine sqlite3_step_f(stmt_handle, completionc) bind(C,name="sqlite3_step_c")
         import c_int
         integer(kind=c_int)             :: stmt_handle(*)
         integer(kind=c_int),intent(out) :: completionc
      end subroutine sqlite3_step_f
   end interface


   call EGRESS('sqlite3_step','start')
   call sqlite3_step_f(stmt%stmt_handle, completionc)
   completion=int(completionc)
   call EGRESS('sqlite3_step','finish')

end subroutine sqlite3_step
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    sqlite3_prepare(3f) - [M_sqlite] Reset the prepared SQL statement so that it can be used again
!!
!!##SYNOPSIS
!!
!!    subroutine sqlite3_prepare(db, command, stmt, columns)
!!
!!     type(SQLITE_DATABASE)       :: db
!!     character(len=*)            :: command
!!     type(SQLITE_STATEMENT)      :: stmt
!!     type(SQLITE_COLUMN),pointer :: columns(:)
!!
!!##DESCRIPTION
!!    Prepare a general SQL statement for later actual execution. The
!!    statement can be any SQL statement.
!!
!!##OPTIONS
!!
!!      db       Variable identifying the database connection
!!      command  The SQL statement to be prepared
!!      stmt     A derived type used as a handle to the prepared statement
!!      columns  An array of the properties of the columns that will be
!!               returned by the statement. The routine returns an allocated
!!               array. You must deallocate it yourself, when it is no
!!               longer needed.
subroutine sqlite3_prepare(db, command, stmt, columns)
implicit none
type(SQLITE_DATABASE),intent(inout) :: db
character(len=*), intent(in)         :: command
type(SQLITE_STATEMENT)               :: stmt
type(SQLITE_COLUMN), pointer         :: columns(:)  ! On return: actual columns!

   interface
      integer function sqlite3_prepare_f(db, commandc, stmt) bind(C,name="sqlite3_prepare_c")
         import c_char
         integer                        :: db(*)
         character(len=1,kind=c_char)   :: commandc(*)
         integer                        :: stmt(*)
      end function sqlite3_prepare_f
   end interface

   interface
      subroutine sqlite3_errmsg_f(handle, errmsg_cptr) bind(C,name="sqlite3_errmsg_c")
         import c_int, c_ptr
         integer(kind=c_int)            :: handle(*)
         type(c_ptr)                    :: errmsg_cptr
      end subroutine sqlite3_errmsg_f
   end interface

   interface
      subroutine sqlite3_column_count_f(handle, count) bind(C,name="sqlite3_column_count_c")
         import c_int
         integer(kind=c_int)            :: handle(*)
         integer(kind=c_int)            :: count
      end subroutine sqlite3_column_count_f
   end interface

   interface
      subroutine sqlite3_column_name_type_f(handle, colidx, cptr_name, cptr_type) bind(C,name="sqlite3_column_name_type_c")
         import c_ptr, c_int, c_char
         integer(kind=c_int)            :: handle(*)
         integer                        :: colidx
         type(c_ptr)                    :: cptr_name
         type(c_ptr)                    :: cptr_type
      end subroutine sqlite3_column_name_type_f
   end interface

   integer                              :: count
   integer                              :: i
   type(c_ptr)                          :: errmsg_cptr
   type(c_ptr)                          :: cptr_name
   type(c_ptr)                          :: cptr_type

   call EGRESS('sqlite3_prepare','start')
   db%error = sqlite3_prepare_f(db%db_handle, Fs2Ca(command), stmt%stmt_handle)

   if(db%error .eq. 0)then
      if(associated(columns))then
         call EGRESS('sqlite3_prepare','finish')
         return ! Assumption: they are already known
      endif

      call sqlite3_column_count_f(stmt%stmt_handle, count)
      allocate(columns(1:count))

      do i = 1,count
         call sqlite3_column_name_type_f(stmt%stmt_handle, i-1, cptr_name, cptr_type)
         columns(i)%name=Cp2Fs_v2(cptr_name)
         columns(i)%type=Cp2Fs_v2(cptr_type)

         select case(columns(i)%type(1:4))
         case('INT ', 'INTE')
            columns(i)%type_set = SQLITE_INT
         case('FLOA', 'DOUB')
            columns(i)%type_set = SQLITE_DOUBLE
         case('CHAR', 'VARC')
            columns(i)%type_set = SQLITE_CHAR
         end select

      enddo
   else
      call sqlite3_errmsg_f(db%db_handle, errmsg_cptr)
      db%errmsg = Cp2Fs_v1(errmsg_cptr)
   endif
   call EGRESS('sqlite3_prepare','finish')

end subroutine sqlite3_prepare
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!! sqlite3_get_table(3f) - [M_sqlite] Call sqlite3_exec() and return the result in an array of strings
!!
!!##SYNOPSIS
!!
!!    subroutine sqlite3_get_table(db, command, result, errmsg)
!!
!!     type(SQLITE_DATABASE)    :: db
!!     character(len=*)         :: command
!!     character(len=*),pointer :: result(:)
!!     character(len=*)         :: errmsg
!!
!!##DESCRIPTION
!!    Get the result of a query in a single two-dimensional array
!!
!!    Note:
!!
!!       The result array is _nullified_ first, then allocated
!!       to hold the resulting table (within the limits of the
!!       character strings). It is up to the user to deallocate
!!       this array when done.
!!
!!    Further note:
!!
!!       Because we have to split the process into two parts,
!!       to allocate an array that is large enough to hold all
!!       strings, use is made of a static variable. As a consequence
!!       this routine is _not_ thread-safe.
!!
!!##OPTIONS
!!     db        Variable identifying the database connection
!!     command   The SQL command (query) to be executed
!!     result    A two-dimensional array of strings that will be filled
!!               with the results of the SQL command. When done, you will
!!               have to deallocate it.
!!     errmsg    Error message (if any). If there is an error, then "result"
!!               will not be allocated, and "errmsg" will contain the
!!               information about the error that occurred.
!!
subroutine sqlite3_get_table(db, command, result, errmsg)
implicit none
type(SQLITE_DATABASE), intent(inout)    :: db
character(len=*), intent(in)            :: command
character(len=*), pointer               :: result(:,:)
character(len=*), intent(out)           :: errmsg

   type(c_ptr)                          :: resultc
   integer                              :: ncol
   integer                              :: nrow
   type(c_ptr)                          :: errmsg_cptr

   interface
      integer function sqlite3_get_table_1_f(handle, commandc, ncol, nrow, errmsg) bind(C,name="sqlite3_get_table_1_c")
         import c_int, c_char, c_ptr
         integer(kind=c_int)          :: handle(*)
         character(len=1,kind=c_char) :: commandc(*)
         integer                      :: ncol
         integer                      :: nrow
         type(c_ptr)                  :: errmsg
      end function sqlite3_get_table_1_f
   end interface

   interface
      function sqlite3_get_table_2_f(ncol, nrow, len_result) result(result) bind(C,name="sqlite3_get_table_2_c")
         import c_ptr, c_char
         integer,value,intent(in)      :: ncol
         integer,value,intent(in)      :: nrow
         integer,value                 :: len_result
         type(c_ptr)                   :: result
      end function sqlite3_get_table_2_f
   end interface

   call EGRESS('sqlite3_get_table','start')

   db%error  = sqlite3_get_table_1_f(db%db_handle, Fs2Ca(command), ncol, nrow, errmsg_cptr)
   db%errmsg=Cp2Fs_v1(errmsg_cptr)

   if(db%error == 0)then
      allocate(result(ncol,nrow+1))
      resultc=sqlite3_get_table_2_f(ncol, nrow, len(result))
      if(.not. c_associated(resultc))then
         write(*,*)'ERROR : *sqlite3_get_table* : C string pointer not associated'
      endif
      call c_f_pointer(resultc,result,[ncol,nrow])
   endif

   errmsg = db%errmsg
   call EGRESS('sqlite3_get_table','finish')

end subroutine sqlite3_get_table
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
function Cp2Fs_v2(cptr_str) result(f_string)
! gets a C string (pointer), and returns the corresponding Fortran string;
! If the C string is null, it returns a NULL character, similar to C's "(null)"
! printed in similar cases:
   use, intrinsic :: iso_c_binding, only: c_ptr,c_f_pointer,c_char,c_null_char
   type(c_ptr), intent(in) :: cptr_str
   character(len=:), allocatable :: f_string
   character(kind=c_char), dimension(:), pointer :: char_array_pointer => null()
   integer,parameter :: maxsize=1024
   character(len=MAXSIZE) :: aux_string
   integer :: i,length=0
   call c_f_pointer(cptr_str,char_array_pointer,[MAXSIZE])
   if(.not.associated(char_array_pointer))then
     allocate(character(len=1)::f_string); f_string=char(0); return
   endif
   aux_string=" "
   length=MAXSIZE-1
   do i=1,MAXSIZE
     if(char_array_pointer(i)==c_null_char)then
       length=i-1; exit
     endif
     aux_string(i:i)=char_array_pointer(i)
   enddo
   allocate(character(len=length)::f_string)
   f_string=aux_string(1:length)
end function Cp2Fs_v2
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
! gets a C string (pointer), and returns the corresponding Fortran string up to 4096(max_len) characters;
! If the C string is null, it returns string C "null" character:
function Cp2Fs_v1(cptr_str) result(f_string)
implicit none
type(c_ptr), intent(in)         :: cptr_str
character(len=:), allocatable   :: f_string

   !!character(kind=c_char), pointer :: fchar_ptr(:) => null()
   !!character(kind=c_char), pointer :: fchar_ptr => null()
   character(kind=c_char), pointer :: fchar_ptr(:) => null()
   integer                         :: i
   integer                         :: length
   integer                         :: clen

   call EGRESS('Cp2Fs_v1','start')
   length=0
   if(.not. c_associated(cptr_str))then
      !!write(6,*)'ERROR : *Cp2Fs_v1* : cptr_str not associated';flush(6)
      f_string=' '
      call EGRESS('Cp2Fs_v1','finish')
      return
   endif
   clen=int(strlen(cptr_str))

   call c_f_pointer(cptr_str,fchar_ptr, [clen])

   if(.not.associated(fchar_ptr))then
     !!write(6,*)'ERROR : *Cp2Fs_v1* : fchar_ptr not associated';flush(6)
     f_string=c_null_char
     call EGRESS('Cp2Fs_v1','finish')
     return
   endif

   allocate(character(len=clen) :: f_string)

   do i=1,clen
     f_string(i:i)=fchar_ptr(i)
   enddo

   call EGRESS('Cp2Fs_v1','finish')
end function Cp2Fs_v1
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
function Ca2Fs_v2(s) result(str)
implicit none
character(kind=c_char,len=1), intent(in) :: s(*)
character(len=:), allocatable :: str

   integer i, nchars

   call EGRESS('Ca2Fs_v2','start')

   i = 1
   do
      if(s(i) == c_null_char) exit
      i = i + 1
   end do

   nchars = i - 1  ! Exclude null character from Fortran string
   allocate(character(len=nchars) :: str)
   str = transfer(s(1:nchars), str)

   call EGRESS('Ca2Fs_v2','finish')
end function Ca2Fs_v2
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
function cp2fp_char(c_str) result(f_str)
use, intrinsic :: iso_c_binding, only: c_ptr, c_f_pointer, c_char
implicit none
type(c_ptr), intent(in)           :: c_str
character(:,kind=c_char), pointer :: f_str

   character(kind=c_char), pointer   :: arr(:)
   call EGRESS('cp2fp_char','start')
   if(.not.c_associated(c_str))then
      write(*,*)'ERROR: *cp2fp_char* s_str not associated'
      f_str=c_null_char
   else
      call c_f_pointer(c_str, arr, [strlen(c_str)]+1)
      call get_scalar_pointer(size(arr), arr, f_str)
   endif
   call EGRESS('cp2fp_char','finish')
end function cp2fp_char
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
subroutine get_scalar_pointer(scalar_len, scalar, ptr)
implicit none
integer, intent(in) :: scalar_len
character(kind=c_char,len=scalar_len), intent(in), target :: scalar(1)
character(len=:,kind=c_char), intent(out), pointer :: ptr

   call EGRESS('sqlite3_scalar_pointer','start')
   ptr => scalar(1)
   call EGRESS('sqlite3_scalar_pointer','finish')
end subroutine get_scalar_pointer
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
pure function Ca2Fs_v1(array)  result (string)
character(len=1),intent(in)  :: array(:)
character(len=size(array))   :: string

! ident_3="@(#)M_system::Ca2Fs_v1(3fp): function copies null-terminated char array to string"

   integer                      :: i

   string=' '
   do i = 1,size(array)
      if(array(i).eq.char(0))then
         exit
      else
         string(i:i) = array(i)
      endif
   enddo

end function Ca2Fs_v1
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
pure function Fs2Ca(string) result (array)
character(len=*),intent(in)     :: string
character(len=1,kind=c_char)    :: array(len(string)+1)

! ident_4="@(#)M_system::Fs2Ca(3fp): function copies string to null terminated char array"

   integer                      :: i

   do i = 1,len_trim(string)
      array(i) = string(i:i)
   enddo
   array(size(array))=c_null_char

end function Fs2Ca
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    egress(3f) - [M_sqlite] write message indicating nesting level of routines
!!##SYNOPSIS
!!
!!    subroutine egress(name,status)
!!
!!     character(len=*),intent(in) :: name
!!     character(len=*),intent(in) :: status
!!
!!##DESCRIPTION
!!     Internal routine for debugging and development. Called at beginning and
!!     exit of each procedure to help identify where crashes occur.
!!
!!##OPTIONS
!!     name    name of procedure being called from
!!     status  'start' or 'finish'
!!
!!##EXAMPLE
!!
subroutine egress(name,status)
implicit none
character(len=*),intent(in) :: name
character(len=*),intent(in) :: status

   integer,save                :: indent=0

   if(.not.M_sqlite3_debug)return

   if(status.eq.'start')then
      indent=indent+1
      write(6,'(a,"STARTED ",a32,1x,a,i0)')repeat('>',indent),name,repeat('=',80-indent-40),indent
   else
      write(6,'(a,"EXITED  ",a32,1x,a,i0)')repeat('>',indent),name,repeat('=',80-indent-40),indent
      indent=indent-1
   endif

   flush(6)

end subroutine egress
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    sqlite3_libversion(3f) - [M_sqlite] obtain string describing library version
!!
!!##SYNOPSIS
!!
!!    version=sqlite3_libversion()
!!
!!      character(len=:),allocatable :: version
!!
!!##DESCRIPTION
!!      Get library version.
!!
!!##EXAMPLE
!!
!!  Sample example code
!!
!!    program demo_sqlite3_libversion
!!    use M_sqlite, only : sqlite3_libversion
!!    use M_sqlite, only : sqlite3_libversion_number
!!    use M_sqlite, only : sqlite3_sourceid
!!    character(len=:),allocatable :: message
!!
!!    message=sqlite3_libversion()
!!    write(*,*)'SQLITE3 LIBRARY VERSION=',message
!!
!!    message = sqlite3_sourceid()
!!    write(*,*)'SQLITE3 SOURCEID=',message
!!
!!    ivalue  = sqlite3_libversion_number()
!!    write(*,*)'SQLITE3 VERSION NUMBER=',ivalue
!!
!!    end program demo_sqlite3_libversion
!!
!!  Typical Results:
!!
!!    SQLITE3 LIBRARY VERSION=3.21.0
!!    SQLITE3 SOURCEID=2017-10-24 18:55:49 1a584e499906b5c87ec7d43d4abce641fdf017c42125b083109bc77c4de48827
!!    SQLITE3 VERSION NUMBER=     3021000
function sqlite3_libversion() result(string__OUT)
implicit none
interface
! SQLITE_API const char *sqlite3_libversion(void);
function ptr_sqlite3_libversion() result (sqlite3_libversion__OUT) bind(C, name='sqlite3_libversion')
   import c_ptr
   implicit none
   type(C_PTR) :: sqlite3_libversion__OUT  ! const char *sqlite3_libversion
end function ptr_sqlite3_libversion
end interface
! WRAPPED: const char *sqlite3_libversion(void);
character(len=:), allocatable :: string__OUT
   string__OUT=Cp2Fs_v2(ptr_sqlite3_libversion())
end function sqlite3_libversion
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    sqlite3_sourceid(3f) - [M_sqlite] obtain string describing library version
!!
!!##SYNOPSIS
!!
!!    version=sqlite3_sourceid()
!!
!!      character(len=:),allocatable :: version
!!
!!##DESCRIPTION
!!      Get library version.
!!
!!##EXAMPLE
!!
!!  Sample example code
!!
!!    program demo_sqlite3_sourceid
!!    use M_sqlite, only : sqlite3_libversion
!!    use M_sqlite, only : sqlite3_libversion_number
!!    use M_sqlite, only : sqlite3_sourceid
!!    character(len=:),allocatable :: message
!!
!!    message=sqlite3_libversion()
!!    write(*,*)'SQLITE3 LIBRARY VERSION=',message
!!
!!    message = sqlite3_sourceid()
!!    write(*,*)'SQLITE3 SOURCEID=',message
!!
!!    ivalue  = sqlite3_libversion_number()
!!    write(*,*)'SQLITE3 VERSION NUMBER=',ivalue
!!
!!    end program demo_sqlite3_sourceid
!!
!!  Typical Results:
!!
!!    SQLITE3 LIBRARY VERSION=3.21.0
!!    SQLITE3 SOURCEID=2017-10-24 18:55:49 1a584e499906b5c87ec7d43d4abce641fdf017c42125b083109bc77c4de48827
!!    SQLITE3 VERSION NUMBER=     3021000
function sqlite3_sourceid() result(string__OUT)
implicit none
interface
! SQLITE_API const char *sqlite3_sourceid(void);
function ptr_sqlite3_sourceid() result (sqlite3_sourceid__OUT) bind(C, name='sqlite3_sourceid')
   import c_ptr
   implicit none
   type(C_PTR) :: sqlite3_sourceid__OUT  ! const char *sqlite3_sourceid
end function ptr_sqlite3_sourceid
end interface
! WRAPPED: const char *sqlite3_sourceid(void);
character(len=:), allocatable :: string__OUT
   string__OUT=Cp2Fs_v2(ptr_sqlite3_sourceid())
end function sqlite3_sourceid
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
function my_strlen(cptr)
implicit none
type(c_ptr) :: cptr
integer     :: my_strlen
interface
   function strlen_f(string) bind(C, name='strlen')
      use, intrinsic :: iso_c_binding, only: c_ptr, c_size_t
      implicit none
      type(c_ptr)          :: string
      integer(c_size_t)    :: strlen_f
   end function strlen_f
end interface

   my_strlen=strlen_f(cptr)

end function my_strlen


subroutine my_print(cptr)
implicit none
type(c_ptr) :: cptr
interface
   function print_f(cstring,cptr) bind(C, name='printf')
      import c_ptr, c_char, c_int
      implicit none
      character(len=1,kind=c_char) :: cstring(*)
      type(c_ptr)                  :: cptr
      integer(kind=c_int)          :: print_f
   end function print_f
end interface
   integer :: idum

   idum=print_f(Fs2Ca("MY_PRINT [%s]\n"),cptr)
   write(6,*);flush(6)

end subroutine my_print
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
subroutine test_suite_M_sqlite()
use M_verify, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg
use M_verify, only : unit_check_level

!! setup
   call test_sqlite3_begin()
   call test_sqlite3_close()
   call test_sqlite3_column_props()
   call test_sqlite3_column_query()
   call test_sqlite3_commit()
   call test_sqlite3_create_table()
   call test_sqlite3_delete_table()
   call test_sqlite3_do()
   call test_sqlite3_errmsg()
   call test_sqlite3_error()
   call test_sqlite3_finalize()
   call test_sqlite3_get_column_char()
   call test_sqlite3_get_column_double()
   call test_sqlite3_get_column_int()
   call test_sqlite3_get_column_real()
   call test_sqlite3_get_table()
   call test_sqlite3_insert()
   call test_sqlite3_libversion()
   call test_sqlite3_next_row()
   call test_sqlite3_open()
   call test_sqlite3_prepare()
   call test_sqlite3_prepare_select()
   call test_sqlite3_query_table()
   call test_sqlite3_reset()
   call test_sqlite3_rollback()
   call test_sqlite3_set_column_char()
   call test_sqlite3_set_column_double()
   call test_sqlite3_set_column_int()
   call test_sqlite3_set_column_real()
   call test_sqlite3_sourceid()
   call test_sqlite3_step()
!! teardown
contains
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_sqlite3_begin()

   call unit_check_start('sqlite3_begin',msg='')
   !!call unit_check('sqlite3_begin', 0.eq.0, 'checking',100)
   call unit_check_done('sqlite3_begin',msg='')
end subroutine test_sqlite3_begin
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_sqlite3_close()

   call unit_check_start('sqlite3_close',msg='')
   !!call unit_check('sqlite3_close', 0.eq.0, 'checking',100)
   call unit_check_done('sqlite3_close',msg='')
end subroutine test_sqlite3_close
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_sqlite3_column_props()

   call unit_check_start('sqlite3_column_props',msg='')
   !!call unit_check('sqlite3_column_props', 0.eq.0, 'checking',100)
   call unit_check_done('sqlite3_column_props',msg='')
end subroutine test_sqlite3_column_props
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_sqlite3_column_query()

   call unit_check_start('sqlite3_column_query',msg='')
   !!call unit_check('sqlite3_column_query', 0.eq.0, 'checking',100)
   call unit_check_done('sqlite3_column_query',msg='')
end subroutine test_sqlite3_column_query
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_sqlite3_commit()

   call unit_check_start('sqlite3_commit',msg='')
   !!call unit_check('sqlite3_commit', 0.eq.0, 'checking',100)
   call unit_check_done('sqlite3_commit',msg='')
end subroutine test_sqlite3_commit
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_sqlite3_create_table()

   call unit_check_start('sqlite3_create_table',msg='')
   !!call unit_check('sqlite3_create_table', 0.eq.0, 'checking',100)
   call unit_check_done('sqlite3_create_table',msg='')
end subroutine test_sqlite3_create_table
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_sqlite3_delete_table()

   call unit_check_start('sqlite3_delete_table',msg='')
   !!call unit_check('sqlite3_delete_table', 0.eq.0, 'checking',100)
   call unit_check_done('sqlite3_delete_table',msg='')
end subroutine test_sqlite3_delete_table
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_sqlite3_do()

   call unit_check_start('sqlite3_do',msg='')
   !!call unit_check('sqlite3_do', 0.eq.0, 'checking',100)
   call unit_check_done('sqlite3_do',msg='')
end subroutine test_sqlite3_do
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_sqlite3_errmsg()

   call unit_check_start('sqlite3_errmsg',msg='')
   !!call unit_check('sqlite3_errmsg', 0.eq.0, 'checking',100)
   call unit_check_done('sqlite3_errmsg',msg='')
end subroutine test_sqlite3_errmsg
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_sqlite3_error()

   call unit_check_start('sqlite3_error',msg='')
   !!call unit_check('sqlite3_error', 0.eq.0, 'checking',100)
   call unit_check_done('sqlite3_error',msg='')
end subroutine test_sqlite3_error
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_sqlite3_finalize()

   call unit_check_start('sqlite3_finalize',msg='')
   !!call unit_check('sqlite3_finalize', 0.eq.0, 'checking',100)
   call unit_check_done('sqlite3_finalize',msg='')
end subroutine test_sqlite3_finalize
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_sqlite3_get_column_char()

   call unit_check_start('sqlite3_get_column_char',msg='')
   !!call unit_check('sqlite3_get_column_char', 0.eq.0, 'checking',100)
   call unit_check_done('sqlite3_get_column_char',msg='')
end subroutine test_sqlite3_get_column_char
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_sqlite3_get_column_double()

   call unit_check_start('sqlite3_get_column_double',msg='')
   !!call unit_check('sqlite3_get_column_double', 0.eq.0, 'checking',100)
   call unit_check_done('sqlite3_get_column_double',msg='')
end subroutine test_sqlite3_get_column_double
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_sqlite3_get_column_int()

   call unit_check_start('sqlite3_get_column_int',msg='')
   !!call unit_check('sqlite3_get_column_int', 0.eq.0, 'checking',100)
   call unit_check_done('sqlite3_get_column_int',msg='')
end subroutine test_sqlite3_get_column_int
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_sqlite3_get_column_real()

   call unit_check_start('sqlite3_get_column_real',msg='')
   !!call unit_check('sqlite3_get_column_real', 0.eq.0, 'checking',100)
   call unit_check_done('sqlite3_get_column_real',msg='')
end subroutine test_sqlite3_get_column_real
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_sqlite3_get_table()

   call unit_check_start('sqlite3_get_table',msg='')
   !!call unit_check('sqlite3_get_table', 0.eq.0, 'checking',100)
   call unit_check_done('sqlite3_get_table',msg='')
end subroutine test_sqlite3_get_table
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_sqlite3_insert()

   call unit_check_start('sqlite3_insert',msg='')
   !!call unit_check('sqlite3_insert', 0.eq.0, 'checking',100)
   call unit_check_done('sqlite3_insert',msg='')
end subroutine test_sqlite3_insert
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_sqlite3_libversion()

   call unit_check_start('sqlite3_libversion',msg='')
   !!call unit_check('sqlite3_libversion', 0.eq.0, 'checking',100)
   call unit_check_done('sqlite3_libversion',msg='')
end subroutine test_sqlite3_libversion
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_sqlite3_next_row()

   call unit_check_start('sqlite3_next_row',msg='')
   !!call unit_check('sqlite3_next_row', 0.eq.0, 'checking',100)
   call unit_check_done('sqlite3_next_row',msg='')
end subroutine test_sqlite3_next_row
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_sqlite3_open()

   call unit_check_start('sqlite3_open',msg='')
   !!call unit_check('sqlite3_open', 0.eq.0, 'checking',100)
   call unit_check_done('sqlite3_open',msg='')
end subroutine test_sqlite3_open
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_sqlite3_prepare()

   call unit_check_start('sqlite3_prepare',msg='')
   !!call unit_check('sqlite3_prepare', 0.eq.0, 'checking',100)
   call unit_check_done('sqlite3_prepare',msg='')
end subroutine test_sqlite3_prepare
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_sqlite3_prepare_select()

   call unit_check_start('sqlite3_prepare_select',msg='')
   !!call unit_check('sqlite3_prepare_select', 0.eq.0, 'checking',100)
   call unit_check_done('sqlite3_prepare_select',msg='')
end subroutine test_sqlite3_prepare_select
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_sqlite3_query_table()

   call unit_check_start('sqlite3_query_table',msg='')
   !!call unit_check('sqlite3_query_table', 0.eq.0, 'checking',100)
   call unit_check_done('sqlite3_query_table',msg='')
end subroutine test_sqlite3_query_table
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_sqlite3_reset()

   call unit_check_start('sqlite3_reset',msg='')
   !!call unit_check('sqlite3_reset', 0.eq.0, 'checking',100)
   call unit_check_done('sqlite3_reset',msg='')
end subroutine test_sqlite3_reset
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_sqlite3_rollback()

   call unit_check_start('sqlite3_rollback',msg='')
   !!call unit_check('sqlite3_rollback', 0.eq.0, 'checking',100)
   call unit_check_done('sqlite3_rollback',msg='')
end subroutine test_sqlite3_rollback
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_sqlite3_set_column_char()

   call unit_check_start('sqlite3_set_column_char',msg='')
   !!call unit_check('sqlite3_set_column_char', 0.eq.0, 'checking',100)
   call unit_check_done('sqlite3_set_column_char',msg='')
end subroutine test_sqlite3_set_column_char
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_sqlite3_set_column_double()

   call unit_check_start('sqlite3_set_column_double',msg='')
   !!call unit_check('sqlite3_set_column_double', 0.eq.0, 'checking',100)
   call unit_check_done('sqlite3_set_column_double',msg='')
end subroutine test_sqlite3_set_column_double
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_sqlite3_set_column_int()

   call unit_check_start('sqlite3_set_column_int',msg='')
   !!call unit_check('sqlite3_set_column_int', 0.eq.0, 'checking',100)
   call unit_check_done('sqlite3_set_column_int',msg='')
end subroutine test_sqlite3_set_column_int
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_sqlite3_set_column_real()

   call unit_check_start('sqlite3_set_column_real',msg='')
   !!call unit_check('sqlite3_set_column_real', 0.eq.0, 'checking',100)
   call unit_check_done('sqlite3_set_column_real',msg='')
end subroutine test_sqlite3_set_column_real
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_sqlite3_sourceid()

   call unit_check_start('sqlite3_sourceid',msg='')
   !!call unit_check('sqlite3_sourceid', 0.eq.0, 'checking',100)
   call unit_check_done('sqlite3_sourceid',msg='')
end subroutine test_sqlite3_sourceid
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_sqlite3_step()

   call unit_check_start('sqlite3_step',msg='')
   !!call unit_check('sqlite3_step', 0.eq.0, 'checking',100)
   call unit_check_done('sqlite3_step',msg='')
end subroutine test_sqlite3_step
!===================================================================================================================================
end subroutine test_suite_M_sqlite
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
end module M_sqlite
!===================================================================================================================================
