subroutine help_usage(l_help)
implicit none
character(len=*),parameter     :: ident="@(#)help_usage(3f): prints help information"
logical,intent(in)             :: l_help
character(len=:),allocatable :: help_text(:)
integer                        :: i
logical                        :: stopit=.false.
stopit=.false.
if(l_help)then
help_text=[ CHARACTER(LEN=128) :: &
'NAME                                                                            ',&
'     phone(1f) - [M_sqlite] create, change and query SQLite3 database containing phone numbers',&
'     (LICENSE:PD)                                                               ',&
'SYNOPSIS                                                                        ',&
' phone [ -db file] ...                                                          ',&
'  [                                                                             ',&
'    -name proper_name                                                           ',&
'    -home phone_number                                                          ',&
'    -work phone_number                                                          ',&
'    -cell phone_number                                                          ',&
'    -email name@address                                                         ',&
'    -comment Comments                                                           ',&
'  ]                                                                             ',&
'                                                                                ',&
' phone -do SQLite3_command [ -db database_filename]                             ',&
' phone -schema [ -db database_filename]                                         ',&
' phone -debug .... other_options                                                ',&
' phone -help|-version                                                           ',&
' phone match                                                                    ',&
'                                                                                ',&
'DESCRIPTION                                                                     ',&
'                                                                                ',&
' Experimental program being used to work with the M_sqlite(3fm) module.         ',&
'                                                                                ',&
' Add entries into table "rolodex" to create a simple phone book SQLite database.',&
' Use SQL directives to query the database.                                      ',&
'                                                                                ',&
' Default database file is "phone.db". It would be $HOME/.db/phone.db" except hitting a bug.',&
'                                                                                ',&
' phone ''SELECT * FROM rolodex;''                                               ',&
' phone ''SELECT * FROM rolodex WHERE name LIKE "%Smith%";''                     ',&
'                                                                                ',&
' phone -do .  # loop reading SQL directives from input until ".quit" is entered.',&
'                                                                                ',&
'    select * from rolodex;                                                      ',&
'    select name,home from rolodex;                                              ',&
'    select name,home from rolodex where lower(name) like ''%smith%'';           ',&
'    .quit                                                                       ',&
'                                                                                ',&
'    select 3.0/4.0 as ''answer''                                                ',&
'    sqlite> SELECT sqlite_version() AS ''SQLite Version''; # get SQLite version ',&
'                                                                                ',&
'EXAMPLE                                                                         ',&
'                                                                                ',&
'AUTHOR                                                                          ',&
'   John S. Urban                                                                ',&
'LICENSE                                                                         ',&
'   Public Domain                                                                ',&
'']
   WRITE(*,'(a)')(trim(help_text(i)),i=1,size(help_text))
   stop ! if --help was specified, stop
endif
end subroutine help_usage
!>
!!##NAME
!!      phone(1f) - [M_sqlite] create, change and query SQLite3 database containing phone numbers
!!      (LICENSE:PD)
!!##SYNOPSIS
!!
!!  phone [ -db file] ...
!!   [
!!     -name proper_name
!!     -home phone_number
!!     -work phone_number
!!     -cell phone_number
!!     -email name@address
!!     -comment Comments
!!   ]
!!
!!  phone -do SQLite3_command [ -db database_filename]
!!  phone -schema [ -db database_filename]
!!  phone -debug .... other_options
!!  phone -help|-version
!!  phone match
!!
!!##DESCRIPTION
!!
!!  Experimental program being used to work with the M_sqlite(3fm) module.
!!
!!  Add entries into table "rolodex" to create a simple phone book SQLite database.
!!  Use SQL directives to query the database.
!!
!!  Default database file is "phone.db". It would be $HOME/.db/phone.db" except hitting a bug.
!!
!!  phone 'SELECT * FROM rolodex;'
!!  phone 'SELECT * FROM rolodex WHERE name LIKE "%Smith%";'
!!
!!  phone -do .  # loop reading SQL directives from input until ".quit" is entered.
!!
!!     select * from rolodex;
!!     select name,home from rolodex;
!!     select name,home from rolodex where lower(name) like '%smith%';
!!     .quit
!!
!!     select 3.0/4.0 as 'answer'
!!     sqlite> SELECT sqlite_version() AS 'SQLite Version'; # get SQLite version
!!
!!##EXAMPLE
!!
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
subroutine help_version(l_version)
implicit none
character(len=*),parameter     :: ident="@(#)help_version(3f): prints version information"
logical,intent(in)             :: l_version
character(len=:),allocatable   :: help_text(:)
integer                        :: i
logical                        :: stopit=.false.
stopit=.false.
if(l_version)then
help_text=[ CHARACTER(LEN=128) :: &
'@(#)PRODUCT:        GPF (General Purpose Fortran) utilities and examples>',&
'@(#)PROGRAM:        phone(1)>',&
'@(#)DESCRIPTION:    phone number utility>',&
'@(#)VERSION:        1.0, 20180318>',&
'@(#)AUTHOR:         John S. Urban>',&
'@(#)HOME PAGE:      http://www.urbanjost.altervista.org/index.html>',&
'@(#)COMPILED:       2023-07-22 01:27:15 UTC-240>',&
'']
   WRITE(*,'(a)')(trim(help_text(i)(5:len_trim(help_text(i))-1)),i=1,size(help_text))
   stop ! if --version was specified, stop
endif
end subroutine help_version
program phoner
   use M_kracken, only : kracken, lget, sget
   use M_time, only    : now
   use M_system, only  : system_mkdir, system_isdir
   use M_system, only  : system_perror
   use M_system, only  : system_mkdir
   use M_system, only  : R_USR,W_USR,X_USR
 !!use M_system, only  : R_GRP,W_GRP,X_GRP
 !!use M_system, only  : R_OTH,W_OTH,X_OTH
 !!use M_system, only  : RWX_G,RWX_O,RWX_U
   use M_sqlite
   implicit none

   type(SQLITE_DATABASE)                    :: db
   type(SQLITE_STATEMENT)                   :: stmt
   type(SQLITE_COLUMN),dimension(:),pointer :: column_names
   type(SQLITE_COLUMN),dimension(7)         :: columns
   logical                                  :: finished
   character(len=256),pointer               :: result(:,:)
   character(len=256)                       :: errmsg

   character(len=:),allocatable             :: env_var
   character(len=:),allocatable             :: pathname
   integer                                  :: env_sz
   integer                                  :: ierr
   integer                                  :: i,j

   logical                                  :: dbg=.false.
   logical                                  :: schema=.false.
   character(len=:),allocatable             :: name
   character(len=:),allocatable             :: home
   character(len=:),allocatable             :: work
   character(len=:),allocatable             :: cell
   character(len=:),allocatable             :: email
   character(len=:),allocatable             :: comment

   character(len=:),allocatable             :: date
   character(len=:),allocatable             :: do_cmd
!===============================================================================
   ! define and crack command line options
   call kracken('phone','-db -help .f. -version .f. -schema -debug .f. -name -home -work -cell -email -comment -do')
   call help_usage(lget('phone_help'))                                ! if -help option is present, display help text and exit
   call help_version(lget('phone_version'))                           ! if -version option is present, display version text and exit
!===============================================================================
   dbg=lget('phone_debug')
   name=trim(sget('phone_name'))
   home=trim(sget('phone_home'))
   work=trim(sget('phone_work'))
   cell=trim(sget('phone_cell'))
   email=trim(sget('phone_email'))
   comment=trim(sget('phone_comment'))
   do_cmd=trim(sget('phone_do'))

   date=now('year-month-day hour:minute:second') ! get current time in format YYYY-MM-DD HH:MM:SS
   schema= lget('phone_schema')
!===============================================================================
   !! BUG: pathnames over 23 characters have problems later when query, gfortran 6.0.4
   !! pathname='AAAAAAAAAAAAAAAphone.db'  !!  GOOD
   !! pathname='AAAAAAAAAAAAAAAAphone.db' !! BAD
   !!           123456789 123456789 123456789 123456789
   pathname=trim(sget('phone_db'))
   if(pathname.eq.' ')then
      call build_default_pathname()
   endif
   if(dbg)write(*,*)'>>>>> PATHNAME=',pathname
!===============================================================================
!  CREATE AND/OR OPEN DATABASE FILE AND RETURN DB KEY
   call sqlite3_open( pathname, db )
   if ( sqlite3_error(db) ) then
      write(*,*) 'Error: ', sqlite3_errmsg(db)
   endif
   if(dbg)write(*,*)'>>>>> OPENED DB'
!===============================================================================
! BUILD TABLE DEFINITION AND CREATE IT
   if(dbg)write(*,*)'>>>>> DEFINE SCHEMA AND CREATE TABLE'
   !!BUILD: block

      call sqlite3_column_props( columns(1), 'name',    SQLITE_CHAR, 80 )
      call sqlite3_column_props( columns(2), 'home',    SQLITE_CHAR, 20 )
      call sqlite3_column_props( columns(3), 'work',    SQLITE_CHAR, 20 )
      call sqlite3_column_props( columns(4), 'cell',    SQLITE_CHAR, 20 )
      call sqlite3_column_props( columns(5), 'email',   SQLITE_CHAR, 80 )
      call sqlite3_column_props( columns(6), 'comment', SQLITE_CHAR, 256 )
      call sqlite3_column_props( columns(7), 'date',    SQLITE_CHAR, 20 )

      call sqlite3_create_table( db, 'rolodex', columns, 'name' )
   !!endblock BUILD
   if(dbg)write(*,*)'>>>>> DATABASE DEFINED'
!===============================================================================
!  AND/OR OPEN DATABASE FILE AND RETURN DB KEY!     ! EXECUTE SQL COMMAND DIRECTLY )
   select case(do_cmd)
   case('')                                         ! no commands
   case('.')                                        ! request for interactive mode
      INTERACTIVE: block
         use M_io, only : read_line
         use M_history, only : redo
         character(len=:),allocatable :: line
         write(*,*)'Enter SQL directives ("." to stop)'
         INFINITE: do while (read_line(line)==0)
            call redo(line)
            select case(line)
            case('.','.quit')
               stop
            case('.schema')
               call print_schema('rolodex')
            case('.help')
               write(*,'(a)')[character(len=80) :: &
               & '=========================================================', &
               & 'COMMAND MACROS:', &
               & '   .|.quit  stop program', &
               & '   .help    display this help text', &
               & '   .schema  show schema for table "rolodex"', &
               & 'EXAMPLES', &
               & '   select * from rolodex where lower(name) like "%smith%"', &
               & '=========================================================', &
               & '']

            case default
               call sqlite3_do( db, line )
               if(sqlite3_error(db))then
                  write(*,*)'*phone* Error in SQL directive:',sqlite3_errmsg(db)
                  write(*,*)'*phone* Error in SQL directive:',db%errmsg
               endif
            endselect
         enddo INFINITE
         stop
      endblock INTERACTIVE
   case default                                      ! do single SQL command and stop
      call sqlite3_do( db, do_cmd )
      if(sqlite3_error(db))then
         write(*,*)'*phone* Error in SQL directive:',sqlite3_errmsg(db)
         write(*,*)'*phone* Error in SQL directive:',db%errmsg
      endif
      stop
   endselect
!===============================================================================
   if(schema)then
      call print_schema('rolodex')
   endif
!===============================================================================
   if(name.ne.'')then
      if(dbg)write(6,*)'ADD SOME DATA TO THE TABLE'
      call sqlite3_set_column( column_names(1), name )
      if(home.ne.'')   call sqlite3_set_column( column_names(2), home )
      if(work.ne.'')   call sqlite3_set_column( column_names(3), work )
      if(cell.ne.'')   call sqlite3_set_column( column_names(4), cell )
      if(email.ne.'')  call sqlite3_set_column( column_names(5), email )
      write(*,*)'>>>>>>>> COMMENT=',comment
      if(comment.ne.'')then
         write(*,*)'comment=',comment
         call sqlite3_set_column( column_names(6), comment )
      else
         write(*,*)'skip entering comment'
      endif
      if(date.ne.'')   call sqlite3_set_column( column_names(7), date )

      call sqlite3_insert( db, 'rolodex', column_names )
      call sqlite3_do( db, "commit" ) ;
   endif
!===============================================================================
   if(dbg)write(6,*)'PRINT TABLE ROW BY ROW'
   call sqlite3_prepare_select( db, 'rolodex', column_names, stmt )
   finished = .false.
   do
      call sqlite3_next_row( stmt, column_names, finished )
      if ( finished ) exit
      do i=1,7
         write(6,'(i0,1x,a,"=",a)')i,trim(column_names(i)%name), trim(column_names(i)%char_value)
      enddo
   enddo
   write(6,'(a)')repeat('=',80);flush(6)
!===============================================================================
   ! Get the entire table
   if(dbg)write(6,*)'GET ENTIRE TABLE';flush(6)
   !
   call sqlite3_get_table(db, "select * from rolodex", result, errmsg)

   if (associated(result)) then
      write(*,*) 'Number of columns: ', size(result,1)
      write(*,*) 'Number of rows:    ', size(result,2)
      do j = 1,size(result,2)
         write(*,'(i0,1x,*(a,1x))') j,(trim(result(i,j)),i=1,size(result,1))
      enddo
      deallocate(result)
   else
      write(*,*) 'Error: result table not allocated'
      write(*,*) 'Error: ', trim(errmsg)
   endif
   write(*,'(a)')repeat('=',80)
!===============================================================================
   call sqlite3_close( db )
!===============================================================================
   if(dbg)write(*,*)"That's all Folks!"
!===============================================================================
contains
!===============================================================================
subroutine build_default_pathname()
   ! build default pathname $HOME/.db/phone.db, creating the $HOME/.db/ directory
   ! if it does not exist.
   env_var='HOME'                                          ! build pathname "$HOME/.db/phone.db"
   call get_environment_variable(env_var, length=env_sz)   ! get length required to hold value of $HOME
   if(allocated(pathname)) deallocate(pathname)
   allocate(character(len=env_sz) :: pathname)             ! make string to hold value of sufficient size
   call get_environment_variable(env_var, pathname)        ! get value
   pathname=pathname//'/.db'                               ! make $HOME/.db if not there
   if(.not.system_isdir(pathname))then
      ierr=system_mkdir(pathname,IANY([R_USR,W_USR,X_USR]))
      if(ierr.ne.0)then
         call system_perror('*phone*:'//pathname)
         stop 1
      endif
   endif
   pathname=pathname//'/'//'phone.db'
end subroutine build_default_pathname
!===============================================================================
subroutine print_schema(tname)
character(len=*),intent(in) :: tname
   ! given table name display schema definition
   if(dbg)write(6,'(a)')'PRINT SCHEMA'
   nullify( column_names )
   call sqlite3_query_table( db,tname, column_names ) ! query the new table column definitions
   write(*,*)'COLUMN NAMES',size(column_names)        ! size
   do i = 1,size(column_names)                        ! name and type
      write(*,*) i,' NAME=',column_names(i)%name,' TYPE=',trim(column_names(i)%type)
   enddo
end subroutine print_schema
!===============================================================================
end program phoner
