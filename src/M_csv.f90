










module M_csv
implicit none
private
character(len=1),public       :: CSV_separator   =','
character(len=10),save,public :: CSV_quotes      ='default'
character(len=30),save,public :: CSV_true        ='TRUE'
character(len=30),save,public :: CSV_false       ='FALSE'
integer,save,public           :: CSV_LUN         = 6
integer,save                  :: G_linecolumn    = 1

! STILL WRITING THIS. JUST DOES SIMPLE WRITES TO FAR
!     This module uses the following default rules:
!     - items are always separated by a single comma (,)
!     - string items are delimited by double quotes (")
!     - embedded double quotes are treated by doubling the quote
!     - trailing blanks are considered irrelevant
! OPTIONS
!    build allocatable string and output
!    allow title line
!    allow delimiter
!    quote just strings with delimiter, quote all strings, quote all values
!    allow for printing DAT date_and_time array as an "SQL date"
!    should doubles have D or E exponent?

! build as a line and output line or
! output each item one at a time
! or build internal array or multi-line string and output all at once

! open file
! write items to specific lun or to one file at a time

! subroutine csv_open(
! subroutine csv_write( integer|real|doubleprecision,character, maybe complex?)  ! scalar
! subroutine csv_write( integer|real|doubleprecision,character, maybe complex?)  ! one-dimensional array as one row
! subroutine csv_write( integer|real|doubleprecision,character, maybe complex?)  ! two-dimensional table
! subroutine csv_close

!     module for reading and writing CSV-files
!
!     The file to write to must already be opened as a LU-number is passed.
!
!     Layout of the CSV-file:
!     - single items are written to the end of the current record
!     - one-dimensional items are also written to the end of the current record
!     - two-dimensional items are written to separate records, one for each row
!     - except for the two-dimensional versions, all routines allow you to suppress advancing to the next record:
!       - for single items you must indicate whether to advance or not
!       - for one-dimensional items, the argument is optional. Default is to advance.
!
! ident_1="@(#)M_csv::csv_write(3f): write scalar intrinsic type using current CSV style"

public csv_write

interface csv_write
   module procedure csv_write_scalar
   module procedure csv_write_row
   module procedure csv_write_table
end interface

public csv

public test_suite_M_csv
contains
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!   csv_write(3f) - [M_csv] prints intrinsic type to a file assigned to LUN CSV_LUN
!!   (LICENSE:PD)
!!##SYNOPSIS
!!
!!
!!   function write_csv(g1,g2,g3,..g20|g(:)|g(:,:))
!!
!!    class(*),intent(in),optional           :: g1, g2, g3, g4, g5, g6, g7, g8, g9, g10
!!                                           &  g11,g12,g13,g14,g15,g16,g17,g18,g19,g20
!!       or
!!    class(*),intent(in),optional           :: g(:)
!!       or
!!    class(*),intent(in),optional           :: g(:,:)
!!
!!
!!##DESCRIPTION
!!
!!   write_csv(3f) writes values in CSV(Comma-Separated Values) format. Either up to
!!   twenty scalar values, a vector, or a matrix is allowed as an argument(s).
!!
!!   The data is written to the LUN CSV_LUN, which is assumed to have been opened
!!   by the program.
!!
!!##OPTIONS
!!   g[1-20]  optional values to print the value of. May
!!            be of type INTEGER, LOGICAL, REAL, DOUBLEPRECISION, COMPLEX,
!!            or CHARACTER. The line is not advanced.
!!   g(:)     a vector is written as a list of values. The line is not advanced.
!!   g(:,:)   each row becomes an output line.
!!
!!   If no arguments are provided the current line is terminated.
!!
!!##EXAMPLES
!!
!!
!!  Sample program:
!!
!!    program  demo_csv_write
!!    !
!!    use M_csv,     only : csv_write
!!    use M_csv,     only : CSV_lun, CSV_TRUE, CSV_FALSE
!!    implicit none
!!    integer                :: i
!!    integer                :: v
!!    integer                :: iarr(10)=[(i*10,i=1,size(iarr))]
!!    real,dimension(3,4)    :: rand2d
!!    integer,dimension(3,4) :: array2d
!!
!!       open(newunit=CSV_lun,file='csv_test.csv',action='write')
!!       CSV_true='TRUE'
!!       CSV_false='FALSE'
!!
!!       ! a number of scalar values in a row
!!       do i = 0,8
!!          v = 10**i
!!          call csv_write( v )
!!       enddo
!!       call csv_write() ! end line
!!
!!       ! strings, some with double-quotes in them
!!       call csv_write( 'Aha','"Aha"','Aha "!"')
!!       call csv_write() ! end line
!!
!!       ! lots of types
!!       call csv_write('string',.true.,.false.,111,23.45,10.20e15)
!!       call csv_write(3456.78901234d0,cmplx(huge(0.0),tiny(0.0)))
!!       call csv_write() ! end line
!!
!!       call csv_write(1.234)                   ! scalars
!!       call csv_write(1.234d0)
!!       call csv_write([1,2,3,4,5,6,7,8,9,10])  ! a vector
!!       call csv_write()                        ! end line
!!
!!       call csv_write(iarr)         ! a vector
!!       call csv_write() ! end line  ! even a vector needs a line end
!!
!!       call random_number( rand2d ) ! a table is written one row per line
!!       array2d = int( rand2d*100.0)
!!       call csv_write( array2d)
!!
!!       close( unit=CSV_lun)
!!
!!    end program demo_csv_write
!!
!!   Results:
!!
!!    1,10,100,1000,10000,100000,1000000,10000000,100000000
!!    "Aha","""Aha""","Aha ""!"""
!!    "string",TRUE,FALSE,111,23.4500008,1.01999997E+16,3456.7890123400002,3.40282347E+38,1.17549435E-38
!!    1.23399997,1.2340000000000000,1,2,3,4,5,6,7,8,9,10
!!    10,20,30,40,50,60,70,80,90,100
!!    64,95,86,28
!!    78,94,18,36
!!    10,89,6,86
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!   csv(3f) - [M_csv] prints up to 20 standard scalar types to a string in CSV style
!!   (LICENSE:PD)
!!##SYNOPSIS
!!
!!
!!   function csv(g1,g2,g3,..g20)
!!
!!    class(*),intent(in),optional           :: g1, g2, g3, g4, g5, g6, g7, g8, g9, g10
!!                                           &  g11,g12,g13,g14,g15,g16,g17,g18,g19,g20
!!    character,len=(:),allocatable          :: csv
!!
!!##DESCRIPTION
!!   csv(3f) builds a CSV string from up to twenty scalar values or an array.
!!
!!   Although it will often work, using csv(3f) in an I/O statement is
!!   not recommended as the csv(3f) routine might generate error messages;
!!   and Fortran does not (yet) support recursive I/O.
!!
!!##OPTIONS
!!   g[1-20]  optional values to print the value of. May
!!            be of type INTEGER, LOGICAL, REAL, DOUBLEPRECISION, COMPLEX,
!!            or CHARACTER.
!!##RETURNS
!!   csv     a string in CSV (Comma-Separated Values) format representing the input values
!!
!!##EXAMPLES
!!
!!
!!  Sample program:
!!
!!    program demo_csv
!!    use M_csv, only : csv
!!    implicit none
!!    character(len=:),allocatable :: pr
!!
!!       write(*,*)'LIST-DIRECTED:'
!!       write(*,*,DELIM='QUOTE')'string',.true.,.false.,111,23.45,10.20e15,3456.78901234d0,cmplx(huge(0.0),tiny(0.0))
!!
!!       write(*,*)'G0:'
!!       write(*,'(*(g0:","))')'string',.true.,.false.,111,23.45,10.20e15,3456.78901234d0,cmplx(huge(0.0),tiny(0.0))
!!
!!       write(*,*)'CSV:'
!!       pr=csv('string',.true.,.false.,111,23.45,10.20e15,3456.78901234d0,cmplx(huge(0.0),tiny(0.0)) )
!!       write(*,'(a)')pr
!!
!!    end program demo_csv
!!
!!   Results:
!!
!!     LIST-DIRECTED:
!!     "string" T F         111   23.4500008       1.01999997E+16   3456.7890123400002        (3.402823466E+38,1.175494351E-38)
!!     G0:
!!    string,T,F,111,23.4500008,0.101999997E+17,3456.7890123400002,0.340282347E+39,0.117549435E-37
!!     CSV:
!!    "string",TRUE,FALSE,111,23.4500008,1.01999997E+16,3456.7890123400002,3.40282347E+38,1.17549435E-38
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
!===================================================================================================================================
function csv(g1, g2, g3, g4, g5, g6, g7, g8, g9, g10, g11, g12, g13, g14, g15, g16, g17, g18, g19, g20)
use M_strings, only : quote
implicit none

! ident_2="@(#)M_csv::csv(3f): writes up to twenty standard scalar types as a line in a CSV file"

class(*),intent(in),optional  ::  g1  ,g2  ,g3  ,g4  ,g5,  g6  ,g7  ,g8  ,g9, g10
class(*),intent(in),optional  :: g11 ,g12 ,g13 ,g14 ,g15, g16 ,g17 ,g18 ,g19, g20
!!class(*),intent(in),optional  :: g6(..) ,g7(..) ,g8(..) ,g9(..)
character(len=:), allocatable :: csv
character(len=4096)           :: line
integer                       :: istart
integer,parameter             :: increment=2

   istart=1
   line=' '
   if(present( g1))call print_g(g1)
   if(present( g2))call print_g(g2)
   if(present( g3))call print_g(g3)
   if(present( g4))call print_g(g4)
   if(present( g5))call print_g(g5)
   if(present( g6))call print_g(g6)
   if(present( g7))call print_g(g7)
   if(present( g8))call print_g(g8)
   if(present( g9))call print_g(g9)
   if(present(g11))call print_g(g10)
   if(present(g11))call print_g(g11)
   if(present(g12))call print_g(g12)
   if(present(g13))call print_g(g13)
   if(present(g14))call print_g(g14)
   if(present(g15))call print_g(g15)
   if(present(g16))call print_g(g16)
   if(present(g17))call print_g(g17)
   if(present(g18))call print_g(g18)
   if(present(g19))call print_g(g19)
   if(present(g20))call print_g(g20)
   csv=trim(line)
contains
!===================================================================================================================================
subroutine print_g(generic)
!use, intrinsic :: iso_fortran_env, only : int8, int16, int32, biggest=>int64, real32, real64, dp=>real128
use,intrinsic :: iso_fortran_env, only : int8, int16, int32, int64, real32, real64, real128
class(*),intent(in) :: generic

!select case(rank(generic))
!case(0)
   select type(generic)
      type is (integer(kind=int8));     write(line(istart:),'(i0)') generic
      type is (integer(kind=int16));    write(line(istart:),'(i0)') generic
      type is (integer(kind=int32));    write(line(istart:),'(i0)') generic
      type is (integer(kind=int64));    write(line(istart:),'(i0)') generic
      type is (real(kind=real32));      write(line(istart:),'(1pg0)') generic
      type is (real(kind=real64));      write(line(istart:),'(1pg0)') generic
      type is (real(kind=real128));     write(line(istart:),'(1pg0)') generic
      type is (logical);                write(line(istart:),'(a)') trim(merge(CSV_true,CSV_false,generic))
      type is (character(len=*));       write(line(istart:),'(a)') quote(generic)
      type is (complex);                write(line(istart:),'(1pg0,a,1pg0)') real(generic),CSV_separator,aimag(generic)
   end select
   if(istart.ne.1)then
      line(istart-1:istart-1)=CSV_separator
   endif
   istart=len_trim(line)+increment
!   case(1)
!end select
end subroutine print_g
!===================================================================================================================================
end function csv
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
!>
!!##NAME
!!    csv_write_scalar(3fp) - Write a scalar intrinsic value to the CSV-file
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!   subroutine csv_write_scalar( value, advance, lun )
!!
!!    class(*),intent(in),optional    :: value
!!    logical,intent(in),optional     :: advance
!!    integer,intent(in),optional     :: lun
!!##DESCRIPTION
!!    The value is written to the current record of the CSV-file
!!##OPTION
!!      lun        LU-number of the CSV-file
!!      value      Value to write
!!      advance    Advance (.true.) or not, so that more items can be
!!                 written to the same record. Default is .FALSE. .
!!##EXAMPLE
!!
!!   Sample program
!!
!!    program demo_csv_write_scalar
!!    use M_csv, only : csv_write_scalar
!!
!!    end program demo_csv_write_scalar
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine csv_write_scalar(g1,g2,g3,g4,g5,g6,g7,g8,g9,g10,g11,g12,g13,g14,g15,g16,g17,g18,g19,g20)
class(*),intent(in),optional ::  g1  ,g2  ,g3  ,g4  ,g5,  g6  ,g7  ,g8  ,g9, g10
class(*),intent(in),optional :: g11 ,g12 ,g13 ,g14 ,g15, g16 ,g17 ,g18 ,g19, g20
character(len=:),allocatable :: string

   if(.not.present(g1))then                                                               ! end current line
      G_linecolumn=1
      write(CSV_LUN,*)
   else                                                                                   ! convert values to a string
      string=csv(g1,g2,g3,g4,g5,g6,g7,g8,g9,g10,g11,g12,g13,g14,g15,g16,g17,g18,g19,g20)

      if(G_linecolumn.ne.1)then
         string=CSV_separator//string
      endif

      write(CSV_LUN,'(a)',advance='no') string
      G_linecolumn=G_linecolumn+len(string)
   endif

end subroutine csv_write_scalar
!>
!!##NAME
!!   csv_write_row(3f) - [M_csv] Write a one-dimensional array of items to the CSV-file
!!   (LICENSE:PD)
!!##SYNOPSIS
!!
!!   Use M_csv, only : csv_write_row, CSV_LUN
!!   subroutine csv_write_row( array )
!!
!!    class(*),intent(in)            :: array(:)
!!
!!##OPTIONS
!!       array      Array to write of any type supported by csv(3f).
!!##RESULT
!!       The array is written to the current record of the CSV-file
!!##EXAMPLE
!!
!!   Sample program:
!!
!!    program demo_csv_write_row
!!    use M_csv,     only : csv_write, CSV_lun
!!    implicit none
!!
!!       CSV_lun=6
!!       call csv_write( [ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 ] )
!!       call csv_write()
!!       call csv_write(iarr)
!!       call csv_write()
!!
!!    end program demo_csv_write_row
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
subroutine csv_write_row( array )
class(*),intent(in) :: array(:)
   integer          :: i

   do i = 1,size(array)
      call csv_write_scalar( array(i) )
   enddo

end subroutine csv_write_row
!>
!!##NAME
!!   csv_write_table(3f) - [M_csv] Write a two-dimensional array of items to the CSV-file
!!   (LICENSE:PD)
!!##SYNOPSIS
!!
!!   use M_csv, only : csv_write_table, CSV_LUN
!!
!!    subroutine csv_write_table(array)
!!    class(*),intent(in) :: array(:,:)
!!##DESCRIPTION
!!    Write a two-dimensional array of intrinsic scalars to LUN CSV_LUN in
!!    CSV (Comma-Separated Values) format.
!!##OPTIONS
!!    array      Array to write in CSV format. May be of any type
!!               supported by csv(3f).
!!
!!##RESULT
!!    The array is written to the current CSV-file assumed open as LUN CSV_LUN.
!!    One row generates one line of the file.
!!##EXAMPLE
!!
!!   Sample program
!!
!!    program demo_csv_write_table
!!    use M_csv,     only : csv_write, CSV_LUN
!!    implicit none
!!    real, dimension(8,14)    :: rand2d
!!    integer, dimension(8,14) :: array2d
!!
!!       ! open CSV file
!!       open(newunit=CSV_LUN,file='csv_test.csv',action='write')
!!
!!       ! file an array with random values
!!       call random_number(rand2d)
!!       array2d = int(rand2d*100.0)
!!
!!       ! write the array in CSV format
!!       call csv_write(array2d)
!!
!!       ! close the file
!!       close(unit=CSV_LUN)
!!
!!    end program demo_csv_write_table
!!
!!  Sample output
!!
!!    35,24,93,51,60,76,14,50,52,94,75,36,39,26
!!    73,91,85,71,84,27,41,6,0,37,66,88,96,44
!!    56,44,66,61,77,40,75,5,18,12,40,96,23,85
!!    45,66,60,28,73,39,71,23,73,44,19,0,77,63
!!    25,89,72,28,55,32,42,46,22,77,57,86,91,79
!!    46,86,37,18,71,86,83,34,14,80,25,45,62,10
!!    12,80,77,53,18,94,56,50,42,37,48,41,25,98
!!    97,17,74,58,35,33,27,27,39,60,46,11,38,87
!!
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
subroutine csv_write_table(array)
class(*),intent(in) :: array(:,:)
integer             :: i,j
   do i = 1,size(array,dim=1)
      do j=1,size(array,dim=2)
         call csv_write( array(i,j) )
      enddo
      call csv_write_scalar()
   enddo
end subroutine csv_write_table
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
subroutine test_suite_M_csv()
use M_verify, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg
use M_verify, only : unit_check_level
!! setup
   call test_csv()
   call test_csv_write_row()
   call test_csv_write_scalar()
   call test_csv_write_table()
!! teardown
contains
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_csv()

   call unit_check_start('csv',msg='')
   !!call unit_check('csv', 0.eq.0, 'checking',100)
   call unit_check_done('csv',msg='')
end subroutine test_csv
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_csv_write_scalar()

   call unit_check_start('csv_write_scalar',msg='')
   !!call unit_check('csv_write_scalar', 0.eq.0, 'checking',100)
   call unit_check_done('csv_write_scalar',msg='')
end subroutine test_csv_write_scalar
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_csv_write_row()

   call unit_check_start('csv_write_row',msg='')
   !!call unit_check('csv_write_row', 0.eq.0, 'checking',100)
   call unit_check_done('csv_write_row',msg='')
end subroutine test_csv_write_row
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_csv_write_table()

   call unit_check_start('csv_write_table',msg='')
   !!call unit_check('csv_write_table', 0.eq.0, 'checking',100)
   call unit_check_done('csv_write_table',msg='')
end subroutine test_csv_write_table
!===================================================================================================================================
end subroutine test_suite_M_csv
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
end module M_csv
