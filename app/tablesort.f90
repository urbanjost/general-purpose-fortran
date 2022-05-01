program tablesort
use, intrinsic :: iso_fortran_env, only : stdin=>input_unit, stdout=>output_unit, stderr=>error_unit
use M_io,      only : read_table
use M_CLI2,    only : set_args, lget ,iget, files=>unnamed
use M_mrgrnk,  only : mrgrnk
implicit none

! ident_1="@(#)tablesort(1)"

character(len=*),parameter         :: g='(*(g0,1x))'
character(len=:),allocatable       :: help_text(:), version_text(:)
integer,allocatable                :: indx(:)
doubleprecision,allocatable        :: table(:,:)
integer                            :: i, j, ierr, icol
   call setup()
   call set_args('--col:c 1',help_text,version_text) ! crack command line
   do i=1,size(files)                                ! for each file read and sort lines
      call read_table(files(i),table,ierr)           ! read file into numeric array
      if(.not.allocated(table).or.ierr.ne.0)then
         write(stdout,g)'*demo_swallow* failed to load file '//files(i)
      else
         icol=iget('col')
         if(icol.le.0.or.icol.gt.size(table,dim=2))then
            write(stderr,g)'ERROR: asked to sort by column ',icol,files(i),' has ',size(table,dim=2)
            cycle
         endif

         if(allocated(indx))deallocate(indx)
         allocate(indx(size(table,dim=1)))
         call mrgrnk(table(:,icol),indx)
         if(lget('verbose'))then
            ! print values
            write(stdout,g)'file...........',files(i)
            write(stdout,g)'size...........',size(table)
            write(stdout,g)'rows...........',size(table,dim=1)
            write(stdout,g)'cols...........',size(table,dim=2)
            write(stdout,g)'sort column....',icol
            write(stdout,g)'minimum........',table(indx(1),icol), minval(table(:,icol))
            write(stdout,g)'maximum........',table(indx(size(table,dim=1)),icol), maxval(table(:,icol))
         endif
         do j=1,size(table,dim=1) ! after
            write(stdout,g)table(indx(j),:)
         enddo
         deallocate(indx,table)
      endif
   enddo
contains
subroutine setup()
help_text=[ CHARACTER(LEN=128) :: &
'NAME',&
'       tablesort(1f) - [FUNIX] sort a file containing a numeric table',&
'       (LICENSE:PD)',&
'',&
'SYNOPSIS',&
'   tablesort [--col NUM] INPUT_FILE(S)|[ --help][ --version]',&
'',&
'DESCRIPTION',&
'   Read a numeric table from a formatted file into memory and sort',&
'   it numerically by the specified column. The default column is the',&
'   left-most, designated as column "1" (one).',&
'',&
'   This is a simple use of the orderpack(3f) module and reads the files',&
'   into memory, which could cause a machine to run out of memory if',&
'   input files are large.',&
'',&
'OPTIONS',&
'       INPUT_FILE(s)  input file(s)',&
'       --col,c        column number to sort by. Columns are numbered',&
'                      from left to right starting with one.',&
'       --verbose      display additional information for each file',&
'       --help         display help text and exit',&
'       --version      display version information and exit',&
'',&
'AUTHOR',&
'   John S. Urban',&
'',&
'LICENSE',&
'   Public Domain',&
'']
!>
!!##NAME
!!        tablesort(1f) - [FUNIX] sort a file containing a numeric table
!!        (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    tablesort [--col NUM] INPUT_FILE(S)|[ --help][ --version]
!!
!!##DESCRIPTION
!!    Read a numeric table from a formatted file into memory and sort
!!    it numerically by the specified column. The default column is the
!!    left-most, designated as column "1" (one).
!!
!!    This is a simple use of the orderpack(3f) module and reads the files
!!    into memory, which could cause a machine to run out of memory if
!!    input files are large.
!!
!!##OPTIONS
!!        INPUT_FILE(s)  input file(s)
!!        --col,c        column number to sort by. Columns are numbered
!!                       from left to right starting with one.
!!        --verbose      display additional information for each file
!!        --help         display help text and exit
!!        --version      display version information and exit
!!
!!##AUTHOR
!!    John S. Urban
!!
!!##LICENSE
!!    Public Domain
version_text=[ CHARACTER(LEN=128) :: &
'PRODUCT:        GPF (General Purpose Fortran) utilities and examples',&
'PROGRAM:        tablesort(1f)',&
'DESCRIPTION:    sort a file containing a numeric table',&
'VERSION:        1.0, 2021-01-10',&
'AUTHOR:         John S. Urban',&
'HOME PAGE:      http://www.urbanjost.altervista.org/index.html',&
'']
end subroutine setup
end program tablesort
