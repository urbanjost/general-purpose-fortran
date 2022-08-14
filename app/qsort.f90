program qsort
use M_io,      only : swallow
use M_CLI2,    only : set_args, lget , files=>unnamed
use M_strings, only : upper, notabs, transliterate
use M_sort,    only : sort_quick_rx
implicit none

! ident_1="@(#) sort lines in a file"

character(len=1024)                :: message
character(len=:),allocatable       :: pageout(:), pageleft(:), help_text(:), version_text(:), makeblank,line
integer,allocatable                :: ii(:)
integer                            :: i, j, k, ilen, ios
   call setup()
   call set_args('',help_text,version_text)                                      ! crack command line
   makeblank=''
   do i=0,127
      select case(i)
      case(0:64,91:96,123:127)
         makeblank=makeblank//achar(i)
      end select
   enddo
   do i=1,size(files)                                                            ! for each file read and reverse lines
      call swallow(files(i),pageout)                                             ! allocate character array and copy file into it
      if(.not.allocated(pageout))then
         write(*,*)'*demo_swallow* failed to load file '//files(i)
      else
         allocate(ii(size(pageout)))
         allocate(character(len=(len(pageout))) :: pageleft(size(pageout)))
         do k=1,size(pageout)
            call notabs(pageout(k),pageleft(k),ilen)
            !pageleft(k)=transliterate(adjustl(upper(pageleft(k))),makeblank,' ')
            line=transliterate(pageleft(k),makeblank,' ')
            pageleft(k)=upper(adjustl(line))
         enddo
         call sort_quick_rx(pageleft,ii)
         write(*,'(a)')(trim(pageout(ii(j))),j=1,size(ii))
         !write(*,'(a)')(trim(pageleft(ii(j))),j=1,size(ii))
         deallocate(ii,pageleft,pageout)
      endif
   enddo
contains
subroutine setup()
help_text=[ CHARACTER(LEN=128) :: &
'NAME',&
'       qsort(1f) - [FUNIX] sort a file',&
'       (LICENSE:PD)',&
'',&
'SYNOPSIS',&
'   qsort INPUT_FILE(S) [ --help][ --version]',&
'',&
'DESCRIPTION',&
'   Sort lines in a file by reading into memory and sorting',&
'   alphabetically. Case and non-alphanumeric characters are ignored.',&
'',&
'   This is a simple use of the M_sort(3f) module and reads the files',&
'   into memory, which could cause a machine to run out of memory if',&
'   input files are large.',&
'',&
'OPTIONS',&
'       INPUT_FILE(s)  input file(s)',&
'       --help         display help text and exit',&
'       --version      display version information and exit',&
'',&
'AUTHOR',&
'   John S. Urban',&
'',&
'LICENSE',&
'   Public Domain',&
'']
version_text=[ CHARACTER(LEN=128) :: &
'PRODUCT:        GPF (General Purpose Fortran) utilities and examples',&
'PROGRAM:        qsort(1f)',&
'DESCRIPTION:    sort lines in a file',&
'VERSION:        1.0, 2021-01-10',&
'AUTHOR:         John S. Urban',&
'HOME PAGE:      http://www.urbanjost.altervista.org/index.html',&
'']
end subroutine setup
end program qsort
