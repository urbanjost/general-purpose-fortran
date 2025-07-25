!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
program shuf
use M_io,        only : fileread
use M_strings,   only : notabs
use M_random,    only : scramble, init_random_seed_by_system_clock
use M_framework, only : stderr
use M_kracken,   only : kracken, lget, sgets, iget, igets             ! add command-line parser module
implicit none
character(len=4096),allocatable  :: FILENAMES(:)
character(len=:),allocatable     :: pageout(:) ! array to hold file in memory
integer,allocatable              :: indx(:)
integer                          :: i,j,k,n
integer,allocatable              :: vals(:)
!-----------------------------------------------------------------------------------------------------------------------------------
   call kracken('shuf','-help .F. -version .f. -e .F. -n -1 -i')    ! define command arguments,default values and crack command line
   call help_usage(lget('shuf_help'))                               ! if -help option is present, display help text and exit
   call help_version(lget('shuf_version'))                          ! if -version option is present, display version text and exit
   FILENAMES=sgets('shuf_oo')
   k=iget('shuf_n')
   vals=igets('shuf_i')
   call init_random_seed_by_system_clock()
!-----------------------------------------------------------------------------------------------------------------------------------
DONE : block
!-----------------------------------------------------------------------------------------------------------------------------------
   if(lget('shuf_e'))then
      n=size(FILENAMES)
      indx=scramble(n)
      n=merge(min(k,n),n,k.ge.0)
      write(*,'(a)')(trim(FILENAMES(indx(i))),i=1,n)
      exit DONE
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   if(size(vals).eq.2)then
      indx=scramble(abs(vals(2)-vals(1))+1)+min(vals(1),vals(2))-1
      n=merge(min(k,size(indx)),size(indx),k.ge.0)
      write(*,'(i0)')(indx(i),i=1,n)
      exit DONE
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   if(size(FILENAMES)==0)FILENAMES=['-']
   do j=1,size(FILENAMES)
      ! allocate character array and copy file into it
      if(FILENAMES(j).eq.'-')then
         call fileread(5,pageout)
      else
         call fileread(FILENAMES(j),pageout)
      endif
      if(.not.allocated(pageout))then
         call stderr('*shuf* failed to load file ',FILENAMES(j))
      else
         n=size(pageout)
         indx=scramble(n)
         n=merge(min(k,n),n,k.ge.0)
         write(*,'(a)')(trim(pageout(indx(i))),i=1,n)
         deallocate(pageout)  ! release memory
      endif
   enddo
endblock DONE
contains
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
'   shuf- - [FUNIX] generate a random permutation of file lines, whole           ',&
'   numbers, or strings                                                          ',&
'   (LICENSE:PD)                                                                 ',&
'SYNOPSIS                                                                        ',&
'  syntax:                                                                       ',&
'                                                                                ',&
'   shuf- FILES(s)   [ -n]                                                       ',&
'   shuf- STRINGS -e [ -n]                                                       ',&
'   shuf- -i LO-HI   [ -n]                                                       ',&
'                                                                                ',&
'DESCRIPTION                                                                     ',&
'   Write a random permutation of the input lines to standard output.            ',&
'                                                                                ',&
'   FILES(s)   files to use as input                                             ',&
'   -e         treat each ARG as an input line                                   ',&
'   -i LO HI   treat each number LO through HI as an input line                  ',&
'   -n         output at most COUNT lines (per file)                             ',&
'   --help     display this help and exit                                        ',&
'   --version  output version information and exit                               ',&
'EXAMPLES                                                                        ',&
'  Sample usage:                                                                 ',&
'                                                                                ',&
'   # generate a random number from 0 to 100                                     ',&
'   shuf- -i 0 100 -n 1                                                          ',&
'   # randomly select xterm(1) color                                             ',&
'   xterm -bg `shuf- -e green black gray blue -n 1`                              ',&
'   # output up to five randomly selected lines from a file                      ',&
'   shuf -n 5 FILE                                                               ',&
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
!!    shuf- - [FUNIX] generate a random permutation of file lines, whole
!!    numbers, or strings
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!   syntax:
!!
!!    shuf- FILES(s)   [ -n]
!!    shuf- STRINGS -e [ -n]
!!    shuf- -i LO-HI   [ -n]
!!
!!##DESCRIPTION
!!    Write a random permutation of the input lines to standard output.
!!
!!    FILES(s)   files to use as input
!!    -e         treat each ARG as an input line
!!    -i LO HI   treat each number LO through HI as an input line
!!    -n         output at most COUNT lines (per file)
!!    --help     display this help and exit
!!    --version  output version information and exit
!!##EXAMPLES
!!
!!   Sample usage:
!!
!!    # generate a random number from 0 to 100
!!    shuf- -i 0 100 -n 1
!!    # randomly select xterm(1) color
!!    xterm -bg `shuf- -e green black gray blue -n 1`
!!    # output up to five randomly selected lines from a file
!!    shuf -n 5 FILE
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
'@(#)PROGRAM:        shuf-(1)>',&
'@(#)DESCRIPTION:    random shuffle of lines in a file or strings or a range of whole numbers>',&
'@(#)VERSION:        2.0, 2022-01-08>',&
'@(#)AUTHOR:         John S. Urban>',&
'@(#)LICENSE:        Public Domain. This is free software: you are free to change and redistribute it.>',&
'@(#)                There is NO WARRANTY, to the extent permitted by law.>',&
'@(#)COMPILED:       2025-06-29 08:18:17 UTC-240>',&
'']
   WRITE(*,'(a)')(trim(help_text(i)(5:len_trim(help_text(i))-1)),i=1,size(help_text))
   stop ! if --version was specified, stop
endif
end subroutine help_version
end program shuf
!         call date_and_time(values=values)                               ! jump through hoops to get a random number from 0 to 1
!         call random_seed(size=n)
!         allocate(seed(1:n))
!         seed(:) = values(8)
!         call random_seed(put=seed)
!         call random_number(chance)
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
