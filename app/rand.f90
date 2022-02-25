!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
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
'NAME                                                                                                                            ',&
'   rand - [FUNIX] generate pseudo-random permutations of file lines, whole                                                      ',&
'   numbers, or strings                                                                                                          ',&
'   (LICENSE:PD)                                                                                                                 ',&
'                                                                                                                                ',&
'SYNOPSIS                                                                                                                        ',&
'  syntax:                                                                                                                       ',&
'                                                                                                                                ',&
'   rand FILES(s)   [ -n]                                                                                                        ',&
'   rand STRINGS -e [ -n]                                                                                                        ',&
'   rand -i LO-HI   [ -n]                                                                                                        ',&
'                                                                                                                                ',&
'DESCRIPTION                                                                                                                     ',&
'   Generates pseudo=random permutations, similar to the shuf(1) command.                                                        ',&
'   Writes pseudo-random permutations of:                                                                                        ',&
'                                                                                                                                ',&
'   o the lines in a file                                                                                                        ',&
'   o a range of whole numbers                                                                                                   ',&
'   o a list of strings                                                                                                          ',&
'                                                                                                                                ',&
'OPTIONS                                                                                                                         ',&
'   FILES(s)   files to use as input                                                                                             ',&
'   -e         treat each ARG as an input line                                                                                   ',&
'   -i LO HI   treat each number LO through HI as an input line                                                                  ',&
'   -n         output at most COUNT lines (per file)                                                                             ',&
'   --help     display this help and exit                                                                                        ',&
'   --version  output version information and exit                                                                               ',&
'                                                                                                                                ',&
'EXAMPLES                                                                                                                        ',&
'  Sample usage:                                                                                                                 ',&
'                                                                                                                                ',&
'   # generate a random number from 0 to 100                                                                                     ',&
'   rand -i 0 100 -n 1                                                                                                           ',&
'   # randomly select xterm(1) color                                                                                             ',&
'   xterm -bg `rand -e green black gray blue -n 1`                                                                               ',&
'                                                                                                                                ',&
'AUTHOR                                                                                                                          ',&
'   John S. Urban                                                                                                                ',&
'                                                                                                                                ',&
'LICENSE                                                                                                                         ',&
'   Public Domain                                                                                                                ',&
'                                                                                                                                ',&
'']
   WRITE(*,'(a)')(trim(help_text(i)),i=1,size(help_text))
   stop ! if --help was specified, stop
endif
end subroutine help_usage
!>
!!##NAME
!!    rand - [FUNIX] generate pseudo-random permutations of file lines, whole
!!    numbers, or strings
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!   syntax:
!!
!!    rand FILES(s)   [ -n]
!!    rand STRINGS -e [ -n]
!!    rand -i LO-HI   [ -n]
!!
!!##DESCRIPTION
!!    Generates pseudo=random permutations, similar to the shuf(1) command.
!!    Writes pseudo-random permutations of:
!!
!!    o the lines in a file
!!    o a range of whole numbers
!!    o a list of strings
!!
!!##OPTIONS
!!    FILES(s)   files to use as input
!!    -e         treat each ARG as an input line
!!    -i LO HI   treat each number LO through HI as an input line
!!    -n         output at most COUNT lines (per file)
!!    --help     display this help and exit
!!    --version  output version information and exit
!!
!!##EXAMPLES
!!
!!   Sample usage:
!!
!!    # generate a random number from 0 to 100
!!    rand -i 0 100 -n 1
!!    # randomly select xterm(1) color
!!    xterm -bg `rand -e green black gray blue -n 1`
!!
!!##AUTHOR
!!    John S. Urban
!!
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
'@(#)PROGRAM:        rand(1)>',&
'@(#)DESCRIPTION:    random shuffle of lines in a file or strings or a range of whole numbers>',&
'@(#)VERSION:        2.0, 2022-01-08>',&
'@(#)AUTHOR:         John S. Urban>',&
'@(#)LICENSE:        Public Domain. This is free software: you are free to change and redistribute it.>',&
'@(#)                There is NO WARRANTY, to the extent permitted by law.>',&
'@(#)COMPILED:       2022-01-09 23:07:44 UTC-300>',&
'']
   WRITE(*,'(a)')(trim(help_text(i)(5:len_trim(help_text(i))-1)),i=1,size(help_text))
   stop ! if --version was specified, stop
endif
end subroutine help_version
program rand
use M_io,      only : swallow
use M_strings, only : notabs
use M_random,  only : scramble, init_random_seed_by_system_clock
use M_verify,   only : stderr
use M_kracken, only : kracken, lget, sgets, iget, igets             ! add command-line parser module
implicit none
character(len=4096),allocatable  :: FILENAMES(:)
character(len=:),allocatable     :: pageout(:) ! array to hold file in memory
integer,allocatable              :: indx(:)
integer                          :: i,j,k,n
integer,allocatable              :: vals(:)
!-----------------------------------------------------------------------------------------------------------------------------------
   call kracken('rand','-help .F. -version .f. -e .F. -n -1 -i')    ! define command arguments,default values and crack command line
   call help_usage(lget('rand_help'))                               ! if -help option is present, display help text and exit
   call help_version(lget('rand_version'))                          ! if -version option is present, display version text and exit
   FILENAMES=sgets('rand_oo')
   k=iget('rand_n')
   vals=igets('rand_i')
   call init_random_seed_by_system_clock()
!-----------------------------------------------------------------------------------------------------------------------------------
DONE : block
!-----------------------------------------------------------------------------------------------------------------------------------
   if(lget('rand_e'))then
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
   do j=1,size(FILENAMES)
      ! allocate character array and copy file into it
      if(FILENAMES(j).eq.'-')then
         call swallow(5,pageout)
      else
         call swallow(FILENAMES(j),pageout)
      endif
      if(.not.allocated(pageout))then
         call stderr('*rand* failed to load file ',FILENAMES(j))
      else
         n=size(pageout)
         indx=scramble(n)
         n=merge(min(k,n),n,k.ge.0)
         write(*,'(a)')(trim(pageout(indx(i))),i=1,n)
         deallocate(pageout)  ! release memory
      endif
   enddo
endblock DONE
end program rand
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
