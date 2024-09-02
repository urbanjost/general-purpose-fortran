!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
program seq
use M_kracken, only : kracken, lget, sget, dgets, parse             ! add command-line parser module
use M_framework,   only : stderr
implicit none
character(len=:),allocatable :: fmt
integer                      :: ios
integer                      :: i
doubleprecision              :: start, increment, last
doubleprecision,allocatable  :: vals(:)
doubleprecision              :: value
character(len=1024)          :: message
!-----------------------------------------------------------------------------------------------------------------------------------
   call parse('MODE','LEAVEQUOTES','YES')                           ! leave double-quotes alone now that past parsing command line
   call kracken('seq','-help .F. -version .f. -f (*(g0,/))')        ! define command arguments,default values and crack command line
!-----------------------------------------------------------------------------------------------------------------------------------
   call help_usage(lget('seq_help'))                                ! if -help option is present, display help text and exit
   call help_version(lget('seq_version'))                           ! if -version option is present, display version text and exit
!-----------------------------------------------------------------------------------------------------------------------------------
   vals = dgets('seq_oo')                                           ! get -oo START INCREMENT LAST
   start=1.0d0
   increment=1.0d0
   last=0.0d0
   select case(size(vals))
    case(1); start=vals(1)
    case(2); start=vals(1); increment=1.0d0  ; last=vals(2)
    case(3); start=vals(1); increment=vals(2); last=vals(3)
    case default
      call stderr('*seq-* ERROR: incorrect number of values=',size(vals))
      call help_usage(.true.)
   end select
!-----------------------------------------------------------------------------------------------------------------------------------
   fmt=sget('seq_f')                                                ! get -f
!-----------------------------------------------------------------------------------------------------------------------------------
   DONE : block
      if(all(vals.eq.int(vals)))then  ! if all values are INTEGER
         do i=int(start),int(last),int(increment)
            write(*,fmt,iostat=ios,iomsg=message,advance='no')i
            if(ios.ne.0)then
               write(*,'(*(a))')'*seq-* ERROR: ',message
               exit
            endif
         enddo
      else
         if(increment.eq.0) exit DONE                    ! an increment of zero would cause an infinite loop
         if(start.le.last.and.increment.lt.0) exit DONE  ! incrementing away from the last value would cause an infinite loop
         if(start.gt.last.and.increment.gt.0) exit DONE  ! incrementing away from the first and last value would cause infinite loop
         value=start
         do
            if(value.gt.last)exit
            write(*,fmt,iostat=ios,iomsg=message,advance='no')value
            if(ios.ne.0)then
               write(*,'(*(a))')'*seq-* ERROR: ',message
               exit
            endif
            value=value+increment
         enddo
      endif
   endblock DONE
!-----------------------------------------------------------------------------------------------------------------------------------
contains
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
'@(#)PROGRAM:        seq-(1)>',&
'@(#)DESCRIPTION:    print a sequence of numbers>',&
'@(#)VERSION:        1.0, 2019-09-04>',&
'@(#)AUTHOR:         John S. Urban>',&
'@(#)LICENSE:        Public Domain. This is free software: you are free to change and redistribute it.>',&
'@(#)                There is NO WARRANTY, to the extent permitted by law.>',&
'@(#)COMPILED:       2024-06-29 21:52:17 UTC-240>',&
'']
   WRITE(*,'(a)')(trim(help_text(i)(5:len_trim(help_text(i))-1)),i=1,size(help_text))
   stop ! if --version was specified, stop
endif
end subroutine help_version
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
'   seq- - [FUNIX] print a sequence of numbers                                   ',&
'   (LICENSE:PD)                                                                 ',&
'                                                                                ',&
'SYNOPSIS                                                                        ',&
'   seq- LAST [OPTION] ...                                                       ',&
'   seq- FIRST LAST [OPTION] ...                                                 ',&
'   seq- FIRST INCREMENT LAST [OPTION] ...                                       ',&
'                                                                                ',&
'DESCRIPTION                                                                     ',&
'   Print numbers from FIRST to LAST, in steps of INCREMENT.                     ',&
'                                                                                ',&
'OPTIONS                                                                         ',&
'   FIRST,LAST,INCREMENT     The values have the following restrictions          ',&
'                            o If FIRST or INCREMENT is omitted, they            ',&
'                              will default to 1.                                ',&
'                            o The sequence ends when the sum of the             ',&
'                              current number and INCREMENT would become         ',&
'                              greater than LAST.                                ',&
'                            o FIRST, INCREMENT, and LAST are interpreted        ',&
'                              as floating point values.                         ',&
'                            o INCREMENT must be positive if FIRST is            ',&
'                              smaller than LAST or no output is produced.       ',&
'                            o INCREMENT must be negative if FIRST is            ',&
'                              greater than LAST or no output is produced.       ',&
'                            o INCREMENT must not be 0.                          ',&
'                            o none of FIRST, INCREMENT and LAST may be NaN.     ',&
'   -f  FORMAT               The Fortran FORMAT used to print the values.        ',&
'                            Use a floating-point Fortran FORMAT for printing    ',&
'                            values unless FIRST, LAST, and INCREMENT are all    ',&
'                            whole numbers. Then the format must be suitable for ',&
'                            printing one argument of type INTEGER.              ',&
'                            The format defaults to "(g0,/)"                     ',&
'   --help                   display this help and exit                          ',&
'   --version                output version information and exit                 ',&
'                                                                                ',&
'EXAMPLE                                                                         ',&
'  sample usage                                                                  ',&
'                                                                                ',&
'   seq- 1 1 10 -f ''(i0)''                                                      ',&
'   12345678910                                                                  ',&
'                                                                                ',&
'   seq- 1 1 3                                                                   ',&
'   1                                                                            ',&
'   2                                                                            ',&
'   3                                                                            ',&
'                                                                                ',&
'   seq- 1 -1 -10 -f ''(i0.3,":")''                                              ',&
'   001:000:-001:-002:-003:-004:-005:-006:-007:-008:-009:-010:                   ',&
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
!!    seq- - [FUNIX] print a sequence of numbers
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    seq- LAST [OPTION] ...
!!    seq- FIRST LAST [OPTION] ...
!!    seq- FIRST INCREMENT LAST [OPTION] ...
!!
!!##DESCRIPTION
!!    Print numbers from FIRST to LAST, in steps of INCREMENT.
!!
!!##OPTIONS
!!    FIRST,LAST,INCREMENT     The values have the following restrictions
!!                             o If FIRST or INCREMENT is omitted, they
!!                               will default to 1.
!!                             o The sequence ends when the sum of the
!!                               current number and INCREMENT would become
!!                               greater than LAST.
!!                             o FIRST, INCREMENT, and LAST are interpreted
!!                               as floating point values.
!!                             o INCREMENT must be positive if FIRST is
!!                               smaller than LAST or no output is produced.
!!                             o INCREMENT must be negative if FIRST is
!!                               greater than LAST or no output is produced.
!!                             o INCREMENT must not be 0.
!!                             o none of FIRST, INCREMENT and LAST may be NaN.
!!    -f  FORMAT               The Fortran FORMAT used to print the values.
!!                             Use a floating-point Fortran FORMAT for printing
!!                             values unless FIRST, LAST, and INCREMENT are all
!!                             whole numbers. Then the format must be suitable for
!!                             printing one argument of type INTEGER.
!!                             The format defaults to "(g0,/)"
!!    --help                   display this help and exit
!!    --version                output version information and exit
!!
!!##EXAMPLE
!!
!!   sample usage
!!
!!    seq- 1 1 10 -f '(i0)'
!!    12345678910
!!
!!    seq- 1 1 3
!!    1
!!    2
!!    3
!!
!!    seq- 1 -1 -10 -f '(i0.3,":")'
!!    001:000:-001:-002:-003:-004:-005:-006:-007:-008:-009:-010:
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
end program seq
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
