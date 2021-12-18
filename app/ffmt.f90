!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
program demo_ffmt
use M_kracken, only : kracken, lget, sget, iget                  ! add command-line parser module
use M_io,      only : read_line
use M_strings, only : fmt, indent
implicit none
! ident_1="@(#)ffmt(1f): simple text formatter for Fortran comments"
character(len=:),allocatable :: line
character(len=:),allocatable :: bigline
integer                      :: iline
integer                      :: width
integer                      :: step
integer                      :: step_before
character(len=20)            :: style
character(len=1)             :: first
!-----------------------------------------------------------------------------------------------------------------------------------
   call kracken('ffmt','-help .f. -version .f. -w 75 ') ! define command arguments,default values and crack command line
   call help_usage(lget('ffmt_help'))                   ! if -help option is present, display help text and exit
   call help_version(lget('ffmt_version'))              ! if -version option is present, display version text and exit
!-----------------------------------------------------------------------------------------------------------------------------------
   width = iget('ffmt_w')
!-----------------------------------------------------------------------------------------------------------------------------------
   step=0
   iline=0
   bigline=''
   style='("!",a)'
   INFINITE: do while (read_line(line)==0)
      iline=iline+1
      if(len(line).gt.0)then
         first=line(1:1)
      else
         first=''
      endif
      if(first.eq.'!')line(1:1)=' '
      step_before=step            ! indent of previous line
      step=indent(line)           ! indent of line just read
      if(iline.eq.1)then
         step_before=step         ! prevent first line from meeting criteria to output a paragraph
         call makeformat()
      endif
      if(first.ne.'!')then
         if(bigline.ne.'')then
            write(*,fmt=style) fmt(bigline,width)
         endif
         write(*,'(a)')line
         bigline=''
         style='("!",a)'
      elseif(line.eq.'')then ! hit blank line so output previous paragraph
         if(bigline.ne.'')then
            write(*,fmt=style) fmt(bigline,width)
         endif
         bigline=''
         style='("!",a)'
         write(*,'("!")')
      elseif(step.ne.step_before)then ! hit new indent so output previous paragraph
         if(bigline.ne.'')then
            write(*,fmt=style) fmt(bigline,width)
         endif
         bigline=line
         call makeformat()
      else
         bigline=bigline//' '//line
      endif
   enddo INFINITE
   if(bigline.ne.'')then
      write(*,style) fmt(bigline,width)
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
contains
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine makeformat
   if(step.ge.2)then                      ! generate format statement
      write(style,'("(""!""",i0,"x,a)")')step-1
   else
      style='("!",a)'
   endif
end subroutine makeformat
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
'@(#)PROGRAM:        ffmt(1)>',&
'@(#)DESCRIPTION:    simple reformatting of paragraphs of Fortran comments>',&
'@(#)VERSION:        1.0, 20190421>',&
'@(#)AUTHOR:         John S. Urban>',&
'@(#)REPORTING BUGS: http://www.urbanjost.altervista.org/>',&
'@(#)HOME PAGE:      http://www.urbanjost.altervista.org/index.html>',&
'@(#)LICENSE:        Public Domain>',&
'@(#)COMPILED:       2021-12-18 15:27:20 UTC-300>',&
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
'NAME                                                                                                                            ',&
'       ffmt(1f) - [FILE EDIT] simple text formatter for Fortran comments                                                        ',&
'       (LICENSE:PD)                                                                                                             ',&
'                                                                                                                                ',&
'SYNOPSIS                                                                                                                        ',&
'       ffmt [OPTION]...                                                                                                         ',&
'                                                                                                                                ',&
'DESCRIPTION                                                                                                                     ',&
'   Ignoring lines not beginning with an exclamation, trim the leading                                                           ',&
'   exclamation and then reformat each paragraph on standard input,                                                              ',&
'   prefixing the output with an exclamation.                                                                                    ',&
'                                                                                                                                ',&
'   A paragraph ends when a blank line is encountered or the left margin                                                         ',&
'   changes.                                                                                                                     ',&
'                                                                                                                                ',&
'OPTIONS                                                                                                                         ',&
'       -w, WIDTH               maximum line width (default of 75 columns)                                                       ',&
'       --help                  display this help and exit                                                                       ',&
'       --version               output version information and exit                                                              ',&
'AUTHOR                                                                                                                          ',&
'   John S. Urban                                                                                                                ',&
'LICENSE                                                                                                                         ',&
'   Public Domain                                                                                                                ',&
'']
   WRITE(*,'(a)')(trim(help_text(i)),i=1,size(help_text))
   stop ! if --help was specified, stop
endif
end subroutine help_usage
!>
!!##NAME
!!        ffmt(1f) - [FILE EDIT] simple text formatter for Fortran comments
!!        (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!        ffmt [OPTION]...
!!
!!##DESCRIPTION
!!    Ignoring lines not beginning with an exclamation, trim the leading
!!    exclamation and then reformat each paragraph on standard input,
!!    prefixing the output with an exclamation.
!!
!!    A paragraph ends when a blank line is encountered or the left margin
!!    changes.
!!
!!##OPTIONS
!!        -w, WIDTH               maximum line width (default of 75 columns)
!!        --help                  display this help and exit
!!        --version               output version information and exit
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
end program demo_ffmt
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
