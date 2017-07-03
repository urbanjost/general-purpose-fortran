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
'   table2html(1f) - [FILE FILTER]filter to format simple columns into an HTML table form',&
'                                                                                ',&
'SYNOPSIS                                                                        ',&
'   table2html [[-classes classnames] [-caption caption line] [-delimiters delim]',&
'              [-asis] [-header] [-tabletags tags] ] |-help|-version             ',&
'                                                                                ',&
'DESCRIPTION                                                                     ',&
'   table2html is a filter that converts tabular text input to an HTML           ',&
'   table. The program can create an HTML table displaying alternating           ',&
'   hues per row.                                                                ',&
'                                                                                ',&
'   The "-classes" option generates code using cascading style sheet             ',&
'   (CSS) class names for alternating rows; default names are "even" and         ',&
'   "odd". The following style definition in a page''s <head> section will       ',&
'   assign different background hues to be used:                                 ',&
'                                                                                ',&
'      <style type="text/css">                                                   ',&
'         .even { background-color: #FFFFFF; }                                   ',&
'         .odd  { background-color: #F0F0F0; }                                   ',&
'      </style>                                                                  ',&
'                                                                                ',&
'OPTIONS                                                                         ',&
'    -caption     a caption line for the table                                   ',&
'    -classes     use CSS class names for different rows (empty argument: odd even)',&
'    -delimiters  input column delimiter (default: whitespace). If a list contains space',&
'                 the space must not be last in the list.                        ',&
'    -header      the first row is assumed to be a header line (default: no header lines)',&
'    -tabletags   specify own HTML table tags instead of the default: border="1" ',&
'    -document    write complete HTML document instead of just table             ',&
'    -asis        by default the characters ><& are converted to their HTML equivalents.',&
'                 This option turns that conversion off.                         ',&
'    -help        display command help and exit                                  ',&
'    -version     display command metadata and exit                              ',&
'EXAMPLES                                                                        ',&
'  Sample usage:                                                                 ',&
'                                                                                ',&
'   # list month as a table                                                      ',&
'   cal|tail -n +2|table2html -caption $(cal|head -n 1) -document |w3m -dump -T text/html|more',&
'']
   WRITE(*,'(a)')(trim(help_text(i)),i=1,size(help_text))
   stop ! if -help was specified, stop
endif
end subroutine help_usage
!-----------------------------------------------------------------------------------------------------------------------------------
!>
!!##NAME
!!    table2html(1f) - [FILE FILTER]filter to format simple columns into an HTML table form
!!
!!##SYNOPSIS
!!
!!    table2html [[-classes classnames] [-caption caption line] [-delimiters delim]
!!               [-asis] [-header] [-tabletags tags] ] |-help|-version
!!
!!##DESCRIPTION
!!    table2html is a filter that converts tabular text input to an HTML
!!    table. The program can create an HTML table displaying alternating
!!    hues per row.
!!
!!    The "-classes" option generates code using cascading style sheet
!!    (CSS) class names for alternating rows; default names are "even" and
!!    "odd". The following style definition in a page's <head> section will
!!    assign different background hues to be used:
!!
!!       <style type="text/css">
!!          .even { background-color: #FFFFFF; }
!!          .odd  { background-color: #F0F0F0; }
!!       </style>
!!
!!##OPTIONS
!!     -caption     a caption line for the table
!!     -classes     use CSS class names for different rows (empty argument: odd even)
!!     -delimiters  input column delimiter (default: whitespace). If a list contains space
!!                  the space must not be last in the list.
!!     -header      the first row is assumed to be a header line (default: no header lines)
!!     -tabletags   specify own HTML table tags instead of the default: border="1"
!!     -document    write complete HTML document instead of just table
!!     -asis        by default the characters ><& are converted to their HTML equivalents.
!!                  This option turns that conversion off.
!!     -help        display command help and exit
!!     -version     display command metadata and exit
!!##EXAMPLES
!!
!!   Sample usage:
!!
!!    # list month as a table
!!    cal|tail -n +2|table2html -caption $(cal|head -n 1) -document |w3m -dump -T text/html|more
!===================================================================================================================================
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
'@(#)PRODUCT:        CLI library utilities and examples>',&
'@(#)PROGRAM:        table2html(1f)>',&
'@(#)DESCRIPTION:    filter to format simple columnar table into an HTML table form>',&
'@(#)VERSION:        1.0, 20170123>',&
'@(#)AUTHOR:         John S. Urban>',&
'@(#)REPORTING BUGS: http://www.urbanjost.altervista.org/>',&
'@(#)HOME PAGE:      http://www.urbanjost.altervista.org/index.html>',&
'@(#)LICENSE:        Public Domain. This is free software: you are free to change and redistribute it.>',&
'@(#)                There is NO WARRANTY, to the extent permitted by law.>',&
'@(#)COMPILED:       Wed, Jun 14th, 2017 10:06:13 AM>',&
'']
   WRITE(*,'(a)')(trim(help_text(i)(5:len_trim(help_text(i))-1)),i=1,size(help_text))
   stop ! if -version was specified, stop
endif
end subroutine help_version
!-----------------------------------------------------------------------------------------------------------------------------------
program table2html
use M_kracken, only : kracken, iget, sgets, sget, lget, IPvalue
use M_strings, only : substitute, split
implicit none
character(len=*),parameter::ident="@(#)table2html(1f): filter to format input into an HTML table form"

character(len=IPvalue)             :: line
character(len=IPvalue),allocatable :: listof_classes(:)
integer                            :: size_of_class
character(len=:),allocatable       :: delimiters             ! characters used to delimit columns
integer                            :: j=0                    ! line count not counting header, assumed to not overflow
logical                            :: asis=.false.
!===================================================================================================================================

                            ! define command options and default values and then process command-line arguments
call kracken('cmd','        &
& -document .F.             &
& -classes odd even         &
& -caption                  &
& -delimiters               &
& -header .F.               &
& -asis .F.                 &
& -help .F.                 &
& -version .F.              &
& -tabletags border=""1""')

call help_usage(lget('cmd_help'))                        ! if -help option is present, display help text and exit
call help_version(lget('cmd_version'))                   ! if -version option is present, display version text and exit

asis=lget('cmd_asis')

if(lget('cmd_document'))then
   write(*,'("<html>")')
   write(*,'("<head>")')
   write(*,'("<title> ",a," </title>")')trim(sget('cmd_caption'))
   write(*,'("<style>")')
   write(*,'(".even { background-color: #FFFFFF; }")')
   write(*,'(".odd  { background-color: #F0F0F0; }")')
   write(*,'("</style>")')
   write(*,'("</head>")')
   write(*,'("<body>")')
endif

delimiters=trim(sget('cmd_delimiters'))                  ! get -delimiters values

write(*,'("<table ",a,">")') trim(sget('cmd_tabletags')) ! start table with specified tags

if(sget('cmd_caption').ne.'')then                        ! process -caption option
   write(*,'("<caption><em>",a,"</em></caption>")') trim(sget('cmd_caption'))
endif

listof_classes=sgets('cmd_classes')                      ! process -classes (either not present, just switch name, or user values)
if(size(listof_classes).eq.0)then                        ! -classes with no values
   listof_classes= [ 'odd ', 'even' ]
endif

size_of_class=0
if(lget('cmd_header'))then                               ! treat first line as a header line
   write(*,'("<thead>")')
   read(*,'(a)',end=999,err=999) line
   call writeline()
   write(*,'("</thead>")')
endif

size_of_class=size(listof_classes)
write(*,'("<tbody>")')                                   ! create main body of table

INFINITE: do
   read(*,'(a)',end=999,err=999) line
   if(len_trim(line).eq.0)cycle INFINITE
   j=j+1
   call writeline()
enddo INFINITE

write(*,'("</tbody>")')

999 continue

write(*,'("</table>")')
if(lget('cmd_document'))then
   write(*,'("</body>")')
   write(*,'("</html>")')
endif
!===================================================================================================================================
contains
!===================================================================================================================================
subroutine writeline()
!
! given a line split it into columns in array(:) and create a table row like this
! <tr>
!   <td class="xxxxxx">array(1)</td>
!   <td class="xxxxxx">array(2)</td>
!   <td class="xxxxxx">array(2)</td>
!      :
!      :
! </tr>
! assuming the list of classes is not empty
!
character(len=4096),allocatable :: array(:)
integer                         :: classi
integer                         :: i,isize
integer,save                    :: items=0 ! man number in a line assuming called once per program execution

if(j.ne.0)then
   classi=mod(j+size_of_class-1,size_of_class)+1
else
   classi=1
endif

call split(line,array,delimiters)                                              ! split line into columns
isize=size(array)
if(isize.eq.0)return
items=max(items,isize)

write(*,'("<tr><!-- line ",i0," -->")')j                                       ! start a row

do i=1,size(array)
   if(.not.asis)then
      call substitute(array(i), '&', '&amp;')
      call substitute(array(i), '<', '&lt;')                                   ! convert characters to HTML equivalents
      call substitute(array(i), '>', '&gt;')
   endif
   if(size_of_class.gt.0)then                                                  ! start a column
      write(*,'(" <td class=""",a,""" >")',advance='no')trim(listof_classes(classi))
   else
      write(*,'(" <td>")',advance='no')
   endif
   if(array(i).eq.' ')then                                                     ! print user data for a column
      write(*,'(1x," &nbsp; </td>")')
   else
      write(*,'(1x,a," </td>")')trim(array(i))
   endif
enddo

do i=size(array),items-1
   write(*,'(" <td> &nbsp; </td>")')
enddo

write(*,'("</tr>")')
end subroutine writeline
!===================================================================================================================================
end program table2html
!===================================================================================================================================
