!-----------------------------------------------------------------------------------------------------------------------------------
!<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>-
!-----------------------------------------------------------------------------------------------------------------------------------
program fifo
   use M_kracken, only : kracken, lget, sget, iget                  ! add command-line parser module
   use M_fixedform
   implicit none
   character(len=:),allocatable :: filename
   character(len=:),allocatable :: answers
   logical                      :: loop, once=.true.
   integer,parameter            :: io=10
!  define command arguments, default values and crack command line
   call kracken('fixedform','-f -loop .f. -help .f. -version .f. -demo "#N#"')
   call help_usage(lget('fixedform_help'))                                ! if -help option is present, display help text and exit
   call help_version(lget('fixedform_version'))                           ! if -version option is present, display version text and exit
!  get commandline values.
   loop=lget('fixedform_loop')
   filename=trim(sget('fixedform_oo'))//trim(sget('fixedform_f')) ! Allow filename as -oo or -f
!  all done parsing; do something with the values
   page_ptr=>page_pd
   icount_ptr=>icount_pd
   if(filename.eq.'')then
      open(io,file='fixedform_demo.tui')
write(io,'(a)')'  ######################################################################'
write(io,'(a)')'  # FIXEDFORM: Rapidly create easy-to-use TUI interfaces from Fortran. #'
write(io,'(a)')'  ######################################################################'
write(io,'(a)')'@SUMMARY:@'
write(io,'(a)')''
write(io,'(a)')' If your program input could be specified on type-written forms with'
write(io,'(a)')' instructions, it should be as easy to create that interface on a computer'
write(io,'(a)')' as it would be to create those forms on paper.'
write(io,'(a)')''
write(io,'(a)')' That is what FIXEDFORM allows you to do. The manpage for M_fixedform(3fm)'
write(io,'(a)')' contains example programs.'
write(io,'(a)')''
write(io,'(a)')'@DESCRIPTION:@'
write(io,'(a)')''
write(io,'(a)')' FIXEDFORM  provides  for  quickly  creating  simple  TUIs (Terminal-based'
write(io,'(a)')' User Interfaces).  Complex  radio  buttons  are  supported as well as'
write(io,'(a)')' text input fields and menu buttons.'
write(io,'(a)')''
write(io,'(a)')' FIXEFORM is appropriate for tasks such as interactively editing command'
write(io,'(a)')' line  arguments  to  a  command and  simple  data-input forms.  It also'
write(io,'(a)')' allows for easy construction and access to basic  instructions for'
write(io,'(a)')' supplying input  values.'
write(io,'(a)')''
write(io,'(a)')' The FIXEDFORM user interface is intuitive and consistent, especially'
write(io,'(a)')' for  Unix and  GNU/Linux  users  familar  with  utilities  such  as'
write(io,'(a)')' the  vim(1) editor.'
write(io,'(a)')''
write(io,'(a)')' FIXEDFORM  can  be  called as a utility from practically any language,'
write(io,'(a)')' or as a subroutine from Fortran.'
write(io,'(a)')''
write(io,'(a)')'@PRINTED OUTPUT@'
write(io,'(a)')''
write(io,'(a)')' Snapshots of the formatted  input  forms  can  be  generated  as  simple  HTML'
write(io,'(a)')' documents using built-in functions. Using ''ctrl-O'' will generate output files'
write(io,'(a)')' named "paperNNNN.html" where NNNN is an incremented whole number starting at'
write(io,'(a)')' "0000".'
write(io,'(a)')''
write(io,'(a)')'@REQUIREMENTS:@'
write(io,'(a)')''
write(io,'(a)')' FIXEDFORM is a terminal-based utility that requires'
write(io,'(a)')'  ^ the ncurses(3c) library'
write(io,'(a)')'  ^ a C compiler'
write(io,'(a)')'  ^ a Fortran compiler that supports the ISO_C_BINDING interface'
write(io,'(a)')''
write(io,'(a)')'@INDEX:@'
write(io,'(a)')''
write(io,'(a)')'    o PROGRAM INTERFACE'
write(io,'(a)')'       o CHARACTER ARRAY'
write(io,'(a)')'       o NAMELIST'
write(io,'(a)')'       o ENVIRONMENTAL VARIABLES'
write(io,'(a)')'       o CALCULATOR VARIABLES'
write(io,'(a)')'    o MAKING A FORM'
write(io,'(a)')'    o USING A FORM'
write(io,'(a)')''
write(io,'(a)')''
write(io,'(a)')'@PROGRAMMING INTERFACE@'
write(io,'(a)')' The  programming interface is especially convenient for Fortran programs'
write(io,'(a)')' to use, which otherwise  often  run  only  in  command-line-interface'
write(io,'(a)')' environments.  A  minimal  amount  of  time  is  required  to  begin'
write(io,'(a)')' creating applications that use it because the forms are easily created'
write(io,'(a)')' using a text editor.'
write(io,'(a)')''
write(io,'(a)')''
write(io,'(a)')'@    The simplest use of FIXEDFORM is when a text file is used to define a     @'
write(io,'(a)')'@    form to be generated much like it could be drawn on paper:                @'
write(io,'(a)')'################################################################################'
write(io,'(a)')'#                                                                              #'
write(io,'(a)')'#  ~ A basic form definition:         ~  ^ `ARED`                              #'
write(io,'(a)')'#  ~ o Underlines become input fields ~  ^ `PWHITE`                            #'
write(io,'(a)')'#  ~ o Up-carets become menu options  ~  ^ `DBLUE`                             #'
write(io,'(a)')'#  ~ o Pound characters define boxes  ~                                        #'
write(io,'(a)')'#  ~ o Text otherwise displays as-is  ~  Connected by pound characters or      #'
write(io,'(a)')'#  ~   for the most part.             ~  adjacent to one another, up-carets    #'
write(io,'(a)')'#  Name:  ___________________            form a radio button.                  #'
write(io,'(a)')'#  Date:  ___________________            #######################               #'
write(io,'(a)')'#  Value: ___________________            ^      ^       ^      ^               #'
write(io,'(a)')'#                                       EAST   WEST   NORTH  SOUTH             #'
write(io,'(a)')'#                                                                              #'
write(io,'(a)')'# When the cursor is over a menu item it is toggled by pressing the space bar. #'
write(io,'(a)')'# A tab character moves to the next selectable item. Typing in an input value  #'
write(io,'(a)')'# changes the value. When the form is complete use the ctrl-S keys to submit.  #'
write(io,'(a)')'################################################################################'
write(io,'(a)')' By  default  all  that  is  returned  when the ctrl-S key is entered is'
write(io,'(a)')' a  character array composed of all the input fields, reading from left'
write(io,'(a)')' to right first and then down the form.  A  selected  menu  item returns'
write(io,'(a)')' a  T character, otherwise it returns an F character. When using this'
write(io,'(a)')' mode with larger forms with more than one selection per line it becomes'
write(io,'(a)')' difficult  to ensure  which value  goes with  which  entry so you have to'
write(io,'(a)')' get more complicated. But for a small basic form that''s pretty much it.'
write(io,'(a)')''
write(io,'(a)')' ~SUMMARY OF BASIC RULES FOR CREATING A FORM:~'
write(io,'(a)')''
write(io,'(a)')'       @INPUT FIELDS@'
write(io,'(a)')'          ~ o Underlines become one-line input fields            ~'
write(io,'(a)')'          ~ o Up-carets become clickable menu options. When      ~'
write(io,'(a)')'          ~   adjacent to a single row or column of pound        ~'
write(io,'(a)')'          ~   characters they act as a radio button. The pound   ~'
write(io,'(a)')'          ~   characters will display as connecting lines.       ~'
write(io,'(a)')''
write(io,'(a)')'       @DISPLAY OPTIONS@'
write(io,'(a)')'          ~ o Adjacent pound characters define boxes             ~'
write(io,'(a)')'          ~   (Single pound characters display as a pound symbol)~'
write(io,'(a)')'          ~ o Text between tilde characters is in reverse-video  ~'
write(io,'(a)')'          ~ o Text between at characters has a green background  ~'
write(io,'(a)')'          ~ o Backslashes can be used for shadowing              ~'
write(io,'(a)')'          ~ o Other text displays as-is as white text on a black ~'
write(io,'(a)')'          ~   background.                                        ~'
write(io,'(a)')''
write(io,'(a)')'   ~######################~   `AA``BB``CC``DD``EE``FF``GG``HH`'
write(io,'(a)')'   ~#   January 2015     #~\  `II``JJ``KK``LL``MM``NN``OO``PP`'
write(io,'(a)')'   ~######################~\  `QQ``RR``SS``TT``UU``VV``WW``XX`'
write(io,'(a)')'   ~#Su#Mo#Tu#We#Th#Fr#Sa#~\  `YY``ZZ`'
write(io,'(a)')'   ~######################~\   grave pairs -- Letter after first grave sets'
write(io,'(a)')'   ~#  #  #  #  # 1# 2# 3#~\                  color pair'
write(io,'(a)')'   ~# 4# 5# 6# 7# 8# 9#10#~\'
write(io,'(a)')'   ~#11#12#13#14#15#16#17#~\'
write(io,'(a)')'   ~#18#19#20#21#22#23#24#~\'
write(io,'(a)')'   ~#25#26#27#28#29#30#31#~\   back-slash -- colored character cell for'
write(io,'(a)')'   ~######################~\                 shading'
write(io,'(a)')'    \\\\\\\\\\\\\\\\\\\\\\\\'
write(io,'(a)')''
write(io,'(a)')'################################################################################'
write(io,'(a)')'# @LIMITATIONS ON USING POUND CHARACTERS:@                                     #'
write(io,'(a)')'# Note that the lines defined by pound characters need separated by non-pounds #'
write(io,'(a)')'# to behave as described, and that only a single row or column should be used  #'
write(io,'(a)')'# to define a radio-button menu. The ABCDEF menu is NOT a radio button because #'
write(io,'(a)')'# it has "o" breaks in the boxes between the box and the menu options.         #'
write(io,'(a)')'#                                              ##############################  #'
write(io,'(a)')'#                                              # Pick all the true choices  #  #'
write(io,'(a)')'#                  ######################      o#####o#####o#####o#####o####o  #'
write(io,'(a)')'#   ^`ARED  `      ^      ^      ^      ^      #     #     #     #     #    #  #'
write(io,'(a)')'#   ^`PWHITE`     TUE    WED    THU    FRI     ^     ^     ^     ^     ^    ^  #'
write(io,'(a)')'#   ^`DBLUE `                                  A     B     C     D     E    F  #'
write(io,'(a)')'#                                                                              #'
write(io,'(a)')'#                                                                              #'
write(io,'(a)')'#  #^TOP          An up-caret makes a button. Connected with pound-characters  #'
write(io,'(a)')'#  #^MIDDLE       they act as a group of radio buttons.                        #'
write(io,'(a)')'#  #^BOTTOM                               ^^^^^^^^^^      ^####^####^####^     #'
write(io,'(a)')'#                   ^##^##^##^##^         0123456789      #  # #    # #  #     #'
write(io,'(a)')'#                   1  2  3  4  5                         #  # #    # #  #     #'
write(io,'(a)')'#                                                         ^##^ ^####^ ^##^     #'
write(io,'(a)')'#                                                                              #'
write(io,'(a)')'################################################################################'
write(io,'(a)')''
write(io,'(a)')' There is a fixed function key panel at the bottom and a fixed message line.'
write(io,'(a)')' The rest of the screen is a scrollable area reserved for the form.'
write(io,'(a)')''
write(io,'(a)')'@THE PREDEFINED BOXES:@'
write(io,'(a)')''
write(io,'(a)')' There are PgUp and PgDn, Home, Help, End and Enter boxes at the screen'
write(io,'(a)')' bottom.'
write(io,'(a)')''
write(io,'(a)')'  o The PgUp and PgDn boxes page thru large forms from top to bottom.'
write(io,'(a)')'  o The Home box puts the upper right corner of the form'
write(io,'(a)')'    in the upper right corner of the scrolling region.'
write(io,'(a)')'  o The Help box displays any help pages defined for the form.'
write(io,'(a)')'  o The End box ends the program.'
write(io,'(a)')'  o The Submit box submits the form data back to the calling program for'
write(io,'(a)')'    processing.'
write(io,'(a)')''
write(io,'(a)')'################################################################################'
write(io,'(a)')'@SCROLLING THE FORM REGION:@'
write(io,'(a)')''
write(io,'(a)')' If the terminal is big enough to display the entire form you don''t have'
write(io,'(a)')' to know how to scroll it (and at least for an xterm(1) window the program'
write(io,'(a)')' will adjust to the terminal window being resized). But if~the form does'
write(io,'(a)')' not fit in the terminal~you have many ways to scroll the visible subsection'
write(io,'(a)')' of the form....'
write(io,'(a)')''
write(io,'(a)')'  o The arrow keys do what you expect and move the cursor until they get to'
write(io,'(a)')'    the screen edge. Attempting to move past the screen edges scrolls the'
write(io,'(a)')'    form region.'
write(io,'(a)')''
write(io,'(a)')'  o The shifted arrow keys also cause screen scrolling.'
write(io,'(a)')''
write(io,'(a)')'  o In addition, the following ctrl-keys smooth-scroll the form display window:'
write(io,'(a)')''
write(io,'(a)')'      o PgUp or ctrl-B (Backward) -- 1 screen up'
write(io,'(a)')'      o PgDn or ctrl-F (Forward)  -- 1 screen down'
write(io,'(a)')'      o ctrl-U (Up)               -- 1/2 screen up'
write(io,'(a)')'      o ctrl-D (Down)             -- 1/2 screen down'
write(io,'(a)')''
write(io,'(a)')'         @SMOOTH SCROLLING@'
write(io,'(a)')'              ctrl-Y'
write(io,'(a)')'                #'
write(io,'(a)')'       ctrl-L ##### ctrl-R'
write(io,'(a)')'                #'
write(io,'(a)')'              ctrl-E'
write(io,'(a)')'################################################################################'
write(io,'(a)')'@CURSOR MOVEMENT:@'
write(io,'(a)')''
write(io,'(a)')'The cursor is moved with the arrow keys or by toggling on vi-mode. In vi-mode'
write(io,'(a)')'the j,k,l,m keys move the cursor as they do in the vi(1) editor until ctrl-v'
write(io,'(a)')'is re-entered.'
write(io,'(a)')''
write(io,'(a)')'The Home key places the top right corner of the form in the top right corner'
write(io,'(a)')'of the form display region and puts the cursor at the corner.'
write(io,'(a)')''
write(io,'(a)')'The Tab key or ctrl-N moves to the next selectable field.'
write(io,'(a)')'The Backtab (ie. shifted Tab) key or ctrl-P moves to the previous'
write(io,'(a)')'selectable field.'
write(io,'(a)')'################################################################################'
write(io,'(a)')'@VI MODE@'
write(io,'(a)')''
write(io,'(a)')'The ESCAPE key toggles vi(1) mode. ctrl-V always puts you in vi(1) mode. You are'
write(io,'(a)')'in vi(1) mode everywhere except underlined text input fields by default.'
write(io,'(a)')''
write(io,'(a)')'ctrl-N or n or tab       -- go to next input field'
write(io,'(a)')'ctrl-P or p or shift-tab -- go to previous input field'
write(io,'(a)')'0                        -- beginning of line'
write(io,'(a)')'g                        -- top of form'
write(io,'(a)')'hjkl                     -- move cursor'
write(io,'(a)')''
write(io,'(a)')'i or insert              -- insert mode in underlined field'
write(io,'(a)')'                            insert key or ESC key or anything other than a'
write(io,'(a)')'                            regular character, delete, arrow keys ends insert'
write(io,'(a)')'                            mode.'
write(io,'(a)')'x or delete              -- delete character in underlined field'
write(io,'(a)')'r                        -- replace mode in underlined field'
write(io,'(a)')'                            This is the default mode when not in vi(1) mode.'
write(io,'(a)')'                            ESCAPE will exit and leave you where you are.'
write(io,'(a)')'################################################################################'
write(io,'(a)')'Color pair examples:'
write(io,'(a)')'   #`AColor Pair A`# Color Pair A#'
write(io,'(a)')'   #`BColor Pair B`# Color Pair B#'
write(io,'(a)')'   #`CColor Pair C`# Color Pair C#'
write(io,'(a)')'   #`DColor Pair D`# Color Pair D#'
write(io,'(a)')'   #`EColor Pair E`# Color Pair E#'
write(io,'(a)')'   #`FColor Pair F`# Color Pair F#'
write(io,'(a)')'   #`GColor Pair G`# Color Pair G#'
write(io,'(a)')'   #`JColor Pair J`# Color Pair J#'
write(io,'(a)')'   #`IColor Pair I`# Color Pair I#'
write(io,'(a)')'   #`JColor Pair J`# Color Pair J#'
write(io,'(a)')'   #`KColor Pair K`# Color Pair K#'
write(io,'(a)')'   #`LColor Pair L`# Color Pair L#'
write(io,'(a)')'   #`MColor Pair M`# Color Pair M#'
write(io,'(a)')'   #`NColor Pair N`# Color Pair N#'
write(io,'(a)')'   #`OColor Pair O`# Color Pair O#'
write(io,'(a)')'   #`PColor Pair P`# Color Pair P#'
write(io,'(a)')'   #`QColor Pair Q`# Color Pair Q#'
write(io,'(a)')'   #`RColor Pair R`# Color Pair R#'
write(io,'(a)')'   #`SColor Pair S`# Color Pair S#'
write(io,'(a)')'   #`TColor Pair T`# Color Pair T#'
write(io,'(a)')'   #`UColor Pair U`# Color Pair U#'
write(io,'(a)')'   #`VColor Pair V`# Color Pair V#'
write(io,'(a)')'   #`WColor Pair W`# Color Pair W#'
write(io,'(a)')'   #`XColor Pair X`# Color Pair X#'
write(io,'(a)')'   #`YColor Pair Y`# Color Pair Y#'
write(io,'(a)')'   #`ZColor Pair Z`# Color Pair Z#'
write(io,'(a)')'################################################################################'
write(io,'(a)')'################################################################################'
      close(10)
      filename='fixedform_demo.tui'
   endif
   call loaddata(filename)      ! fill the page(*) with user data

   do while (once)
         call fixedform(tabs=answers)
         write(*,*)answers
         once=loop
   enddo
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
'@(#)PROGRAM:        fixedform(1)>',&
'@(#)DESCRIPTION:    display a TUI definition and return values>',&
'@(#)VERSION:        1.0, 20150508>',&
'@(#)AUTHOR:         John S. Urban>',&
'@(#)COMPILED:       2021-12-18 15:27:12 UTC-300>',&
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
'    fixedfrom(1f) - [FUNIX] read in a TUI definition and return values from screen panel                                        ',&
'    (LICENSE:PD)                                                                                                                ',&
'                                                                                                                                ',&
'SYNOPSIS                                                                                                                        ',&
'    fixedform [file|-demo]|[ --help| --version]                                                                                 ',&
'                                                                                                                                ',&
'DESCRIPTION                                                                                                                     ',&
'    fixedform(1) reads in a TUI screen form definition and displays it and returns                                              ',&
'    the values used to fill out the screen.                                                                                     ',&
'                                                                                                                                ',&
'OPTIONS                                                                                                                         ',&
'    file       name of file containing TUI definition                                                                           ',&
'    -loop      continue reading and displaying the the form unit a ''q'' is entered                                             ',&
'    --help     display this help and exit                                                                                       ',&
'    --version  output version information and exit                                                                              ',&
'    --demo                                                                                                                      ',&
'                                                                                                                                ',&
'EXAMPLES                                                                                                                        ',&
'    Sample commands                                                                                                             ',&
'                                                                                                                                ',&
'       fixedform  FILE1                                                                                                         ',&
'                                                                                                                                ',&
'REPORTING BUGS                                                                                                                  ',&
'    Report fixedform bugs to <http://www.urbanjost.altervista.org/index.html>                                                   ',&
'                                                                                                                                ',&
'SEE ALSO                                                                                                                        ',&
'    yes(1), repeat(1), xargs(1)                                                                                                 ',&
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
!!     fixedfrom(1f) - [FUNIX] read in a TUI definition and return values from screen panel
!!     (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!     fixedform [file|-demo]|[ --help| --version]
!!
!!##DESCRIPTION
!!     fixedform(1) reads in a TUI screen form definition and displays it and returns
!!     the values used to fill out the screen.
!!
!!##OPTIONS
!!     file       name of file containing TUI definition
!!     -loop      continue reading and displaying the the form unit a 'q' is entered
!!     --help     display this help and exit
!!     --version  output version information and exit
!!     --demo
!!
!!##EXAMPLES
!!
!!     Sample commands
!!
!!        fixedform  FILE1
!!
!!##REPORTING BUGS
!!     Report fixedform bugs to <http://www.urbanjost.altervista.org/index.html>
!!
!!##SEE ALSO
!!     yes(1), repeat(1), xargs(1)
!!##AUTHOR
!!    John S. Urban
!!##LICENSE
!!    Public Domain
end program fifo
!-----------------------------------------------------------------------------------------------------------------------------------
!<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>-
!-----------------------------------------------------------------------------------------------------------------------------------
