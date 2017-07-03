!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
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
'    asa2pdf(1f) - [FILE FILTER]Convert text files with/without                  ',&
'                  ASA carriage control to an Adobe PDF file.                    ',&
'                                                                                ',&
'SYNOPSIS                                                                        ',&
'   asa2pdf -o output_filename -i input_filename                                 ',&
'           -g gray_scale_shade -b lines_alternately_shaded -d dashcode          ',&
'           -s top_middle_page_label -t top_left_page_label                      ',&
'           -P # add page numbers                                                ',&
'           -l lines_per_page -f font_name -S columns_to_shift_data              ',&
'           -N # add line numbers                                                ',&
'           -H page_height -W page_width -u points_per_unit                      ',&
'           -L left_margin -R right_margin -B bottom_margin -T top_margin        ',&
'           -help -version -show                                                 ',&
'                                                                                ',&
'DESCRIPTION                                                                     ',&
'                                                                                ',&
'   Basically, asa2pdf(1) emulates a line printer that recognizes ASA            ',&
'   carriage control. That is, it lets you convert ASCII text files using        ',&
'   ASA carriage control into Adobe "clear text" PDF files instead of a          ',&
'   printed page.                                                                ',&
'                                                                                ',&
'   The PDF is clear-text ASCII so that it is easy to still use other            ',&
'   Unix/Linux utilities such as spell(1), diff(1), grep(1), .... on the         ',&
'   output files.                                                                ',&
'                                                                                ',&
'   To properly view the output requires a PDF processor (such                   ',&
'   as xpdf(1),acroread(1)/AcroRd32, gv(1) or ghostview(1), ...).                ',&
'   Most modern systems can view, mail and print PDF files.                      ',&
'                                                                                ',&
'   The default layout generates a landscape 132-column 60-line format with      ',&
'   every other two lines shaded. A variety of switches are available to         ',&
'   let you easily print files with no vertical carriage control, and in         ',&
'   portrait mode too. There are options to use dashed lines instead of          ',&
'   shading, to set different margins, and so on.                                ',&
'                                                                                ',&
'   WHAT IS ASA CARRIAGE CONTROL?                                                ',&
'                                                                                ',&
'   The ASA carriage control standard was the first important formatting         ',&
'   standard for printing and viewing text files. The standard was almost        ',&
'   universally adapted by printer manufacturers of the time (and printers       ',&
'   were a much more common output device than interactive displays).            ',&
'                                                                                ',&
'   Most commercial high-level programs at the time the standard was             ',&
'   created were either FORTRAN or COBOL; so nearly all early FORTRAN            ',&
'   output used ASA carriage control                                             ',&
'   (ASA was the American Standards Association -- now ANSI).                    ',&
'   This FORTRAN/ASA association became so strong that the standard is           ',&
'   sometimes referred to as the "Fortran carriage control standard" (FCC).      ',&
'   Indeed, even though ASA is no longer commonly directly supported on          ',&
'   desktop printers, it was part of the Fortran 90 standard (this was           ',&
'   dropped in Fortran 2003 -- how a printer processes files is really           ',&
'   not directly part of any programming language).                              ',&
'                                                                                ',&
'   Times have changed, and the once nearly ubiquitous ASA standard              ',&
'   is poorly supported on Unix and MSWindows machines in particular             ',&
'   (Direct operating-system support of ASA files was once common, but           ',&
'   is now rare).                                                                ',&
'                                                                                ',&
'   But no alternative as simple has emerged for output files                    ',&
'   that truly replaces the ASA standard (although machine control               ',&
'   characters (ctrl-H, ctrl-L, ...) have come close they have their             ',&
'   own issues).                                                                 ',&
'                                                                                ',&
'   So many programs using ASA-based formatting have not been changed,           ',&
'   and use commands like asa(1)/nasa(1), and fpr(1) to allow the files to       ',&
'   be printed as desired but NOT to generally be viewed properly on-line,       ',&
'   and printing itself is becoming less common.                                 ',&
'                                                                                ',&
'   So the problem isn''t so much with ASA files, but that today''s              ',&
'   infrastructure does not support the format well. The asa2pdf(1)              ',&
'   program bridges the gap by allowing you to still make and manipulate         ',&
'   ASA files until you want to print or email them, at which time you           ',&
'   can quickly convert them to an Adobe PDF file.                               ',&
'                                                                                ',&
'USAGE                                                                           ',&
'                                                                                ',&
'   asa2pdf(1) reads input from standard input. By default the first             ',&
'   character of each line is interpreted as a control character. Lines          ',&
'   beginning with any character other than those listed in the ASA              ',&
'   carriage-control characters table or in the list of extensions below         ',&
'   are interpreted as if they began with a blank, and an appropriate            ',&
'   diagnostic appears on standard error. The first character of each            ',&
'   line is not printed.                                                         ',&
'                                                                                ',&
'   ASA Carriage Control Characters                                              ',&
'                                                                                ',&
'        +------------+-----------------------------------------------+          ',&
'        | Character  |                                               |          ',&
'        +------------+-----------------------------------------------+          ',&
'        | +          | Do not advance; overstrike previous line.     |          ',&
'        | blank      | Advance one line.                             |          ',&
'        | null lines | Treated as if they started with a blank       |          ',&
'        | 0          | Advance two lines.                            |          ',&
'        | -          | Advance three lines (IBM extension).          |          ',&
'        | 1          | Advance to top of next page.                  |          ',&
'        | all others | Discarded (except for extensions listed below)|          ',&
'        +------------+-----------------------------------------------+          ',&
'   Extensions                                                                   ',&
'                                                                                ',&
'       H   Advance one-half line.                                               ',&
'       R   Do not advance; overstrike previous line. Use red text color         ',&
'       G   Do not advance; overstrike previous line. Use green text color       ',&
'       B   Do not advance; overstrike previous line. Use blue text color        ',&
'       r   Advance one line. Use red text color                                 ',&
'       g   Advance one line. Use green text color                               ',&
'       b   Advance one line. Use blue text color                                ',&
'       ^   Overprint but add 127 to the ADE value of the character              ',&
'           (ie., use ASCII extended character set)                              ',&
'                                                                                ',&
'OPTIONS                                                                         ',&
'       -o outputfile  Name of Adobe PDF output file to create                   ',&
'       -i inputfile   Name of text file to read. Defaults to stdin.             ',&
'                                                                                ',&
'    PRINTABLE PAGE AREA                                                         ',&
'                                                                                ',&
'      The page size may be specified using -H for height, -W for width, and -u  ',&
'      to indicate the points per unit (72 makes H and W in inches,              ',&
'      1 is used when units are in font points). For example:                    ',&
'                                                                                ',&
'       -u 72 -H 8.5 -W 11   # page Height and Width in inches                   ',&
'       -T 0.5 -B 0.5 -L 0.5 -R 0.5   # margins (Top, Bottom, Left, Right)       ',&
'                                                                                ',&
'      common media sizes with -u 1:                                             ',&
'                                                                                ',&
'       +-------------------+------+------------+                                ',&
'       | name              |  W   |        H   |                                ',&
'       +-------------------+------+------------+                                ',&
'       | Letterdj (11x8.5) | 792  |       612  | (LandScape)                    ',&
'       | A4dj              | 842  |       595  |                                ',&
'       | Letter (8.5x11)   | 612  |       792  | (Portrait)                     ',&
'       | Legal             | 612  |       1008 |                                ',&
'       | A5                | 420  |       595  |                                ',&
'       | A4                | 595  |       842  |                                ',&
'       | A3                | 842  |       1190 |                                ',&
'       +-------------------+------+------------+                                ',&
'                                                                                ',&
'    SHADING                                                                     ',&
'        -g 0.800781      gray-scale value for shaded bars ( 0 < g 1 )           ',&
'                         0 is black, 1 is white.                                ',&
'        -i 2             repeat shade pattern every N lines                     ',&
'        -d '' ''           dashcode pattern                                     ',&
'                         The pattern is a series of integers defining an        ',&
'                         on-off sequence in user units used to create a         ',&
'                         dash pattern. A single digit "N" implies a pattern     ',&
'                         of "N N". (seems buggy)                                ',&
'                                                                                ',&
'    MARGIN LABELS                                                               ',&
'       -s ''''             top middle page label.                               ',&
'       -t ''''             top left page label.                                 ',&
'       -P                add page numbers to right corners                      ',&
'                                                                                ',&
'    TEXT OPTIONS                                                                ',&
'       -l 60             lines per page                                         ',&
'       -f Courier        font names: Courier, Courier-Bold,Courier-Oblique      ',&
'                         Helvetica, Symbol, Times-Bold, Helvetica-Bold,         ',&
'                         ZapfDingbats, Times-Italic, Helvetica-Oblique,         ',&
'                         Times-BoldItalic, Helvetica-BoldOblique,               ',&
'                         Times-Roman, Courier-BoldOblique                       ',&
'                                                                                ',&
'       -S 0              right shift 1 for non-ASA files                        ',&
'       -N                add line numbers                                       ',&
'    INFORMATION                                                                 ',&
'       -version          display version number                                 ',&
'       -help             display this help                                      ',&
'                                                                                ',&
'ENVIRONMENT VARIABLES                                                           ',&
'     o $IMPACT_TOP Will be printed in large red letters across the page top.    ',&
'     o $IMPACT_GRAY sets the default gray-scale value, same as the -g switch.   ',&
'                                                                                ',&
'EXAMPLES                                                                        ',&
' Sample input:                                                                  ',&
'                                                                                ',&
'  > The numbers are plain underlined double-struck over-struck                  ',&
'  >+                      __________ double-struck ///////////                  ',&
'  >R                                               ///////////                  ',&
'  > abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789-=_+()*&^%$#@!\|[]{};'':",.<>/?`~',&
'  >                                                                             ',&
'  >r red                                                                        ',&
'  >g     green                                                                  ',&
'  >b           blue                                                             ',&
'  > PRIMARY:                                                                    ',&
'  >R         red                                                                ',&
'  >G             green                                                          ',&
'  >B                   blue                                                     ',&
'  > 1/2 line advance                                                            ',&
'  >H                1                                                           ',&
'  >H                 2         a-1                                              ',&
'  >H                  3       Z                                                 ',&
'  >H                   4       b                                                ',&
'  > back to a normal line                                                       ',&
'                                                                                ',&
' Sample commands:                                                               ',&
'                                                                                ',&
'     # use non-ASA file to generate portrait mode with a dashed line under every line',&
'     asa2pdf -S 1 -W 8.5 -H 11 -i 1 -d ''2 4 1'' -T 1 -B .75 -o paper.pdf < INFILE',&
'                                                                                ',&
'     # banner on top                                                            ',&
'     env IMPACT_GRAY=1 IMPACT_TOP=CONFIDENTIAL asa2pdf -o paper.pdf < test.txt  ',&
'                                                                                ',&
'     # 132-column landscape                                                     ',&
'      asa2pdf -s LANDSCAPE -o paper.pdf <asa2pdf.c                              ',&
'                                                                                ',&
'     # 132-column landscape with line numbers with dashed lines                 ',&
'      asa2pdf -s ''LANDSCAPE LINE NUMBERS'' -d ''3 1 2'' \                      ',&
'      -N -T .9 -o paper.pdf <asa2pdf.c                                          ',&
'                                                                                ',&
'     # portrait 80-column non-ASA file with dashed lines                        ',&
'      asa2pdf -s PORTRAIT -S 1 -W 8.5 -H 11 -i 1 -d ''2 4 1'' \                 ',&
'      -T 1 -B .75 -o paper.pdf < asa2pdf.c                                      ',&
'                                                                                ',&
'     # portrait 80-column with line numbers , non-ASA                           ',&
'      asa2pdf -s ''PORTRAIT LINE NUMBERS'' -l 66 -S 1 -W 8.5 -H 11 \            ',&
'      -i 1 -T 1 -B .75 -N -o paper.pdf < asa2pdf.c                              ',&
'                                                                                ',&
'     # titling                                                                  ',&
'      asa2pdf -d ''1 0 1'' -t "$USER" -i 1 -P -N -T 1 \                         ',&
'      -s "asa2pdf.c" -o paper.pdf <asa2pdf.c                                    ',&
'                                                                                ',&
'SEE ALSO                                                                        ',&
'                                                                                ',&
'   ALTERNATIVES TO ASA2PDF                                                      ',&
'                                                                                ',&
'   About the only standard ASA support on Unix variants is that some            ',&
'   contain the asa(1)/fpr(1) and nasa(1) commands for converting ASA text       ',&
'   files into and from text files with machine control (MC) characters          ',&
'   such as form-feed, backspace, carriage-return, .... Most personal            ',&
'   printers will no longer properly print ASA files directly, but they          ',&
'   will often correctly print files with simple MC characters                   ',&
'   (Note that the asa(1) command is referenced in the POSIX.2 standard).        ',&
'                                                                                ',&
'   Furthermore, if a printer does not directly support MC characters,           ',&
'   text conversion utilities such as enscript(1) and a2ps(1) can                ',&
'   often be used to print the files (usually by converting the files            ',&
'   to PostScript or PCL). Such utilities support features such as               ',&
'   titling, page numbering, and other useful options.                           ',&
'                                                                                ',&
'   Programs like "Adobe Distiller" can convert text to a PDF; as well as        ',&
'   editors such as OpenOffice. In fact, most modern document-formatting         ',&
'   editors can read in an ASCII text file and save it as an Adobe               ',&
'   PDF file.                                                                    ',&
'                                                                                ',&
'   HTML and PostScript/PDF and PCL files are the alternatives often             ',&
'   incorporated to satisfy simple formatting criteria --                        ',&
'   yet HTML is not printer-oriented;                                            ',&
'   and PDF files are complex to write from a simple program, and PCL is         ',&
'   vendor-specific and has few on-line viewers available for it.                ',&
'                                                                                ',&
'                                                                                ',&
'   Assuming converting the Fortran program to just write a plain ASCII          ',&
'   file instead of an ASA file is not acceptable, More extensive flat-text      ',&
'   formatting is available using                                                ',&
'                                                                                ',&
'   o HTML, *roff and LaTex-related file formats                                 ',&
'   o libraries for writing more sophisticated PostScript, PDF, and HTML/CSS files',&
'   o XML files formatted using Cascading Style Sheet (CSS) files                ',&
'   o RTF (Rich Text Format) files                                               ',&
'                                                                                ',&
'   Other Unix commands that can be useful in working with plain text and        ',&
'   MC character files are                                                       ',&
'                                                                                ',&
'      pr(1)           can be used to add page numbers and titles.               ',&
'      expand(1)       can remove tab characters                                 ',&
'      fold(1),fmt(1)  can be used to wrap the text                              ',&
'      cut(1)          can let you trim or select columns                        ',&
'      cat -n          can be used to add number lines                           ',&
'      paste(1)        can be used to put files side-by-side.                    ',&
'                                                                                ',&
'asa(1)/nasa(1), fpr(1), enscript(1), a2ps(1), and ps2pdf(1).                    ',&
'                                                                                ',&
'']
   WRITE(*,'(a)')(trim(help_text(i)),i=1,size(help_text))
   stop ! if -help was specified, stop
endif
end subroutine help_usage
!-----------------------------------------------------------------------------------------------------------------------------------
!>
!!##NAME
!!     asa2pdf(1f) - [FILE FILTER]Convert text files with/without
!!                   ASA carriage control to an Adobe PDF file.
!!
!!##SYNOPSIS
!!
!!    asa2pdf -o output_filename -i input_filename
!!            -g gray_scale_shade -b lines_alternately_shaded -d dashcode
!!            -s top_middle_page_label -t top_left_page_label
!!            -P # add page numbers
!!            -l lines_per_page -f font_name -S columns_to_shift_data
!!            -N # add line numbers
!!            -H page_height -W page_width -u points_per_unit
!!            -L left_margin -R right_margin -B bottom_margin -T top_margin
!!            -help -version -show
!!
!!##DESCRIPTION
!!
!!    Basically, asa2pdf(1) emulates a line printer that recognizes ASA
!!    carriage control. That is, it lets you convert ASCII text files using
!!    ASA carriage control into Adobe "clear text" PDF files instead of a
!!    printed page.
!!
!!    The PDF is clear-text ASCII so that it is easy to still use other
!!    Unix/Linux utilities such as spell(1), diff(1), grep(1), .... on the
!!    output files.
!!
!!    To properly view the output requires a PDF processor (such
!!    as xpdf(1),acroread(1)/AcroRd32, gv(1) or ghostview(1), ...).
!!    Most modern systems can view, mail and print PDF files.
!!
!!    The default layout generates a landscape 132-column 60-line format with
!!    every other two lines shaded. A variety of switches are available to
!!    let you easily print files with no vertical carriage control, and in
!!    portrait mode too. There are options to use dashed lines instead of
!!    shading, to set different margins, and so on.
!!
!!    WHAT IS ASA CARRIAGE CONTROL?
!!
!!    The ASA carriage control standard was the first important formatting
!!    standard for printing and viewing text files. The standard was almost
!!    universally adapted by printer manufacturers of the time (and printers
!!    were a much more common output device than interactive displays).
!!
!!    Most commercial high-level programs at the time the standard was
!!    created were either FORTRAN or COBOL; so nearly all early FORTRAN
!!    output used ASA carriage control
!!    (ASA was the American Standards Association -- now ANSI).
!!    This FORTRAN/ASA association became so strong that the standard is
!!    sometimes referred to as the "Fortran carriage control standard" (FCC).
!!    Indeed, even though ASA is no longer commonly directly supported on
!!    desktop printers, it was part of the Fortran 90 standard (this was
!!    dropped in Fortran 2003 -- how a printer processes files is really
!!    not directly part of any programming language).
!!
!!    Times have changed, and the once nearly ubiquitous ASA standard
!!    is poorly supported on Unix and MSWindows machines in particular
!!    (Direct operating-system support of ASA files was once common, but
!!    is now rare).
!!
!!    But no alternative as simple has emerged for output files
!!    that truly replaces the ASA standard (although machine control
!!    characters (ctrl-H, ctrl-L, ...) have come close they have their
!!    own issues).
!!
!!    So many programs using ASA-based formatting have not been changed,
!!    and use commands like asa(1)/nasa(1), and fpr(1) to allow the files to
!!    be printed as desired but NOT to generally be viewed properly on-line,
!!    and printing itself is becoming less common.
!!
!!    So the problem isn't so much with ASA files, but that today's
!!    infrastructure does not support the format well. The asa2pdf(1)
!!    program bridges the gap by allowing you to still make and manipulate
!!    ASA files until you want to print or email them, at which time you
!!    can quickly convert them to an Adobe PDF file.
!!
!!##USAGE
!!
!!    asa2pdf(1) reads input from standard input. By default the first
!!    character of each line is interpreted as a control character. Lines
!!    beginning with any character other than those listed in the ASA
!!    carriage-control characters table or in the list of extensions below
!!    are interpreted as if they began with a blank, and an appropriate
!!    diagnostic appears on standard error. The first character of each
!!    line is not printed.
!!
!!    ASA Carriage Control Characters
!!
!!         +------------+-----------------------------------------------+
!!         | Character  |                                               |
!!         +------------+-----------------------------------------------+
!!         | +          | Do not advance; overstrike previous line.     |
!!         | blank      | Advance one line.                             |
!!         | null lines | Treated as if they started with a blank       |
!!         | 0          | Advance two lines.                            |
!!         | -          | Advance three lines (IBM extension).          |
!!         | 1          | Advance to top of next page.                  |
!!         | all others | Discarded (except for extensions listed below)|
!!         +------------+-----------------------------------------------+
!!    Extensions
!!
!!        H   Advance one-half line.
!!        R   Do not advance; overstrike previous line. Use red text color
!!        G   Do not advance; overstrike previous line. Use green text color
!!        B   Do not advance; overstrike previous line. Use blue text color
!!        r   Advance one line. Use red text color
!!        g   Advance one line. Use green text color
!!        b   Advance one line. Use blue text color
!!        ^   Overprint but add 127 to the ADE value of the character
!!            (ie., use ASCII extended character set)
!!
!!##OPTIONS
!!        -o outputfile  Name of Adobe PDF output file to create
!!        -i inputfile   Name of text file to read. Defaults to stdin.
!!
!!     PRINTABLE PAGE AREA
!!
!!       The page size may be specified using -H for height, -W for width, and -u
!!       to indicate the points per unit (72 makes H and W in inches,
!!       1 is used when units are in font points). For example:
!!
!!        -u 72 -H 8.5 -W 11   # page Height and Width in inches
!!        -T 0.5 -B 0.5 -L 0.5 -R 0.5   # margins (Top, Bottom, Left, Right)
!!
!!       common media sizes with -u 1:
!!
!!        +-------------------+------+------------+
!!        | name              |  W   |        H   |
!!        +-------------------+------+------------+
!!        | Letterdj (11x8.5) | 792  |       612  | (LandScape)
!!        | A4dj              | 842  |       595  |
!!        | Letter (8.5x11)   | 612  |       792  | (Portrait)
!!        | Legal             | 612  |       1008 |
!!        | A5                | 420  |       595  |
!!        | A4                | 595  |       842  |
!!        | A3                | 842  |       1190 |
!!        +-------------------+------+------------+
!!
!!     SHADING
!!         -g 0.800781      gray-scale value for shaded bars ( 0 < g 1 )
!!                          0 is black, 1 is white.
!!         -i 2             repeat shade pattern every N lines
!!         -d ' '           dashcode pattern
!!                          The pattern is a series of integers defining an
!!                          on-off sequence in user units used to create a
!!                          dash pattern. A single digit "N" implies a pattern
!!                          of "N N". (seems buggy)
!!
!!     MARGIN LABELS
!!        -s ''             top middle page label.
!!        -t ''             top left page label.
!!        -P                add page numbers to right corners
!!
!!     TEXT OPTIONS
!!        -l 60             lines per page
!!        -f Courier        font names: Courier, Courier-Bold,Courier-Oblique
!!                          Helvetica, Symbol, Times-Bold, Helvetica-Bold,
!!                          ZapfDingbats, Times-Italic, Helvetica-Oblique,
!!                          Times-BoldItalic, Helvetica-BoldOblique,
!!                          Times-Roman, Courier-BoldOblique
!!
!!        -S 0              right shift 1 for non-ASA files
!!        -N                add line numbers
!!     INFORMATION
!!        -version          display version number
!!        -help             display this help
!!
!!##ENVIRONMENT VARIABLES
!!      o $IMPACT_TOP Will be printed in large red letters across the page top.
!!      o $IMPACT_GRAY sets the default gray-scale value, same as the -g switch.
!!
!!##EXAMPLES
!!
!!  Sample input:
!!
!!   > The numbers are plain underlined double-struck over-struck
!!   >+                      __________ double-struck ///////////
!!   >R                                               ///////////
!!   > abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789-=_+()*&^%$#@!\|[]{};':",.<>/?`~
!!   >
!!   >r red
!!   >g     green
!!   >b           blue
!!   > PRIMARY:
!!   >R         red
!!   >G             green
!!   >B                   blue
!!   > 1/2 line advance
!!   >H                1
!!   >H                 2         a-1
!!   >H                  3       Z
!!   >H                   4       b
!!   > back to a normal line
!!
!!  Sample commands:
!!
!!      # use non-ASA file to generate portrait mode with a dashed line under every line
!!      asa2pdf -S 1 -W 8.5 -H 11 -i 1 -d '2 4 1' -T 1 -B .75 -o paper.pdf < INFILE
!!
!!      # banner on top
!!      env IMPACT_GRAY=1 IMPACT_TOP=CONFIDENTIAL asa2pdf -o paper.pdf < test.txt
!!
!!      # 132-column landscape
!!       asa2pdf -s LANDSCAPE -o paper.pdf <asa2pdf.c
!!
!!      # 132-column landscape with line numbers with dashed lines
!!       asa2pdf -s 'LANDSCAPE LINE NUMBERS' -d '3 1 2' \
!!       -N -T .9 -o paper.pdf <asa2pdf.c
!!
!!      # portrait 80-column non-ASA file with dashed lines
!!       asa2pdf -s PORTRAIT -S 1 -W 8.5 -H 11 -i 1 -d '2 4 1' \
!!       -T 1 -B .75 -o paper.pdf < asa2pdf.c
!!
!!      # portrait 80-column with line numbers , non-ASA
!!       asa2pdf -s 'PORTRAIT LINE NUMBERS' -l 66 -S 1 -W 8.5 -H 11 \
!!       -i 1 -T 1 -B .75 -N -o paper.pdf < asa2pdf.c
!!
!!      # titling
!!       asa2pdf -d '1 0 1' -t "$USER" -i 1 -P -N -T 1 \
!!       -s "asa2pdf.c" -o paper.pdf <asa2pdf.c
!!
!!##SEE ALSO
!!
!!    ALTERNATIVES TO ASA2PDF
!!
!!    About the only standard ASA support on Unix variants is that some
!!    contain the asa(1)/fpr(1) and nasa(1) commands for converting ASA text
!!    files into and from text files with machine control (MC) characters
!!    such as form-feed, backspace, carriage-return, .... Most personal
!!    printers will no longer properly print ASA files directly, but they
!!    will often correctly print files with simple MC characters
!!    (Note that the asa(1) command is referenced in the POSIX.2 standard).
!!
!!    Furthermore, if a printer does not directly support MC characters,
!!    text conversion utilities such as enscript(1) and a2ps(1) can
!!    often be used to print the files (usually by converting the files
!!    to PostScript or PCL). Such utilities support features such as
!!    titling, page numbering, and other useful options.
!!
!!    Programs like "Adobe Distiller" can convert text to a PDF; as well as
!!    editors such as OpenOffice. In fact, most modern document-formatting
!!    editors can read in an ASCII text file and save it as an Adobe
!!    PDF file.
!!
!!    HTML and PostScript/PDF and PCL files are the alternatives often
!!    incorporated to satisfy simple formatting criteria --
!!    yet HTML is not printer-oriented;
!!    and PDF files are complex to write from a simple program, and PCL is
!!    vendor-specific and has few on-line viewers available for it.
!!
!!
!!    Assuming converting the Fortran program to just write a plain ASCII
!!    file instead of an ASA file is not acceptable, More extensive flat-text
!!    formatting is available using
!!
!!    o HTML, *roff and LaTex-related file formats
!!    o libraries for writing more sophisticated PostScript, PDF, and HTML/CSS files
!!    o XML files formatted using Cascading Style Sheet (CSS) files
!!    o RTF (Rich Text Format) files
!!
!!    Other Unix commands that can be useful in working with plain text and
!!    MC character files are
!!
!!       pr(1)           can be used to add page numbers and titles.
!!       expand(1)       can remove tab characters
!!       fold(1),fmt(1)  can be used to wrap the text
!!       cut(1)          can let you trim or select columns
!!       cat -n          can be used to add number lines
!!       paste(1)        can be used to put files side-by-side.
!!
!! asa(1)/nasa(1), fpr(1), enscript(1), a2ps(1), and ps2pdf(1).
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
'@(#)PROGRAM:        asa2pdf(1f)>',&
'@(#)DESCRIPTION:    convert text files with ASA carriage return to Adobe PDF files>',&
'@(#)VERSION:        2.0, 20170210>',&
'@(#)AUTHOR:         John S. Urban>',&
'@(#)COMPILED:       Wed, Jun 14th, 2017 10:08:41 AM>',&
'']
   WRITE(*,'(a)')(trim(help_text(i)(5:len_trim(help_text(i))-1)),i=1,size(help_text))
   stop ! if -version was specified, stop
endif
end subroutine help_version
!-----------------------------------------------------------------------------------------------------------------------------------
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
program asa2pdf
use M_debug, only   : stderr
use M_kracken, only : kracken, sget, rget, iget, lget
use M_strings, only : s2v, v2s
implicit none

! size of printable area
! Default unit is 72 points per inch

 character(len=256),save      :: GLOBAL_CENTER_TITLE       = ' '
 character(len=256),save      :: GLOBAL_DASHCODE           = ' '
 character(len=256),save      :: GLOBAL_FONT               = 'Courier'
 character(len=256),save      :: GLOBAL_LEFT_TITLE         = ' '
 character(len=:),allocatable :: GLOBAL_PAGE_LIST
 logical                      :: GLOBAL_LINENUMBERS        = .false.
 logical                      :: GLOBAL_PAGES              = .false.
 integer                      :: GLOBAL_ADD                = 0
 integer                      :: GLOBAL_LINECOUNT          = 0
 integer                      :: GLOBAL_NUM_PAGES          = 0
 integer                      :: GLOBAL_OBJECT_ID          = 1
 integer                      :: GLOBAL_PAGECOUNT          = 0
 integer                      :: GLOBAL_PAGE_TREE_ID
 integer                      :: GLOBAL_SHADE_STEP         = 2
 integer                      :: GLOBAL_SHIFT              = 0
 integer                      :: GLOBAL_STREAM_ID, GLOBAL_STREAM_LEN_ID
 integer                      :: GLOBAL_STREAM_START
 integer,parameter            :: GLOBAL_DIRECT             = 12
 integer,parameter            :: GLOBAL_OUTFILE            = 11
 integer                      :: GLOBAL_INFILE             = 10
 real                         :: GLOBAL_FONT_SIZE
 real                         :: GLOBAL_GRAY_SCALE         =   0.800781 ! gray-scale value
 real                         :: GLOBAL_LEAD_SIZE
 real                         :: GLOBAL_LINES_PER_PAGE     =  60.0
 real                         :: GLOBAL_PAGE_DEPTH         = 612.0
 real                         :: GLOBAL_PAGE_MARGIN_BOTTOM =  36.0
 real                         :: GLOBAL_PAGE_MARGIN_LEFT   =  40.0
 real                         :: GLOBAL_PAGE_MARGIN_RIGHT  =  39.0
 real                         :: GLOBAL_PAGE_MARGIN_TOP    =  36.0
 real                         :: GLOBAL_PAGE_WIDTH         = 792.0 ! Default is 72 points per inch
 real                         :: GLOBAL_TITLE_SIZE         =  20.0
 real                         :: GLOBAL_UNIT_MULTIPLIER    =   1.0
 real                         :: GLOBAL_YPOS

   character(len=100):: varname
   integer :: ios

   GLOBAL_PAGE_LIST=''

   call get_environment_variable("IMPACT_GRAY",varname)
   if(varname.eq.'') varname='0.800781' ! gray-scale value
   GLOBAL_GRAY_SCALE=s2v(varname)
   if(GLOBAL_GRAY_SCALE.lt.0) GLOBAL_GRAY_SCALE=0.800781

   call kracken('asa2pdf',' &
   & -o asa.pdf &
   ! SHADING
   ! gray-scale value  for shaded bars ( 0 < g < 1 ); 0 is black, 1 is white
   & -g 0.800781 &
   ! repeat shade pattern every N lines
   & -i 2 &
   ! dashcode pattern (seems buggy)
   & -d &
   ! MARGIN LABELS
   ! top middle page label.
   & -s &
   ! top left page label.
   & -t &
   ! add page numbers to right corners
   & -P       .F. &
   ! TEXT OPTIONS
   ! lines per page
   & -l 60 &
   ! font names
   & -f Courier &
   ! right shift N characters for non-ASA files
   & -S 0 &
   ! add line numbers
   & -N       .F. &
   ! PRINTABLE PAGE AREA
   !   The page size may be specified using -H for height, -W for width, and -u
   !   to indicate the points per unit (72 makes H and W in inches,
   !   1 is used when units are in font points). For example:
   ! page height
   & -H 612.0  &
   ! page width
   & -W 792.0 &
   ! units per inch
   & -u 1    &
   ! MARGINS
   ! left margin
   & -L 40.0 &
   ! right margin
   & -R 39.0 &
   ! bottom margin
   & -B 36.0 &
   ! top margin
   & -T 36.0 &

   & -show    .F. &
   & -help    .F. &
   & -version .F. &
   &')

   call help_usage(lget('asa2pdf_help'))              ! display help information and stop if true
   call help_version(lget('asa2pdf_version'))         ! display version information and stop if true

   OPEN(UNIT=GLOBAL_OUTFILE, FILE=trim(sget('asa2pdf_o')), ACCESS="STREAM", iostat=ios,form='formatted')
   if(ios.ne.0)then
      call stderr("E-R-R-O-R: asa2pdf(1) cannot open output file "//trim(sget('asa2pdf_o')))
      stop 2
   endif

   if(sget('asa2pdf_i').ne.'')then
      OPEN(UNIT=GLOBAL_INFILE, FILE=trim(sget('asa2pdf_i')), iostat=ios,form='formatted')
      if(ios.ne.0)then
         call stderr("E-R-R-O-R: asa2pdf(1) cannot open input file "//trim(sget('asa2pdf_i')))
         stop 2
      endif
   else
      GLOBAL_INFILE=5
   endif

   GLOBAL_UNIT_MULTIPLIER =     rget('asa2pdf_u')                          ! unit_divisor
   GLOBAL_PAGE_MARGIN_LEFT =    rget('asa2pdf_L')*GLOBAL_UNIT_MULTIPLIER;  ! Left margin
   GLOBAL_PAGE_MARGIN_RIGHT =   rget('asa2pdf_R')*GLOBAL_UNIT_MULTIPLIER;  ! Right margin
   GLOBAL_PAGE_MARGIN_BOTTOM =  rget('asa2pdf_B')*GLOBAL_UNIT_MULTIPLIER;  ! Bottom margin
   GLOBAL_PAGE_MARGIN_TOP =     rget('asa2pdf_T')*GLOBAL_UNIT_MULTIPLIER;  ! Top margin
   GLOBAL_PAGE_DEPTH =          rget('asa2pdf_H')*GLOBAL_UNIT_MULTIPLIER;  ! Height
   GLOBAL_PAGE_WIDTH =          rget('asa2pdf_W')*GLOBAL_UNIT_MULTIPLIER;  ! Width

   GLOBAL_GRAY_SCALE =          rget('asa2pdf_g')                          ! grayscale value for bars
   GLOBAL_LINES_PER_PAGE=       rget('asa2pdf_l')                          ! lines per page
   GLOBAL_SHADE_STEP =          iget('asa2pdf_b')                          ! increment for bars
   GLOBAL_SHIFT =               MAX(0,iget('asa2pdf_S'))                   ! right shift

   GLOBAL_CENTER_TITLE=sget('asa2pdf_s')                                   ! special label
   GLOBAL_LEFT_TITLE=sget('asa2pdf_t')                                     ! margin left label

   GLOBAL_DASHCODE=sget('asa2pdf_d')                                       ! dash code
   GLOBAL_FONT=sget('asa2pdf_f')                                           ! font

   GLOBAL_LINENUMBERS=lget('asa2pdf_N')                                    ! number lines
   GLOBAL_PAGES= lget('asa2pdf_P')                                         ! number pages

   if(GLOBAL_SHADE_STEP < 1 )then
      call stderr("W-A-R-N-I-N-G: asa2pdf(1) resetting -i "//v2s(GLOBAL_SHADE_STEP))
      GLOBAL_SHADE_STEP=1;
   endif

   if(GLOBAL_LINES_PER_PAGE < 1 )then
      call stderr("W-A-R-N-I-N-G: asa2pdf(1) resetting -l "//v2s(GLOBAL_LINES_PER_PAGE))
      GLOBAL_LINES_PER_PAGE=60;
   endif

   if(lget('asa2pdf_show'))then
      call showhelp()
      stop 3
   endif

   open(unit=GLOBAL_DIRECT,iostat=ios,access='direct',form='formatted',recl=34,status='scratch')
   if(ios.ne.0)then
      call stderr("E-R-R-O-R: asa2pdf(1) cannot open scratch file ")
      stop 3
   endif

   call dopages()

   CLOSE(UNIT=GLOBAL_OUTFILE,iostat=ios)
   CLOSE(UNIT=GLOBAL_DIRECT,iostat=ios)
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
contains
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
subroutine showhelp()
   write(*,'("-u ",g0," # unit multiplier")')          GLOBAL_UNIT_MULTIPLIER
   write(*,'("-T ",g0," # Top margin")')               GLOBAL_PAGE_MARGIN_TOP/GLOBAL_UNIT_MULTIPLIER
   write(*,'("-B ",g0," # Bottom margin")')            GLOBAL_PAGE_MARGIN_BOTTOM/GLOBAL_UNIT_MULTIPLIER
   write(*,'("-L ",g0," # Left margin")')              GLOBAL_PAGE_MARGIN_LEFT/GLOBAL_UNIT_MULTIPLIER
   write(*,'("-R ",g0," # Right margin")')             GLOBAL_PAGE_MARGIN_RIGHT/GLOBAL_UNIT_MULTIPLIER

   write(*,'("-W ",g0," # page Width")')               GLOBAL_PAGE_WIDTH/GLOBAL_UNIT_MULTIPLIER
   write(*,'("-H ",g0," # page Height")')              GLOBAL_PAGE_DEPTH/GLOBAL_UNIT_MULTIPLIER

   write(*,'("-g ",g0,    " # shading gray scale value ([black]0 <= g <= 1[white]")') GLOBAL_GRAY_SCALE
   write(*,'("-i ",i0,t14," # shading line increment")')    GLOBAL_SHADE_STEP
   write(*,'("-d ",a,     " # shading line dashcode")')     trim(GLOBAL_DASHCODE)

   write(*,'("-l ",g0,t14," # lines per page")')            GLOBAL_LINES_PER_PAGE
   write(*,'("-f ",a,t14, " # font name")')                 trim(GLOBAL_FONT)

   write(*,'("-s ",a,     " # margin label")')              trim(GLOBAL_CENTER_TITLE)
   write(*,'("-t ",a,     " # margin left label")')         trim(GLOBAL_LEFT_TITLE)
   write(*,'("-S ",i0,t14," # right shift")')               GLOBAL_SHIFT

   write(*,'("-N [flag=",g0,"] # add line numbers ")')      GLOBAL_LINENUMBERS
   write(*,'("-P [flag=",L1,"] # add page numbers")')       GLOBAL_PAGES
end subroutine showhelp
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##REFERENCE
!!    8.4.3.6       Line Dash Pattern
!!
!!    The line dash pattern shall control the pattern of dashes and gaps used to stroke paths. It shall be specified by
!!    a dash array and a dash phase. The dash array's elements shall be numbers that specify the lengths of
!!    alternating dashes and gaps; the numbers shall be nonnegative and not all zero. The dash phase shall specify
!!    the distance into the dash pattern at which to start the dash. The elements of both the dash array and the dash
!!    phase shall be expressed in user space units.
!!
!!    Before beginning to stroke a path, the dash array shall be cycled through, adding up the lengths of dashes and
!!    gaps. When the accumulated length equals the value specified by the dash phase, stroking of the path shall
!!    begin, and the dash array shall be used cyclically from that point onward. Table 56 shows examples of line
!!    dash patterns. As can be seen from the table, an empty dash array and zero phase can be used to restore the
!!    dash pattern to a solid line.
!!
!!                       Table 56 ­ Examples of Line Dash Patterns
!!
!!      Dash Array       Appearance                   Description
!!      and Phase
!!
!!      [] 0                                          No dash; solid, unbroken lines
!!
!!      [3] 0                                         3 units on, 3 units off, ...
!!
!!      [2] 1                                         1 on, 2 off, 2 on, 2 off, ...
!!
!!      [2 1] 0                                       2 on, 1 off, 2 on, 1 off, ...
!!
!!      [3 5] 6                                       2 off, 3 on, 5 off, 3 on, 5 off, ...
!!
!!      [ 2 3 ] 11                                    1 on, 3 off, 2 on, 3 off, 2 on, ...
!!
!!    Dashed lines shall wrap around curves and corners just as solid stroked lines do. The ends of each dash shall
!!    be treated with the current line cap style, and corners within dashes shall be treated with the current line join
!!    style. A stroking operation shall take no measures to coordinate the dash pattern with features of the path; it
!!    simply shall dispense dashes and gaps along the path in the pattern defined by the dash array.
!!
!!    When a path consisting of several subpaths is stroked, each subpath shall be treated independently--that is,
!!    the dash pattern shall be restarted and the dash phase shall be reapplied to it at the beginning of each subpath.
!===================================================================================================================================
subroutine print_bars()
real :: x1
real :: y1
real :: height
real :: width
real :: step

   write(GLOBAL_OUTFILE,'(f0.6," g")')GLOBAL_GRAY_SCALE ! gray-scale value
   ! If you want to add color,
   ! R G B rg where R G B are red, green, blue components
   ! in range 0.0 to 1.0 sets fill color, "RG" sets line
   ! color instead of fill color.
   !
   ! 0.60 0.82 0.60 rg
   !

   write(GLOBAL_OUTFILE,'(i0," i")')1

   x1=GLOBAL_PAGE_MARGIN_LEFT-0.1*GLOBAL_FONT_SIZE
   height=GLOBAL_SHADE_STEP*GLOBAL_LEAD_SIZE
   y1 = GLOBAL_PAGE_DEPTH - GLOBAL_PAGE_MARGIN_TOP - height- 0.22*GLOBAL_FONT_SIZE
   width=GLOBAL_PAGE_WIDTH-GLOBAL_PAGE_MARGIN_LEFT-GLOBAL_PAGE_MARGIN_RIGHT
   step=1.0

   if(GLOBAL_DASHCODE.ne.'')then
      write(GLOBAL_OUTFILE, '("0 w [",a,"] 0 d")')GLOBAL_DASHCODE      ! dash code array plus offset
   endif

   do while ( y1 >= (GLOBAL_PAGE_MARGIN_BOTTOM-height) )
      if(GLOBAL_DASHCODE .eq.'')then
            ! a shaded bar
            write(GLOBAL_OUTFILE,'(4(f0.6,1x),"re f")')x1,y1,width,height
            step=2.0

            !! x1 y1 m x2 y2 l S
            !! xxx w  # line width
            !write(GLOBAL_OUTFILE,'("0.6 0.8 0.6 RG",/,1x,f0.6,1x, f0.6," m ",%f %f," l S")')x1,y1,x1+width,y1

      else
             write(GLOBAL_OUTFILE,'(f0.6,1x,f0.6," m ")',advance='no') x1 ,y1
             write(GLOBAL_OUTFILE,'(f0.6,1x,f0.6," l s")')x1+width,y1
      endif
      y1=y1-step*height
   enddo

   if(GLOBAL_DASHCODE .ne. '')then
      write(GLOBAL_OUTFILE, '("[] 0 d")') ! set dash pattern to solid line
   endif

   write(GLOBAL_OUTFILE,'(i0," G")') 0
   write(GLOBAL_OUTFILE,'(i0," g")') 0 ! gray-scale value

 end subroutine print_bars
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
subroutine end_page()
integer :: stream_len
integer :: page_id

   page_id = GLOBAL_OBJECT_ID
   GLOBAL_OBJECT_ID=GLOBAL_OBJECT_ID+1

   call store_page(page_id)
   write(GLOBAL_OUTFILE,'("ET")')

   stream_len = tell_position(GLOBAL_OUTFILE) - GLOBAL_STREAM_START
   write(GLOBAL_OUTFILE,'("endstream",/,"endobj")')

   call start_object(GLOBAL_STREAM_LEN_ID)
   write(GLOBAL_OUTFILE,'(i0,/,"endobj")')stream_len

   call start_object(page_id);
   write(GLOBAL_OUTFILE,'("<</Type/Page/Parent ",i0," 0 R/Contents ",i0," 0 R>>",/,"endobj")')GLOBAL_PAGE_TREE_ID, GLOBAL_STREAM_ID

end subroutine end_page
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
subroutine printstring(buffer)
character(len=*) :: buffer
! Print string as (escaped_string) where ()\ characters have a preceding \ character added
character(len=1) :: c
integer          :: i

   write(GLOBAL_OUTFILE,'(a)',advance='no')'('

   if(GLOBAL_LINENUMBERS )then
      write(GLOBAL_OUTFILE,'(i6.6,1x)')GLOBAL_LINECOUNT
   endif

   do i=1,len(buffer)

      c=char(ichar(buffer(i:i))+GLOBAL_ADD)

      select case(c)
      case ('(',')','\')
         write(GLOBAL_OUTFILE,'("\")',advance='no')
      end select

      write(GLOBAL_OUTFILE,'(a)',advance='no')c

   enddo

   write(GLOBAL_OUTFILE,'(")")',advance='no')

end subroutine printstring
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
function tell_position(lun) result (position)
integer,intent(in) :: lun
integer :: position
integer :: ios
  INQUIRE(UNIT=lun, POS=position,iostat=ios)
  if(ios.ne.0)then
     call stderr('*asa2pdf* cannot determine position of output file')
  endif
  position=position-1
end function tell_position
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
subroutine printme(xvalue,yvalue,string)
real,intent(in)             :: xvalue
real,intent(in)             :: yvalue
character(len=*),intent(in) :: string
   write(GLOBAL_OUTFILE,'("BT /F2 ",f0.6," Tf ",f0.6," ",f0.6," Td")')GLOBAL_TITLE_SIZE,xvalue,yvalue
   call printstring(string)
   write(GLOBAL_OUTFILE,'(" Tj ET")')
end subroutine printme
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
subroutine dopages()
integer :: catalog_id
integer :: font_id0
integer :: font_id1
integer :: start_xref
character(len=34) :: string
integer :: i

write(GLOBAL_OUTFILE,'("%PDF-1.0")')

!   Note: If a PDF file contains binary data, as most do , it is
!   recommended that the header line be immediately followed by a
!   comment line containing at least four binary characters--that is,
!   characters whose codes are 128 or greater. This will ensure proper behavior of file
!   transfer applications that inspect data near the beginning of a
!   file to determine whether to treat the file's contents as text or as binary.

   write(GLOBAL_OUTFILE,'("%",*(a))')char(128),char(129),char(130),char(131)
   write(GLOBAL_OUTFILE,'("% PDF: Adobe Portable Document Format")')

   GLOBAL_LEAD_SIZE=(GLOBAL_PAGE_DEPTH-GLOBAL_PAGE_MARGIN_TOP-GLOBAL_PAGE_MARGIN_BOTTOM)/GLOBAL_LINES_PER_PAGE
   GLOBAL_FONT_SIZE=GLOBAL_LEAD_SIZE

   GLOBAL_OBJECT_ID = 1;
   GLOBAL_PAGE_TREE_ID = GLOBAL_OBJECT_ID
   GLOBAL_OBJECT_ID= GLOBAL_OBJECT_ID +1

   call do_text()

   font_id0 = GLOBAL_OBJECT_ID
   call start_object(font_id0)
   GLOBAL_OBJECT_ID= GLOBAL_OBJECT_ID +1

   write(GLOBAL_OUTFILE,'("<</Type/Font/Subtype/Type1/BaseFont/",a,"/Encoding/WinAnsiEncoding>>")')trim(GLOBAL_FONT)
   write(GLOBAL_OUTFILE,'("endobj")')

   font_id1 = GLOBAL_OBJECT_ID
   call start_object(font_id1)
   GLOBAL_OBJECT_ID= GLOBAL_OBJECT_ID +1

   write(GLOBAL_OUTFILE,'("<</Type/Font/Subtype/Type1/BaseFont/",a,"/Encoding/WinAnsiEncoding>>")')trim(GLOBAL_FONT)
   write(GLOBAL_OUTFILE,'("endobj")')

   call start_object(GLOBAL_PAGE_TREE_ID)

   write(GLOBAL_OUTFILE,'("<</Type /Pages /Count ",i0)') GLOBAL_NUM_PAGES

   write(GLOBAL_OUTFILE,'("/Kids[")')
   write(GLOBAL_OUTFILE,'(a)') GLOBAL_PAGE_LIST ! '(i0," 0 R",new_line("A"))'
   write(GLOBAL_OUTFILE,'("]")')

   write(GLOBAL_OUTFILE,'("/Resources<</ProcSet[/PDF/Text]/Font<</F0 ",i0," 0 R")') font_id0
   write(GLOBAL_OUTFILE,'("/F1 ",i0," 0 R")') font_id1
   write(GLOBAL_OUTFILE,'(" /F2<</Type/Font/Subtype/Type1/BaseFont/Courier-Bold/Encoding/WinAnsiEncoding >> >>")')

   !write(GLOBAL_OUTFILE,'(">>/MediaBox [ 0 0 ",f0.6,1x,f0.6," ]")') GLOBAL_PAGE_WIDTH, GLOBAL_PAGE_DEPTH
   write(GLOBAL_OUTFILE,'(">>/MediaBox [ 0 0 ",a,1x,a," ]")') v2s(GLOBAL_PAGE_WIDTH), v2s(GLOBAL_PAGE_DEPTH)

   write(GLOBAL_OUTFILE,'(">>")')
   write(GLOBAL_OUTFILE,'("endobj")')

   catalog_id = GLOBAL_OBJECT_ID
   GLOBAL_OBJECT_ID= GLOBAL_OBJECT_ID +1
   call start_object(catalog_id)
   write(GLOBAL_OUTFILE,'("<</Type/Catalog/Pages ",i0," 0 R>>")') GLOBAL_PAGE_TREE_ID
   write(GLOBAL_OUTFILE,'("endobj")')
   start_xref = tell_position(GLOBAL_OUTFILE)
   write(GLOBAL_OUTFILE,'("xref")')
   write(GLOBAL_OUTFILE,'("0 ",i0)') GLOBAL_OBJECT_ID
   write(GLOBAL_OUTFILE,'("0000000000 65535 f ")')

   do i=1,GLOBAL_OBJECT_ID-1
      read(GLOBAL_DIRECT,'(a)',REC=i) string
      write(GLOBAL_OUTFILE,'(a)',advance='no') trim(string)
   enddo

   write(GLOBAL_OUTFILE,'("trailer")')
   write(GLOBAL_OUTFILE,'("<<")')
   write(GLOBAL_OUTFILE,'("/Size ",i0)') GLOBAL_OBJECT_ID
   write(GLOBAL_OUTFILE,'("/Root ",i0," 0 R")') catalog_id
   write(GLOBAL_OUTFILE,'(">>")')
   write(GLOBAL_OUTFILE,'("startxref")')
   write(GLOBAL_OUTFILE,'(i0)') start_xref
   write(GLOBAL_OUTFILE,'("%%EOF")')
end subroutine dopages
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
 subroutine start_page()
   GLOBAL_STREAM_ID = GLOBAL_OBJECT_ID
   GLOBAL_OBJECT_ID= GLOBAL_OBJECT_ID +1
   GLOBAL_STREAM_LEN_ID = GLOBAL_OBJECT_ID
   GLOBAL_OBJECT_ID= GLOBAL_OBJECT_ID +1
   GLOBAL_PAGECOUNT= GLOBAL_PAGECOUNT+1
   call start_object(GLOBAL_STREAM_ID)

   write(GLOBAL_OUTFILE,'("<< /Length ",i0," 0 R >>")') GLOBAL_STREAM_LEN_ID
   write(GLOBAL_OUTFILE,'("stream")')

   GLOBAL_STREAM_START = tell_position(GLOBAL_OUTFILE)
   call print_bars()
   call print_margin_label()

   write(GLOBAL_OUTFILE,'("BT")')
   write(GLOBAL_OUTFILE,'("/F0 ",f0.6," Tf")') GLOBAL_FONT_SIZE

   GLOBAL_YPOS = GLOBAL_PAGE_DEPTH - GLOBAL_PAGE_MARGIN_TOP

   write(GLOBAL_OUTFILE,'(f0.6,1x,f0.6," Td")') GLOBAL_PAGE_MARGIN_LEFT, GLOBAL_YPOS
   write(GLOBAL_OUTFILE,'(f0.6," TL")') GLOBAL_LEAD_SIZE

end subroutine start_page
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
subroutine print_margin_label()
character(len=80) :: line
real              :: charwidth
real              :: start
logical           :: hold

hold=GLOBAL_LINENUMBERS
GLOBAL_LINENUMBERS=.false.

call printme_top()

if(GLOBAL_CENTER_TITLE .ne.  '' )then

   ! assuming fixed-space font Courier-Bold
   charwidth=GLOBAL_TITLE_SIZE*0.60
   start=GLOBAL_PAGE_MARGIN_LEFT &
      &    +((GLOBAL_PAGE_WIDTH-GLOBAL_PAGE_MARGIN_LEFT-GLOBAL_PAGE_MARGIN_RIGHT)/2.0) &
      &    -(len_trim(GLOBAL_CENTER_TITLE)*charwidth/2.0)

   call printme(start,GLOBAL_PAGE_DEPTH-GLOBAL_PAGE_MARGIN_TOP+0.12*GLOBAL_TITLE_SIZE,GLOBAL_CENTER_TITLE)
   call printme(start,GLOBAL_PAGE_MARGIN_BOTTOM-GLOBAL_TITLE_SIZE,GLOBAL_CENTER_TITLE)
endif

if(GLOBAL_PAGES)then                                 ! print page numbers on page
   charwidth=GLOBAL_TITLE_SIZE*0.60
   write(line,'("Page ",i0.4)')GLOBAL_PAGECOUNT
   start=GLOBAL_PAGE_WIDTH-GLOBAL_PAGE_MARGIN_RIGHT-(len_trim(line)*charwidth) ! Right Justified
   call printme(start,GLOBAL_PAGE_DEPTH-GLOBAL_PAGE_MARGIN_TOP+0.12*GLOBAL_TITLE_SIZE,line)
   call printme(start,GLOBAL_PAGE_MARGIN_BOTTOM-GLOBAL_TITLE_SIZE,line)
endif

if(GLOBAL_LEFT_TITLE .ne. "" )then
   start=GLOBAL_PAGE_MARGIN_LEFT     ! Left justified
   call printme(start,GLOBAL_PAGE_DEPTH-GLOBAL_PAGE_MARGIN_TOP+0.12*GLOBAL_TITLE_SIZE,GLOBAL_LEFT_TITLE)
   call printme(start,GLOBAL_PAGE_MARGIN_BOTTOM-GLOBAL_TITLE_SIZE,GLOBAL_LEFT_TITLE)
endif

GLOBAL_LINENUMBERS=hold

end subroutine print_margin_label
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
subroutine printme_top()
character(len=256) :: IMPACT_TOP
real :: charwidth
real :: xvalue
real :: yvalue
real :: text_size=20.0
call get_environment_variable('IMPACT_TOP',impact_top)
if( impact_top .ne. '' )then
   charwidth=text_size*0.60                              ! assuming fixed-space font Courier-Bold
   write(GLOBAL_OUTFILE,'("1.0 0.0 0.0 rg")')            ! gray-scale value
   yvalue=GLOBAL_PAGE_DEPTH-text_size
   xvalue=GLOBAL_PAGE_MARGIN_LEFT &
      & +((GLOBAL_PAGE_WIDTH-GLOBAL_PAGE_MARGIN_LEFT-GLOBAL_PAGE_MARGIN_RIGHT)/2.0) &
      & -(len_trim(IMPACT_TOP)*charwidth/2.0)

   write(GLOBAL_OUTFILE,'("BT /F2 ",f0.6," Tf ",f0.6,1x,f0.6," Td")')text_size,xvalue,yvalue
   call printstring(IMPACT_TOP)
   write(GLOBAL_OUTFILE,'(" Tj ET")')

   write(GLOBAL_OUTFILE,'("0.0 0.0 0.0 rg")')            ! gray-scale value
endif
end subroutine printme_top
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
subroutine increment_ypos(mult)
real,intent(in) :: mult
   if (GLOBAL_YPOS < GLOBAL_PAGE_DEPTH - GLOBAL_PAGE_MARGIN_TOP ) then  ! if not at top of page
      GLOBAL_YPOS = GLOBAL_YPOS + GLOBAL_LEAD_SIZE*mult
   endif
end subroutine increment_ypos
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
subroutine store_page(id)
integer,intent(in) :: id
character(len=80) :: string
   write(string,'(i0," 0 R")')id
   GLOBAL_PAGE_LIST = GLOBAL_PAGE_LIST // trim(string) //new_line('A')
   GLOBAL_NUM_PAGES= GLOBAL_NUM_PAGES + 1
end subroutine store_page
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
subroutine start_object(id)
integer,intent(in) :: id
character(len=34)  :: string

   ! record position of start of object in file for writing the reference table at the end
   write(string,'(i10.10," 00000 n ",a)') tell_position(GLOBAL_OUTFILE) ,new_line('a')
   write(GLOBAL_DIRECT,'(a)',rec=id)string

   ! write the beginning of the object definition
   write(GLOBAL_OUTFILE,'(i0," 0 obj")') id

end subroutine start_object
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
subroutine do_text()
character(len=8192) :: buffer
character(len=1)    :: ASA
integer             :: black
integer             :: ios

call start_page()
buffer=' '

INFINITE: do
   read(GLOBAL_INFILE,'(a)',iostat=ios) buffer(GLOBAL_SHIFT+1:)
   if(ios.ne.0)exit INFINITE
   GLOBAL_LINECOUNT= GLOBAL_LINECOUNT+1
   black=0
   GLOBAL_ADD=0

   ! +1 for roundoff , using floating point point units
   if(GLOBAL_YPOS <= (GLOBAL_PAGE_MARGIN_BOTTOM+1) .and. len_trim(buffer) .ne. 0 .and. buffer(1:1) .ne. '+' ) then
      call end_page()
      call start_page()
   endif

   if(len_trim(buffer) .eq. 0)then ! blank line
      write(GLOBAL_OUTFILE,'("T*")')
   else
      ASA=buffer(1:1);

      select case(ASA)

         case ('1')     ! start a new page before processing data on line
            if (GLOBAL_YPOS < GLOBAL_PAGE_DEPTH - GLOBAL_PAGE_MARGIN_TOP )then
               call end_page()
               call start_page()
            endif

         case ('0')        ! put out a blank line before processing data on line
               write(GLOBAL_OUTFILE,'("T*")')
               GLOBAL_YPOS = GLOBAL_YPOS - GLOBAL_LEAD_SIZE

         case ('-')        ! put out two blank lines before processing data on line
            write(GLOBAL_OUTFILE,'("T*")')
            GLOBAL_YPOS = GLOBAL_YPOS - GLOBAL_LEAD_SIZE;
            GLOBAL_YPOS = GLOBAL_YPOS - GLOBAL_LEAD_SIZE;

         case ('+')        ! print at same y-position as previous line
            write(GLOBAL_OUTFILE,'("0 ",f0.6," Td")')GLOBAL_LEAD_SIZE
            call increment_ypos(1.0)

         case ('R','G','B')  ! RED/GREEN/BLUE print at same y-position as previous line
            if(ASA .eq. 'R') write(GLOBAL_OUTFILE,'("1.0 0.0 0.0 rg")') ! red text
            if(ASA .eq. 'G') write(GLOBAL_OUTFILE,'("0.0 1.0 0.0 rg")') ! green text
            if(ASA .eq. 'B') write(GLOBAL_OUTFILE,'("0.0 0.0 1.0 rg")') ! blue text
            black=1
            write(GLOBAL_OUTFILE,'("0 ",f6.0," Td")')GLOBAL_LEAD_SIZE
            call increment_ypos(1.0)

         case ('H')        ! 1/2 line advance
            write(GLOBAL_OUTFILE,'("0 ",f0.6," Td")')GLOBAL_LEAD_SIZE/2.0
            call increment_ypos(0.5)

         case ('r','g','b') ! RED, GREEN, BLUE  print
            if(ASA .eq. 'r') write(GLOBAL_OUTFILE,'("1.0 0.0 0.0 rg")') ! red text
            if(ASA .eq. 'g') write(GLOBAL_OUTFILE,'("0.0 1.0 0.0 rg")') ! green text
            if(ASA .eq. 'b') write(GLOBAL_OUTFILE,'("0.0 0.0 1.0 rg")') ! blue text
            black=1

         case ('^')        ! print at same y-position as previous line like + but add 127 to character
            write(GLOBAL_OUTFILE,'("0 ",f0.6," Td")')GLOBAL_LEAD_SIZE
            call increment_ypos(1.0)
            GLOBAL_ADD=127

         case (char(12))       ! ctrl-L is a common form-feed character on Unix, but NOT ASA
            call end_page()
            call start_page()

         case (' ')
         case default
         call stderr("unknown ASA carriage control character "//ASA)

      end select

      call printstring(trim(buffer(2:)))
      write(GLOBAL_OUTFILE,'("''")')

   endif
   GLOBAL_YPOS = GLOBAL_YPOS - GLOBAL_LEAD_SIZE
   if(black .ne. 0)then
      write(GLOBAL_OUTFILE,'("0.0 0.0 0.0 rg")') ! black text
   endif

   enddo INFINITE
   call end_page()
end subroutine do_text
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
end program asa2pdf
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
