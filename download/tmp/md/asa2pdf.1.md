[UP]

-----------------------------------------------------------------------------------------------------------------------------------
                                               Manual Reference Pages  - asa2pdf (1)
-----------------------------------------------------------------------------------------------------------------------------------
                                                                 
NAME

    asa2pdf(1f) - [FILE FILTER]Convert text files with/without ASA carriage control to an Adobe PDF file.

CONTENTS

    Synopsis
    Description
    Usage
    Options
    Environment Variables
    Examples
    See Also

SYNOPSIS

    asa2pdf -o output_filename -i input_filename
    -g gray_scale_shade -b lines_alternately_shaded -d dashcode
    -s top_middle_page_label -t top_left_page_label
    -P # add page numbers
    -l lines_per_page -f font_name -S columns_to_shift_data
    -N # add line numbers
    -H page_height -W page_width -u points_per_unit
    -L left_margin -R right_margin -B bottom_margin -T top_margin
    -help -version -show

DESCRIPTION

    Basically, asa2pdf(1) emulates a line printer that recognizes ASA carriage control. That is, it lets you convert ASCII text
    files using ASA carriage control into Adobe "clear text" PDF files instead of a printed page.

    The PDF is clear-text ASCII so that it is easy to still use other Unix/Linux utilities such as spell(1), diff(1), grep(1), ....
    on the output files.

    To properly view the output requires a PDF processor (such as xpdf(1),acroread(1)/AcroRd32, gv(1) or ghostview(1), ...). Most
    modern systems can view, mail and print PDF files.

    The default layout generates a landscape 132-column 60-line format with every other two lines shaded. A variety of switches are
    available to let you easily print files with no vertical carriage control, and in portrait mode too. There are options to use
    dashed lines instead of shading, to set different margins, and so on.

    WHAT IS ASA CARRIAGE CONTROL?

    The ASA carriage control standard was the first important formatting standard for printing and viewing text files. The standard
    was almost universally adapted by printer manufacturers of the time (and printers were a much more common output device than
    interactive displays).

    Most commercial high-level programs at the time the standard was created were either FORTRAN or COBOL; so nearly all early
    FORTRAN output used ASA carriage control (ASA was the American Standards Association -- now ANSI). This FORTRAN/ASA association
    became so strong that the standard is sometimes referred to as the "Fortran carriage control standard" (FCC). Indeed, even
    though ASA is no longer commonly directly supported on desktop printers, it was part of the Fortran 90 standard (this was
    dropped in Fortran 2003 -- how a printer processes files is really not directly part of any programming language).

    Times have changed, and the once nearly ubiquitous ASA standard is poorly supported on Unix and MSWindows machines in
    particular (Direct operating-system support of ASA files was once common, but is now rare).

    But no alternative as simple has emerged for output files that truly replaces the ASA standard (although machine control
    characters (ctrl-H, ctrl-L, ...) have come close they have their own issues).

    So many programs using ASA-based formatting have not been changed, and use commands like asa(1)/nasa(1), and fpr(1) to allow
    the files to be printed as desired but NOT to generally be viewed properly on-line, and printing itself is becoming less
    common.

    So the problem isn t so much with ASA files, but that today s infrastructure does not support the format well. The asa2pdf(1)
    program bridges the gap by allowing you to still make and manipulate ASA files until you want to print or email them, at which
    time you can quickly convert them to an Adobe PDF file.

USAGE

    asa2pdf(1) reads input from standard input. By default the first character of each line is interpreted as a control character.
    Lines beginning with any character other than those listed in the ASA carriage-control characters table or in the list of
    extensions below are interpreted as if they began with a blank, and an appropriate diagnostic appears on standard error. The
    first character of each line is not printed.

    ASA Carriage Control Characters

            +------------+-----------------------------------------------+
            | Character  |                                               |
            +------------+-----------------------------------------------+
            | +          | Do not advance; overstrike previous line.     |
            | blank      | Advance one line.                             |
            | null lines | Treated as if they started with a blank       |
            | 0          | Advance two lines.                            |
            | -          | Advance three lines (IBM extension).          |
            | 1          | Advance to top of next page.                  |
            | all others | Discarded (except for extensions listed below)|
            +------------+-----------------------------------------------+

    Extensions

           H   Advance one-half line.
           R   Do not advance; overstrike previous line. Use red text color
           G   Do not advance; overstrike previous line. Use green text color
           B   Do not advance; overstrike previous line. Use blue text color
           r   Advance one line. Use red text color
           g   Advance one line. Use green text color
           b   Advance one line. Use blue text color
           ^   Overprint but add 127 to the ADE value of the character
               (ie., use ASCII extended character set)



OPTIONS

         -o outputfile Name of Adobe PDF output file to create

         -i inputfile Name of text file to read. Defaults to stdin.

    PRINTABLE PAGE AREA

    The page size may be specified using -H for height, -W for width, and -u to indicate the points per unit (72 makes H and W in
    inches, 1 is used when units are in font points). For example:

           -u 72 -H 8.5 -W 11   # page Height and Width in inches
           -T 0.5 -B 0.5 -L 0.5 -R 0.5   # margins (Top, Bottom, Left, Right)



    common media sizes with -u 1:

           +-------------------+------+------------+
           | name              |  W   |        H   |
           +-------------------+------+------------+
           | Letterdj (11x8.5) | 792  |       612  | (LandScape)
           | A4dj              | 842  |       595  |
           | Letter (8.5x11)   | 612  |       792  | (Portrait)
           | Legal             | 612  |       1008 |
           | A5                | 420  |       595  |
           | A4                | 595  |       842  |
           | A3                | 842  |       1190 |
           +-------------------+------+------------+



    SHADING

             -g 0.800781 gray-scale value for shaded bars ( 0 < g 1 ) 0 is black, 1 is white.

             -i 2 repeat shade pattern every N lines

             -d   dashcode pattern The pattern is a series of integers defining an on-off sequence in user units used to create a
             dash pattern. A single digit "N" implies a pattern of "N N". (seems buggy)

    MARGIN LABELS

       -s   top middle page label.

       -t   top left page label.

       -P add page numbers to right corners

    TEXT OPTIONS

             -l 60 lines per page

             -f Courier font names: Courier, Courier-Bold,Courier-Oblique Helvetica, Symbol, Times-Bold, Helvetica-Bold,
             ZapfDingbats, Times-Italic, Helvetica-Oblique, Times-BoldItalic, Helvetica-BoldOblique, Times-Roman,
             Courier-BoldOblique

             -S 0 right shift 1 for non-ASA files

             -N add line numbers

    INFORMATION

       -version display version number

       -help display this help

ENVIRONMENT VARIABLES

    o  $IMPACT_TOP Will be printed in large red letters across the page top.

    o  $IMPACT_GRAY sets the default gray-scale value, same as the -g switch.

EXAMPLES

    Sample input:

      > The numbers are plain underlined double-struck over-struck
      >+                      __________ double-struck ///////////
      >R                                               ///////////
      > abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789-=_+()*&^%$#@!\|[]{}; :",.<>/? ~
      >
      >r red
      >g     green
      >b           blue
      > PRIMARY:
      >R         red
      >G             green
      >B                   blue
      > 1/2 line advance
      >H                1
      >H                 2         a-1
      >H                  3       Z
      >H                   4       b
      > back to a normal line



    Sample commands:

         # use non-ASA file to generate portrait mode with a dashed line under every line
         asa2pdf -S 1 -W 8.5 -H 11 -i 1 -d  2 4 1  -T 1 -B .75 -o paper.pdf < INFILE


         # banner on top
         env IMPACT_GRAY=1 IMPACT_TOP=CONFIDENTIAL asa2pdf -o paper.pdf < test.txt


         # 132-column landscape
          asa2pdf -s LANDSCAPE -o paper.pdf <asa2pdf.c


         # 132-column landscape with line numbers with dashed lines
          asa2pdf -s  LANDSCAPE LINE NUMBERS  -d  3 1 2  \
          -N -T .9 -o paper.pdf <asa2pdf.c


         # portrait 80-column non-ASA file with dashed lines
          asa2pdf -s PORTRAIT -S 1 -W 8.5 -H 11 -i 1 -d  2 4 1  \
          -T 1 -B .75 -o paper.pdf < asa2pdf.c


         # portrait 80-column with line numbers , non-ASA
          asa2pdf -s  PORTRAIT LINE NUMBERS  -l 66 -S 1 -W 8.5 -H 11 \
          -i 1 -T 1 -B .75 -N -o paper.pdf < asa2pdf.c


         # titling
          asa2pdf -d  1 0 1  -t "$USER" -i 1 -P -N -T 1 \
          -s "asa2pdf.c" -o paper.pdf <asa2pdf.c



SEE ALSO

    ALTERNATIVES TO ASA2PDF

    About the only standard ASA support on Unix variants is that some contain the asa(1)/fpr(1) and nasa(1) commands for converting
    ASA text files into and from text files with machine control (MC) characters such as form-feed, backspace, carriage-return,
    .... Most personal printers will no longer properly print ASA files directly, but they will often correctly print files with
    simple MC characters (Note that the asa(1) command is referenced in the POSIX.2 standard).

    Furthermore, if a printer does not directly support MC characters, text conversion utilities such as enscript(1) and a2ps(1)
    can often be used to print the files (usually by converting the files to PostScript or PCL). Such utilities support features
    such as titling, page numbering, and other useful options.

    Programs like "Adobe Distiller" can convert text to a PDF; as well as editors such as OpenOffice. In fact, most modern
    document-formatting editors can read in an ASCII text file and save it as an Adobe PDF file.

    HTML and PostScript/PDF and PCL files are the alternatives often incorporated to satisfy simple formatting criteria -- yet HTML
    is not printer-oriented; and PDF files are complex to write from a simple program, and PCL is vendor-specific and has few on-
    line viewers available for it.

    Assuming converting the Fortran program to just write a plain ASCII file instead of an ASA file is not acceptable, More
    extensive flat-text formatting is available using

    o   HTML, *roff and LaTex-related file formats

    o   libraries for writing more sophisticated PostScript, PDF, and HTML/CSS files

    o   XML files formatted using Cascading Style Sheet (CSS) files

    o   RTF (Rich Text Format) files

    Other Unix commands that can be useful in working with plain text and MC character files are

          pr(1)           can be used to add page numbers and titles.
          expand(1)       can remove tab characters
          fold(1),fmt(1)  can be used to wrap the text
          cut(1)          can let you trim or select columns
          cat -n          can be used to add number lines
          paste(1)        can be used to put files side-by-side.



asa(1)/nasa(1), fpr(1), enscript(1), a2ps(1), and ps2pdf(1).

-----------------------------------------------------------------------------------------------------------------------------------

                                                            asa2pdf (1)                                               July 02, 2017

Generated by manServer 1.08 from 5a78ce55-3b0b-44de-83d7-f2392ceda3f9 using man macros.
                                                             [asa2pdf]
