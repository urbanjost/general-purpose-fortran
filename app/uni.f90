program uni
! @(#) convert UTF-8 to backslash escape sequences, vice-versa, convert case, ...
use, intrinsic :: iso_fortran_env, only: stdin => input_unit, stderr => error_unit, stdout => output_unit
use, intrinsic :: iso_fortran_env, only: iostat_end, iostat_eor
use M_unicode, only : readline, split, lower, upper, len, trim, isascii
use M_unicode, only : expand_html, reverse_line=>reverse
use M_unicode, only : add_backslash, remove_backslash=>escape
use M_unicode, only : isascii, slurp, repeat, pound_to_box, add_border
use M_unicode, only : ut => unicode_type, assignment(=), ch=>character
use M_unicode, only : operator(==), operator(//)
use M_CLI2,    only : set_mode, set_args, get_args, sgets, specified, files=>unnamed
implicit none
integer                      :: i, j, ulen, alen, iostat, lun, linenum, knd
integer,allocatable          :: ints(:)
type(ut)                     :: line
type(ut),allocatable         :: text(:)
logical                      :: verbose, debug, length, escape, noescape, ucase, lcase, wide
logical                      :: code, allascii, border, html, entities, example, reverse, nofile
character(len=:),allocatable :: filenames(:), style_box, style_border, styles(:)
character(len=*),parameter   :: g0='(*(g0))'
character(len=*),parameter   :: formu= '("char(int(z''",z0,"''),kind=ucs4)":,"// &")'
character(len=*),parameter   :: form2= '("char([",*(i0:,","))'
character(len=256)           :: iomsg
   iomsg=''
   open (unit=stdin, pad='yes')
   call setup()
   DATUM: do i=1,size(filenames)

      if(specified('box'))then
            text=pound_to_box(get_text(filenames(i)),style=style_box)
      endif
      if(specified('border'))then
            if(specified('box'))then
               text=add_border(text,style=style_border)
            else
               text=add_border(get_text(filenames(i)),style=style_border)
            endif
      endif
      if(specified('border').or.specified('box'))then
         call print_text(text)
         cycle DATUM
      endif

      if(nofile)then
         lun=-1
      elseif(filenames(i).eq.'-'.or.filenames(i).eq.'')then
         lun=stdin
      else
         open(newunit=lun,file=filenames(i),action='read',pad='yes',iostat=iostat,iomsg=iomsg)
         if(iostat.ne.0)then
            write(stderr,g0)'<ERROR>*uni*:',trim(iomsg)
            iomsg=''
            cycle
         endif
      endif
      INFINITE: do linenum=1,huge(0)-1
         if(nofile)then
            if(linenum.gt.size(filenames))exit DATUM
            line=filenames(linenum)
         else
            line=readline(lun,iostat=iostat)
            if(iostat.ne.0)exit
         endif
         if(html)     line=expand_html(line)
         if(lcase)    line=lower(line)
         if(ucase)    line=upper(line)
         if(noescape) line=remove_backslash(line)
         if(reverse)  line=reverse_line(line)
         if(escape)   line=add_backslash(line)
         if(code.and.knd==2) then
            ! @(#) generate Fortran statements using KIND='iso_10464' that represents the lines
            if(line.eq.'')then
               write(stdout,g0)'! ISO-10646 ENCODING:',ch(line)
               write(stdout,g0)'character(len=*,kind=ucs4),parameter :: line',linenum,'= ucs4_""'
               write(stdout,g0)
            elseif(isascii(line))then
               write(stdout,g0)'character(len=*,kind=ucs4),parameter :: line',linenum,'= ucs4_"&'
               write(stdout,g0)ch(line%replace('"','""')),'"'
            else
               write(stdout,g0)'! ISO-10646 ENCODING:',ch(line)
               write(stdout,g0)'character(len=*,kind=ucs4),parameter :: line',linenum,'= &'
               write(stdout,form2,advance='no')(line%codepoint(j,j),j=1,len(line))
               write(stdout,g0)'],kind=ucs4)'
               write(stdout,g0)
            endif
         elseif(code) then
            ! @(#) generate Fortran statements using KIND='iso_10464' that represents the lines
            if(line.eq.'')then
               write(stdout,g0) '! ISO-10646 ENCODING:',ch(line)
               write(stdout,g0) 'character(len=*,kind=ucs4),parameter :: line',linenum,'= ucs4_""'
               write(stdout,g0)
            elseif(isascii(line))then
               write(stdout,g0) 'character(len=*,kind=ucs4),parameter :: line',linenum,'= ucs4_"&'
               write(stdout,g0) ch(line%replace('"','""')),'"'
            else
               write(stdout,g0) '! ISO-10646 ENCODING:',ch(line)
               write(stdout,g0) 'character(len=*,kind=ucs4),parameter :: line',linenum,'= &'
               write(stdout,formu)(line%codepoint(j,j),j=1,len(line))
               write(stdout,g0)
            endif
         elseif(length)then
            ulen=len(line)
            alen=len(line%character())
            allascii=isascii(line)
            write(stdout,'(i0.5,1x,i0,1x,a,1x,i0,": ",a)') &
         & linenum,ulen,merge('==','/=',allascii),alen,line%character()
         elseif(wide)then
            ! write and identify lines not composed entirely of ASCII-7
            ints=line
            if(maxval(ints).gt.127)then
               ! write the line number line in brackets
               write(stdout,'(i8,1x,a)')linenum,'['//ch(line)//']'
               ! write the line with all but ASCII7 replaced with escape codes
               write(stdout,'(9x,a)')'['//add_backslash(line)//']'
            endif
         else
            write(stdout,g0)line%character()
         endif
      enddo INFINITE
      if(iostat /= iostat_end)then
         write(stdout,g0)'<ERROR> failed on read of input line ',linenum,':',line%character()
      endif
   enddo DATUM
contains
subroutine setup()
!! Put everything to do with command parsing here
character(len=:),allocatable :: help_text(:), version_text(:)
integer                      :: startrange, endrange
integer                      :: i
character(len=*),parameter   :: &

   & form = '(1x,"char(int(z''",z0,"''),kind=ucs4)":,"// &")', &
   & form_M = '(1x,"int(z''",z0,"'')":,", &")'               , &
   & form_zhtml = '(1x,*(:"&#x",z0,";"))'                    , &
   & form_html = '(1x,*(:"&#",i0,";"))'

type(ut)                     :: ustr
help_text=[ CHARACTER(LEN=128) :: &
'NAME                                                                            ',&
'   uni(1f) - [CONVERSION] identify and convert and format Unicode-related text  ',&
'   (LICENSE:PD)                                                                 ',&
'                                                                                ',&
'SYNOPSIS                                                                        ',&
'    uni [--escape|--noescape] [--lcase|--ucase] --html --reverse |              ',&
'    [ [--box STYLE | --border STYLE]                                            ',&
'    --start STARTCODE --finish ENDCODE |                                        ',&
'    --code [--styles NAMES] |                                                   ',&
'    --wide |                                                                    ',&
'    --length |                                                                  ',&
'    --entities |                                                                ',&
'    --example |                                                                 ',&
'    --text |                                                                    ',&
'    infile(s)                                                                   ',&
'                                                                                ',&
'To see short names and defaults enter "uni --usage"                             ',&
'                                                                                ',&
'DESCRIPTION                                                                     ',&
'   uni performs operations such as                                              ',&
'                                                                                ',&
'   + converting between UTF-8 and ASCII-7 C-style escape sequences              ',&
'   + changing case of multi-byte characters                                     ',&
'   + drawing box characters using "#" characters                                ',&
'   + displaying ranges of Unicode characters in several common formats          ',&
'     for use in generating code or text or HTML                                 ',&
'   + locating multi-byte characters in what is primarily an ASCII file          ',&
'   + converting html entity characters to UTF-8                                 ',&
'   + identifying sundry UTF-8 encoded text.                                     ',&
'                                                                                ',&
'   uni(1) defaults to displaying only lines containing a wide                   ',&
'   (ie. multi-byte) character along with the line number; with each line        ',&
'   as-is and then with wide characters converted to C++-style escape            ',&
'   sequences. That is, the default is "uni --wide".                             ',&
'                                                                                ',&
'   The primary Unicode block for the Greek alphabet is the Greek                ',&
'   and Coptic section (U+0370-U+03FF; standard letters, numbers, and            ',&
'   symbols) which contains most modern monotonic Greek letters while            ',&
'   the Greek Extended block (U+1F00-U+1FFF; additional characters with          ',&
'   diacritics). is used for polytonic Greek. So to see the basic Greek          ',&
'   alphabet enter                                                               ',&
'                                                                                ',&
'       uni --start 880 --finish 1023                                            ',&
'                                                                                ',&
'   Key details about the Unicode codespace:                                     ',&
'                                                                                ',&
'   While the UTF-8 encoding scheme is theoretically capable of                  ',&
'   representing much larger codepoints (up to 0x7FFFFFFF), it was               ',&
'   restricted by RFC 3629 to stop at U+10FFFF (1 114 111, in decimal)           ',&
'   to match the Unicode standard''s UTF-16 constraint.                          ',&
'                                                                                ',&
'   **Valid vs. Assigned**: Not all code points within the supported             ',&
'   range are assigned to characters or are valid for use. Some ranges           ',&
'   are reserved for private use and non-characters. As of January 2024,         ',&
'   only a small fraction (fewer than 4%) of the possible code points            ',&
'   had assigned meanings.                                                       ',&
'                                                                                ',&
'   This limit ensures compatibility with the UTF-16 encoding, which uses        ',&
'   surrogate pairs to represent characters beyond the Basic Multilingual        ',&
'   Plane (BMP).                                                                 ',&
'                                                                                ',&
'OPTIONS                                                                         ',&
'   BASIC CONVERSION                                                             ',&
'                                                                                ',&
'   --escape,E    convert non-ASCII7 characters to C-style escape sequences      ',&
'   --noescape,N  convert C-style escape sequences to UTF8 encoded data          ',&
'                                                                                ',&
'   --html,H      expand HTML character entities of the form &NAME; and          ',&
'                 &#NNNNN;.                                                      ',&
'                                                                                ',&
'   --reverse,R   reverse the glyphs on a line                                   ',&
'                                                                                ',&
'   --lcase,L     convert uppercase to lowercase                                 ',&
'   --ucase,U     convert lowercase to uppercase                                 ',&
'                                                                                ',&
'   IDENTIFY AND QUANTIFY INPUT                                                  ',&
'   --length,L    prefix lines with line number, glyph and byte count            ',&
'                 of input line.                                                 ',&
'                                                                                ',&
'   --wide,W      identify and write lines not composed entirely of ASCII-7.     ',&
'                 If no parameters are specified this is the default.            ',&
'   FORMATTING                                                                   ',&
'   --code,C      write as Fortran code using KIND=ISO_10646                     ',&
'                                                                                ',&
'   --styles,s NAMES  Display style name(s) for "--code" option. Default         ',&
'                     is all styles. The "test" style just streams the           ',&
'                     UTF-8 values of the specified values. For other            ',&
'                     allowed names ("decimal", "utf8", "c", "standard",         ',&
'                     "htmlx", "htmld", "ucs4", "codex", "hex") see the          ',&
'                     following section "STYLES". Multiple names separated       ',&
'                     by commas are allowed.                                     ',&
'                                                                                ',&
'   --box,B STYLE box style choice from set {"light","bold","double"}.           ',&
'                 Causes pound character to be used to construct boxes           ',&
'                 using box characters.                                          ',&
'                                                                                ',&
'                 Input characters are assumed to be monospaced.                 ',&
'                                                                                ',&
'                 If specified other non-conversion options are ignored          ',&
'                 except --border.                                               ',&
'                                                                                ',&
'   --border,b STYLE  place box around text, choosing box style from set         ',&
'                     {"light","bold","double"}.  Input characters are           ',&
'                     assumed to be monospaced.                                  ',&
'                                                                                ',&
'                     If specified other non-conversion options are ignored.     ',&
'                     except --box.                                              ',&
'   MODES                                                                        ',&
'   --verbose,V   echo the input as well as the computed values                  ',&
'   --text,t      strings on the command that would be treated as filenames      ',&
'                 are treated as text instead.                                   ',&
'                                                                                ',&
'   INFORMATION                                                                  ',&
'   --start,S     starting codepoint to generate a list of glyphs from.          ',&
'                                                                                ',&
'                 If specified other options are ignored except --finish.        ',&
'                                                                                ',&
'   --finish,F    ending codepoint to generate a list of glyphs from.            ',&
'                 1 114 111, is the highest value that can be represented        ',&
'                 using a single or a pair of 16-bit code units in the           ',&
'                 UTF-16 encoding.                                               ',&
'                                                                                ',&
'                 If specified other options are ignored except --start.         ',&
'                                                                                ',&
'   --entities,e  display table of HTML character entities and stop.             ',&
'                 Other parameters are ignored.                                  ',&
'   --example,x   display sample input file and stop.                            ',&
'                 Other parameters are ignored.                                  ',&
'   STANDARD                                                                     ',&
'   --help,h      display this help and exit                                     ',&
'   --usage,u     display state of command options and exit                      ',&
'   --version,v   output version information and exit                            ',&
'                                                                                ',&
'STYLES                                                                          ',&
'                                                                                ',&
'Unicode codepoints are primarily written in hexadecimal, often prefixed         ',&
'with "U+" followed by four to six digits (e.g., U+0041, U+1F600). They          ',&
'represent abstract characters (not glyphs!) across 17 planes, with              ',&
'the Basic Multilingual Plane (BMP) covering most modern text. They are          ',&
'encoded in storage as UTF-8, UTF-16, or UTF-32.                                 ',&
'                                                                                ',&
'The available style names ("decimal", "utf8", "c", "standard", "htmlx",         ',&
'"htmld", "ucs4", "codex", "hex") for the --styles switch, based on common       ',&
'ways to represent Unicode Codepoints.                                           ',&
'                                                                                ',&
'+ DECIMAL                                                                       ',&
'    + the codepoint value in decimal                                            ',&
'+ UTF8                                                                          ',&
'    + The Unicode codepoint value encoded as UTF-8 data                         ',&
'+ STANDARD                                                                      ',&
'    + The standard format is U+ followed by the hexadecimal value.              ',&
'                 Examples: U+0041 (letter ''A''), U+1F600 (😀 emoji).         ',&
'+ C,J,HTMLD,HTMLX,UCS4,CODEX                                                    ',&
'    + Programming Escapes:                                                      ',&
'       + C: Python/C++/Java: \u0041 (4-digit format) or \U0001F600 (8-digit     ',&
'          format).                                                              ',&
'       + J: JavaScript: \u{1F600} (ES6+).                                       ',&
'       + HTMLD,HTMLD: CSS/HTML forms \0041 or &#x1F600;.                        ',&
'       + UCS4: Fortran UCS4 Hex: char(int(''z1f600'',kind=ucs4)                 ',&
'       + CODEX: int(''z1f600'')                                                 ',&
'+ HEX                                                                           ',&
'    + hexadecimal value of codepoint                                            ',&
'+ Other (not supported)                                                         ',&
'    + Normalization Forms:                                                      ',&
'      The same character might be represented as a single code point            ',&
'      (e.g., ñ U+00F1) or via normalization forms (NFC, NFD) which             ',&
'      break it into a base letter (n) and a combining mark (~).                 ',&
'    + UTF-16: 2-byte or 4-byte (surrogate pair) sequences.                      ',&
'    + UTF-32: Fixed 4-byte representation.                                      ',&
'    + UTF-8: 1–4 byte sequences, often seen as 0x byte values                 ',&
'      (e.g., 0xD0A4).                                                           ',&
'                                                                                ',&
'EXAMPLE                                                                         ',&
'  Sample runs:                                                                  ',&
'                                                                                ',&
'   # basic Greek alphabet                                                       ',&
'   uni --start 880 --finish 1023                                                ',&
'                                                                                ',&
'   # test current font                                                          ',&
'   uni --start 32 --finish 1114111 --test                                       ',&
'                                                                                ',&
'   # box characters                                                             ',&
'   # The majority of Unicode box-drawing characters are in the Box              ',&
'   # Drawing block, which runs from decimal code points 9472 to 9599,           ',&
'   # corresponding to hexadecimal U+2500 to U+257F.                             ',&
'                                                                                ',&
'   uni -S 9472 -F 9599                                                          ',&
'                                                                                ',&
'   # find any lines with non-ASCII7 characters                                  ',&
'   uni -W The_Crow_and_the_Fox.utf8                                             ',&
'                                                                                ',&
'   # convert a file with wide characters to C-style escape codes                ',&
'   # (that can be used with M_unicode module).                                  ',&
'   uni --escape <<\end_of_data                                                  ',&
'   七転び八起き。                                                        ',&
'   転んでもまた立ち上がる。                                         ',&
'   くじけずに前を向いて歩いていこう。                          ',&
'   end_of_data                                                                  ',&
'                                                                                ',&
'  Sample output(wrapped):                                                       ',&
'                                                                                ',&
'   >\u4E03\u8EE2\u3073\u516B\u8D77\u304D\u3002                                  ',&
'   >\u8EE2\u3093\u3067\u3082\u307E\u305F\u7ACB\u3061\u4E0A\u304C                ',&
'   \u308B\u3002                                                                 ',&
'   >\u304F\u3058\u3051\u305A\u306B\u524D\u3092\u5411\u3044\u3066                ',&
'   \u6B69\u3044\u3066\u3044\u3053\u3046\u3002                                   ',&
'                                                                                ',&
'   uni --box bold <<\end_of_data                                                ',&
'   #################################                                            ',&
'   # Warning: proceed with caution #                                            ',&
'   #################################                                            ',&
'   end_of_data                                                                  ',&
'                                                                                ',&
'  Sample output:                                                                ',&
'                                                                                ',&
'   ┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓',&
'   ┃ Warning: proceed with caution ┃                                                              ',&
'   ┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛',&
'                                                                                                      ',&
'   uni -t "Warning. Warning Will Robinson!" --border double                                           ',&
'   ┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓',&
'   ┃Warning. Warning Will Robinson!┃                                                              ',&
'   ┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛',&
'   uni -t ''&#128512; &#128516; &#128525; &#128151;'' -H                                              ',&
'   😀 😄 😍 💗                                                                                ',&
'SEE ALSO                                                                                              ',&
'   dos2unix(1)/unix2dos(1), iconv(1)                                                                  ',&
'                                                                                                      ',&
'']
version_text=[ CHARACTER(LEN=128) :: &
'@(#)PROGRAM:     uni(1)            >',&
'@(#)DESCRIPTION: convert UTF-8 encoded text to and from ASCII-7 and C-style escape sequences >',&
'@(#)VERSION:     1.0 2026-02-24       >                                                       ',&
'@(#)AUTHOR:      John S. Urban        >                                                       ',&
'@(#)LICENSE:     Public Domain        >                                                       ',&
'']
   call set_mode([character(len=20) :: 'strict','ignorecase'])
   ! a single call to set_args can define the options and their defaults, set help
   ! text and version information, and crack command line.
   border=.FALSE.
   call set_args( '&
    & --border:b " " &
    & --box:B " " &
    & --code:C F &
    & --entities:e F &
    & --escape:E F &
    & --noescape:N F &
    & --example:x F &
    & --html:H F &
    & --kind:K 1 &
    & --lcase:L F &
    & --ucase:U F &
    & --length:l F &
    & --text:t F &
    & --reverse:R F &
    & --start:S 0 &
    & --finish:F 1114111 &
    & --styles:s "decimal,utf8,c,standard,htmlx,htmld,ucs4,codex,hex" &
    & --wide:W F &
    & --debug:D F', &
    & help_text, version_text)

   if(specified('border')) call get_args('border', style_border )
   if(specified('box'))    call get_args('box',    style_box )

   call get_args('code',     code )
   call get_args('debug',    debug )
   call get_args('entities', entities )
   call get_args('escape',   escape,     'noescape', noescape )
   call get_args('example',  example )
   call get_args('html',     html )
   call get_args('kind',     knd )
   call get_args('lcase',    lcase,      'ucase',    ucase    )
   call get_args('length',   length )
   call get_args('reverse',  reverse )
   call get_args('start',    startrange, 'finish',   endrange )
   call get_args('verbose',  verbose )
   call get_args('wide',     wide )
   call get_args('text',     nofile )
   styles=sgets('styles')
   if( specified('border') ) border=.TRUE.
   if( specified('box') .and. style_box==' ') style_box='bold'
   if( specified('border') .and. style_border==' ') style_border='bold'
   if(entities)then
      line=expand_html()
      stop
   endif
   if(size(files).eq.0)then
      filenames=[" "]
   else
      filenames=files
   endif
   if(example)then
      call example_file()
   endif
   ! process --start and --finish
   if( specified('start')  .and. (.not.specified('finish')) ) endrange=startrange
   if( specified('start') .or. specified('finish') )then
      styles=to_lower(styles)
      if(any(styles.eq.'test'))then
         do i=startrange,max(min(endrange,1114111),0)
            ustr=i
            write(stdout,'(1x,a)',advance='no')ch(ustr) ! character
         enddo
         write(stdout,*)
      else
         do i=startrange,max(min(endrange,1114111),0)
            ustr=i
            do j=1,size(styles)
             select case(styles(j))
              case('decimal'); write(stdout,'(1x,i0)',advance='no') i    ! codepoint
              case('utf8'); write(stdout,'(1x,a)',advance='no')ch(ustr)  ! character
              case('c') ;write(stdout,'(1x,''\U'',z8.8)',advance='no')i  ! \U00000064
              case('hex') ;write(stdout,'(1x,z0)',advance='no')i         ! 64
              case('standard') ;write(stdout,'(1x,''U+'',z0.4)',advance='no')i  ! U+64
              case('htmlx')  ;write(stdout,form_zhtml,advance='no')i     ! &#x4EBA;
              case('htmld')  ;write(stdout,form_html,advance='no')i      ! &#20154;
              case('ucs4')   ;write(stdout,form,advance='no')i           ! char(int(z'nnn',kind=ucs4))
              case('codex')  ;write(stdout,form_M,advance='no')i         ! int(z'nnn')
             end select
            enddo
            write(stdout,*)
         enddo
      endif
      stop
   endif
   ! if no actions specified default to --wide
   if( .not.any(specified([ character(len=20) :: 'border', 'box', 'code', 'entities', &
   & 'escape', 'noescape', 'example', 'html', 'kind', 'lcase', 'ucase', 'length', &
   & 'reverse', 'start', 'finish', 'styles', 'wide'])))then
      wide=.true.
   endif
   if(debug)then
      write(stderr,'(*(g0))')'nofile=',nofile
      write(stderr,'(*(g0))')'size(filenames)=',size(filenames)
      do i=1,size(filenames)
         write(stderr,'(*(g0))')'[',filenames(i),']'
      enddo
   endif
end subroutine setup

subroutine example_file()
integer                                                              :: i
character(len=128),parameter :: example_data(*)=[ CHARACTER(LEN=128) :: &
'The Greek alphabet consists of 24 letters, from Alpha to Omega, widely',&
'used in mathematics, science, and engineering. The letters are        ',&
'                                                                      ',&
'   Alpha (Αα), Beta (Ββ), Gamma (Γγ), Delta (Δδ), Epsilon (Εε), Zeta',&
'   (Ζζ), Eta (Ηη), Theta (Θθ), Iota (Ιι), Kappa (Κκ), Lambda        ',&
'   (Λλ), Mu (Μμ), Nu (Νν), Xi (Ξξ), Omicron (Οο), Pi (Ππ), Rho    ',&
'   (Ρρ), Sigma (Σσ/ς), Tau (Ττ), Upsilon (Υυ), Phi (Φφ), Chi       ',&
'   (Χχ), Psi (Ψψ), and Omega (Ωω).                                      ',&
'                                                                              ',&
'Sigma (Σ, σ/ς) (Note: ς is used only at the end of words)                 ',&
'                                                                              ',&
'## As C++ escape sequences                                                                                                ',&
'   Alpha (\u0391\u03B1), Beta (\u0392\u03B2), Gamma (\u0393\u03B3), Delta (\u0394\u03B4), Epsilon (\u0395\u03B5), Zeta    ',&
'   (\u0396\u03B6), Eta (\u0397\u03B7), Theta (\u0398\u03B8), Iota (\u0399\u03B9), Kappa (\u039A\u03BA), Lambda            ',&
'   (\u039B\u03BB), Mu (\u039C\u03BC), Nu (\u039D\u03BD), Xi (\u039E\u03BE), Omicron (\u039F\u03BF), Pi (\u03A0\u03C0), Rho',&
'   (\u03A1\u03C1), Sigma (\u03A3\u03C3/\u03C2), Tau (\u03A4\u03C4), Upsilon (\u03A5\u03C5), Phi (\u03A6\u03C6), Chi       ',&
'   (\u03A7\u03C7), Psi (\u03A8\u03C8), and Omega (\u03A9\u03C9).                                                          ',&
'                                                                                                                          ',&
'Sigma (\u03A3, \u03C3/\u03C2) (Note: \u03C2 is used only at the end of words)                                             ',&
'                                                                                                                          ',&
'## As HTML character entities                                                                                             ',&
'&Alpha;,&alpha;, &Beta;,&beta;, &Gamma;,&gamma;,                                                                          ',&
'&Delta;,&delta;, &Epsilon;,&epsilon;, &Zeta;,&zeta;,                                                                      ',&
'&Eta;,&eta;, &Theta;,&theta;, &Iota;,&iota;,                                                                              ',&
'&Kappa;,&kappa;, &Lambda;,&lambda;, &Mu;,&mu;,                                                                            ',&
'&Nu;,&nu;, &Xi;,&xi;, &Omicron;,&omicron;,                                                                                ',&
'&Pi;,&pi;, &Rho;,&rho;, &Sigma;,&sigma;,                                                                                  ',&
'&Tau;,&tau;, &Upsilon;,&upsilon;, &Phi;,&phi;,                                                                            ',&
'&Chi;,&chi;, &Psi;,&psi;, &Omega;,&omega;                                                                                  ',&
'                                                                                                                          ',&
'']
   write(stdout,'(a)')(trim(example_data(i)),i=1,size(example_data))
   stop
end subroutine example_file

function get_text(fname) result(text_out)
character(len=*),intent(in)  :: fname
type(ut),allocatable         :: text_out(:)
type(ut),allocatable         :: line
character(len=:),allocatable :: iomsg
integer                      :: i

   if(nofile)then
      text_out=[fname]
   else
      text_out=slurp(trim(fname),iomsg=iomsg)

      if(iomsg.ne.'')then
         write(stderr,*)'*uni* failed to load file '//fname
         write(stderr,*) iomsg
      endif
   endif

   if(.not.allocated(text_out))text_out=['']
   do i=1,size(text_out)
      line=text_out(i)
      if(html)     line=expand_html(line)
      if(lcase)    line=lower(line)
      if(ucase)    line=upper(line)
      if(noescape) line=remove_backslash(line)
      if(reverse)  line=reverse_line(line)
      if(escape)   line=add_backslash(line)
      text_out(i)=line
   enddo

end function get_text

subroutine print_text(text)
type(ut),intent(in),allocatable :: text(:)
integer                         :: i

   write(stdout,'(*(a:))',advance='no')(text(i)%character(),new_line('a'),i=1,size(text))

end subroutine print_text

elemental pure function to_lower(str) result (string)
character(*), intent(in)    :: str
character(len(str))         :: string
integer                     :: i
integer,parameter           :: diff = iachar('A')-iachar('a')
   string = str
   ! step thru each letter in the string in specified range
   do concurrent (i = 1:len_trim(str))
      select case (str(i:i))
      case ('A':'Z')
         string(i:i) = achar(iachar(str(i:i))-diff)   ! change letter to miniscule
      case default
      end select
   enddo
end function to_lower

end program uni
