program terminal_attributes
use M_attr,  only : attr, attr_update, attr_mode
use M_CLI2,  only : set_args, sget, iget, remaining, lget, unnamed, specified
implicit none
character(len=*),parameter   :: ident="@(#)tat(1f): read stdin and run it through M_attr::attr to display color"
character(len=1024)          :: line
character(len=:),allocatable :: prefix
integer                      :: iwidth
integer                      :: ios
integer                      :: i
character(len=:),allocatable :: help_text(:), version_text(:)
   line=''
   call setup()
   call set_args(' --style:s "color" --debug F --chars:n 0 -prefix:p " "', help_text,version_text)
   ! if command arguments use those instead of reading stdin
   ! example: tat '<clear><B><w><bo><CSI>12;36f Good Morning! '
   iwidth=iget('chars')
   call attr_mode(sget('style'))
   if(specified('prefix'))then
      prefix=sget('prefix')
   else
      prefix=''
   endif

   if(lget('debug'))then
      write(*,*)'REMAINING:',remaining
      write(*,*)'UNNAMED:  ',unnamed
      write(*,*)'STYLE:    ',sget('style')
      write(*,*)'CHARS:    ',iwidth
   endif

   if(size(unnamed).ne.0)then
      do i=1,size(unnamed)
         write(*,'(a)') attr(unnamed(i),chars=iwidth)
      enddo
   else
      do
         read(*,'(a)',iostat=ios)line
         if(ios.ne.0)exit
         if(len(prefix).ne.0)then
            line=prefix//line
         endif
         write(*,'(a)') attr(trim(line),chars=iwidth)
      enddo
      write(*,'(a)',advance='no') attr('<reset>')
   endif
contains
subroutine setup()
help_text=[character(len=80) :: &
'NAME                                                                           ',&
'    tat(1f) - [M_attr] filter terminal attribute strings                       ',&
'    (LICENSE:MIT)                                                              ',&
'SYNOPSIS                                                                       ',&
'    tat [[string(s)][ --chars N] [ --prefix STR] [ --style MODE] ]|            ',&
'    [ --help| --version]                                                       ',&
'DESCRIPTION                                                                    ',&
'   tat(1) ("Terminal Attributes") is like cat(1), except it processes          ',&
'   special strings in the input specifying terminal attributes such as color   ',&
'   and underlining using an HTML-like syntax via the M_attr(3f) module.        ',&
'                                                                               ',&
'OPTIONS                                                                        ',&
'   STRINGS     if present process and print these strings instead of reading   ',&
'               and processing stdin.                                           ',&
'   --style,s   Set output mode ("color"|"plain"|"raw"). Default is "color".    ',&
'   --chars,n   column to fill background color out to. Default is 0 (zero);    ',&
'               meaning to not pad the lines. Note multi-byte character sets    ',&
'               and non-printable characters will not work properly with this   ',&
'               option, but typical plain ASCII will.                           ',&
'   --prefix,p  string to place in front of input lines from stdin. Typically   ',&
'               used to set background and text color, as with "<B><w><bo>".    ',&
'                                                                               ',&
'   --help      display this help and exit                                      ',&
'   --version   output version information and exit                             ',&
'                                                                               ',&
'EXAMPLES                                                                       ',&
'  Sample commands                                                              ',&
'                                                                               ',&
'     cmd|tat -chars 132 -prefix "<B><w>"                                       ',&
'     cmd|tat "<clear><B><w><bo><CSI>12;36f Good Morning!"                      ',&
'     cmd|tat --chars $COLUMNS --prefix "<B><w><bo>"                            ',&
'LIMITATIONS                                                                    ',&
'AUTHOR                                                                         ',&
'   John S. Urban                                                               ',&
'LICENSE                                                                        ',&
'   MIT                                                                         ',&
'']
version_text=[character(len=80) :: &
'PRODUCT:        GPF (General Purpose Fortran) utilities and examples           ',&
'PROGRAM:        tat(1)                                                         ',&
'DESCRIPTION:    filter applies terminal attributes as defined by M_attr(3f)    ',&
!'VERSION:        1.0, 20210801                                                  ',&
'VERSION:        2.0, 2024-12-31                                                ',&
'AUTHOR:         John S. Urban                                                  ',&
'REPORTING BUGS: http://www.urbanjost.altervista.org/                           ',&
'HOME PAGE:      http://www.urbanjost.altervista.org/index.html                 ',&
'LICENSE:        MIT']
end subroutine setup
end program terminal_attributes
