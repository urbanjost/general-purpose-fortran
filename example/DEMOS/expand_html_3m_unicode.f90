     program demo_expand_html
     use iso_fortran_env, only : stdout => output_unit
     use M_unicode,       only : expand_html, unicode_type, assignment(=)
     use M_unicode,       only : ut => unicode_type, operator(==)
     use M_unicode,       only : ch => character
     implicit none
     character(len=*),parameter :: g='(*(g0))'
     integer                    :: i
     character(len=*),parameter :: data(*)=[character(len=132) :: &
     '             HTML Character Entity Test Page', &
     '   Description     Entity  Entity  Rendered ', &
     '                   Name    Number  Result', &
     'Less than          &amp;lt;    &amp;#60;   <&lt;&#60;', &
     'Greater than       &amp;gt;    &amp;#62;   >&gt;&#62;', &
     'Ampersand          &amp;amp;   &amp;#38;   &amp;&amp;&#38;', &
     'Copyright          &amp;copy;  &amp;#169;  ©&copy;&#169;', &
     'Registered         &amp;reg;   &amp;#174;  ®&reg;&#174;', &
     'Trademark          &amp;trade; &amp;#8482; ™&trade;&#8482;', &
     'Euro               &amp;euro;  &amp;#8364; €&euro;&#8364;', &
     'Pound              &amp;pound; &amp;#163;  £&pound;&#163;', &
     'Non-breaking space &amp;nbsp;  &amp;#160;  Before &nbsp;&#160;After']
        do i=1,size(data)
           write(stdout,g)trim(ch(expand_html(data(i))))
        enddo
     end program demo_expand_html
