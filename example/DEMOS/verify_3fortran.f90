      program demo_verify
      implicit none
      ! some useful character sets
      character,parameter :: &
       & int*(*)   = '1234567890', &
       & low*(*)   = 'abcdefghijklmnopqrstuvwxyz', &
       & upp*(*)   = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ', &
       & punc*(*)  = "!""#$%&'()*+,-./:;<=>?@[\]^_`{|}~", &
       & blank*(*) = ' ', &
       & tab       = char(11), &
       & prnt*(*) = int//low//upp//blank//punc

      character(len=:),allocatable :: string
      integer :: i
          print *, 'basics:'
          print *, VERIFY ('ABBA', 'A')                ! has the value 2.
          print *, VERIFY ('ABBA', 'A', BACK = .TRUE.) ! has the value 3.
          print *, VERIFY ('ABBA', 'AB')               ! has the value 0.

         print *,'find first non-uppercase letter'
         ! will produce the location of "d", because there is no match in UPP
         write(*,*) 'something unmatched',verify("ABCdEFG", upp)

         print *,'if everything is matched return zero'
         ! will produce 0 as all letters have a match
         write(*,*) 'everything matched',verify("ffoorrttrraann", "nartrof")

         print *,'easily categorize strings as uppercase, lowercase, ...'
         ! easy C-like functionality but does entire strings not just characters
         write(*,*)'isdigit 123?',verify("123", int) == 0
         write(*,*)'islower abc?',verify("abc", low) == 0
         write(*,*)'isalpha aBc?',verify("aBc", low//upp) == 0
         write(*,*)'isblank aBc dEf?',verify("aBc dEf", blank//tab ) /= 0
         ! check if all printable characters
         string="aB;cde,fgHI!Jklmno PQRSTU vwxyz"
         write(*,*)'isprint?',verify(string,prnt) == 0
         ! this now has a nonprintable tab character in it
         string(10:10)=char(11)
         write(*,*)'isprint?',verify(string,prnt) == 0

         print *,'VERIFY(3) is very powerful using expressions as masks'
         ! verify(3) is often used in a logical expression
         string=" This is NOT all UPPERCASE "
         write(*,*)'all uppercase/spaces?',verify(string, blank//upp) == 0
         string=" This IS all uppercase "
         write(*,*) 'string=['//string//']'
         write(*,*)'all uppercase/spaces?',verify(string, blank//upp) == 0

        ! set and show complex string to be tested
         string='  Check this out. Let me know  '
         ! show the string being examined
         write(*,*) 'string=['//string//']'
         write(*,*) '        '//repeat(int,4) ! number line

         ! the Fortran functions returns a position just not a logical like C
         print *, 'returning a position not just a logical is useful'
         ! which can be very useful for parsing strings
         write(*,*)'first non-blank character',verify(string, blank)
         write(*,*)'last non-blank character',verify(string, blank,back=.true.)
         write(*,*)'first non-letter non-blank',verify(string,low//upp//blank)

        !VERIFY(3) is elemental so you can check an array of strings in one call
        print *, 'elemental'
         ! are strings all letters (or blanks)?
         write(*,*) 'array of strings',verify( &
         ! strings must all be same length, so force to length 10
         & [character(len=10) :: "YES","ok","000","good one","Nope!"], &
         & low//upp//blank) == 0

         ! rarer, but the set can be an array, not just the strings to test
         ! you could do ISPRINT() this (harder) way :>
         write(*,*)'isprint?',.not.all(verify("aBc", [(char(i),i=32,126)])==1)
         ! instead of this way
         write(*,*)'isprint?',verify("aBc",prnt) == 0

      end program demo_verify
