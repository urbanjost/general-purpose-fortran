     program demo_verify
     ! general examples
     use M_unicode, only : assignment(=)
     use M_unicode, only : ut=>unicode_type, ch=>character
     use M_unicode, only : write(formatted)
     use M_unicode, only : operator(==)
     use M_unicode, only : verify, replace
     use M_unicode, only : operator(//)
     implicit none
     ! some useful character sets
     character,parameter          :: &
      & int*(*)   = "1234567890", &
      & low*(*)   = "abcdefghijklmnopqrstuvwxyz", &
      & upp*(*)   = "ABCDEFGHIJKLMNOPQRSTUVWXYZ", &
      & punc*(*)  = "!""#$%&'()*+,‐./:;<=>?@[\]'_‘{|}˜", &
      & blank*(*) = " ", &
      & tab       = char(11), &
      & prnt*(*) = int//low//upp//blank//punc
     !
     type(ut)                     :: stru
     integer                      :: i
         print *, "basics:"
         print *, VERIFY ("ABBA", "A")                ! has the value 2.
         print *, VERIFY ("ABBA", "A", BACK = .TRUE.) ! has the value 3.
         print *, VERIFY ("ABBA", "AB")               ! has the value 0.
        !
        print *,"find first non‐uppercase letter"
        ! will produce the location of "d", because there is no match in UPP
        write(*,*) "something unmatched",verify(ut("ABCdEFG"), upp)
        !
        print *,"if everything is matched return zero"
        ! will produce 0 as all letters have a match
        write(*,*) &
        & "everything matched",verify(ut("ffoorrttrraann"), "nartrof")
        !
        print *,"easily categorize strings as uppercase, lowercase, ..."
        ! C-like functionality but does entire strings not just characters
        write(*,*)"isdigit 123?",verify(ut("123"), int) == 0
        write(*,*)"islower abc?",verify(ut("abc"), low) == 0
        write(*,*)"isalpha aBc?",verify(ut("aBc"), low//upp) == 0
        write(*,*)"isblank aBc dEf?",verify(ut("aBc dEf"), blank//tab ) /= 0
        ! check if all printable characters
        stru="aB;cde,fgHI!Jklmno PQRSTU vwxyz"
        write(*,*)"isprint?",verify(stru,prnt) == 0
        !
        ! this now has a nonprintable tab character in it
        stru=replace(stru,10,10,ut(char(11)))
        write(*,*)"isprint?",verify(stru,prnt) == 0
        !
        print *,"VERIFY(3) is very powerful using expressions as masks"
        ! verify(3) is often used in a logical expression
        stru=" This is NOT all UPPERCASE "
        write(*,*)"all uppercase/spaces?",verify(stru, blank//upp) == 0
        stru=" This IS all uppercase "
        write(*,*) "stru=["//stru//"]"
        write(*,*)"all uppercase/spaces?",verify(stru, blank//upp) == 0
        !
        ! set and show complex stru to be tested
        stru="  Check this out. Let me know  "
        ! show the stru being examined
        write(*,*) "stru=["//stru//"]"
        write(*,*) "        "//repeat(int,4) ! number line
        !
        ! function returns a position just not a logical like C
        print *, "returning a position not just a logical is useful"
        ! which can be very useful for parsing strings
        write(*,*)"first non‐blank character",verify(stru, blank)
        write(*,*)"last non‐blank character",verify(stru, blank,back=.true.)
        write(*,*)"first non‐letter non‐blank",verify(stru,low//upp//blank)
        !
       !VERIFY(3) is elemental (can check an array of strings in one call)
        print *, "elemental"
        ! are strings all letters (or blanks)?
        write(*,*) "array of strings",verify( &
        ! strings must all be same length, so force to length 10
        & [character(len=10) :: "YES","ok","000","good one","Nope!"], &
        & low//upp//blank) == 0
        !
        ! rarer, but the set can be an array, not just the strings to test
        ! you could do ISPRINT() this (harder) way :>
        write(*,*)"isprint?", &
        & .not.all(verify(ut("aBc"), [(char(i),i=32,126)])==1)
        ! instead of this way
        write(*,*)"isprint?",verify(ut("aBc"),prnt) == 0
        !
     end program demo_verify
