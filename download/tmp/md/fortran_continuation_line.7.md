[UP]

-----------------------------------------------------------------------------------------------------------------------------------
                                      Manual Reference Pages  - fortran_continuation_line (7)
-----------------------------------------------------------------------------------------------------------------------------------
                                                                 
NAME

    fortran_continuation_line(7f) - [FORTRAN] Fortran Continuation Lines

CONTENTS

    Description
    Specifically

DESCRIPTION

    If a statement is too long to fit on a line, it can be continued with the following methods: If a line is ended with an
    ampersand, &, it will be continued on the next line. Continuation is normally to the first character of the next non-comment
    line.

       A = 174.5 * Year   &
           + Count / 100

    The above is equivalent to the following

       A = 174.5 * Year  + Count / 100



    Note that & is not part of the statement.

       A = 174.5 * Year   &
       !  this is a comment line
           + Count / 100



    The above is equivalent to the following, since the comment is ignored by the compiler:

       A = 174.5 * Year  + Count / 100



    If the first non-blank character of the continuation line is &, continuation is to the first character after the &:

       A = 174.5 + ThisIsALong&
            &VariableName * 123.45



    is equivalent to

       A = 174.5 + ThisIsALongVariableName * 123.45



    In this case, there should be no spaces between the last character and the & on the first line. For example,

       A = 174.5 + ThisIsALong   &
            &VariableName * 123.45



    is equivalent to

       A = 174.5 + ThisIsALong   VariableName * 123.45



    Note that there are spaces between ThisIsALong and VariableName. In this way, a token (name and number) can be split over two
    lines. However, this is not recommended

SPECIFICALLY

        3.3.2.4 Free form statement continuation

    1.    The character "&" is used to indicate that the current statement is continued on the next line that is not a comment
          line. Comment lines cannot be continued; an "&" in a comment has no effect. Comments may occur within a continued
          statement. When used for continuation, the "&" is not part of the statement. No line shall contain a single "&" as the
          only nonblank character or as the only nonblank character before an "!" that initiates a comment.

    2.    If a noncharacter context is to be continued, an "&" shall be the last nonblank character on the line, or the last
          nonblank character before an "!". There shall be a later line that is not a comment; the statement is continued on the
          next such line. If the first nonblank character on that line is an "&", the statement continues at the next character
          position following that "&"; otherwise, it continues with the first character position of that line.

    3.    If a lexical token is split across the end of a line, the first nonblank character on the first following noncomment line
          shall be an "&" immediately followed by the successive characters of the split token.

    4.    If a character context is to be continued, an "&" shall be the last nonblank character on the line and shall not be
          followed by commentary. There shall be a later line that is not a comment; an "&" shall be the first nonblank character
          on the next such line and the statement continues with the next character following that "&".

    So this is OK:

       POINT=[&   ! define a Point <X,Y,Z>
       & 10, &    ! the X component
       & 20, &    ! the Y component
       & 30  ]    ! the Z component



    because you can have comments after the ampersand when it is not a string. But this is not OK:

       STRING=[ &   ! create a sentence
       & This&      ! first word
       & is&        ! second word
       & sentence&  ! third word
       & a ]        ! forth word (a comment here is OK)



    Because when continuing a string you cannot have a comment after the ampersand. This is OK:

       STRING=[ &
       ! create a sentence
       & This&
       ! first word
       & is&
       ! second word
       & sentence&
       ! third word
       & a ]        ! forth word (a comment here is OK)





    Long strings:

       Subroutine LongString()


       Character (len=200) :: string1, String2
       string1 = "A very long string that won t fit on a single &
                  &line can be made through proper continuation."


       string2 = "A very long string that won t fit on a single " // &
                 "line can be made through proper continuation."
       if (string1 == string2) then
         print *, "string1 and 2 are identical!"
         print *, "string1 & 2=",string1
       else
         print *, "string1 and 2 don t match!"
       endif


       End Subroutine LongString



-----------------------------------------------------------------------------------------------------------------------------------

                                                   fortran_continuation_line (7)                                      July 02, 2017

Generated by manServer 1.08 from dc69f3a2-0d97-40d0-b10c-2ce277bcaf74 using man macros.
                                                           [fortran_co]
