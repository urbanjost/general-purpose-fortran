!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
program test_suite_M_BRE
use, intrinsic :: iso_fortran_env, only : ERROR_UNIT
use :: M_verify,   only : unit_check, unit_check_good, unit_check_bad, unit_check_done, unit_check_start, unit_check_level
use :: M_verify,   only : unit_check_command, unit_check_keep_going, unit_check_level
use :: M_BRE, only : getpat, match, error
use :: M_BRE, only : MAXPAT, YES, ERR
implicit none
integer,parameter :: HT=9
unit_check_command=''
unit_check_keep_going=.true.
unit_check_level=0
call unit_check_start('M_BRE')
!    mymatch("regexp",     "String",        expected result )
call mymatch("Foo",        "FooBar",        .true.   )
call mymatch("Poo",        "FooBar",        .false.  )
call mymatch("Bar",        "FooBar",        .true.   )
call mymatch("Par",        "FooBar",        .false.  )
call mymatch("Foo",        "Foo",           .true.   )
call mymatch("Fo",         "Foo",           .true.   )
call mymatch("Foo",        "Fo",            .false.  )
call mymatch("ooB",        "FooBar",        .true.   )
call mymatch("ooP",        "FooBar",        .false.  )
call mymatch(".",          "FooBar",        .true.   )
call mymatch("P.",         "FooBar",        .false.  )
call mymatch("^Foo",       "FooBar",        .true.   )
call mymatch("^Bar",       "FooBar",        .false.  )
call mymatch("Foo$",       "FooBar",        .false.  )
call mymatch("Bar$",       "FooBar",        .true.   )
call mymatch(".*o",        "FooBar",        .true.   )
call mymatch("o*o",        "FooBar",        .true.   )
call mymatch("P*o",        "FooBar",        .true.   )
call mymatch("Fo*o",       "FooBar",        .true.   )
call mymatch("Po*o",       "FooBar",        .false.  )
call mymatch("F[po]o",     "FooBar",        .true.   )
call mymatch("F[op]o",     "FooBar",        .true.   )
call mymatch("F[qp]o",     "FooBar",        .false.  )
call mymatch("F[^po]o",    "FooBar",        .false.  )
call mymatch("F[^op]o",    "FooBar",        .false.  )
call mymatch("F[^qp]o",    "FooBar",        .true.   )
call mymatch("F[po]*o",    "FooBar",        .true.   )
call mymatch("F[56]*o",    "F5oBar",        .true.   )
call mymatch("F[46]*o",    "F5oBar",        .false.  )
call mymatch("F[46]*5",    "F5oBar",        .true.   )
call mymatch("F[46]*5o",   "F5oBar",        .true.   )
call mymatch("F[op]*o",    "FooBar",        .true.   )
call mymatch("F[qp]*o",    "FooBar",        .true.   )
call mymatch("P[qp]*o",    "FooBar",        .false.  )
call mymatch("F[^po]*o",   "FooBar",        .true.   )
call mymatch("F[^op]*o",   "FooBar",        .true.   )
call mymatch("F[^qp]*o",   "FooBar",        .true.   )
call mymatch("P[^qp]*o",   "FooBar",        .false.  )

call mymatch("[0-9][0-9]*$",  "0123456789",  .true.  )
call mymatch("[0-9][0-9]*$",  "A0123456789", .true.  )
call mymatch("^[0-9][0-9]*$", "A0123456789", .false. )
call mymatch("^[0-9][0-9]*$", "",            .false. )
call mymatch("^[0-9]$", "",                  .false. )
call mymatch("^[0-9]*$", "",                 .true.  )
call mymatch("^$", "",                        .true. )
call mymatch("^$", " ",                       .false.)
call mymatch("^[A-Z ][A-Z ]*$", "",          .false. )
call mymatch("^[ ]*[A-Z][A-Z ]*$", " THIS IS ALL UPPERCASE",    .true.   )
call mymatch("^[ ]*[a-z][a-z ]*$", " this is all lowercase",    .true.   )
call mymatch("^[ ]*[A-Z][A-Z ]*$", " THIS IS not ALL UPPERCASE",    .false.  )
call mymatch("^[ ]*[a-z][a-z ]*$", " this is NOT all lowercase",    .false.  )

! check dash in character class at beginning and end instead of in range
call mymatch("X[-+]Y", "X-Y",                        .true. )
call mymatch("X[-+]Y", "X+Y",                        .true. )
call mymatch("X[+-]Y", "X-Y",                        .true. )
call mymatch("X[-+]Y", "Y-X",                        .false. )
call mymatch("X[-+]Y", "Y+X",                        .false. )
call mymatch("X[+-]Y", "Y-X",                        .false. )
call mymatch("X[+-]Y", "Y+X",                        .false. )
call mymatch("X[+-]Y", "X+Y",                        .true. )
! tabs
call mymatch("X\tY", "X"//char(HT)//"Y",             .true. )
call mymatch("X[\tab]Y", "X"//char(HT)//"Y",         .true. )
call mymatch("X[\tab]Y", "XtY",                      .false. )
call mymatch("X[\tab]Y", "XaY",                      .true. )

call mymatch("[0-9][0-9]*\.[0-9]*",   "1.9",           .true.   )
call mymatch("[0-9][0-9]*\.[0-9]*",   "1.99",          .true.   )
call mymatch("[0-9][0-9]*\.[0-9]*",   "1.999",         .true.   )
call mymatch("[0-9][0-9]*\.[0-9]*",   "1.9999",        .true.   )
call mymatch("[0-9][0-9]*\.[0-9]*",   "1.99999",       .true.   )
call mymatch("[0-9][0-9]*\.[0-9]*",   "11.99999",      .true.   )
call mymatch("[0-9][0-9]*\.[0-9]*",   "111.99999",     .true.   )
call mymatch("[0-9][0-9]*\.[0-9]*",   "1111.99999",    .true.   )
call mymatch("[0-9][0-9]*\.[0-9]*",   "11111.99999",   .true.   )
call mymatch("[0-9][0-9]*\.[0-9]*",   "123456.99999",  .true.   )
call mymatch("^[0-9][0-9]*\.[0-9]*",  "1.9",           .true.   )
call mymatch("^[0-9][0-9]*\.[0-9]*",  "1.99",          .true.   )
call mymatch("^[0-9][0-9]*\.[0-9]*",  "1.999",         .true.   )
call mymatch("^[0-9][0-9]*\.[0-9]*",  "1.9999",        .true.   )
call mymatch("^[0-9][0-9]*\.[0-9]*",  "1.99999",       .true.   )
call mymatch("^[0-9][0-9]*\.[0-9]*",  "11.99999",      .true.   )
call mymatch("^[0-9][0-9]*\.[0-9]*",  "111.99999",     .true.   )
call mymatch("^[0-9][0-9]*\.[0-9]*",  "1111.99999",    .true.   )
call mymatch("^[0-9][0-9]*\.[0-9]*",  "11111.99999",   .true.   )
call mymatch("^[0-9][0-9]*\.[0-9]*",  "111111.99999",  .true.   )
call mymatch("a[0-9][0-9]*\.[0-9]*",  "a1.9",          .true.   )
call mymatch("a[0-9][0-9]*\.",        "a1.9",          .true.   )
call mymatch("a[0-9][0-9]*",          "a1.9",          .true.   )
call mymatch("a",                "a1.9",          .true.   )
call mymatch("\\",               "\",             .true.   )
call mymatch("\.",               "\",             .false.  )
call mymatch(".",                "\",             .true.   )
call mymatch("F[qpo", "FooBar", .false.) ! intentional bad regex

call unit_check_done('M_BRE')
contains
!===================================================================================================================================
subroutine mymatch(expression,string,expected)
character(len=*),intent(in) :: expression
character(len=*),intent(in) :: string
logical,intent(in)          :: expected
integer                     :: jj
logical                     :: answer
integer                     :: pat(MAXPAT)

   jj=getpat(expression,pat)
   if(jj .eq. ERR) then
      call unit_check('M_BRE',.not.expected,&
      &'illegal pattern for regex',expression,'and string',trim(string),'expected',expected,'getpat=',jj)
   else
      answer=match(string,pat).eq.YES
      call unit_check('M_BRE',answer .eqv. expected,&
      &'for regex',expression,'and string',trim(string),'expected',expected,'got',answer,'getpat=',jj)
   endif

end subroutine mymatch
!===================================================================================================================================
end program test_suite_M_BRE
