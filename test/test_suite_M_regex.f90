program test_program
use M_regex, only: regex_type, regcomp, regexec, regmatch, regfree, regerror
use M_verify, only: unit_check, unit_check_good, unit_check_bad, unit_check_done, unit_check_start, unit_check_level
use M_verify, only : unit_check_command, unit_check_keep_going, unit_check_level, unit_check_stop
implicit none
 
type(regex_type)             :: regex
integer,parameter            :: maxmatch=10
integer                      :: matches(2,maxmatch)

character(len=:),allocatable :: input_line
character(len=:),allocatable :: expression
logical                      :: match
integer                      :: i

   expression= "([0-9\.\-\*\/]+)+"
   input_line= "30*0 250*1 5 6 7"
   call regcomp(regex,expression,'x')
   match=regexec(regex,input_line,matches)
   write(*,*)'MATCH=',match
   DO i=1,maxmatch
      if(matches(1,i).le.0)exit
      write(*,*) 'match="',regmatch(i,input_line,matches),'"'
   ENDDO

   unit_check_keep_going=.true.
!  unit_check_level=1
   unit_check_level=0
   call regfree(regex)

   call test_regex()
   call unit_check_stop()
contains

subroutine test_regex()
call unit_check_start('M_regex')
!              "Regexp,      String,          expected result"
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
call mymatch(".+o",        "FooBar",        .true.   )
call mymatch("o+o",        "FooBar",        .true.   )
call mymatch("P+o",        "FooBar",        .false.  )
call mymatch("Fo+o",       "FooBar",        .true.   )
call mymatch("Po+o",       "FooBar",        .false.  )
call mymatch(".?o",        "FooBar",        .true.   )
call mymatch("o?o",        "FooBar",        .true.   )
call mymatch("P?o",        "FooBar",        .true.   )
call mymatch("Fo?o",       "FooBar",        .true.   )
call mymatch("Po?o",       "FooBar",        .false.  )
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
call mymatch("F[po]?o",    "FooBar",        .true.   )
call mymatch("F[56]?o",    "F5oBar",        .true.   )
call mymatch("F[46]?o",    "F5oBar",        .false.  )
call mymatch("F[46]?5",    "F5oBar",        .true.   )
call mymatch("F[46]?5o",   "F5oBar",        .true.   )
call mymatch("F[op]?o",    "FooBar",        .true.   )
call mymatch("F[qp]?o",    "FooBar",        .true.   )
call mymatch("P[qp]?o",    "FooBar",        .false.  )
call mymatch("F[^po]?o",   "FooBar",        .true.   )
call mymatch("F[^op]?o",   "FooBar",        .true.   )
call mymatch("F[^qp]?o",   "FooBar",        .true.   )
call mymatch("P[^qp]?o",   "FooBar",        .false.  )
call mymatch("F[po]+o",    "FooBar",        .true.   )
call mymatch("F[56]+o",    "F5oBar",        .true.   )
call mymatch("F[46]+o",    "F5oBar",        .false.  )
call mymatch("F[46]+5",    "F5oBar",        .false.  )
call mymatch("F[46]+5o",   "F5oBar",        .false.  )
call mymatch("F[op]+o",    "FooBar",        .true.   )
call mymatch("F[qp]+o",    "FooBar",        .false.  )
call mymatch("P[qp]+o",    "FooBar",        .false.  )
call mymatch("F[^po]+o",   "FooBar",        .false.  )
call mymatch("F[^op]+o",   "FooBar",        .false.  )
call mymatch("F[^qp]+o",   "FooBar",        .true.   )
call mymatch("P[^qp]+o",   "FooBar",        .false.  )
call mymatch("[0-9]+\.[0-9]*",   "1.9",           .true.   )
call mymatch("[0-9]+\.[0-9]*",   "1.99",          .true.   )
call mymatch("[0-9]+\.[0-9]*",   "1.999",         .true.   )
call mymatch("[0-9]+\.[0-9]*",   "1.9999",        .true.   )
call mymatch("[0-9]+\.[0-9]*",   "1.99999",       .true.   )
call mymatch("[0-9]+\.[0-9]*",   "11.99999",      .true.   )
call mymatch("[0-9]+\.[0-9]*",   "111.99999",     .true.   )
call mymatch("[0-9]+\.[0-9]*",   "1111.99999",    .true.   )
call mymatch("[0-9]+\.[0-9]*",   "11111.99999",   .true.   )
call mymatch("[0-9]+\.[0-9]*",   "111111.99999",  .true.   )
call mymatch("^[0-9]+\.[0-9]*",  "1.9",           .true.   )
call mymatch("^[0-9]+\.[0-9]*",  "1.99",          .true.   )
call mymatch("^[0-9]+\.[0-9]*",  "1.999",         .true.   )
call mymatch("^[0-9]+\.[0-9]*",  "1.9999",        .true.   )
call mymatch("^[0-9]+\.[0-9]*",  "1.99999",       .true.   )
call mymatch("^[0-9]+\.[0-9]*",  "11.99999",      .true.   )
call mymatch("^[0-9]+\.[0-9]*",  "111.99999",     .true.   )
call mymatch("^[0-9]+\.[0-9]*",  "1111.99999",    .true.   )
call mymatch("^[0-9]+\.[0-9]*",  "11111.99999",   .true.   )
call mymatch("^[0-9]+\.[0-9]*",  "111111.99999",  .true.   )
call mymatch("a[0-9]+\.[0-9]*",  "a1.9",          .true.   )
call mymatch("a[0-9]+\.",     "a1.9",          .true.   )
call mymatch("a[0-9]+",       "a1.9",          .true.   )
call mymatch("a",          "a1.9",          .true.   )
call mymatch("\\",         "\",             .true.   )
call mymatch("\.",         "\",             .false.  )
call mymatch(".",          "\",             .true.   )
call mymatch("F[qpo", "FooBar", .false.) ! intentional bad REGEX

call unit_check_done('M_regex')
end subroutine test_regex

subroutine mymatch(expression,string,expected)
use, intrinsic :: iso_fortran_env, only : ERROR_UNIT
use M_verify, only: unit_check, unit_check_good, unit_check_bad, unit_check_done, unit_check_start, unit_check_level
character(len=*),intent(in) :: expression
character(len=*),intent(in) :: string
logical,intent(in)          :: expected
   type(regex_type)             :: regex
   integer,parameter            :: maxmatch=10
   integer                      :: matches(2,maxmatch)
   logical                      :: match
   integer                      :: istat
   integer                      :: i
   call regcomp(regex,expression,'x',status=istat)
   match=.false.
   if(istat/=0) then
      if(unit_check_level.gt.0)then
         write(ERROR_UNIT,'("runtime error in regcomp(3f):",a,", expression=",a)') regerror(regex,istat),expression
      endif
   else
      match=regexec(regex,string,matches,status=istat)
      if(istat/=0) then
         if(unit_check_level.gt.0)then
            write(ERROR_UNIT,'("runtime error in regexec:(3f)",a)') regerror(regex,istat)
         endif
      else if(match)then
         do i=1,maxmatch
            if(matches(1,i).le.0)exit
            if(unit_check_level.gt.0)then
               write(*,*) 'match="',regmatch(i,string,matches),'"'
            endif
         enddo
      endif
   endif
   call regfree(regex)
   call unit_check('M_regex',match .eqv. expected,msg='for REGEX '//trim(expression)//' and string '//trim(string))
end subroutine mymatch


end program test_program
