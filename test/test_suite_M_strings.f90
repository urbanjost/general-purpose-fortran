module M_testsuite_M_strings
use,intrinsic :: iso_fortran_env,only : std_in=>input_unit,std_out=>output_unit,std_err=>error_unit
use M_framework__verify
use M_strings
implicit none
character(len=*),parameter :: options=' -section 3 -library libGPF -filename `pwd`/M_strings.FF &
& -documentation y -ufpp   y -ccall  n -archive  GPF.a '
character(len=*),parameter :: g='(*(g0,1x))'
contains
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_suite_m_strings()
   call test_adjustc()
   call test_base()
   call test_base2()
   call test_bundle()
   call test_c2s()
   call test_change()
   call test_chomp()
   call test_clip()
   call test_codebase()
   call test_compact()
   call test_cpad()
   call test_crop()
   call test_dble()
   call test_decodebase()
   call test_delim()
   call test_describe()
   call test_edit_distance()
   call test_expand()
   call test_getvals()
   call test_glob()
   call test_indent()
   call test_int()
   call test_isalnum()
   call test_isalpha()
   call test_isascii()
   call test_isblank()
   call test_iscntrl()
   call test_isdigit()
   call test_isgraph()
   call test_islower()
   call test_isnumber()
   call test_isprint()
   call test_ispunct()
   call test_isspace()
   call test_isupper()
   call test_isxdigit()
   call test_join()
   call test_lenset()
   call test_len_white()
   call test_listout()
   call test_lower()
   call test_lpad()
   call test_match_delimiter()
   call test_merge_str()
   call test_modif()
   call test_m_strings()
   call test_nint()
   call test_noesc()
   call test_nospace()
   call test_notabs()
   call test_pad()
   call test_quote()
   call test_real()
   call test_replace()
   call test_reverse()
   call test_rotate13()
   call test_rpad()
   call test_s2c()
   call test_s2v()
   call test_s2vs()
   call test_setbits()
   call test_split()
   call test_squeeze()
   call test_stretch()
   call test_string_to_value()
   call test_string_to_values()
   call test_strtok()
   call test_substitute()
   call test_switch()
   call test_transliterate()
   call test_trimzeros_()
   call test_unquote()
   call test_upper()
   call test_v2s()
   call test_value_to_string()
   call test_visible()
   call test_zpad()
end subroutine test_suite_m_strings
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_glob()
! This main() routine passes a bunch of test strings into the above code.
! In performance comparison mode, it does that over and over.  Otherwise,
! it does it just once.  Either way, it outputs a passed/failed result.
!
integer :: nReps
logical :: allpassed
integer :: i
   call unit_check_start('glob',' -description ''match string with a pattern containing * and ? wildcard characters'' '//OPTIONS)
  allpassed = .true.

  nReps = 1000000
  nReps = 10    ! Can choose as many repetitions as you're expecting in the real world.

  do i=1,nReps
     ! Cases with repeating character sequences.
     allpassed=  test("a*abab",       "a*b",    .true.)   .and.  allpassed
     allpassed=  test("ab",           "*?",     .true.)   .and.  allpassed
     allpassed=  test("abc",          "*?",     .true.)   .and.  allpassed
     allpassed=  test("abcccd",       "*ccd",   .true.)   .and.  allpassed
     allpassed=  test("bLah",         "bLaH",   .false.)  .and.  allpassed
     allpassed=  test("mississippi",  "*sip*",  .true.)   .and.  allpassed
     allpassed= &
      & test("xxxx*zzzzzzzzy*f", "xxx*zzy*f", .true.) .and. allpassed
     allpassed= &
      & test("xxxx*zzzzzzzzy*f", "xxxx*zzy*fffff", .false.) .and. allpassed
     allpassed= &
      & test("mississipissippi", "*issip*ss*", .true.) .and. allpassed
     allpassed= &
      & test("xxxxzzzzzzzzyf", "xxxx*zzy*fffff", .false.) .and. allpassed
     allpassed= &
      & test("xxxxzzzzzzzzyf", "xxxx*zzy*f", .true.) .and. allpassed
     allpassed=  test("xyxyxyzyxyz",  "xy*z*xyz",  .true.)   .and.  allpassed
     allpassed=  test("xyxyxyxyz",    "xy*xyz",    .true.)   .and.  allpassed
     allpassed=  test("mississippi",  "mi*sip*",   .true.)   .and.  allpassed
     allpassed=  test("ababac",       "*abac*",    .true.)   .and.  allpassed
     allpassed=  test("aaazz",        "a*zz*",     .true.)   .and.  allpassed
     allpassed=  test("a12b12",       "*12*23",    .false.)  .and.  allpassed
     allpassed=  test("a12b12",       "a12b",      .false.)  .and.  allpassed
     allpassed=  test("a12b12",       "*12*12*",   .true.)   .and.  allpassed
     ! Additional cases where the '*' char appears in the tame string.
     allpassed=  test("*",     "*",      .true.)   .and.  allpassed
     allpassed=  test("a*r",   "a*",     .true.)   .and.  allpassed
     allpassed=  test("a*ar",  "a*aar",  .false.)  .and.  allpassed
     ! More double wildcard scenarios.
     allpassed=  test("XYXYXYZYXYz",  "XY*Z*XYz",   .true.)   .and.  allpassed
     allpassed=  test("missisSIPpi",  "*SIP*",      .true.)   .and.  allpassed
     allpassed=  test("mississipPI",  "*issip*PI",  .true.)   .and.  allpassed
     allpassed=  test("xyxyxyxyz",    "xy*xyz",     .true.)   .and.  allpassed
     allpassed=  test("miSsissippi",  "mi*sip*",    .true.)   .and.  allpassed
     allpassed=  test("miSsissippi",  "mi*Sip*",    .false.)  .and.  allpassed
     allpassed=  test("abAbac",       "*Abac*",     .true.)   .and.  allpassed
     allpassed=  test("aAazz",        "a*zz*",      .true.)   .and.  allpassed
     allpassed=  test("A12b12",       "*12*23",     .false.)  .and.  allpassed
     allpassed=  test("a12B12",       "*12*12*",    .true.)   .and.  allpassed
     allpassed=  test("oWn",          "*oWn*",      .true.)   .and.  allpassed
     ! Completely tame (no wildcards) cases.
     allpassed= test("bLah", "bLah", .true.) .and. allpassed
     ! Simple mixed wildcard tests suggested by IBMer Marlin Deckert.
     allpassed= test("a", "*?", .true.) .and. allpassed
     ! More mixed wildcard tests including coverage for false positives.
     allpassed=  test("a",      "??",         .false.)  .and.  allpassed
     allpassed=  test("ab",     "?*?",        .true.)   .and.  allpassed
     allpassed=  test("ab",     "*?*?*",      .true.)   .and.  allpassed
     allpassed=  test("abc",    "?**?*?",     .true.)   .and.  allpassed
     allpassed=  test("abc",    "?**?*&?",    .false.)  .and.  allpassed
     allpassed=  test("abcd",   "?b*??",      .true.)   .and.  allpassed
     allpassed=  test("abcd",   "?a*??",      .false.)  .and.  allpassed
     allpassed=  test("abcd",   "?**?c?",     .true.)   .and.  allpassed
     allpassed=  test("abcd",   "?**?d?",     .false.)  .and.  allpassed
     allpassed=  test("abcde",  "?*b*?*d*?",  .true.)   .and.  allpassed
     ! Single-character-match cases.
     allpassed=  test("bLah",   "bL?h",  .true.)   .and.  allpassed
     allpassed=  test("bLaaa",  "bLa?",  .false.)  .and.  allpassed
     allpassed=  test("bLah",   "bLa?",  .true.)   .and.  allpassed
     allpassed=  test("bLaH",   "?Lah",  .false.)  .and.  allpassed
     allpassed=  test("bLaH",   "?LaH",  .true.)   .and.  allpassed
     allpassed=  test('abcdefghijk'  ,  '?b*',      .true.)  .and.  allpassed
     allpassed=  test('abcdefghijk'  ,  '*c*',      .true.)  .and.  allpassed
     allpassed=  test('abcdefghijk'  ,  '*c',       .false.) .and.  allpassed
     allpassed=  test('abcdefghijk'  ,  '*c*k',     .true.)  .and.  allpassed
     allpassed=  test('LS'           ,  '?OW',      .false.) .and.  allpassed
     allpassed=  test('teztit'       ,  'tez*t*t',  .true.)  .and.  allpassed
       ! Two pattern match problems that might pose difficulties
     allpassed=  test('e '           , '*e* ',         .true.)  .and.  allpassed
     allpassed=  test('abcde       ' , '*e      *',    .true.)  .and.  allpassed
     allpassed=  test('bababa'       , 'b*ba',         .true.)  .and.  allpassed
     allpassed=  test('baaaaax'      , 'b*ax',         .true.)  .and.  allpassed
     allpassed=  test('baaaaa'       , 'b*ax',         .false.) .and.  allpassed
     allpassed=  test('baaaaax'      , 'b*a',          .false.) .and.  allpassed
     allpassed=  test(''             , 'b*',           .false.) .and.  allpassed
     allpassed=  test(''             , '*',            .true.)  .and.  allpassed
     allpassed=  test('b'            , '',             .false.) .and.  allpassed
     allpassed=  test('3'            , '??',           .false.) .and.  allpassed
     ! known flaws
     allpassed=  test('baaaaa'//char(0), 'b*a'//char(0), .true.) .and.  allpassed
     allpassed=  test(''//char(0),       ''//char(0),    .true.) .and.  allpassed
     ! Many-wildcard scenarios.
     allpassed= test(&
     &"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa&
     &aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaab",&
     &"a*a*a*a*a*a*aa*aaa*a*a*b",&
     &.true.) .and. allpassed
     allpassed= test(&
     &"abababababababababababababababababababaacacacacacacac&
     &adaeafagahaiajakalaaaaaaaaaaaaaaaaaffafagaagggagaaaaaaaab",&
     &"*a*b*ba*ca*a*aa*aaa*fa*ga*b*",&
     &.true.) .and. allpassed
     allpassed= test(&
     &"abababababababababababababababababababaacacacacacaca&
     &cadaeafagahaiajakalaaaaaaaaaaaaaaaaaffafagaagggagaaaaaaaab",&
     &"*a*b*ba*ca*a*x*aaa*fa*ga*b*",&
     &.false.) .and. allpassed
     allpassed= test(&
     &"abababababababababababababababababababaacacacacacacacad&
     &aeafagahaiajakalaaaaaaaaaaaaaaaaaffafagaagggagaaaaaaaab",&
     &"*a*b*ba*ca*aaaa*fa*ga*gggg*b*",&
     &.false.) .and. allpassed
     allpassed= test(&
     &"abababababababababababababababababababaacacacacacacacad&
     &aeafagahaiajakalaaaaaaaaaaaaaaaaaffafagaagggagaaaaaaaab",&
     &"*a*b*ba*ca*aaaa*fa*ga*ggg*b*",&
     &.true.) .and. allpassed
     allpassed= test("aaabbaabbaab", "*aabbaa*a*", .true.) .and. allpassed
     allpassed= &
     test("a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*",&
     &"a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*", .true.) .and. allpassed
     allpassed= test("aaaaaaaaaaaaaaaaa",&
     &"*a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*", .true.) .and. allpassed
     allpassed= test("aaaaaaaaaaaaaaaa",&
     &"*a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*", .false.) .and. allpassed
     allpassed= test(&
     &"abc*abcd*abcde*abcdef*abcdefg*abcdefgh*abcdefghi*abcdefghij&
     &*abcdefghijk*abcdefghijkl*abcdefghijklm*abcdefghijklmn",&
     & "abc*abc*abc*abc*abc*abc*abc*abc*abc*abc*abc*abc*abc*abc&
     &*abc*abc*abc*",&
     &.false.) .and. allpassed
     allpassed= test(&
     &"abc*abcd*abcde*abcdef*abcdefg*abcdefgh*abcdefghi*abcdefghij&
     &*abcdefghijk*abcdefghijkl*abcdefghijklm*abcdefghijklmn",&
     &"abc*abc*abc*abc*abc*abc*abc*abc*abc*abc*abc*abc*",&
     &.true.) .and. allpassed
     allpassed= test("abc*abcd*abcd*abc*abcd",&
     &"abc*abc*abc*abc*abc", .false.) .and. allpassed
     allpassed= test( "abc*abcd*abcd*abc*abcd*abcd&
     &*abc*abcd*abc*abc*abcd", &
     &"abc*abc*abc*abc*abc*abc*abc*abc*abc*abc*abcd",&
     &.true.) .and. allpassed
     allpassed= test("abc",&
     &"********a********b********c********", .true.) .and. allpassed
     allpassed=&
     &test("********a********b********c********", "abc",.false.).and.allpassed
     allpassed= &
     &test("abc", "********a********b********b********",.false.).and.allpassed
     allpassed= test("*abc*", "***a*b*c***", .true.) .and. allpassed
     ! A case-insensitive algorithm test.
     ! allpassed=test("mississippi", "*issip*PI", .true.) .and. allpassed
  enddo

   call unit_check_done('glob')
!===================================================================================================================================
   contains
!===================================================================================================================================
   ! This is a test program for wildcard matching routines.  It can be used
   ! either to test a single routine for correctness, or to compare the timings
   ! of two (or more) different wildcard matching routines.
   !
   function test(tame, wild, bExpectedResult) result(bpassed)
   !x!use M_strings, only : glob
      character(len=*) :: tame
      character(len=*) :: wild
      logical          :: bExpectedResult
      logical          :: bResult
      logical          :: bPassed
      bResult = .true.    ! We'll do "&=" cumulative checking.
      bPassed = .false.   ! Assume the worst.
      bResult = glob(tame, wild) ! Call a wildcard matching routine.

      ! To assist correctness checking, output the two strings in any failing scenarios.
      if (bExpectedResult .eqv. bResult) then
         bPassed = .true.
         !if(nReps == 1) write(std_err,g)"Passed match on ",tame," vs. ", wild
      else
         !if(nReps == 1) write(std_err,g)"Failed match on ",tame," vs. ", wild
      endif
      if(i==1)call unit_check('glob',bExpectedResult.eqv.bResult,'string',tame,'pattern',wild,'expected',bExpectedResult)
   end function test
end subroutine test_glob
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_replace()
character(len=:),allocatable :: targetline

   call unit_check_start('replace',' '//OPTIONS)
   targetline='this is the input string'

   call testit('th','TH','THis is THe input string')

   ! a null old substring means "at beginning of line"
   call testit('','BEFORE:', 'BEFORE:THis is THe input string')

   ! a null new string deletes occurrences of the old substring
   call testit('i','', 'BEFORE:THs s THe nput strng')

   targetline=replace('a b ab baaa aaaa aa aa a a a aa aaaaaa','aa','CCCC',occurrence=3,repeat=3)
   call unit_check('replace',targetline == 'a b ab baaa aaCCCC CCCC CCCC a a a aa aaaaaa','example of using RANGE',targetline)

   targetline=replace('10-9','-',' -')
   call unit_check('replace',targetline == '10 -9','example of leading space in new=',targetline)

   targetline=replace('12-10','-',' -')
   call unit_check('replace',targetline == '12 -10','example of leading space in new=',targetline)

   targetline=replace('-','-',' -')
   call unit_check('replace',targetline == ' -','spaces=',targetline)

   targetline=replace('---','-',' -')
   call unit_check('replace',targetline == ' - - -','spaces=',targetline)

   targetline=replace('12-10-','-',' -')
   call unit_check('replace',targetline == '12 -10 -','spaces=',targetline)

   call unit_check_done('replace',msg='finished test of replacing substrings')

contains
subroutine testit(old,new,expected)
character(len=*),intent(in) :: old,new,expected

   if(unit_check_level > 0)then
      write(std_err,g)repeat('=',79)
      write(std_err,g)'STARTED ['//targetline//']'
      write(std_err,g)'OLD['//old//']', ' NEW['//new//']'
   endif

   targetline=replace(targetline,old,new)

   if(unit_check_level > 0)then
      write(std_err,g)'GOT     ['//targetline//']'
      write(std_err,g)'EXPECTED['//expected//']'
   endif
   call unit_check('replace',targetline == expected,msg='')
end subroutine testit
end subroutine test_replace
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_join()
character(len=:),allocatable  :: s(:)
   call unit_check_start('join',' &
      & -description ''append an array of character variables with specified separator into a single CHARACTER variable'' '&
      & //OPTIONS)
   s=[character(len=10) :: 'United',' we',' stand,',' divided',' we fall.']

   call testit( join(s),                            'United we stand, divided we fall.')
   call testit( join(s,trm=.false.),                'United     we        stand,    divided   we fall.')
   call testit( join(s,trm=.false.,sep='|'),        'United    | we       | stand,   | divided  | we fall.')
   call testit( join(s,sep='<>'),                   'United<> we<> stand,<> divided<> we fall.')
   call testit( join(s,sep=';',left='[',right=']'), '[United];[ we];[ stand,];[ divided];[ we fall.]')
   call testit( join(s,left='[',right=']'),         '[United][ we][ stand,][ divided][ we fall.]')
   call testit( join(s,left='>>'),                  '>>United>> we>> stand,>> divided>> we fall.')
   s=[character(len=0) :: ]
   call testit( join(s),                            '')
   call testit( join(s,trm=.false.),                '')
   call testit( join(s,trm=.false.,sep='|'),        '')
   call testit( join(s,sep='<>'),                   '')
   call testit( join(s,sep=';',left='[',right=']'), '[]')
   call testit( join(s,left='[',right=']'),         '[]')
   call testit( join(s,left='>>'),                  '>>')
   call unit_check_done('join',msg='join array of strings into a single string controlling separators and white space')
contains
subroutine testit(generated,expected)
character(len=*),intent(in) :: generated
character(len=*),intent(in) :: expected
   if(unit_check_level > 0)then
      write(std_err,g)'JOIN(3F) TEST'
      write(std_err,g)'INPUT       ','['//s//']'
      write(std_err,g)'GENERATED   ',generated
      write(std_err,g)'EXPECTED    ',expected
   endif
   call unit_check('join',generated == expected,msg='output is '//generated)
end subroutine testit
end subroutine test_join
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_base()
   character(len=:),allocatable :: in(:)
   character(len=:),allocatable :: expected(:)
   call unit_check_start('base',' '//OPTIONS)

   ! convert base2 values to base10 in brief mode
   in=[character(len=32) :: '10','1010','101010','10101010','1010101010','101010101010']
   expected=[character(len=10) :: '2','10','42','170','682','2730']
   call checkit(in,expected,2,10)

   ! convert base10 values to base2
   in=[character(len=10) :: '2','10','42','170','682','2730']
   expected=[ character(len=32) :: '10', '1010', '101010', '10101010', '1010101010', '101010101010']
   call checkit(in,expected,10,2)

   ! convert base10 values to base3
   in=[character(len=7) :: '10','20','30','40','50']
   expected = [character(len=10) :: '101', '202', '1010', '1111', '1212']
   call checkit(in,expected,10,3)

   ! convert values of various explicit bases to base10
   call checkit(['11'],['3'],2,10)
   call checkit(['1212'],['50'],3,10)
   call checkit(['123123'],['1755'],4,10)

! convert values of various explicit bases to base2 in brief mode
   call checkit(['1111'],['1111'],2,2)
   call checkit(['10'],['11'],3,2)
   call checkit(['10'],['100'],4,2)
   call checkit(['10'],['1000'],8,2)
   call checkit(['10'],['10000'],16,2)

   call unit_check_done('base')
contains
subroutine checkit(in,answers,inbase,outbase)
character(len=*),intent(in)  :: in(:)
character(len=*),intent(in)  :: answers(:)
integer,intent(in)           :: inbase
integer,intent(in)           :: outbase
character(len=256)           :: answer
integer                      :: i
   do i=1,size(in)
      if(base(in(i),inbase,answer,outbase))then
           call unit_check('base',answer == answers(i), &
              'converting '//trim(in(i))//' got '//trim(answers(i))//' expected '//trim(answer) )
       else
          call unit_check_bad('base',msg='Error in decoding/encoding number.')
       endif
   enddo
end subroutine checkit
end subroutine test_base
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_decodebase()
character(len=:),allocatable :: in(:)
integer,allocatable          :: expected(:)
   call unit_check_start('decodebase',' '//OPTIONS)

   ! convert base2 values to base10 in brief mode
   in=[character(len=32) :: '10','1010','101010','10101010','1010101010','101010101010']
   expected=[2,10,42,170,682,2730]
   call checkit(in,expected,2)

   ! convert values of various explicit bases to base10
   call checkit(['11'],[3],2)
   call checkit(['1212'],[50],3)
   call checkit(['123123'],[1755],4)
   call checkit(['10'],[16],16)
   call checkit(['10'],[8],8)

   call unit_check_done('decodebase')
contains
subroutine checkit(in,answers,inbase)
character(len=*),intent(in)  :: in(:)
integer,intent(in)           :: answers(:)
integer,intent(in)           :: inbase
integer                      :: answer
integer                      :: i
   do i=1,size(in)
      if(decodebase(in(i),inbase,answer))then
           call unit_check('decodebase',answer == answers(i), &
              'converting '//trim(in(i)) )
       else
          call unit_check_bad('decodebase',msg='Error in decoding/encoding number.')
       endif
   enddo
end subroutine checkit
end subroutine test_decodebase
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_base2()
character(len=:),allocatable :: in(:)
integer,allocatable          :: expected(:)
   call unit_check_start('base2',' '//OPTIONS)

   ! convert base10 values to base2 strings
   in=[character(len=32) :: '10','1010','101010','10101010','1010101010','101010101010']
   expected=[2,10,42,170,682,2730]
   call checkit(in,expected)

   call unit_check_done('base2')
contains
subroutine checkit(answer,values)
character(len=*),intent(in)  :: answer(:)
integer,intent(in)           :: values(:)
character(len=32)            :: out
integer                      :: i
   do i=1,size(answer)
      call unit_check('base2',base2(values(i)) == answer(i), &
       & 'checking for '//trim(answer(i))//' in base 2 from value '//v2s(values(i)) )
   enddo
end subroutine checkit
end subroutine test_base2
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_codebase()
character(len=:),allocatable :: in(:)
integer,allocatable          :: expected(:)
   call unit_check_start('codebase',' '//OPTIONS)

   ! convert base10 values to base2 strings
   in=[character(len=32) :: '10','1010','101010','10101010','1010101010','101010101010']
   expected=[2,10,42,170,682,2730]
   call checkit(in,expected,2)

   ! convert values to various explicit bases
   call checkit(['11'],[3],2)
   call checkit(['1212'],[50],3)
   call checkit(['123123'],[1755],4)
   call checkit(['10'],[16],16)
   call checkit(['10'],[8],8)

   call unit_check_done('codebase')
contains
subroutine checkit(answer,values,outbase)
character(len=*),intent(in)  :: answer(:)
integer,intent(in)           :: values(:)
integer,intent(in)           :: outbase
character(len=32)            :: out
integer                      :: i
   do i=1,size(answer)
      if(codebase(values(i),outbase,out) )then
           call unit_check('codebase',out == answer(i), &
              'checking for '//trim(answer(i))//' in base '//v2s(outbase)//' from value '//v2s(values(i)) )
       else
          call unit_check_bad('codebase',msg='Error answer decoding/encoding number.')
       endif
   enddo
end subroutine checkit
end subroutine test_codebase
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_delim()
character(len=80) :: line
character(len=80) :: dlm
integer,parameter :: n=10
character(len=20) :: array(n)=' '
integer           :: ibegin(n),iterm(n)
integer           :: icount
integer           :: ilen
   call unit_check_start('delim',' &
      & -description ''subroutine parses string using delimiters and stores tokens into array'' '//OPTIONS)
   line=' first  second 10.3 words_of_stuff  '
   dlm=' '

   call testit()
   call unit_check('delim',icount == 4,msg=' check number of tokens')
   call unit_check('delim',ilen == 34,msg=' check position of last character')
   call unit_check('delim',all(array(:icount) == [character(len=20) :: 'first','second','10.3','words_of_stuff']),msg='tokens')

   ! change delimiter list and what is calculated or parsed
   dlm=' aeiou'    ! NOTE SPACE IS FIRST
   call testit()
   call unit_check('delim',all(array(:icount) == &
           [character(len=10) :: 'f','rst','s','c','nd','10.3','w','rds_','f_st','ff']),msg='delims')

  call unit_check_done('delim')
contains
subroutine testit()
integer                 :: i10
   ! show line being parsed
   ! call parsing procedure
   call delim(line,array,n,icount,ibegin,iterm,ilen,dlm)

   if(unit_check_level > 0)then
      write(std_err,'(a)')'PARSING=['//trim(line)//'] on '//trim(dlm)
      write(std_err,g)'number of tokens found=',icount
      write(std_err,g)'last character in column ',ilen
      if(icount > 0)then
         if(ilen /= iterm(icount))then
            write(std_err,g)'ignored from column ',iterm(icount)+1,' to ',ilen
         endif
         do i10=1,icount
            ! check flag to see if ARRAY() was set
            if(array(1) /= '#N#')then
               ! from returned array
               write(std_err,'(a,a,a)',advance='no')'[',array(i10)(:iterm(i10)-ibegin(i10)+1),']'
            endif
         enddo
         ! using start and end positions in IBEGIN() and ITERM()
         write(std_err,g)
         do i10=1,icount
            ! from positions in original line
            write(std_err,'(a,a,a)',advance='no') '[',line(ibegin(i10):iterm(i10)),']'
         enddo
         write(std_err,g)
      endif
   endif
end subroutine testit
end subroutine test_delim
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_split
!-!  split: parse a string using specified delimiter characters and store tokens into an array
!-!$SYSTEM  goodbad split start -library libGPF -filename `pwd`/M_strings.FF --section 3
!-!   USE M_strings, ONLY: split
INTRINSIC SIZE
CHARACTER(LEN=:),ALLOCATABLE    :: line
CHARACTER(LEN=:),ALLOCATABLE    :: order
CHARACTER(LEN=:),ALLOCATABLE    :: dlm
CHARACTER(LEN=:),ALLOCATABLE    :: array(:)
character(len=10)               :: orders(3)=['sequential', '          ', 'reverse   ' ]
! return strings composed of delimiters or not IGNORE|RETURN|IGNOREEND
character(len=10)               :: nulls(3)=['ignore    ', 'return    ', 'ignoreend ' ]
!-----------------------------------------------------------------------------------------------------------------------------------
   call unit_check_start('split',' &
      & -description ''subroutine parses string on delimiters and store tokens into an array'' '//OPTIONS)

   dlm=''
   LINE='abcdef ghijklmnop qrstuvwxyz  1:2  333333 a b cc    '
   order=orders(3)
   CALL testit()
   CALL split(line,array,dlm,order,nulls(1))
   if(unit_check_level > 0)then
      write(std_err,g)size(array)
   endif
   order=orders(2)
   CALL split(line,array,dlm,order,nulls(2))
   if(unit_check_level > 0)then
      write(std_err,g)size(array)
   endif
   order=orders(1)
   CALL split(line,array,dlm,order,nulls(3))
   if(unit_check_level > 0)then
      write(std_err,g)size(array)
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   LINE=' abcdef ghijklmnop qrstuvwxyz  1:2  333333 a b cc    '
   CALL testit()
!-----------------------------------------------------------------------------------------------------------------------------------
   LINE='        abcdef ghijklmnop qrstuvwxyz  1:2  333333 a b cc    '
   CALL testit()
!-----------------------------------------------------------------------------------------------------------------------------------
   LINE=' aABCDEF  ; b;;c d e;  ;  '
   CALL testit()
!-----------------------------------------------------------------------------------------------------------------------------------
   dlm=';'
   CALL testit()
!-----------------------------------------------------------------------------------------------------------------------------------
   dlm='; '
   CALL testit()
!-----------------------------------------------------------------------------------------------------------------------------------
   dlm=';'
   LINE=';;;abcdef;ghijklmnop;qrstuvwxyz;;1:2;;333333;a;b;cc;;'
   CALL testit()
   LINE=';;abcdef;ghijklmnop;qrstuvwxyz;;1:2;;333333;a;b;cc;'
   CALL testit()
   LINE=';abcdef;ghijklmnop;qrstuvwxyz;;1:2;;333333;a;b;cc;'
   CALL testit()
   LINE='abcdef;ghijklmnop;qrstuvwxyz;;1:2;;333333;a;b;cc'
   CALL testit()
!-----------------------------------------------------------------------------------------------------------------------------------
   line='a b c d e f g h i j k l m n o p q r s t u v w x y z'
   CALL split(line,array)
   call unit_check('split',size(array) == 26,msg='test delimiter')
!-----------------------------------------------------------------------------------------------------------------------------------
   dlm=' '
   CALL split(line,array,dlm)
   call unit_check('split',size(array) == 26,msg='test delimiter')
!-----------------------------------------------------------------------------------------------------------------------------------
   call unit_check_done('split')
!-----------------------------------------------------------------------------------------------------------------------------------
   CONTAINS
!-----------------------------------------------------------------------------------------------------------------------------------
   SUBROUTINE testit()
   integer :: i
   if(unit_check_level > 0)then
      write(std_err,'(80("="))')
      write(std_err,'(A)')'parsing ['//TRIM(line)//']'//'with delimiters set to ['//dlm//'] and order '//trim(order)
   endif
   CALL split(line,array,dlm,order)
   if(unit_check_level > 0)then
      write(std_err,'("number of tokens found=",i0)')SIZE(array)
      write(std_err,'(I0,T10,A)')(i,TRIM(array(i)),i=1,SIZE(array))
   endif
   END SUBROUTINE testit
end subroutine test_split
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_m_strings
!-!use M_strings, only: reverse
!-!use M_strings, only: lower
!-!use M_strings, only: switch
!-!use M_strings, only: isgraph,isprint
character(len=36),parameter :: lc='abcdefghijklmnopqrstuvwxyz0123456789'
character(len=36),parameter :: uc='ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789'
character(len=1)            :: chars(36)
call unit_check_start('combined')
!-----------------------------------------------------------------------------------------------------------------------------------
! COMBINED TESTS
chars=switch(uc)     ! convert string to character array
chars=chars(36:1:-1) ! reverse order of characters
call unit_check('combined',lower(reverse(switch(chars))) == lc,msg='combined lower(),reverse(),switch()')
!-----------------------------------------------------------------------------------------------------------------------------------
if(unit_check_level > 0)then
   write(std_err,g)'isprint'
   write(std_err,g)'   letter a      ',isprint('a')
   write(std_err,g)'   horizontal tab',isprint(char(9))
   write(std_err,g)'   array of letters;.',isprint([';','.',' '])
   write(std_err,g)'   array of letters',isprint(switch(uc))
   write(std_err,g)'   array of letters',isprint(uc)
endif
!-----------------------------------------------------------------------------------------------------------------------------------
if(unit_check_level > 0)then
   write(std_err,g)'isgraph'
   write(std_err,g)'   letter a      ',isgraph('a')
   write(std_err,g)'   horizontal tab',isgraph(char(9))
   write(std_err,g)'   array of letters;.',isgraph([';','.',' '])
   write(std_err,g)'   array of letters',isgraph(switch(uc))
   write(std_err,g)'   array of letters',isgraph(uc)
endif
!-----------------------------------------------------------------------------------------------------------------------------------
call unit_check_done('combined')
end subroutine test_m_strings
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_chomp()
!-!use M_strings, only : chomp
character(len=:),allocatable  :: str
character(len=:),allocatable  :: token
character(len=66),allocatable :: delimiters
integer                       :: ipass
   call unit_check_start('chomp',' &
      & -description ''function consumes input line and returns next token using delimiters'' '//OPTIONS)

   str = 'a b ccc ddd x12#$)$*#@Z1!( ab cd ef'
   delimiters=' #@$)*!('
   ipass=0
   do while ( chomp(str,token,delimiters)  >=  0 )
      ipass=ipass+1
      if(unit_check_level > 0)then
         write( std_err,g) ipass,'TOKEN=['//trim(token)//']'
      endif
      select case(ipass)
      case(1); call unit_check('chomp', token == 'a'   ,'token=',token)
      case(2); call unit_check('chomp', token == 'b'   ,'token=',token)
      case(3); call unit_check('chomp', token == 'ccc' ,'token=',token)
      case(4); call unit_check('chomp', token == 'ddd' ,'token=',token)
      case(5); call unit_check('chomp', token == 'x12' ,'token=',token)
      case(6); call unit_check('chomp', token == 'Z1'  ,'token=',token)
      case(7); call unit_check('chomp', token == 'ab'  ,'token=',token)
      case(8); call unit_check('chomp', token == 'cd'  ,'token=',token)
      case(9); call unit_check('chomp', token == 'ef'  ,'token=',token)
      end select
   enddo

   call unit_check_done('chomp')
end subroutine test_chomp
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine test_substitute
!-!use M_strings, only : substitute
character(len=:),allocatable    :: targetline   ! input line to be changed
character(len=:),allocatable    :: old          ! old substring to replace
character(len=:),allocatable    :: new          ! new substring
integer                         :: ml           ! ml sets the left  margin
integer                         :: mr           ! mr sets the right margin
integer                         :: ier          ! error code. if ier = -1 bad directive, >= 0then ier changes made
!-----------------------------------------------------------------------------------------------------------------------------------
   call unit_check_start('substitute','-description ''subroutine globally replaces old substring with new string'' '//OPTIONS)
   targetline='This an that and any other '
   old='an'
   new='##'
   ml=1
   mr=len(targetline)
   if(unit_check_level > 0)then
      write(std_err,g)'ORIGINAL: '//targetline
   endif
   call substitute(targetline,old,new,ier,ml,mr) !Globally substitute one substring for another in string
   if(unit_check_level > 0)then
      write(std_err,g)'C@'//OLD//'@'//NEW//'@ ==>'//targetline
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   if(ier /= 3)then
      if(unit_check_level > 0)then
         write(std_err,g)ier,targetline
      endif
      call unit_check_bad('substitute')
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   if(targetline /= 'This ## that ##d ##y other ')then
      call unit_check_bad('substitute')
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   targetline='This and that, This and that,                               '
   if(unit_check_level > 0)then
      write(std_err,g)'ORIGINAL: '//targetline
   endif

   old=''
   new='BEGINNING: '
   call substitute(targetline,old,new) !Globally substitute one substring for another in string
   if(unit_check_level > 0)then
      write(std_err,g)'C@'//OLD//'@'//NEW//'@ ==>'//targetline
   endif

   old='This'
   new='THIS'
   call substitute(targetline,old,new) !Globally substitute one substring for another in string
   if(unit_check_level > 0)then
      write(std_err,g)'C@'//OLD//'@'//NEW//'@ ==>'//targetline
   endif

   old='that'
   new='LONGER STRING'
   call substitute(targetline,old,new) !Globally substitute one substring for another in string
   if(unit_check_level > 0)then
      write(std_err,g)'C@'//OLD//'@'//NEW//'@ ==>'//targetline
   endif

   old='LONGER STRING'
   new=''
   call substitute(targetline,old,new) !Globally substitute one substring for another in string
   if(unit_check_level > 0)then
      write(std_err,g)'C@'//OLD//'@'//NEW//'@ ==>'//targetline
   endif

   if ( targetline  /=  'BEGINNING: THIS and , THIS and ,')then
      call unit_check_bad('substitute')
      stop 3
   endif
!-----------------------------------------------------------------------------------------------------------------------------------
   call unit_check_good('substitute')
!-----------------------------------------------------------------------------------------------------------------------------------
end subroutine test_substitute
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_change
!-!use M_strings, only : change
!character(len=132) :: direc
character(len=132)  :: line=' The rain in Spain falls mainly on the plain. '
integer             :: ier
   if(unit_check_level > 0)then
      write(std_err,g)' LINE='//trim(line)
   endif
   ! indicate test of change(3f) has begun
   call unit_check_start('change',' -description ''replace substring with new string with a directive like line editor'' '//OPTIONS)
   call change(line, 'c/ain/AIN'     ,ier)
   if(unit_check_level > 0)then
      write(std_err,g)'IER=',ier
   endif
   call unit_check('change',ier == 4,'without trailing slash')

   call change(line, 'c/ The r/R/'   ,ier)
   if(unit_check_level > 0)then
      write(std_err,g)'IER=',ier
   endif
   call unit_check('change',ier == 1,'with trailing slash')

   call change(line, 'c/ /'          ,ier)
   if(unit_check_level > 0)then
      write(std_err,g)'IER=',ier
   endif
   call unit_check('change',ier >= 7,'no new string') ! remove spaces

   call change(line, 'c//PREFIX:'    ,ier)
   if(unit_check_level > 0)then
      write(std_err,g)'IER=',ier
   endif
   call unit_check('change',ier == 1,'null new string')

   call change(line, 'c/XYZ/xxxxxx:' ,ier)
   if(unit_check_level > 0)then
      write(std_err,g)'IER=',ier
   endif
   call unit_check('change',ier == 0,'no matching old string')

   if(unit_check_level > 0)then
      write(std_err,g)'IER=',ier,' LINE='//trim(line)
   endif
   call unit_check('change','PREFIX:RAINinSpAINfallsmAINlyontheplAIN.'  ==  line,'check cumulative changes')

   call unit_check_done('change') ! indicate test of change(3f) passed
end subroutine test_change
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_strtok()
integer,parameter             :: length=264
character(len=length)         :: inline
integer                       :: istart(length/2+1)
integer                       :: iend(length/2+1)
integer,allocatable           :: istart_expected(:)
integer,allocatable           :: iend_expected(:)
character(len=:),allocatable  :: words_expected(:)
character(len=*),parameter    :: delimiters=' ;,'
integer                       :: is,ie
integer                       :: itoken
   call unit_check_start('strtok',' '//OPTIONS)
   istart_expected=[ 2,  7,  10,  12,  17,  20,  28,  32,  35 ]
   iend_expected=[ 5,  8,  10,  15,  18,  25,  30,  32,  35 ]
   words_expected=[ character(len=10) :: 'this', 'is', 'a', 'test', 'of', 'strtok', 'A:B', ':', 'C']

   inline=' this is a test of strtok; A:B :;,C;;'

   itoken=0 ! must set ITOKEN=0 before looping on strtok(3f) on a new string.
   if(unit_check_level > 0)then
      write(std_err,g)trim(inline)
   endif
   do while ( strtok(inline,itoken,is,ie,delimiters) )
      istart(itoken)=is
      iend(itoken)=ie
      if(unit_check_level > 0)then
         write( std_err,g) itoken,'TOKEN=['//(inline(istart(itoken):iend(itoken)))//']',istart(itoken),iend(itoken)
      endif
   enddo
   call unit_check('strtok',all(istart(:itoken) == istart_expected) .and. &
      all(iend(:itoken) == iend_expected), &
      msg='parse a line into tokens with strtok(3f)')
   call unit_check_done('strtok')
end subroutine test_strtok
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_modif()
character(len=256)           :: line
character(len=:),allocatable :: COMMAND_LINE
   call unit_check_start('modif',' &
      & -description ''change string using a directive similar to XEDIT editor MODIFY command'' '//OPTIONS )
   line='ABCDEFGHIJKLMNOPQRSTUVWXYZ'
   command_line='###%aaaa# 1 2 3&  ^up'
   call unit_check('modif',line == 'ABCDEFGHIJKLMNOPQRSTUVWXYZ',msg=line)
   call unit_check('modif',line == 'ABCDEFGHIJKLMNOPQRSTUVWXYZ',msg=command_line)
   call modif(line,COMMAND_LINE)
   command_line='###%aaaa# 1 2 3&  ^up'
   call unit_check('modif',line == '%aaaaJ1L2N3 QRupSTUVWXYZ',msg=line)
   call unit_check_done('modif')
end subroutine test_modif
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_len_white()
   call unit_check_start('len_white',' &
      & -description ''find location of last non-whitespace character'' '//OPTIONS )
   call unit_check('len_white',len_white('A b c  '//char(9)//char(10)//char(11)//char(12)//char(13)) == 5,msg='')
   call unit_check_done('len_white',msg='len_white(3f) tests completed')
end subroutine test_len_white
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_clip()
   call unit_check_start('clip',' &
      & -description ''function trims leading and trailing spaces'' '//OPTIONS )
   call unit_check('clip',clip('    A B CC D      ') == 'A B CC D',msg='clip string test 1')
   call unit_check('clip',clip('A B CC D') == 'A B CC D',msg='clip string test 2')
   call unit_check('clip',clip('A B CC D    ') == 'A B CC D',msg='clip string test 3')
   call unit_check('clip',clip('     A B CC D    ') == 'A B CC D',msg='clip string test 4')
   call unit_check_done('clip')
end subroutine test_clip
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_crop()
   call unit_check_start('crop',' &
      & -description ''function trims leading and trailing spaces'' '//OPTIONS )
   call unit_check('crop',crop('    A B CC D      ') == 'A B CC D',msg='crop string test 1')
   call unit_check('crop',crop('A B CC D') == 'A B CC D',msg='crop string test 2')
   call unit_check('crop',crop('A B CC D    ') == 'A B CC D',msg='crop string test 3')
   call unit_check('crop',crop('     A B CC D    ') == 'A B CC D',msg='crop string test 4')
   call unit_check_done('crop')
end subroutine test_crop
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_transliterate
!-!use M_strings, only: transliterate
character(len=36),parameter :: lc='abcdefghijklmnopqrstuvwxyz0123456789'
character(len=36),parameter :: uc='ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789'
   call unit_check_start('transliterate',' &
      & -description ''when characters in set one are found replace them with characters from set two'' '//OPTIONS )
call unit_check('transliterate',transliterate('AbCDefgHiJklmnoPQRStUvwxyZ',lc,uc) == uc(1:26),msg='transliterate to uppercase')
call unit_check('transliterate',transliterate('AbCDefgHiJklmnoPQRStUvwxyZ',uc,lc) == lc(1:26),msg='transliterate to lowercase')
call unit_check_done('transliterate')
end subroutine test_transliterate
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_rotate13()
character(len=:),allocatable  :: s
character(len=:),allocatable  :: e
   call unit_check_start('rotate13',' -description ''apply trivial encryption algorithm ROT13 to a string'' '//OPTIONS )
   s='United we stand, divided we fall.'
   e='Havgrq jr fgnaq, qvivqrq jr snyy.'
   ! add //'' to change function call to expression to avoid gfortran bug
   call unit_check('rotate13',rotate13(s) == e,  s,'==>',rotate13(s)//'') 
   call unit_check_done('rotate13',msg='')
end subroutine test_rotate13
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_reverse
!-!use M_strings, only: reverse
character(len=36),parameter :: lc='abcdefghijklmnopqrstuvwxyz0123456789'
!-----------------------------------------------------------------------------------------------------------------------------------
   call unit_check_start('reverse',' -description ''elemental function reverses character order in a string'' '//OPTIONS )
if(reverse(lc) == '9876543210zyxwvutsrqponmlkjihgfedcba')then
   call unit_check_good('reverse')
else
   if(unit_check_level > 0)then
      write(std_err,g)'error: reverse '
      write(std_err,g)'iN:  ['//lc//']'
      write(std_err,g)'OUT: ['//reverse(lc)//']'
   endif
   call unit_check_bad('reverse')
endif
!-----------------------------------------------------------------------------------------------------------------------------------
end subroutine test_reverse
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_upper
!-!use M_strings, only: upper
character(len=36),parameter :: lc='abcdefghijklmnopqrstuvwxyz0123456789'
character(len=36),parameter :: uc='ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789'
character(len=36),parameter :: rnge='abcdefghIJKLMNopqrstuvwxyz0123456789'
!-----------------------------------------------------------------------------------------------------------------------------------
   call unit_check_start('upper',' -description ''elemental function converts string to uppercase'' '//OPTIONS )
   call unit_check('upper',upper(lc) == uc,upper(lc))
   call unit_check('upper',upper(lc,9,14) == rnge,'range',upper(lc,9,14),'expected',rnge)
   call unit_check_done('upper')
!-----------------------------------------------------------------------------------------------------------------------------------
end subroutine test_upper
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_lower
!-!use M_strings, only: lower
character(len=36),parameter :: lc='abcdefghijklmnopqrstuvwxyz0123456789'
character(len=36),parameter :: uc='ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789'
character(len=36),parameter :: rnge='ABCDEFGHijklmnOPQRSTUVWXYZ0123456789'
!-----------------------------------------------------------------------------------------------------------------------------------
   call unit_check_start('lower',' -description ''elemental function converts string to miniscule'' '//OPTIONS )
   call unit_check('lower',lower(uc) == lc,'lower',lower(uc),'expected',lc)
   call unit_check('upper',lower(uc,9,14) == rnge,'range',lower(uc,9,14),'expected',rnge)
   call unit_check_done('lower')
!-----------------------------------------------------------------------------------------------------------------------------------
end subroutine test_lower
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_switch
!-!use m_switch, only: reverse
!-!use m_switch, only: switch
character(len=36),parameter :: lc='abcdefghijklmnopqrstuvwxyz0123456789'
character(len=36),parameter :: uc='abcdefghijklmnopqrstuvwxyz0123456789'
character(len=1)            :: chars(36)
integer :: i
!-----------------------------------------------------------------------------------------------------------------------------------
if(unit_check_level > 0)then
   write(std_err,g)'switch:' ! switch: switch between single string and an array of single characters; generic name for {a2s,s2a}
   write(std_err,g)'switch LC string to an array'
   write(std_err,'(i0,1x,*(a,1x))') size(switch(lc)),switch(lc)
   write(std_err,g)'switch UC string to an array'
   write(std_err,'(i0,1x,*(a,1x))') size(switch(uc)),switch(uc)
endif
   call unit_check_start('switch',' &
      & -description ''generic switch between a string and an array of single characters (a2s,s2a)'' '//OPTIONS )
if(size(switch(uc)) /= 36)then
   call unit_check_bad('switch')
endif
chars=switch(uc)
do i=1,size(chars)
   if(chars(i) /= uc(i:i))then
      call unit_check_bad('switch')
   endif
enddo

if(unit_check_level > 0)then
   write(std_err,g)'put string UC into array CHARS'
endif
chars=switch(uc)
if(unit_check_level > 0)then
   write(std_err,g)'put CHARS array into CHARS array in reverse order like reverse'
endif
chars=chars(36:1:-1)
if(unit_check_level > 0)then
   write(std_err,g)'put CHARS array into string reversed and compare to original UC string'
endif
if( uc  /=  switch(chars(36:1:-1)) )then
   if(unit_check_level > 0)then
      write(std_err,g)switch(chars(36:1:-1))
   endif
   call unit_check_bad('switch')
endif
call unit_check_good('switch')
!-----------------------------------------------------------------------------------------------------------------------------------
end subroutine test_switch
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_s2c()
   call unit_check_start('s2c',' &
      & -description ''convert character variable to array of character(len=1) with null terminator for C compatibility'' '&
      & //OPTIONS )
   call unit_check_done('s2c',msg='UNTESTED')
end subroutine test_s2c
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_c2s()
   call unit_check_start('c2s',' &
      & -description ''convert null-terminated array of character(len=1) to string for strings returned by C '' '&
      & //OPTIONS )
   call unit_check_done('c2s',msg='UNTESTED')
end subroutine test_c2s
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_indent()
character(len=1024) :: in
   call unit_check_start('indent',' -description ''count number of leading spaces'' ' //OPTIONS )

   in='    should be four'
   call unit_check('indent',indent(in) == 4,msg=in)

   in='should be zero'
   call unit_check('indent',indent(in) == 0,msg=in)

   in='   should be three'
   call unit_check('indent',indent(in) == 3,msg=in)

   call unit_check_done('indent')
end subroutine test_indent
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_visible()
integer :: i
character(len=2) :: controls(0:31)
   call unit_check_start('visible', ' '//OPTIONS )
   do i=32,126
      call unit_check('visible',visible(char(i)) == char(i),i,visible(char(i)))
   enddo
   controls=['^@  ', '^A  ', '^B  ', '^C  ', '^D  ', '^E  ', '^F  ', '^G  ', '^H  ', '^I  ', &
             '^J  ', '^K  ', '^L  ', '^M  ', '^N  ', '^O  ', '^P  ', '^Q  ', '^R  ', '^S  ', &
             '^T  ', '^U  ', '^V  ', '^W  ', '^X  ', '^Y  ', '^Z  ', '^[  ', '^\  ', '^]  ', &
             '^^  ', '^_  ']
   do i=0,31
      call unit_check('visible',visible(char(i)) == controls(i),i,visible(char(i)))
   enddo
   call unit_check('visible',visible(char(127)) == '^?',127,visible(char(i)))
   if(unit_check_level > 0)then
      do i=0,255
         write(std_err,'(i0,1x,a,1x,i0)')i,visible(char(i)),i
      enddo
   endif
   do i=32,126
      ! add //'' to change function call to expression to avoid gfortran-11 bug
      call unit_check('visible',char(i) == visible(char(i)),i,visible(char(i))//'',char(i)//'')
   enddo
   call unit_check_done('visible')
end subroutine test_visible
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_expand()

! ident_73="@(#)M_strings::test_expand(3f): test filter to expand escape sequences in input lines"

integer :: i
character(len=80) :: line
   call unit_check_start('expand','  -description ''expand escape sequences in a string'' '//OPTIONS )
   call unit_check('expand',expand('\e\d0912J') == char(27)//'[2J','a vt102 sequence to clear the screen')
   call unit_check('expand',expand('this is a test') == 'this is a test',msg='test plain text')

   !check all ASCII values
   do i=0,127
       write(line,'("%d",i3.3)')i
       call unit_check('expand',expand(line,'%') == char(i),msg='check all valid decimal values')
       write(line,'("%o",o3.3)')i
       call unit_check('expand',expand(line,'%') == char(i),msg='check all valid octal values')
       write(line,'("%x",z2.2)')i
       call unit_check('expand',expand(line,'%') == char(i),msg='check all hexadecimal values')
   enddo

   call unit_check('expand',expand('%d008%d027%d013%d011%d007%d009','%') ==  &
           char(8)//char(27)//char(13)//char(11)//char(7)//char(9),msg='test decimal escape characters')
   call unit_check('expand',expand('%b%e%r%v%a%t','%') ==  &
           char(8)//char(27)//char(13)//char(11)//char(7)//char(9),msg='test escape characters')
   call unit_check_done('expand')

end subroutine test_expand
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_notabs()
character(len=:),allocatable :: inline
character(len=:),allocatable :: expected
character(len=1024)          :: outline
integer                      :: iout
   call unit_check_start('notabs',' &
      & -description ''convert tabs to spaces in output while maintaining columns, assuming a tab is set every 8 characters'' '&
      & //OPTIONS )
   inline= 'one '//char(9)//'and'//repeat(char(9),3)//'two'
   expected='one     and                     two'
   call notabs(inline,outline,iout)
   if(unit_check_level /= 0)then
      write(std_err,g)'*test_notabs*',inline
      write(std_err,g)'*test_notabs*',outline
      write(std_err,g)'*test_notabs*',len_trim(outline),iout
   endif
   call unit_check('notabs',outline == expected.and.iout == 35,msg='expand a line with tabs in it')
   call unit_check_done('notabs',msg='')
end subroutine test_notabs
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_adjustc
character(len=80),allocatable :: expected(:)
character(len=80),allocatable :: left(:)
character(len=80),allocatable :: input(:)
integer                       :: i
   call unit_check_start('adjustc',' &
      & -description ''elemental function centers string within the length of the input string'' '//OPTIONS )
   expected=[ character(len=80) ::                                                     &
   '12345678901234567890123456789012345678901234567890123456789012345678901234567890', &
   '                            An Ode to Centered Text                             ', &
   '                                                                                ', &
   '       Centered text is acceptable when used for short phrases or titles,       ', &
   '              like the name on your BUSINESS CARDS or LETTERHEAD.               ', &
   '              In documents, you can center major section headings               ', &
   '                  like "Introduction" and "Table of Contents."                  ', &
   '                     But if you enjoy centering text, then                      ', &
   '                          you should learn to use the                           ', &
   '                                HARD LINE BREAK                                 ', &
   '                              so your lines start                               ', &
   '                                  in sensible                                   ', &
   '                                    places.                                     ', &
   '                                      OK?                                       ', &
   '                                                                                ']
   left=expected
   input=expected
   ! make copy with all strings left-justified
   do i=1,size(left)
      left(i)=adjustl(left(i))
   enddo
   if(unit_check_level > 0)write(std_err,'(a)')left

   ! now center the left-justified copy
   do i=1,size(left)
      input(i)=adjustc(left(i))
   enddo
   ! check against expected output
   call unit_check('adjustc',all(expected == input),msg='text centering')

   ! indent lines different amounts
   do i=1,size(left)
      input(i)=repeat(' ',i-1)//left(i)
   enddo
   if(unit_check_level > 0)write(std_err,'(a)')input

   ! recenter it again
   do i=1,size(left)
      input(i)=adjustc(left(i))
   enddo
   if(unit_check_level > 0)write(std_err,'(a)')input
   call unit_check('adjustc',all(expected == input),msg='text centering')

   call unit_check_done('adjustc')
end subroutine test_adjustc
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_nospace
!-!use M_strings, only: nospace
   character(len=:),allocatable :: string
   string='  This     is      a     test  '
   string=nospace(string)
   call unit_check_start('nospace',' &
      & -description ''function replaces whitespace with nothing'' '//OPTIONS )
   if (string  /=  'Thisisatest')then
      call unit_check_bad('nospace')
   endif
   call unit_check_good('nospace')
end subroutine test_nospace
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_stretch()
   call unit_check_start('stretch',' -description ''return a string of at least specified length'' '//OPTIONS )
   call unit_check('stretch',stretch('Hello World',20)//'!' == 'Hello World         !',msg='check if padded')
   call unit_check('stretch',len(stretch('Hello World',20)) == 20,msg='check padded length')
   call unit_check('stretch',len(stretch('Hello World',2)) == 11 &
           .and.stretch('Hello World',2) == 'Hello World', &
           msg='check not truncated')
   call unit_check_done('stretch',msg='tests completed')
end subroutine test_stretch
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_pad()
   call unit_check_start('pad',' -description ''return a string of at least specified length'' '//OPTIONS )
   call unit_check('pad',pad('Hello World',20)//'!' == 'Hello World         !',msg='check if padded')
   call unit_check('pad',len(pad('Hello World',20)) == 20,msg='check padded length')
   call unit_check('pad',len(pad('Hello World',2)) == 11 &
           .and.pad('Hello World',2) == 'Hello World', &
           msg='check not truncated')
   call unit_check_done('pad',msg='tests completed')
end subroutine test_pad
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_zpad()
   call unit_check_start('zpad',' -description ''return a string left-padded with zeros'' '//OPTIONS )
   call unit_check('zpad',zpad(4,4) == '0004',zpad(4,4),'vs','0004')
   call unit_check('zpad',zpad(4,4) == '0004',zpad(4,4),'vs','0004')
   call unit_check('zpad',zpad(' 123 ',4) == '0123',zpad(' 123 ',4),'vs','0123')
   call unit_check('zpad',all(zpad([1,12,123,1234]) == ['0001','0012','0123','1234']),'["0001","0012","0123","1234"]')
   call unit_check_done('zpad',msg='tests completed')
end subroutine test_zpad
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_lpad()
   call unit_check_start('lpad',' -description ''return a left-padded string'' '//OPTIONS )
   call unit_check('lpad',lpad(4,4) == '   4',lpad(4,4),'vs','   4')
   call unit_check('lpad',lpad(4,4) == '   4',lpad(4,4),'vs','   4')
   call unit_check('lpad',lpad(' 123 ',4) == ' 123 ',lpad(' 123 ',4),'vs',' 123')
   call unit_check('lpad',all(lpad([1,12,123,1234]) == ['   1','  12',' 123','1234']),'["   1","  12"," 123","1234"]')
   call unit_check_done('lpad',msg='tests completed')
end subroutine test_lpad
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_cpad()
   call unit_check_start('cpad',' -description ''return a left-padded string'' '//OPTIONS )
   call unit_check('cpad',cpad(4,3) == ' 4 ',cpad(4,3),'vs',' 4 ')
   call unit_check('cpad',cpad('123',8) == '  123   ',cpad('123',8),'vs','  123   ')
   call unit_check('cpad',all(cpad([1,12,123,1234]) == [' 1  ',' 12 ','123 ','1234']),'[" 1  "," 12 ","123 ","1234"]')
   call unit_check_done('cpad',msg='tests completed')
end subroutine test_cpad
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_rpad()
   call unit_check_start('rpad',' -description ''return a right-padded string'' '//OPTIONS )
   call unit_check('rpad',rpad(4,4) == '4   ',rpad(4,2),'vs','4   ')
   call unit_check('rpad',rpad(-4,4) == '-4   ',rpad(-4,2),'vs','-4  ')
   call unit_check('rpad','['//rpad(' 123 ',4)//']' == '['//'123 '//']','['//rpad(' 123 ',4)//']','vs [','123 '//']')
   call unit_check('rpad',all(rpad([1,12,123,1234]) == ['1   ','12  ','123 ','1234']),'["1   ","12  ","123 ","1234"]')
   call unit_check_done('rpad',msg='tests completed')
end subroutine test_rpad
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_lenset()
character(len=10)            :: string='abcdefghij'
   call unit_check_start('lenset',' -description ''return a string as specified length'' '//OPTIONS )

        call unit_check('lenset',len(lenset(string, 5)) == 5)
        call unit_check('lenset',len(lenset(string,20)) == 20)
        call unit_check('lenset',lenset(string,20) == 'abcdefghij')
        call unit_check('lenset',lenset(string, 5) == 'abcde')
   call unit_check_done('lenset')
end subroutine test_lenset
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_merge_str()
character(len=:), allocatable :: answer
   call unit_check_start('merge_str',' -description ''make strings of equal length and then call MERGE(3f) intrinsic'' '//OPTIONS )

   answer=merge_str('first string', 'second string is longer',10 == 10)
   if(unit_check_level > 0)then
      write(std_err,g)'['//answer//']',len(answer)
   endif
   call unit_check('merge_str',answer == 'first string'.and.len(answer) == 12,msg='check true value ')

   answer=merge_str('first string', 'second string is longer',10 /= 10)
   if(unit_check_level > 0)then
      write(std_err,g)'['//answer//']',len(answer)
   endif
   call unit_check('merge_str',answer == 'second string is longer'.and.len(answer) == 23,msg='check false value')

   call unit_check_done('merge_str')
end subroutine test_merge_str
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_compact
!-!use M_strings, only: compact
   call unit_check_start('compact',' &
      & -description ''left justify string and replace duplicate whitespace with single characters or nothing'' '//OPTIONS )
   if (compact('  This  is     a    test  ')  /=  'This is a test')then
      call unit_check_bad('compact')
      stop 1
   endif
   if (compact('This is a test')  /=  'This is a test')then
      call unit_check_bad('compact')
      stop 2
   endif
   if (compact('This-is-a-test')  /=  'This-is-a-test')then
      call unit_check_bad('compact')
      stop 3
   endif
   if (compact('  This  is     a    test  ',char='')  /=  'Thisisatest')then
      call unit_check_bad('compact')
      stop 4
   endif
   if (compact('  This  is     a    test  ',char='t')  /=  'Thististattest')then
      call unit_check_bad('compact')
      stop 5
   endif
   call unit_check_good('compact')
end subroutine test_compact
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_noesc  ! test noesc
!-!use M_strings, only : noesc
character(len=23) :: in,out,clr
integer           :: i10
  ! Use goodbad(1) to indicate the test sequence was begun
   call unit_check_start('noesc',' '//OPTIONS )
   do i10=0,127
      write(in, '(i3.3,1x,4a)')i10,char(i10),char(i10),char(i10),' eol'
      write(clr,'(i3.3,1x,"    eol")')i10
      out=noesc(in)
      if(unit_check_level > 0)then
         write(std_err,'(a)')trim(in)
         write(std_err,'(a)')trim(out)
      endif
      SELECT CASE (i10)
      CASE (:31,127)
        if(out /= clr)then
           write(std_err,g)'Error: noesc did not replace a string with blanks that it should have'
           call unit_check_bad('noesc')
        endif
      CASE DEFAULT
        if(in /= out)then
           write(std_err,g)'Error: noesc changed a string it should not have'
           call unit_check_bad('noesc')
        endif
      END SELECT
   enddo
   call unit_check_good('noesc')
end subroutine test_noesc
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_string_to_value
!-!use M_strings, only: string_to_value, s2v, v2s
CHARACTER(len=80) :: STRING
real              :: RVALUE
doubleprecision   :: DVALUE
doubleprecision   :: SUM, SUM2, DELTA
integer           :: IVALUE
integer           :: GOOD
integer           :: ierr
!===================================================================================================================================
   call unit_check_start('string_to_value',' &
      & -description ''generic subroutine returns REAL|DOUBLEPRECISION|INTEGER value from string (a2d,a2r,a2i)'' ' //OPTIONS )
!===================================================================================================================================
   STRING=' -40.5e-2 '
   CALL string_to_value(STRING,RVALUE,IERR)
   CALL string_to_value(STRING,DVALUE,IERR)
   CALL string_to_value(STRING,IVALUE,IERR)
   if(unit_check_level > 0)then
      write(std_err,g) 'string_to_value: real value is ',-40.5e-2
      write(std_err,g) 'string_to_value: double value is ',-40.5d-2
      write(std_err,g) 'string_to_value: real value of string ['//trim(STRING)//'] is ',RVALUE
      write(std_err,g) 'string_to_value: double value of string ['//trim(STRING)//'] is ',DVALUE
      write(std_err,g) 'string_to_value: integer value of string ['//trim(STRING)//'] is ',IVALUE
   endif
   STRING=' -40.5d-2 '
   if(unit_check_level > 0)then
      CALL string_to_value(STRING,RVALUE,IERR)
      write(std_err,g) 'string_to_value: real value of string ['//trim(STRING)//'] is ',RVALUE
      CALL string_to_value(STRING,DVALUE,IERR)
      write(std_err,g) 'string_to_value: double value of string ['//trim(STRING)//'] is ',DVALUE
       CALL string_to_value(STRING,IVALUE,IERR)
      write(std_err,g) 'string_to_value: integer value of string ['//trim(STRING)//'] is ',IVALUE
   endif
   good=0
   call unit_check('string_to_value',rvalue == -40.5e-2)
      good=good*10+1
   call unit_check('string_to_value',dvalue == -40.5d-2)
      good=good*10+1
   call unit_check('string_to_value',dvalue-spacing(dvalue) <= -40.5d-2.and.dvalue+spacing(dvalue) >= -40.5d-2)
      good=good*10+1
   call unit_check('string_to_value',rvalue-spacing(rvalue) <= -40.5e-2.and.rvalue+spacing(rvalue) >= -40.5e-2)
      good=good*10+1
!===================================================================================================================================
   SUM=0.0d0
   string='5.555555555555555555555555555555555'
   CALL string_to_value(STRING,RVALUE,IERR)
   SUM=SUM+RVALUE
   CALL string_to_value(STRING,DVALUE,IERR)
   SUM=SUM+DVALUE
   CALL string_to_value(STRING,IVALUE,IERR)
   SUM=SUM+IVALUE
!===================================================================================================================================
   SUM2=5.555555555555555555555555555555555d0+5.555555555555555555555555555555555e0+INT(5.555555555555555555555555555555555)
   DELTA=spacing(0.0d0)+spacing(0.0)
   if(unit_check_level > 0)then
      write(std_err,'(80("="))')
      write(std_err,g) 'string_to_value: real value is ', 5.555555555555555555555555555555555e0
      write(std_err,g) 'string_to_value: double value is ', 5.555555555555555555555555555555555d0
      write(std_err,g) 'string_to_value: value of string ['//trim(STRING)//'] is ',RVALUE
      write(std_err,g) 'string_to_value: value of string ['//trim(STRING)//'] is ',DVALUE
      write(std_err,g) 'string_to_value: value of string ['//trim(STRING)//'] is ',IVALUE
      write(std_err,g) 'string_to_value: SUM=', SUM
      write(std_err,g) 'string_to_value: SUM2=', SUM2
      write(std_err,g) 'string_to_value: DELTA=', DELTA
   endif
   if(sum == sum2)then
      good=good*10+1
      if(unit_check_level > 0)then
      write(std_err,g)'string_to_value: good ',good
      endif
   else
      call unit_check_bad('string_to_value')
   endif
   if(sum+delta >= sum2.and.sum-delta <= sum2)then
      good=good*10+1
      if(unit_check_level > 0)then
      write(std_err,g)'string_to_value: good ',good
      endif
   else
      call unit_check_bad('string_to_value')
   endif
!===================================================================================================================================
   call unit_check_good('string_to_value')
!===================================================================================================================================
end subroutine test_string_to_value
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_s2v()
doubleprecision SUM, SUM2, DELTA
   SUM2=5.555555555555555555555555555555555d0+5.555555555555555555555555555555555e0+INT(5.555555555555555555555555555555555)
   DELTA=spacing(0.0d0)+spacing(0.0)

   call unit_check_start('s2v',' -description ''function returns doubleprecision value from string'' '//OPTIONS )

   SUM=s2v('5.55555555555555555555555555e0')+REAL(s2v('5.55555555555555555555555555d0'))+INT(s2v('5.55555555555555555555555555'))
   if(unit_check_level > 0)then
      write(std_err,g) 's2v: SUM2=', SUM2
      write(std_err,g) 's2v: SUM=', SUM
      write(std_err,g) 's2v: DELTA=', DELTA
   endif
   call unit_check('s2v',sum+delta >= sum2.and.sum-delta <= sum2, 'SUM=',sum,'SUM2=',sum2,'DELTA=',delta)
   call unit_check_done('s2v')
end subroutine test_s2v
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_value_to_string
!-!use M_strings, only: value_to_string
CHARACTER(LEN=80) :: STRING
doubleprecision   :: DVALUE
real              :: RVALUE
integer           :: IVALUE
integer           :: ILEN
integer           :: IERR
integer           :: IERRSUM=0
!===================================================================================================================================
   call unit_check_start('value_to_string',' &
      & -description ''generic subroutine returns string given numeric REAL|DOUBLEPRECISION|INTEGER value'' '//OPTIONS )
   DVALUE=5.5555555555555555555555d0
   call value_to_string(DVALUE,STRING,ILEN,IERR)
   if(unit_check_level > 0)then
      write(std_err,g)'value_to_string: DOUBLE TEST VALUE=',dvalue,'STRING=',trim(string),' ILEN=',ilen,'IERR=',ierr
   endif
   IERRSUM=IERRSUM+IERR
   if(ILEN <= 0)IERRSUM=IERRSUM+1000

   RVALUE=3.3333333333333333333333
   call value_to_string(RVALUE,STRING,ILEN,IERR)
   if(unit_check_level > 0)then
      write(std_err,g)'value_to_string: REAL TEST VALUE=',rvalue,'STRING=',trim(string),' ILEN=',ilen,'IERR=',ierr
   endif
   IERRSUM=IERRSUM+IERR
   if(ILEN <= 0)IERRSUM=IERRSUM+10000

   IVALUE=1234567890
   call value_to_string(IVALUE,STRING,ILEN,IERR)
   if(unit_check_level > 0)then
      write(std_err,g)'value_to_string: INTEGER TEST VALUE=',ivalue,'STRING=',trim(string),' ILEN=',ilen,'IERR=',ierr
   endif
   IERRSUM=IERRSUM+IERR
   if(string /= '1234567890')then
       IERRSUM=IERRSUM+100000
   endif
   if(ILEN /= 10)then
       IERRSUM=IERRSUM+1000000
   endif

   IVALUE=0
   call value_to_string(IVALUE,STRING,ILEN,IERR)
   if(unit_check_level > 0)then
      write(std_err,g)'value_to_string: INTEGER TEST VALUE=',ivalue,'STRING=',trim(string),' ILEN=',ilen,'IERR=',ierr
   endif

   IVALUE=-12345
   call value_to_string(IVALUE,STRING,ILEN,IERR)
   if(unit_check_level > 0)then
      write(std_err,g)'value_to_string: INTEGER TEST VALUE=',ivalue,'STRING=',trim(string),' ILEN=',ilen,'IERR=',ierr
   endif
   if(string /= '-12345')then
       IERRSUM=IERRSUM+1000000
   endif
   if(ILEN /= 6)then
       IERRSUM=IERRSUM+10000000
   endif
!===================================================================================================================================
   call unit_check('value_to_string',ierrsum == 0,msg='value_to_string'//v2s(ierrsum))
   call unit_check_done('value_to_string')
!===================================================================================================================================
end subroutine test_value_to_string
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_v2s()
!use M_math, only : almost
real            :: SUM
doubleprecision :: SUM2
   call unit_check_start('v2s',' &
      & -description ''generic function returns string given numeric REAL|DOUBLEPRECISION|INTEGER value'' '//OPTIONS )

   SUM2=5.555555555555555555555555555555555d0
   SUM=5.555555555555555555555555555555555e0
!   call unit_check('v2s',almost(REAL(s2v(v2s(SUM))),SUM,7),'real',SUM,REAL(s2v(v2s(SUM))))
!   call unit_check('v2s',almost(s2v(v2s(SUM2)),SUM2,15),'doubleprecision',SUM2,s2v(v2s(SUM)))
   call unit_check('v2s',v2s(1234) == '1234','integer',1234)
   call unit_check('v2s',v2s(.true.) == 'T','logical',v2s(.true.))
   call unit_check('v2s',v2s(.false.) == 'F','logical',v2s(.false.))
   call unit_check_done('v2s')
end subroutine test_v2s
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_isnumber
!-!use M_strings, only: isnumber
   call unit_check_start('isnumber',' '//OPTIONS )
   call unit_check('isnumber',isnumber(' 123 ')                                            ==  1,  'integer string')
   call unit_check('isnumber',isnumber(' -123. ')                                          ==  2,  'whole number string')
   call unit_check('isnumber',isnumber(' -123.0')                                          ==  3,  'real string')
   call unit_check('isnumber',isnumber(' -100.50')                                         ==  3,  'real string')
   call unit_check('isnumber',all( [isnumber('4.4e0 '),isnumber('1e1'),isnumber('-3D-4')]  ==  4), 'exponent string')
   call unit_check('isnumber',isnumber(' Not a number')                                    <  0,  'non-numeric string')
   call unit_check_done('isnumber')
end subroutine test_isnumber
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_trimzeros_()
   call unit_check_start('trimzeros_',' '//OPTIONS )
   call unit_check_done('trimzeros_',msg='UNTESTED')
end subroutine test_trimzeros_
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_listout()
integer,allocatable :: icurve_lists(:)        ! icurve_lists is input array
integer :: icurve_expanded(1000)  ! icurve_expanded is output array
integer :: inums                  ! number of icurve_lists values on input, number of icurve_expanded numbers on output
integer :: i
integer :: ierr
   call unit_check_start('listout',' &
      & -description ''copy ICURVE() to ICURVE_EXPANDED() expanding negative numbers to ranges (1-10 means 1 thru 10)'' '//OPTIONS )
   icurve_lists=[1, 20, -30, 101, 100, 99, 100, -120, 222, -200]
   inums=size(icurve_lists)
   call listout(icurve_lists,icurve_expanded,inums,ierr)
   call unit_check('listout',ierr == 0,msg='check error status ierr='//v2s(ierr))
   call unit_check('listout',all(icurve_expanded(:inums) == [1,(i,i=20,30),101,100,99,(i,i=100,120),(i,i=222,200,-1)]),msg='expand')
   call unit_check_done('listout')
end subroutine test_listout
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_quote()
integer                      :: i
integer,parameter            :: line_length=50
character(len=:),allocatable :: test_in(:)
character(len=:),allocatable :: test_out(:)

   test_in=[ character(len=line_length) ::      &
    'this is a test',                           &
    'test a "quote" around a string'     ]

   test_out=[ character(len=line_length) ::     &
    '"this is a test"',                         &
    '"test a ""quote"" around a string"' ]

   call unit_check_start('quote',' '//OPTIONS )

   do i=1,size(test_in)
      if(unit_check_level > 0)then
         write(std_err,'(a)')'ORIGINAL ['//test_in(i)//']'
         write(std_err,'(a)')'QUOTED   ['//quote(test_in(i))//']'
      endif
      call unit_check('quote',quote(test_in(i)) == test_out(i),quote(test_in(i)),'==>',test_out(i))
   enddo
   call unit_check_done('quote')
end subroutine test_quote
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_unquote()
integer,parameter            :: line_length=1024
character(len=line_length)   :: quoted_str
character(len=:),allocatable :: unquoted_str
character(len=1),parameter   :: esc='\'
character(len=line_length)   :: msg
character(len=line_length)   :: dummy
integer                      :: ios
integer                      :: i
character(len=:),allocatable :: tests(:)

   tests=[ character(len=line_length) :: &
      '"this is a test"',                         &
      '"test a ""quote"" around a string"' ]

   call unit_check_start('unquote',' '//OPTIONS )
   do i=1,size(tests)
      quoted_str=tests(i)
      unquoted_str=unquote(trim(quoted_str),esc)                    ! the string processed by unquote(3f)
      read(quoted_str,*,iostat=ios,iomsg=msg)dummy                  ! read the string list-directed to compare the results
      if(unit_check_level > 0)then
         write(std_err,'(a)')'QUOTED        ['//trim(quoted_str)//']'     ! the original string
         write(std_err,'(a)')'UNQUOTED      ['//unquoted_str//']'
         if(ios /= 0)then
            write(std_err,g)trim(msg)
         else
            write(std_err,'(a)')'LIST DIRECTED ['//trim(dummy)//']'
         endif
      endif
      call unit_check('unquote',unquoted_str == dummy,msg=trim(dummy)//'==>'//unquoted_str)
   enddo
   call unit_check_done('unquote')
end subroutine test_unquote
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_describe
!-!use M_strings, only: describe
integer,parameter             :: number_of_chars=128
character(len=1)              :: char
integer                       :: i
character(len=*),parameter    :: at='@ at (at cost of, at sign, each at, commercial at, commat, &
&rollmop, monkey|pigs|elephant tail, snail, arroba, strudel, asperand, ampersat, rose, cabbage, swirl, whorl)'
!-----------------------------------------------------------------------------------------------------------------------------------
! initialize database description of routine
   call unit_check_start('describe',' -description ''returns a string describing character'' '//OPTIONS )

! call all descriptions to exercise procedure
if(unit_check_level > 0)then
   do i=0,number_of_chars-1
      write(std_err,g)i,char(i),' ',describe(char(i))
   enddo
endif

! unit tests
call unit_check('describe', describe(char( 23) )  ==   'ctrl-W (ETB) end of transmission block' , 'describe ctrl-W')
call unit_check('describe', &
   describe(char( 33) )  ==   '! exclamation point (screamer, gasper, slammer, startler, bang, shriek, pling)' , &
   'describe exclamation point')
call unit_check('describe', describe(char( 52) )  ==   '4 four'                                 , 'describe four')
call unit_check('describe', describe(char( 63) )  ==   '? question mark'                        , 'describe question mark')
call unit_check('describe', describe(char( 64) )  ==   at                                       , 'describe at sign')
call unit_check('describe', describe(char( 74) )  ==   'J majuscule J'                          , 'describe J')
call unit_check('describe', describe(char( 117))  ==   'u miniscule u'                          , 'describe u')
call unit_check('describe', describe(char( 126))  ==   '~ tilde'                                , 'describe tilde')
call unit_check_done('describe')

end subroutine test_describe
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_getvals()
integer,parameter  :: longest_line=256
real               :: rvalues(longest_line/2+1)
integer            :: ivalues(longest_line/2+1)
doubleprecision    :: dvalues(longest_line/2+1)
integer            :: icount,ierr
   call unit_check_start('getvals',' '//OPTIONS )

   call getvals('11,,,22,33,-44, 55 , ,66  ',ivalues,icount,ierr)
   call unit_check('getvals',all(ivalues(:icount) == [11,22,33,-44,55,66]),msg='integer test')

   call getvals('1234.56 3.3333, 5.5555',rvalues,icount,ierr)
   call unit_check('getvals',all(rvalues(:icount) == [1234.56,3.3333,5.5555]),msg='real test')

   call getvals('1234.56d0 3.3333d0, 5.5555d0',dvalues,icount,ierr)
   if(unit_check_level > 0)then
      write(std_err,g)dvalues(:icount)
      write(std_err,g)[1234.56d0,3.3333d0,5.5555d0]
   endif
   call unit_check('getvals',all(dvalues(:icount) == [1234.56d0,3.3333d0,5.5555d0]),msg='double test')

   call unit_check_done('getvals')
end subroutine test_getvals
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_string_to_values()
character(len=80)  :: s=' 10 20e3;3.45 -400.3e-2;1234; 5678 '
integer,parameter  :: isz=10
real               :: array(isz)
integer            :: ierr
integer            :: inums
   call unit_check_start('string_to_values',' -description ''subroutine returns values from a string'' '//OPTIONS )

   call string_to_values(s,10,array,inums,' ;',ierr)
   call unit_check('string_to_values',all(array(:inums) == [10.0,20e3,3.45,-400.3e-2,1234.0,5678.0]),s)
   call string_to_values('10;2.3;3.1416',isz,array,inums,' ;',ierr)
   call unit_check('string_to_values',all(array(:inums) == [10.0,2.3,3.1416]),array(1),array(2),array(3))
   call unit_check('string_to_values',inums == 3,'number of values is',inums)
   call unit_check_done('string_to_values',msg='')
end subroutine test_string_to_values
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_s2vs()
character(len=80)           :: s=' 10 20e3;3.45 -400.3e-2;1234; 5678 '
doubleprecision,allocatable :: values(:)
integer,allocatable         :: ivalues(:)
   call unit_check_start('s2vs',' -description ''function returns a doubleprecision array of numbers from a string'' '//OPTIONS )

   values=s2vs(s)
   ivalues=int(s2vs(s))
   call unit_check('s2vs',size(values) == 6, msg='number of values')
   call unit_check('s2vs',all(ivalues == [10, 20000, 3, -4,1234,5678]))
   call unit_check_done('s2vs')
end subroutine test_s2vs
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_squeeze()
   call unit_check_start('squeeze',' -description ''remove duplicate adjacent characters from strings'''//OPTIONS)
   call unit_check('squeeze',all( &
   & [ character(len=10) :: squeeze('abEeedeeee1','e'),squeeze('geek','e'),squeeze('a  b  c de    A',' ')] ==  &
   & [ character(len=10) :: 'abEede1','gek','a b c de A'] ) )
   call unit_check_done('squeeze')
end subroutine test_squeeze
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_edit_distance()
integer,allocatable         :: ivalues(:)
   call unit_check_start('edit_distance',' -description ''return naive edit distance using Levenshtein algorithm'''//OPTIONS)
   ivalues=[ edit_distance('kittens','sitting'),edit_distance('geek','gesek'),edit_distance('Saturday','Sunday')]
   call unit_check('edit_distance',all(ivalues == [3,1,3]))
   call unit_check_done('edit_distance')
end subroutine test_edit_distance
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_bundle()
   call unit_check_start('bundle',' -description ''return array from list of strings'''//OPTIONS)
   call unit_check('bundle',all(bundle('kittens','sit','three') == ["kittens","sit    ","three  "]), &
                                                                  &"'kittens','sit    ','three  '")
   call unit_check_done('bundle')
end subroutine test_bundle
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_isprint
!-!use M_strings, only: isprint
integer :: i
   call unit_check_start('isprint',' &
      & -description ''elemental function determines if CHR is an ASCII printable character'' '//OPTIONS )
   do i=1,255
      SELECT CASE (i)
      CASE (32:126)
         if (isprint(char(i)) .eqv. .false.)then
            write(std_err,g)'   ',i,isprint(char(i))
            call unit_check_bad('isprint')
            stop 2
         endif
      CASE DEFAULT
         if (isprint(char(i)) .eqv. .true.)then
            write(std_err,g)'   ',i,isprint(char(i))
            call unit_check_bad('isprint')
            stop 3
         endif
      END SELECT
   enddo
call unit_check_good('isprint')
end subroutine test_isprint
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_isgraph
!-!use M_strings, only: isgraph
integer :: i
   call unit_check_start('isgraph',' &
      & -description ''elemental function true if CHR is an ASCII printable character except considers a space non-printable'' '&
      & //OPTIONS )
   do i=1,255
      SELECT CASE (i)
      CASE (33:126)
         if (isgraph(char(i)) .eqv. .false.)then
            write(std_err,g)'   ',i,isgraph(char(i))
            call unit_check_bad('isgraph')
            stop 2
         endif
      CASE DEFAULT
         if (isgraph(char(i)) .eqv. .true.)then
            write(std_err,g)'   ',i,isgraph(char(i))
            call unit_check_bad('isgraph')
            stop 3
         endif
      END SELECT
   enddo
   call unit_check_good('isgraph')
end subroutine test_isgraph
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_isalpha
!-!use M_strings, only: isalpha
integer,parameter             :: number_of_chars=128
character(len=1)              :: ch
integer                       :: i
   call unit_check_start('isalpha',' &
      & -description ''elemental function returns .true. if CHR is a letter and .false. otherwise'' '//OPTIONS )
   do i=0,number_of_chars-1
      ch=char(i)
      SELECT CASE (ch)
      CASE ('a':'z','A':'Z')
         if (isalpha(ch) .eqv. .false.)then
            write(std_err,g)'isalpha: failed on character ',i,isalpha(ch)
            call unit_check_bad('isalpha')
            stop 1
         endif
      CASE DEFAULT
         if (isalpha(ch) .eqv. .true.)then
            write(std_err,g)'isalpha: failed on character ',i,isalpha(ch)
            call unit_check_bad('isalpha')
            stop 2
         endif
      END SELECT
   enddo
   call unit_check_good('isalpha')
end subroutine test_isalpha
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_isxdigit
!-!use M_strings, only: isxdigit
integer,parameter             :: number_of_chars=128
character(len=1)              :: ch
integer                       :: i
!-----------------------------------------------------------------------------------------------------------------------------------
   call unit_check_start('isxdigit',' &
      & -description ''elemental function returns .true. if CHR is a hexadecimal digit (0-9, a-f, or A-F).'' '//OPTIONS )
   do i=0,number_of_chars-1
      ch=char(i)
      SELECT CASE (ch)
      CASE ('a':'f','A':'F','0':'9')
         if (isxdigit(char(i)) .eqv. .false.)then
            write(std_err,g)'isxdigit: failed on character ',i,isxdigit(char(i))
            call unit_check_bad('isxdigit')
            stop 1
         endif
      CASE DEFAULT
         if (isxdigit(char(i)) .eqv. .true.)then
            write(std_err,g)'isxdigit: failed on character ',i,isxdigit(char(i))
            call unit_check_bad('isxdigit')
            stop 2
         endif
      END SELECT
   enddo
   call unit_check_good('isxdigit')
end subroutine test_isxdigit
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_isdigit
!-!use M_strings, only: isdigit
integer,parameter             :: number_of_chars=128
character(len=1)              :: char
integer                       :: i
   call unit_check_start('isdigit',' &
      & -description ''elemental function returns .true. if CHR is a digit (0,1,...,9) and .false. otherwise'' '//OPTIONS )
   do i=0,number_of_chars-1
      SELECT CASE (i)
      CASE (48:57)
         if (isdigit(char(i)) .eqv. .false.)then
            write(std_err,g)'isdigit: failed on character ',i,isdigit(char(i))
            call unit_check_bad('isdigit')
            stop 1
         endif
      CASE DEFAULT
         if (isdigit(char(i)) .eqv. .true.)then
            write(std_err,g)'isdigit: failed on character ',i,isdigit(char(i))
            call unit_check_bad('isdigit')
            stop 2
         endif
      END SELECT
   enddo
   call unit_check_good('isdigit')
end subroutine test_isdigit
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_isblank
!-!use M_strings, only: isblank
integer,parameter             :: number_of_chars=128
character(len=1)              :: char
integer                       :: i
   call unit_check_start('isblank',' &
      & -description ''elemental function returns .true. if CHR is a blank character (space or horizontal tab.'' '//OPTIONS )
   do i=0,number_of_chars-1
      select case (i)
      case (9,32)
         if (isblank(char(i)) .eqv. .false.)then
            write(std_err,g)'isblank: failed on character ',i,isblank(char(i))
            call unit_check_bad('isblank')
            stop 1
         endif
      case default
         if (isblank(char(i)) .eqv. .true.)then
            write(std_err,g)'isblank: failed on character ',i,isblank(char(i))
            call unit_check_bad('isblank')
            stop 2
         endif
      end select
   enddo
   call unit_check_good('isblank')
end subroutine test_isblank
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_isascii
!-!use M_strings, only: isascii
integer,parameter             :: number_of_chars=128
character(len=1)              :: char
integer                       :: i
   call unit_check_start('isascii',' &
      & -description ''elemental function returns .true. if the low order byte of c is in the range char(0) to char(127)'' '&
      & //OPTIONS )
   do i=0,number_of_chars-1
      SELECT CASE (i)
      CASE (0:127)
         if (isascii(char(i)) .eqv. .false.)then
            write(std_err,g)'isascii: failed on character ',i,isascii(char(i))
            call unit_check_bad('isascii')
            stop 1
         endif
      CASE DEFAULT
         if (isascii(char(i)) .eqv. .true.)then
            write(std_err,g)'isascii: failed on character ',i,isascii(char(i))
            call unit_check_bad('isascii')
            stop 2
         endif
      END SELECT
   enddo
   call unit_check_good('isascii')
end subroutine test_isascii
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_isspace
!-!use M_strings, only: isspace
integer,parameter             :: number_of_chars=128
character(len=1)              :: char
integer                       :: i
!-----------------------------------------------------------------------------------------------------------------------------------
   call unit_check_start('isspace',' &
      & -description ''elemental function true if CHR is null, space, tab, carriage return, new line, vertical tab, or formfeed'' '&
      & //OPTIONS )
   do i=0,number_of_chars-1
      SELECT CASE (i)
      CASE (0,9:13,32)
         if (isspace(char(i)) .eqv. .false.)then
            write(std_err,g)'isspace: failed on character ',i,isspace(char(i))
            call unit_check_bad('isspace')
            stop 1
         endif
      CASE DEFAULT
         if (isspace(char(i)) .eqv. .true.)then
            write(std_err,g)'isspace: failed on character ',i,isspace(char(i))
            call unit_check_bad('isspace')
            stop 2
         endif
      END SELECT
   enddo
   call unit_check_good('isspace')
end subroutine test_isspace
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_iscntrl
!-!use M_strings, only: iscntrl
integer,parameter             :: number_of_chars=128
character(len=1)              :: char
integer                       :: i
!-----------------------------------------------------------------------------------------------------------------------------------
   call unit_check_start('iscntrl',' &
      & -description ''elemental function returns .true. if CHR is a delete character or ordinary control character'' '//OPTIONS )
   do i=0,number_of_chars-1
      SELECT CASE (i)
      CASE (0:31,127)
         if (iscntrl(char(i)) .eqv. .false.)then
            write(std_err,g)'iscntrl: failed on character ',i,iscntrl(char(i))
            call unit_check_bad('iscntrl')
            stop 1
         endif
      CASE DEFAULT
         if (iscntrl(char(i)) .eqv. .true.)then
            write(std_err,g)'iscntrl: failed on character ',i,iscntrl(char(i))
            call unit_check_bad('iscntrl')
            stop 2
         endif
      END SELECT
   enddo
   call unit_check_good('iscntrl')
end subroutine test_iscntrl
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_ispunct
!-!use M_strings, only: ispunct
integer,parameter             :: number_of_chars=128
character(len=1)              :: char
integer                       :: i
!-----------------------------------------------------------------------------------------------------------------------------------
   call unit_check_start('ispunct',' &
      & -description ''elemental function returns .true. if CHR is a printable punctuation character'' '//OPTIONS )
   do i=0,number_of_chars-1
      SELECT CASE (i)
      CASE (33:47, 58:64, 91:96, 123:126)
         if (ispunct(char(i)) .eqv. .false.)then
            write(std_err,g)'ispunct: failed on character ',i,ispunct(char(i))
            call unit_check_bad('ispunct')
            stop 1
         endif
      CASE DEFAULT
         if (ispunct(char(i)) .eqv. .true.)then
            write(std_err,g)'ispunct: failed on character ',i,ispunct(char(i))
            call unit_check_bad('ispunct')
            stop 2
         endif
      END SELECT
   enddo
   call unit_check_good('ispunct')
end subroutine test_ispunct
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_isupper
!-!use M_strings, only: isupper
integer,parameter             :: number_of_chars=128
character(len=1)              :: ch
integer                       :: i
!-----------------------------------------------------------------------------------------------------------------------------------
   call unit_check_start('isupper',' &
      & -description ''elemental function returns .true. if CHR is an uppercase letter (A-Z)'' '//OPTIONS )
   do i=0,number_of_chars-1
      ch=char(i)
      SELECT CASE (ch)
      CASE ('A':'Z')
         if (isupper(ch) .eqv. .false.)then
            write(std_err,g)'isupper: failed on character ',i,isupper(ch)
            call unit_check_bad('isupper')
            stop 1
         endif
      CASE DEFAULT
         if (isupper(ch) .eqv. .true.)then
            write(std_err,g)'isupper: failed on character ',i,isupper(ch)
            call unit_check_bad('isupper')
            stop 2
         endif
      END SELECT
   enddo
   call unit_check_good('isupper')
end subroutine test_isupper
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_islower
!-!use M_strings, only: islower
integer,parameter             :: number_of_chars=128
character(len=1)              :: ch
integer                       :: i
!-----------------------------------------------------------------------------------------------------------------------------------
   call unit_check_start('islower',' &
      & -description ''elemental function returns .true. if CHR is a miniscule letter (a-z)'' '//OPTIONS )
   do i=0,number_of_chars-1
      ch=char(i)
      SELECT CASE (ch)
      CASE ('a':'z')
         if (islower(ch) .eqv. .false.)then
            write(std_err,g)'islower: failed on character ',i,islower(ch)
            call unit_check_bad('islower')
            stop 1
         endif
      CASE DEFAULT
         if (islower(ch) .eqv. .true.)then
            write(std_err,g)'islower: failed on character ',i,islower(ch)
            call unit_check_bad('islower')
            stop 2
         endif
      END SELECT
   enddo
   call unit_check_good('islower')
end subroutine test_islower
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_isalnum
!-!use M_strings, only: isalnum
integer,parameter             :: number_of_chars=128
character(len=1)              :: ch
integer                       :: i
!-----------------------------------------------------------------------------------------------------------------------------------
   call unit_check_start('isalnum',' &
      & -description ''elemental function returns .true. if CHR is a letter or digit'' '//OPTIONS )
   do i=0,number_of_chars-1
      ch=char(i)
      SELECT CASE (ch)
      CASE ('a':'z','A':'Z','0':'9')
         if (isalnum(char(i)) .eqv. .false.)then
            write(std_err,g)'isalnum: failed on character ',i,isalnum(char(i))
            call unit_check_bad('isalnum')
            stop 1
         endif
      CASE DEFAULT
         if (isalnum(char(i)) .eqv. .true.)then
            write(std_err,g)'isalnum: failed on character ',i,isalnum(char(i))
            call unit_check_bad('isalnum')
            stop 2
         endif
      END SELECT
   enddo
   call unit_check_good('isalnum')
end subroutine test_isalnum
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_nint()
   call unit_check_start('nint',' ' //OPTIONS )
   call unit_check('nint',nint('1234.4') == 1234,msg='test string to integer for overloaded NINT("1234.4")')
   call unit_check('nint',nint('1234.5') == 1235,msg='test string to integer for overloaded NINT("1234.5")')
   call unit_check('nint',nint('1234.6') == 1235,msg='test string to integer for overloaded NINT("1234.6")')
   call unit_check_done('nint',msg=' overload of NINT()')
end subroutine test_nint
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_int()
   call unit_check_start('int',' ' //OPTIONS )
   call unit_check('int',int('1234') == 1234,msg='test string to integer for overloaded INT()')
   call unit_check_done('int',msg=' overload of INT()')
end subroutine test_int
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_real()
   call unit_check_start('real',' '//OPTIONS )
   call unit_check('real', real('3.0d0') == 3.0d0,msg='test string to real for overloaded REAL()')
   call unit_check_done('real',msg='overload of REAL(3f)')
end subroutine test_real
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_dble()
   call unit_check_start('dble',' '//OPTIONS )
   call unit_check('dble', dble('3.0d0') == 3.0d0,msg='test string to double for overloaded DBLE()')
   call unit_check_done('dble',msg='overload of DBLE(3f)')
end subroutine test_dble
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_setbits()
   !character(len=:),allocatable :: string
   call unit_check_start('setbits',' -description    ''set all bits in an INTEGER word with a string'' '//OPTIONS )
   !string='11111111'
   !call unit_check('setbits',setbits(string) == 0,setbits(string))
   !string='1111111111111111'
   !call unit_check('setbits',setbits(string) == 0,setbits(string))
   !string='11111111111111111111111111111111'
   !call unit_check('setbits',setbits(string) == 0,setbits(string))
   !string='1111111111111111111111111111111111111111111111111111111111111111'
   !call unit_check('setbits',setbits(string) == 0,setbits(string))
   call unit_check_done('setbits',msg='setbits(3f) tests completed')
end subroutine test_setbits
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_match_delimiter()
   !character(len=:),allocatable :: string
   call unit_check_start('match_delimiter',' -description    ''find matching delimiter'' '//OPTIONS )
   !string='11111111'
   !call unit_check('match_delimiter',match_delimiter(string) == 0,match_delimiter(string))
   !string='1111111111111111'
   !call unit_check('match_delimiter',match_delimiter(string) == 0,match_delimiter(string))
   !string='11111111111111111111111111111111'
   !call unit_check('match_delimiter',match_delimiter(string) == 0,match_delimiter(string))
   !string='1111111111111111111111111111111111111111111111111111111111111111'
   !call unit_check('match_delimiter',match_delimiter(string) == 0,match_delimiter(string))
   call unit_check_done('match_delimiter',msg='match_delimiter(3f) tests completed')
end subroutine test_match_delimiter
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
end module M_testsuite_M_strings
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
program runtest
use M_framework
use M_framework__verify, only : unit_check_level, unit_check_stop
use M_testsuite_M_strings
implicit none
!  unit_check_level=1
   unit_check_level=0
   call test_suite_M_strings()
   call unit_check_stop()
end program runtest
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
