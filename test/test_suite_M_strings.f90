module M_testsuite_M_strings
use,intrinsic :: iso_fortran_env,only : std_in=>input_unit,std_out=>output_unit,std_err=>error_unit
use M_framework,  only : unit_test_level,  unit_test_stop, unit_test_msg, str, &
                       & unit_test_start, unit_test,  unit_test_end, unit_test_stop
use M_strings
implicit none
character(len=*),parameter :: options=' -section 3 -library libGPF -filename `pwd`/M_strings.FF &
& -documentation y -prep y -ccall  n -archive GPF.a '
character(len=*),parameter :: g='(*(g0,1x))'
logical,parameter :: T=.true., F=.false.
character(len=*),parameter :: lc='abcdefghijklmnopqrstuvwxyz0123456789'
character(len=*),parameter :: uc='ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789'
contains
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_suite_m_strings()
   call test_adjustc()
   call test_aton()
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
   call test_dilate()
   call test_edit_distance()
   call test_ends_with()
   call test_expand()
   call test_find_field()
   call test_fortran_name()
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
   call test_longest_common_substring()
   call test_lower()
   call test_lpad()
   call test_matching_delimiter()
   call test_merge_str()
   call test_modif()
   call test_msg()
   call test_m_strings()
   call test_nint()
   call test_noesc()
   call test_nospace()
   call test_notabs()
   call test_pad()
   call test_paragraph()
   call test_quote()
   call test_real()
   call test_replace()
   call test_reverse()
   call test_rotate13()
   call test_percent_encode()
   call test_rpad()
   call test_s2c()
   call test_s2v()
   call test_s2vs()
   call test_sep()
   call test_setbits()
   call test_split()
   call test_split2020()
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
   call test_upper_quoted()
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
   call unit_test_start('glob','[COMPARE] match string with a pattern containing * and ? wildcard characters')
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

   call unit_test_end('glob')
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
      if(i==1)call unit_test('glob',bExpectedResult.eqv.bResult,'string',tame,'pattern',wild,'expected',bExpectedResult)
   end function test
end subroutine test_glob
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_replace()
character(len=:),allocatable :: targetline

   call unit_test_start('replace','[EDITING] function replaces one substring for another in string')
   targetline='this is the input string'

   call testit('th','TH','THis is THe input string')

   ! a null old substring means "at beginning of line"
   call testit('','BEFORE:', 'BEFORE:THis is THe input string')

   ! a null new string deletes occurrences of the old substring
   call testit('i','', 'BEFORE:THs s THe nput strng')

   targetline=replace('a b ab baaa aaaa aa aa a a a aa aaaaaa','aa','CCCC',occurrence=3,repeat=3)
   call unit_test('replace',targetline == 'a b ab baaa aaCCCC CCCC CCCC a a a aa aaaaaa','example of using RANGE',targetline)

   targetline=replace('10-9','-',' -')
   call unit_test('replace',targetline == '10 -9','example of leading space in new=',targetline)

   targetline=replace('12-10','-',' -')
   call unit_test('replace',targetline == '12 -10','example of leading space in new=',targetline)

   targetline=replace('-','-',' -')
   call unit_test('replace',targetline == ' -','spaces=',targetline)

   targetline=replace('---','-',' -')
   call unit_test('replace',targetline == ' - - -','spaces=',targetline)

   targetline=replace('12-10-','-',' -')
   call unit_test('replace',targetline == '12 -10 -','spaces=',targetline)

   call unit_test_end('replace',msg='finished test of replacing substrings')

contains
subroutine testit(old,new,expected)
character(len=*),intent(in) :: old,new,expected
character(len=:),allocatable :: info, original
   original=targetline
   targetline=replace(targetline,old,new)
   if(unit_test_level > 0)then
      info=str('GIVEN[',original,']OLD[',old,']NEW['//new//']GOT[',targetline,']EXPECTED['//expected,']',sep='')
   else
      info=''
   endif
   call unit_test('replace',targetline == expected,info)
end subroutine testit
end subroutine test_replace
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_join()
character(len=:),allocatable  :: s(:)
   call unit_test_start('join','[EDITING] append CHARACTER variable array into a single CHARACTER with specified separator')
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
   call unit_test_end('join',msg='join array of strings into a single string controlling separators and white space')
contains
subroutine testit(generated,expected)
character(len=*),intent(in) :: generated
character(len=*),intent(in) :: expected
   if(unit_test_level > 0)then
      write(std_err,g)'JOIN(3F) TEST'
      write(std_err,g)'INPUT       ','['//s//']'
      write(std_err,g)'GENERATED   ',generated
      write(std_err,g)'EXPECTED    ',expected
   endif
   call unit_test('join',generated == expected,msg='output is '//generated)
end subroutine testit
end subroutine test_join
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_base()
   character(len=:),allocatable :: in(:)
   character(len=:),allocatable :: expected(:)
   call unit_test_start('base','[BASE] convert whole number string in base [2-36] to string in alternate base [2-36]')

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

   call unit_test_end('base')
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
           call unit_test('base',answer == answers(i), &
              'converting '//trim(in(i))//' got '//trim(answers(i))//' expected '//trim(answer),wordy=F )
       else
          call unit_test('base',F,msg='Error in decoding/encoding number.')
       endif
   enddo
end subroutine checkit
end subroutine test_base
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_decodebase()
character(len=:),allocatable :: in(:)
integer,allocatable          :: expected(:)
   call unit_test_start('decodebase','[BASE] convert whole number string in base [2-36] to base 10 number')

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

   call unit_test_end('decodebase')
contains
subroutine checkit(in,answers,inbase)
character(len=*),intent(in)  :: in(:)
integer,intent(in)           :: answers(:)
integer,intent(in)           :: inbase
integer                      :: answer
integer                      :: i
   do i=1,size(in)
      if(decodebase(in(i),inbase,answer))then
           call unit_test('decodebase',answer == answers(i),'converting '//trim(in(i)) ,wordy=F)
       else
          call unit_test('decodebase',F,msg='Error in decoding/encoding number.')
       endif
   enddo
end subroutine checkit
end subroutine test_decodebase
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_base2()
character(len=:),allocatable :: in(:)
integer,allocatable          :: expected(:)
   call unit_test_start('base2','[BASE] convert whole number to string in base 2')

   ! convert base10 values to base2 strings
   in=[character(len=32) :: '10','1010','101010','10101010','1010101010','101010101010']
   expected=[2,10,42,170,682,2730]
   call checkit(in,expected)

   call unit_test_end('base2')
contains
subroutine checkit(answer,values)
character(len=*),intent(in)  :: answer(:)
integer,intent(in)           :: values(:)
character(len=32)            :: out
integer                      :: i
   do i=1,size(answer)
      call unit_test('base2',base2(values(i)) == answer(i), &
       & 'checking for '//trim(answer(i))//' in base 2 from value '//v2s(values(i)) )
   enddo
end subroutine checkit
end subroutine test_base2
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_codebase()
character(len=:),allocatable :: in(:)
integer,allocatable          :: expected(:)
   call unit_test_start('codebase','[BASE] convert whole number in base 10 to string in base [2-36]')

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

   call unit_test_end('codebase')
contains
subroutine checkit(answer,values,outbase)
character(len=*),intent(in)  :: answer(:)
integer,intent(in)           :: values(:)
integer,intent(in)           :: outbase
character(len=32)            :: out
integer                      :: i
   do i=1,size(answer)
      if(codebase(values(i),outbase,out) )then
           call unit_test('codebase',out == answer(i), &
              'checking for '//trim(answer(i))//' in base '//v2s(outbase)//' from value '//v2s(values(i)),wordy=F )
       else
          call unit_test('codebase',F,msg='Error answer decoding/encoding number.')
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
   call unit_test_start('delim','[TOKENS] parse a string and store tokens into an array')
   line=' first  second 10.3 words_of_stuff  '
   dlm=' '

   call testit()
   call unit_test('delim',icount == 4,msg=' check number of tokens')
   call unit_test('delim',ilen == 34,msg=' check position of last character')
   call unit_test('delim',all(array(:icount) == [character(len=20) :: 'first','second','10.3','words_of_stuff']),msg='tokens')

   ! change delimiter list and what is calculated or parsed
   dlm=' aeiou'    ! NOTE SPACE IS FIRST
   call testit()
   call unit_test('delim',all(array(:icount) == &
           [character(len=10) :: 'f','rst','s','c','nd','10.3','w','rds_','f_st','ff']),msg='delims')

  call unit_test_end('delim')
contains
subroutine testit()
integer                 :: i10
   ! show line being parsed
   ! call parsing procedure
   call delim(line,array,n,icount,ibegin,iterm,ilen,dlm)

   if(unit_test_level > 0)then
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
INTRINSIC SIZE
CHARACTER(LEN=:),ALLOCATABLE    :: line
CHARACTER(LEN=:),ALLOCATABLE    :: order
CHARACTER(LEN=:),ALLOCATABLE    :: dlm
CHARACTER(LEN=:),ALLOCATABLE    :: array(:)
character(len=10)               :: orders(3)=['sequential', '          ', 'reverse   ' ]
! return strings composed of delimiters or not IGNORE|RETURN|IGNOREEND
character(len=10)               :: nulls(3)=['ignore    ', 'return    ', 'ignoreend ' ]
!
   call unit_test_start('split','[TOKENS] parse string into an array using specified delimiters')

   dlm=''
   LINE='abcdef ghijklmnop qrstuvwxyz  1:2  333333 a b cc    '
   order=orders(3)
   CALL testit()
   CALL split(line,array,dlm,order,nulls(1))
   if(unit_test_level > 0)then
      write(std_err,g)size(array)
   endif
   order=orders(2)
   CALL split(line,array,dlm,order,nulls(2))
   if(unit_test_level > 0)then
      write(std_err,g)size(array)
   endif
   order=orders(1)
   CALL split(line,array,dlm,order,nulls(3))
   if(unit_test_level > 0)then
      write(std_err,g)size(array)
   endif
!
   LINE=' abcdef ghijklmnop qrstuvwxyz  1:2  333333 a b cc    '
   CALL testit()
!
   LINE='        abcdef ghijklmnop qrstuvwxyz  1:2  333333 a b cc    '
   CALL testit()
!
   LINE=' aABCDEF  ; b;;c d e;  ;  '
   CALL testit()
!
   dlm=';'
   CALL testit()
!
   dlm='; '
   CALL testit()
!
   dlm=';'
   LINE=';;;abcdef;ghijklmnop;qrstuvwxyz;;1:2;;333333;a;b;cc;;'
   CALL testit()
   LINE=';;abcdef;ghijklmnop;qrstuvwxyz;;1:2;;333333;a;b;cc;'
   CALL testit()
   LINE=';abcdef;ghijklmnop;qrstuvwxyz;;1:2;;333333;a;b;cc;'
   CALL testit()
   LINE='abcdef;ghijklmnop;qrstuvwxyz;;1:2;;333333;a;b;cc'
   CALL testit()
!
   line='a b c d e f g h i j k l m n o p q r s t u v w x y z'
   CALL split(line,array)
   call unit_test('split',size(array) == 26,msg='test delimiter')
!
   dlm=' '
   CALL split(line,array,dlm)
   call unit_test('split',size(array) == 26,msg='test delimiter')
!
   call unit_test_end('split')
!
   CONTAINS
   SUBROUTINE testit()
   integer :: i
   if(unit_test_level > 0)then
      write(std_err,'(80("="))')
      write(std_err,'(A)')'parsing ['//TRIM(line)//']'//'with delimiters set to ['//dlm//'] and order '//trim(order)
   endif
   CALL split(line,array,dlm,order)
   if(unit_test_level > 0)then
      write(std_err,'("number of tokens found=",i0)')SIZE(array)
      write(std_err,'(I0,T10,A)')(i,TRIM(array(i)),i=1,SIZE(array))
   endif
   END SUBROUTINE testit
end subroutine test_split
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_m_strings
character(len=1)            :: chars(36)
call unit_test_start('combined')
! COMBINED TESTS
chars=switch(uc)     ! convert string to character array
chars=chars(36:1:-1) ! reverse order of characters
call unit_test('combined',lower(reverse(switch(chars))) == lc,msg='combined lower(),reverse(),switch()')
if(unit_test_level > 0)then
   write(std_err,g)'isprint'
   write(std_err,g)'   letter a      ',isprint('a')
   write(std_err,g)'   horizontal tab',isprint(char(9))
   write(std_err,g)'   array of letters;.',isprint([';','.',' '])
   write(std_err,g)'   array of letters',isprint(switch(uc))
   write(std_err,g)'   array of letters',isprint(uc)
endif
if(unit_test_level > 0)then
   write(std_err,g)'isgraph'
   write(std_err,g)'   letter a      ',isgraph('a')
   write(std_err,g)'   horizontal tab',isgraph(char(9))
   write(std_err,g)'   array of letters;.',isgraph([';','.',' '])
   write(std_err,g)'   array of letters',isgraph(switch(uc))
   write(std_err,g)'   array of letters',isgraph(uc)
endif
call unit_test_end('combined')
end subroutine test_m_strings
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_chomp()
character(len=:),allocatable  :: str
character(len=:),allocatable  :: token
character(len=66),allocatable :: delimiters
integer                       :: ipass
   call unit_test_start('chomp','[TOKENS] Tokenize a string, consuming it one token per call')

   str = 'a b ccc ddd x12#$)$*#@Z1!( ab cd ef'
   delimiters=' #@$)*!('
   ipass=0
   do while ( chomp(str,token,delimiters)  >=  0 )
      ipass=ipass+1
      if(unit_test_level > 0)then
         write( std_err,g) ipass,'TOKEN=['//trim(token)//']'
      endif
      select case(ipass)
      case(1); call unit_test('chomp', token == 'a'   ,'token=',token)
      case(2); call unit_test('chomp', token == 'b'   ,'token=',token)
      case(3); call unit_test('chomp', token == 'ccc' ,'token=',token)
      case(4); call unit_test('chomp', token == 'ddd' ,'token=',token)
      case(5); call unit_test('chomp', token == 'x12' ,'token=',token)
      case(6); call unit_test('chomp', token == 'Z1'  ,'token=',token)
      case(7); call unit_test('chomp', token == 'ab'  ,'token=',token)
      case(8); call unit_test('chomp', token == 'cd'  ,'token=',token)
      case(9); call unit_test('chomp', token == 'ef'  ,'token=',token)
      end select
   enddo

   call unit_test_end('chomp')
end subroutine test_chomp
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_substitute
character(len=80),allocatable   :: targetline   ! input line to be changed, MUST BE LONG ENOUGH TO HOLD CHANGES
character(len=:),allocatable    :: old          ! old substring to replace
character(len=:),allocatable    :: new          ! new substring
character(len=:),allocatable    :: result
character(len=:),allocatable    :: expected
character(len=:),allocatable    :: string
integer                         :: ml           ! ml sets the left  margin
integer                         :: mr           ! mr sets the right margin
integer                         :: ier          ! error code. if ier = -1 bad directive, >= 0then ier changes made
   call unit_test_start('substitute','[EDITING] subroutine globally substitutes one substring for another in string')
   targetline='This an that and any other '
   old='an'
   new='##'
   ml=1
   mr=len(targetline)

   call substitute(targetline,old,new,ier,ml,mr) !Globally substitute one substring for another in string
   call unit_test('substitute',ier ==3,'test if ier is 3, ier=',ier,'targetline=',targetline)
   call unit_test('substitute',targetline == 'This ## that ##d ##y other ','checking en to ##')
   ! NOTE: with one targetline all tests pass or get red herrings
   targetline=' This an that and any other '

   old=''
   new='BEGINNING:'
   expected='BEGINNING: This an that and any other'
   call tryit()

   old='This'
   new='THIS'
   expected='BEGINNING: THIS an that and any other'
   call tryit()

   old='that'
   new='LONGER STRING'
   expected='BEGINNING: THIS an LONGER STRING and any other'
   call tryit()

   old='LONGER STRING '
   new=''
   expected='BEGINNING: THIS an and any other'
   call tryit()

   call unit_test_end('substitute')

   contains

   subroutine tryit()
      string='OLD:'//old//' NEW:'//new
      if(unit_test_level > 0) string=string //' ORIGINAL: '//targetline
      !call substitute(targetline,old,new)
      ml=1
      mr=len(targetline)
      call substitute(targetline,old,new,ier,ml,mr)
      call unit_test('substitute',targetline == expected,string)
   end subroutine tryit

end subroutine test_substitute
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_change
!character(len=132) :: direc
character(len=132)  :: line=' The rain in Spain falls mainly on the plain. '
integer             :: ier
   if(unit_test_level > 0)then
      write(std_err,g)' LINE='//trim(line)
   endif
   ! indicate test of change(3f) has begun
   call unit_test_start('change','[EDITING] change old string to new string with a directive like a line editor')

   call change(line, 'c/ain/AIN'     ,ier)
   call unit_test('change',ier == 4,'without trailing slash,expect ier==4, ier=',ier)

   call change(line, 'c/ The r/R/'   ,ier)
   call unit_test('change',ier == 1,'with trailing slash,expect ier==1,ier=',ier)

   call change(line, 'c/ /'          ,ier)
   call unit_test('change',ier >= 7,'no new string, expect ier >= 7,ier=',ier) ! remove spaces

   call change(line, 'c//PREFIX:'    ,ier)
   call unit_test('change',ier == 1,'null new string, expect ier == 1, ier=',ier)

   call change(line, 'c/XYZ/xxxxxx:' ,ier)
   call unit_test('change',ier == 0,'no matching old string, expect ier == 0, ier=',ier)

   if(unit_test_level > 0)then
      write(std_err,g)'IER=',ier,' LINE='//trim(line)
   endif
   call unit_test('change','PREFIX:RAINinSpAINfallsmAINlyontheplAIN.'  ==  line,'check cumulative changes')

   call unit_test_end('change') ! indicate test of change(3f) passed
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
   call unit_test_start('strtok','[TOKENS] Tokenize a string')
   istart_expected=[ 2,  7,  10,  12,  17,  20,  28,  32,  35 ]
   iend_expected=[ 5,  8,  10,  15,  18,  25,  30,  32,  35 ]
   words_expected=[ character(len=10) :: 'this', 'is', 'a', 'test', 'of', 'strtok', 'A:B', ':', 'C']

   inline=' this is a test of strtok; A:B :;,C;;'

   itoken=0 ! must set ITOKEN=0 before looping on strtok(3f) on a new string.
   if(unit_test_level > 0)then
      write(std_err,g)trim(inline)
   endif
   do while ( strtok(inline,itoken,is,ie,delimiters) )
      istart(itoken)=is
      iend(itoken)=ie
      if(unit_test_level > 0)then
         write( std_err,g) itoken,'TOKEN=['//(inline(istart(itoken):iend(itoken)))//']',istart(itoken),iend(itoken)
      endif
   enddo
   call unit_test('strtok',all(istart(:itoken) == istart_expected) .and. &
      all(iend(:itoken) == iend_expected), &
      msg='parse a line into tokens with strtok(3f)')
   call unit_test_end('strtok')
end subroutine test_strtok
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_modif()
character(len=256)           :: line
character(len=:),allocatable :: COMMAND_LINE
   call unit_test_start('modif','[EDITING] emulate the MODIFY command from the line editor XEDIT')
   line='ABCDEFGHIJKLMNOPQRSTUVWXYZ'
   command_line='###%aaaa# 1 2 3&  ^up'
   call unit_test('modif',line == 'ABCDEFGHIJKLMNOPQRSTUVWXYZ',msg=line)
   call unit_test('modif',line == 'ABCDEFGHIJKLMNOPQRSTUVWXYZ',msg=command_line)
   call modif(line,COMMAND_LINE)
   command_line='###%aaaa# 1 2 3&  ^up'
   call unit_test('modif',line == '%aaaaJ1L2N3 QRupSTUVWXYZ',msg=line)
   call unit_test_end('modif')
end subroutine test_modif
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_len_white()
   call unit_test_start('len_white','[LENGTH] get length of string trimmed of whitespace.')
   call unit_test('len_white',len_white('A b c  '//char(9)//char(10)//char(11)//char(12)//char(13)) == 5,msg='')
   call unit_test_end('len_white',msg='')
end subroutine test_len_white
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_clip()
   call unit_test_start('clip','[WHITESPACE] trim leading and trailing blanks from a string')
   call unit_test('clip',clip('    A B CC D      ') == 'A B CC D',msg='clip string test 1')
   call unit_test('clip',clip('A B CC D') == 'A B CC D',msg='clip string test 2')
   call unit_test('clip',clip('A B CC D    ') == 'A B CC D',msg='clip string test 3')
   call unit_test('clip',clip('     A B CC D    ') == 'A B CC D',msg='clip string test 4')
   call unit_test_end('clip')
end subroutine test_clip
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_crop()
   call unit_test_start('crop','[WHITESPACE] trim leading and trailing blanks and control characters from a string')
   call unit_test('crop',crop('    A B CC D      ') == 'A B CC D',msg='crop string test 1')
   call unit_test('crop',crop('A B CC D') == 'A B CC D',msg='crop string test 2')
   call unit_test('crop',crop('A B CC D    ') == 'A B CC D',msg='crop string test 3')
   call unit_test('crop',crop('     A B CC D    ') == 'A B CC D',msg='crop string test 4')
   call unit_test_end('crop')
end subroutine test_crop
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_transliterate
   call unit_test_start('transliterate','[EDITING] replace characters from old set with characters from new set')
   call unit_test('transliterate',transliterate('AbCDefgHiJklmnoPQRStUvwxyZ',lc,uc) == uc(1:26),msg='transliterate to uppercase')
   call unit_test('transliterate',transliterate('AbCDefgHiJklmnoPQRStUvwxyZ',uc,lc) == lc(1:26),msg='transliterate to lowercase')
   call unit_test_end('transliterate')
end subroutine test_transliterate
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_percent_encode()
character(len=:),allocatable  :: s
character(len=:),allocatable  :: e
integer :: i
   call unit_test_start('percent_encode','[ENCODE] percent-encode a string')
   s='[this is a string]'
   e='%5Bthis%20is%20a%20string%5D'
   ! add //'' to change function call to expression to avoid gfortran bug
   call unit_test('percent_encode',percent_encode(s) == e,  s,'==>',percent_encode(s)//'')

   s=switch([(achar(i),i=0,255)])
   e='%00%01%02%03%04%05%06%07%08%09%0A%0B%0C%0D%0E%0F%10&
     &%11%12%13%14%15%16%17%18%19%1A%1B%1C%1D%1E%1F%20%21&
     &%22%23%24%25%26%27%28%29%2A%2B%2C-.%2F0123456789%3A&
     &%3B%3C%3D%3E%3F%40ABCDEFGHIJKLMNOPQRSTUVWXYZ%5B%5C%5D%5E_&
     &%60abcdefghijklmnopqrstuvwxyz%7B%7C%7D~%7F%80%81%82%83%84%85&
     &%86%87%88%89%8A%8B%8C%8D%8E%8F%90%91%92%93%94%95%96%97&
     &%98%99%9A%9B%9C%9D%9E%9F%A0%A1%A2%A3%A4%A5%A6%A7%A8%A9&
     &%AA%AB%AC%AD%AE%AF%B0%B1%B2%B3%B4%B5%B6%B7%B8%B9%BA%BB&
     &%BC%BD%BE%BF%C0%C1%C2%C3%C4%C5%C6%C7%C8%C9%CA%CB%CC%CD&
     &%CE%CF%D0%D1%D2%D3%D4%D5%D6%D7%D8%D9%DA%DB%DC%DD%DE%DF&
     &%E0%E1%E2%E3%E4%E5%E6%E7%E8%E9%EA%EB%EC%ED%EE%EF%F0%F1&
     &%F2%F3%F4%F5%F6%F7%F8%F9%FA%FB%FC%FD%FE%FF'
   ! add //'' to change function call to expression to avoid gfortran bug
   call unit_test('percent_encode',percent_encode(s) == e,  'entire ASCII256 set')

   call unit_test_end('percent_encode',msg='')
end subroutine test_percent_encode
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_rotate13()
character(len=:),allocatable  :: s
character(len=:),allocatable  :: e
   call unit_test_start('rotate13','[ENCODE] apply trivial ROT13 encryption to a string')
   s='United we stand, divided we fall.'
   e='Havgrq jr fgnaq, qvivqrq jr snyy.'
   ! add //'' to change function call to expression to avoid gfortran bug
   call unit_test('rotate13',rotate13(s) == e,  s,'==>',rotate13(s)//'')
   call unit_test_end('rotate13',msg='')
end subroutine test_rotate13
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_reverse
   call unit_test_start('reverse','elemental function reverses character order in a string')
   if(reverse(lc) == '9876543210zyxwvutsrqponmlkjihgfedcba')then
      call unit_test('reverse',.true.)
   else
      if(unit_test_level > 0)then
         write(std_err,g)'error: reverse '
         write(std_err,g)'iN:  ['//lc//']'
         write(std_err,g)'OUT: ['//reverse(lc)//']'
      endif
      call unit_test('reverse',.false.)
   endif
   call unit_test_end('reverse')

end subroutine test_reverse
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_upper
character(len=36),parameter :: rnge='abcdefghIJKLMNopqrstuvwxyz0123456789'

   call unit_test_start('upper','[CASE] changes a string to uppercase')
   call unit_test('upper',upper(lc) == uc,upper(lc))
   call unit_test('upper',upper(lc,9,14) == rnge,'range',upper(lc,9,14),'expected',rnge)
   call unit_test_end('upper')

end subroutine test_upper
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_lower
character(len=36),parameter :: rnge='ABCDEFGHijklmnOPQRSTUVWXYZ0123456789'

   call unit_test_start('lower','[CASE] changes a string to lowercase over specified range')
   call unit_test('lower',lower(uc) == lc,'lower',lower(uc),'expected',lc)
   call unit_test('lower',lower(uc,9,14) == rnge,'range',lower(uc,9,14),'expected',rnge)
   call unit_test_end('lower')

end subroutine test_lower
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_switch
character(len=1)            :: chars(36)
integer :: i

call unit_test_start('switch','[ARRAY] converts between CHARACTER scalar and array of single characters')
call unit_test('switch',size(switch(uc)).eq.36,' expected 36 characters, got',switch(switch(uc)))
chars=switch(uc)
call unit_test('switch',all([(chars(i) == uc(i:i),i=1,size(chars))]),'check chars are same as string')
call unit_test_msg('switch', 'put string UC into array CHARS',if=unit_test_level > 0)
chars=switch(uc)
call unit_test_msg('switch', 'put CHARS array into CHARS array in reverse order like reverse',if=unit_test_level > 0)
chars=chars(36:1:-1)
call unit_test_msg('switch', 'put CHARS array into string reversed and compare to original UC string',if=unit_test_level > 0)
call unit_test('switch', uc  ==  switch(chars(36:1:-1)), switch(chars(36:1:-1)))
call unit_test_end('switch')
end subroutine test_switch
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_s2c()
integer :: i
   call unit_test_start('s2c','[ARRAY] convert character variable to array of characters with null terminator for C compatibility')
   call unit_test('s2c', all([(lc(i:i),i=1,len(lc)),char(0)].eq.s2c(lc)), 'compare s2c(lc) to expected letters')
   call unit_test_end('s2c',msg='')
end subroutine test_s2c
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_c2s()
integer :: i
   call unit_test_start('c2s','[ARRAY] convert C string pointer to Fortran character string')
   !call unit_test('c2s', c2s([(lc(i:i),i=1,len(lc)),char(0)]) .eq.lc,'compare s2c(lets) to expected letters')
   call unit_test_end('c2s',msg='')
end subroutine test_c2s
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_indent()
character(len=1024) :: in
   call unit_test_start('indent','[WHITESPACE] count number of leading spaces in a string')

   in='    should be four'
   call unit_test('indent',indent(in) == 4,msg=in)

   in='should be zero'
   call unit_test('indent',indent(in) == 0,msg=in)

   in='   should be three'
   call unit_test('indent',indent(in) == 3,msg=in)

   call unit_test_end('indent')
end subroutine test_indent
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_visible()
integer :: i
character(len=1) :: ch
character(len=2),parameter :: controls(0:31) =[ &
& '^@', '^A', '^B', '^C', '^D', '^E', '^F', '^G', '^H', '^I', &
& '^J', '^K', '^L', '^M', '^N', '^O', '^P', '^Q', '^R', '^S', &
& '^T', '^U', '^V', '^W', '^X', '^Y', '^Z', '^[', '^\', '^]', &
& '^^', '^_']
logical :: show
   show=merge(T,F,unit_test_level>0)
   call unit_test_start('visible','[NONALPHA] expand a string to control and meta-control representations')
   do i=0,127
      ch=char(i)
      select case(i)
      case(32:126); call unit_test('visible',visible(ch) == ch,i,    'testing',visible(ch),wordy=show)
      case(0:31);   call unit_test('visible',visible(ch) == controls(i),i,'testing',visible(ch),wordy=show)
      case(127);    call unit_test('visible',visible(char(127)) == '^?',127,   'testing',visible(ch),wordy=show)
      end select
   enddo
   call unit_test_end('visible')
   ! add //'' to change function call to expression to avoid gfortran-11 bug
end subroutine test_visible
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_expand()

integer :: i
character(len=80) :: line
   call unit_test_start('expand','[NONALPHA] expand C-like escape sequences')
   call unit_test('expand',expand('\e\d0912J') == char(27)//'[2J','a vt102 sequence to clear the screen')
   call unit_test('expand',expand('this is a test') == 'this is a test',msg='test plain text')

   !check all ASCII values
   do i=0,127
       write(line,'("%d",i3.3)')i
       call unit_test('expand',expand(line,'%') == char(i),'check decimal value',line,wordy=F)
       write(line,'("%o",o3.3)')i
       call unit_test('expand',expand(line,'%') == char(i),'check octal value',line,wordy=F)
       write(line,'("%x",z2.2)')i
       call unit_test('expand',expand(line,'%') == char(i),'check hexadecimal value',line,wordy=F)
   enddo

   call unit_test('expand',expand('%d008%d027%d013%d011%d007%d009','%') ==  &
           char(8)//char(27)//char(13)//char(11)//char(7)//char(9),msg='test decimal escape characters')
   call unit_test('expand',expand('%b%e%r%v%a%t','%') ==  &
           char(8)//char(27)//char(13)//char(11)//char(7)//char(9),msg='test escape characters')
   call unit_test_end('expand','checked BOZ values plus tests shown')

end subroutine test_expand
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_notabs()
character(len=:),allocatable :: inline
character(len=:),allocatable :: expected
character(len=1024)          :: outline
integer                      :: iout
   call unit_test_start('notabs','[NONALPHA] convert tabs to spaces while maintaining columns, assuming tab is set every 8 chars' )
   inline= 'one '//char(9)//'and'//repeat(char(9),3)//'two'
   expected='one     and                     two'
   call notabs(inline,outline,iout)
   if(unit_test_level /= 0)then
      write(std_err,g)'*test_notabs*',inline
      write(std_err,g)'*test_notabs*',outline
      write(std_err,g)'*test_notabs*',len_trim(outline),iout
   endif
   call unit_test('notabs',outline == expected.and.iout == 35,msg='expand a line with tabs in it')
   call unit_test_end('notabs',msg='')
end subroutine test_notabs
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_adjustc
character(len=80),allocatable :: expected(:)
character(len=80),allocatable :: left(:)
character(len=80),allocatable :: input(:)
integer                       :: i
   call unit_test_start('adjustc','elemental function centers string within the length of the input string')
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
   if(unit_test_level > 0)write(std_err,'(a)')left

   ! now center the left-justified copy
   do i=1,size(left)
      input(i)=adjustc(left(i))
   enddo
   ! check against expected output
   call unit_test('adjustc',all(expected == input),msg='text centering')

   ! indent lines different amounts
   do i=1,size(left)
      input(i)=repeat(' ',i-1)//left(i)
   enddo
   if(unit_test_level > 0)write(std_err,'(a)')input

   ! recenter it again
   do i=1,size(left)
      input(i)=adjustc(left(i))
   enddo
   if(unit_test_level > 0)write(std_err,'(a)')input
   call unit_test('adjustc',all(expected == input),msg='text centering')

   call unit_test_end('adjustc')
end subroutine test_adjustc
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_nospace
   character(len=:),allocatable :: stringin
   character(len=:),allocatable :: stringout
   call unit_test_start('nospace','[WHITESPACE] remove all whitespace from input string')
   stringin='  This     is      a     test  '
   stringout=nospace(stringin)
   call unit_test('nospace', stringout  ==  'Thisisatest','input=',stringin,'output=',stringout )
   call unit_test('nospace', len(nospace('')) == 0,       'null input got length',len(nospace(''))+0) ! +0 to avoid gfortrah-11 bug
   call unit_test('nospace', nospace('ABCD') == 'ABCD',   'input with no spaces',nospace('ABCD')//'') ! // to avoid gfortran-11 bug
   call unit_test('nospace', len(nospace('    ')) == 0,   'input all spaces',nospace('    ')//'')
   call unit_test_end('nospace')
end subroutine test_nospace
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_stretch()
   call unit_test_start('stretch','[LENGTH] return string padded to at least specified length')
   call unit_test('stretch',stretch('Hello World',20)//'!' == 'Hello World         !',msg='check if padded')
   call unit_test('stretch',len(stretch('Hello World',20)) == 20,msg='check padded length')
   call unit_test('stretch',len(stretch('Hello World',2)) == 11 &
     & .and.stretch('Hello World',2) == 'Hello World', &
     & msg='check not truncated')
   call unit_test_end('stretch',msg='')
end subroutine test_stretch
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_pad()
   call unit_test_start('pad','[LENGTH] return string padded to at least specified length')
   call unit_test('pad',pad('Hello World',20)//'!' == 'Hello World         !',msg='check if padded')
   call unit_test('pad',len(pad('Hello World',20)) == 20,msg='check padded length')
   call unit_test('pad',len(pad('Hello World',2)) == 11 &
       & .and.pad('Hello World',2) == 'Hello World', &
       & msg='check not truncated')
   call unit_test_end('pad',msg='')
end subroutine test_pad
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_zpad()
   call unit_test_start('zpad','[LENGTH] pad a string on the left with zeros to specified length')
   call unit_test('zpad',zpad(4,4) == '0004',zpad(4,4),'vs','0004')
   call unit_test('zpad',zpad(4,4) == '0004',zpad(4,4),'vs','0004')
   call unit_test('zpad',zpad(' 123 ',4) == '0123',zpad(' 123 ',4),'vs','0123')
   call unit_test('zpad',all(zpad([1,12,123,1234]) == ['0001','0012','0123','1234']),'["0001","0012","0123","1234"]')
   call unit_test_end('zpad',msg='')
end subroutine test_zpad
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_lpad()
   call unit_test_start('lpad','[LENGTH] convert to a cropped string and then blank-pad on the left to requested length')
   call unit_test('lpad',lpad(4,4) == '   4',lpad(4,4),'vs','   4')
   call unit_test('lpad',lpad(4,4) == '   4',lpad(4,4),'vs','   4')
   call unit_test('lpad',lpad(' 123 ',4) == ' 123 ',lpad(' 123 ',4),'vs',' 123')
   call unit_test('lpad',all(lpad([1,12,123,1234]) == ['   1','  12',' 123','1234']),'["   1","  12"," 123","1234"]')
   call unit_test_end('lpad',msg='')
end subroutine test_lpad
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_cpad()
   call unit_test_start('cpad','[LENGTH] convert to a cropped string and then centers the string to specified length')
   call unit_test('cpad',cpad(4,3) == ' 4 ',cpad(4,3),'vs',' 4 ')
   call unit_test('cpad',cpad('123',8) == '  123   ',cpad('123',8),'vs','  123   ')
   call unit_test('cpad',all(cpad([1,12,123,1234]) == [' 1  ',' 12 ','123 ','1234']),'[" 1  "," 12 ","123 ","1234"]')
   call unit_test_end('cpad',msg='')
end subroutine test_cpad
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_rpad()
   call unit_test_start('rpad','[LENGTH] convert to a string and pad on the right to requested length')
   call unit_test('rpad',rpad(4,4) == '4   ',rpad(4,2),'vs','4   ')
   call unit_test('rpad',rpad(-4,4) == '-4   ',rpad(-4,2),'vs','-4  ')
   call unit_test('rpad','['//rpad(' 123 ',4)//']' == '['//'123 '//']','['//rpad(' 123 ',4)//']','vs [','123 '//']')
   call unit_test('rpad',all(rpad([1,12,123,1234]) == ['1   ','12  ','123 ','1234']),'["1   ","12  ","123 ","1234"]')
   call unit_test_end('rpad',msg='')
end subroutine test_rpad
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_lenset()
character(len=10)            :: string='abcdefghij'
   call unit_test_start('lenset','[LENGTH] return string trimmed or padded to specified length')
   call unit_test('lenset',len(lenset(string, 5)) == 5)
   call unit_test('lenset',len(lenset(string,20)) == 20)
   call unit_test('lenset',lenset(string,20) == 'abcdefghij')
   call unit_test('lenset',lenset(string, 5) == 'abcde')
   call unit_test_end('lenset')
end subroutine test_lenset
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_merge_str()
character(len=:), allocatable :: answer
   call unit_test_start('merge_str','[LENGTH] pads strings to same length and then calls MERGE(3f)')

   answer=merge_str('first string', 'second string is longer',10 == 10)
   if(unit_test_level > 0)then
      write(std_err,g)'['//answer//']',len(answer)
   endif
   call unit_test('merge_str',answer == 'first string'.and.len(answer) == 12,msg='check true value ')

   answer=merge_str('first string', 'second string is longer',10 /= 10)
   if(unit_test_level > 0)then
      write(std_err,g)'['//answer//']',len(answer)
   endif
   call unit_test('merge_str',answer == 'second string is longer'.and.len(answer) == 23,msg='check false value')

   call unit_test_end('merge_str')
end subroutine test_merge_str
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_compact
   call unit_test_start('compact','[WHITESPACE] converts contiguous whitespace to a single character (or nothing)')
   call unit_test('compact',compact('  This  is     a    test  ') == 'This is a test','reduce to single spaces')
   call unit_test('compact',compact('This is a test') == 'This is a test','input has no adjacent whitespace characters')
   call unit_test('compact',compact('This-is-a-test') == 'This-is-a-test','input has no whitespace characters')
   call unit_test('compact',compact('  This  is     a    test  ',char='') == 'Thisisatest','remove all whitespace')
   call unit_test('compact',compact('  This  is     a    test  ',char='t') == 'Thististattest','replace blank ranges with "t"')
   call unit_test_end('compact')
end subroutine test_compact
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_noesc  ! test noesc
character(len=23) :: in,out,clr
integer           :: i10
logical           :: goodbad
  ! Use goodbad(1) to indicate the test sequence was begun
   goodbad=.true.
   call unit_test_start('noesc','[NONALPHA] convert non-printable characters to a space')
   do i10=0,127
      write(in, '(i3.3,1x,4a)')i10,char(i10),char(i10),char(i10),' eol'
      write(clr,'(i3.3,1x,"    eol")')i10
      out=noesc(in)
      if(unit_test_level > 0)then
         write(std_err,'(a)')trim(in)
         write(std_err,'(a)')trim(out)
      endif
      SELECT CASE (i10)
      CASE (:31,127)
        if(out /= clr)then
           goodbad=.false.
           call unit_test_msg('noesc','Error: noesc did not replace a string with blanks that it should have')
        endif
      CASE DEFAULT
        if(in /= out)then
           call unit_test_msg('noesc','Error: noesc changed a string it should not have')
           goodbad=.false.
        endif
      END SELECT
   enddo
   call unit_test('noesc',goodbad)
end subroutine test_noesc
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_string_to_value
CHARACTER(len=80) :: STRING
real              :: RVALUE
doubleprecision   :: DVALUE
doubleprecision   :: SUM, SUM2, DELTA
integer           :: IVALUE
integer           :: GOOD
integer           :: ierr
!===================================================================================================================================
   call unit_test_start('string_to_value','[TYPE] subroutine returns numeric value of specified type from string')
!===================================================================================================================================
   STRING=' -40.5e-2 '
   CALL string_to_value(STRING,RVALUE,IERR)
   CALL string_to_value(STRING,DVALUE,IERR)
   CALL string_to_value(STRING,IVALUE,IERR)
   if(unit_test_level > 0)then
      write(std_err,g) 'string_to_value: real value is ',-40.5e-2
      write(std_err,g) 'string_to_value: double value is ',-40.5d-2
      write(std_err,g) 'string_to_value: real value of string ['//trim(STRING)//'] is ',RVALUE
      write(std_err,g) 'string_to_value: double value of string ['//trim(STRING)//'] is ',DVALUE
      write(std_err,g) 'string_to_value: integer value of string ['//trim(STRING)//'] is ',IVALUE
   endif
   STRING=' -40.5d-2 '
   if(unit_test_level > 0)then
      CALL string_to_value(STRING,RVALUE,IERR)
      write(std_err,g) 'string_to_value: real value of string ['//trim(STRING)//'] is ',RVALUE
      CALL string_to_value(STRING,DVALUE,IERR)
      write(std_err,g) 'string_to_value: double value of string ['//trim(STRING)//'] is ',DVALUE
       CALL string_to_value(STRING,IVALUE,IERR)
      write(std_err,g) 'string_to_value: integer value of string ['//trim(STRING)//'] is ',IVALUE
   endif
   good=0
   call unit_test('string_to_value',rvalue == -40.5e-2)
      good=good*10+1
   call unit_test('string_to_value',dvalue == -40.5d-2)
      good=good*10+1
   call unit_test('string_to_value',dvalue-spacing(dvalue) <= -40.5d-2.and.dvalue+spacing(dvalue) >= -40.5d-2)
      good=good*10+1
   call unit_test('string_to_value',rvalue-spacing(rvalue) <= -40.5e-2.and.rvalue+spacing(rvalue) >= -40.5e-2)
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
   if(unit_test_level > 0)then
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
   call unit_test('string_to_value',sum == sum2,'with no tolerance expected',sum2,'got',sum)
   call unit_test('string_to_value', sum+delta >= sum2.and.sum-delta <= sum2 ,'expected',sum2,'got',sum)
!===================================================================================================================================
   call unit_test_end('string_to_value')
!===================================================================================================================================
end subroutine test_string_to_value
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_s2v()
doubleprecision SUM, SUM2, DELTA
   SUM2=5.555555555555555555555555555555555d0+5.555555555555555555555555555555555e0+INT(5.555555555555555555555555555555555)
   DELTA=spacing(0.0d0)+spacing(0.0)

   call unit_test_start('s2v','[TYPE] function returns doubleprecision value from string')

   SUM=s2v('5.55555555555555555555555555e0')+REAL(s2v('5.55555555555555555555555555d0'))+INT(s2v('5.55555555555555555555555555'))
   if(unit_test_level > 0)then
      write(std_err,g) 's2v: SUM2=', SUM2
      write(std_err,g) 's2v: SUM=', SUM
      write(std_err,g) 's2v: DELTA=', DELTA
   endif
   call unit_test('s2v',sum+delta >= sum2.and.sum-delta <= sum2, 'SUM=',sum,'SUM2=',sum2,'DELTA=',delta)
   call unit_test_end('s2v')
end subroutine test_s2v
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_value_to_string
CHARACTER(LEN=80) :: STRING
doubleprecision   :: DVALUE
real              :: RVALUE
integer           :: IVALUE
integer           :: ILEN
integer           :: IERR
integer           :: IERRSUM=0
!===================================================================================================================================
   call unit_test_start('value_to_string','[TYPE] return numeric string from a numeric value')
   DVALUE=5.5555555555555555555555d0
   call value_to_string(DVALUE,STRING,ILEN,IERR)
   if(unit_test_level > 0)then
      write(std_err,g)'value_to_string: DOUBLE TEST VALUE=',dvalue,'STRING=',trim(string),' ILEN=',ilen,'IERR=',ierr
   endif
   IERRSUM=IERRSUM+IERR
   if(ILEN <= 0)IERRSUM=IERRSUM+1000

   RVALUE=3.3333333333333333333333
   call value_to_string(RVALUE,STRING,ILEN,IERR)
   if(unit_test_level > 0)then
      write(std_err,g)'value_to_string: REAL TEST VALUE=',rvalue,'STRING=',trim(string),' ILEN=',ilen,'IERR=',ierr
   endif
   IERRSUM=IERRSUM+IERR
   if(ILEN <= 0)IERRSUM=IERRSUM+10000

   IVALUE=1234567890
   call value_to_string(IVALUE,STRING,ILEN,IERR)
   if(unit_test_level > 0)then
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
   if(unit_test_level > 0)then
      write(std_err,g)'value_to_string: INTEGER TEST VALUE=',ivalue,'STRING=',trim(string),' ILEN=',ilen,'IERR=',ierr
   endif

   IVALUE=-12345
   call value_to_string(IVALUE,STRING,ILEN,IERR)
   if(unit_test_level > 0)then
      write(std_err,g)'value_to_string: INTEGER TEST VALUE=',ivalue,'STRING=',trim(string),' ILEN=',ilen,'IERR=',ierr
   endif
   if(string /= '-12345')then
       IERRSUM=IERRSUM+1000000
   endif
   if(ILEN /= 6)then
       IERRSUM=IERRSUM+10000000
   endif
!===================================================================================================================================
   call unit_test('value_to_string',ierrsum == 0,msg='value_to_string'//v2s(ierrsum))
   call unit_test_end('value_to_string')
!===================================================================================================================================
end subroutine test_value_to_string
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_v2s()
!use M_math, only : almost
real            :: SUM
doubleprecision :: SUM2
   call unit_test_start('v2s','[TYPE] return numeric string from a numeric value')

   SUM2=5.555555555555555555555555555555555d0
   SUM=5.555555555555555555555555555555555e0
!   call unit_test('v2s',almost(REAL(s2v(v2s(SUM))),SUM,7),'real',SUM,REAL(s2v(v2s(SUM))))
!   call unit_test('v2s',almost(s2v(v2s(SUM2)),SUM2,15),'doubleprecision',SUM2,s2v(v2s(SUM)))
   call unit_test('v2s',v2s(1234) == '1234','integer',1234)
   call unit_test('v2s',v2s(.true.) == 'T','logical',v2s(.true.))
   call unit_test('v2s',v2s(.false.) == 'F','logical',v2s(.false.)  )
   call unit_test_end('v2s')
end subroutine test_v2s
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_isnumber
   call unit_test_start('isnumber','[TYPE] determine if a string represents a number')
   call unit_test('isnumber',isnumber(' 123 ')                                            ==  1,  'integer string')
   call unit_test('isnumber',isnumber(' -123. ')                                          ==  2,  'whole number string')
   call unit_test('isnumber',isnumber(' -123.0')                                          ==  3,  'real string')
   call unit_test('isnumber',isnumber(' -100.50')                                         ==  3,  'real string')
   call unit_test('isnumber',all( [isnumber('4.4e0 '),isnumber('1e1'),isnumber('-3D-4')]  ==  4), 'exponent string')
   call unit_test('isnumber',isnumber(' Not a number')                                    <  0,  'non-numeric string')
   call unit_test_end('isnumber')
end subroutine test_isnumber
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_trimzeros_()
character(len=:),allocatable  :: str1, str2
character(len=1),allocatable  :: char1(:), char2(:)
   call unit_test_start('trimzeros_')
   call unit_test_end('trimzeros_','currently internal')
end subroutine test_trimzeros_
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_listout()
integer,allocatable :: icurve_lists(:) ! icurve_lists is input array
integer :: icurve_expanded(1000)       ! icurve_expanded is output array
integer :: inums                       ! number of icurve_lists values on input, number of icurve_expanded numbers on output
integer :: i
integer :: ierr
   call unit_test_start('listout','[NUMERIC] expand list of numbers where negative numbers denote range ends (1 -10 => 1 thru 10)')
   icurve_lists=[1, 20, -30, 101, 100, 99, 100, -120, 222, -200]
   inums=size(icurve_lists)
   call listout(icurve_lists,icurve_expanded,inums,ierr)
   call unit_test('listout',ierr == 0,msg='check error status ierr='//v2s(ierr))
   call unit_test('listout',all(icurve_expanded(:inums) == [1,(i,i=20,30),101,100,99,(i,i=100,120),(i,i=222,200,-1)]),msg='expand')
   call unit_test_end('listout')
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

   call unit_test_start('quote','[QUOTES] add quotes to string as if written with list-directed input')

   do i=1,size(test_in)
      if(unit_test_level > 0)then
         write(std_err,'(a)')'ORIGINAL ['//test_in(i)//']'
         write(std_err,'(a)')'QUOTED   ['//quote(test_in(i))//']'
      endif
      call unit_test('quote',quote(test_in(i)) == test_out(i),quote(test_in(i)),'==>',test_out(i))
   enddo
   call unit_test_end('quote')
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

   call unit_test_start('unquote','[QUOTES] remove quotes from string as if read with list-directed input')
   do i=1,size(tests)
      quoted_str=tests(i)
      unquoted_str=unquote(trim(quoted_str),esc)                    ! the string processed by unquote(3f)
      read(quoted_str,*,iostat=ios,iomsg=msg)dummy                  ! read the string list-directed to compare the results
      if(unit_test_level > 0)then
         write(std_err,'(a)')'QUOTED        ['//trim(quoted_str)//']'     ! the original string
         write(std_err,'(a)')'UNQUOTED      ['//unquoted_str//']'
         if(ios /= 0)then
            write(std_err,g)trim(msg)
         else
            write(std_err,'(a)')'LIST DIRECTED ['//trim(dummy)//']'
         endif
      endif
      call unit_test('unquote',unquoted_str == dummy,msg=trim(dummy)//'==>'//unquoted_str)
   enddo
   call unit_test_end('unquote')
end subroutine test_unquote
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_describe
integer,parameter             :: number_of_chars=128
character(len=1)              :: char
integer                       :: i
character(len=*),parameter    :: at='@ at (at cost of, at sign, each at, commercial at, commat, &
&rollmop, monkey|pigs|elephant tail, snail, arroba, strudel, asperand, ampersat, rose, cabbage, swirl, whorl)'
!
   ! initialize database description of routine
   call unit_test_start('describe','[DESCRIBE] returns a string describing the name of a single character')
   ! call all descriptions to exercise procedure
   if(unit_test_level > 0)then
      do i=0,number_of_chars-1
         write(std_err,g)i,char(i),' ',describe(char(i))
      enddo
   endif

   ! unit tests
   call unit_test('describe', describe(char( 23) )  ==   'ctrl-W (ETB) end of transmission block' , 'describe ctrl-W')
   call unit_test('describe', &
      describe(char( 33) )  ==   '! exclamation point (screamer, gasper, slammer, startler, bang, shriek, pling)' , &
      'describe exclamation point')
   call unit_test('describe', describe(char( 52) )  ==   '4 four'                                 , 'describe four')
   call unit_test('describe', describe(char( 63) )  ==   '? question mark'                        , 'describe question mark')
   call unit_test('describe', describe(char( 64) )  ==   at                                       , 'describe at sign')
   call unit_test('describe', describe(char( 74) )  ==   'J majuscule J'                          , 'describe J')
   call unit_test('describe', describe(char( 117))  ==   'u miniscule u'                          , 'describe u')
   call unit_test('describe', describe(char( 126))  ==   '~ tilde'                                , 'describe tilde')
   call unit_test_end('describe')

end subroutine test_describe
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_getvals()
integer,parameter  :: longest_line=256
real               :: rvalues(longest_line/2+1)
integer            :: ivalues(longest_line/2+1)
doubleprecision    :: dvalues(longest_line/2+1)
integer            :: icount,ierr
character(len=:),allocatable :: given
   call unit_test_start('getvals','[TYPE] read REAL values from character variable up to size of VALUES() array')

   call getvals('11,,,22,33,-44, 55 , ,66  ',ivalues,icount,ierr)
   call unit_test('getvals',all(ivalues(:icount) == [11,22,33,-44,55,66]),msg='integer test')

   call getvals('1234.56 3.3333, 5.5555',rvalues,icount,ierr)
   call unit_test('getvals',all(rvalues(:icount) == [1234.56,3.3333,5.5555]),msg='real test')

   given='1234.56d0 3.3333d0, 5.5555d0'
   call getvals(given,dvalues,icount,ierr)
   call unit_test_msg('getvals','got',str(dvalues(:icount))//'','given',given,if=unit_test_level>0)
   call unit_test('getvals',all(dvalues(:icount) == [1234.56d0,3.3333d0,5.5555d0]),msg='double test')

   call unit_test_end('getvals')
end subroutine test_getvals
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_string_to_values()
character(len=80)  :: s=' 10 20e3;3.45 -400.3e-2;1234; 5678 '
integer,parameter  :: isz=10
real               :: array(isz)
integer            :: ierr
integer            :: inums
   call unit_test_start('string_to_values','[TYPE] read a string representing numbers into a numeric array')

   call string_to_values(s,10,array,inums,' ;',ierr)
   call unit_test('string_to_values',all(array(:inums) == [10.0,20e3,3.45,-400.3e-2,1234.0,5678.0]),s)
   call string_to_values('10;2.3;3.1416',isz,array,inums,' ;',ierr)
   call unit_test('string_to_values',all(array(:inums) == [10.0,2.3,3.1416]),array(1),array(2),array(3))
   call unit_test('string_to_values',inums == 3,'number of values is',inums)
   call unit_test_end('string_to_values',msg='')
end subroutine test_string_to_values
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_s2vs()
character(len=80)           :: s=' 10 20e3;3.45 -400.3e-2;1234; 5678 '
doubleprecision,allocatable :: values(:)
integer,allocatable         :: ivalues(:)
   call unit_test_start('s2vs','[TYPE] function returns a doubleprecision array of numbers from a string')
   values=s2vs(s)
   ivalues=int(s2vs(s))
   call unit_test('s2vs',size(values) == 6, msg='number of values')
   call unit_test('s2vs',all(ivalues == [10, 20000, 3, -4,1234,5678]))
   call unit_test_end('s2vs')
end subroutine test_s2vs
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_squeeze()
   call unit_test_start('squeeze','[EDITING] delete adjacent duplicate occurrences of a character from a string')
   call unit_test('squeeze',all( &
   & [ character(len=10) :: squeeze('abEeedeeee1','e'),squeeze('geek','e'),squeeze('a  b  c de    A',' ')] ==  &
   & [ character(len=10) :: 'abEede1','gek','a b c de A'] ) )
   call unit_test_end('squeeze')
end subroutine test_squeeze
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_edit_distance()
integer,allocatable         :: ivalues(:)
   call unit_test_start('edit_distance','[DESCRIBE] returns a naive edit distance using the Levenshtein distance algorithm')
   ivalues=[ edit_distance('kittens','sitting'),edit_distance('geek','gesek'),edit_distance('Saturday','Sunday')]
   call unit_test('edit_distance',all(ivalues == [3,1,3]))
   call unit_test_end('edit_distance')
end subroutine test_edit_distance
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_bundle()
   call unit_test_start('bundle','[ARRAY] return up to twenty strings of arbitrary length as an array')
   call unit_test('bundle',all(bundle('kittens','sit','three') == ["kittens","sit    ","three  "]), &
                                                                  &"'kittens','sit    ','three  '")
   call unit_test_end('bundle')
end subroutine test_bundle
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_isprint
integer,parameter             :: number_of_chars=256
character(len=1)              :: ch
integer                       :: i
   call unit_test_start('isprint','[COMPARE] returns .true. if character is an ASCII printable character')
   do i=0,number_of_chars-1
      ch=char(i)
      SELECT CASE (i)
      ! isgraph(ch).eqv.T instead of isgraph(ch) to avoid gfortran-11 bug
      CASE (32:126) ; call unit_test('isprint',isprint(ch),     'testing character',i,isprint(ch).eqv.T,wordy=F)
      CASE DEFAULT  ; call unit_test('isprint',.not.isprint(ch),'testing character',i,isprint(ch).eqv.T,wordy=F)
      END SELECT
   enddo
call unit_test_end('isprint')
end subroutine test_isprint
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_isgraph
integer,parameter             :: number_of_chars=256
character(len=1)              :: ch
integer                       :: i
   call unit_test_start('isgraph','[COMPARE] true if a printable character except a space is considered non-printable')
   do i=0,number_of_chars-1
      ch=char(i)
      SELECT CASE (i)
      ! isgraph(ch).eqv.T instead of isgraph(ch) to avoid gfortran-11 bug
      CASE (33:126) ; call unit_test('isgraph',isgraph(ch),     'testing character',i,isgraph(ch).eqv. T,wordy=F)
      CASE DEFAULT  ; call unit_test('isgraph',.not.isgraph(ch),'testing character',i,isgraph(ch).eqv. T,wordy=F)
      END SELECT
   enddo
   call unit_test_end('isgraph')
end subroutine test_isgraph
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_isalpha
integer,parameter             :: number_of_chars=128
character(len=1)              :: ch
integer                       :: i
   call unit_test_start('isalpha','[COMPARE] returns .true. if character is a letter and .false. otherwise')
   do i=0,number_of_chars-1
      ch=char(i)
      SELECT CASE (ch)
      CASE ('a':'z','A':'Z'); call unit_test('isalpha',isalpha(ch),'testing character',i,isalpha(ch),wordy=F)
      CASE DEFAULT;           call unit_test('isalpha',.not.isalpha(ch),'testing character',i,isalpha(ch),wordy=F)
      END SELECT
   enddo
   call unit_test_end('isalpha')
end subroutine test_isalpha
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_isxdigit
integer,parameter             :: number_of_chars=128
character(len=1)              :: ch
integer                       :: i
   call unit_test_start('isxdigit','[COMPARE] returns .true. if character is a hexadecimal digit (0-9, a-f, or A-F).')
   do i=0,number_of_chars-1
      ch=char(i)
      SELECT CASE (ch)
      CASE ('a':'f','A':'F','0':'9') ; call unit_test('isxdigit',isxdigit(ch),'testing character',i,isxdigit(ch),wordy=F)
      CASE DEFAULT ;                   call unit_test('isxdigit',.not.isxdigit(ch),'testing character',i,isxdigit(ch),wordy=F)
      END SELECT
   enddo
   call unit_test_end('isxdigit')
end subroutine test_isxdigit
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_isdigit
integer,parameter             :: number_of_chars=128
character(len=1)              :: ch
integer                       :: i
   call unit_test_start('isdigit','[COMPARE] returns .true. if character is a digit (0, 1, ..., 9) and .false. otherwise')
   do i=0,number_of_chars-1
      ch=char(i)
      SELECT CASE (i)
      CASE (48:57) ; call unit_test('isdigit',isdigit(ch),'testing character',i,isdigit(ch),wordy=F)
      CASE DEFAULT ; call unit_test('isdigit',.not.isdigit(ch),'testing character',i,isdigit(ch),wordy=F)
      END SELECT
   enddo
   call unit_test_end('isdigit')
end subroutine test_isdigit
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_isblank
integer,parameter             :: number_of_chars=128
character(len=1)              :: ch
integer                       :: i
   call unit_test_start('isblank','[COMPARE] returns .true. if character is a blank character (space or horizontal tab).')
   do i=0,number_of_chars-1
      ch=char(i)
      select case (i)
      case (9,32)  ; call unit_test('isblank',isblank(ch),     'testing character',i,isblank(ch),wordy=F)
      case default ; call unit_test('isblank',.not.isblank(ch),'testing character',i,isblank(ch),wordy=F)
      end select
   enddo
   call unit_test_end('isblank')
end subroutine test_isblank
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_isascii
integer,parameter             :: number_of_chars=128
character(len=1)              :: ch
integer                       :: i
   call unit_test_start('isascii','[COMPARE] returns .true. if the character is in the range char(0) to char(256)')
   do i=0,number_of_chars-1
      ch=char(i)
      SELECT CASE (i)
      CASE (0:127) ; call unit_test('isascii',isascii(ch),     'testing character',i,isascii(ch),wordy=F)
      CASE DEFAULT ; call unit_test('isascii',.not.isascii(ch),'testing character',i,isascii(ch),wordy=F)
      END SELECT
   enddo
   call unit_test_end('isascii')
end subroutine test_isascii
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_isspace
integer,parameter             :: number_of_chars=128
character(len=1)              :: ch
integer                       :: i
   call unit_test_start('isspace','[COMPARE] true if is a null,space,tab,carriage return, new line, vertical tab, or formfeed')
   do i=0,number_of_chars-1
      ch=char(i)
      SELECT CASE (i)
      CASE (0,9:13,32) ; call unit_test('isspace',isspace(ch),     'testing character',i,isspace(ch),wordy=F)
      CASE DEFAULT     ; call unit_test('isspace',.not.isspace(ch),'testing character',i,isspace(ch),wordy=F)
      END SELECT
   enddo
   call unit_test_end('isspace')
end subroutine test_isspace
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_iscntrl
integer,parameter             :: number_of_chars=128
character(len=1)              :: ch
integer                       :: i
   call unit_test_start('iscntrl','[COMPARE] returns .true. if character is a delete character or ordinary control character')
   do i=0,number_of_chars-1
      ch=char(i)
      SELECT CASE (i)
      CASE (0:31,127) ; call unit_test('iscntrl',iscntrl(ch),     'testing character',i,iscntrl(ch),wordy=F)
      CASE DEFAULT    ; call unit_test('iscntrl',.not.iscntrl(ch),'testing character',i,iscntrl(ch),wordy=F)
      END SELECT
   enddo
   call unit_test_end('iscntrl')
end subroutine test_iscntrl
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_ispunct
integer,parameter             :: number_of_chars=128
character(len=1)              :: ch
integer                       :: i
   call unit_test_start('ispunct','[COMPARE] returns .true. if character is a printable punctuation character')
   do i=0,number_of_chars-1
      ch=char(i)
      SELECT CASE (i)
      CASE (33:47, 58:64, 91:96, 123:126) ; call unit_test('ispunct',ispunct(ch),     'testing character',i,ispunct(ch),wordy=F)
      CASE DEFAULT ;                        call unit_test('ispunct',.not.ispunct(ch),'testing character',i,ispunct(ch),wordy=F)
      END SELECT
   enddo
   call unit_test_end('ispunct')
end subroutine test_ispunct
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_isupper
integer,parameter             :: number_of_chars=128
character(len=1)              :: ch
integer                       :: i
   call unit_test_start('isupper','[COMPARE] returns .true. if character is an uppercase letter (A-Z)')
   do i=0,number_of_chars-1
      ch=char(i)
      SELECT CASE (ch)
      CASE ('A':'Z') ; call unit_test('isupper',isupper(ch),     'testing character',i,isupper(ch),wordy=F)
      CASE DEFAULT ;   call unit_test('isupper',.not.isupper(ch),'testing character',i,isupper(ch),wordy=F)
      END SELECT
   enddo
   call unit_test_end('isupper')
end subroutine test_isupper
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_islower
integer,parameter             :: number_of_chars=128
character(len=1)              :: ch
integer                       :: i
   call unit_test_start('islower','[COMPARE] returns .true. if character is a miniscule letter (a-z)')
   do i=0,number_of_chars-1
      ch=char(i)
      SELECT CASE (ch)
      CASE ('a':'z'); call unit_test ('islower',islower(ch) ,    'testing character',i,islower(ch),wordy=F)
      CASE DEFAULT;   call unit_test ('islower',.not.islower(ch),'testing character',i,islower(ch),wordy=F)
      END SELECT
   enddo
   call unit_test_end('islower')
end subroutine test_islower
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_isalnum
integer,parameter             :: number_of_chars=128
character(len=1)              :: ch
integer                       :: i
   call unit_test_start('isalnum','[COMPARE] elemental function returns .true. if CHR is a letter or digit')
   do i=0,number_of_chars-1
      ch=char(i)
      SELECT CASE (ch)
      CASE ('a':'z','A':'Z','0':'9')
         call unit_test ('isalnum',isalnum(char(i)) .eqv. .true.,'testing character',i,isalnum(char(i)),wordy=F)
      CASE DEFAULT
         call unit_test ('isalnum',isalnum(char(i)) .eqv. .false., 'testing character',i,isalnum(char(i)),wordy=F)
      END SELECT
   enddo
   call unit_test_end('isalnum')
end subroutine test_isalnum
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_nint()
   call unit_test_start('nint','[TYPE] overloads NINT(3f) so it can handle character arguments')
   call unit_test('nint',nint('1234.4') == 1234,msg='test string to integer for overloaded NINT("1234.4")')
   call unit_test('nint',nint('1234.5') == 1235,msg='test string to integer for overloaded NINT("1234.5")')
   call unit_test('nint',nint('1234.6') == 1235,msg='test string to integer for overloaded NINT("1234.6")')
   call unit_test_end('nint',msg=' overload of NINT()')
end subroutine test_nint
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_int()
   call unit_test_start('int','[TYPE] overloads INT(3f) so it can handle character arguments')
   call unit_test('int',int('1234') == 1234,msg='test string to integer for overloaded INT()')
   call unit_test_end('int',msg=' overload of INT()')
end subroutine test_int
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_real()
   call unit_test_start('real','[TYPE] overloads REAL(3f) so it can handle character arguments')
   call unit_test('real', real('3.0d0') == 3.0d0,msg='test string to real for overloaded REAL()')
   call unit_test_end('real',msg='overload of REAL(3f)')
end subroutine test_real
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_dble()
  call unit_test_start('dble','[TYPE] overloads DBLE(3f) so it can handle character arguments')
   call unit_test('dble', dble('3.0d0') == 3.0d0,msg='test string to double for overloaded DBLE()')
   call unit_test_end('dble',msg='overload of DBLE(3f)')
end subroutine test_dble
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_setbits()
   !character(len=:),allocatable :: string
   call unit_test_start('setbits','set all bits in an INTEGER word with a string')
   !string='11111111'
   !call unit_test('setbits',setbits(string) == 0,setbits(string))
   !string='1111111111111111'
   !call unit_test('setbits',setbits(string) == 0,setbits(string))
   !string='11111111111111111111111111111111'
   !call unit_test('setbits',setbits(string) == 0,setbits(string))
   !string='1111111111111111111111111111111111111111111111111111111111111111'
   !call unit_test('setbits',setbits(string) == 0,setbits(string))
   call unit_test_end('setbits',msg='')
end subroutine test_setbits
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_fortran_name()
integer :: i
logical,parameter :: expect(*)= [F, T, T, F, T, T, F, F, F, F, F, F, T, F]
character(len=80),parameter :: names(*)=[character(len=80) ::  &
'_name',               &
'long_variable_name',  &
'name_',               &
'12L',                 &
'a__b__c',             &
'PropertyOfGas',       &
'3%3',                 &
'one two ',            &
'$NAME',               &
' ',                   &
'Variable-name',       &
'1234567890123456789012345678901234567890123456789012345678901234567890',       &
'A',                   &
'x@x'                  ]
   call unit_test_start('fortran_name','check if string is a valid Fortran variable name')
   call unit_test('fortran_name',all(fortran_name(names).eqv.expect),'elemental test')
   do i=1,size(names)
      call unit_test('fortran_name',fortran_name(names(i)).eqv.expect(i),names(i),'expected',expect(i))
   enddo
   call unit_test_end('fortran_name')
end subroutine test_fortran_name
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_ends_with()
call unit_test_start('ends_with', '[COMPARE] test if string ends with specified suffix(es)')
call unit_test('ends_with',.not.ends_with('prog.a',['.o','.i','.s']),'test prog.a with [.o,.i,.s]')
call unit_test('ends_with',ends_with('prog.f90',['.F90','.f90','.f  ','.F  ']),'test prog.f90 with .F90, .f90, .f, .F')
call unit_test('ends_with',ends_with('prog.pdf','.pdf'),'test prog.pdf with .pdf')
call unit_test('ends_with',.not.ends_with('prog.doc','.txt'),'test prog.doc with .txt')
call unit_test_end('ends_with')
end subroutine test_ends_with
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_aton()
   call unit_test_start('aton','[TYPE] function returns argument as a numeric value from a string')
   call unit_test_end('aton')
end subroutine test_aton
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_dilate()
character(len=:),allocatable :: in
character(len=:),allocatable :: expected
integer                      :: i
   call unit_test_start('dilate','[NONALPHA] expand tab characters')
        in='  this is my string  '
        ! change spaces to tabs to make a sample input
        do i=1,len(in)
           if(in(i:i) == ' ')in(i:i)=char(9)
        enddo
        expected="                this    is      my      string"
        call unit_test('dilate',dilate(in).eq.expected,'expected',expected,'got',dilate(in))
   call unit_test_end('dilate')
end subroutine test_dilate
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_find_field()
   call unit_test_start('find_field','[TOKENS] parse a string into tokens')
   call unit_test_end('find_field')
end subroutine test_find_field
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_longest_common_substring()
   call unit_test_start('longest_common_substring','[COMPARE] function that returns the longest common substring of two strings')
   call compare('testing123testingthing','thisis',             'thi')
   call compare('testing',             'sting',              'sting')
   call compare('thisisatest_stinger','testing123testingthing','sting')
   call compare('thisisatest_stinger', 'thisis',            'thisis')
   call compare('thisisatest',         'testing123testing',   'test')
   call compare('thisisatest',      'thisisatest',     'thisisatest')
   call unit_test_end('longest_common_substring')
   contains

subroutine compare(a,b,answer)
character(len=*),intent(in) :: a, b, answer
character(len=:),allocatable :: match
   match=longest_common_substring(a,b)
   call unit_test('longest_common_substring',answer == match, &
   & 'comparing',quote(a),'and',quote(b),'got',quote(match),'expected',quote(answer) )
end subroutine compare

end subroutine test_longest_common_substring
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_matching_delimiter()
use M_strings, only : matching_delimiter
character(len=128)  :: str
integer             :: imatch
   call unit_test_start('matching_delimiter','[QUOTES] find position of matching delimiter')
!  Allowable delimiters are (), [], {}, <>.
   str=' a [[[[b] and ] then ] finally ]'
   call unit_test('matching_delimiter',getval(1)  ==  0 ,'test get zero for invalid delimiter')
   call unit_test('matching_delimiter',getval(4)  == 32 ,'expect 32, got',getval(4)+0) ! +0 for gfortran-11 bug
   call unit_test('matching_delimiter',getval(5)  == 22 ,'expect 22, got',getval(5)+0)
   call unit_test('matching_delimiter',getval(6)  == 15 ,'expect 15, got',getval(6)+0)
   call unit_test('matching_delimiter',getval(7)  ==  9 ,'expect 9, got',getval(7)+0)
   call unit_test('matching_delimiter',getval(32) ==  4 ,'expect 4, got',getval(32)+0)
   str=' a (<[{b} and ] then > finally )'
   call unit_test('matching_delimiter',getval(4)  == 32 ,'expect 32, got',getval(4)+0) ! +0 for gfortran-11 bug
   call unit_test('matching_delimiter',getval(5)  == 22 ,'expect 22, got',getval(5)+0)
   call unit_test('matching_delimiter',getval(6)  == 15 ,'expect 15, got',getval(6)+0)
   call unit_test('matching_delimiter',getval(7)  ==  9 ,'expect 9, got',getval(7)+0)
   call unit_test('matching_delimiter',getval(32) ==  4 ,'expect 4, got',getval(32)+0)
   call unit_test_end('matching_delimiter')
contains
function getval(ipos)
integer,intent(in) :: ipos
integer :: getval
   call matching_delimiter(str,ipos,getval)
end function 
end subroutine test_matching_delimiter
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_msg()
   call unit_test_start('msg','[TYPE] converts any standard scalar type to a string')
   call unit_test_end('msg')
end subroutine test_msg
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_paragraph()
   call unit_test_start('paragraph','[TOKENS] break a long line into a paragraph')
   call unit_test_end('paragraph')
end subroutine test_paragraph
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_sep()
   call unit_test_start('sep','[TOKENS] function to parse string into an array using specified delimiters')
   call unit_test_end('sep')
end subroutine test_sep
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_split2020()
   call unit_test_start('split2020','[TOKENS] parse a string into tokens using proposed f2023 method')
   call unit_test_end('split2020')
end subroutine test_split2020
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_upper_quoted()
character(len=:),allocatable  :: s
   call unit_test_start('upper_quoted','[CASE] convert string to uppercase skipping strings quoted per Fortran syntax rules')
   s=' ABCDEFG abcdefg "Double-Quoted" ''Single-Quoted'' "with "" Quote" everything else'
   call unit_test('upper_quoted',&
   & upper_quoted(s).eq.' ABCDEFG ABCDEFG "Double-Quoted" ''Single-Quoted'' "with "" Quote" EVERYTHING ELSE',&
   &'complex string')
   if(unit_test_level.gt.0)then
      call unit_test_msg('upper_quoted','input is  ['//s//']')
      call unit_test_msg('upper_quoted','result is ['//upper_quoted(s)//']')
   endif
   call unit_test('upper_quoted',all(upper_quoted(["a'b'c",'d"e"f',"ghijk"]).eq.["A'b'C",'D"e"F',"GHIJK"]),'elemental test')
   call unit_test_end('upper_quoted')
end subroutine test_upper_quoted
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
end module M_testsuite_M_strings
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
program runtest
use M_framework
use M_framework__verify, only : unit_test_level, unit_test_stop
use M_testsuite_M_strings
implicit none
!  unit_test_level=1
   unit_test_level=0
   call test_suite_M_strings()

   ! untested
   call unit_test_stop()
end program runtest
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
