     program demo_glob
     implicit none
     ! This main() routine passes a bunch of test strings
     ! into the above code.  In performance comparison mode,
     ! it does that over and over. Otherwise, it does it just
     ! once. Either way, it outputs a passed/failed result.
     !
     integer :: nReps
     logical :: allpassed
     integer :: i
      allpassed = .true.

      nReps = 10000
      ! Can choose as many repetitions as you're expecting
      ! in the real world.
      nReps = 1

      do i=1,nReps
       ! Cases with repeating character sequences.
       allpassed= test("a*abab", "a*b", .true.) .and. allpassed
       allpassed= test("ab", "*?", .true.) .and. allpassed
       allpassed= test("abc", "*?", .true.) .and. allpassed
       allpassed= test("abcccd", "*ccd", .true.) .and. allpassed
       allpassed= test("bLah", "bLaH", .false.) .and. allpassed
       allpassed= test("mississippi", "*sip*", .true.) .and. allpassed
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
       allpassed= test("xyxyxyzyxyz", "xy*z*xyz", .true.) .and. allpassed
       allpassed= test("xyxyxyxyz", "xy*xyz", .true.) .and. allpassed
       allpassed= test("mississippi", "mi*sip*", .true.) .and. allpassed
       allpassed= test("ababac", "*abac*", .true.) .and. allpassed
       allpassed= test("aaazz", "a*zz*", .true.) .and. allpassed
       allpassed= test("a12b12", "*12*23", .false.) .and. allpassed
       allpassed= test("a12b12", "a12b", .false.) .and. allpassed
       allpassed= test("a12b12", "*12*12*", .true.) .and. allpassed

       ! Additional cases where the '*' char appears in the tame string.
       allpassed= test("*", "*", .true.) .and. allpassed
       allpassed= test("a*r", "a*", .true.) .and. allpassed
       allpassed= test("a*ar", "a*aar", .false.) .and. allpassed

       ! More double wildcard scenarios.
       allpassed= test("XYXYXYZYXYz", "XY*Z*XYz", .true.) .and. allpassed
       allpassed= test("missisSIPpi", "*SIP*", .true.) .and. allpassed
       allpassed= test("mississipPI", "*issip*PI", .true.) .and. allpassed
       allpassed= test("xyxyxyxyz", "xy*xyz", .true.) .and. allpassed
       allpassed= test("miSsissippi", "mi*sip*", .true.) .and. allpassed
       allpassed= test("miSsissippi", "mi*Sip*", .false.) .and. allpassed
       allpassed= test("abAbac", "*Abac*", .true.) .and. allpassed
       allpassed= test("aAazz", "a*zz*", .true.) .and. allpassed
       allpassed= test("A12b12", "*12*23", .false.) .and. allpassed
       allpassed= test("a12B12", "*12*12*", .true.) .and. allpassed
       allpassed= test("oWn", "*oWn*", .true.) .and. allpassed

       ! Completely tame (no wildcards) cases.
       allpassed= test("bLah", "bLah", .true.) .and. allpassed

       ! Simple mixed wildcard tests suggested by IBMer Marlin Deckert.
       allpassed= test("a", "*?", .true.) .and. allpassed

       ! More mixed wildcard tests including coverage for false positives.
       allpassed= test("a", "??", .false.) .and. allpassed
       allpassed= test("ab", "?*?", .true.) .and. allpassed
       allpassed= test("ab", "*?*?*", .true.) .and. allpassed
       allpassed= test("abc", "?**?*?", .true.) .and. allpassed
       allpassed= test("abc", "?**?*&?", .false.) .and. allpassed
       allpassed= test("abcd", "?b*??", .true.) .and. allpassed
       allpassed= test("abcd", "?a*??", .false.) .and. allpassed
       allpassed= test("abcd", "?**?c?", .true.) .and. allpassed
       allpassed= test("abcd", "?**?d?", .false.) .and. allpassed
       allpassed= test("abcde", "?*b*?*d*?", .true.) .and. allpassed

       ! Single-character-match cases.
       allpassed= test("bLah", "bL?h", .true.) .and. allpassed
       allpassed= test("bLaaa", "bLa?", .false.) .and. allpassed
       allpassed= test("bLah", "bLa?", .true.) .and. allpassed
       allpassed= test("bLaH", "?Lah", .false.) .and. allpassed
       allpassed= test("bLaH", "?LaH", .true.) .and. allpassed

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
       &test("********a********b********c********", "abc", .false.) .and. allpassed
       allpassed= &
       &test("abc", "********a********b********b********", .false.) .and. allpassed
       allpassed= test("*abc*", "***a*b*c***", .true.) .and. allpassed

       ! A case-insensitive algorithm test.
       ! allpassed=test("mississippi", "*issip*PI", .true.) .and. allpassed
      enddo

      if (allpassed)then
         write(*,'(a)')"Passed",nReps
      else
         write(*,'(a)')"Failed"
      endif
     contains
     ! This is a test program for wildcard matching routines.
     ! It can be used either to test a single routine for correctness,
     ! or to compare the timings of two (or more) different wildcard
     ! matching routines.
     !
     function test(tame, wild, bExpectedResult) result(bPassed)
     use M_strings, only : glob
        character(len=*) :: tame
        character(len=*) :: wild
        logical          :: bExpectedResult
        logical          :: bResult
        logical          :: bPassed
        bResult = .true.    ! We'll do "&=" cumulative checking.
        bPassed = .false.   ! Assume the worst.
        write(*,*)repeat('=',79)
        bResult = glob(tame, wild) ! Call a wildcard matching routine.

        ! To assist correctness checking, output the two strings in any
        ! failing scenarios.
        if (bExpectedResult .eqv. bResult) then
           bPassed = .true.
           if(nReps == 1) write(*,*)"Passed match on ",tame," vs. ", wild
        else
           if(nReps == 1) write(*,*)"Failed match on ",tame," vs. ", wild
        endif

     end function test
     end program demo_glob
