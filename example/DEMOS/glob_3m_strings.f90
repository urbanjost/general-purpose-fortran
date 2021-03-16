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
            allpassed=allpassed .and. test("a*abab", "a*b", .true.)
            !!cycle
            allpassed=allpassed .and. test("ab", "*?", .true.)
            allpassed=allpassed .and. test("abc", "*?", .true.)
            allpassed=allpassed .and. test("abcccd", "*ccd", .true.)
            allpassed=allpassed .and. test("bLah", "bLaH", .false.)
            allpassed=allpassed .and. test("mississippi", "*sip*", .true.)
            allpassed=allpassed .and. &
             & test("xxxx*zzzzzzzzy*f", "xxx*zzy*f", .true.)
            allpassed=allpassed .and. &
             & test("xxxx*zzzzzzzzy*f", "xxxx*zzy*fffff", .false.)
            allpassed=allpassed .and. &
             & test("mississipissippi", "*issip*ss*", .true.)
            allpassed=allpassed .and. &
             & test("xxxxzzzzzzzzyf", "xxxx*zzy*fffff", .false.)
            allpassed=allpassed .and. &
             & test("xxxxzzzzzzzzyf", "xxxx*zzy*f", .true.)
            allpassed=allpassed .and. test("xyxyxyzyxyz", "xy*z*xyz", .true.)
            allpassed=allpassed .and. test("xyxyxyxyz", "xy*xyz", .true.)
            allpassed=allpassed .and. test("mississippi", "mi*sip*", .true.)
            allpassed=allpassed .and. test("ababac", "*abac*", .true.)
            allpassed=allpassed .and. test("aaazz", "a*zz*", .true.)
            allpassed=allpassed .and. test("a12b12", "*12*23", .false.)
            allpassed=allpassed .and. test("a12b12", "a12b", .false.)
            allpassed=allpassed .and. test("a12b12", "*12*12*", .true.)

            ! Additional cases where the '*' char appears in the tame string.
            allpassed=allpassed .and. test("*", "*", .true.)
            allpassed=allpassed .and. test("a*r", "a*", .true.)
            allpassed=allpassed .and. test("a*ar", "a*aar", .false.)

            ! More double wildcard scenarios.
            allpassed=allpassed .and. test("XYXYXYZYXYz", "XY*Z*XYz", .true.)
            allpassed=allpassed .and. test("missisSIPpi", "*SIP*", .true.)
            allpassed=allpassed .and. test("mississipPI", "*issip*PI", .true.)
            allpassed=allpassed .and. test("xyxyxyxyz", "xy*xyz", .true.)
            allpassed=allpassed .and. test("miSsissippi", "mi*sip*", .true.)
            allpassed=allpassed .and. test("miSsissippi", "mi*Sip*", .false.)
            allpassed=allpassed .and. test("abAbac", "*Abac*", .true.)
            allpassed=allpassed .and. test("aAazz", "a*zz*", .true.)
            allpassed=allpassed .and. test("A12b12", "*12*23", .false.)
            allpassed=allpassed .and. test("a12B12", "*12*12*", .true.)
            allpassed=allpassed .and. test("oWn", "*oWn*", .true.)

            ! Completely tame (no wildcards) cases.
            allpassed=allpassed .and. test("bLah", "bLah", .true.)

            ! Simple mixed wildcard tests suggested by IBMer Marlin Deckert.
            allpassed=allpassed .and. test("a", "*?", .true.)

            ! More mixed wildcard tests including coverage for false positives.
            allpassed=allpassed .and. test("a", "??", .false.)
            allpassed=allpassed .and. test("ab", "?*?", .true.)
            allpassed=allpassed .and. test("ab", "*?*?*", .true.)
            allpassed=allpassed .and. test("abc", "?**?*?", .true.)
            allpassed=allpassed .and. test("abc", "?**?*&?", .false.)
            allpassed=allpassed .and. test("abcd", "?b*??", .true.)
            allpassed=allpassed .and. test("abcd", "?a*??", .false.)
            allpassed=allpassed .and. test("abcd", "?**?c?", .true.)
            allpassed=allpassed .and. test("abcd", "?**?d?", .false.)
            allpassed=allpassed .and. test("abcde", "?*b*?*d*?", .true.)

            ! Single-character-match cases.
            allpassed=allpassed .and. test("bLah", "bL?h", .true.)
            allpassed=allpassed .and. test("bLaaa", "bLa?", .false.)
            allpassed=allpassed .and. test("bLah", "bLa?", .true.)
            allpassed=allpassed .and. test("bLaH", "?Lah", .false.)
            allpassed=allpassed .and. test("bLaH", "?LaH", .true.)

            ! Many-wildcard scenarios.
            allpassed=allpassed .and. test(&
            &"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa&
            &aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaab",&
            &"a*a*a*a*a*a*aa*aaa*a*a*b",&
            &.true.)
            allpassed=allpassed .and. test(&
            &"abababababababababababababababababababaacacacacacacac&
            &adaeafagahaiajakalaaaaaaaaaaaaaaaaaffafagaagggagaaaaaaaab",&
            &"*a*b*ba*ca*a*aa*aaa*fa*ga*b*",&
            &.true.)
            allpassed=allpassed .and. test(&
            &"abababababababababababababababababababaacacacacacaca&
            &cadaeafagahaiajakalaaaaaaaaaaaaaaaaaffafagaagggagaaaaaaaab",&
            &"*a*b*ba*ca*a*x*aaa*fa*ga*b*",&
            &.false.)
            allpassed=allpassed .and. test(&
            &"abababababababababababababababababababaacacacacacacacad&
            &aeafagahaiajakalaaaaaaaaaaaaaaaaaffafagaagggagaaaaaaaab",&
            &"*a*b*ba*ca*aaaa*fa*ga*gggg*b*",&
            &.false.)
            allpassed=allpassed .and. test(&
            &"abababababababababababababababababababaacacacacacacacad&
            &aeafagahaiajakalaaaaaaaaaaaaaaaaaffafagaagggagaaaaaaaab",&
            &"*a*b*ba*ca*aaaa*fa*ga*ggg*b*",&
            &.true.)
            allpassed=allpassed .and. test("aaabbaabbaab", "*aabbaa*a*", .true.)
            allpassed=allpassed .and. &
            test("a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*",&
            &"a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*", .true.)
            allpassed=allpassed .and. test("aaaaaaaaaaaaaaaaa",&
            &"*a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*", .true.)
            allpassed=allpassed .and. test("aaaaaaaaaaaaaaaa",&
            &"*a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*a*", .false.)
            allpassed=allpassed .and. test(&
            &"abc*abcd*abcde*abcdef*abcdefg*abcdefgh*abcdefghi*abcdefghij&
            &*abcdefghijk*abcdefghijkl*abcdefghijklm*abcdefghijklmn",&
            & "abc*abc*abc*abc*abc*abc*abc*abc*abc*abc*abc*abc*abc*abc&
            &*abc*abc*abc*",&
            &.false.)
            allpassed=allpassed .and. test(&
            &"abc*abcd*abcde*abcdef*abcdefg*abcdefgh*abcdefghi*abcdefghij&
            &*abcdefghijk*abcdefghijkl*abcdefghijklm*abcdefghijklmn",&
            &"abc*abc*abc*abc*abc*abc*abc*abc*abc*abc*abc*abc*",&
            &.true.)
            allpassed=allpassed .and. test("abc*abcd*abcd*abc*abcd",&
            &"abc*abc*abc*abc*abc", .false.)
            allpassed=allpassed .and. test( "abc*abcd*abcd*abc*abcd*abcd&
            &*abc*abcd*abc*abc*abcd", &
            &"abc*abc*abc*abc*abc*abc*abc*abc*abc*abc*abcd",&
            &.true.)
            allpassed=allpassed .and. test("abc",&
            &"********a********b********c********", .true.)
            allpassed=allpassed .and.&
            &test("********a********b********c********", "abc", .false.)
            allpassed=allpassed .and. &
            &test("abc", "********a********b********b********", .false.)
            allpassed=allpassed .and. test("*abc*", "***a*b*c***", .true.)

            ! A case-insensitive algorithm test.
            ! allpassed=allpassed .and. test("mississippi", "*issip*PI", .true.)
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
          function test(tame, wild, bExpectedResult) result(bpassed)
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
