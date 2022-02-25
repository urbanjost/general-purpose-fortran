     program demo_longest_common_substring
     use M_strings, only : longest_common_substring
     implicit none
        call  compare('testing123testingthing', 'thisis',                 'thi')
        call  compare('testing',                'sting',                  'sting')
        call  compare('thisisatest_stinger',    'testing123testingthing', 'sting')
        call  compare('thisisatest_stinger',    'thisis',                 'thisis')
        call  compare('thisisatest',            'testing123testing',      'test')
        call  compare('thisisatest',            'thisisatest',            'thisisatest')
     contains

     subroutine compare(a,b,answer)
     character(len=*),intent(in) :: a, b, answer
     character(len=:),allocatable :: match
     character(len=*),parameter :: g='(*(g0))'
     integer :: i
        match=longest_common_substring(a,b)
        write(*,g) 'comparing "',a,'" and "',b,'"'
        write(*,g) merge('(PASSED) "','(FAILED) "',answer.eq.match), &
        & match,'"; expected "',answer,'"'
     end subroutine compare

     end program demo_longest_common_substring
