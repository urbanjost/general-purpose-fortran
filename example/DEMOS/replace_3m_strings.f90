          program demo_replace
          use M_strings, only : replace
          implicit none
          character(len=:),allocatable :: targetline

          targetline='this is the input string'

          call testit('th','TH','THis is THe input string')

          ! a null old substring means "at beginning of line"
          call testit('','BEFORE:', 'BEFORE:THis is THe input string')

          ! a null new string deletes occurrences of the old substring
          call testit('i','', 'BEFORE:THs s THe nput strng')

          write(*,*)'Examples of the use of RANGE='

          targetline=replace('a b ab baaa aaaa','a','A')
          write(*,*)'replace a with A ['//targetline//']'

          targetline=replace('a b ab baaa aaaa','a','A',range=[3,5])
          write(*,*)'replace a with A instances 3 to 5 ['//targetline//']'

          targetline=replace('a b ab baaa aaaa','a','',range=[3,5])
          write(*,*)'replace a with null instances 3 to 5 ['//targetline//']'

          targetline=&
          &replace('a b ab baaa aaaa aa aa a a a aa aaaaaa',&
          & 'aa','CCCC',range=[3,5])
          write(*,*)'replace aa with CCCC instances 3 to 5 ['//targetline//']'

          contains
          subroutine testit(old,new,expected)
          character(len=*),intent(in) :: old,new,expected
          write(*,*)repeat('=',65)
          write(*,*)'STARTED ['//targetline//']'
          write(*,*)'OLD['//old//']', ' NEW['//new//']'
          targetline=replace(targetline,old,new)
          write(*,*)'GOT     ['//targetline//']'
          write(*,*)'EXPECTED['//expected//']'
          write(*,*)'TEST    [',targetline.eq.expected,']'
          end subroutine testit

          end program demo_replace
