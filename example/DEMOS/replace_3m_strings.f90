     program demo_replace
     use M_strings, only : replace
     implicit none
     character(len=:),allocatable :: line

     write(*,*)replace('Xis is Xe string','X','th')
     write(*,*)replace('Xis is xe string','x','th',ignorecase=.true.)
     write(*,*)replace('Xis is xe string','X','th',ignorecase=.false.)

     ! a null old substring means "at beginning of line"
     write(*,*) replace('my line of text','','BEFORE:')

     ! a null new string deletes occurrences of the old substring
     write(*,*) replace('I wonder i ii iii','i','')

     ! Examples of the use of RANGE

     line=replace('aaaaaaaaa','a','A',occurrence=1,repeat=1)
     write(*,*)'replace first a with A ['//line//']'

     line=replace('aaaaaaaaa','a','A',occurrence=3,repeat=3)
     write(*,*)'replace a with A for 3rd to 5th occurrence ['//line//']'

     line=replace('ababababa','a','',occurrence=3,repeat=3)
     write(*,*)'replace a with null instances 3 to 5 ['//line//']'

     line=replace( &
      & 'a b ab baaa aaaa aa aa a a a aa aaaaaa',&
      & 'aa','CCCC',occurrence=-1,repeat=1)
     write(*,*)'replace lastaa with CCCC ['//line//']'

     write(*,*)replace('myf90stuff.f90.f90','f90','for',occurrence=-1,repeat=1)
     write(*,*)replace('myf90stuff.f90.f90','f90','for',occurrence=-2,repeat=2)

     end program demo_replace
