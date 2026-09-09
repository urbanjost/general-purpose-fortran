     program demo_replace
     use M_unicode, only : ut=>unicode_type
     use M_unicode, only : unicode_type
     use M_unicode, only : character, replace
     use M_unicode, only : write(formatted)
     implicit none
     type(unicode_type) :: line
     !
     write(*,'(DT)') &
     & replace(ut('Xis is Xe string'),ut('X'),ut('th') )
     write(*,'(DT)') &
     & replace(ut('Xis is xe string'),ut('x'),ut('th'),ignorecase=.true.)
     write(*,'(DT)') &
     & replace(ut('Xis is xe string'),ut('X'),ut('th'),ignorecase=.false.)
     !
     ! a null old substring means "at beginning of line"
     write(*,'(DT)') &
     & replace(ut('my line of text'),ut(''),ut('BEFORE:'))
     !
     ! a null new string deletes occurrences of the old substring
     write(*,'(DT)') replace(ut('I wonder i ii iii'),ut('i'),ut(''))
     !
     ! Examples of the use of RANGE
     !
     line=replace(ut('aaaaaaaaa'),ut('a'),ut('A'),occurrence=1,repeat=1)
     write(*,*)'replace first a with A ['//line%character()//']'
     !
     line=replace(ut('aaaaaaaaa'),ut('a'),ut('A'),occurrence=3,repeat=3)
     write(*,*)'replace a with A for 3rd to 5th occurrence [' &
     & //line%character()//']'
     !
     line=replace(ut('ababababa'),ut('a'),ut(''),occurrence=3,repeat=3)
     write(*,*)'replace a with null instances 3 to 5 ['// &
     & line%character()//']'
     !
     line=replace( &
      & ut('a b ab baaa aaaa aa aa a a a aa aaaaaa'),&
      & ut('aa'),ut('CCCC'),occurrence=-1,repeat=1)
     write(*,*)'replace lastaa with CCCC ['//line%character()//']'
     !
     write(*,'(DT)')replace(ut('myf90stuff.f90.f90'),&
     & ut('f90'),ut('for'),occurrence=-1,repeat=1)
     write(*,'(DT)')replace(ut('myf90stuff.f90.f90'),&
     & ut('f90'),ut('for'),occurrence=-2,repeat=2)
     !
     end program demo_replace
