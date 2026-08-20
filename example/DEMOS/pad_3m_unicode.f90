    program demo_pad
    use M_unicode, only  : pad, assignment(=)
    !use M_unicode, only : write(formatted)
    use M_unicode, only  : len
    use M_unicode, only  : ch=> character
    use M_unicode, only  : ut=> unicode_type
    implicit none
    type(ut)                   :: string
    type(ut)                   :: answer
    integer                    :: i
    !character(len=*),parameter :: u='(*(DT))'
    character(len=*),parameter :: u='(*(g0))'
      !
      string='abcdefghij'
      !
      write(*,*)'pad on right till 20 characters long'
      answer=pad(string,20)
      write(*,'("[",g0,"]",/)') answer%character()
      !
      write(*,*)'original is not trimmed for short length requests'
      answer=pad(string,5)
      write(*,'("[",g0,"]",/)') answer%character()
      !
      i=30
      write(*,*)'pad with specified string and left-justified integers'
      write(*,'(1x,g0,1x,i0)') &
       & ch(pad(ut('CHAPTER 1 : The beginning '),i,ut('.') )), 1   , &
       & ch(pad(ut('CHAPTER 2 : The end '),i,ut('.') )),       1234, &
       & ch(pad(ut('APPENDIX '),i,ut('.') )),                  1235
      !
      write(*,*)'pad with specified string and right-justified integers'
      write(*,'(1x,g0,i7)') &
       & ch(pad(ut('CHAPTER 1 : The beginning '),i,ut('.') )), 1   , &
       & ch(pad(ut('CHAPTER 2 : The end '),i,ut('.') )),       1234, &
       & ch(pad(ut('APPENDIX '),i,ut('.') )),                  1235
      !
      write(*,*)'pad on left with zeros'
      write(*,u)ch(pad(ut('12'),5,ut('0'),right=.false.))
      !
      write(*,*)'various lengths with clip .true. and .false.'
      write(*,u)ch(pad(ut('12345 '),30,ut('_'),right=.false.))
      write(*,u)ch(pad(ut('12345 '),30,ut('_'),right=.false.,clip=.true.))
      write(*,u)ch(pad(ut('12345 '), 7,ut('_'),right=.false.))
      write(*,u)ch(pad(ut('12345 '), 7,ut('_'),right=.false.,clip=.true.))
      write(*,u)ch(pad(ut('12345 '), 6,ut('_'),right=.false.))
      write(*,u)ch(pad(ut('12345 '), 6,ut('_'),right=.false.,clip=.true.))
      write(*,u)ch(pad(ut('12345 '), 5,ut('_'),right=.false.))
      write(*,u)ch(pad(ut('12345 '), 5,ut('_'),right=.false.,clip=.true.))
      write(*,u)ch(pad(ut('12345 '), 4,ut('_'),right=.false.))
      write(*,u)ch(pad(ut('12345 '), 4,ut('_'),right=.false.,clip=.true.))
  end program demo_pad
