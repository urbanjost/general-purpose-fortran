     program demo_tokenize
     use M_unicode, only : tokenize, ut=>unicode_type,ch=>character
     use M_unicode, only : assignment(=),operator(/=)
     implicit none
     !
     ! some useful formats
     character(len=*),parameter ::       &
      & brackets='(*("[",g0,"]":,","))' ,&
      & a_commas='(a,*(g0:,","))'       ,&
      & gen='(*(g0))'
     !
     ! Execution of TOKEN form (return array of tokens)
     !
        block
        type(ut)             :: string
        type(ut),allocatable :: tokens(:)
        integer              :: i
           string = '  first,second ,third       '
           call tokenize(string, set=';,', tokens=tokens )
           write(*,brackets)ch(tokens)

           string = '  first , second ,third       '
           call tokenize(string, set=' ,', tokens=tokens )
           write(*,brackets)(tokens(i)%character(),i=1,size(tokens))
           ! remove blank tokens
           tokens=pack(tokens, tokens /= '' )
           write(*,brackets)ch(tokens)
     !
        endblock
     !
     ! Execution of BOUNDS form (return position of tokens)
     !
        block
        type(ut)                   :: string
        character(len=*),parameter :: set = " ,"
        integer,allocatable        :: first(:), last(:)
           write(*,gen)repeat('1234567890',6)
           string = 'first,second,,fourth'
           write(*,gen)ch(string)
           call tokenize (string, set, first, last)
           write(*,a_commas)'FIRST=',first
           write(*,a_commas)'LAST=',last
           write(*,a_commas)'HAS LENGTH=',last-first.gt.0
        endblock
     !
     end program demo_tokenize
