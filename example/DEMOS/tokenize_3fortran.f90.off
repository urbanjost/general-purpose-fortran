          program demo_tokenize
          !use M_strings, only : tokenize=>split2020
          implicit none
          ! some useful formats
          character(len=*),parameter :: brackets='(*("[",g0,"]":,","))'
          character(len=*),parameter :: a_commas='(a,*(g0:,","))'
          character(len=*),parameter :: space='(*(g0:,1x))'
          character(len=*),parameter :: gen='(*(g0))'

          ! Execution of TOKEN form (return array of tokens)

          block
             character (len=:), allocatable :: string
             character (len=:), allocatable :: tokens(:)
             character (len=:), allocatable :: kludge(:)
             integer                        :: i
             string = '  first,second ,third       '
             call tokenize(string, set=';,', tokens=tokens )
             write(*,brackets)tokens

             string = '  first , second ,third       '
             call tokenize(string, set=' ,', tokens=tokens )
             write(*,brackets)(trim(tokens(i)),i=1,size(tokens))
             ! remove blank tokens
             ! <<<
             !tokens=pack(tokens, tokens /= '' )
             ! gfortran 13.1.0 bug -- concatenate //'' and use scratch
             ! variable KLUDGE. JSU: 2024-08-18
             kludge=pack(tokens//'', tokens /= '' )
             ! >>>
             write(*,brackets)kludge

          endblock

          ! Execution of BOUNDS form (return position of tokens)

          block
             character (len=:), allocatable :: string
             character (len=*),parameter :: set = " ,"
             integer, allocatable        :: first(:), last(:)
             write(*,gen)repeat('1234567890',6)
             string = 'first,second,,fourth'
             write(*,gen)string
             call tokenize (string, set, first, last)
             write(*,a_commas)'FIRST=',first
             write(*,a_commas)'LAST=',last
             write(*,a_commas)'HAS LENGTH=',last-first.gt.0
          endblock

          end program demo_tokenize
