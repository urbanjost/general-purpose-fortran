      program demo_join
      use M_strings, only: join
      implicit none
      character(len=*),parameter   :: w='(/,*(g0,/,g0))'
      character(len=:),allocatable :: s(:)
        s=[character(len=10) :: &
          & ' United', &
          & 'we', &
          & 'stand,', &
        & 'divided', &
          & 'we fall.']
        write(*,w) 'SIMPLE JOIN:                  ',&
           join(s)
        write(*,w) 'SIMPLE JOIN WITH SEPARATOR:   ',&
           join(s,sep=' ')
        write(*,w) 'CUSTOM SEPARATOR:             ',&
           join(s,sep='==>')
        write(*,w) 'LEFT AND RIGHT AND SEPARATOR: ',&
           join(s,sep=';',left='[',right=']')
        write(*,w) 'NO TRIMMING:                  ',&
           join(s,trm=.false.)
        write(*,w) 'LEFT AND RIGHT:               ',&
           join(s,left='[',right=']')
        write(*,w) 'START,END AND EVERYTHING:     ',&
           join(s,trm=.false.,sep=',',start='[',end=']',left='"',right='"')
        write(*,w) 'TABLE'
        call line()
        write(*,'(a)') join(s(1:3),trm=.false.,sep='|',start='|',end='|')
        write(*,'(a)') join([s(4:5),repeat(' ',len(s))],&
        & trm=.false.,sep='|',start='|',end='|')
        call line()
      contains
      subroutine line()
      integer :: i
        write(*,'(a)') join([(repeat('-',len(s)),i=1,3)],&
        & sep='#',start='#',end='#')
      end subroutine line
      end program demo_join
