         program demo_join
         use M_strings, only: join
         implicit none
         character(len=:),allocatable  :: s(:)
         character(len=:),allocatable  :: out
         integer                       :: i
           s=[character(len=10) :: 'United',' we',' stand,', &
           & ' divided',' we fall.']
           out=join(s)
           write(*,'(a)') out
           write(*,'(a)') join(s,trm=.false.)
           write(*,'(a)') (join(s,trm=.false.,sep='|'),i=1,3)
           write(*,'(a)') join(s,sep='<>')
           write(*,'(a)') join(s,sep=';',left='[',right=']')
           write(*,'(a)') join(s,left='[',right=']')
           write(*,'(a)') join(s,left='>>')
         end program demo_join
