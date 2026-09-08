       program demo_occurrences
       use M_orderpack, only : occurrences
       ! determine how many times each value appears in an input array
       implicit none
       character(len=*),parameter    :: g='(*(g0,1x))'
       character(len=20),allocatable :: strings(:)
       integer,allocatable           :: cindx(:)
       integer                       :: csz
       integer                       :: i
          ! each name appears the number of times its name represents
          strings= [ character(len=20) ::                           &
          & 'two  ',  'four ', 'three', 'five',   'five',           &
          & 'two  ',  'four ', 'three', 'five',   'five',           &
          & 'four ',  'four ', 'three', 'one  ',  'five']
          csz=size(strings)
          if(allocated(cindx))deallocate(cindx)
          allocate(cindx(csz))
          call occurrences(strings,cindx)
          write(*,g)(trim(strings(i)),i=1,csz)
          write(*,g)cindx
       end program demo_occurrences
