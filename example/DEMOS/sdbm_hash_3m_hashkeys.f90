           program demo_sdbm_hash
           use M_hashkeys, only : sdbm_hash, int128
           implicit none
           integer(kind=int128)         :: hash
           character(len=:),allocatable :: string
           integer                      :: i
           ! string
           string='test sdbm_hash'
           hash=sdbm_hash(string)
           write(*,*)'string=',string,' hash=',hash
           ! array of characters
           hash=sdbm_hash(['t','e','s','t',' ','s','d','b','m','_','h','a','s','h'])
           write(*,*)'string=',string,' hash=',hash
           ! continued hash
           hash=sdbm_hash(['t','e','s','t'])
           hash=sdbm_hash([' ','s','d','b','m'],continue=.true.)
           hash=sdbm_hash(['_','h','a','s','h'],continue=.true.)
           write(*,*)'string=',string,' hash=',hash
           ! array of integers
           hash=sdbm_hash([(i,i=0,100)])
           write(*,*)'hash for values 0 to 100 is ',hash
           !
           end program demo_sdbm_hash
