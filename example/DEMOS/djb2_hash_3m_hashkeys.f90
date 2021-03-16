           program demo_djb2_hash
           use M_hashkeys, only : djb2_hash, int128
           implicit none
           integer(kind=int128)         :: hash
           character(len=:),allocatable :: string
           integer                      :: i
           ! string
           string='test djb2_hash'
           hash=djb2_hash(string)
           write(*,*)'string=',string,' hash=',hash
           ! array of characters
           hash=djb2_hash(['t','e','s','t',' ','d','j','b','2','_','h','a','s','h'])
           write(*,*)'string=',string,' hash=',hash
           ! continued hash
           hash=djb2_hash(['t','e','s','t'])
           hash=djb2_hash([' ','d','j','b','2'],continue=.true.)
           hash=djb2_hash(['_','h','a','s','h'],continue=.true.)
           write(*,*)'string=',string,' hash=',hash
           ! array of integers
           hash=djb2_hash([(i,i=0,100)])
           write(*,*)'hash for values 0 to 100 is ',hash
           !
           end program demo_djb2_hash
