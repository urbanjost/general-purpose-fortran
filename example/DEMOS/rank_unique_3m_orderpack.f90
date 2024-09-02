     program demo_rank_unique
     ! rank an array, with removal of duplicate entries.
     use M_orderpack, only : rank_unique
     implicit none
     character(len=*),parameter :: g='(*(g0,1x))'
     integer,allocatable :: INVALS(:)
     !
     INVALS=[10,5,7,1,4,5,6,8,9,10,1]
     call printme()
     INVALS=[-1,0,-2,0,-3,0,-4]
     call printme()
     contains
     subroutine printme()
     integer,allocatable :: irngt(:)
     integer :: nuni
        if(allocated(irngt))deallocate(irngt)
        allocate(irngt(size(INVALS)))
        write(*,g)'ORIGINAL:',INVALS
        call rank_unique(INVALS,irngt,nuni)
        write(*,g)'NUMBER OF UNIQUE INDICES:',nuni
        write(*,g)'RETURNED INDICES:',irngt(:nuni)
        write(*,g)'SORTED DATA:',INVALS(irngt(:nuni))
     end subroutine
     end program demo_rank_unique
