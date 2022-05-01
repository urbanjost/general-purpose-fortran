     program demo_prank_unique
     ! ranks array, removing duplicates
     use M_orderpack, only : prank_unique
     implicit none
     character(len=*),parameter :: g='(*(g0,1x))'
     integer,allocatable :: INVALS(:)
     integer,allocatable :: irngt(:)
     integer :: nord
     !
     write(*,g)'If enough values are unique, will return NORD indices'
     if(allocated(irngt))deallocate(irngt)
     INVALS=[10,5,7,1,4,5,6,8,9,10,1]
     nord=5
     allocate(irngt(nord))
     call printme()
     !
     !BUG!write(*,g)'If not enough values are unique, will change NORD'
     !BUG!INVALS=[-1,0,-1,0,-1,0,-1]
     !BUG!nord=5
     !BUG!if(allocated(irngt))deallocate(irngt)
     !BUG!allocate(irngt(nord))
     !BUG!call printme()
     contains
     subroutine printme()
        write(*,g)'ORIGINAL:',INVALS
        write(*,g)'NUMBER OF INDICES TO SORT:',nord
        call prank_unique(INVALS,irngt,nord)
        write(*,g)'NUMBER OF INDICES RETURNED:',nord
        write(*,g)'RETURNED INDICES:',irngt(:nord)
        write(*,g)nord,'SMALLEST UNIQUE VALUES:',INVALS(irngt(:nord))
     end subroutine
     end program demo_prank_unique
