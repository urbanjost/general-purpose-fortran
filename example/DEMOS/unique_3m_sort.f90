        program demo_unique
        use M_sort, only : unique
        implicit none
        character(len=:),allocatable :: strings(:)
        integer,allocatable :: ints(:)
        integer :: icount
        integer :: ilong

        strings=[character(len=20) :: 'orange','green','green', &
        & 'red','white','blue','yellow','blue','magenta','cyan','black']
        ints=[30,1,1,2,3,4,4,-10,20,20,30]
        ilong=maxval(len_trim(strings))

        write(*,'(a,*(a,1x))')'ORIGINAL:',strings(:)(:ilong)
        write(*,'("SIZE=",i0)')size(strings)
        call unique(strings,icount)
        write(*,*)
        write(*,'(a,*(a,1x))')'AFTER   :',strings(1:icount)(:ilong)
        write(*,'("SIZE=",i0)')size(strings)
        write(*,'("ICOUNT=",i0)')icount

        write(*,'(a,*(g0,1x))')'ORIGINAL:',ints
        write(*,'("SIZE=",i0)')size(ints)
        call unique(ints,icount)
        write(*,*)
        write(*,'(a,*(g0,1x))')'AFTER   :',ints(1:icount)
        write(*,'("SIZE=",i0)')size(ints)
        write(*,'("ICOUNT=",i0)')icount

        end program demo_unique
