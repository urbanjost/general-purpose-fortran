           program demo_unique
           use M_sort, only : unique
           implicit none
           character(len=:),allocatable :: strings(:)
           integer                      :: icount

           strings=[character(len=2) :: '1','1','2','3','4','4','10','20','20','30']
           write(*,'(a,*(a3,1x))')'ORIGINAL:',strings
           write(*,'("SIZE=",i0)')size(strings)

           call unique(strings,icount)

           write(*,*)
           write(*,'(a,*(a3,1x))')'AFTER   :',strings(1:icount)(:2)
           write(*,'("SIZE=",i0)')size(strings)
           write(*,'("ICOUNT=",i0)')icount

           end program demo_unique
