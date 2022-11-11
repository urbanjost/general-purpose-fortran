        program demo_repeat
        implicit none
            write(*,'(a)') repeat("^v", 36)         ! line break
            write(*,'(a)') repeat("_", 72)          ! line break
            write(*,'(a)') repeat("1234567890", 7)  ! number line
            write(*,'(a)') repeat("         |", 7)  !
        end program demo_repeat
