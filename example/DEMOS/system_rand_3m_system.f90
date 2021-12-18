        program demo_system_rand
        use M_system, only : system_srand, system_rand
        implicit none
        integer :: i

        call system_srand(1001)
        do i=1,10
           write(*,*)system_rand()
        enddo
        write(*,*)

        end program demo_system_rand
