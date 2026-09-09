        program demo_system_srand
        use M_system, only : system_srand, system_rand
        implicit none
        integer :: i,j
        do j=1,2
           call system_srand(1001)
           do i=1,10
              write(*,*)system_rand()
           enddo
           write(*,*)
        enddo
        end program demo_system_srand
