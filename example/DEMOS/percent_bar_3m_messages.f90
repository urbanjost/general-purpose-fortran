        program demo_percent_bar
        use M_messages, only : percent_bar
        implicit none
        integer :: arbitrary_max
        integer :: arbitrary_count
        real    :: percent
        real    :: old_percent
        arbitrary_max = 333
        percent = 0.00
        old_percent = -1.00
        do arbitrary_count = 1, arbitrary_max
            percent = real(arbitrary_count) / real(arbitrary_max - 1)
            if(old_percent /= percent) call percent_bar(percent)
            old_percent=percent
            call execute_command_line('sleep 0.0')
        enddo
        end program demo_percent_bar
