     program demo_unit_test_system
     use M_framework, only: &
        unit_test_start,  &
        unit_test,        &
        unit_test_system, &
        unit_test_end
     implicit none
     if (command_argument_count()  ==  0) then
        call unit_test_start('myroutine')
        call unit_test('false', unit_test_system('false') == 0, 'check false')
        call unit_test('true', unit_test_system('true') == 0, 'check true')
        call unit_test('notthere', unit_test_system('notthere') == 0, &
        & 'check notthere')
        call unit_test('*',&
        & unit_test_system('* and options', verbose=.true.) == 0, 'check "*"')
        call unit_test_end('myroutine')
     else
        write (*, *) 'called with an option'
     endif
     end program demo_unit_test_system
