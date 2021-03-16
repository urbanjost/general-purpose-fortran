          program demo_update
          use M_escape, only : esc, update
             write(*,'(a)') esc('<clear>TEST CUSTOMIZED:')
             ! add custom keywords
             call update('blink',char(27)//'[5m')
             call update('/blink',char(27)//'[38m')

             write(*,'(a)') esc('<blink>Items for Friday</blink>')

             write(*,'(a)',advance='no') esc('<r>RED</r>,')
             write(*,'(a)',advance='no') esc('<b>BLUE</b>,')
             write(*,'(a)',advance='yes') esc('<g>GREEN</g>')

             ! delete
             call update('r')
             call update('/r')
             ! replace
             call update('b','<<<<')
             call update('/b','>>>>')
             write(*,'(a)',advance='no') esc('<r>RED</r>,')
             write(*,'(a)',advance='no') esc('<b>BLUE</b>,')
             write(*,'(a)',advance='yes') esc('<g>GREEN</g>')

          end program demo_update
