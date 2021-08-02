            program demo_update
            use M_attr, only : attr, attr_update
               write(*,'(a)') attr('<clear>TEST CUSTOMIZATIONS:')
               ! add custom keywords
               call attr_update('blink',char(27)//'[5m')
               call attr_update('/blink',char(27)//'[25m')
               write(*,*)
               write(*,'(a)') attr('<blink>Items for Friday</blink>')
               call attr_update('ouch',attr( &
               ' <R><bo><w>BIG mistake!</R></w> '))
               write(*,*)
               write(*,'(a)') attr('<ouch> Did not see that coming.')
               write(*,*)
               write(*,'(a)') attr( &
               'ORIGINALLY: <r>Apple</r>, <b>Sky</b>, <g>Grass</g>')
               ! delete
               call attr_update('r')
               call attr_update('/r')
               ! replace (or create)
               call attr_update('b','<<<<')
               call attr_update('/b','>>>>')
               write(*,*)
               write(*,'(a)') attr( &
               'CUSTOMIZED: <r>Apple</r>, <b>Sky</b>, <g>Grass</g>')
            end program demo_update
