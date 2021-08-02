           program demo_esc
           use M_attr, only : attr, attr_mode, attr_update
              call printstuff('defaults')

              call attr_mode(manner='plain')
              call printstuff('plain:')

              call printstuff('raw')

              call attr_mode(manner='color')
              call printstuff('')

              write(*,'(a)') attr('TEST ADDING A CUSTOM SEQUENCE:')
              call attr_update('blink',char(27)//'[5m')
              call attr_update('/blink',char(27)//'[25m')
              write(*,'(a)') attr('<blink>Items for Friday</blink>')

           contains
           subroutine printstuff(label)
           character(len=*),intent(in)  :: label
           character(len=:),allocatable :: array(:)
             call attr_mode(manner=label)

             array=[character(len=60) ::    &
              'TEST MANNER='//label,                      &
              '<r>RED</r>,<g>GREEN</g>,<b>BLUE</b>',      &
              '<c>CYAN</c>,<m>MAGENTA</g>,<y>YELLOW</y>', &
              '<w>WHITE</w> and <e>EBONY</e>']
             write(*,'(a)') attr(array)

             write(*,'(a)') attr('Adding <bo>bold</bo>')
             write(*,'(a)') attr('<bo><r>RED</r>,<g>GREEN</g>,<b>BLUE</b></bo>')
             write(*,'(a)') attr('<bo><c>CYAN</c>,<m>MAGENTA</g>,<y>YELLOW</y></bo>')
             write(*,'(a)') attr('<bo><w>WHITE</w> and <e>EBONY</e></bo>')

             write(*,'(a)') attr('Adding <ul>underline</ul>')
             write(*,'(a)') attr(&
              &'<bo><ul><r>RED</r>,<g>GREEN</g>,<b>BLUE</b></ul></bo>')
             write(*,'(a)') attr(&
              &'<bo><ul><c>CYAN</c>,<m>MAGENTA</g>,<y>YELLOW</y></ul></bo>')
             write(*,'(a)') attr('<bo><ul><w>WHITE</w> and <e>EBONY</e></ul></bo>')

             write(*,'(a)') attr('Adding <ul>italic</ul>')
             write(*,'(a)') attr(&
              &'<bo><ul><it><r>RED</r>,<g>GREEN</g>,<b>BLUE</b></it></ul></bo>')
             write(*,'(a)') attr(&
              &'<bo><ul><it><c>CYAN</c>,<m>MAGENTA</g>,<y>YELLOW</it></y></ul></bo>')
             write(*,'(a)') attr('<bo><ul><it><w>WHITE</w> and <e>EBONY</e></ul></bo>')

             write(*,'(a)') attr('Adding <in>inverse</in>')
             write(*,'(a)') attr('<in><bo><ul><it><r>RED</r>,<g>GREEN</g>,&
              &<b>BLUE</b></it></ul></bo></in>')
             write(*,'(a)') attr('<in><bo><ul><it><c>CYAN</c>,<m>MAGENTA</g>,&
              &<y>YELLOW</it></y></ul></bo></in>')
             write(*,'(a)') attr(&
              &'<in><bo><ul><it><w>WHITE</w> and <e>EBONY</e></ul></bo></in>')
           end subroutine printstuff
           end program demo_esc
