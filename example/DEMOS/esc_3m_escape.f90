          program demo_esc
          use M_escape, only : esc, esc_mode, update
             write(*,'(a)') esc('<clear>TEST DEFAULTS:')
             call printstuff()

             write(*,'(a)') esc('TEST MANNER=PLAIN:')
             call esc_mode(manner='plain')
             call printstuff()

             write(*,'(a)') esc('TEST MANNER=RAW:')
             call esc_mode(manner='raw')
             call printstuff()

             write(*,'(a)') esc('TEST MANNER=color:')
             call esc_mode(manner='color')
             call printstuff()

             write(*,'(a)') esc('TEST ADDING A CUSTOM SEQUENCE:')
             call update('blink',char(27)//'[5m')
             call update('/blink',char(27)//'[38m')
             write(*,'(a)') esc('<blink>Items for Friday</blink>')

          contains
          subroutine printstuff()

            write(*,'(a)') esc('<r>RED</r>,<g>GREEN</g>,<b>BLUE</b>')
            write(*,'(a)') esc('<c>CYAN</c>,<m>MAGENTA</g>,<y>YELLOW</y>')
            write(*,'(a)') esc('<w>WHITE</w> and <e>EBONY</e>')

            write(*,'(a)') esc('Adding <bo>bold</bo>')
            write(*,'(a)') esc('<bo><r>RED</r>,<g>GREEN</g>,<b>BLUE</b></bo>')
            write(*,'(a)') esc('<bo><c>CYAN</c>,<m>MAGENTA</g>,<y>YELLOW</y></bo>')
            write(*,'(a)') esc('<bo><w>WHITE</w> and <e>EBONY</e></bo>')

            write(*,'(a)') esc('Adding <ul>underline</ul>')
            write(*,'(a)') esc(&
             &'<bo><ul><r>RED</r>,<g>GREEN</g>,<b>BLUE</b></ul></bo>')
            write(*,'(a)') esc(&
             &'<bo><ul><c>CYAN</c>,<m>MAGENTA</g>,<y>YELLOW</y></ul></bo>')
            write(*,'(a)') esc('<bo><ul><w>WHITE</w> and <e>EBONY</e></ul></bo>')

            write(*,'(a)') esc('Adding <ul>italic</ul>')
            write(*,'(a)') esc(&
             &'<bo><ul><it><r>RED</r>,<g>GREEN</g>,<b>BLUE</b></it></ul></bo>')
            write(*,'(a)') esc(&
             &'<bo><ul><it><c>CYAN</c>,<m>MAGENTA</g>,<y>YELLOW</it></y></ul></bo>')
            write(*,'(a)') esc('<bo><ul><it><w>WHITE</w> and <e>EBONY</e></ul></bo>')

            write(*,'(a)') esc('Adding <in>inverse</in>')
            write(*,'(a)') esc(&
             &'<in><bo><ul><it><r>RED</r>,<g>GREEN</g>,&
             &<b>BLUE</b></it></ul></bo></in>')
            write(*,'(a)') esc(&
             &'<in><bo><ul><it><c>CYAN</c>,<m>MAGENTA</g>,&
             &<y>YELLOW</it></y></ul></bo></in>')
            write(*,'(a)') esc(&
             &'<in><bo><ul><it><w>WHITE</w> and <e>EBONY</e></ul></bo></in>')
       end subroutine printstuff

              end program demo_esc
