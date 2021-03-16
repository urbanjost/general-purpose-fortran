          program demo_attr
          use M_escape, only : attr, esc_mode
          implicit none
               call printme('color')
               call printme('plain')
               call printme('raw')
          contains
          subroutine printme(mymode)
          character(len=*),intent(in) :: mymode
             call esc_mode(mymode)
             write(*,'(a)')mymode
             write(*,'(*(g0))',advance='no')attr('red:BLUE:bold','Hello!'), &
              & 'and everything is back to defaults or ', &
              & attr('RED:blue:bold'),'Hello Again!', &
              & attr('/RED'),' Well, the text color is still blue.',attr('reset')
             write(*,'(*(g0))',advance='yes')' Back to a normal write statement.'
          end subroutine printme
          end program demo_attr
