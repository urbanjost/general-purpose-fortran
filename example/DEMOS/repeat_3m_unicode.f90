      program demo_repeat
      use M_unicode, only : ut=>unicode_type,repeat,escape,write(formatted)
      implicit none
         write(*,'(DT)') repeat(escape("\u2025*"), 35)
         write(*,'(DT)') repeat(ut("_"), 70)          ! line break
         write(*,'(DT)') repeat(ut("1234567890"), 7)  ! number line
         write(*,'(DT)') repeat(ut("         |"), 7)  !
      end program demo_repeat
