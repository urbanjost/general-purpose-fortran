           program demo_adjustl
           implicit none
           character(len=20) :: str = '   sample string'
           character(len=:),allocatable :: astr
              !
              ! basic use
              str = adjustl(str)
              write(*,'("[",a,"]")') str, trim(str)
              !
              ! an allocatable string stays the same length
              ! and is not trimmed.
              astr='    allocatable string   '
              write(*,'("[",a,"]")') adjustl(astr)
              !
           end program demo_adjustl
