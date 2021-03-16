           program demo_merge_str
           use M_strings, only : merge_str
           implicit none
           character(len=:), allocatable :: answer
              answer=merge_str('first string', &
               & 'second string is longer',10.eq.10)
              write(*,'("[",a,"]")') answer
              answer=merge_str('first string', &
               & 'second string is longer',10.ne.10)
              write(*,'("[",a,"]")') answer
           end program demo_merge_str
