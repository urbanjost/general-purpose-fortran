           program demo_adjustr
           implicit none
           integer :: right
           character(len=*),parameter :: bracket='("[",a,"]")'
           character(len=20) :: str = ' sample string '
           character(len=:),allocatable :: astr
              call number_line()
              !
              ! basic usage
              str = adjustr(str)
              write(*,bracket) str

              ! exploring usage:
              ! An allocatable string and arbitrary margin.
              ! Set a right margin and adjust to it. Note
              ! this would truncate if the margin is less
              ! than the length of STR
              right=50
              astr=adjustr(str//repeat(' ',max(0,right-len(str))))
              write(*,bracket) astr
              !
              call number_line()
              !
           contains
              subroutine number_line()
              ! print a short number line
                 write(*,bracket)repeat('1234567890',5)
              end subroutine number_line
           end program demo_adjustr
