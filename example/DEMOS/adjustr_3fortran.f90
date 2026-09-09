      program demo_adjustr
      implicit none
      character(len=20) :: str
         ! print a short number line
         write(*,'(a)')repeat('1234567890',2)

        ! basic usage
         str = '  sample string '
         write(*,'(a)') str
         str = adjustr(str)
         write(*,'(a)') str

         !
         ! elemental
         !
         write(*,'(a)')repeat('1234567890',5)
         write(*,'(a)')adjustr([character(len=50) :: &
         '  first           ', &
         '     second       ', &
         '         third    ' ])
         write(*,'(a)')repeat('1234567890',5)

      end program demo_adjustr
