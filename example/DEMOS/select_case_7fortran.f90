         program demo_select_case
         implicit none
         integer :: n
         n=4
         select case (n)
          case (1, 3:5, 8) ! selects 1, 3, 4, 5, 8
             write(*,*)'case A',n
          case default
             write(*,*)'default',n
         end select
         end program demo_select_case
