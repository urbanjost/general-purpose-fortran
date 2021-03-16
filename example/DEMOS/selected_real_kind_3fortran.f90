          program demo_selected_real_kind
          implicit none
            integer,parameter :: p6 = selected_real_kind(6)
            integer,parameter :: p10r100 = selected_real_kind(10,100)
            integer,parameter :: r400 = selected_real_kind(r=400)
            real(kind=p6) :: x
            real(kind=p10r100) :: y
            real(kind=r400) :: z

            print *, precision(x), range(x)
            print *, precision(y), range(y)
            print *, precision(z), range(z)
          end program demo_selected_real_kind
