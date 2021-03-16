          program demo_csv
          use M_csv, only : csv
          implicit none
          character(len=:),allocatable :: pr

             write(*,*)'LIST-DIRECTED:'
             write(*,*,DELIM='QUOTE')'string',.true.,.false.,111,23.45,10.20e15,3456.78901234d0,cmplx(huge(0.0),tiny(0.0))

             write(*,*)'G0:'
             write(*,'(*(g0:","))')'string',.true.,.false.,111,23.45,10.20e15,3456.78901234d0,cmplx(huge(0.0),tiny(0.0))

             write(*,*)'CSV:'
             pr=csv('string',.true.,.false.,111,23.45,10.20e15,3456.78901234d0,cmplx(huge(0.0),tiny(0.0)) )
             write(*,'(a)')pr

              end program demo_csv
