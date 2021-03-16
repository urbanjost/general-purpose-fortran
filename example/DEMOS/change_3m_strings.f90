          program demo_change

           use M_strings, only : change
           implicit none
           character(len=132) :: line='This is a test string to change'
           integer            :: ierr
              write(*,*)trim(line)
              ! change miniscule a to uppercase A
              call change(line,'c/a/A/',ierr)
              write(*,*)trim(line)
              ! put string at beginning of line
              call change(line,'c//prefix: /',ierr)
              write(*,*)trim(line)
              ! remove blanks
              call change(line,'c/ //',ierr)
              write(*,*)trim(line)
       end program demo_change
