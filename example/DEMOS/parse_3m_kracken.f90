          program demo_parse
          use M_kracken, only : parse, sget, iget, rget
          use M_strings, only : chomp
          implicit none
          character(len=:),allocatable  :: verb
          character(len=*),parameter    :: delimiters=' ;,'
          integer     :: i
          integer     :: ierr
          character(len=132) :: line
          character(len=132), parameter :: commands(5)= [character(len=132) :: &
            'start -i 10 -message this is a message', &
            'end -i 20 -j 30 -k 55.55 ', &
            'list', &
            'help -oo', &
            'end -i 44.44 ']
            do i=1,size(commands)
               line=commands(i) ! need mutable line
               if(chomp(line,verb,delimiters).ge. 0)then
                  call parse(verb,line,'add',ierr)
                  write(*,*)'do whatever a '//verb//' command does'
                  select case(verb)
                  case('start')
                     write(*,*)trim(sget('start_i'))
                     write(*,*)trim(sget('start_message'))
                  case('end')
                     write(*,*)iget('end_i')
                     write(*,*)iget('end_j')
                     write(*,*)rget('end_k')
                  case('list')
                     write(*,*)'list things'
                  case('help')
                     write(*,*)'show help text'
                  endselect
               endif
            enddo
            ! look at some of the values as strings or numbers
            write(*,*)trim(sget('start_i'))
            write(*,*)trim(sget('start_message'))
            write(*,*)iget('end_i')
            write(*,*)iget('end_j')
            write(*,*)rget('end_k')
          end program demo_parse
