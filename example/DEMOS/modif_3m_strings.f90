          program demo_modif
          use M_strings, only : modif
          implicit none
          character(len=256)           :: line
          integer                      :: ios
          integer                      :: count
          integer                      :: COMMAND_LINE_LENGTH
          character(len=:),allocatable :: COMMAND_LINE
             ! get command name length
             call get_command_argument(0,length=count)
             ! get command line length
             call get_command(length=COMMAND_LINE_LENGTH)
             ! allocate string big enough to hold command line
             allocate(character(len=COMMAND_LINE_LENGTH+200) :: COMMAND_LINE)
             ! get command line as a string
             call get_command(command=COMMAND_LINE)
             ! trim leading spaces just in case
             COMMAND_LINE=adjustl(COMMAND_LINE)
             ! remove command name
             COMMAND_LINE=adjustl(COMMAND_LINE(COUNT+2:))
             INFINITE: do
                read(*,'(a)',iostat=ios)line
                if(ios.ne.0)exit
                call modif(line,COMMAND_LINE)
                write(*,'(a)')trim(line)
             enddo INFINITE
          end program demo_modif
