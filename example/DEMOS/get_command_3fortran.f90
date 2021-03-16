          program demo_get_command
          implicit none
          integer                      :: COMMAND_LINE_LENGTH
          character(len=:),allocatable :: COMMAND_LINE
             ! get command line length
             call get_command(length=COMMAND_LINE_LENGTH)
             ! allocate string big enough to hold command line
             allocate(character(len=COMMAND_LINE_LENGTH) :: COMMAND_LINE)
             ! get command line as a string
             call get_command(command=COMMAND_LINE)
             ! trim leading spaces just in case
             COMMAND_LINE=adjustl(COMMAND_LINE)
             write(*,'("OUTPUT:",a)')COMMAND_LINE
          end program demo_get_command
