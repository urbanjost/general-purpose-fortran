      program demo_get_command
      implicit none
      integer                      :: command_line_length
      character(len=:),allocatable :: command_line
         ! get command line length
         call get_command(length=command_line_length)
         ! allocate string big enough to hold command line
         allocate(character(len=command_line_length) :: command_line)
         ! get command line as a string
         call get_command(command=command_line)
         ! trim leading spaces just in case
         command_line=adjustl(command_line)
         write(*,'("OUTPUT:",a)')command_line
      end program demo_get_command
