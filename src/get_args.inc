   subroutine get_args()
      ! The NAMELIST cannot be passed as an option to a routine so this
      ! routine must be in a contained routine or directly in the body of
      ! the routine that declares the NAMELIST. get_args(3f) should not
      ! need changed except for possibly the length of HOLD_NAMELIST
      use M_commandline,    only : commandline, print_dictionary, oneline
      integer :: ios, i
      character(len=255) :: message ! use for I/O error messages
      character(len=:),allocatable :: readme  ! stores updated namelist
      ! make big enough for all of namelist
      character(len=10000) :: hold_namelist(60)
      ! the routine needs a copy of the options to determine what values
      ! are character and logical versus numeric
      write(hold_namelist,nml=args,iostat=ios,iomsg=message)
      if(ios.eq.0)then
         ! pass in the namelist and get an updated copy that includes
         ! values specified on the command line
         readme=commandline(oneline(hold_namelist))
         ! read the updated namelist to update the values in the namelist group
         read(readme,nml=args,iostat=ios,iomsg=message)
      endif
      if(ios.ne.0)then
         write(*,'("ERROR:",i0,1x,a)')ios, trim(message)
         call print_dictionary()
         stop 1
      endif
      ! all done cracking the command line
   end subroutine get_args
