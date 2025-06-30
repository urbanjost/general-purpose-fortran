     !program demo_M_namelist
     module M_namelist__arguments
     use M_namelist,    only : print_dictionary, oneline, unnamed

     ! >>> CHANGE THIS
     ! declare and initialize a namelist. Letter_ denotes an uppercase short command keyword
     real              :: x=111.1, y=222.2, z=333.3
     real              :: point(3)=[10.0,20.0,30.0]
     character(len=80) :: title=" "
     logical           :: l=.false., l_=.false.
     logical           :: help=.false., version=.false., v=.false., h=.false.
     equivalence       (help,h),(version,v)
     namelist /args/ x,y,z,point,title,help,h,version,v,l,l_
     ! << END OF CHANGES

     contains
        subroutine get_args()
        integer :: iostat
        character(len=255) :: message ! use for I/O error messages
        character(len=1000) :: hold_namelist(60)
           hold_namelist=''
           write(hold_namelist,nml=args,iostat=iostat,iomsg=message)
           if(iostat.eq.0)then
              read(hold_namelist,nml=args,iostat=iostat,iomsg=message)
           endif
           if(iostat.ne.0)then
              write(*,'("ERROR:",i0,1x,a)')iostat, trim(message)
              call print_dictionary()
              stop 1
           endif
        end subroutine get_args
     end module M_namelist__arguments

     program short
     use M_namelist__arguments, only : get_args, unnamed
     use M_namelist__arguments  ! make user variables available
     implicit none
     integer :: i
        call get_args()  ! crack command line options
        ! >> USER YOUR VARIABLES HERE. FOR EXAMPLE:
        write(*,*)'VALUES ARE NOW ', new_line('A'),&
        &'x        ',x,              new_line('A'),&
        &'y        ',y,              new_line('A'),&
        &'z        ',z,              new_line('A'),&
        &'point    ',point,          new_line('A'),&
        &'title    ',title,          new_line('A'),&
        &'help     ',help,'h ',h,    new_line('A'),&
        &'version  ',version,'v ',v, new_line('A'),&
        &'l        ',l,              new_line('A'),&
        &'l_       ',l_
        if(allocated(unnamed))then
           if(size(unnamed).gt.0)then
              write(*,'(a)')'UNNAMED:'
              write(*,'(i6.6,3a)')(i,'[',unnamed(i),']',i=1,size(unnamed))
           endif
        endif
        !<< END OF EXAMPLE USAGE OF VARIABLES
     end program short
     !end program demo_M_namelist
