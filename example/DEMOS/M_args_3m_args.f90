          !program demo_M_args
          module M_arguments
          use M_args,    only : get_namelist, print_dictionary, unnamed, oneline

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
             integer :: ios
             character(len=255) :: message ! use for I/O error messages
             character(len=:),allocatable :: readme  ! stores updated namelist
             character(len=10000) :: hold_namelist(60)
                hold_namelist=''
                write(hold_namelist,nml=args,iostat=ios,iomsg=message)
                if(ios.eq.0)then
                   readme=get_namelist(oneline(hold_namelist))
                   read(readme,nml=args,iostat=ios,iomsg=message)
                endif
                if(ios.ne.0)then
                   write(*,'("ERROR:",i0,1x,a)')ios, trim(message)
                   call print_dictionary()
                   stop 1
                endif
             end subroutine get_args
          end module M_arguments

          program short
          use M_arguments, only : get_args, unnamed
          use M_arguments  ! make user variables available
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
             if(size(unnamed).gt.0)then
                write(*,'(a)')'UNNAMED:'
                write(*,'(i6.6,3a)')(i,'[',unnamed(i),']',i=1,size(unnamed))
             endif
             !<< END OF EXAMPLE USAGE OF VARIABLES
          end program short
          !end program demo_M_args
