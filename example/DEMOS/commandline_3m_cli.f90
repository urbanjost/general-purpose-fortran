              program demo_commandline
                 use M_CLI,  only : unnamed, commandline, check_commandline
                 implicit none
                 integer                      :: i
                 character(len=255)           :: message ! for I/O error
                 character(len=:),allocatable :: readme  ! updated namelist
                 integer                      :: ios

              ! declare a namelist
                 real               :: x, y, z, point(3), p(3)
                 character(len=80)  :: title
                 logical            :: l, l_
                 equivalence       (point,p)
                 namelist /args/ x,y,z,point,p,title,l,l_

              ! Define the prototype
              !  o All parameters must be listed with a default value.
              !  o logicals should be specified with a value of F or T.
              !  o string values  must be double-quoted.
              !  o lists must be comma-delimited. No spaces allowed in lists.
              !  o all long names must be lowercase. An uppercase short name
              !    -A maps to variable A_
              !  o if variables are equivalenced only one should be used on
              !    the command line
                 character(len=*),parameter  :: cmd='&
                 & -x 1 -y 2 -z 3     &
                 & --point -1,-2,-3   &
                 & --title "my title" &
                 & -l F -L F'
                 ! reading in a NAMELIST definition defining the entire NAMELIST
                 ! now get the values from the command prototype and
                 ! command line as NAMELIST input
                 readme=commandline(cmd)
                 read(readme,nml=args,iostat=ios,iomsg=message)
                 call check_commandline(ios,message)
                 ! all done cracking the command line

                 ! use the values in your program.
                 write(*,nml=args)
                 ! the optional unnamed values on the command line are
                 ! accumulated in the character array "UNNAMED"
                 if(size(unnamed).gt.0)then
                    write(*,'(a)')'files:'
                    write(*,'(i6.6,3a)')(i,'[',unnamed(i),']',i=1,size(unnamed))
                 endif
              end program demo_commandline
