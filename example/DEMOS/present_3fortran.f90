      program demo_present
      implicit none
      integer :: answer
         ! argument to func() is not present
         answer=func()
         write(*,*) answer
         ! argument to func() is present
         answer=func(1492)
         write(*,*) answer
      contains

      integer function func(x)
      ! the optional characteristic on this definition allows this variable
      ! to not be specified on a call; and also allows it to subsequently
      ! be passed to PRESENT(3):
      integer, intent(in), optional :: x
      integer :: x_local

        ! basic
         if(present(x))then
           ! if present, you can use x like any other variable.
           x_local=x
         else
           ! if not, you cannot define or reference x except to
           ! pass it as an optional parameter to another procedure
           ! or in a call to present(3f)
           x_local=0
         endif

         func=x_local**2

        ! passing the argument on to other procedures
         ! so something like this is a bad idea because x is used
         ! as the first argument to merge(3f) when it might not be
         ! present
         ! xlocal=merge(x,0,present(x)) ! NO!!

         ! We can pass it to another procedure if another
         ! procedure declares the argument as optional as well,
         ! or we have tested that X is present
         call tattle('optional argument x',x)
         if(present(x))call not_optional(x)
      end function

      subroutine tattle(label,arg)
      character(len=*),intent(in) :: label
      integer,intent(in),optional :: arg
         if(present(arg))then
            write(*,*)label,' is present'
         else
            write(*,*)label,' is not present'
         endif
      end subroutine tattle

      subroutine not_optional(arg)
      integer,intent(in) :: arg
         write(*,*)'already tested X is defined',arg
      end subroutine not_optional

      end program demo_present
