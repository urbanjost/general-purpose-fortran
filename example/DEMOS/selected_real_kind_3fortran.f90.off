      program demo_selected_real_kind
      use, intrinsic :: iso_fortran_env
      implicit none
      integer,parameter :: p6 = selected_real_kind(6)
      integer,parameter :: p10r100 = selected_real_kind(10,100)
      integer,parameter :: r400 = selected_real_kind(r=400)
      real(kind=p6) :: x
      real(kind=p10r100) :: y
      real(kind=r400) :: z

         write(*,*) 'real_kinds    =', real_kinds(:)
         write(*,*) 'real constants=', &
         & real16, real32, real64, real128 !, bfloat16
         write(*,*) 'integer_kinds=', integer_kinds(:)
         write(*,*) 'int constants=', &
         & int8, int16, int32, int64  !, int128

         print *, precision(x), range(x)
         print *, precision(y), range(y)
         print *, precision(z), range(z)
      end program demo_selected_real_kind
