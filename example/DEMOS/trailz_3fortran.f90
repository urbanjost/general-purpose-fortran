      program demo_trailz

      ! some common integer kinds
      use, intrinsic :: iso_fortran_env, only : &
       & integer_kinds, int8, int16, int32, int64

      implicit none

      ! a handy format
      character(len=*),parameter :: &
       & show = '(1x,"value=",i4,", value(bits)=",b32.32,1x,", trailz=",i3)'

      integer(kind=int64) :: bigi
        ! basics
         write(*,*)'Note default integer is',bit_size(0),'bits'
         print  show,  -1, -1,  trailz(-1)
         print  show,   0,  0,  trailz(0)
         print  show,   1,  1,  trailz(1)
         print  show,  96, 96,  trailz(96)
        ! elemental
         print *, 'elemental and any integer kind:'
         bigi=2**5
         write(*,*) trailz( [ bigi, bigi*256, bigi/2 ] )
         write(*,'(1x,b64.64)')[ bigi, bigi*256, bigi/2 ]

      end program demo_trailz
