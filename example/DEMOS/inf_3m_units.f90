       program demo_inf
       use,intrinsic :: iso_fortran_env, only: real32, real64, real128
       use M_units, only : inf
       implicit none
       real(kind=real32)  :: r32
       real(kind=real64)  :: r64
       real(kind=real128) :: r128
       character(len=256) :: message
       integer            :: ios
          r32=inf(0.0_real32)
          r64=inf(0.0_real64)
          r128=inf(0.0_real128)
          write(*,*,iomsg=message,iostat=ios)r32,r64,r128
          if(ios.ne.0)write(*,*)trim(message)
          write(*,'(z0)',iomsg=message,iostat=ios)r32,r64,r128
          if(ios.ne.0)write(*,*)trim(message)
          write(*,'(g0)',iomsg=message,iostat=ios)r32,r64,r128
          if(ios.ne.0)write(*,*)trim(message)
          write(*,'(f3.1)',iomsg=message,iostat=ios)r32,r64,r128
          if(ios.ne.0)write(*,*)trim(message)
          write(*,'(f2.1)',iomsg=message,iostat=ios)r32,r64,r128
          if(ios.ne.0)write(*,*)trim(message)
       end program demo_inf
