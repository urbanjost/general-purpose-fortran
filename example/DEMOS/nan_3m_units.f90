          program demo_nan
          use,intrinsic :: iso_fortran_env, only: real32, real64, real128
          use M_units, only : nan
          implicit none
          real(kind=real32)  :: r32
          real(kind=real64)  :: r64
          real(kind=real128) :: r128
          character(len=256) :: message
          integer            :: ios

             r32=nan(0.0_real32)
             r64=nan(0.0_real64)
             r128=nan(0.0_real128)

             ! examples printing the NaN values
             ! list directed format
             write(*,*,iomsg=message,iostat=ios)r32,r64,r128
             if(ios.ne.0)write(*,*)trim(message)
             ! hexadecimal format to show different kinds
             write(*,'(*(z0,1x))',iomsg=message,iostat=ios)r32,r64,r128
             if(ios.ne.0)write(*,*)trim(message)
             ! G0 format
             write(*,'(*(g0,1x))',iomsg=message,iostat=ios)r32,r64,r128
             if(ios.ne.0)write(*,*)trim(message)
             ! if a specific numeric field is used
             write(*,'(*(f3.1,1x))',iomsg=message,iostat=ios)r32,r64,r128
             if(ios.ne.0)write(*,*)trim(message)
             ! if format is less than three characters
             write(*,'(*(f2.1,1x))',iomsg=message,iostat=ios)r32,r64,r128
             if(ios.ne.0)write(*,*)trim(message)

             ! an option to terminate a program when a NaN is encountered
             ! (if X is NaN the comparison with 0. is always false.)
             if (.not.(r32<=0.0) .and. .not.(r32>=0.0))then
                write(*,*)'found nan'
                stop
             endif

             ALT1: block
                integer :: x = 2143289344
                print *, transfer(x, 1.0)    ! prints "nan" on i686
             endblock ALT1

              end program demo_nan
