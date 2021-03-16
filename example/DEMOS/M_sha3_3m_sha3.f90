          program demo_M_sha3
          use M_sha3
          implicit none
          character(len=128) :: fname, arg
             call get_command_argument( 1, arg )
             if ( arg(1:1) .eq. '-' ) then
                if ( trim(arg) .eq. '-a' ) then
                   call sha3_auto_test()
                else
                   call get_command_argument( 2, fname )
                  select case(trim(arg))
                  case('-224'); call sha3_file( 224, trim(fname) )
                  case('-256'); call sha3_file( 256, trim(fname) )
                  case('-384'); call sha3_file( 384, trim(fname) )
                  case('-512'); call sha3_file( 512, trim(fname) )
                  case default
                      print *,'usage: "sha3 -a" or "sha3 (-224|-256|-384|-512) fname"'
                  end select
                endif
             else
                print *, 'usage: "sha3 -a" or "sha3 (-224|-256|-384|-512) fname"'
                print *, 'usage: "sha3 -a" or "sha3 (-224|-256|-384|-512) fname"'
             endif
       end program demo_M_sha3
