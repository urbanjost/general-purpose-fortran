          program demo_system_rmdir
          use M_system, only : system_perror
          use M_system, only : system_rmdir, system_mkdir
          use M_system, only : RWX_U
          implicit none
          integer :: ierr
          write(*,*)'BEFORE TRY TO CREATE _scratch/'
          call execute_command_line('ls -ld _scratch')

          write(*,*)'TRY TO CREATE _scratch/'
          ierr=system_mkdir('_scratch',RWX_U)
          write(*,*)'IERR=',ierr
          call execute_command_line('ls -ld _scratch')

          write(*,*)'TRY TO REMOVE _scratch/'
          ierr=system_rmdir('_scratch')
          write(*,*)'IERR=',ierr
          call execute_command_line('ls -ld _scratch')

          write(*,*)'TRY TO REMOVE _scratch when it should be gone/'
          ierr=system_rmdir('_scratch')
          call system_perror('*test of system_rmdir*')
          write(*,*)'IERR=',ierr
          call execute_command_line('ls -ld _scratch')

          end program demo_system_rmdir
