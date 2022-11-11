        program demo_is_iostat_eor
        use iso_fortran_env, only : iostat_eor
        implicit none
        integer :: inums(50), lun, ios

          open(newunit=lun, file='_test.dat', form='unformatted')
          write(lun, '(a)') '10 20 30'
          write(lun, '(a)') '40 50 60 70'
          write(lun, '(a)') '80 90'
          write(lun, '(a)') '100'

          do
             read(lun, *, iostat=ios) inums
             write(*,*)'iostat=',ios
             if(is_iostat_eor(ios)) stop 'end of record'
          enddo

          close(lun,iostat=ios,status='delete')

        end program demo_is_iostat_eor
