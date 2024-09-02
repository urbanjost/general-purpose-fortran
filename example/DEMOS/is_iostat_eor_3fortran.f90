      program demo_is_iostat_eor
      use iso_fortran_env, only : iostat_eor
      implicit none
      integer :: inums(5), lun, ios

        ! create a test file to read from
         open(newunit=lun, form='formatted',status='scratch')
         write(lun, '(a)') '10 20 30'
         write(lun, '(a)') '40 50 60 70'
         write(lun, '(a)') '80 90'
         write(lun, '(a)') '100'
         rewind(lun)

         do
            read(lun, *, iostat=ios) inums
            write(*,*)'iostat=',ios
            if(is_iostat_eor(ios)) then
               stop 'end of record'
            elseif(is_iostat_end(ios)) then
               print *,'end of file'
               exit
            elseif(ios.ne.0)then
               print *,'I/O error',ios
               exit
            endif
         enddo

         close(lun,iostat=ios,status='delete')

      end program demo_is_iostat_eor
