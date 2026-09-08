      program demo_is_iostat_eor
      use iso_fortran_env, only : iostat_eor
      implicit none
      integer :: inums(5), lun, ios

        ! create a test file to read from
         open(newunit=lun, form='formatted',status='scratch',action='readwrite')
         write(lun, '(a)')     &
         '10   20   30',       &
         '40   50   60   70',  &
         '80   90',            &
         '100',                &
         '110 120 130',        &
         '140'
         rewind(lun)

         do
            read(lun, *, iostat=ios) inums
            write(*,*)'iostat=',ios
            if(is_iostat_eor(ios)) then
               inums=-huge(0)
               print *, 'end of record'
            elseif(is_iostat_end(ios)) then
               print *,'end of file'
               inums=-huge(0)
               exit
            elseif(ios.ne.0)then
               print *,'I/O error',ios
               inums=-huge(0)
               exit
            else
               write(*,'(*(g0,1x))')'inums=',inums
            endif
         enddo

         close(lun,iostat=ios,status='delete')

      end program demo_is_iostat_eor
