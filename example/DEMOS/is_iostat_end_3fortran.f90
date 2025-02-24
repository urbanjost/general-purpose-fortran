      program demo_iostat
      implicit none
      integer,parameter  :: wp=kind(0.0d0)
      real(kind=wp)      :: value
      integer            :: iostat
      integer            :: lun
      character(len=256) :: message
         ! make a scratch input file for demonstration purposes
         call makefile(lun)
         write(*,*)'Begin entering numeric values, one per line'
         do
            read(lun,*,iostat=iostat,iomsg=message)value
            if(iostat.eq.0)then
               write(*,*)'VALUE=',value
            elseif( is_iostat_end(iostat) ) then
               stop 'end of file. Goodbye!'
            else
               write(*,*)'ERROR:',iostat,trim(message)
               exit
            endif
            !
         enddo
      contains
      subroutine makefile(lun)
      ! make a scratch file just for demonstration purposes
      integer :: lun
      integer :: i
      character(len=255),parameter  :: fakefile(*)=[character(len=255) :: &

      '3.141592653589793238462643383279502884197169399375105820974944592307 &
       &/ pi', &

      '0.577215664901532860606512090082402431042 &
       &/ The Euler-Mascheroni constant (Gamma)', &

      '2.71828182845904523536028747135266249775724709369995 &
       &/ Napier''s constant "e"&
       & is the base of the natural logarithm system,&
       & named in honor of Euler ', &

      '1.6180339887498948482045868 &
       &/ Golden_Ratio', &

      '1 / unity', &
      '']
      !'/ end of data']

         open(newunit=lun,status='replace',file='data.txt',action='readwrite')
         write(lun,'(a)')(trim(fakefile(i)),i=1,size(fakefile))
         rewind(lun)
      end subroutine makefile
      end program demo_iostat
