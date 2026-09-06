      program demo_this_image
      implicit none
      integer :: value[*]
      integer :: i
         value = this_image()
         sync all
         if (this_image() == 1) then
            do i = 1, num_images()
               write(*,'(2(a,i0))') 'value[', i, '] is ', value[i]
            end do
         endif
      end program demo_this_image
