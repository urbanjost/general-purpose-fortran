      program demo_num_images
      implicit none
      integer :: value[*]
      real    :: p[*]
      integer :: i

         value = this_image()
         sync all
         if (this_image() == 1) then
           do i = 1, num_images()
             write(*,'(2(a,i0))') 'value[', i, '] is ', value[i]
           end do
         endif

       ! The following code uses image 1 to read data and
       ! broadcast it to other images.
         if (this_image()==1) then
            p=1234.5678
            do i = 2, num_images()
               p[i] = p
            end do
         end if
         sync all

      end program demo_num_images
