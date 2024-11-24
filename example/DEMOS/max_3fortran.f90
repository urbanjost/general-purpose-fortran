      program demo_max
      implicit none
      real :: arr1(4)= [10.0,11.0,30.0,-100.0]
      real :: arr2(5)= [20.0,21.0,32.0,-200.0,2200.0]
      integer :: box(3,4)= reshape([-6,-5,-4,-3,-2,-1,1,2,3,4,5,6],shape(box))

        ! basic usage
         ! this is simple enough when all arguments are scalar

         ! the most positive value is returned, not the one with the
         ! largest magnitude
         write(*,*)'scalars:',max(10.0,11.0,30.0,-100.0)
         write(*,*)'scalars:',max(-22222.0,-0.0001)

         ! strings do not need to be of the same length
         write(*,*)'characters:',max('the','words','order')

         ! leading spaces are significant; everyone is padded on the right
         ! to the length of the longest argument
         write(*,*)'characters:',max('c','bb','a')
         write(*,*)'characters:',max(' c','b','a')

        ! elemental
         ! there must be at least two arguments, so even if A1 is an array
         ! max(A1) is not valid. See MAXVAL(3) and/or MAXLOC(3) instead.

         ! strings in a single array do need to be of the same length
         ! but the different objects can still be of different lengths.
         write(*,"(*('""',a,'""':,1x))")MAX(['A','Z'],['BB','Y '])
         ! note the result is now an array with the max of every element
         ! position, as can be illustrated numerically as well:
         write(*,'(a,*(i3,1x))')'box=   ',box
         write(*,'(a,*(i3,1x))')'box**2=',sign(1,box)*box**2
         write(*,'(a,*(i3,1x))')'max    ',max(box,sign(1,box)*box**2)

         ! Remember if any argument is an array by the definition of an
         ! elemental function all the array arguments must be the same shape.

         ! to find the single largest value of multiple arrays you could
         ! use something like
         !    MAXVAL([arr1, arr2])
         ! or probably better (more likely to avoid creating a large temp array)
         !    max(maxval(arr1),maxval(arr2))
         ! instead

         ! so this returns an array of the same shape as any input array
         ! where each result is the maximum that occurs at that position.
         write(*,*)max(arr1,arr2(1:4))
         ! this returns an array just like BOX  except all values less than
         ! zero are set to zero:
         write(*,*)max(box,0)
         ! When mixing arrays and scalars you can think of the scalars
         ! as being a copy of one of the arrays with all values set to
         ! the scalar value.

      end program demo_max
