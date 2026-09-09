     program demo_nullify
     implicit none
     real, pointer :: dart1 (:)
     real, pointer :: dart2 (:)
     real, allocatable, target :: island (:)

     allocate (island (7))
     island = 1.0
     island (1:7:2) = 10.0

     write (*,'(a,7f8.0)') 'target   ',island
     dart1 => island
     write (*,'(a,7f8.0)') 'pointer 1',dart1

     dart2 => dart1
     write (*,'(/a)') merge('dart2 is pointed    ',&
                            'dart2 is not pointed',associated(dart2))
     write (*,'(a,7f8.0)') 'pointer 2',dart2

     nullify (dart1)
     write (*,'(/a)') merge('dart1 is pointed    ',&
                            'dart1 is not pointed',associated(dart1))

     ! so if dart2 pointed to dart1 and dart1 is nullified can you test dart2?
     write (*,'(/a)') merge('dart2 is pointed    ',&
                            'dart2 is not pointed',associated(dart2))

     write (*,'( a,7f8.0)') 'pointer 2',dart2

     end program demo_nullify
