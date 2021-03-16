           program demo_associated
           implicit none
           real, target  :: tgt(2) = [1., 2.]
           real, pointer :: ptr(:)
              ptr => tgt
              if (associated(ptr)     .eqv. .false.) &
              & stop 'POINTER NOT ASSOCIATED'
              if (associated(ptr,tgt) .eqv. .false.) &
              & stop 'POINTER NOT ASSOCIATED TO TARGET'
           end program demo_associated
