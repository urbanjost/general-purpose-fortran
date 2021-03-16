          program demo_generate_uuid
          use M_uuid, only : generate_uuid
          implicit none
          character(len=36) :: uuid
             !
             uuid=generate_uuid(1)  ! version 1 (time-based UUID)
             write(*,'(a36)')uuid
             !
             uuid=generate_uuid(4)  ! version 4 (pseudo-RNG-based), default
             !
             ! RFC 4122 defines a Uniform Resource Name (URN) namespace for UUIDs.
             write(*,'("urn:uuid:",a36)')uuid
             !
             ! a good scratch file name
             open(file='/tmp/scratch_'//uuid,unit=10)
             !
          end program demo_generate_uuid
