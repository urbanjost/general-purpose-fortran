       program demo_generate_uuid
       ! generate 36-character UUID string
       use M_uuid, only : generate_uuid
       implicit none
       character(len=36)   :: uuid
       character(len=4096) :: filename
          !
          ! version 1 (time-based UUID)
          uuid=generate_uuid(version=1)
          write(*,'(a36)')uuid
          !
          ! version 4 (pseudo-RNG-based), default
          uuid=generate_uuid(version=4)
          write(*,'(a36)')uuid
          !
          ! RFC 4122 defines a Uniform Resource Name (URN) namespace for UUIDs.
          write(*,'("urn:uuid:",a36)')uuid
          !
          ! a good scratch file name
          open(file='/tmp/scratch_'//uuid,unit=10)
          inquire(unit=10,name=filename)
          write(*,'(*(g0))') trim(filename)
          close(unit=10,status='delete')
       end program demo_generate_uuid
