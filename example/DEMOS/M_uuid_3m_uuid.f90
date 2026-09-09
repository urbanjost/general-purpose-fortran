      program demo_M_uuid
      ! generate 36-character UUID string
      use M_uuid, only : generate_uuid
      implicit none
      character(len=36)   :: uuid
      character(len=4096) :: filename
         ! version 1 (time-based UUID)
         write(*,'(a36)') generate_uuid(version=1)
         ! version 4 (pseudo-RNG-based), default
         write(*,'(a36)') generate_uuid(version=4)
         ! RFC 4122 defines a UUID Uniform Resource Name (URN) namespace
         write(*,'("urn:uuid:",a36)') generate_uuid(version=4)
         ! a good scratch file name
         open(file='/tmp/scratch_'//generate_uuid(),unit=10)
         inquire(unit=10,name=filename)
         write(*,'(*(g0))') trim(filename)
         close(unit=10,status='delete')
      end program demo_M_uuid
