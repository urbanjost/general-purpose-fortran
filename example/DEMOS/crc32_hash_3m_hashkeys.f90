          program demo_crc32_hash
          use,intrinsic :: ISO_FORTRAN_ENV, only : int64
          use M_hashkeys, only : crc32_hash
          implicit none
          integer :: i
          integer(int64) :: crc
          character(*), parameter :: s = "The quick brown fox jumps over the lazy dog"
             ! string
             crc=crc32_hash(s)
             print "(Z8)", crc
             print "(i0)", crc
             ! character array
             print "(i0)", crc32_hash([ &
                     & 'T','h','e',' ',&
                     & 'q','u','i','c','k',' ',&
                     & 'b','r','o','w','n',' ',&
                     & 'f','o','x',' '])
             print "(i0)", crc32_hash([ &
                     & 'j','u','m','p','s',' ',&
                     & 'o','v','e','r',' ',&
                     & 't','h','e',' ',&
                     & 'l','a','z','y',' ',&
                     & 'd','o','g'],continue=.true.)
             ! numeric array
             print "(i0)", crc32_hash([(i,i=1,100)])
          end program demo_crc32_hash
