          program demo_bit_size
          use,intrinsic :: iso_fortran_env, only : int8, int16, int32, int64
          implicit none
          integer(kind=int64)          :: answer
          integer                      :: ilen
             write(*,'(i0)')bit_size(bit_size(0_int8))
             write(*,'(i0)')bit_size(bit_size(0_int16))
             write(*,'(i0)')bit_size(bit_size(0_int32))
             write(*,'(i0)')bit_size(bit_size(0_int64))
             answer=0_int64
             ilen=999
             ! notice use of INT(3f)
             ilen=min(ilen,int(bit_size(answer)))
             ! arguments to MIN(3f) would be of different TYPES
             !ilen=min(ilen,bit_size(answer))
             write(*,'(i0)')ilen
          end program demo_bit_size
