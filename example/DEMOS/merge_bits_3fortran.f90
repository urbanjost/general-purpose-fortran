      program demo_merge_bits
      use,intrinsic :: iso_fortran_env,  only : int8, int16, int32, int64
      implicit none
      integer(kind=int16) :: if_one,if_zero,msk
      character(len=*),parameter :: fmt='(*(g0, 1X))'

         ! basic usage
         print *,'MERGE_BITS( 5,10,41) should be 3.=>',merge_bits(5,10,41)
         print *,'MERGE_BITS(13,18,22) should be 4.=>',merge_bits(13,18,22)

         ! use some values in base2 illustratively:
         if_one =int(b'1010101010101010',kind=int16)
         if_zero=int(b'0101010101010101',kind=int16)

         msk=int(b'0101010101010101',kind=int16)
         print '("should get all zero bits =>",b16.16)', &
         & merge_bits(if_one,if_zero,msk)

         msk=int(b'1010101010101010',kind=int16)
         print '("should get all ones bits =>",b16.16)', &
         & merge_bits(if_one,if_zero,msk)

         ! using BOZ values
         print fmt, &
         & merge_bits(32767_int16,    o'12345',         32767_int16), &
         & merge_bits(o'12345', 32767_int16, b'0000000000010101'), &
         & merge_bits(32767_int16,    o'12345',             z'1234')

         ! a do-it-yourself equivalent for comparison and validation
         print fmt, &
         & ior(iand(32767_int16, 32767_int16),                   &
         &   iand(o'12345', not(32767_int16))),                  &

         & ior(iand(o'12345', int(o'12345', kind=int16)),        &
         &   iand(32767_int16, not(int(o'12345', kind=int16)))), &

         & ior(iand(32767_int16, z'1234'),                       &
         &   iand(o'12345', not(int( z'1234', kind=int16))))

      end program demo_merge_bits
