      program demo_mvbits
      use,intrinsic :: iso_fortran_env,  only : int8, int16, int32, int64
      implicit none
      integer(kind=int32) :: intfrom, intto, abcd_int
      character(len=*),parameter :: bits= '(g0,t30,b32.32)'
      character(len=*),parameter :: fmt= '(g0,t30,a,t40,b32.32)'

          intfrom=huge(0)  ! all bits are 1 accept the sign bit
          intto=0          ! all bits are 0

          !! CHANGE BIT 0
          ! show the value and bit pattern
          write(*,bits)intfrom,intfrom
          write(*,bits)intto,intto

          ! copy bit 0 from intfrom to intto to show the rightmost bit changes
          !          (from,    frompos, len,    to, topos)
          call mvbits(intfrom,       0,   1, intto,     0) ! change bit 0
          write(*,bits)intto,intto

          !! COPY PART OF A VALUE TO ITSELF
          ! can copy bit from a value to itself
          call mvbits(intfrom,0,1,intfrom,31)
          write(*,bits)intfrom,intfrom

          !! MOVING BYTES AT A TIME
          ! make native integer value with bit patterns
          ! that happen to be the same as the beginning of the alphabet
          ! to make it easy to see the bytes are reversed
          abcd_int=transfer('abcd',0)
          ! show the value and bit pattern
          write(*,*)'native'
          write(*,fmt)abcd_int,abcd_int,abcd_int

          ! change endian of the value
          abcd_int=int_swap32(abcd_int)
          ! show the values and their bit pattern
          write(*,*)'non-native'
          write(*,fmt)abcd_int,abcd_int,abcd_int

       contains

       pure elemental function int_swap32(intin) result(intout)
       ! Convert a 32 bit integer from big Endian to little Endian,
       ! or conversely from little Endian to big Endian.
       !
       integer(kind=int32), intent(in)  :: intin
       integer(kind=int32) :: intout
          ! copy bytes from input value to new position in output value
          !          (from,  frompos, len,     to, topos)
          call mvbits(intin,       0,   8, intout,    24) ! byte1 to byte4
          call mvbits(intin,       8,   8, intout,    16) ! byte2 to byte3
          call mvbits(intin,      16,   8, intout,     8) ! byte3 to byte2
          call mvbits(intin,      24,   8, intout,     0) ! byte4 to byte1
       end function int_swap32

       end program demo_mvbits
