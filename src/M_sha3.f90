!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
module M_sha3
!>
!!##NAME
!!    M_sha3(3fm) - [M_sha3] a module implementing the SHA-3 hash function
!!##SYNOPSIS
!!
!!    use M_sha3, only : sha3
!!    use M_sha3, only : sha3_update
!!    use M_sha3, only : sha3_state
!!    use M_sha3, only : sha3_digest
!!    use M_sha3, only : sha3_hexdigest
!!    use M_sha3, only : sha3_file
!!    use M_sha3, only : sha3_auto_test
!!##DESCRIPTION
!!    This module implements the SHA-3 hash function, according to FIPS
!!    PUB 202, SHA-3 Standard: Permutation-Based Hash and Extendable-Output
!!    Functions, a NIST publication.
!!
!!    Originally based on routines from http://alcinoe.net/fortran.html
!!
!!    In this module, we focus on hashing strings of bytes (as opposed to
!!    strings of bits whose length is not a multiple of 8). We also focus
!!    on providing a fixed-length digest, rather than extendable output. For
!!    us, bytes mean integers of kind 1.
!!
!!    There are two ways of using the module:
!!
!!      - a functional form, in which the whole array of bytes to hash
!!        is passed to a function, which returns an array of bytes:
!!
!!           digest = sha3( buffer, d )
!!
!!        where d is an integer (default kind) that specifies the digest
!!        length in bits (so that 'digest' should have a size of d/8)
!!
!!      - a subroutine form, which is typically used like this:
!!
!!            type(sha3_state) :: S
!!            call sha3_update( S, buffer1, d )
!!            call sha3_update( S, buffer2 )
!!            ...
!!            call sha3_digest( S, digest )
!!        where you pass the data to hash little by little with
!!        'sha3_update', and finish the process with 'sha3_digest' (after
!!        you which can start anew with the same state)
!!
!!    According to the standard, the digest size d may be one of 224, 256,
!!    384, 512, which results in arrays of bytes of size 28, 32, 48 and
!!    64. These arrays of bytes can be converted into a hexadecimal string
!!    of length 56, 64, 96 and 128 by calling the 'sha3_hexdigest' function:
!!
!!         hd = sha3_hexdigest( digest )
!!
!!    If the data to hash is a string, one may convert it to an array of
!!    bytes or integer(kind=int8) using the transfer intrinsic:
!!
!!       buffer = transfer( string, buffer )
!!
!!    where size(buffer) = len(string)
!!
!!    The final routine exported by the module is sha3_auto_test(), which
!!    hashes some test vectors, as found on:
!!
!!       http://www.di-mgt.com.au/sha_testvectors.html
!!
!!    and some files in the directory 'test_vectors', for which
!!    the digest was found using the Python implementation from
!!
!!       https://github.com/gvanas/KeccakCodePackage.
!!
!!##EXAMPLE
!!
!!   Sample program
!!
!!    program demo_M_sha3
!!    use M_sha3
!!    implicit none
!!    character(len=128) :: fname, arg
!!       call get_command_argument( 1, arg )
!!       if ( arg(1:1) .eq. '-' ) then
!!          if ( trim(arg) .eq. '-a' ) then
!!             call sha3_auto_test()
!!          else
!!             call get_command_argument( 2, fname )
!!            select case(trim(arg))
!!            case('-224'); call sha3_file( 224, trim(fname) )
!!            case('-256'); call sha3_file( 256, trim(fname) )
!!            case('-384'); call sha3_file( 384, trim(fname) )
!!            case('-512'); call sha3_file( 512, trim(fname) )
!!            case default
!!                print *,'usage: "sha3 -a" or "sha3 (-224|-256|-384|-512) fname"'
!!            end select
!!          endif
!!       else
!!          print *, 'usage: "sha3 -a" or "sha3 (-224|-256|-384|-512) fname"'
!!          print *, 'usage: "sha3 -a" or "sha3 (-224|-256|-384|-512) fname"'
!!       endif
!! end program demo_M_sha3
use,intrinsic :: ISO_FORTRAN_ENV, only : int8,int16,int32,int64
use M_strings, only : setbits8
implicit none
private

! this is one set of parameters for Keccak (standard one for SHA-3)
! with this set of parameters, a lane is encoded with an integer(8) (64 bits)
integer, parameter :: LANE = 8
integer, parameter :: W    = 64
integer, parameter :: ELL  = 6

integer(kind=int64), dimension(5,5) :: sbuf

! pre-computed values of the RC parameter in function iota
integer(kind=int64),save,dimension(24) :: RC_C
!integer(kind=int64),parameter,dimension(24) :: RC_C = [ &
   !int(z'8000000000000000',kind=int64), &
   !int(z'4101000000000000',kind=int64), &
   !int(z'5101000000000001',kind=int64), &
   !int(z'0001000100000001',kind=int64), &
   !int(z'D101000000000000',kind=int64), &
   !int(z'8000000100000000',kind=int64), &
   !int(z'8101000100000001',kind=int64), &
   !int(z'9001000000000001',kind=int64), &
   !int(z'5100000000000000',kind=int64), &
   !int(z'1100000000000000',kind=int64), &
   !int(z'9001000100000000',kind=int64), &
   !int(z'5000000100000000',kind=int64), &
   !int(z'D101000100000000',kind=int64), &
   !int(z'D100000000000001',kind=int64), &
   !int(z'9101000000000001',kind=int64), &
   !int(z'C001000000000001',kind=int64), &
   !int(z'4001000000000001',kind=int64), &
   !int(z'0100000000000001',kind=int64), &
   !int(z'5001000000000000',kind=int64), &
   !int(z'5000000100000001',kind=int64), &
   !int(z'8101000100000001',kind=int64), &
   !int(z'0101000000000001',kind=int64), &
   !int(z'8000000100000000',kind=int64), &
   !int(z'1001000100000001',kind=int64) ]
   !transfer(z'8000000000000000',0_int64), &
   !transfer(z'4101000000000000',0_int64), &
   !transfer(z'5101000000000001',0_int64), &
   !transfer(z'0001000100000001',0_int64), &
   !transfer(z'D101000000000000',0_int64), &
   !transfer(z'8000000100000000',0_int64), &
   !transfer(z'8101000100000001',0_int64), &
   !transfer(z'9001000000000001',0_int64), &
   !transfer(z'5100000000000000',0_int64), &
   !transfer(z'1100000000000000',0_int64), &
   !transfer(z'9001000100000000',0_int64), &
   !transfer(z'5000000100000000',0_int64), &
   !transfer(z'D101000100000000',0_int64), &
   !transfer(z'D100000000000001',0_int64), &
   !transfer(z'9101000000000001',0_int64), &
   !transfer(z'C001000000000001',0_int64), &
   !transfer(z'4001000000000001',0_int64), &
   !transfer(z'0100000000000001',0_int64), &
   !transfer(z'5001000000000000',0_int64), &
   !transfer(z'5000000100000001',0_int64), &
   !transfer(z'8101000100000001',0_int64), &
   !transfer(z'0101000000000001',0_int64), &
   !transfer(z'8000000100000000',0_int64), &
   !transfer(z'1001000100000001',0_int64) ]

type sha3_state
   integer :: d ! size of digest in bits
   integer :: c ! capacity in bits
   integer :: r ! rate, in bits
   integer(kind=int64), dimension(5,5) :: S ! state
   integer(kind=int8), dimension(:), pointer :: buffer
   integer :: bufsize = -1 ! the number of bytes actually usable in buffer
end type sha3_state

public :: sha3, sha3_update, sha3_state, sha3_digest, sha3_hexdigest, sha3_file, sha3_auto_test

public test_suite_M_sha3
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
contains
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
subroutine sha3_file( d, fname, hdigest )
! hashes a file and either returns the sha3_hexdigest in a string, or display it to
! stdout, along with the file name. d is the digest size in bits (224,256,384,512)
  use M_system, only : system_stat
  integer,                    intent(in)  :: d
  character(len=*),           intent(in)  :: fname
  character(len=*), optional, intent(out) :: hdigest

  integer(kind=int8), dimension(d/8)              :: digest

  logical                                 :: fexist
  integer                                 :: fsize, i, j, nread, nrem
  type(sha3_state)                        :: S
  integer(kind=int8), dimension(:), allocatable   :: buffer
  integer(kind=int64), dimension(13)      :: values
  character(len=128)                      :: dg

  ! does this file exist? if yes, what is its size?
  inquire( file=trim(adjustl(fname)), exist=fexist )
  if ( .not. fexist ) then
     print *, 'file not found.'
     return
  endif
  call system_stat( trim(fname), values )
  fsize = int(values(8))

  ! read the file into a buffer with the appropriate size
  allocate( buffer(4096) )
  open( unit=39, file=trim(adjustl(fname)), form='unformatted', access='direct', recl=1 )
  nrem = fsize
  j    = 0
  do
     nread = min(nrem,4096)
     do i = 1, nread
        j = j + 1
        read( 39, rec=j ) buffer(i)
     enddo
     if ( nread == 4096 ) then
        call sha3_update( S, buffer, d )
     else
        call sha3_update( S, buffer(1:nread), d )
     endif
     nrem = nrem - nread
     if ( nrem <= 0 ) exit
  enddo
  close( 39 )

  call sha3_digest( S, digest )
  dg = sha3_hexdigest( digest )
  if ( present(hdigest) ) then
     hdigest = trim(dg)
  else
     print '(3a)', trim(dg), ' ', trim(fname)
  endif

  deallocate( buffer )

end subroutine sha3_file
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
function sha3_hexdigest( d )
! returns a digest d (a list of bytes) as an hexadecimal string

  integer(kind=int8), dimension(:), intent(in) :: d
  character(len=size(d)*2) :: sha3_hexdigest

  write( sha3_hexdigest, '(100Z2.2)' ) d

end function sha3_hexdigest
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
subroutine sha3_update( state, buffer, d )
! this routine

  type(sha3_state),         intent(inout) :: state
  integer(kind=int8), dimension(:), intent(in)    :: buffer
  integer, optional,        intent(in)    :: d

  integer, save :: r8
  integer       :: j

  if ( state%bufsize == -1 ) then
     ! means we never, ever called sha3_update before, and thus the buffer pointer
     ! in state is in limbo
     nullify( state%buffer )
  endif

  if ( state%bufsize < 0 ) then
     ! means that we start working on a new input
     if ( present(d) ) then
        state%d = d
     else
        state%d = 224
     endif
     if ( state%d == 224 ) then
        state%c = 448
     elseif ( state%d == 256 ) then
        state%c = 512
     elseif ( state%d == 384 ) then
        state%c = 768
     elseif ( state%d == 512 ) then
        state%c = 1024
     else
        ! todo
     endif
     state%r = 25*W - state%c
     ! initialize state
     state%S = 0_int64
     allocate( state%buffer(state%r / 4 ) )
     state%bufsize = 0 ! buffer allocated, but empty
     r8 = state%r / 8
  endif

  ! in case there was data left in the *state* buffer from a previous call
  ! to sha3_update, we append the received data to it
  if ( state%bufsize > 0 ) then
     ! complete the state buffer
     j = min( size(buffer), r8 - state%bufsize ) ! how many bytes from buffer to use
     state%buffer( state%bufsize+1 : state%bufsize+j ) = buffer(1:j)
     state%bufsize = state%bufsize + j
     if ( state%bufsize >= r8 ) then
        call sha3_block( state%S, state%buffer(1:r8), r8 )
        state%bufsize = 0
        ! hash the remainder of the data (if any)
        do
           if ( j+r8 >= size(buffer) ) exit
           ! hash this block, w
           call sha3_block( state%S, buffer(j+1:j+r8), r8 )
           ! go to next input block
           j = j + r8
        enddo
     else
        return
     endif
  else
     ! hash what we can from buffer
     j = 0
     do
        if ( j+r8 >= size(buffer) ) exit
        ! hash this block, w
        call sha3_block( state%S, buffer(j+1:j+r8), r8 )
        ! go to next input block
        j = j + r8
     enddo
  endif

  ! add the remainder to state%buffer:
  ! just accumulate data, because this cannot be hashed without taking
  ! padding into account
  if ( state%bufsize + (size(buffer) - j) > size(state%buffer) ) then
     print *, 'error, buffer is too small ???'
  else
     state%buffer( state%bufsize+1 : state%bufsize+size(buffer)-j ) = buffer( j+1:size(buffer) )
     state%bufsize = state%bufsize + size(buffer) - j
     if ( state%bufsize < 0 ) print *, 'error, buffer size < 0'
  endif

  ! is buffer large enough to process a block ?
  if ( state%bufsize >= r8 ) then
     call sha3_block( state%S, state%buffer(1:r8), r8 )
     ! "resize" buffer
     state%buffer(1:state%bufsize-r8) = state%buffer(r8+1:state%bufsize)
     state%bufsize = state%bufsize - r8
  endif

end subroutine sha3_update
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
subroutine sha3_block( S, buffer, r8 )
! given a state matrix S, a full buffer of bytes (i.e., r8 bytes), this routine
! absorbs the content of buffer into the sponge.

  integer(kind=int64), dimension(5,5), intent(inout) :: S
  integer(kind=int8),    dimension(:),   intent(in)    :: buffer
  integer,                       intent(in)    :: r8

  integer :: i, k, a, b
  integer(kind=int8), dimension(LANE) :: bytes

  a = 1 ; b = 1
  do i = 1, r8 / LANE ! loop on each lane
     do k = 1, LANE ! revert the bytes in each lane
        bytes(9-k) = sha3_reverse( buffer((i-1)*8+k) )
     enddo
     ! XOR the message with state
     S(a,b) = ieor( S(a,b), transfer( bytes, S(a,b) ) )
     a = a + 1
     if ( a == 6 ) then
        a = 1 ; b = b + 1
     endif
  enddo

  ! apply the sha3_keccak_p function on the state
  do i = 2*ELL + 12 - (2*ELL+12), 2*ELL + 12 - 1
     call sha3_round( S, i )
  enddo

end subroutine sha3_block
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
subroutine sha3_digest( state, digest )

  type(sha3_state),         intent(inout) :: state
  integer(kind=int8), dimension(:), intent(out)   :: digest

  integer(kind=int8) :: bug
  integer :: i, j
  integer(kind=int8), dimension(25*LANE) :: string

  ! it remains to apply padding the the current buffer, add this to sponge
  ! apply keccak, and squeeze

  ! the problem may be that, depending on the size of the buffer, we may have
  ! one or two r-bits blocks after padding
  digest = 0_int8

  ! proceed to padding. in here, we know that bufsize is strictly less than r/8 bytes
  ! (contrary to the sha3 function)
  i = mod( state%bufsize + 1, state%r/8 ) ! how many bytes to add
  if ( i == 0 ) then
     ! just add one byte for padding, and we have a full block ready to hash

     !>>>>>>>>>>>>
     !*!GFORTRAN 8.3 BUG!*!state%buffer( state%r/8 ) = transfer(int(b'10000110',kind=int8),state%buffer(1))
     bug=setbits8('10000110')
     state%buffer( state%r/8 ) = transfer(bug,state%buffer(1))
     !<<<<<<<<<<<<

  else
     state%buffer( state%bufsize + 1 ) = transfer(int(b'00000110',kind=int8),state%buffer(1))
     state%buffer( state%bufsize + 2 : state%r/8 - 1 ) = 0_int8

     !>>>>>>>>>>>>
     !*!GFORTRAN 8.3 BUG!*!state%buffer( state%r/8 ) = transfer(int(b'10000000',kind=int8),state%buffer(1))
     bug=setbits8('10000000')
     state%buffer( state%r/8 ) = transfer(bug,state%buffer(1))
     !<<<<<<<<<<<<

  endif

  ! absorb this last block...
  call sha3_block( state%S, state%buffer(1:state%r/8), state%r/8 )

  ! ...and squeeze
  if ( state%d < state%r ) then
     ! go back from state matrix to string
     string = sha3_state2string2( state%S, 25*W/8 )
     digest = string(1:state%d/8)
     do i = 1, state%d/8
        digest(i) = sha3_reverse(digest(i))
     enddo
  else
     j = 0 ! number of bytes currently outputted
!!$     do
!!$        i = min( r/8, d/8 - j )
!!$        sha3_sponge(j+1:j+i) = S(1:i) ! get r bits from state
!!$        j = j + i ! update the number of bytes outputted
!!$        ! exit when we have enough
!!$        if ( j >= d/8 ) exit
!!$        ! otherwise, continue squeezing
!!$        S = sha3_keccak_p( S, 25*W, 2*ELL+12 )
!!$     enddo
  endif

  ! once the digest has been provide, there are some tasks to perform
  ! (reinit the state and deallocation)
  deallocate( state%buffer )
  nullify( state%buffer )
  state%bufsize = -1

end subroutine sha3_digest
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
function sha3( buffer, d )

! SHA3 can produce variable-length digests, having length d in bits

! we assume that d is a multiple of 8

  integer(kind=int8), dimension(:), intent(in) :: buffer
  integer,                  intent(in) :: d ! output length
  integer(kind=int8), dimension(d/8) :: sha3

  select case ( d )
     case ( 224 ) ! SHA3 224
        sha3 = sha3_keccak( buffer, 224, 448 )
     case ( 256 ) ! SHA3 256
        sha3 = sha3_keccak( buffer, 256, 512 )
     case ( 384 ) ! SHA3 384
        sha3 = sha3_keccak( buffer, 384, 768 )
     case ( 512 ) ! SHA3 512
        sha3 = sha3_keccak( buffer, 512, 1024 )
     case default
        if ( d > 0 ) then
           sha3 = sha3_keccak( buffer, d, 256 )
        else
           sha3 = sha3_keccak( buffer, -d, 512 )
        endif
  end select

end function sha3
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
function sha3_keccak( M, d, c )

  integer(kind=int8), dimension(:), intent(in) :: M
  integer,                  intent(in) :: d ! output length of digest
  integer,                  intent(in) :: c ! capacity (distinguishes variants of K)
  integer(kind=int8), dimension(d/8) :: sha3_keccak

  ! here, M should have been padded with '1111' in XOF mode, '01' otherwise
  sha3_keccak = sha3_sponge( M, d, 25*W - c )

end function sha3_keccak
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
function sha3_sponge( M, d, r )

  integer(kind=int8), dimension(:), intent(in) :: M
  integer,                  intent(in) :: d ! output length of digest
  integer,                  intent(in) :: r ! rate
  integer(kind=int8), dimension(d/8) :: sha3_sponge

  integer :: i, c, n, j, k

  integer(kind=int8), dimension(25*LANE) :: S        ! state, as a string
  integer(kind=int8), dimension(r/8)     :: padding

  ! capacity is b - rate
  c = 25*W - r
  n = 0

  ! 0. PADDING------------------------------------------------------
  ! our goal is to determine 'padding', which is an array of r/8 bytes
  ! that contains the end of the message M plus the required padding
  if ( d == 224 .or. d == 256 .or. d == 384 .or. d == 512 ) then
     ! classic hashing: append '01', plus Pad10*1, such that message
     ! length (in bits) is a multiple of r, or rather, for us, such that
     ! message length in bytes is a multiple of r/8
     i = mod( size(M) + 1, r/8 ) ! how many bytes to add
     if ( i > 0 ) i = r/8 - i
     if ( i == 0 ) then
        ! it is ok to add just one byte
        do j = 1, r/8-1
           padding(j) = sha3_reverse( M(size(M)-(r/8-1)+j) )
        enddo
        padding(r/8) = transfer(int(b'01100001',kind=int8),padding(1))
        n = (size(M) - (r/8-1)) / (r/8)
     else
        padding = 0_int8
        do j = 1, r/8-1-i
           padding(j) = sha3_reverse( M( size(M)-(r/8-1-i)+j ) )
        enddo
        padding(r/8-i) = transfer(int(b'01100000',kind=int8),padding(1))
        padding(r/8)   = transfer(int(b'00000001',kind=int8),padding(1))
        n = (size(M) - (r/8-1-i)) / (r/8)
     endif
  else
     ! XOF mode: append '1111', plus Pad10*1
     !TODO
  endif

  ! n is the number of r-bits = r/8 bytes blocks in the message that are
  ! not affected by padding. For short messages, n = 0, because the message
  ! *with* padding fits in a single r-bits block (block "padding")

  j = 0      ! indices the sub-block of M that is treated
  S = 0_int8 ! state starts initially full of 0

  if ( n == 0 ) then ! message is sufficiently short to be fully inside padding
     ! initial XOR'd state
     do k = 1, r/8
        S(k) = ieor( S(k), padding(k) )
     enddo
  else
     ! 1. ABSORBING----------------------------------------------------
     do i = 1, n
        ! xor S and the next block of input to hash (byte by byte)
        do k = 1, r/8
           S(k) = ieor( S(k), sha3_reverse( M(j+k) ) )
        enddo
        ! for the remainder of S, it is xor'd with 0, i.e., unchanged
        j = j + r/8
        S = sha3_keccak_p( S, 25*W, 2*ELL+12 )
     enddo
     ! the last block has in general been padded (this last block may be the first!!)
     do k = 1, r/8
        S(k) = ieor( S(k), padding(k) )
     enddo
  endif

  ! this is the last
  S = sha3_keccak_p( S, 25*W, 2*ELL+12 )

  ! 2. SQUEEZING---------------------------------------------------
  if ( d < r ) then
     sha3_sponge = S(1:d/8)
  else
     j = 0 ! number of bytes currently outputted
     do
        i = min( r/8, d/8 - j )
        sha3_sponge(j+1:j+i) = S(1:i) ! get r bits from state
        j = j + i ! update the number of bytes outputted
        ! exit when we have enough
        if ( j >= d/8 ) exit
        ! otherwise, continue squeezing
        S = sha3_keccak_p( S, 25*W, 2*ELL+12 )
     enddo
  endif

  ! reverse the bytes we output
  do i = 1, d/8
     sha3_sponge(i) = sha3_reverse( sha3_sponge(i) )
  enddo

  !print '(a,100(z2.2))', 'sponge = ', sha3_sponge

end function sha3_sponge
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
function sha3_keccak_p( S, b, nr )

  integer(kind=int8), dimension(:), intent(in) :: S  ! input "string"
  integer,                  intent(in) :: b  ! size of input, in bits
  integer,                  intent(in) :: nr ! number of rounds
  integer(kind=int8), dimension(b/8) :: sha3_keccak_p

  integer(kind=int64), dimension(5,5) :: state
  integer :: ir

  ! convert S to state
  state = sha3_string2state( S )

  ! perform rounds
  do ir = 2*ELL + 12 - nr, 2*ELL + 12 - 1
     call sha3_round( state, ir )
  enddo

  ! convert from state to string
  sha3_keccak_p = sha3_state2string2( state, b/8 )

end function sha3_keccak_p
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
subroutine sha3_round( state, round_index )

  integer(kind=int64), dimension(5,5), intent(inout) :: state
  integer,                       intent(in)    :: round_index
  logical,save :: initialized=.false.

  if(.not.initialized)then
     initialized=.true.
     INITIALIZE: block
     integer                     :: i
     character(len=16),parameter :: strings(*)=[ &
      '8000000000000000', '4101000000000000', '5101000000000001', '0001000100000001', 'D101000000000000', '8000000100000000', &
      '8101000100000001', '9001000000000001', '5100000000000000', '1100000000000000', '9001000100000000', '5000000100000000', &
      'D101000100000000', 'D100000000000001', '9101000000000001', 'C001000000000001', '4001000000000001', '0100000000000001', &
      '5001000000000000', '5000000100000001', '8101000100000001', '0101000000000001', '8000000100000000', '1001000100000001' ]
     character(len=16)           :: readme
        do i=1,size(RC_C)
          readme=strings(i)
          read(readme,'(z16)') RC_C(i)
        enddo
     end block INITIALIZE
  endif
  ! the five steps of a round are made of the theta, rho, pi, khi and iota steps

  call sha3_theta( state )
  call sha3_rho( state )
  call sha3_pi( state )
  call sha3_khi( state )
  ! iota is simple, no need to call a function for that
  state(1,1) = ieor( state(1,1), RC_C(round_index+1) )

end subroutine sha3_round
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
subroutine sha3_theta( A )

  integer(kind=int64), dimension(5,5), intent(inout)  :: A

  integer(kind=int64), dimension(5) :: C, D
  integer :: x, y

  do x = 1, 5
     C(x) = ieor( A(x,1), ieor( A(x,2), ieor( A(x,3), ieor( A(x,4), A(x,5) ) ) ) )
  enddo

  D(1) = ieor( C(5), ishftc( C(2), -1 ) )
  D(2) = ieor( C(1), ishftc( C(3), -1 ) )
  D(3) = ieor( C(2), ishftc( C(4), -1 ) )
  D(4) = ieor( C(3), ishftc( C(5), -1 ) )
  D(5) = ieor( C(4), ishftc( C(1), -1 ) )

  do y = 1, 5
     do x = 1, 5
        A(x,y) = ieor( A(x,y), D(x) )
     enddo
  enddo

end subroutine sha3_theta
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
subroutine sha3_rho( A )

  integer(kind=int64), dimension(5,5), intent(inout)  :: A

  integer :: x, y, z, t

  x = 1 ; y = 0
  do t = 0, 23
     z = (t+1)*(t+2)/2
     A(x+1,y+1) = ishftc( A(x+1,y+1), -mod( z, 64 ) )
     z = y
     y = mod( 2*x + 3*y, 5 )
     x = z
  enddo

end subroutine sha3_rho
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
subroutine sha3_pi( A )

  integer(kind=int64), dimension(5,5), intent(inout)  :: A

  integer(kind=int64) :: t

  t = A(4,4)
  A(4,4) = A(3,4)
  A(3,4) = A(2,3)
  A(2,3) = A(3,2)
  A(3,2) = A(1,3)
  A(1,3) = A(2,1)
  A(2,1) = A(2,2)
  A(2,2) = A(5,2)
  A(5,2) = A(3,5)
  A(3,5) = A(5,3)
  A(5,3) = A(1,5)
  A(1,5) = A(3,1)
  A(3,1) = A(3,3)
  A(3,3) = A(4,3)
  A(4,3) = A(5,4)
  A(5,4) = A(4,5)
  A(4,5) = A(1,4)
  A(1,4) = A(5,1)
  A(5,1) = A(5,5)
  A(5,5) = A(2,5)
  A(2,5) = A(4,2)
  A(4,2) = A(2,4)
  A(2,4) = A(1,2)
  A(1,2) = A(4,1)
  A(4,1) = t

end subroutine sha3_pi
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
subroutine sha3_khi( A )

  integer(kind=int64), dimension(5,5), intent(inout)  :: A

  integer :: x, y, x1, x2

  sbuf = A

  do x = 1, 5
     x1 = x + 1
     if ( x == 5 ) x1 = 1
     x2 = x + 2
     if ( x2 > 5 ) x2 = x2 - 5
     do y = 1, 5
        A(x,y) = ieor( sbuf(x,y), iand( not( sbuf(x1,y) ), sbuf(x2,y) ) )
     enddo
  enddo

end subroutine sha3_khi
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
function sha3_reverse(b)

! reverses the order of the bits in byte b

  integer(kind=int8), intent(in) :: b
  integer(kind=int8) :: sha3_reverse

!*!<<<<<<<<<<<<<<<<<<<
!*!  BUG: GNU Fortran (GCC) 8.3.1 20191121 (Red Hat 8.3.1-5)
!*!  integer(kind=int8),parameter :: Z0F=int(z'0F',kind=int8), &
!*!                          Z33=int(z'33',kind=int8), &
!*!                          Z55=int(z'55',kind=int8), &
!*!                          ZAA=int(z'AA',kind=int8), &
!*!                          ZCC=int(z'CC',kind=int8), &
!*!                          ZF0=int(z'F0',kind=int8)
!*!===================
logical,save :: setup=.false.
character(len=2) :: num

integer(kind=int8),save :: Z0F, Z33, Z55, ZAA, ZCC, ZF0
   if(.not.setup)then
      num='0F'; read(num,'(z2)')Z0F
      num='33'; read(num,'(z2)')Z33
      num='55'; read(num,'(z2)')Z55
      num='AA'; read(num,'(z2)')ZAA
      num='CC'; read(num,'(z2)')ZCC
      num='F0'; read(num,'(z2)')ZF0
      setup=.true.
   endif
!*!>>>>>>>>>>>>>>>>>>>

  sha3_reverse = ior( ishft( iand( b, zF0 ), -4 ), ishft( iand( b, z0F ), 4 ) )
  sha3_reverse = ior( ishft( iand( sha3_reverse, zCC ), -2 ), ishft( iand( sha3_reverse, z33 ), 2 ) )
  sha3_reverse = ior( ishft( iand( sha3_reverse, zAA ), -1 ), ishft( iand( sha3_reverse, z55 ), 1 ) )

end function sha3_reverse
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
function sha3_string2state( S )

! an input string is (in principle) as string of bits of length b, but always
! encoded as an array of bytes (b/8 bytes)
! w/8 consecutive bytes form a lane, and lanes are stored in a state matrix
! in the order  A(1,1)  A(2,1)  A(3,1)  A(4,1)  A(5,1)   A(1,2) ... A(5,5)

  integer(kind=int8), dimension(:), intent(in) :: S  ! input "string" as a list of bytes
  integer(kind=int64), dimension(5,5) :: sha3_string2state

  integer(kind=int8), dimension(8) :: reve
  integer :: x, y, z, i

  z = 0
  do y = 1, 5
     do x = 1, 5
        do i = 1, 8
           reve(9-i) = S(z+i)
        enddo
        sha3_string2state(x,y) = transfer( reve, sha3_string2state(x,y) )
        z = z + LANE
     enddo
  enddo

end function sha3_string2state
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
function sha3_state2string2( S, sz )

! convert a state S to a string (array) of sz bytes

  integer,                       intent(in) :: sz
  integer(kind=int64), dimension(5,5), intent(in) :: S
  integer(kind=int8), dimension(sz) :: sha3_state2string2  ! input "string" as a list of bytes

  integer(kind=int8), dimension(8) :: bytes
  integer :: x, y, z, i

  ! convert S to state
  z = LANE + 1
  do y = 1, 5
     do x = 1, 5
        bytes(1:8) = transfer( S(x,y), bytes(1:8) )
        do i = 1, 8
           sha3_state2string2(z-i) = bytes(i)
        enddo
        z = z + LANE
     enddo
  enddo

end function sha3_state2string2
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
subroutine sha3_auto_test()

  !call sha3_test11()
  call sha3_test21()
  call sha3_test31()
  call sha3_test41()
  call sha3_test51()
  call sha3_test61()
contains
!================================================================================
subroutine sha3_test11()
!================================================================================
  integer(kind=int8), dimension(512/8) :: digest
  integer(kind=int8), dimension(:), allocatable :: buffer
  type(sha3_state) :: S

  print *
  print *, 'TEST11  : hash empty string'
  print '(a,a128)', '         ', sha3_hexdigest( sha3(buffer,512))
  allocate( buffer(0) )
  call sha3_update( S, buffer, 512 )
  call sha3_digest( S, digest )
  print '(a,a128)', '         ',sha3_hexdigest( digest )
  print '(a,2a128)', '         A69F73CCA23A9AC5C8B567DC185A756E97C982164FE25859E0D1DCC1475C80', &
       'A615B2123AF1F5F94C11E3E9402C3AC558F500199D95B6D3E301758586281DCD26'
end subroutine sha3_test11
!================================================================================
subroutine sha3_test21()
!================================================================================
  character(len=1024) :: m
  integer(kind=int8), dimension(224/8) :: digest
  integer(kind=int8), dimension(:), allocatable :: buffer
  type(sha3_state) :: S

  print *
  print *, 'TEST21  : hash "abc"'
  m = 'abc'
  allocate( buffer(len_trim(m)) )
  buffer = transfer( trim(m), buffer )
  print *, '        ', sha3_hexdigest( sha3( buffer, 224 ) )
  call sha3_update( S, buffer, 224 )
  call sha3_digest( S, digest )
  print *, '        ', sha3_hexdigest( digest )
  print *, '        E642824C3F8CF24AD09234EE7D3C766FC9A3A5168D0C94AD73B46FDF'

  deallocate( buffer )

end subroutine sha3_test21
!================================================================================
subroutine sha3_test31()
!================================================================================
  character(len=1024) :: m
  integer(kind=int8), dimension(224/8) :: digest
  integer(kind=int8), dimension(:), allocatable :: buffer
  type(sha3_state) :: S

  print *
  print *, 'TEST31  : hash "abc...stu"'

  m = 'abcdefghbcdefghicdefghijdefghijkefghijklfghijklmghijklmnhijklmnoijklmnopjklmnopqklmnopqrlmnopqrsmnopqrstnopqrstu'
  allocate( buffer(len_trim(m)) )
  buffer = transfer( trim(m), buffer )
  print *, '        ', sha3_hexdigest( sha3( buffer, 224 ) )
  call sha3_update( S, buffer, 224 )
  call sha3_digest( S, digest )
  print *, '        ', sha3_hexdigest( digest )
  print *, '        543E6868E1666C1A643630DF77367AE5A62A85070A51C14CBF665CBC'

  deallocate( buffer )

end subroutine sha3_test31
!================================================================================
subroutine sha3_test41()
!================================================================================
  integer, parameter :: N = 1000*1000
  integer, parameter :: M = 100
  integer(kind=int8), dimension(224/8) :: digest
  integer(kind=int8), dimension(:), allocatable :: buffer
  type(sha3_state) :: S
  integer :: i, j
  real :: t1, t2, d1, d2, d3

  print *
  print *, 'TEST41  : hash "a"*',N

  allocate( buffer(N) )
  do i = 1, N
     buffer(i) = 97_int8
  enddo

  call cpu_time( t1 )
  call sha3_update( S, buffer, 224 )
  call sha3_digest( S, digest )
  call cpu_time( t2 )
  d1 = t2 - t1
  print *, '        ', sha3_hexdigest( digest )
  call cpu_time( t1 )
  digest = sha3( buffer, 224 )
  call cpu_time( t2 )
  d2 = t2 - t1
  ! now provide it in small packets
  call cpu_time( t1 )
  j = 0
  do i = 1, N/M
     call sha3_update( S, buffer(j+1:j+M) )
     j = j + M
  enddo
  call sha3_digest( S, digest )
  call cpu_time( t2 )
  d3 = t2 - t1
  print *, '        ', sha3_hexdigest( digest )
  print *, '        D69335B93325192E516A912E6D19A15CB51C6ED5C15243E7A7FD653C'
  deallocate( buffer )

  !print *, 'timings: ', d1, d2, d3

  !call sha3_file( 'sha3.f90', 224, digest )

end subroutine sha3_test41
!================================================================================
subroutine sha3_test51()
!================================================================================
  integer               :: i, j
  character(len=128)    :: digest, fname, fname2
  character(len=256)    :: line
  integer, dimension(4) :: dv, mds

  dv = (/ 224, 256, 384, 512 /)
  mds = (/ 56, 64, 96, 128 /)

  print *
  print *, 'TEST 51 : hash files and compare digests with reference'

  ! loop on test vectors
  do i = 1, 5
     write( fname2, '(a,i3.3,a)' ) 'test_vectors/test_', i, '.digests'
     open( unit=12, file=trim(fname2) )
     print *, '   file #', i
     ! loop on SHA3 variant
     do j = 1, 4
        write( fname, '(a,i3.3,a)' ) 'test_vectors/test_', i, '.msg'
        call sha3_file( dv(j), fname, digest )
        write( *, '(10x,i3,1x,a)' ) dv(j), trim(digest)
        read( 12, '(a)' ) line
        write( *, '(10x,a)' ) trim(line)
        print *
     enddo
     close( 12 )
     print *
  enddo

end subroutine sha3_test51
!================================================================================
subroutine sha3_test61()
!================================================================================
  integer, parameter :: N = 100*1024*1024
  integer(kind=int8), dimension(224/8) :: digest
  integer(kind=int8), dimension(:), allocatable :: buffer
  type(sha3_state) :: S
  integer :: i
  real :: t1, t2, d1

  print *
  print *, 'TEST61  : speed test (hash 100 MiB)'

  allocate( buffer(N) )
  do i = 1, N
     buffer(i) = 97_int8
  enddo

  call cpu_time( t1 )
  call sha3_update( S, buffer, 224 )
  call sha3_digest( S, digest )
  call cpu_time( t2 )
  d1 = t2 - t1
  print *, '        ', sha3_hexdigest( digest )

  print *, 'timings: ', d1, 's'
  deallocate( buffer )

end subroutine sha3_test61

end subroutine sha3_auto_test
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
subroutine test_suite_M_sha3()
use M_verify, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg
use M_verify, only : unit_check_level

!! setup
   call test_sha3()
   call test_sha3_auto_test()
   call test_sha3_digest()
   call test_sha3_file()
   call test_sha3_hexdigest()
   call test_sha3_update()
!! teardown
contains
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_sha3()

   call unit_check_start('sha3',msg='')
   !!call unit_check('sha3', 0.eq.0, 'checking',100)
   call unit_check_done('sha3',msg='')
end subroutine test_sha3
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_sha3_auto_test()

   call unit_check_start('sha3_auto_test',msg='')
   !!call unit_check('sha3_auto_test', 0.eq.0, 'checking',100)
   call unit_check_done('sha3_auto_test',msg='')
end subroutine test_sha3_auto_test
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_sha3_digest()

   call unit_check_start('sha3_digest',msg='')
   !!call unit_check('sha3_digest', 0.eq.0, 'checking',100)
   call unit_check_done('sha3_digest',msg='')
end subroutine test_sha3_digest
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_sha3_file()

   call unit_check_start('sha3_file',msg='')
   !!call unit_check('sha3_file', 0.eq.0, 'checking',100)
   call unit_check_done('sha3_file',msg='')
end subroutine test_sha3_file
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_sha3_hexdigest()

   call unit_check_start('sha3_hexdigest',msg='')
   !!call unit_check('sha3_hexdigest', 0.eq.0, 'checking',100)
   call unit_check_done('sha3_hexdigest',msg='')
end subroutine test_sha3_hexdigest
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_sha3_update()

   call unit_check_start('sha3_update',msg='')
   !!call unit_check('sha3_update', 0.eq.0, 'checking',100)
   call unit_check_done('sha3_update',msg='')
end subroutine test_sha3_update
!===================================================================================================================================
end subroutine test_suite_M_sha3
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
end module M_sha3
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
