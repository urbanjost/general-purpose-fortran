module module_luhn
use M_hashkeys__public, only : luhn_checksum
private
public test_luhn_checksum
contains
subroutine test_luhn_checksum()
use M_strings, only : transliterate
use M_framework__verify, only : unit_test_start,unit_test,unit_test_done,unit_test_good,unit_test_bad,unit_test_msg
use M_framework__verify, only : unit_test_level
use M_framework__msg,   only : str
implicit none
   character(len=:),allocatable :: ccards(:), string, buff
   call unit_test_start('luhn_checksum',msg='')
   ! good values
   ccards=[ character(len=20) :: '79927398713', '49927398716', '123456-781234567-0', '4578 4230 1376 9219' ]
   call checkem(.true.)
   ! bad values
   ccards=[ character(len=20) :: &
      '79927398710','79927398711','79927398712','79927398714', &
      '79927398715','79927398716','79927398717','79927398718','79927398719', &
      '49927398717', '1234567812345678' ]
   call checkem(.false.)
   call unit_test_done('luhn_checksum',msg='')
   contains
   subroutine checkem(goodbad)
   logical,intent(in) :: goodbad
   integer :: i, j
      ! validate these numbers
      do i=1,size(ccards)
         j=len(trim(ccards(i)))
         string=luhn_checksum(ccards(i)(:j-1))
         buff=str(ccards(i)(:j-1))
         call unit_test('luhn_checksum', &
           & (transliterate(ccards(i),' -','').eq.string).eqv.goodbad, &
           & 'input',buff,'output',string)
      enddo
   end subroutine checkem
end subroutine test_luhn_checksum
end module module_luhn
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
module module_sha256
! TEST SUITE FOR THE bitsy SHA-256 FORTRAN IMPLEMENTATION
! Author: Mikael Leetmaa
! Date:   05 Jan 2014
use M_framework, only : unit_test, unit_test_start, unit_test_done
use M_framework, only : unit_test_level
use M_framework, only : unit_test_stop
use M_hashkeys__public
use,intrinsic :: ISO_FORTRAN_ENV, only : int8,int16,int32,int64,real32,real64,real128
use,intrinsic :: iso_c_binding
implicit none
private
public test_suite_sha256
contains
subroutine test_suite_sha256()
integer(kind=int32),parameter :: ipad1              =int(b'00000000000000000000000000000011',kind=int32)
!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
integer(kind=int32),parameter :: ipad2     =ibset(int(ibclr(int(b'11111111111111111111111111111111',kind=int64),31),kind=int32),31)
integer(kind=int32),parameter :: ipad3     =ibset(int(ibclr(int(b'10010000101001110011001110010011',kind=int64),31),kind=int32),31)
integer(kind=int32),parameter :: ipad4     =ibset(int(ibclr(int(b'11001001101001110011001110010011',kind=int64),31),kind=int32),31)
integer(kind=int32),parameter :: ipad5     =ibset(int(ibclr(int(b'10000001101001010011000110100001',kind=int64),31),kind=int32),31)
integer(kind=int32),parameter :: ipad6     =ibset(int(ibclr(int(b'11000000000000000000000000000000',kind=int64),31),kind=int32),31)
integer(kind=int32),parameter :: shftc4_r2 =ibset(int(ibclr(int(b'11110010011010011100110011100100',kind=int64),31),kind=int32),31)
integer(kind=int32),parameter :: empty_str_bin_flip &
 & =ibset(int(ibclr(int(b'10000000000000000000000000000000',kind=int64),31),kind=int32),31)
integer(kind=int32),parameter :: big_endian_464 &
 & =ibset(int(ibclr(int(b'11010000000000010000000000000000',kind=int64),31),kind=int32),31)
!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

integer(kind=int32),parameter :: shftc4_l12         =int(b'01110011001110010011110010011010',kind=int32)
integer(kind=int32),parameter :: shft5_r8           =int(b'00000000100000011010010100110001',kind=int32)
integer(kind=int32),parameter :: shft5_l11          =int(b'00101001100011010000100000000000',kind=int32)
integer(kind=int32),parameter :: abc_bin            =int(b'00000001011000110110001001100001',kind=int32)
integer(kind=int32),parameter :: a_bin              =int(b'00000000000000000000000101100001',kind=int32)
integer(kind=int32),parameter :: empty_str_bin      =int(b'00000000000000000000000000000001',kind=int32)
integer(kind=int32),parameter :: empty_bin          =int(b'00000000000000000000000000000000',kind=int32)
integer(kind=int32),parameter :: abc_bin_flip       =int(b'01100001011000100110001110000000',kind=int32)
integer(kind=int32),parameter :: a_bin_flip         =int(b'01100001100000000000000000000000',kind=int32)

integer(kind=int32),parameter :: abca_bin           =int(b'01100001011000110110001001100001',kind=int32)
integer(kind=int32),parameter :: bcab_bin           =int(b'01100010011000010110001101100010',kind=int32)
integer(kind=int32),parameter :: cabc_bin           =int(b'01100011011000100110000101100011',kind=int32)
integer(kind=int32),parameter :: ca_one_zero        =int(b'00000000000000010110000101100011',kind=int32)
integer(kind=int32),parameter :: little_endian_464  =int(b'00000000000000000000000111010000',kind=int32)

integer(kind=int32),parameter :: abc_bin_ref        =int(b'00000001011000110110001001100001',kind=int32)
integer(kind=int32),parameter :: abc_bin_swap       =int(b'01100001011000100110001100000001',kind=int32)

integer(kind=int32),parameter :: abca_bin_flip      =int(b'01100001011000100110001101100001',kind=int32)
integer(kind=int32),parameter :: bcab_bin_flip      =int(b'01100010011000110110000101100010',kind=int32)
integer(kind=int32),parameter :: cabc_bin_flip      =int(b'01100011011000010110001001100011',kind=int32)
integer(kind=int32),parameter :: ca_one_zero_flip   =int(b'01100011011000011000000000000000',kind=int32)

   call test_swap32
   call test_ishftc
   call test_ishft
   call pad_message1
   call pad_message2
   call test_ch
   call test_maj
   call test_cs0
   call test_cs1
   call test_ms0
   call test_ms1
   call test_sha256_1
   call test_sha256_5
   call test_sha256_6
   call test_sha256_11
contains
! Test the swap function.
   subroutine test_swap32
      call unit_test_start('swap32')
      call unit_test('swap32',swap32(abc_bin)==abc_bin_swap,'test swap32 function')
      call unit_test('swap32',abc_bin==abc_bin_ref,'test swap value')
      call unit_test_done('swap32')
   end subroutine test_swap32

   subroutine test_ishftc
! Make sure the intrinsic ishftc function does what we think.
      integer(kind=int32) :: a
      call unit_test_start('ishftc')
      a = ishftc(ipad4, -2)
      call unit_test('ishftc',a==shftc4_r2,'verify ishftc A')
      a = ishftc(ipad4, 12)
      call unit_test('ishftc',a==shftc4_l12,'verify ishftc B')
      call unit_test_done('ishftc')
   end subroutine test_ishftc

! Make sure the intrinsic ishft function does what we think.
   subroutine test_ishft
      integer(kind=int32) :: a
      call unit_test_start('ishft')
      a = ishft(ipad5, -8)
      call unit_test('ishft',a==shft5_r8,'verify ishft A')
      a = ishft(ipad5, 11)
      call unit_test('ishft',a==shft5_l11,'verify ishft B')
      call unit_test_done('ishft')
   end subroutine test_ishft

! Test the message padding.
   subroutine pad_message1
      character(len=1000) :: str
      integer(kind=int32) :: inp(16)
      integer(kind=8) :: length
      integer :: pos0, break
      integer :: swap = 1

      call unit_test_start('pad_message1')
      ! Set the message to "".
      str = ""
      pos0   = 1
      break  = 0
      length = 0
      call consume_chunk(str, length, inp, pos0, break, swap)

      ! Check the first word.
      call unit_test('pad_message1',inp(1)==empty_str_bin_flip,'message padding A')

      ! Set the message to "abc".
      str = "abc"
      pos0   = 1
      break  = 0
      length = 3
      call consume_chunk(str, length, inp, pos0, break, swap)

      ! Check the first word.
      call unit_test('pad_message1',inp(1)==abc_bin_flip,'message padding B')

      ! Set the message to "a".
      str = "a"
      pos0   = 1
      break  = 0
     length = 1
      call consume_chunk(str, length, inp, pos0, break, swap)

      ! Check the first word.
      call unit_test('pad_message1',inp(1)==a_bin_flip,'message padding C')

      call unit_test_done('pad_message1')
   end subroutine pad_message1

! Test the message padding.
   subroutine pad_message2
      character(len=1024) :: str
      integer(kind=int32) :: inp(16)
      integer(kind=8) :: length
      integer :: pos0, break
      integer :: swap = 1

      ! Set the message.
      str = "abcabcabcabcabcaabcabcabcabcabcaabcabcabcabcabcaabcabcabca"

      pos0   = 1
      break  = 0
      length = 58
      call consume_chunk(str, length, inp, pos0, break, swap)

      ! Check the whole message.
      call unit_test_start('pad_message2')
      call unit_test('pad_message2',inp(1)== abca_bin_flip,'message padding 2')
      call unit_test('pad_message2',inp(2)== bcab_bin_flip,'message padding 2')
      call unit_test('pad_message2',inp(3)== cabc_bin_flip,'message padding 2')
      call unit_test('pad_message2',inp(4)== abca_bin_flip,'message padding 2')
      call unit_test('pad_message2',inp(5)== abca_bin_flip,'message padding 2')
      call unit_test('pad_message2',inp(6)== bcab_bin_flip,'message padding 2')
      call unit_test('pad_message2',inp(7)== cabc_bin_flip,'message padding 2')
      call unit_test('pad_message2',inp(8)== abca_bin_flip,'message padding 2')
      call unit_test('pad_message2',inp(9)== abca_bin_flip,'message padding 2')
      call unit_test('pad_message2',inp(10)==bcab_bin_flip,'message padding 2')
      call unit_test('pad_message2',inp(11)==cabc_bin_flip,'message padding 2')
      call unit_test('pad_message2',inp(12)==abca_bin_flip,'message padding 2')
      call unit_test('pad_message2',inp(13)==abca_bin_flip,'message padding 2')
      call unit_test('pad_message2',inp(14)==bcab_bin_flip,'message padding 2')
      call unit_test('pad_message2',inp(15)==ca_one_zero_flip,'message padding 2')
      call unit_test('pad_message2',inp(16)==empty_bin,'message padding 2')

      call consume_chunk(str, length, inp, pos0, break, swap)

      call unit_test('pad_message2',inp(1)== empty_bin,'message padding 2')
      call unit_test('pad_message2',inp(2)== empty_bin,'message padding 2')
      call unit_test('pad_message2',inp(3)== empty_bin,'message padding 2')
      call unit_test('pad_message2',inp(4)== empty_bin,'message padding 2')
      call unit_test('pad_message2',inp(5)== empty_bin,'message padding 2')
      call unit_test('pad_message2',inp(6)== empty_bin,'message padding 2')
      call unit_test('pad_message2',inp(7)== empty_bin,'message padding 2')
      call unit_test('pad_message2',inp(8)== empty_bin,'message padding 2')
      call unit_test('pad_message2',inp(9)== empty_bin,'message padding 2')
      call unit_test('pad_message2',inp(10)==empty_bin,'message padding 2')
      call unit_test('pad_message2',inp(11)==empty_bin,'message padding 2')
      call unit_test('pad_message2',inp(12)==empty_bin,'message padding 2')
      call unit_test('pad_message2',inp(13)==empty_bin,'message padding 2')
      call unit_test('pad_message2',inp(14)==empty_bin,'message padding 2')
      call unit_test('pad_message2',inp(15)==empty_bin,'message padding 2')
      call unit_test('pad_message2',inp(16)==little_endian_464,'message padding 2')

      call unit_test_done('pad_message2')

   end subroutine pad_message2
! Test the ch function.
   subroutine test_ch
      integer(kind=int32) :: e, f, g
      integer(kind=int32) :: aa, bb
      e = ipad1
      f = ipad2
      g = ipad3
      aa = iand(not(e),g)
      bb = iand(e,f)
      call unit_test_start('test_ch')
      call unit_test('test_ch',ieor(aa,bb)==maj(e,f,g),'test the ch function')
      call unit_test_done('test_ch')
   end subroutine test_ch

! Test the maj function.
   subroutine test_maj
      integer(kind=int32) :: a, b, c
      integer(kind=int32) :: aa, bb, cc

      call unit_test_start('test_maj')
      a = ipad1
      b = ipad2
      c = ipad3
      aa = iand(a,b)
      bb = iand(a,c)
      cc = iand(b,c)
      call unit_test('test_maj',ieor(aa,ieor(bb,cc))==maj(a,b,c),'test the maj function')

      a = ipad2
      b = ipad3
      c = ipad4
      aa = iand(a,b)
      bb = iand(a,c)
      cc = iand(b,c)
      call unit_test('test_maj',ieor(aa,ieor(bb,cc))==maj(a,b,c),'test the maj function')

      a = ipad3
      b = ipad4
      c = ipad5
      aa = iand(a,b)
      bb = iand(a,c)
      cc = iand(b,c)
      call unit_test('test_maj',ieor(aa,ieor(bb,cc))==maj(a,b,c),'test the maj function')

      a = ipad4
      b = ipad5
      c = ipad6
      aa = iand(a,b)
      bb = iand(a,c)
      cc = iand(b,c)
      call unit_test('test_maj',ieor(aa,ieor(bb,cc))==maj(a,b,c),'test the maj function')

      call unit_test_done('test_maj')

   end subroutine test_maj

! Test the major sigma-0 function.
   subroutine test_cs0
      integer(kind=int32) :: a, b, c
      call unit_test_start('test_cs0')
      a   = ishftc(ipad1, -2)
      b   = ishftc(ipad1, -13)
      c   = ishftc(ipad1, -22)
      call unit_test('test_cs0',ieor(a,ieor(b,c))==cs0(ipad1),'test the major sigma-9 function')

      a   = ishftc(ipad2, -2)
      b   = ishftc(ipad2, -13)
      c   = ishftc(ipad2, -22)
      call unit_test('test_cs0',ieor(a,ieor(b,c))==cs0(ipad2),'test the major sigma-9 function')

      a   = ishftc(ipad3, -2)
      b   = ishftc(ipad3, -13)
      c   = ishftc(ipad3, -22)
      call unit_test('test_cs0',ieor(a,ieor(b,c))==cs0(ipad3),'test the major sigma-9 function')

      a   = ishftc(ipad4, -2)
      b   = ishftc(ipad4, -13)
      c   = ishftc(ipad4, -22)
      call unit_test('test_cs0',ieor(a,ieor(b,c))==cs0(ipad4),'test the major sigma-9 function')

      a   = ishftc(ipad5, -2)
      b   = ishftc(ipad5, -13)
      c   = ishftc(ipad5, -22)
      call unit_test('test_cs0',ieor(a,ieor(b,c))==cs0(ipad5),'test the major sigma-9 function')

      a   = ishftc(ipad6, -2)
      b   = ishftc(ipad6, -13)
      c   = ishftc(ipad6, -22)
      call unit_test('test_cs0',ieor(a,ieor(b,c))==cs0(ipad6),'test the major sigma-9 function')

      call unit_test_done('test_cs0')
   end subroutine test_cs0

! Test the major sigma-1 function.
   subroutine test_cs1
      integer(kind=int32) :: a, b, c
      call unit_test_start('test_cs1')
      a   = ishftc(ipad1, -6)
      b   = ishftc(ipad1, -11)
      c   = ishftc(ipad1, -25)
      call unit_test('test_cs1',ieor(a,ieor(b,c))==cs1(ipad1),'test the major sigma-9 function')

      a   = ishftc(ipad2, -6)
      b   = ishftc(ipad2, -11)
      c   = ishftc(ipad2, -25)
      call unit_test('test_cs1',ieor(a,ieor(b,c))==cs1(ipad2),'test the major sigma-9 function')

      a   = ishftc(ipad3, -6)
      b   = ishftc(ipad3, -11)
      c   = ishftc(ipad3, -25)
      call unit_test('test_cs1',ieor(a,ieor(b,c))==cs1(ipad3),'test the major sigma-9 function')

      a   = ishftc(ipad4, -6)
      b   = ishftc(ipad4, -11)
      c   = ishftc(ipad4, -25)
      call unit_test('test_cs1',ieor(a,ieor(b,c))==cs1(ipad4),'test the major sigma-9 function')

      a   = ishftc(ipad5, -6)
      b   = ishftc(ipad5, -11)
      c   = ishftc(ipad5, -25)
      call unit_test('test_cs1',ieor(a,ieor(b,c))==cs1(ipad5),'test the major sigma-9 function')

      a   = ishftc(ipad6, -6)
      b   = ishftc(ipad6, -11)
      c   = ishftc(ipad6, -25)
      call unit_test('test_cs1',ieor(a,ieor(b,c))==cs1(ipad6),'test the major sigma-9 function')

      call unit_test_done('test_cs1')

   end subroutine test_cs1

! Test the minor sigma-0 function.
   subroutine test_ms0
      integer(kind=int32) :: a, b, c

      call unit_test_start('test_ms0')
      a   = ishftc(ipad1, -7)
      b   = ishftc(ipad1, -18)
      c   =  ishft(ipad1, -3)
      call unit_test('test_ms0',ieor(a,ieor(b,c))==ms0(ipad1),'test the minor sigma-0 function')

      a   = ishftc(ipad2, -7)
      b   = ishftc(ipad2, -18)
      c   =  ishft(ipad2, -3)
      call unit_test('test_ms0',ieor(a,ieor(b,c))==ms0(ipad2),'test the minor sigma-0 function')

      a   = ishftc(ipad3, -7)
      b   = ishftc(ipad3, -18)
      c   =  ishft(ipad3, -3)
      call unit_test('test_ms0',ieor(a,ieor(b,c))==ms0(ipad3),'test the minor sigma-0 function')

      a   = ishftc(ipad4, -7)
      b   = ishftc(ipad4, -18)
      c   =  ishft(ipad4, -3)
      call unit_test('test_ms0',ieor(a,ieor(b,c))==ms0(ipad4),'test the minor sigma-0 function')

      a   = ishftc(ipad5, -7)
      b   = ishftc(ipad5, -18)
      c   =  ishft(ipad5, -3)
      call unit_test('test_ms0',ieor(a,ieor(b,c))==ms0(ipad5),'test the minor sigma-0 function')

      a   = ishftc(ipad6, -7)
      b   = ishftc(ipad6, -18)
      c   =  ishft(ipad6, -3)
      call unit_test('test_ms0',ieor(a,ieor(b,c))==ms0(ipad6),'test the minor sigma-0 function')

      call unit_test_done('test_ms0')

   end subroutine test_ms0

! Test the minor sigma-1 function.
   subroutine test_ms1
      integer(kind=int32) :: a, b, c
      call unit_test_start('test_ms1')

      a   = ishftc(ipad1, -17)
      b   = ishftc(ipad1, -19)
      c   =  ishft(ipad1, -10)
      call unit_test('test_ms1',ieor(a,ieor(b,c))==ms1(ipad1),'test the minor sigma-1 function')

      a   = ishftc(ipad2, -17)
      b   = ishftc(ipad2, -19)
      c   =  ishft(ipad2, -10)
      call unit_test('test_ms1',ieor(a,ieor(b,c))==ms1(ipad2),'test the minor sigma-1 function')

      a   = ishftc(ipad3, -17)
      b   = ishftc(ipad3, -19)
      c   =  ishft(ipad3, -10)
      call unit_test('test_ms1',ieor(a,ieor(b,c))==ms1(ipad3),'test the minor sigma-1 function')

      a   = ishftc(ipad4, -17)
      b   = ishftc(ipad4, -19)
      c   =  ishft(ipad4, -10)
      call unit_test('test_ms1',ieor(a,ieor(b,c))==ms1(ipad4),'test the minor sigma-1 function')

      a   = ishftc(ipad5, -17)
      b   = ishftc(ipad5, -19)
      c   =  ishft(ipad5, -10)
      call unit_test('test_ms1',ieor(a,ieor(b,c))==ms1(ipad5),'test the minor sigma-1 function')

      a   = ishftc(ipad6, -17)
      b   = ishftc(ipad6, -19)
      c   =  ishft(ipad6, -10)
      call unit_test('test_ms1',ieor(a,ieor(b,c))==ms1(ipad6),'test the minor sigma-1 function')

      call unit_test_done('test_ms1')

   end subroutine test_ms1

! Test the sha256 function with a set of reference strings.
   subroutine test_sha256_1
      character(len=1000000) :: str
      call unit_test_start('test_sha256_1')
      str = ""
      call unit_test('test_sha256_1',sha256(str)=="E3B0C44298FC1C149AFBF4C8996FB92427AE41E4649B934CA495991B7852B855",'sha256 1')
      str = "abc"
      call unit_test('test_sha256_1',sha256(str)=="BA7816BF8F01CFEA414140DE5DAE2223B00361A396177A9CB410FF61F20015AD",'sha256 2')
      str = "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq"
      call unit_test('test_sha256_1',sha256(str)=="248D6A61D20638B8E5C026930C3E6039A33CE45964FF2167F6ECEDD419DB06C1",'sha256 3')
      str = "abcdefghbcdefghicdefghijdefghijkefghijklfghijklmghijklmnhijklmnoijklmnopjklmnopqklmnopqrlmnopqrsmnopqrstnopqrstu"
      call unit_test('test_sha256_1',sha256(str)=="CF5B16A778AF8380036CE59E7B0492370B249B11E8F07A51AFAC45037AFEE9D1",'sha256 4')

      call unit_test_done('test_sha256_1')
   end subroutine test_sha256_1

   subroutine test_sha256_5
      character(len=1000000) :: str
      character(len=64)      :: ref
      integer :: i
      call unit_test_start('test_sha256_5')
      do i=1,1000000
         str(i:i) = "a"
      enddo
      call unit_test('test_sha256_5',sha256(str)=="CDC76E5C9914FB9281A1C7E284D73E67F1809A48A497200E046D39CCC7112CD0",'sha256 5')
      ! Check the quick and dirty implementation as well.
      ref = "69E3FACD5F08321F78117BD53476E5321845433356F106E7013E68EC367F3017"
      call unit_test('test_sha256_5',dirty_sha256(str)==ref,'test sha256 6')

      call unit_test_done('test_sha256_5')
   end subroutine test_sha256_5

   subroutine test_sha256_6
      character(len=1000000) :: str
      call unit_test_start('test_sha256_6')
      str = "message digest"
      call unit_test('test_sha256_6',sha256(str)=="F7846F55CF23E14EEBEAB5B4E1550CAD5B509E3348FBC4EFA3A1413D393CB650",'sha256 6')
      str = "secure hash algorithm"
      call unit_test('test_sha256_6',sha256(str)=="F30CEB2BB2829E79E4CA9753D35A8ECC00262D164CC077080295381CBD643F0D",'sha256 7 ')
      str = "SHA256 is considered to be safe"
      call unit_test('test_sha256_6',sha256(str)=="6819D915C73F4D1E77E4E1B52D1FA0F9CF9BEAEAD3939F15874BD988E2A23630",'sha256 8 ')
      str = "For this sample, this 63-byte string will be used as input data"
      call unit_test('test_sha256_6',sha256(str)=="F08A78CBBAEE082B052AE0708F32FA1E50C5C421AA772BA5DBB406A2EA6BE342",'sha256 9 ')
      str = "This is exactly 64 bytes long, not counting the terminating byte"
      call unit_test('test_sha256_6',sha256(str)=="AB64EFF7E88E2E46165E29F2BCE41826BD4C7B3552F6B382A9E7D3AF47C245F8",'sha256 10 ')
      call unit_test_done('test_sha256_6')
   end subroutine test_sha256_6

   subroutine test_sha256_11
      !integer,parameter     :: big=16777216  ! too big for ifort
      !integer,parameter     :: big=167777  ! too big for ifort
      integer,parameter     :: big=16777
      character(len=big*64) :: str
      integer :: i
      call unit_test_start('test_sha256_11')
      !write(*,*)'A long test'
      do i=1,big
         str(1+(i-1)*64:i*64) = "abcdefghbcdefghicdefghijdefghijkefghijklfghijklmghijklmnhijklmno"
      enddo
      !call unit_test('test_sha256_11',sha256(str)=="50E72A0E26442FE2552DC3938AC58658228C0CBFB1D2CA872AE435266FCD055E",'sha256 11')

      !call unit_test('test_sha256_11',sha256(str)=="6BC568C54C0BB123FBCA27DAD40067345DD9FBE61E1376FE3C27902943FCF6A5",&
      !& 'sha256 11 GOT',sha256(str),'expected 6BC568C54C0BB123FBCA27DAD40067345DD9FBE61E1376FE3C27902943FCF6A5')

      ! add //'' to avoid gfortran-11 bug
      call unit_test('test_sha256_11',sha256(str)=="711CC2AB7E0A98D1EDBDA435A7B219E8AAA12661F347339A14041208751373C6", &
      & 'sha256 11 GOT',sha256(str)//'','expected 711CC2AB7E0A98D1EDBDA435A7B219E8AAA12661F347339A14041208751373C6')

      call unit_test_done('test_sha256_11')
   end subroutine test_sha256_11

end subroutine test_suite_sha256
end module module_sha256

program runtest
use M_framework, only : unit_test_stop
use module_luhn
use module_sha256
implicit none
   write(*,*)'STARTED test_suite_M_hashkeys'
   call test_luhn_checksum()
   call test_suite_sha256()
   write(*,*)'COMPLETED test_suite_M_hashkeys'
   call unit_test_stop()
contains
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
end program runtest
