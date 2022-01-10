PROGRAM TEST_M_display
use M_verify, only : unit_check, unit_check_start, unit_check_good, unit_check_bad
use M_verify, only : unit_check_command, unit_check_keep_going, unit_check_level

  ! MAIN TEST PROGRAM FOR M_display. SEE ALSO test_M_display_fpp.F90

  USE M_display
  ! USE DISP_I1MOD  ! uncomment this line if testing of disp_i1mod (1 byte integers) is required
  ! USE DISP_I2MOD  ! uncomment this line if testing of disp_i2mod (2 byte integers) is required
  ! USE DISP_I4MOD  ! uncomment this line if testing of disp_i4mod (4 byte integers) is required
  ! USE DISP_I8MOD  ! uncomment this line if testing of disp_i8mod (8 byte integers) is required
  ! USE DISP_L1MOD  ! uncomment this line if testing of disp_l1mod (1 byte logicals) is required
  ! USE DISP_R16MOD ! uncomment this line if testing of disp_r16mod (quad precision) is required
  !
  ! Note that default integers are often 4 (sometimes 8) bytes, Note also that many computers/compilers do not
  ! support quad precision, and some support 10 byte reals (at least g95 on x86 platforms, it uses kind=10)
  !
  ! Copyright (c) 2008, Kristjan Jonasson, Dept. of Computer Science, University of
  ! Iceland (jonasson@hi.is). This software is free. For details see the file README.

  implicit none
  integer, parameter :: verbose = 2  ! 0 = quiet
  !                                  ! 1 = report only names of test routines
  !                                  ! 2 = report also what the tests display
  integer, parameter :: irange = 9   ! Run for irange = 2, 4, 9 and 18 for test of 1, 2, 4 and 8 byte integers
  !                                  ! (most likely)
  integer, parameter :: logikind = kind(.false.)        ! Run for logikind = 1 (probably) for test of 1 byte logicals
  integer, parameter :: sik = selected_int_kind(irange) ! Usually either 0, 1, 2, 3 or 1, 2, 4, 8
  integer, parameter :: srk = kind(0.0)                 ! Run (at least) for srk = kind(0.0) and kind(0d0) 
  
  integer :: compare_no = 0
  character(30) :: assert_string = ''
  character(3)  :: adv = 'no'
  character(90) :: fmt = '("  Testing M_display, int kind=",I0,", real kind=",I0,", logical kind=",I0,"...")'

   unit_check_command=''
   unit_check_keep_going=.true.
   unit_check_level=0
   write(*,'(a)')'STARTED M_display'
  ! Use goodbad(1) to indicate the test sequence was begun
   call unit_check_start('M_display',' &
      & -section 3  &
      & -library libGPF  &
      & -filename `pwd`/M_display.FF &
      & -documentation y &
      &  -prep         y &
      &  -ccall        n &
      &  -archive      GPF.a &
      & ')

  if (verbose > 0) adv = 'yes'
  write(*, fmt, advance = adv) sik, srk, logikind
  call open_8
  call tests_from_manual
  call test_disp_set1
  call test_disp_set2
  call test_integer
  call test_real
  call test_complex
  call test_logical
  call test_char
  call test_scalar_string
  call test_numbering
  call test_zeroas
  call test_advance_double
  call test_advance_no
  call test_empty
  call test_tostring
  call test_error_messages
  call close_8
  write(*,'("  OK")')
  call unit_check_good('M_display')
  write(*,'(a)')'COMPLETED M_display'

CONTAINS
!===================================================================================================================================
  subroutine msg1(st)
    ! Print st if verbose is >= 1
    character(*), intent(in) :: st
    if (verbose >= 1) write(*,'(2x,a)') trim(st)
  end subroutine msg1
!===================================================================================================================================
  subroutine msg2(st)
    ! Print st if verbose is >= 2
    character(*), intent(in) :: st
    if (verbose >= 2) write(*,'(4x,a)') trim(st)
  end subroutine msg2
!===================================================================================================================================
  subroutine w4blnk
    ! Write 4 blanks
    write(*,'("    ")', advance = 'no')
  end subroutine w4blnk
!===================================================================================================================================
  subroutine test_disp_set1
    ! Check default values of disp_settings
    type(disp_settings) ds
    ds = disp_get()
    call assert(ds % advance   == 'YES',  '1A')
    call assert(ds % trim      == 'AUTO', '1B')
    call assert(ds % sep       == '  ',   '1C')
    call assert(ds % matsep    == '   ',  '1D')
    call assert(ds % orient    == 'COL',  '1E')
    call assert(ds % style     == 'LEFT', '1F')
    call assert(ds % seplen    == 2,      '1H')
    call assert(ds % matseplen == 3,      '1I')
    call assert(ds % digmax    == 6,      '1J')
    call assert(ds % zeroas    == '',     '1K')
    call assert(ds % zaslen    == 0,      '1L')
    call assert(ds % unit      == -3,     '1M')
    call assert(ds % unit      == ASTERISK_UNIT, '1M')
  end subroutine test_disp_set1
!===================================================================================================================================
  subroutine test_disp_set2
    ! Set, retrieve settings with disp_get and check contents of retrieved settings
    type(disp_settings) ds
    call disp_set(advance = 'yes')
    call disp_set(matsep = 'ss')
    call disp_set(sep = 'cc')
    call disp_set(orient = 'row')
    call disp_set(style = 'xpad')
    call disp_set(unit = 7)
    call disp_set(digmax = 13)
    call disp_set(zeroas = '**')
    ds = disp_get()
    call assert(ds % advance    == 'YES',  '2A')
    call assert(ds % matsep     == 'ss',   '2B')
    call assert(ds % sep        == 'cc',   '2C')
    call assert(ds % orient     == 'ROW',  '2D')
    call assert(ds % style      == 'xpad', '2E')
    call assert(ds % unit       == 7,      '2G')
    call assert(ds % digmax     == 13,     '2H')
    call assert(ds % zeroas     == '**',   '2I')
    call assert(ds % zaslen     == 2,      '2J')
    call disp_set(orient = 'col')
    call disp_set(style = 'UNDERLINE')
    ds = disp_get()
    call assert(ds % orient     == 'COL',  '2K')
    call assert(ds % style      == 'UNDERLINE', '2L')
    call disp_set_factory
  end subroutine test_disp_set2
!===================================================================================================================================

  subroutine test_error_messages
    if (verbose < 1) return
    call msg2('')
    call msg1('TEST_ERROR_MESSAGES')
    call msg1('  (Following should print 17 error messages,')
    call msg1('  5 for disp_set, 8 for disp and 4 for tostring_set)')
    call disp_set(unit = NULL_UNIT)
    call w4blnk; call disp_set(advance = 'abc')
    call w4blnk; call disp_set(style = 'xyz')
    call w4blnk; call disp_set(digmax = 0)
    call w4blnk; call disp_set(digmax = 90)
    call w4blnk; call disp_set(orient = 'xyz')
    call disp_set(digmax = 1)  ! should not give error
    call disp_set(digmax = 89) ! should not give error
    call disp(0.17, digmax=89) ! should not give error
    call w4blnk; call disp(0.17, digmax=0)
    call w4blnk; call disp(0.17, digmax=90)
    call w4blnk; call disp(0.17, digmax=2, fmt = 'f0.1')
    call w4blnk; call disp(0.17, advance='xyz')
    call w4blnk; call disp(0.17, trim='xyz')
    call w4blnk; call disp('x=', 0.17, style='xyz')
    call w4blnk; call disp((/0.17/), orient='xyz')
    call w4blnk; call disp((1,1), fmt_imag = 'F10.2')
    call w4blnk; call tostring_set(trimb='xyz')
    call w4blnk; call tostring_set(trimz='xyz')
    call w4blnk; call tostring_set(rfmt='xyz')
    call w4blnk; call tostring_set(ifmt='xyz')
    call disp_set_factory
  end subroutine test_error_messages

!===================================================================================================================================
  subroutine test_integer
    ! Check that disp works with integers
    integer(sik) :: i1(1,2) = reshape((/-9_sik*10_sik**(irange-1)+1, 9_sik*10_sik**(irange-1)-1/), (/1,2/))
    integer(sik) :: i2(2,2) = reshape((/100, -99, -99, 0/), (/2, 2/))
    integer(sik) :: i3(2,2) = reshape((/12, 1, -12, 0/), (/2, 2/))
    integer(sik) :: i4(2)   = (/1, -2/)
    integer(sik) :: i5(1)   = (/0/)
    integer(sik) :: i6(1,2) = reshape((/13, -12/), (/1, 2/))
    integer(sik) :: i7(2), i8
    character(2*irange+3) :: s1(1)
    character(9) :: s2(3), s3(2), s3a(2), s4(3), s4a(2)
    character(2) :: s5(1)
    character(7) :: s6(3)
    character(10) :: s7(1)
    character(13) :: s7a(1)
    character(3) :: fmt, s8(2)
    character(10) :: st
    integer k500, k5000
    !
    call assert_init('TEST_INTEGER')
    st = ''
    if (sik == kind(0)) st = ' (default)'
    call msg1('  ('//tostring(bit_size(i1))//' bit'//trim(st)//' integers)')

    s1(1) = '-8' // repeat('9',irange-1) // ', 8' // repeat('9',irange-1)
    !
    s2 (1) = '----AB---'
    s2 (2) = '100,  -99'
    s2 (3) = '-99,    0'
    !
    s3 (1) = 'a=12::-12'
    s3 (2) = '   1::  0'
    s3a(1) = 'a= 12,-12'
    s3a(2) = '    1,  0'
    !
    s4 (1) = 'Longtitle'
    s4 (2) = '    1    '
    s4 (3) = '   -2    '
    !
    s4a(1) = 'Longtitle'
    s4a(2) = '   1;-2  '
    !
    s5(1) = 'X0'
    !
    s6(1) = '  XY  '
    s6(2) = '======'
    s6(3) = ' 13-12'
    !
    s7 (1) = 'ab=5  5000'
    s7a(1) = 'ab=   5  5000'
    !
    s8(1) = 'abc'
    s8(2) = '500'    
    !
    ! Tests with default format
    call disp_set(unit=8)
    call disp(i1, sep = ', ');                                      call compare(s1)
    call disp('AB', i2, style = 'pad', sep=',  ');                  call compare(s2)
    call disp('a=', i3, sep = '::');                                call compare(s3)
    call disp('Longtitle', i4, style = 'pad', orient = 'col');      call compare(s4)
    call disp('X', i5);                                             call compare(s5)
    call disp('XY', i6, style = '=underline', sep = '', trim='no'); call compare(s6)
    !
    ! Tests with specified format
    write(fmt, '("i",i2)') irange + 1
    call disp(i1, fmt, sep = ',');                                              call compare(s1)
    call disp('AB', i2, 'i3', style = 'pad', sep=',  ');                        call compare(s2)
    call disp('a=', i3, 'i3', sep = ',');                                       call compare(s3a)
    call disp('Longtitle', i4, 'i2', style = 'above');                          call compare(s4)
    call disp('Longtitle', i4, 'i2', style = 'above', orient = 'col');          call compare(s4)
    call disp('Longtitle', i4, 'i2', style = 'pad', orient = 'row', sep = ';'); call compare(s4a)
    call disp('X', i5, 'i1');                                                   call compare(s5)
    call disp('XY', i6, 'i3', style = '=underline', sep = '');                  call compare(s6)
    !
    ! Test 4 byte numbers
    if (irange >= 4) then
      k500 = 500
      k5000 = 5000
      i7 = (/5,k5000/)
      i8 = k500
      call disp_set(orient = 'row')
      ! Without format:
      call disp('ab=', i7);                call compare(s7)
      call disp('abc', i8, style = 'pad'); call compare(s8)
      ! With format:
      call disp('ab=', i7, 'i4');                  call compare(s7a)
      call disp('abc', i8, 'i0', style = 'above'); call compare(s8)
    endif
    call disp_set_factory
  end subroutine test_integer

!===================================================================================================================================
  subroutine test_real
    ! Check that disp works with reals
    real(srk) :: r1(2,2) = reshape((/2.146_srk, -1.231_srk, 0.008_srk, 20.33_srk/), (/2,2/))
    real(srk) :: r2(1,2) = reshape((/12.0_srk, -1234.5678_srk/), (/1,2/))
    real(srk) :: r3(2,1)
    real(srk) :: r4(1,1) = reshape((/0.0_srk/), (/1,1/))
    real(srk) :: r5 = 0.0_srk
    integer :: rrng
    !
    character(3) rrangec
    character(6) r3_2chr
    !
    character(12) :: s1(2), f1(2)
    character(13) :: f3(2)
    character(15) :: f2(2)
    character(16) :: s3(2)
    character(17) :: s2(2), s3a(2)
    character(9)  :: s4(3), f4(3)
    character(18) :: prc
    !
    call assert_init('TEST_REAL')
    prc = ''
    if (srk == kind(0.0)) prc = ', single precision'
    if (srk == kind(0d0)) prc = ', double precision'
    call msg1('  (kind = '//tostring(srk)//trim(prc)//')')

    s1(1) = ' 2.15   0.01'
    s1(2) = '-1.23  20.33'
    f1 = s1
    !
    s2(1) = '--------AB-------'
    s2(2) = '   12.00,-1234.57'
    f2(1) = '-------AB------'
    f2(2) = '   12.0,-1234.6'
    !
    ! A number close to the largest possible real
    ! (r3 could for example be [1.2e21, -2e37] for single precision and [1.2e21, -2e307] for double)
    rrng = min(999, range(0.0_srk)) ! test not designed for numbers >= 10**1000.
    write(rrangec, '(i3.3)') rrng
    s3 (1) = 'A =  1.20000E+21'
    s3a(1) = 'A =  1.20000E+021'
    s3 (2) = '    -2.00000E+'//rrangec(2:3)
    s3a(2) = '    -2.00000E+'//rrangec
    f3(1) = 'A =  1.2E+021'
    f3(2) = '    -2.0E+'//rrangec
    r3(1,1) = 1.2e21
    r3_2chr = '-2e'//rrangec
    read(r3_2chr, *) r3(2,1)
    !
    s4(1) = 'Longtitle'
    s4(2) = '---------'
    s4(3) = ' 0.00000 '
    f4(1) = 'Longtitle'
    f4(2) = '*********'
    f4(3) = '   0.    '
    !
    ! Test calls without format (r2(1,:) tests vector):
    call disp_set(unit=8, style = 'PAD', sep = ',')
    call disp(r1, digmax=4, style = 'LEFT', sep = '  ');   call compare(s1, 'ok-1')
    call disp('AB', r2, trim = 'no');                      call compare(s2, 'ok-2')
    call disp('AB', r2(1,:), trim = 'no', orient = 'row'); call compare(s2, 'ok-3')
    call disp('A = ', r3, style = 'left');                 
    if (rrng <= 99)                                        call compare(s3, 'ok-4')
    if (rrng >  99)                                        call compare(s3a,'ok-4')
    call disp('Longtitle', r4, style = 'underline');       call compare(s4, 'ok-6')
    call disp('Longtitle', r5, style = 'underline');       call compare(s4, 'ok-7')

    ! Test calls with format:
    call disp(r1, 'f5.2', style = 'LEFT', sep = '  ');        call compare(f1, 'ok-8')
    call disp('AB', r2, 'f7.1');                              call compare(f2, 'ok-9')
    call disp('AB', r2(1,:), 'f7.1', orient = 'row');         call compare(f2, 'ok-10')
    call disp('A = ', r3, 'es9.1e3', style = 'left');         call compare(f3, 'ok-11')
    call disp('Longtitle', r4, 'f2.0', style = '*underline'); call compare(f4, 'ok-12')
    call disp_set_factory
  end subroutine test_real

!===================================================================================================================================
  subroutine test_numbering
    integer       :: i
    real(srk)     :: r1(2,2) = reshape((/2.146_srk, -1.231_srk, 0._srk, 0.33_srk/), (/2,2/))
    integer(sik)  :: i2(2)   = (/0,0/), i3(11) = (/(0,i=1,11)/), i4(1,2) = reshape((/0,0/), (/1,2/))
    character(15) :: s1a(3), s1b(3), s1c(3), s1d(3), s1e(3), s1f(4)
    character(20) :: s1g(3)
    character(9)  :: s2a(2), s2b(2), s2c(2), s2d(2), s2e(3), s4(2)
    character(42) :: s3c(2)
    s1a = (/'      1     2 ' ,&
         &  '1   2.15  0.00' ,&
         &  '2  -1.23  0.33' /)
    s1b = (/'     10    11 ' ,&
         &  '1   2.15  0.00' ,&
         &  '2  -1.23  0.33' /)
    s1c = (/'   10001  10002' ,&
         &  '1    2.1    0.0' ,&
         &  '2   -1.2    0.3' /)
    s1d = (/'    10   11 ' ,&
         &  '1, 2.15,0.00' ,&
         &  '2,-1.23,0.33' /)
    s1e = (/'    10    11 ' ,&
         &  '1, 2.15, 0.00' ,&
         &  '2,-1.23, 0.33' /)
    s1f = (/'------s1f-----' ,&
         &  '      1     2 ' ,&
         &  '1   2.15  0.00' ,&
         &  '2  -1.23  0.33' /)
    s1g = (/'            1     2 ' ,&
         &  's1g = 1   2.15  0.00' ,&
         &  '      2  -1.23  0.33' /)
    !
    s2a = (/'1  2' ,&
         &  '0  0' /)
    s2b = (/'-2  -1' ,&
         &  ' 0   0' /)
    s2c = (/'1  0' ,&
         &  '2  0' /)
    s2d = (/'XX = 1  0' ,&
         &  '     2  0' /)
    s2e = (/'-XX-' ,&
         &  '1  0' ,&
         &  '2  0' /)
    s3c = (/'-10  -9  -8  -7  -6  -5  -4  -3  -2  -1  0' ,&
         &  '  0   0   0   0   0   0   0   0   0   0  0' /)
    s4 = (/'   1  2' ,&
         & '1  0  0' /)
    !
    call assert_init('TEST_NUMBERING')
    call disp_set(unit = 8)
    call disp(r1, 'F0.2', style='number')                  ; call compare(s1a)
    call disp(r1, 'F0.2', style='number', lbound=(/1,10/)) ; call compare(s1b)
    call disp(r1, 'F0.2', lbound=(/1,10/))                 ; call compare(s1b)
    call disp(r1, 'F0.1', lbound=(/1,10001/))              ; call compare(s1c)
    call disp(r1, 'F0.2', sep=',', lbound=(/1,10/))        ; call compare(s1d)
    call disp(r1, 'F5.2', sep=',', lbound=(/1,10/))        ; call compare(s1e)
    call disp('s1f', r1, 'F0.2', style='pad & number')     ; call compare(s1f)
    call disp('s1g = ', r1, 'F0.2', style='number')        ; call compare(s1g)
    call disp(i2, style='number', orient='row') ; call compare(s2a)
    call disp(i2, lbound=(/1/), orient='row')   ; call compare(s2a)
    call disp(i2, lbound=(/-2/), orient='row')  ; call compare(s2b)
    call disp(i2, style='number')               ; call compare(s2c)
    call disp('XX = ', i2, style='number')      ; call compare(s2d)
    call disp('XX', i2, style='pad & number')   ; call compare(s2e)
    call disp(i3, lbound=(/-10/), orient='row') ; call compare(s3c)
    call disp(i4, style='number')               ; call compare(s4)
  end subroutine test_numbering
!===================================================================================================================================

  subroutine test_advance_double
    call assert_init('TEST_ADVANCE_DOUBLE')
    call disp_set(unit = 8)
    call disp(5, advance = 'double')
    call disp(6)
    call compare((/'5',' ','6'/))
    call disp_set(advance = 'double')
    call disp(7, advance = 'no')
    call disp(8)
    call disp(9)
    call compare((/'7   8','     ','9    ','     '/))
    call disp_set_factory
  end subroutine test_advance_double
!===================================================================================================================================
  subroutine test_advance_no
    call assert_init('TEST_ADVANCE_NO')
    call disp_set(unit = 8)
    call disp(1, advance = 'no')
    call disp((/2,3/), advance = 'yes')
    call compare((/'1   2','    3'/))
    call disp_set_factory
  end subroutine test_advance_no
!===================================================================================================================================
  subroutine test_zeroas
    ! Test various features of using zeroas
    real(srk)     :: d1(3,2) = reshape((/2.146_srk, -1.231_srk, 1._srk, 0._srk, 20.33_srk, 0._srk/), (/3,2/))
    real(srk)     :: d2(2)   = (/0._srk, 0._srk/)    
    real(srk)     :: d3(2)   = (/12e20_srk, 0._srk/)
    real(srk)     :: d4      = 0._srk
    real(srk)     :: r1(3,2) = reshape((/2.146_srk, -1.231_srk, 1._srk, 0._srk, 20.33_srk, 0._srk/), (/3,2/))
    real(srk)     :: r2(2)   = (/0._srk, 0._srk/)    
    integer(sik)  :: i1(4)   = (/1,0,11,0/)
    integer(sik)  :: i2(2,2) = reshape((/0,0,11,12/), (/2,2/))
    character(12) :: s1a(3), s1b(3), s1c(3), s1d(3), s1e(3), s1f(3)
    character(3)  :: s2(1)
    character(16) :: s3(2), s3a(2)
    character(9)  :: s4(2)
    character(2)  :: s5(4)
    character(5)  :: s6(2), s8(2)
    character(12) :: s7(3)
    !
    s1a = (/' 2.15      *' ,&
         &  '-1.23  20.33' ,&
         &  ' 1.00      *' /)
    s1b = (/' 2.15   0.  ' ,&
         &  '-1.23  20.33' ,&
         &  ' 1.00   0.  ' /)
    s1c = (/' 2.15   000.' ,&
         &  '-1.23  20.33' ,&
         &  ' 1.00   000.' /)
    s1d = (/' 2.15   .000' ,&
         &  '-1.23  20.33' ,&
         &  ' 1.00   .000' /)
    s1e = (/' 2.1  zero  ' ,&
         &  '-1.2  20.3  ' ,&
         &  ' 1.0  zero  ' /)
    s1f = (/' 2.1  zeroas' ,&
         &  '-1.2    20.3' ,&
         &  ' 1.0  zeroas' /)
    !
    s2 = '0,0'
    !
    s3(1) = 'A = 1.20000E+21'
    s3(2) = '              0'
    !
    s3a(1) = 'A = 1.20000+021'
    s3a(2) = '              0'
    !
    s4(1) = 'Longtitle'
    s4(2) = '    0    '
    !
    s5 = (/' 1','  ','11','  '/)
    !
    s6 = (/'.  11','.  12'/)
    !
    s7 = (/'   999  1000', '1    .    11', '2    .    12'/)
    !
    s8 = (/'0  11','0  12'/)
    !
    call assert_init('TEST_ZEROAS')
    call disp_set(unit=8, style = 'PAD', zeroas = '0')
    call disp(d1, digmax=4, zeroas = '*');       call compare(s1a)
    call disp(d1, digmax=4, zeroas = '0.');      call compare(s1b)
    call disp(d1, digmax=4, zeroas = '000.');    call compare(s1c)
    call disp(d1, digmax=4, zeroas = '.000');    call compare(s1d)
    call disp(d1, 'f4.1', zeroas = 'zeroas');    call compare(s1e)
    call disp(d1, digmax=3, zeroas = 'zeroas');  call compare(s1f)
    call disp(d2, sep = ',', orient = 'row');    call compare(s2)
    call disp('A = ', d3, style = 'left');       call compare(s3, 'zas', s3a)
    call disp('Longtitle', d4);                  call compare(s4)
    call disp(r1, digmax=4, zeroas = '*');       call compare(s1a)
    call disp(r1, digmax=4, zeroas = '0.');      call compare(s1b)
    call disp(r2, sep = ',', orient = 'row');    call compare(s2)
    call disp(i1, zeroas = ' ');                 call compare(s5)
    call disp(i2, zeroas = '.');                 call compare(s6)
    call disp(i2, zeroas='.', lbound=(/1,999/)); call compare(s7)
    call disp(i2, zeroas = '');                  call compare(s8)
    call disp_set_factory
  end subroutine test_zeroas
!===================================================================================================================================
  subroutine test_complex
    ! Test disp with complex data
    real(srk)    :: r1(2,2) = reshape((/2.146_srk, -1.231_srk, 0.008_srk, 20.33_srk/), (/2,2/))
    real(srk)    :: r2(2) = (/12.0_srk, -1234.5678_srk/)
    complex(srk) :: c1(2,2), c2(2), c3
    character(29) :: s1(2)
    character(31) :: s1a(2), s1b(2)
    character(36) :: s2(2)
    character(16) :: s3(1)
    !
    call assert_init('TEST_COMPLEX')
    s1(1) = ' 2.15 + 4.29i   0.01 +  0.02i'
    s1(2) = '-1.23 - 2.46i  20.33 + 40.66i'
    !
    s1a(1) = ' 2.1460 + 4.3i   0.0080 +  0.0i'
    s1a(2) = '-1.2310 - 2.5i  20.3300 + 40.7i'  
    !
    s1b(1) = ' 2.1 + 4.2920i   0.0 +  0.0160i'
    s1b(2) = '-1.2 - 2.4620i  20.3 + 40.6600i'  
    !
    s2(1) = '---------------AB--------------'
    s2(2) = '12.0 + 12.0i, -1234.6 - 1234.6i'
    !
    s3(1) = '1.2345 - 2.3457i'
    !
    c1 = cmplx(r1, 2*r1)
    c2 = cmplx(r2, r2)
    c3 = (1.2345_srk, -2.34567_srk)
    !
    call disp_set(unit=8)
    call disp(c1, 'F0.2');                                call compare(s1)
    call disp(c1, 'F0.4', fmt_imag = 'F0.1');             call compare(s1a)
    call disp(c1, 'F0.1', fmt_imag = 'F0.4');             call compare(s1b)
    call disp_set(style = 'PAD', digmax = 5, sep = ', ');
    call disp('AB', c2, orient = 'row');                  call compare(s2)
    call disp(c3);                                        call compare(s3)
    call disp_set_factory
  end subroutine test_complex 
!===================================================================================================================================
  subroutine test_char
    ! Check that disp works for outputting character strings with explicit or default format
    character(3) :: c1(2,2) = reshape((/'ABC','   ','A  ','  B'/), (/2,2/))
    character(3) :: c2(1,2) = reshape((/'ABC','AB '/), (/1,2/))
    character(1) :: c3(2,1) = reshape((/'X','Y'/), (/2,1/))
    character(3) :: c4(1,1) = reshape((/'ABC'/), (/1,1/))
    character(8) :: s1(2)
    character(9) :: s2(2)
    character(5) :: s3(2)
    character(9) :: s4(3)
    !
    call assert_init('TEST_CHAR')
    s1 = (/ 'ABC  A  ' ,&
         &  '       B' /)
    s2 = (/ '--title--' ,&
         &  ' ABC, AB ' /)
    s3 = (/ 'A = X' ,&
         &  '    Y' /)
    s4 = (/ 'Longtitle' ,&
         &  '*********' ,&
         &  '   ABC   ' /)
    call disp_set(unit=8, style = 'PAD')
    ! SPECIFIED FORMAT:
    call disp(c1, 'A', style = 'left');                    call compare(s1)
    call disp('title', c2, 'A4', sep = ',');               call compare(s2)
    call disp('A = ', c3, 'a1', style = 'left');           call compare(s3)
    call disp('Longtitle', c4, 'A', style = '*underline'); call compare(s4)
    ! DEFAULT FORMAT:
    call disp(c1, style = 'left');                    call compare(s1)
    call disp('A = ', c3, style = 'left');            call compare(s3)
    call disp('Longtitle', c4, style = '*underline'); call compare(s4)
    call disp_set_factory
  end subroutine test_char
!===================================================================================================================================
  subroutine test_logical
    ! Check that disp works for outputting logicals with explicit or default format
    logical(logikind) :: l1(2,2)
    logical(logikind) :: l2(1,2)
    logical(logikind) :: l3(2,1)
    logical(logikind) :: l4
    character(5), dimension(2)    :: s1, s2, s3, s4
    character                     :: s5(1)
    integer(selected_int_kind(2)) :: nbytes=0
    l1 = reshape((/.true., .true., .false., .false./), (/2,2/))
    l2 = reshape((/.false., .true./), (/1,2/))
    l3 = reshape((/.true., .false./), (/2,1/))
    l4 = .true.
    nbytes = int(size(transfer(l4, (/nbytes/))), selected_int_kind(2))
    !
    call assert_init('TEST_LOGICAL')
    call msg1('  ('//tostring(int(nbytes))//' byte logicals)')
    s1 = (/ ' T, F' ,&
         &  ' T, F' /)
    s2 = (/ 'T  F ' ,&
         &  'T  F ' /)
    s3 = (/ 'title' ,&
         &  'F  T ' /)
    s4 = (/ 'A = T' ,&
         &  '    F' /)
    s5 = (/ 'T' /)
    !
    call disp_set(unit=8)
    !SPECIFIED FORMAT:
    call disp(l1, 'L2', sep = ',');              call compare(s1)
    call disp('title', l2, 'L1', style = 'pad'); call compare(s3)
    call disp('A = ', l3, 'L1');                 call compare(s4)
    !DEFAULT FORMAT:
    call disp(l1);                               call compare(s2)
    call disp('title', l2, style = 'pad');       call compare(s3)
    call disp('A = ', l3);                       call compare(s4)
    call disp(l4);                               call compare(s5)
    call disp_set_factory
  end subroutine test_logical
!===================================================================================================================================
  subroutine tests_from_manual
    call assert_init('TESTS_FROM_MANUAL')
    call tests_from_manual_1
    call tests_from_manual_2
    call tests_from_manual_3
    call tests_from_manual_4
    call tests_from_manual_5
  end subroutine tests_from_manual
!===================================================================================================================================
  subroutine tests_from_manual_1
    ! Test examples in section 1 in the manual
    integer X(3,3)
    integer i, j, k, Y(3)
    real A(4,4), B(4,4)
    complex C(3,3)
    character(37) Ashouldbe(4)
    character(36) ANshouldbe(5)
    character(46) Bshouldbe(4)
    character(20) XYshouldbe(3)
    character(55) Cshouldbe(3)

    call assert_init('...section 1')
    A = reshape((/ (( real(exp(dble(i+j-1))) ,i=1,4), j=1,4) /), (/4,4/))
    B = reshape((/ (( real(exp(dble(i*j  ))), i=1,4), j=1,4) /), (/4,4/))
    X = reshape((/ 7, 8, 3, 4, 0, 2, 1, 3, 6 /), (/3,3/), order=(/2,1/))
    Y = (/ 11, 2, 7 /)
    forall(i=1:3, k=1:3) C(i,k)=log(cmplx(-i*k))**k
    Ashouldbe = (/ &
         'A =  2.72    7.39   20.09    54.60', &
         '     7.39   20.09   54.60   148.41', &
         '    20.09   54.60  148.41   403.43', &
         '    54.60  148.41  403.43  1096.63' /)
    ANshouldbe = (/ &
         '     1       2       3        4  ', &
         '1   2.72    7.39   20.09    54.60', &
         '2   7.39   20.09   54.60   148.41', &
         '3  20.09   54.60  148.41   403.43', &
         '4  54.60  148.41  403.43  1096.63' /)
    Bshouldbe = (/ &
         '2.71828E+0  7.38906E+0  2.00855E+1  5.45981E+1', &
         '7.38906E+0  5.45981E+1  4.03429E+2  2.98096E+3', &
         '2.00855E+1  4.03429E+2  8.10308E+3  1.62755E+5', &
         '5.45981E+1  2.98096E+3  1.62755E+5  8.88611E+6' /)   
    XYshouldbe = (/ &
         'X = 7  8  3   Y = 11', &
         '    4  0  2        2', &
         '    1  3  6        7' /)
    Cshouldbe = (/ &
         'C = 0.000 + 3.142i  -9.389 +  4.355i  -31.203 - 19.631i', &
         '    0.693 + 3.142i  -7.948 +  8.710i  -47.300 -  0.749i', &
         '    1.099 + 3.142i  -6.659 + 11.258i  -54.449 + 14.495i' /)
    call disp_set(unit = 8)
    call disp('A = ', A)
    call compare(Ashouldbe, '1-A')
    call disp(A, style = 'number')
    call compare(ANshouldbe, '1-A')
    !
    call disp(B)
    call compare(Bshouldbe, '1-B')
    !
    call disp('X = ', X, ADVANCE='no')
    call disp('Y = ', Y)

    call compare(XYshouldbe, '1-XY')
    !
    call disp('C = ', C, 'F0.3')
    call compare(Cshouldbe, '1-C')
    !
    call disp_set_factory
  end subroutine tests_from_manual_1
!===================================================================================================================================
  subroutine tests_from_manual_2
    ! Test examples in section 2 in the manual
    REAL :: A(3) = (/ 1.2345, 2.3456, 3.4567 /)
    CHARACTER(29) S
    call assert_init('...section 2')
    CALL DISP('A = ', A, UNIT = 8, SEP = ', ', ORIENT = 'ROW')
    rewind(8)
    read(8,1) S
    call msg2(S)
    call assert(S == 'A = 1.23450, 2.34560, 3.45670', '2')
    call reopen_8
    1 format(A)
  end subroutine tests_from_manual_2
!===================================================================================================================================
  subroutine tests_from_manual_3
    ! Test examples in section 3 in the manual
    real :: Matr(2,2) = reshape( (/1.2, 5.6, 4.2, 18.3/), (/2,2/) )
    real :: z(2,3) = reshape((/14.28, 1.42, 14285714.0, 141421.0, 0.47, 0.69/), (/2,3/))
    real :: x(2,2), pe(2,3)
    real :: A(0:3,0:3), B(4,4)
    integer xy(3,3), uv(3), i, j, k
    character(35) shouldbe_A(5)
    character(74) shouldbe_D(4)
    character(57) shouldbe_E(4)
    character(20) shouldbe_F(3)
    character(24) shouldbe_I(6)
    character(63) shouldbe_zas(4)
    call assert_init('...section 3')
    ! SECTION 3.1:
    call disp_set(unit=8)
    call disp(-44.6,'ES11.4E2')
    call disp(-44.6,'F8.4')
    call compare((/'-4.4600E+01','-44.6000   '/), '3-A')
    !
    CALL DISP('str',FMT='A4')
    CALL DISP('str','A4')
    call compare((/' str ','strA4'/), '3-B')
    !
    ! SECTION 3.2:
    !
    ! INTRODUCTION
    CALL DISP('X=', (/12.3, 16.78/), DIGMAX=3, ORIENT='row') 
    call compare((/'X=12.3  16.8'/), '3-D')
    !
    ! FMT_IMAG
    CALL DISP((1.31,2.47),'F0.1','F0.2')
    call compare((/'1.3 + 2.47i'/), '3-C')
    !
    ! LBOUND
    forall (i=0:3, j=0:3) A(i,j) = real(exp(dble(i+j-1)))
    call disp(A, style='number', lbound=lbound(A))
    shouldbe_A = (/ &
         '     0       1       2        3  ', &
         '0  0.368   1.000   2.718    7.389', &
         '1  1.000   2.718   7.389   20.086', &
         '2  2.718   7.389  20.086   54.598', &
         '3  7.389  20.086  54.598  148.413' /)
    call compare(shouldbe_A)
    call disp(A, lbound=lbound(A))
    call compare(shouldbe_A)
    !
    ! SEP
    CALL DISP(reshape((/-1,5,5,10/),(/2,2/)), SEP=', ') 
    call compare((/'-1,  5', ' 5, 10'/), '3-E')
    !
    ! STYLE
    call disp('a', (/1,2,3/), style='*underline', orient='row')
    call compare((/'   a   ','*******','1  2  3'/))
    !
    call disp_set(matsep = '    ')
    call disp('Matr = ', Matr, digmax=3, Style='left'         , advance='no')
    call disp('Matr',    Matr, digmax=3, Style='pad'          , advance='no')
    call disp('Matr',    Matr, digmax=3, Style='underline'    , advance='no')
    call disp(           Matr, digmax=3, Style='number'       , advance='no')
    call disp('Matr',    Matr, digmax=3, Style='_pad & number', advance='yes')
    shouldbe_D = (/ &
         'Matr = 1.2   4.2    ---Matr--       Matr          1     2     ____Matr____', &
         '       5.6  18.3    1.2   4.2    ---------    1  1.2   4.2        1     2 ', &
         '                    5.6  18.3    1.2   4.2    2  5.6  18.3    1  1.2   4.2', &
         '                                 5.6  18.3                    2  5.6  18.3' /)
    call compare(shouldbe_D, '3-F')
    !
    ! TRIM
    call disp_set(style='pad', orient='row', advance='no', sep=' ', matsep='   ')
    shouldbe_E = (/&
         '----X----   -------Y------   -----U-----   -------V------' ,&
         '1  2    3      1    2    3   333 22 4444    333   22 4444' ,&
         '2 22   34      2   22   34                               ' ,&
         '3 32 1234      3   32 1234                               ' /)
    xy = reshape((/1,2,3,2,22,32,3,34,1234/),(/3,3/))
    uv = (/333,22,4444/)
    call disp('X', xy, trim='yes')
    call disp('Y', xy, trim='no')
    call disp('U', uv, trim='yes')
    call disp('V', uv, trim='no',advance='yes')
    call compare(shouldbe_E, '3-G')
    call disp_set_factory;
    !
    call disp_set(style='above', unit = 8)
    forall(i=1:2, k=1:3) pe(i,k) = real(exp(dble(k)**i))
    call disp('power exponentials', pe, trim='yes')
    shouldbe_F = (/&
         ' power exponentials ',&
         '2.72   7.39    20.09',&
         '2.72  54.60  8103.08'/)
    call compare(shouldbe_F, '3-H')
    call disp_set_factory; call disp_set(unit = 8)
    !
    x = reshape((/1.2e5, 2.3e-3, -4.1e-2, 8.6e1/), (/2,2/))
    call disp_set(style='left', sep=' ')
    call disp('X=', x, 'es7.1e1', trim='yes')
    call compare((/'X=1.2E+5 -4.1E-2','  2.3E-3  8.6E+1'/), '3-I')
    call disp('X=', x, 'es7.1e1', trim='no' ) 
    call compare((/'X= 1.2E+5 -4.1E-2','   2.3E-3  8.6E+1'/), '3-J')
    call disp_set_factory; call disp_set(unit = 8)
    !
    ! ZEROAS
    forall(i=1:4,j=1:4) B(i,j) = max(0., j-i+1.)
    where(B > 0) B = 1/B
    shouldbe_zas = (/ &
         'A = 1.000  0.500  0.333  0.250   B = 1.000  0.500  0.333  0.250', &
         '        0  1.000  0.500  0.333        .     1.000  0.500  0.333', &
         '        0      0  1.000  0.500        .      .     1.000  0.500', &
         '        0      0      0  1.000        .      .      .     1.000' /)
    call disp('A = ', B, 'F0.3', zeroas = '0', advance='no')
    call disp('B = ', B, 'F0.3', zeroas='.') 
    call compare(shouldbe_zas, '3-ZAS')
    call disp_set_factory; call disp_set(unit=8)
    !
    ! SECTION 3.3:
    call disp('TESTING NULL_UNIT -- THIS SHOULD NOT BE DISPLAYED', unit = -1)
    call disp('TESTING NULL_UNIT -- THIS SHOULD NOT BE DISPLAYED', unit = NULL_UNIT)
    call assert(ASTERISK_UNIT == -3, 'asterisk_unit')
    call assert(PUTSTR_UNIT == -2, 'putstr_unit')
    call assert(NULL_UNIT == -1, 'null_unit')
    !
    ! SECTION 3.5
    call disp('F0.2', z, 'f0.2', style = 'pad')
    call disp("F13.2, TRIM='yes'", z, 'f7.2', style = 'pad', trim = 'yes')
    shouldbe_I =(/&
         & "----------F0.2----------" ,&
         & "14.28  14285714.00  0.47" ,&
         & " 1.42    141421.00  0.69" ,&
         & "--F13.2, TRIM='yes'-    " ,&
         & "14.28  *******  0.47    " ,&
         & " 1.42  *******  0.69    " /)
    call compare(shouldbe_I, '3-I')
    !
    call disp_set_factory
  end subroutine tests_from_manual_3
!===================================================================================================================================
  subroutine tests_from_manual_4
    ! Test examples in section 4 in the manual
    type(disp_settings) ds
    real :: x(3) = (/12.2, 9.6, -2.0/), y(3) = (/1.3, 13.0, 4.0/)
    integer :: z(3) = (/1, 3, 4/)
    character(15) :: s(3) = (/ '12.2 |  1.3 | 1' ,&
         &                     ' 9.6 | 13.0 | 3' ,&
         &                     '-2.0 |  4.0 | 4' /)
    call assert_init('...section 4')
    call test_disp_set1
    call disp_set(style = 'PAD', sep = ' ')
    ds = disp_get()
    call assert(ds % style == 'PAD', '4a')
    call assert(ds % sep == ' ' .and. ds % seplen == 1, '4b')
    call disp_set_factory
    call disp_set(unit=8, digmax = 3)
    call disp(x, advance='no')
    call disp(y, advance='no')
    call disp_set(matsep=' | ')
    call disp(z, advance='yes')
    call compare(s, '4c')
    call disp_xy(reshape(x,(/1,3/)), reshape(y,(/1,3/)))
    ds = disp_get()
    call assert(ds % sep == ' ' .and. ds % digmax == 3)
    call compare((/'x=12.20,9.60,-2.00', 'y=1.30,13.00,4.00 '/), '4d')
    call disp_set_factory
  end subroutine tests_from_manual_4
!===================================================================================================================================
  subroutine disp_xy(x,y)
    ! Utility for tests_from_manual_4
    ! use M_display (already "used" here, but explicit in manual)
    real, intent(in) :: x(:,:), y(:,:)
    type(disp_settings) ds
    ds = disp_get()
    call disp_set(digmax=4, sep=',')
    call disp('x=', x)
    call disp('y=', y)
    call disp_set(ds)
  end subroutine disp_xy
!===================================================================================================================================
  subroutine tests_from_manual_5
    ! Test examples in section 5 in the manual
    character(50) s
    integer i
    real(srk) sr
    character(*), parameter :: &  ! Alternative results:
         s1 = '1, 256, 6561, 65536, 3.90625+005', &
         s2 = '0.707107 + 0.707107i, 0.840896 + 0.840896i', &
         s4 = '1.1, 2.2+010, 3.3+020'
    real :: x = 1.5
    !
    call assert_init('...section 5')
    call disp_set(unit = 8)
    !
    ! SECTIONS 5.1-5.3
    call disp('The square of '//tostring(x)//' is '//tostring(x*x))
    call compare((/'The square of 1.5 is 2.25'/))
    CALL TOSTRING_SET(SEP='; ')
    s = tostring((/1,2/))                       ; call scompare(s, '1; 2')
    call tostring_set_factory
    !
    ! SECTION 5.4
    s = tostring(atan(1.0))                     ; call tscompare(s, (/0.785398/), '1a')
    !tostring(exp((/-3d0,-1d0,0d0,1d0/))) see (*) below
    s = tostring(real((/(i,i=1,5)/))**8)        ; call scompare(s, '1, 256, 6561, 65536, 3.90625E+05', s1)
    s = tostring((/1.23456,1.2300,1.23456e6/))  ; call tscompare(s, (/1.23456, 1.23, 1.23456E+06/), '1c')
    s = tostring(real((/(i,i=1,5)/))**8,'f0.1') ; call scompare(s, '1.0, 256.0, 6561.0, 65536.0, 390625.0')
    s = tostring(real((/(i,i=1,5)/))**8,'f6.1') ; call scompare(s, '1.0, 256.0, 6561.0, ******, ******')
    s = tostring((/1200000.,-1.2e-9/))          ; call scompare(s, '1.2E+06, -1.2E-09')
    !
    s = tostring(-77)                           ; call scompare(s, '-77')
    s = tostring((/(i,i=-3,3)/)**11)            ; call scompare(s, '-177147, -2048, -1, 0, 1, 2048, 177147')
    s = tostring((/(i,i=-3,3)/)**11, 'i7')      ; call scompare(s, '-177147, -2048, -1, 0, 1, 2048, 177147')
    s = tostring((/(i,i=-3,3)/)**11, 'i4')      ; call scompare(s, '****, ****, -1, 0, 1, 2048, ****')
    !
    s = tostring((1,3)/(4,2))                   ; call scompare(s, '0.5 + 0.5i')
    s = tostring(cmplx((/-1,-2/))**0.25)        ; call scompare(s, '0.70711 + 0.70711i, 0.8409 + 0.8409i', s2)
    !
    s = tostring((/.true., .false., .false./))  ; call scompare(s, 'T, F, F')
    s = tostring(.true., 'L2')                  ; call scompare(s, 'T')
    !
    call tostring_set(sep=';')
    s = tostring((/1,2,30/))                    ; call scompare(s, '1;2;30')
    !
    call tostring_set(trimb='NO')
    s = tostring(real((/(i,i=1,5)/))**8,'f6.1') ; call scompare(s, '   1.0; 256.0;6561.0;******;******')
    s = tostring((/1,2,30/),'i3')               ; call scompare(s, '  1;  2; 30')
    s = tostring((/(i,i=-3,3)/)**11, 'i4')      ; call scompare(s, '****;****;  -1;   0;   1;2048;****')
    s = tostring((/1,2,30/),'i0')               ; call scompare(s, '1;2;30')
    s = tostring(.true.,'L3')                   ; call scompare(s, '  T')
    !
    call tostring_set(trimz='NONE',sep=', ',trimb='YES')
    s = tostring(real((/(i,i=1,4)/))**8)        ; call tscompare(s, (/1., 256., 6561., 65536./), '1d')
    s = tostring((/1.23456,1.2300,1.23456e6/))  ; call tscompare(s, (/1.23456, 1.23, 1.23456E+06/), '1e')
    if (range(0._srk) >= 103) then
      s = '1.2d103'
      read(s,*) sr
      s = tostring(sr, 'ES11.5')                      ; call scompare(s, '1.20000+103')
      call tostring_set(trimz='ALL')
      s = tostring(sr)                                ; call scompare(s, '1.2+103')
      s = tostring((/1.1_srk,2.2e10_srk,3.3e20_srk/)) ; call scompare(s, '1.1, 2.2E+10, 3.3E+20', s4)
      s = tostring(exp((/-3._srk,-1._srk,0._srk,1._srk/))); call tscompare(s, (/4.97871E-02, 0.36788, 1., 2.71828/), '1b')  ! (*)
    endif
    !
    call tostring_set(trimz='ALL')
    s = tostring(real((/(i,i=1,5)/))**8,'f0.1') ; call scompare(s, '1, 256, 6561, 65536, 390625')
    !
    call tostring_set(rfmt='G12.4E2')
    s = tostring(real((/(i,i=1,5)/))**8)        ; call scompare(s, '1, 256, 6561, 0.6554E+05, 0.3906E+06')
    s = tostring((/1200000.,-1.2e-9/))          ; call scompare(s, '0.12E+07, -0.12E-08')
    !
    call disp_set_factory
    call tostring_set_factory
  end subroutine tests_from_manual_5
!===================================================================================================================================
  subroutine tscompare(s, vec, msg)
    character(*), intent(in) :: s, msg
    real, intent(in) :: vec(:)
    real svec(size(vec))
    integer i
    read(s, *) svec
    do i=1,size(vec)
       call assert(abs(vec(i) - svec(i))/abs(vec(i)) < 1e-4, 'tscompare:'//msg)
    enddo
  end subroutine tscompare
!===================================================================================================================================
  subroutine test_empty
    ! Test display of empty vectors / matrices of various dimensions (0 by 0, 0 by 1, 1 by 0, 0 by 2 etc.)
    character(16) shouldbe(14)
    character(20) shouldbe_char(14)
    call assert_init('TEST_EMPTY')
    shouldbe = (/ &
         'v=,v=   ,v,-v-,*', &
         'v,*             ', &
         ' ,*             ', &
         ' v              ', &
         '---             ', &
         'M02= ,M02=   ,* ', &
         'M02,  M02 ,*    ', &
         '---,------,     ', &
         'M20=,M20=,*     ', &
         '    ,    ,      ', &
         'M20,M20,*       ', &
         '---,---,        ', &
         '   ,   ,        ', &
         '   ,   ,        ' /)
    shouldbe_char = (/ &
         'v=  ,v=   ,-v,-v-,* ', &
         'v,*                 ', &
         ' ,*                 ', &
         ' v                  ', &
         '---                 ', &
         'M02=     ,M02=   ,* ', &
         '  M02 , M02,*       ', &
         '------,----,        ', &
         'M20=,M20=,*         ', &
         '    ,    ,          ', &
         'M20,M20,*           ', &
         '---,---,            ', &
         '   ,   ,            ', &
         '   ,   ,            ' /)
    call disp_set(unit=8, advance='no', matsep=',')
    call test_empty_int;     call compare(shouldbe, 'int')
    call test_empty_real;    call compare(shouldbe, 'real')
    call test_empty_logical; call compare(shouldbe, 'logical')
    call test_empty_char;    call compare(shouldbe_char, 'char')
    call disp_set_factory
    call assert_init()
  end subroutine test_empty
!===================================================================================================================================
  subroutine test_empty_int
    integer :: ivec(0), i02mat(0,2), i20mat(2,0)
    ivec = 0; i02mat = 0; i20mat = 0
    call msg2('...empty integer')
    call disp('v=', ivec)
    call disp('v=', ivec, 'I3') 
    call disp('v', ivec, style='pad')
    call disp('v', ivec, 'I3', style='pad')
    call disp('*', advance='yes')
    call disp('v', ivec, style='pad', orient='row')
    call disp((/'*','*'/), advance='yes')
    call disp('v', ivec, 'I3', style='underline', advance='yes')
    call disp('M02=', i02mat, sep=':')
    call disp('M02=', i02mat, 'I1', sep=':')
    call disp('*', advance='yes')
    call disp('M02', i02mat, style='underline')
    call disp('M02', i02mat, 'I2', style='underline')
    call disp('*', advance='yes')
    call disp('M20=', i20mat, sep=':')
    call disp('M20=', i20mat, 'I1', sep=':')
    call disp('*', advance='yes')
    call disp('M20', i20mat, style='underline')
    call disp('M20', i20mat, 'I2', style='underline')
    call disp('*', advance='yes')
  end subroutine test_empty_int
!===================================================================================================================================
  subroutine test_empty_logical
    logical(logikind) :: lvec(0), l02mat(0,2), l20mat(2,0)
    lvec = .false.; l02mat = .false.; l20mat = .false.
    call assert_init('...empty logical')
    call disp('v=', lvec)
    call disp('v=', lvec, 'L3')
    call disp('v', lvec, style='pad')
    call disp('v', lvec, 'L3', style='pad')
    call disp('*', advance='yes')
    call disp('v', lvec, style='pad', orient='row')
    call disp((/'*','*'/), advance='yes')
    call disp('v', lvec, 'L3', style='underline', advance='yes')
    call disp('M02=', l02mat, sep=':')
    call disp('M02=', l02mat, 'L1', sep=':')
    call disp('*', advance='yes')
    call disp('M02', l02mat, style='underline')
    call disp('M02', l02mat, 'L2', style='underline')
    call disp('*', advance='yes')
    call disp('M20=', l20mat, sep=':')
    call disp('M20=', l20mat, 'L1', sep=':')
    call disp('*', advance='yes')
    call disp('M20', l20mat, style='underline')
    call disp('M20', l20mat, 'L2', style='underline')
    call disp('*', advance='yes')
  end subroutine test_empty_logical
!===================================================================================================================================
  subroutine test_empty_real
    real :: rvec(0), r02mat(0,2), r20mat(2,0)
    rvec = 0.0; r02mat = 0.0; r20mat = 0.0
    call assert_init('...empty real')
    call disp('v=', rvec)
    call disp('v=', rvec, 'F3.0')
    call disp('v', rvec, style='pad')
    call disp('v', rvec, 'F3.0', style='pad')
    call disp('*', advance='yes')
    call disp('v', rvec, style='pad', orient='row')
    call disp((/'*','*'/), advance='yes')
    call disp('v', rvec, 'F3.0', style='underline', advance='yes')
    call disp('M02=', r02mat, sep=':')
    call disp('M02=', r02mat, 'F1.0', sep=':')
    call disp('*', advance='yes')
    call disp('M02', r02mat, style='underline')
    call disp('M02', r02mat, 'F2.0', style='underline')
    call disp('*', advance='yes')
    call disp('M20=', r20mat, sep=':')
    call disp('M20=', r20mat, 'F1.0', sep=':')
    call disp('*', advance='yes')
    call disp('M20', r20mat, style='underline')
    call disp('M20', r20mat, 'F2.0', style='underline')
    call disp('*', advance='yes')
  end subroutine test_empty_real
!===================================================================================================================================
  subroutine test_empty_char
    character(2) :: cvec(0), c02mat(0,2), c20mat(2,0)
    cvec = ''; c02mat = ''; c20mat = '';
    call assert_init('...empty char')
    call disp('v=', cvec)
    call disp('v=', cvec, 'A3')
    call disp('v', cvec, style='pad')
    call disp('v', cvec, 'A3', style='pad')
    call disp('*', advance='yes')
    call disp('v', cvec, style='pad', orient='row')
    call disp((/'*','*'/), advance='yes')
    call disp('v', cvec, 'A3', style='underline', advance='yes')
    call disp('M02=', c02mat, sep=':')
    call disp('M02=', c02mat, 'A1', sep=':')
    call disp('*', advance='yes')
    call disp('M02', c02mat, style='underline')
    call disp('M02', c02mat, 'A1', style='underline')
    call disp('*', advance='yes')
    call disp('M20=', c20mat, sep=':')
    call disp('M20=', c20mat, 'A1', sep=':')
    call disp('*', advance='yes')
    call disp('M20', c20mat, style='underline')
    call disp('M20', c20mat, 'A2', style='underline')
    call disp('*', advance='yes')
  end subroutine test_empty_char
!===================================================================================================================================
  subroutine test_scalar_string
    ! Test features with scalar-string (and missing item)
    character(6) shouldbe(7)
    !
    call assert_init('TEST_SCALAR_STRING')
    shouldbe = (/ &
         'A     ', &
         '      ', &
         ' B   C', &
         'T     ', &
         '-     ', &
         'D     ', &
         'E=E   ' /)
    call disp_set(unit=8)
    call disp('A')
    call disp()
    call disp('B', fmt='A2', advance='no')
    call disp('C', advance='yes')
    call disp('T', 'D', style='underline')
    call disp('E=', 'E', advance='no')
    call disp(advance='yes')
    call compare(shouldbe, 'x')
    call disp_set_factory
  end subroutine test_scalar_string
!===================================================================================================================================
  subroutine test_tostring
    character(50) s
    call assert_init('TEST_TOSTRING')
    ! First test:
    call assert(len(tostring(1e23, 'F0.2')) == 26 .or. len(tostring(1e23, 'F0.2')) == 27)
    s = tostring(1e23, 'F0.2')
    call assert(s(1:6) == '100000' .and. len_trim(s) == 27 .or. s(1:6) == '999999' .and. len_trim(s) == 26)
    ! Tests when factory defaults are in effect:
    call test_tostring_factory
    ! Tests of tostring when tostring_set is used to change defaults:
    call tostring_set(sep='::::')        ; call scompare(tostring((/1,2,3/)),  '1::::2::::3')
    call tostring_set(ifmt='i3')         ; call scompare(tostring((/1,2/)),    '1::::2'     )
    call tostring_set(trimb='NO')        ; call scompare(tostring((/1,2/)),    '  1::::  2' )
    call tostring_set(ifmt='i0')         ; call scompare(tostring((/1,2/)),    '1::::2'     )
    call tostring_set(sep='=1234567==')  ; call scompare(tostring((/1,2/)),    '1=1234567=2')
    call tostring_set(rfmt='E7.1E2')     ; call scompare(tostring(1.0),        '0.1E+01'    )
    call tostring_set(rfmt='1PE7.1E2')   ; call scompare(tostring(1.0),        '1.0E+00'    )
    call tostring_set(rfmt='1PE6.0E2')   ; call scompare(tostring(1.0),        '1.E+00'     )
    call tostring_set(trimz='NONE')      ; call scompare(tostring(1.0,'F4.1'), ' 1.0'       )
    call tostring_set(trimz='ALL')       ; call scompare(tostring(1.0,'F4.1'), ' 1'         )
    call tostring_set(ifmt='i2',sep=',') ; call scompare(tostring((/123,20/)), '**,20'      )
    call tostring_set(trimb='YES')       ; call scompare(tostring(1.0,'F4.1'), '1'          )
    call tostring_set(rfmt='F0.3',ifmt='I4',trimb='NO', trimz='NONE',sep=';')
    call scompare(tostring((/12345,123/))    , '****; 123'    )
    call scompare(tostring((/1.1239,123.45/)), '1.124;123.450')
    call scompare(tostring((/(1.0_srk,0.0_srk),(2.1_srk,-8.7_srk)/)), '1.000 + 0.000i;2.100 - 8.700i')
    call tostring_set_factory
    ! Confirm that behavior is back to original:
    call test_tostring_factory
  end subroutine test_tostring
!===================================================================================================================================
  subroutine test_tostring_factory
    ! Tests of tostring when original defaults are in effect
    call assert(len(tostring(5.86)) == 4)
    call scompare(tostring(5.86)                   , '5.86')
    call scompare(tostring(5.86e10)                , '5.86E+10', '5.86+010')
    call scompare(tostring((/1,100,10000/))        , '1, 100, 10000')
    call scompare(tostring((/1., 100., 10000./))   , '1, 100, 10000')
    call scompare(tostring(10.8, 'F8.3')           , '10.800')
    call scompare(tostring(10.8, 'F   8  .   3')   , '10.800')
    call scompare(tostring(10.8, 'F2.1')           , '**')
    call scompare(tostring(10.8, 'U9.2')           , 'Illegal format')
    call scompare(tostring(10.8, '333')            , 'Illegal format')
    call scompare(tostring(10.8, 'ES')             , 'Illegal format')
    call scompare(tostring(10.8, '2F')             , 'Illegal format')
    call scompare(tostring(10.8, 'SP,1PG10.3')     , '+10.8')
    call scompare(tostring(10.8e9,'SP,1PG10.3E2')  , '+1.08E+10', '+1.08+010')
    call scompare(tostring((1.23, -3.222))         , '1.23 - 3.222i')
    call scompare(tostring((/(1,1),(2,2)/))        , '1 + 1i, 2 + 2i')
    call scompare(tostring((/(1,1),(2,2)/),'F4.1') , '1.0 + 1.0i, 2.0 + 2.0i')
  end subroutine test_tostring_factory
!===================================================================================================================================
  subroutine scompare(s, shouldbe, sb1, sb2, sb3)
    ! Utility for test_tostring. Print s to the screen and assert that it is
    ! matches either shouldbe, sb1, sb2 or sb3.
    character(*), intent(in) :: s, shouldbe, sb1, sb2, sb3
    optional sb1, sb2, sb3
    call msg2('"'//trim(s)//'" should be "'//trim(shouldbe)//'"')
    if (present(sb3)) call assert(s == shouldbe .or. s == sb1 .or. s == sb2 .or. s == sb3)
    if (present(sb2)) call assert(s == shouldbe .or. s == sb1 .or. s == sb2)
    if (present(sb1)) call assert(s == shouldbe .or. s == sb1)
    if (.not. present(sb1)) call assert(s == shouldbe)
  end subroutine scompare
!===================================================================================================================================
  subroutine compare(sok, message, sok1)
    ! Utility for all the test routines. Print to the screen what the last disp calls
    ! displayed, and assert that what was displayed matches either sok or sok1.
    character(*), intent(in) :: sok(:), sok1(:), message
    optional                 :: message, sok1
    character(100)           :: s, mess
    integer ios1, i
    rewind(8)
    compare_no = compare_no + 1
    if (present(message)) then
      mess = message
    else
      write(mess, '(a,"-",i0)') trim(assert_string), compare_no
    endif
    do i = 1,9999
      read(8,1,iostat=ios1) s
      if (ios1 < 0) exit
      call msg2(s)
      call assert(i <= size(sok), message)
      if (present(sok1))       call assert(sok(i) == s .or. sok1(i) == s, mess)
      if (.not. present(sok1)) call assert(sok(i) == s, mess)
    enddo
    call assert(i == size(sok) + 1, mess)
    call reopen_8
    1 format(A)
  end subroutine compare
!===================================================================================================================================
  subroutine reopen_8
    call close_8
    call open_8
  end subroutine reopen_8

  subroutine open_8
    open(8, file = 'testtmp.dat', status = 'replace')
  end subroutine open_8
  
  subroutine close_8
    close(8)
  end subroutine close_8
!===================================================================================================================================
  subroutine assert_init(st)
    ! Set assert-string (used by subroutine compare) to st, compare_no to zero and display st
    ! if verbose is true. If st is absent, set assert_string to ''
    character(*), optional, intent(in) :: st
    if (present(st)) then
      assert_string = st
      call msg2('')
      call msg1(st)
    else
      assert_string = ''
    endif
    compare_no = 0
  end subroutine assert_init
!===================================================================================================================================
  subroutine assert(s, msg)
    ! Assert that s is true. If not print "assertion failed" and msg if it is present
    logical, intent(in) :: s
    character(*), optional, intent(in) :: msg
    if (.not.s) then
      if (present(msg)) then
        print '(a, ": assertion failed")', trim(msg)
      else
        print '("assertion failed")'
      endif
      stop 2
    endif
  end subroutine assert
!===================================================================================================================================
END PROGRAM TEST_M_display
