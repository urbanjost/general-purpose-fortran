program runtest
use, intrinsic :: iso_fortran_env, only: &
& stdin => input_unit,   &
& stdout => output_unit, &
& stderr => error_unit
use, intrinsic :: iso_fortran_env, only: real_kinds,real32,real64
use, intrinsic :: iso_fortran_env, only: int8, int16, int32, int64
use M_framework, only : unit_test_start, unit_test, unit_test_msg
use M_framework, only : unit_test_end, unit_test_stop, unit_test_mode
use M_framework, only : unit_test_level, unit_test_flags
use M_framework, only : unit_test_expected
!use M_xxxx
implicit none
logical, parameter :: T=.true., F=.false.
logical            :: matched
integer,parameter  :: dp=kind(0.0d0)
integer,parameter :: array(2,3)= reshape([1, 2, 3, 4, 5, 6], [2, 3])
real(kind=dp) :: nan, inf
character(len=20) :: line
real(kind=dp),parameter :: PI = 3.14159265358979323846264338327950288419716939937510_dp
real(kind=dp),parameter :: d2r=PI/180.0_dp

line='NAN'; read(line,*) nan
line='Infinity'; read(line,*) inf

! optional call to change default modes
   call unit_test_mode(       &
       keep_going=T,           &
       flags=[0],              &
       luns=[stderr],          &
       command='',             &
       brief=F,                &
       match='',               &
       interactive=F,          &
       CMDLINE=T,              &
       debug=F)

   unit_test_level=0

call unit_test_start("intrinsics",msg="",matched=matched)
if(matched)then
call unit_test_expected('abs',       abs(-10), 10)
call unit_test_expected('abs',       abs(10), 10)
call unit_test_expected('abs',       abs(-5.0), 5.0)
call unit_test_expected('abs',       abs(5.0), 5.0)
call unit_test_expected('achar',     achar(65), 'A')
call unit_test_expected('acos',      acos(-1.0_dp),PI)
call unit_test_expected('acos',      acos( ( 1.0,  0.0) ), (0.0,-0.00) )
call unit_test_expected('acos',      acos(0.866_dp), 0.52364958093182890_dp)
call unit_test_expected('acos',      acos(-1.0_dp), 3.14159265358979323846264338327950288419716939937510d0)
call unit_test_expected('acosh',     abs(acosh(2.0) - 1.3169578969) < 1E-6, .true.)
call unit_test_expected("adjustl",   adjustl("    "), "    ")
call unit_test_expected("adjustl",   adjustl(""), "")
call unit_test_expected("adjustl",   adjustl("A"), "A")
call unit_test_expected("adjustl",   adjustl("   Hello  "), "Hello     ")
call unit_test_expected('adjustr',   adjustr("   Hello  "), "     Hello")
call unit_test_expected('aimag',     AIMAG((1.0, 2.0)), 2.0)
call unit_test_expected('aint',      AINT(3.7), 3.0)
call unit_test_expected('all',       ALL([.true., .true., .false.]), .false.)
call unit_test_expected('anint',     ANINT(3.7), 4.0)
call unit_test_expected('any',       any([.true., .true., .false.]), T)
call unit_test_expected('asin',      ASIN(0.5), 0.5235987756)
call unit_test_expected('asinh',     ASINH(1.0) - 0.8813735870 < 1.0e-6,.true.)
call unit_test_expected('atan2',     ATAN2(2.0, 1.0) - 1.1071487178 < 1.0e-6, .true.)
call unit_test_expected('atan',      atan(1.0)- 0.7853981634 < 1e-6, .true. )
call unit_test_expected('atanh',     atanh(0.5) - 0.5493061443 <1e-6, .true. )
call unit_test_expected('bessel_j0', bessel_j0(1.0)- 0.7651976866 <1e-6, .true. )
call unit_test_expected('bessel_j1', bessel_j1(1.0) - 0.4400505857 < 1E-6, .true. ) 
call unit_test_expected('bessel_y1', BESSEL_Y1(1.0) -0.7812128213 < 1E-6, .true. )
call unit_test_expected('bge',       BGE(10, 5), .true. )
call unit_test_expected('bgt',       BGT(10, 5), .true. )
call unit_test_expected('bit_size',  BIT_SIZE(10),32 )
call unit_test_expected('ble',       ble(5, 10), .true. )
call unit_test_expected('blt',       blt(5, 10), .true. )
call unit_test_expected('btest',     btest(9, 3), .true. )
call unit_test_expected('ceiling',   CEILING(3.7), 4 )
call unit_test_expected('char',      CHAR(65), 'A')
call unit_test_expected('cmplx',     CMPLX(1.0, 2.0), (1.0, 2.0) )
call unit_test_expected('conjg',     CONJG((1.0,2.0)), (1.0, -2.0) )
call unit_test_expected('cos',       abs(cos(0.5) - 0.8775825619) < 1E-6,.true.)
call unit_test_expected('cosh',      COSH(0.5) - 1.1276259652 < 1e-6 )
call unit_test_expected('dim',       DIM(5.0, 3.0),2.0)
call unit_test_expected('dot_product', DOT_PRODUCT( [1.0, 2.0, 3.0], [4.0, 5.0, 6.0] ), 32.0 )
call unit_test_expected('dprod',     DPROD(2.0, 3.0), 6.0_dp )
call unit_test_expected('erfc',      ERFC(0.5) - 0.479500122 < 1e-6 )
call unit_test_expected('erf',       ERF(0.5) - 0.520499878  < 1e-6 )
call unit_test_expected('exp',       EXP(1.0)- 2.718281828 < 1E-6 ) 
call unit_test_expected('float',     FLOAT(5), 5.0 )
call unit_test_expected('floor',     FLOOR(5.8), 5 )
call unit_test_expected('gamma',     GAMMA(5.0), 24.0 )
call unit_test_expected('hypot',     HYPOT(3.0, 4.0), 5.0 )
call unit_test_expected('iachar',    IACHAR('A'), 65 )
call unit_test_expected('iand',      iand(5,3),1)
call unit_test_expected('ichar',     ICHAR('A'), 65 )
call unit_test_expected('ieor',      IEOR(5, 3), 6 )
call unit_test_expected('int',       INT(3.7), 3 )
call unit_test_expected('ior',       IOR(5, 3), 7 )
call unit_test_expected('ishftc',    ISHFTC(5, 2), 20 )
call unit_test_expected('ishft',     ISHFT(5, 2), 20 )
call unit_test_expected('len_trim',  LEN_TRIM("Hello     "), 5 )
call unit_test_expected('lle',       LLE("Hello", "World"), .TRUE. )
call unit_test_expected('log10',     LOG10(100.0), 2.0 )
call unit_test_expected('repeat',    repeat('Hello',3),'HelloHelloHello')
call unit_test_expected('reshape',   reshape([1,2,3,4,5,6],[2,3]), array )
call unit_test_expected('scale',     scale(5.0,2), 20.0 )
call unit_test_expected('shape',     shape(array), [2,3] )
call unit_test_expected('shiftl',    shiftl(100,3), 800 )
call unit_test_expected('shiftl',    shiftl( [ 1431655765, -1431655766, -1 ],9), [ -1431655936, 1431655424, -512 ])
call unit_test_expected('shiftr',    shiftr(20,2), 5 )
call unit_test_expected('sign',      sign(2.5,-1.0), -2.5 )
call unit_test_expected('size',      [size(array,1),size(array,2),size(array)], [2,3,6])
call unit_test_expected('spacing',   spacing( 1.0), epsilon(1.0))
call unit_test_expected('sqrt',      sqrt( 4.0), 2.0 )
call unit_test_expected('sum',       SUM( [1, 2, 3, 4, 5]), 15 )
call unit_test_expected('tanh',      tanh(1.0), 0.7615941559557649 )
call unit_test_expected('tan',       tan(1.0), 1.557407724654902 )
call unit_test_expected('trailz',    trailz(8), 3)
call unit_test_expected('trim',      len_trim(trim("   Hello World   ")), 14)
call unit_test_expected('trim',      trim("   Hello World   "), "   Hello World")
call unit_test_expected('ubound',    ubound(array,2), 3)
call unit_test_expected('maskr',     maskr(5),31)
call unit_test_expected('max',       max(5,3),5)
call unit_test_expected('maxval',    maxval([1,5,2]),5)
call unit_test_expected('min',       min(5,3),3)
call unit_test_expected('minval',    minval([1,5,2]),1)
call unit_test_expected('mod',       mod(10,3),1)
call unit_test_expected('modulo',    modulo(10.5,3.0),1.5)
call unit_test_expected('nint',      nint(1.6),2)
test_null: block
   integer, pointer :: ptr => NULL()
   call unit_test_expected('null',      .not.associated(ptr))
endblock test_null
call unit_test_expected('pack',      pack([1,2,3,4,5], mask = [.true., .false., .true., .false., .true.]),[1, 3, 5])
call unit_test_expected('popcnt',    popcnt(5),2)
call unit_test_expected('product',   product([1,2,3]),6)
call unit_test_expected('rank',      rank(array),2)
call unit_test_expected('dshiftr',   dshiftr (1, 2**30, 2), 1342177280)
call unit_test_expected('dshiftl',   dshiftl (1, 2**30, 2), 5)
call unit_test_expected('index',     index ('abcdefghijklmnop','e'), 5)
call unit_test_expected('sin',       sin( 0.0 ), 0.00 )
call unit_test_expected('sin',       sin( PI ) - 0.00_dp <epsilon(0.0_dp) )

test_sinh: block
real(kind=dp),parameter :: x=-1.0_dp
call unit_test_expected('sinh',      sinh(x), -1.1752011936438014_dp)
call unit_test_expected('sinh',      sinh([ x, 2.0*x, x/3.0 ]) - &
        & [ -1.1752011936438014_dp, -3.6268604078470190_dp, -0.33954055725615012_dp ] .le. 2*epsilon(0.0_dp) )
!call unit_test_expected('sinh',      sinh(huge(0.0d0)),inf)  ! gfortran prevents overflow value from being used
call unit_test_expected('sinh',      sinh(nan) - NaN /= 0)
call unit_test_expected('sinh',      sinh(inf), Inf)
endblock test_sinh

endif
call unit_test_end("intrinsics",msg="")
   call test_suite_allocated()
   call test_suite_associated()
   call test_suite_atomic_add()
   call test_suite_atomic_and()
   call test_suite_atomic_cas()
   call test_suite_atomic_define()
   call test_suite_atomic_fetch_add()
   call test_suite_atomic_fetch_and()
   call test_suite_atomic_fetch_or()
   call test_suite_atomic_fetch_xor()
   call test_suite_atomic_or()
   call test_suite_atomic_ref()
   call test_suite_atomic_xor()
   call test_suite_bessel_jn()
   call test_suite_bessel_y0()
   call test_suite_bessel_y1()
   call test_suite_c_associated()
   call test_suite_c_f_pointer()
   call test_suite_c_f_procpointer()
   call test_suite_c_funloc()
   call test_suite_c_loc()
   call test_suite_co_broadcast()
   call test_suite_co_lbound()
   call test_suite_co_max()
   call test_suite_co_min()
   call test_suite_command_argument_count()
   call test_suite_compiler_options()
   call test_suite_compiler_version()
   call test_suite_co_reduce()
   call test_suite_co_sum()
   call test_suite_co_ubound()
   call test_suite_count()
   call test_suite_cpu_time()
   call test_suite_cshift()
   call test_suite_c_sizeof()
   call test_suite_date_and_time()
   call test_suite_dble()
   call test_suite_digits()
   call test_suite_eoshift()
   call test_suite_epsilon()
   call test_suite_erfc_scaled()
   call test_suite_event_query()
   call test_suite_execute_command_line()
   call test_suite_exponent()
   call test_suite_extends_type_of()
   call test_suite_findloc()
   call test_suite_fraction()
   call test_suite_get_command_argument()
   call test_suite_get_command()
   call test_suite_get_environment_variable()
   call test_suite_huge()
   call test_suite_iall()
   
   call test_suite_iany()
   call test_suite_ibclr()
   call test_suite_ibits()
   call test_suite_ibset()
   call test_suite_image_index()
   call test_suite_iparity()
   call test_suite_is_contiguous()
   call test_suite_is_iostat_end()
   call test_suite_is_iostat_eor()
   call test_suite_kind()
   call test_suite_lbound()
   call test_suite_lcobound()
   call test_suite_leadz()
   call test_suite_len()
   call test_suite_lge()
   call test_suite_lgt()
   call test_suite_llt()
   call test_suite_log()
   call test_suite_log_gamma()
   call test_suite_logical()
   call test_suite_maskl()
   call test_suite_matmul()
   call test_suite_maxexponent()
   call test_suite_maxloc()
   call test_suite_merge_bits()
   call test_suite_merge()
   call test_suite_minexponent()
   call test_suite_minloc()
   call test_suite_move_alloc()
   call test_suite_mvbits()
   call test_suite_nearest()
   call test_suite_new_line()
   call test_suite_norm2()
   call test_suite_not()
   call test_suite_num_images()
   call test_suite_out_of_range()
   call test_suite_parity()
   call test_suite_poppar()
   call test_suite_precision()
   call test_suite_present()
   call test_suite_radix()
   call test_suite_random_init()
   call test_suite_random_number()
   call test_suite_random_seed()
   call test_suite_range()
   call test_suite_real()
   call test_suite_reduce()
   call test_suite_rrspacing()
   call test_suite_same_type_as()
   call test_suite_scan()
   call test_suite_selected_char_kind()
   call test_suite_selected_int_kind()
   call test_suite_selected_real_kind()
   call test_suite_set_exponent()
   call test_suite_shifta()
   call test_suite_sngl()
   call test_suite_spread()
   call test_suite_storage_size()
   call test_suite_suite_M_calculator()
   call test_suite_system_clock()
   call test_suite_this_image()
   call test_suite_tiny()
   call test_suite_transfer()
   call test_suite_transpose()
   call test_suite_ucobound()
   call test_suite_unpack()
   call test_suite_verify()
   call unit_test_stop()

contains

subroutine test_suite_allocated()
   call unit_test_start("allocated",msg="",matched=matched)
   if(.not.matched)return
   !!call unit_test("allocated", 0 .eq. 0, "checking",100)
   call unit_test_end("allocated",msg="")
end subroutine test_suite_allocated

subroutine test_suite_associated()
   call unit_test_start("associated",msg="",matched=matched)
   if(.not.matched)return
   !!call unit_test("associated", 0 .eq. 0, "checking",100)
   call unit_test_end("associated",msg="")
end subroutine test_suite_associated

subroutine test_suite_atomic_add()
   call unit_test_start("atomic_add",msg="",matched=matched)
   if(.not.matched)return
   !!call unit_test("atomic_add", 0 .eq. 0, "checking",100)
   call unit_test_end("atomic_add",msg="")
end subroutine test_suite_atomic_add

subroutine test_suite_atomic_and()
   call unit_test_start("atomic_and",msg="",matched=matched)
   if(.not.matched)return
   !!call unit_test("atomic_and", 0 .eq. 0, "checking",100)
   call unit_test_end("atomic_and",msg="")
end subroutine test_suite_atomic_and

subroutine test_suite_atomic_cas()
   call unit_test_start("atomic_cas",msg="",matched=matched)
   if(.not.matched)return
   !!call unit_test("atomic_cas", 0 .eq. 0, "checking",100)
   call unit_test_end("atomic_cas",msg="")
end subroutine test_suite_atomic_cas

subroutine test_suite_atomic_define()
   call unit_test_start("atomic_define",msg="",matched=matched)
   if(.not.matched)return
   !!call unit_test("atomic_define", 0 .eq. 0, "checking",100)
   call unit_test_end("atomic_define",msg="")
end subroutine test_suite_atomic_define

subroutine test_suite_atomic_fetch_add()
   call unit_test_start("atomic_fetch_add",msg="",matched=matched)
   if(.not.matched)return
   !!call unit_test("atomic_fetch_add", 0 .eq. 0, "checking",100)
   call unit_test_end("atomic_fetch_add",msg="")
end subroutine test_suite_atomic_fetch_add

subroutine test_suite_atomic_fetch_and()
   call unit_test_start("atomic_fetch_and",msg="",matched=matched)
   if(.not.matched)return
   !!call unit_test("atomic_fetch_and", 0 .eq. 0, "checking",100)
   call unit_test_end("atomic_fetch_and",msg="")
end subroutine test_suite_atomic_fetch_and

subroutine test_suite_atomic_fetch_or()
   call unit_test_start("atomic_fetch_or",msg="",matched=matched)
   if(.not.matched)return
   !!call unit_test("atomic_fetch_or", 0 .eq. 0, "checking",100)
   call unit_test_end("atomic_fetch_or",msg="")
end subroutine test_suite_atomic_fetch_or

subroutine test_suite_atomic_fetch_xor()
   call unit_test_start("atomic_fetch_xor",msg="",matched=matched)
   if(.not.matched)return
   !!call unit_test("atomic_fetch_xor", 0 .eq. 0, "checking",100)
   call unit_test_end("atomic_fetch_xor",msg="")
end subroutine test_suite_atomic_fetch_xor

subroutine test_suite_atomic_or()
   call unit_test_start("atomic_or",msg="",matched=matched)
   if(.not.matched)return
   !!call unit_test("atomic_or", 0 .eq. 0, "checking",100)
   call unit_test_end("atomic_or",msg="")
end subroutine test_suite_atomic_or

subroutine test_suite_atomic_ref()
   call unit_test_start("atomic_ref",msg="",matched=matched)
   if(.not.matched)return
   !!call unit_test("atomic_ref", 0 .eq. 0, "checking",100)
   call unit_test_end("atomic_ref",msg="")
end subroutine test_suite_atomic_ref

subroutine test_suite_atomic_xor()
   call unit_test_start("atomic_xor",msg="",matched=matched)
   if(.not.matched)return
   !!call unit_test("atomic_xor", 0 .eq. 0, "checking",100)
   call unit_test_end("atomic_xor",msg="")
end subroutine test_suite_atomic_xor

subroutine test_suite_bessel_jn()
   call unit_test_start("bessel_jn",msg="",matched=matched)
   if(.not.matched)return
   !!call unit_test("bessel_jn", 0 .eq. 0, "checking",100)
   call unit_test_end("bessel_jn",msg="")
end subroutine test_suite_bessel_jn

subroutine test_suite_bessel_y0()
   call unit_test_start("bessel_y0",msg="",matched=matched)
   if(.not.matched)return
   !!call unit_test("bessel_y0", 0 .eq. 0, "checking",100)
   call unit_test_end("bessel_y0",msg="")
end subroutine test_suite_bessel_y0

subroutine test_suite_bessel_y1()
   call unit_test_start("bessel_y1",msg="",matched=matched)
   if(.not.matched)return
   !!call unit_test("bessel_y1", 0 .eq. 0, "checking",100)
   call unit_test_end("bessel_y1",msg="")
end subroutine test_suite_bessel_y1

subroutine test_suite_c_associated()
   call unit_test_start("c_associated",msg="",matched=matched)
   if(.not.matched)return
   !!call unit_test("c_associated", 0 .eq. 0, "checking",100)
   call unit_test_end("c_associated",msg="")
end subroutine test_suite_c_associated

subroutine test_suite_c_f_pointer()
   call unit_test_start("c_f_pointer",msg="",matched=matched)
   if(.not.matched)return
   !!call unit_test("c_f_pointer", 0 .eq. 0, "checking",100)
   call unit_test_end("c_f_pointer",msg="")
end subroutine test_suite_c_f_pointer

subroutine test_suite_c_f_procpointer()
   call unit_test_start("c_f_procpointer",msg="",matched=matched)
   if(.not.matched)return
   !!call unit_test("c_f_procpointer", 0 .eq. 0, "checking",100)
   call unit_test_end("c_f_procpointer",msg="")
end subroutine test_suite_c_f_procpointer

subroutine test_suite_c_funloc()
   call unit_test_start("c_funloc",msg="",matched=matched)
   if(.not.matched)return
   !!call unit_test("c_funloc", 0 .eq. 0, "checking",100)
   call unit_test_end("c_funloc",msg="")
end subroutine test_suite_c_funloc

subroutine test_suite_c_loc()
   call unit_test_start("c_loc",msg="",matched=matched)
   if(.not.matched)return
   !!call unit_test("c_loc", 0 .eq. 0, "checking",100)
   call unit_test_end("c_loc",msg="")
end subroutine test_suite_c_loc

subroutine test_suite_co_broadcast()
   call unit_test_start("co_broadcast",msg="",matched=matched)
   if(.not.matched)return
   !!call unit_test("co_broadcast", 0 .eq. 0, "checking",100)
   call unit_test_end("co_broadcast",msg="")
end subroutine test_suite_co_broadcast

subroutine test_suite_co_lbound()
   call unit_test_start("co_lbound",msg="",matched=matched)
   if(.not.matched)return
   !!call unit_test("co_lbound", 0 .eq. 0, "checking",100)
   call unit_test_end("co_lbound",msg="")
end subroutine test_suite_co_lbound

subroutine test_suite_co_max()
   call unit_test_start("co_max",msg="",matched=matched)
   if(.not.matched)return
   !!call unit_test("co_max", 0 .eq. 0, "checking",100)
   call unit_test_end("co_max",msg="")
end subroutine test_suite_co_max

subroutine test_suite_co_min()
   call unit_test_start("co_min",msg="",matched=matched)
   if(.not.matched)return
   !!call unit_test("co_min", 0 .eq. 0, "checking",100)
   call unit_test_end("co_min",msg="")
end subroutine test_suite_co_min

subroutine test_suite_command_argument_count()
   call unit_test_start("command_argument_count",msg="",matched=matched)
   if(.not.matched)return
   !!call unit_test("command_argument_count", 0 .eq. 0, "checking",100)
   call unit_test_end("command_argument_count",msg="")
end subroutine test_suite_command_argument_count

subroutine test_suite_compiler_options()
   call unit_test_start("compiler_options",msg="",matched=matched)
   if(.not.matched)return
   !!call unit_test("compiler_options", 0 .eq. 0, "checking",100)
   call unit_test_end("compiler_options",msg="")
end subroutine test_suite_compiler_options

subroutine test_suite_compiler_version()
   call unit_test_start("compiler_version",msg="",matched=matched)
   if(.not.matched)return
   !!call unit_test("compiler_version", 0 .eq. 0, "checking",100)
   call unit_test_end("compiler_version",msg="")
end subroutine test_suite_compiler_version

subroutine test_suite_co_reduce()
   call unit_test_start("co_reduce",msg="",matched=matched)
   if(.not.matched)return
   !!call unit_test("co_reduce", 0 .eq. 0, "checking",100)
   call unit_test_end("co_reduce",msg="")
end subroutine test_suite_co_reduce

subroutine test_suite_cos()
   call unit_test_start("cos",msg="",matched=matched)
   if(.not.matched)return
   call unit_test_end("cos",msg="")
end subroutine test_suite_cos

subroutine test_suite_co_sum()
   call unit_test_start("co_sum",msg="",matched=matched)
   if(.not.matched)return
   !!call unit_test("co_sum", 0 .eq. 0, "checking",100)
   call unit_test_end("co_sum",msg="")
end subroutine test_suite_co_sum

subroutine test_suite_co_ubound()
   call unit_test_start("co_ubound",msg="",matched=matched)
   if(.not.matched)return
   !!call unit_test("co_ubound", 0 .eq. 0, "checking",100)
   call unit_test_end("co_ubound",msg="")
end subroutine test_suite_co_ubound

subroutine test_suite_count()
   call unit_test_start("count",msg="",matched=matched)
   if(.not.matched)return
   !!call unit_test("count", 0 .eq. 0, "checking",100)
   call unit_test_end("count",msg="")
end subroutine test_suite_count

subroutine test_suite_cpu_time()
   call unit_test_start("cpu_time",msg="",matched=matched)
   if(.not.matched)return
   !!call unit_test("cpu_time", 0 .eq. 0, "checking",100)
   call unit_test_end("cpu_time",msg="")
end subroutine test_suite_cpu_time

subroutine test_suite_cshift()
   call unit_test_start("cshift",msg="",matched=matched)
   if(.not.matched)return
   !!call unit_test("cshift", 0 .eq. 0, "checking",100)
   call unit_test_end("cshift",msg="")
end subroutine test_suite_cshift

subroutine test_suite_c_sizeof()
   call unit_test_start("c_sizeof",msg="",matched=matched)
   if(.not.matched)return
   !!call unit_test("c_sizeof", 0 .eq. 0, "checking",100)
   call unit_test_end("c_sizeof",msg="")
end subroutine test_suite_c_sizeof

subroutine test_suite_date_and_time()
   call unit_test_start("date_and_time",msg="",matched=matched)
   if(.not.matched)return
   !!call unit_test("date_and_time", 0 .eq. 0, "checking",100)
   call unit_test_end("date_and_time",msg="")
end subroutine test_suite_date_and_time

subroutine test_suite_dble()
   call unit_test_start("dble",msg="",matched=matched)
   if(.not.matched)return
   !!call unit_test("dble", 0 .eq. 0, "checking",100)
   call unit_test_end("dble",msg="")
end subroutine test_suite_dble

subroutine test_suite_digits()
   call unit_test_start("digits",msg="",matched=matched)
   if(.not.matched)return
   !!call unit_test("digits", 0 .eq. 0, "checking",100)
   call unit_test_end("digits",msg="")
end subroutine test_suite_digits

subroutine test_suite_eoshift()
   call unit_test_start("eoshift",msg="",matched=matched)
   if(.not.matched)return
   !!call unit_test("eoshift", 0 .eq. 0, "checking",100)
   call unit_test_end("eoshift",msg="")
end subroutine test_suite_eoshift

subroutine test_suite_epsilon()
   call unit_test_start("epsilon",msg="",matched=matched)
   if(.not.matched)return
   !!call unit_test("epsilon", 0 .eq. 0, "checking",100)
   call unit_test_end("epsilon",msg="")
end subroutine test_suite_epsilon

subroutine test_suite_erfc_scaled()
   call unit_test_start("erfc_scaled",msg="",matched=matched)
   if(.not.matched)return
   !!call unit_test("erfc_scaled", 0 .eq. 0, "checking",100)
   call unit_test_end("erfc_scaled",msg="")
end subroutine test_suite_erfc_scaled

subroutine test_suite_event_query()
   call unit_test_start("event_query",msg="",matched=matched)
   if(.not.matched)return
   !!call unit_test("event_query", 0 .eq. 0, "checking",100)
   call unit_test_end("event_query",msg="")
end subroutine test_suite_event_query

subroutine test_suite_execute_command_line()
   call unit_test_start("execute_command_line",msg="",matched=matched)
   if(.not.matched)return
   !!call unit_test("execute_command_line", 0 .eq. 0, "checking",100)
   call unit_test_end("execute_command_line",msg="")
end subroutine test_suite_execute_command_line

subroutine test_suite_exponent()
   call unit_test_start("exponent",msg="",matched=matched)
   if(.not.matched)return
   !!call unit_test("exponent", 0 .eq. 0, "checking",100)
   call unit_test_end("exponent",msg="")
end subroutine test_suite_exponent

subroutine test_suite_extends_type_of()
   call unit_test_start("extends_type_of",msg="",matched=matched)
   if(.not.matched)return
   !!call unit_test("extends_type_of", 0 .eq. 0, "checking",100)
   call unit_test_end("extends_type_of",msg="")
end subroutine test_suite_extends_type_of

subroutine test_suite_findloc()
   call unit_test_start("findloc",msg="",matched=matched)
   if(.not.matched)return
   !!call unit_test("findloc", 0 .eq. 0, "checking",100)
   call unit_test_end("findloc",msg="")
end subroutine test_suite_findloc

subroutine test_suite_fraction()
   call unit_test_start("fraction",msg="",matched=matched)
   if(.not.matched)return
   !!call unit_test("fraction", 0 .eq. 0, "checking",100)
   call unit_test_end("fraction",msg="")
end subroutine test_suite_fraction

subroutine test_suite_get_command_argument()
   call unit_test_start("get_command_argument",msg="",matched=matched)
   if(.not.matched)return
   !!call unit_test("get_command_argument", 0 .eq. 0, "checking",100)
   call unit_test_end("get_command_argument",msg="")
end subroutine test_suite_get_command_argument

subroutine test_suite_get_command()
   call unit_test_start("get_command",msg="",matched=matched)
   if(.not.matched)return
   !!call unit_test("get_command", 0 .eq. 0, "checking",100)
   call unit_test_end("get_command",msg="")
end subroutine test_suite_get_command

subroutine test_suite_get_environment_variable()
   call unit_test_start("get_environment_variable",msg="",matched=matched)
   if(.not.matched)return
   !!call unit_test("get_environment_variable", 0 .eq. 0, "checking",100)
   call unit_test_end("get_environment_variable",msg="")
end subroutine test_suite_get_environment_variable

subroutine test_suite_huge()
   call unit_test_start("huge",msg="",matched=matched)
   if(.not.matched)return
   !!call unit_test("huge", 0 .eq. 0, "checking",100)
   call unit_test_end("huge",msg="")
end subroutine test_suite_huge

subroutine test_suite_iall()
   call unit_test_start("iall",msg="",matched=matched)
   if(.not.matched)return
   !!call unit_test("iall", 0 .eq. 0, "checking",100)
   call unit_test_end("iall",msg="")
end subroutine test_suite_iall

subroutine test_suite_iany()
   call unit_test_start("iany",msg="",matched=matched)
   if(.not.matched)return
   !!call unit_test("iany", 0 .eq. 0, "checking",100)
   call unit_test_end("iany",msg="")
end subroutine test_suite_iany

subroutine test_suite_ibclr()
   call unit_test_start("ibclr",msg="",matched=matched)
   if(.not.matched)return
   !!call unit_test("ibclr", 0 .eq. 0, "checking",100)
   call unit_test_end("ibclr",msg="")
end subroutine test_suite_ibclr

subroutine test_suite_ibits()
   call unit_test_start("ibits",msg="",matched=matched)
   if(.not.matched)return
   !!call unit_test("ibits", 0 .eq. 0, "checking",100)
   call unit_test_end("ibits",msg="")
end subroutine test_suite_ibits

subroutine test_suite_ibset()
   call unit_test_start("ibset",msg="",matched=matched)
   if(.not.matched)return
   !!call unit_test("ibset", 0 .eq. 0, "checking",100)
   call unit_test_end("ibset",msg="")
end subroutine test_suite_ibset

subroutine test_suite_image_index()
   call unit_test_start("image_index",msg="",matched=matched)
   if(.not.matched)return
   !!call unit_test("image_index", 0 .eq. 0, "checking",100)
   call unit_test_end("image_index",msg="")
end subroutine test_suite_image_index

subroutine test_suite_iparity()
   call unit_test_start("iparity",msg="",matched=matched)
   if(.not.matched)return
   !!call unit_test("iparity", 0 .eq. 0, "checking",100)
   call unit_test_end("iparity",msg="")
end subroutine test_suite_iparity

subroutine test_suite_is_contiguous()
   call unit_test_start("is_contiguous",msg="",matched=matched)
   if(.not.matched)return
   !!call unit_test("is_contiguous", 0 .eq. 0, "checking",100)
   call unit_test_end("is_contiguous",msg="")
end subroutine test_suite_is_contiguous

subroutine test_suite_is_iostat_end()
   call unit_test_start("is_iostat_end",msg="",matched=matched)
   if(.not.matched)return
   !!call unit_test("is_iostat_end", 0 .eq. 0, "checking",100)
   call unit_test_end("is_iostat_end",msg="")
end subroutine test_suite_is_iostat_end

subroutine test_suite_is_iostat_eor()
   call unit_test_start("is_iostat_eor",msg="",matched=matched)
   if(.not.matched)return
   !!call unit_test("is_iostat_eor", 0 .eq. 0, "checking",100)
   call unit_test_end("is_iostat_eor",msg="")
end subroutine test_suite_is_iostat_eor

subroutine test_suite_kind()
   call unit_test_start("kind",msg="",matched=matched)
   if(.not.matched)return
   !!call unit_test("kind", 0 .eq. 0, "checking",100)
   call unit_test_end("kind",msg="")
end subroutine test_suite_kind

subroutine test_suite_lbound()
   call unit_test_start("lbound",msg="",matched=matched)
   if(.not.matched)return
   !!call unit_test("lbound", 0 .eq. 0, "checking",100)
   call unit_test_end("lbound",msg="")
end subroutine test_suite_lbound

subroutine test_suite_lcobound()
   call unit_test_start("lcobound",msg="",matched=matched)
   if(.not.matched)return
   !!call unit_test("lcobound", 0 .eq. 0, "checking",100)
   call unit_test_end("lcobound",msg="")
end subroutine test_suite_lcobound

subroutine test_suite_leadz()
   call unit_test_start("leadz",msg="",matched=matched)
   if(.not.matched)return
   !!call unit_test("leadz", 0 .eq. 0, "checking",100)
   call unit_test_end("leadz",msg="")
end subroutine test_suite_leadz

subroutine test_suite_len()
   call unit_test_start("len",msg="",matched=matched)
   if(.not.matched)return
   !!call unit_test("len", 0 .eq. 0, "checking",100)
   call unit_test_end("len",msg="")
end subroutine test_suite_len

subroutine test_suite_lge()
   call unit_test_start("lge",msg="",matched=matched)
   if(.not.matched)return
   !!call unit_test("lge", 0 .eq. 0, "checking",100)
   call unit_test_end("lge",msg="")
end subroutine test_suite_lge

subroutine test_suite_lgt()
   call unit_test_start("lgt",msg="",matched=matched)
   if(.not.matched)return
   !!call unit_test("lgt", 0 .eq. 0, "checking",100)
   call unit_test_end("lgt",msg="")
end subroutine test_suite_lgt

subroutine test_suite_llt()
   call unit_test_start("llt",msg="",matched=matched)
   if(.not.matched)return
   !!call unit_test("llt", 0 .eq. 0, "checking",100)
   call unit_test_end("llt",msg="")
end subroutine test_suite_llt

subroutine test_suite_log()
   call unit_test_start("log",msg="",matched=matched)
   if(.not.matched)return
   !!call unit_test("log", 0 .eq. 0, "checking",100)
   call unit_test_end("log",msg="")
end subroutine test_suite_log

subroutine test_suite_log_gamma()
   call unit_test_start("log_gamma",msg="",matched=matched)
   if(.not.matched)return
   !!call unit_test("log_gamma", 0 .eq. 0, "checking",100)
   call unit_test_end("log_gamma",msg="")
end subroutine test_suite_log_gamma

subroutine test_suite_logical()
   call unit_test_start("logical",msg="",matched=matched)
   if(.not.matched)return
   !!call unit_test("logical", 0 .eq. 0, "checking",100)
   call unit_test_end("logical",msg="")
end subroutine test_suite_logical

subroutine test_suite_maskl()
   call unit_test_start("maskl",msg="",matched=matched)
   if(.not.matched)return
   !!call unit_test("maskl", 0 .eq. 0, "checking",100)
   call unit_test_end("maskl",msg="")
end subroutine test_suite_maskl

subroutine test_suite_matmul()
   call unit_test_start("matmul",msg="",matched=matched)
   if(.not.matched)return
   !!call unit_test("matmul", 0 .eq. 0, "checking",100)
   call unit_test_end("matmul",msg="")
end subroutine test_suite_matmul

subroutine test_suite_maxexponent()
   call unit_test_start("maxexponent",msg="",matched=matched)
   if(.not.matched)return
   !!call unit_test("maxexponent", 0 .eq. 0, "checking",100)
   call unit_test_end("maxexponent",msg="")
end subroutine test_suite_maxexponent

subroutine test_suite_maxloc()
   call unit_test_start("maxloc",msg="",matched=matched)
   if(.not.matched)return
   !!call unit_test("maxloc", 0 .eq. 0, "checking",100)
   call unit_test_end("maxloc",msg="")
end subroutine test_suite_maxloc

subroutine test_suite_merge_bits()
   call unit_test_start("merge_bits",msg="",matched=matched)
   if(.not.matched)return
   !!call unit_test("merge_bits", 0 .eq. 0, "checking",100)
   call unit_test_end("merge_bits",msg="")
end subroutine test_suite_merge_bits

subroutine test_suite_merge()
   call unit_test_start("merge",msg="",matched=matched)
   if(.not.matched)return
   !!call unit_test("merge", 0 .eq. 0, "checking",100)
   call unit_test_end("merge",msg="")
end subroutine test_suite_merge

subroutine test_suite_minexponent()
   call unit_test_start("minexponent",msg="",matched=matched)
   if(.not.matched)return
   !!call unit_test("minexponent", 0 .eq. 0, "checking",100)
   call unit_test_end("minexponent",msg="")
end subroutine test_suite_minexponent

subroutine test_suite_minloc()
   call unit_test_start("minloc",msg="",matched=matched)
   if(.not.matched)return
   !!call unit_test("minloc", 0 .eq. 0, "checking",100)
   call unit_test_end("minloc",msg="")
end subroutine test_suite_minloc

subroutine test_suite_move_alloc()
   call unit_test_start("move_alloc",msg="",matched=matched)
   if(.not.matched)return
   !!call unit_test("move_alloc", 0 .eq. 0, "checking",100)
   call unit_test_end("move_alloc",msg="")
end subroutine test_suite_move_alloc

subroutine test_suite_mvbits()
   call unit_test_start("mvbits",msg="",matched=matched)
   if(.not.matched)return
   !!call unit_test("mvbits", 0 .eq. 0, "checking",100)
   call unit_test_end("mvbits",msg="")
end subroutine test_suite_mvbits

subroutine test_suite_nearest()
   call unit_test_start("nearest",msg="",matched=matched)
   if(.not.matched)return
   !!call unit_test("nearest", 0 .eq. 0, "checking",100)
   call unit_test_end("nearest",msg="")
end subroutine test_suite_nearest

subroutine test_suite_new_line()
   call unit_test_start("new_line",msg="",matched=matched)
   if(.not.matched)return
   !!call unit_test("new_line", 0 .eq. 0, "checking",100)
   call unit_test_end("new_line",msg="")
end subroutine test_suite_new_line

subroutine test_suite_norm2()
   call unit_test_start("norm2",msg="",matched=matched)
   if(.not.matched)return
   !!call unit_test("norm2", 0 .eq. 0, "checking",100)
   call unit_test_end("norm2",msg="")
end subroutine test_suite_norm2

subroutine test_suite_not()
   call unit_test_start("not",msg="",matched=matched)
   if(.not.matched)return
   !!call unit_test("not", 0 .eq. 0, "checking",100)
   call unit_test_end("not",msg="")
end subroutine test_suite_not

subroutine test_suite_num_images()
   call unit_test_start("num_images",msg="",matched=matched)
   if(.not.matched)return
   !!call unit_test("num_images", 0 .eq. 0, "checking",100)
   call unit_test_end("num_images",msg="")
end subroutine test_suite_num_images

subroutine test_suite_out_of_range()
   call unit_test_start("out_of_range",msg="",matched=matched)
   if(.not.matched)return
   !!call unit_test("out_of_range", 0 .eq. 0, "checking",100)
   call unit_test_end("out_of_range",msg="")
end subroutine test_suite_out_of_range

subroutine test_suite_parity()
   call unit_test_start("parity",msg="",matched=matched)
   if(.not.matched)return
   !!call unit_test("parity", 0 .eq. 0, "checking",100)
   call unit_test_end("parity",msg="")
end subroutine test_suite_parity

subroutine test_suite_poppar()
   call unit_test_start("poppar",msg="",matched=matched)
   if(.not.matched)return
   !!call unit_test("poppar", 0 .eq. 0, "checking",100)
   call unit_test_end("poppar",msg="")
end subroutine test_suite_poppar

subroutine test_suite_precision()
   call unit_test_start("precision",msg="",matched=matched)
   if(.not.matched)return
   !!call unit_test("precision", 0 .eq. 0, "checking",100)
   call unit_test_end("precision",msg="")
end subroutine test_suite_precision

subroutine test_suite_present()
   call unit_test_start("present",msg="",matched=matched)
   if(.not.matched)return
   !!call unit_test("present", 0 .eq. 0, "checking",100)
   call unit_test_end("present",msg="")
end subroutine test_suite_present

subroutine test_suite_radix()
   call unit_test_start("radix",msg="",matched=matched)
   if(.not.matched)return
   !!call unit_test("radix", 0 .eq. 0, "checking",100)
   call unit_test_end("radix",msg="")
end subroutine test_suite_radix

subroutine test_suite_random_init()
   call unit_test_start("random_init",msg="",matched=matched)
   if(.not.matched)return
   !!call unit_test("random_init", 0 .eq. 0, "checking",100)
   call unit_test_end("random_init",msg="")
end subroutine test_suite_random_init

subroutine test_suite_random_number()
   call unit_test_start("random_number",msg="",matched=matched)
   if(.not.matched)return
   !!call unit_test("random_number", 0 .eq. 0, "checking",100)
   call unit_test_end("random_number",msg="")
end subroutine test_suite_random_number

subroutine test_suite_random_seed()
   call unit_test_start("random_seed",msg="",matched=matched)
   if(.not.matched)return
   !!call unit_test("random_seed", 0 .eq. 0, "checking",100)
   call unit_test_end("random_seed",msg="")
end subroutine test_suite_random_seed

subroutine test_suite_range()
   call unit_test_start("range",msg="",matched=matched)
   if(.not.matched)return
   !!call unit_test("range", 0 .eq. 0, "checking",100)
   call unit_test_end("range",msg="")
end subroutine test_suite_range

subroutine test_suite_real()
   call unit_test_start("real",msg="",matched=matched)
   if(.not.matched)return
   !!call unit_test("real", 0 .eq. 0, "checking",100)
   call unit_test_end("real",msg="")
end subroutine test_suite_real

subroutine test_suite_reduce()
   call unit_test_start("reduce",msg="",matched=matched)
   if(.not.matched)return
   !!call unit_test("reduce", 0 .eq. 0, "checking",100)
   call unit_test_end("reduce",msg="")
end subroutine test_suite_reduce

subroutine test_suite_rrspacing()
   call unit_test_start("rrspacing",msg="",matched=matched)
   if(.not.matched)return
   !!call unit_test("rrspacing", 0 .eq. 0, "checking",100)
   call unit_test_end("rrspacing",msg="")
end subroutine test_suite_rrspacing

subroutine test_suite_same_type_as()
   call unit_test_start("same_type_as",msg="",matched=matched)
   if(.not.matched)return
   !!call unit_test("same_type_as", 0 .eq. 0, "checking",100)
   call unit_test_end("same_type_as",msg="")
end subroutine test_suite_same_type_as

subroutine test_suite_scan()
   call unit_test_start("scan",msg="",matched=matched)
   if(.not.matched)return
   !!call unit_test("scan", 0 .eq. 0, "checking",100)
   call unit_test_end("scan",msg="")
end subroutine test_suite_scan

subroutine test_suite_selected_char_kind()
   call unit_test_start("selected_char_kind",msg="",matched=matched)
   if(.not.matched)return
   !!call unit_test("selected_char_kind", 0 .eq. 0, "checking",100)
   call unit_test_end("selected_char_kind",msg="")
end subroutine test_suite_selected_char_kind

subroutine test_suite_selected_int_kind()
   call unit_test_start("selected_int_kind",msg="",matched=matched)
   if(.not.matched)return
   !!call unit_test("selected_int_kind", 0 .eq. 0, "checking",100)
   call unit_test_end("selected_int_kind",msg="")
end subroutine test_suite_selected_int_kind

subroutine test_suite_selected_real_kind()
   call unit_test_start("selected_real_kind",msg="",matched=matched)
   if(.not.matched)return
   !!call unit_test("selected_real_kind", 0 .eq. 0, "checking",100)
   call unit_test_end("selected_real_kind",msg="")
end subroutine test_suite_selected_real_kind

subroutine test_suite_set_exponent()
   call unit_test_start("set_exponent",msg="",matched=matched)
   if(.not.matched)return
   !!call unit_test("set_exponent", 0 .eq. 0, "checking",100)
   call unit_test_end("set_exponent",msg="")
end subroutine test_suite_set_exponent

subroutine test_suite_shifta()
   call unit_test_start("shifta",msg="",matched=matched)
   if(.not.matched)return
   !!call unit_test("shifta", 0 .eq. 0, "checking",100)
   call unit_test_end("shifta",msg="")
end subroutine test_suite_shifta

subroutine test_suite_sngl()
   call unit_test_start("sngl",msg="",matched=matched)
   if(.not.matched)return
   !!call unit_test("sngl", 0 .eq. 0, "checking",100)
   call unit_test_end("sngl",msg="")
end subroutine test_suite_sngl

subroutine test_suite_spread()
   call unit_test_start("spread",msg="",matched=matched)
   if(.not.matched)return
   !!call unit_test("spread", 0 .eq. 0, "checking",100)
   call unit_test_end("spread",msg="")
end subroutine test_suite_spread

subroutine test_suite_storage_size()
   call unit_test_start("storage_size",msg="",matched=matched)
   if(.not.matched)return
   !!call unit_test("storage_size", 0 .eq. 0, "checking",100)
   call unit_test_end("storage_size",msg="")
end subroutine test_suite_storage_size

subroutine test_suite_suite_M_calculator()
   call unit_test_start("suite_M_calculator",msg="",matched=matched)
   if(.not.matched)return
   !!call unit_test("suite_M_calculator", 0 .eq. 0, "checking",100)
   call unit_test_end("suite_M_calculator",msg="")
end subroutine test_suite_suite_M_calculator

subroutine test_suite_system_clock()
   call unit_test_start("system_clock",msg="",matched=matched)
   if(.not.matched)return
   !!call unit_test("system_clock", 0 .eq. 0, "checking",100)
   call unit_test_end("system_clock",msg="")
end subroutine test_suite_system_clock

subroutine test_suite_this_image()
   call unit_test_start("this_image",msg="",matched=matched)
   if(.not.matched)return
   !!call unit_test("this_image", 0 .eq. 0, "checking",100)
   call unit_test_end("this_image",msg="")
end subroutine test_suite_this_image

subroutine test_suite_tiny()
   call unit_test_start("tiny",msg="",matched=matched)
   if(.not.matched)return
   !!call unit_test("tiny", 0 .eq. 0, "checking",100)
   call unit_test_end("tiny",msg="")
end subroutine test_suite_tiny

subroutine test_suite_transfer()
   call unit_test_start("transfer",msg="",matched=matched)
   if(.not.matched)return
   !!call unit_test("transfer", 0 .eq. 0, "checking",100)
   call unit_test_end("transfer",msg="")
end subroutine test_suite_transfer

subroutine test_suite_transpose()
   call unit_test_start("transpose",msg="",matched=matched)
   if(.not.matched)return
   !!call unit_test("transpose", 0 .eq. 0, "checking",100)
   call unit_test_end("transpose",msg="")
end subroutine test_suite_transpose

subroutine test_suite_ucobound()
   call unit_test_start("ucobound",msg="",matched=matched)
   if(.not.matched)return
   !!call unit_test("ucobound", 0 .eq. 0, "checking",100)
   call unit_test_end("ucobound",msg="")
end subroutine test_suite_ucobound

subroutine test_suite_unpack()
   call unit_test_start("unpack",msg="",matched=matched)
   if(.not.matched)return
   !!call unit_test("unpack", 0 .eq. 0, "checking",100)
   call unit_test_end("unpack",msg="")
end subroutine test_suite_unpack

subroutine test_suite_verify()
   call unit_test_start("verify",msg="",matched=matched)
   if(.not.matched)return
   !!call unit_test("verify", 0 .eq. 0, "checking",100)
   call unit_test_end("verify",msg="")
end subroutine test_suite_verify

end program runtest

