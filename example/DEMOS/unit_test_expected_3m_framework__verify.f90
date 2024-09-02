        program demo_unit_test_expected
        use, intrinsic :: iso_fortran_env, only: &
        & stdin => input_unit, stdout => output_unit, stderr => error_unit
        use M_framework, only:                &
        &  unit_test_mode,                    &
        &  start     =>  unit_test_start,     &
        &  expected  =>  unit_test_expected,  &
        &  stop      =>  unit_test_stop,      &
        & unit_test_level, unit_test_flags
        implicit none
        logical, parameter :: T=.true., F=.false.
        ! optional call to change default modes
           call unit_test_mode(  &
               keep_going=T,     &
               flags=[0],        &
               luns=[stderr],    &
               command='',       &
               brief=F,          &
               match='',         &
               interactive=F,    &
               CMDLINE=T,        &
               debug=F)

           unit_test_level=0
           ! unit tests for ABS(3f) intrinsic
           call start('abs')
           ! integer
           call expected('abs',abs(-10),10)
           call expected('abs',abs( 10),10)
           ! real and elemental
           call expected('abs',abs( [-10.0, 10.0]),10.0)
           ! complex
           call expected('abs',abs(( 3.0,-4.0)),5.0)
           call expected('abs',abs((-3.0, 4.0)),5.0)
           call expected('abs',abs((-3.0,-4.0)),5.0)
           call expected('abs',abs(( 3.0, 4.0)),5.0)
           call stop('abs')
        end program demo_unit_test_expected
