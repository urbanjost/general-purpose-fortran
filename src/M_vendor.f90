module M_vendor
private
public system_isatty
contains
!>  call compiler-specific ISATTY() function or return .FALSE.
#undef ISATTY

#ifdef __INTEL_COMPILER
    function system_isatty(lun)
    use IFPORT
    integer,intent(in) :: lun
    logical :: system_isatty
       system_isatty=isatty(lun)
    end function system_isatty
#define ISATTY
#endif

#ifdef __NVCOMPILER_MAJOR__X
    ! __NVCOMPILER_MAJOR__ __NVCOMPILER_MINOR__ __NVCOMPILER_PATCHLEVEL__
    function system_isatty(lun)
    use DFPORT
    integer,intent(in) :: lun
    logical :: system_isatty
       system_isatty=isatty(lun)
    end function system_isatty
#define ISATTY
#endif

#ifdef __GFORTRAN__
    function system_isatty(lun)
    integer,intent(in) :: lun
    logical :: system_isatty
       system_isatty=isatty(lun)
    end function system_isatty
#define ISATTY
#endif
#ifndef ISATTY
    function system_isatty(lun)
    integer,intent(in) :: lun
    logical :: system_isatty
       system_isatty=.false.
    end function system_isatty
#define ISATTY
#endif
end module M_vendor
