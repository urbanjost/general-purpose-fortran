










module M_vendor
private
public system_isatty
contains
!>  call compiler-specific ISATTY() function or return .FALSE.



    function system_isatty(lun)
    integer,intent(in) :: lun
    logical :: system_isatty
       system_isatty=.false.
    end function system_isatty
end module M_vendor
