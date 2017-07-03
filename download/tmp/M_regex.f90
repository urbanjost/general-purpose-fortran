!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
module M_regex
!-------------------------------------------------------------------
! Fortran interface to POSIX regex, using ISO_C_BINDING.
!
! Regex is defined as an API using C headers. It does not define the
! exact value of flag tokens, just the names. It also uses an opaque
! data structure and a declared numeric type for the match array.
! Therefore, the code must either be generated for each target
! platform, or it must use wrapper functions written in C.
!
! Fortran wrapper functions are also required to present a normal
! Fortran API, and to not require C conversions by the caller.
!
! The interface here is not strictly correct, because it does not
! explicitly convert Fortran strings to the C character kind.
! Fortran only supports conversion of string kinds by assignment,
! or by a rather slow internal WRITE. For now, the easiest approach
! is to assume that C and Fortran default  character kinds are the
! same. This is generally true, but UTF-8 strings are likely to
! cause problems.
!-------------------------------------------------------------------
! TODO:
! * More documentation.
! * Implement allocatable-length strings when commonly available.
! * Maybe store the matches array inside the regex_type structure?
!-------------------------------------------------------------------
use ISO_C_Binding, only: C_ptr, C_int, C_size_t, C_char, C_NULL_char, C_NULL_ptr
use ISO_Fortran_Env, only: ERROR_UNIT
use M_strings, only : s2c
implicit none
private

! Fortran regex structure holds a pointer to an opaque C structure
type regex_type
   type(C_ptr) :: preg
end type regex_type

! API:
public regcomp  ! subroutine regcomp(this,pattern,flags,status)                    ! Compile a regex into a regex object
public regexec  ! function regexec(this,string,matches,flags,status) result(match) ! Execute a compiled regex against a string
public regerror ! subroutine regerror(this,errcode,errmsg,errmsg_len)              ! Get the string message for a status error value
public regmatch !
public regfree  ! subroutine regfree(this)                                         ! Release
public regex_type

!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
contains
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    regcomp(3f) - [M_regex]Compile a regex into a regex object
!!##SYNOPSIS
!!
!!    subroutine regcomp(this,pattern,flags,status)
!!
!!      type(regex_type), intent(out) :: this           ! new regex object
!!      character(len=*), intent(in) :: pattern         ! regex pattern string
!!      character(len=*), intent(in), &
!!                           optional :: flags ! flag characters:
!!                                             ! x = extended regex (REG_EXTENDED)
!!                                             ! m = multi-line     (REG_NEWLINE)
!!                                             ! i = case-insensitive (REG_ICASE)
!!                                             ! n = no MATCH required (REG_NOSUB)
!!      integer, intent(out), optional :: status ! If absent, errors are fatal
!!##DESCRIPTION
!!##OPTIONS
!!##RETURNS
!!##EXAMPLE
!!
!===================================================================================================================================
subroutine regcomp(this,pattern,flags,status)
   interface
     subroutine C_regcomp(preg,pattern,flags,status) bind(C,name="C_regcomp")
       import
       type(C_ptr), intent(in), value :: preg
       character(len=1,kind=C_char), intent(in) :: pattern(*)
       character(len=1,kind=C_char), intent(in) :: flags(*)
       integer(C_int), intent(inout) :: status
     end subroutine C_regcomp
   end interface
   interface
     subroutine C_regalloc(preg_return) bind(C,name="C_regalloc")
       import
       type(C_ptr), intent(out) :: preg_return
     end subroutine C_regalloc
   end interface

   type(regex_type), intent(out)          :: this
   character(len=*), intent(in)           :: pattern
   character(len=*), intent(in), optional :: flags
   integer, intent(out), optional         :: status
   integer(C_int)                         :: status_
   character(len=10,kind=C_char)          :: flags_

   flags_=' '
   if (present(flags)) flags_=flags
   this%preg = C_NULL_ptr
   call C_regalloc(this%preg)
   call C_regcomp(this%preg, s2c(trim(pattern)), s2c(trim(flags)), status_)
   if (present(status)) then
     status=status_
   else if (status_/=0) then

     stop 'Regex runtime error: regcomp failed.'
   end if
end subroutine regcomp
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    regexec(3f) - [M_regex] Execute a compiled regex against a string
!!##SYNOPSIS
!!
!!    function regexec(this,string,matches,flags,status) result(match)
!!
!!      logical :: match ! .TRUE. if the pattern matched
!!      type(regex_type), intent(in) :: this ! regex object
!!      character(len=*), intent(in) :: string ! target string
!!      character(len=*), intent(in), &
!!                       optional :: flags ! flag characters (for partial lines):
!!                                         ! b = no beginning-of-line (REG_NOTBOL)
!!                                         ! e = no end-of-line (REG_NOTEOL)
!!      integer, intent(out), optional :: matches(:,:) ! match locations,
!!                                                     ! dimension(2,nmatch)
!!      integer, intent(out), optional :: status ! If absent, errors are fatal
!!##DESCRIPTION
!!##OPTIONS
!!##RETURNS
!!##EXAMPLE
!!
!===================================================================================================================================
logical function regexec(this,string,matches,flags,status) result(match)
   interface
      subroutine C_regexec(preg,string,nmatch,matches,flags,status) bind(C,name="C_regexec")
        import
        type(C_ptr), intent(in), value           :: preg
        character(len=1,kind=C_char), intent(in) :: string(*)
        integer(C_int), intent(in), value        :: nmatch
        integer(C_int), intent(out)              :: matches(2,nmatch)
        character(len=1,kind=C_char), intent(in) :: flags(*)
        integer(C_int), intent(out)              :: status
      end subroutine C_regexec
   end interface
   type(regex_type), intent(in)           :: this
   character(len=*), intent(in)           :: string
   character(len=*), intent(in), optional :: flags
   integer, intent(out), optional         :: matches(:,:)
   integer, intent(out), optional         :: status
! local
    integer(C_int) :: status_, matches_(2,1)
    character(len=10,kind=C_char) :: flags_
! begin
    flags_=' '
    if (present(flags))then
       flags_=flags
    endif
    if (present(matches)) then
      call C_regexec(this%preg, s2c(trim(string)), size(matches,2),matches, s2c(trim(flags_)), status_)
    else
      call C_regexec(this%preg, s2c(trim(string)), int(0,C_int),matches_, s2c(trim(flags_)), status_)
    end if
    match = status_==0
    if (present(status)) then
      status=status_
    else if (status_/=0.and.status_/=1) then
      stop 'Regex runtime error: regexec failed.'
    end if
  end function regexec
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    regmatch(3f) - [M_regex]
!!##SYNOPSIS
!!
!!##DESCRIPTION
!!##OPTIONS
!!##RETURNS
!!##EXAMPLE
!!
!===================================================================================================================================
function regmatch(match,string,matches)
integer, intent(in)                              :: match, matches(2,*)
character(len=*), intent(in)                     :: string
character(len=matches(2,match)-matches(1,match)) :: regmatch

   regmatch = string(matches(1,match)+1:matches(2,match))

end function regmatch
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    regerror(3f) - [M_regex] Get the string message for a status error value
!!##SYNOPSIS
!!
!!    subroutine regerror(this,errcode,errmsg,errmsg_len)
!!
!!      type(regex_type), intent(in) :: this
!!      integer, intent(in)          :: errcode
!!      character, intent(out)       :: errmsg
!!      integer, intent(out)         :: errmsg_len
!!
!!##DESCRIPTION
!!##OPTIONS
!!##RETURNS
!!##EXAMPLE
!!
!===================================================================================================================================
subroutine regerror(this,errcode,errmsg,errmsg_len)
   interface
      function C_regerror(errcode, preg, errbuf, errbuf_size) result(regerror) bind(C,name="regerror")
        import
        integer(C_size_t) :: regerror
        integer(C_int), value :: errcode
        type(C_ptr), intent(in), value :: preg
        character(len=1,kind=C_char), intent(out) :: errbuf
        integer(C_size_t), value :: errbuf_size
      end function C_regerror
   end interface
   type(regex_type), intent(in) :: this
   integer, intent(in)          :: errcode
   character, intent(out)       :: errmsg
   integer, intent(out)         :: errmsg_len

   errmsg_len = C_regerror(int(errcode,C_int), this%preg, errmsg, int(len(errmsg),C_size_t))

end subroutine regerror
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
!>
!!##NAME
!!    regfree(3f) - [M_regex] Release
!!##SYNOPSIS
!!
!!    subroutine regfree(this)
!!
!!      type(regex_type), intent(inout) :: this
!!
!!##DESCRIPTION
!!
!!      regfree(3f) frees any dynamically-allocated storage used by the internal form of an RE.
!!
!!      The regfree(3f) function frees any dynamically-allocated storage associated with the compiled RE pointed to by THIS.  The
!!      remaining regex_type is no longer a valid compiled RE and the effect of supplying it to regexec() or regerror() is undefined.
!!
!!##OPTIONS
!!##RETURNS
!!##EXAMPLE
!!
!!##SEE ALSO
!!      These routines implement IEEE Std 1003.2 ("POSIX.2") regular expressions ("RE"s); see re_format(7).
!!
!!      IEEE Std 1003.2 (``POSIX.2''), sections 2.8 (Regular Expression Notation) and B.5 (C Binding for Regular Expression Matching).
!===================================================================================================================================
subroutine regfree(this)

   interface
      subroutine C_regfree(preg) bind(C,name="regfree")
         import
         type(C_ptr), intent(in), value :: preg
      end subroutine C_regfree
   end interface

   type(regex_type), intent(inout) :: this
   call C_regfree(this%preg)
   this%preg = C_NULL_ptr
end subroutine regfree
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
end module M_regex
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
