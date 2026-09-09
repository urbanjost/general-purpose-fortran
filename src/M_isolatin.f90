module M_isolatin
! Unicode-related procedures not requiring compiler support of ISO-10646
! first presented in https://fortran-lang.discourse.group/t/how-to-use-utf-8-in-gfortran/9949
! including enhancements and latin support from Francois Jacq, 2025-08
!
use iso_fortran_env, only: error_unit, stderr=>error_unit
use M_unicode, only: utf8_to_codepoints,  codepoints_to_utf8
implicit none

private
public  :: isolatin_to_unicode, unicode_to_isolatin
public  :: utf8_to_isolatin,    isolatin_to_utf8

contains
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
pure subroutine isolatin_to_unicode(isolatin,unicode,nerr)
character           ,intent(in)  :: isolatin(:)
integer, allocatable,intent(out) :: unicode(:)
integer             ,intent(out) :: nerr

integer :: i, n, char_code

   nerr = 0
   n = size(isolatin)
   allocate(unicode(n))

   do i = 1, n
      char_code = ichar(isolatin(i))
      ! Only 8 characters do not correspond to unicode
      select case (char_code)
      case (164); unicode(i) = 8364 ! Symbol Euro
      case (166); unicode(i) = 352  ! S caron
      case (168); unicode(i) = 353  ! s caron
      case (180); unicode(i) = 381  ! Z caron
      case (184); unicode(i) = 382  ! z caron
      case (188); unicode(i) = 338  ! OE majuscule
      case (189); unicode(i) = 339  ! oe minuscule
      case (190); unicode(i) = 376  ! Y trema
      case default
         unicode(i) = char_code
      end select
   enddo

end subroutine isolatin_to_unicode
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
pure subroutine unicode_to_isolatin(unicode,isolatin,nerr)

integer              ,intent(in)  :: unicode(:)
character,allocatable,intent(out) :: isolatin(:)
integer              ,intent(out) :: nerr
integer                           :: i, n, cp

   nerr=0
   n = size(unicode)
   allocate(isolatin(n))

   do i = 1, n
      cp = unicode(i)
      select case (cp) ! 8 special characters
      case (8364); isolatin(i) = char(164) ! Euro
      case (352);  isolatin(i) = char(166) ! S caron
      case (353);  isolatin(i) = char(168) ! s caron
      case (381);  isolatin(i) = char(180) ! Z caron
      case (382);  isolatin(i) = char(184) ! z caron
      case (338);  isolatin(i) = char(188) ! OE majuscule
      case (339);  isolatin(i) = char(189) ! oe minuscule
      case (376);  isolatin(i) = char(190) ! Y trema
      case (0:163, 165, 167, 169:179, 181:183, 185:187, 191:255)
         isolatin(i) = char(cp)
      case default
         nerr=nerr+1
         isolatin(i) = '?' ! replacement character
      end select
   enddo

end subroutine unicode_to_isolatin
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
pure subroutine isolatin_to_utf8(isolatin,utf8,nerr)
character            ,intent(in)  :: isolatin(:)
character,allocatable,intent(out) :: utf8(:)
integer              ,intent(out) :: nerr
integer,allocatable :: unicode(:)

   call isolatin_to_unicode(isolatin,unicode,nerr)
   call codepoints_to_utf8(unicode,utf8,nerr)

end subroutine isolatin_to_utf8
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
pure subroutine utf8_to_isolatin(utf8,isolatin,nerr)
character            ,intent(in)  :: utf8(:)
character,allocatable,intent(out) :: isolatin(:)
integer              ,intent(out) :: nerr
integer,allocatable :: unicode(:)

   call utf8_to_codepoints(utf8,unicode,nerr)
   call unicode_to_isolatin(unicode,isolatin,nerr)

end subroutine utf8_to_isolatin
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!===================================================================================================================================
end module M_isolatin
