!-------------------------------------------------------------------------------
! @(#) A 64-bit KISS random number generator by George Margaglia.
! From: FortranWiki.org
! Originally posted to comp.lang.fortran in the message 64-bit KISS RNGs.
! No license was specified.
!
! Revised on April 14, 2010 21:40:44 by Jason Blevins (75.178.9.182)
! This version was modified by Jason Blevins to use "implicit none"
! and to portably declare the 64-bit/eight-byte integer type.
!-------------------------------------------------------------------------------
!-------------------------------------------------------------------------------
function kiss64()
    character(len=*),parameter :: ident="@(#)kiss64(3f):64-bit KISS random number generator by George Margaglia."
    integer, parameter         :: i8b = selected_int_kind(18)  ! eight-byte integer
    integer(i8b), save         :: x, y, z, c
    integer(i8b)               :: t, k, m, s, kiss64
    data x, y, z, c &
         / 1234567890987654321_i8b, &
           362436362436362436_i8b, &
           1066149217761810_i8b, &
           123456123456123456_i8b /
!-------------------------------------------------------------------------------
    m(x,k) = ieor(x, ishft(x,k))  ! statement function
    s(x) = ishft(x, -63)          ! statement function
!-------------------------------------------------------------------------------
    t = ishft(x, 58) + c
    if (s(x) .eq. s(t)) then
       c = ishft(x, -6) + s(x)
    else
       c = ishft(x, -6) + 1 - s(x + t)
    endif
    x = t + x
    y = m(m(m(y,13_i8b),-17_i8b), 43_i8b)
    z = 6906969069_i8b * z + 1234567
    kiss64 = x + y + z
end function kiss64
!-------------------------------------------------------------------------------
