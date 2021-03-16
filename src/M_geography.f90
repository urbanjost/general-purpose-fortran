










module M_geography
private
public geographical_distance
public test_suite_M_geography
contains
!>
!!##NAME
!!    geographical_distance - [M_geography] Compute the distance between two points over the Earth's surface.
!!    (LICENSE:GPL3)
!!
!!##SYNOPSIS
!!
!!    doubleprecision function geographical_distance (ll1, bb1, ll2, bb2, miles)
!!
!!       doubleprecision,intent(in) :: ll1
!!       doubleprecision,intent(in) :: bb1
!!       doubleprecision,intent(in) :: ll2
!!       doubleprecision,intent(in) :: bb2
!!       logical,intent(in)      :: miles)
!!
!!##DESCRIPTION
!!
!!    Compute the distance in miles or kilometers between two points on
!!    the Earth's surface, given the latitude and longitude of the two
!!    locations in degrees.
!!
!!##PARAMETERS
!!     ll1    Longitude of first location (degrees)
!!     bb1    Latitude of first location (degrees)
!!     ll2    Longitude of second location (degrees)
!!     bb2    Latitude of second location (degrees)
!!     miles  Return result in miles (T) or km (F)
!!
!!        Definition at line 41 of file earth.f90.
!!
!!        References SUFR_constants::d2r, SUFR_kinds::double, and SUFR_constants::earthr.
!!
!!##AUTHOR
!!    geographical_distance(3f) was originally written by AstroFloyd
!!    (http://astrofloyd.org) (Sat Apr 1 2017)as part of the
!!    libSUFR package, see: http://libsufr.sourceforge.net/
!!
!!    Copyright (c) 2002-2017 AstroFloyd - astrofloyd.org
!!
!!    This is free software: you can redistribute it and/or modify it
!!    under the terms of the GNU General Public License as published by
!!    the Free Software Foundation, either version 3 of the License, or
!!    (at your option) any later version.
!!
!!    This software is distributed in the hope that it will be useful,
!!    but WITHOUT ANY WARRANTY; without even the implied warranty of
!!    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
!!    General Public License for more details.
!!
!!    You should have received a copy of the GNU General Public License
!!    along with this code. If not, see <http://www.gnu.org/licenses/>.
!***********************************************************************************************************************************
function geographical_distance(ll1,bb1, ll2,bb2, miles)
implicit none
doubleprecision,intent(in) :: ll1,bb1, ll2,bb2
logical,intent(in)         :: miles
   doubleprecision,parameter :: pi=4*atan(1.0d0)
   doubleprecision,parameter :: r2d = 180.0d0/pi
   doubleprecision,parameter :: d2r = 1.0d0/r2d
   doubleprecision,parameter :: earthr = 6378136.6d2
   doubleprecision           :: a,fl,l1,l2,b1,b2,geographical_distance
   doubleprecision           :: f,g,l,s,c,o,r,d,h1,h2

   a   = earthr*1.d-5                      ! Earth's radius in km
   fl  = 0.003352810665d0                  ! Earth's flattening

   l1 = ll1*d2r
   b1 = bb1*d2r
   l2 = ll2*d2r
   b2 = bb2*d2r

   f = (b1+b2)*0.5d0
   g = (b1-b2)*0.5d0
   l = (l1-l2)*0.5d0

   s = sin(g)**2 * cos(l)**2 + cos(f)**2 * sin(l)**2
   c = cos(g)**2 * cos(l)**2 + sin(f)**2 * sin(l)**2
   o = atan2(sqrt(s),sqrt(c))
   r = sqrt(s*c)/(o + tiny(o))
   d = 2*o*a
   h1 = (3*r-1)/(2*c + tiny(c))
   h2 = (3*r+1)/(2*s + tiny(s))

   geographical_distance = d*(1.d0  +  fl*h1*sin(f)**2 * cos(g)**2  -  fl*h2*cos(f)**2 * sin(g)**2)
   ! Miles rather than km - this is just one of the many definitions of a mile!
   if(miles) geographical_distance = geographical_distance * 0.62137119d0

end function geographical_distance
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
subroutine test_suite_M_geography()
use M_verify, only : unit_check_start,unit_check,unit_check_done,unit_check_good,unit_check_bad,unit_check_msg
use M_verify, only : unit_check_level

!! setup
   call test_geographical_distance()
!! teardown
contains
!TTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTTT
subroutine test_geographical_distance()

   call unit_check_start('geographical_distance',msg='')
   !!call unit_check('geographical_distance', 0.eq.0, 'checking',100)
   call unit_check_done('geographical_distance',msg='')
end subroutine test_geographical_distance
!===================================================================================================================================
end subroutine test_suite_M_geography
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================

end module M_geography
