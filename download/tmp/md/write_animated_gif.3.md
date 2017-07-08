[UP]

-----------------------------------------------------------------------------------------------------------------------------------
                                         Manual Reference Pages  - write_animated_gif (3)
-----------------------------------------------------------------------------------------------------------------------------------
                                                                 
NAME

    write_animated_gif(3f) - [M_writegif_animated] Codes pixel-maps with palette into animated GIF format. Optional transparent
    color

CONTENTS

    Synopsis
    Description
    Options
    Example
    Author
    License

SYNOPSIS

    subroutine write_animated_gif(filename,pixel,colormap,transparent,delay)


       character(len=*),intent(in)         :: filename
       integer,intent(in),dimension(:,:,:) :: pixel
       integer,intent(in),dimension(:,0:)  :: colormap
       integer,intent(in),optional         :: transparent
       integer,intent(in),optional         :: delay



DESCRIPTION

    Writes gif89 image, given pixel array and color map. This version can create an animated gif.

OPTIONS

        FileName file to create or replace

        Pixel Pixel values 0 to ncol

        ColorMap Color map (RGB 0:255 for colours 0:ncol)

        Transparent Transparent color; optional

        Delay Delay time [ 1/100 of seconds]; optional

EXAMPLE

    Sample call:

       !***************************************************************************
       !> author: Jacob Williams
       !
       !  Use the gif module to create a sample animated gif.
       !
       !# See also
       !  * [Make a circle illusion animation](http://codegolf.stackexchange.com/questions/34887/make-a-circle-illusion-animation)


       program circle_illusion
       use, intrinsic :: iso_fortran_env, only: wp=>real64
       use M_writegif_animated, only : write_animated_gif
       implicit none


       logical,parameter :: new = .true.


       integer,parameter  :: n        = 200  !! size of image (square)
       real(wp),parameter :: rcircle  = n/2  !! radius of the big circle
       integer,parameter  :: time_sep = 5    !! deg


       real(wp),parameter :: deg2rad = acos(-1.0_wp)/180.0_wp


       integer,dimension(:,:,:),allocatable :: pixel    !! pixel values


       real(wp),dimension(2) :: xy
       real(wp)              :: r,t
       integer               :: i,j,k,row,col,m,n_cases,ang_sep,iframe


       integer,dimension(3,0:5)  :: colormap
       integer,parameter  :: white = 0
       integer,parameter  :: gray  = 1
       integer,parameter  :: red   = 2
       integer,parameter  :: green = 3
       integer,parameter  :: blue  = 4
       integer,parameter  :: black = 5


       colormap(:,black) = [0,0,0]
       colormap(:,white) = [255,255,255]
       colormap(:,gray)  = [200,200,200]
       colormap(:,red)   = [255,0,0]
       colormap(:,green) = [0,255,0]
       colormap(:,blue)  = [0,0,255]


       if (new) then
           ang_sep = 5
           n_cases = 3
       else
           ang_sep = 20
           n_cases = 0
       end if


       !how many frames:
       iframe=0
       do k=0,355,time_sep
           iframe=iframe+1
       end do
       allocate(pixel(iframe,0:n,0:n))


       iframe=0
       do k=0,355,time_sep


           !frame number:
           iframe=iframe+1


           !clear entire image:
           pixel(iframe,:,:) = white


           if (new) call draw_circle(n/2,n/2,red,n/2)


           !draw polar grid:
           do j=0,180-ang_sep,ang_sep
               do i=-n/2, n/2
                   call spherical_to_cartesian(dble(i),dble(j)*deg2rad,xy)
                   call convert(xy,row,col)
                   if (new) then
                       pixel(iframe,row,col) = gray
                   else
                       pixel(iframe,row,col) = black
                   end if
               end do
           end do


           !draw dots:
           do m=0,n_cases
               do j=0,360-ang_sep,ang_sep
                   r = sin(m*90.0_wp*deg2rad + (k + j)*deg2rad)*rcircle
                   t = dble(j)*deg2rad
                   call spherical_to_cartesian(r,t,xy)
                   call convert(xy,row,col)
                   if (new) then
                       !call draw_circle(row,col,black,10)    !v2
                       !call draw_circle(row,col,m,5)         !v2
                       call draw_circle(row,col,mod(j,3)+3,5) !v3
                   else
                       call draw_square(row,col,red)          !v1
                   end if
               end do
           end do



        end do

        call write_animated_gif( circle_illusion.gif ,pixel,colormap,delay=5)

        deallocate(pixel)

        contains !*************************************************************************** !> author: Jacob Williams !

               ! Draw a square.

               subroutine draw_square(r,c,icolor)


               implicit none
               integer,intent(in) :: r      !! row of center
               integer,intent(in) :: c      !! col of center
               integer,intent(in) :: icolor !! color value


               integer,parameter :: d = 10 !square size


               pixel(iframe,max(0,r-d):min(n,r+d),max(0,c-d):min(n,c+d)) = icolor


               end subroutine draw_square

        !*************************************************************************** !> author: Jacob Williams !

               ! Draw a circle.

               subroutine draw_circle(r,c,icolor,d)


               implicit none


               integer,intent(in) :: r      !! row of center
               integer,intent(in) :: c      !! col of center
               integer,intent(in) :: icolor !! color value
               integer,intent(in) :: d      !! diameter


               integer :: i,j


               do i=max(0,r-d),min(n,r+d)
                   do j=max(0,c-d),min(n,c+d)
                       if (sqrt(dble(i-r)**2 + dble(j-c)**2)<=d) &
                           pixel(iframe,i,j) = icolor
                   end do
               end do


               end subroutine draw_circle

        !*************************************************************************** !> author: Jacob Williams !

               ! Convert from x,y to row,col.

               subroutine convert(xy,row,col)


               implicit none


               real(wp),dimension(2),intent(in) :: xy  !! coordinates
               integer,intent(out) :: row
               integer,intent(out) :: col


               row = int(-xy(2) + n/2.0_wp)
               col = int( xy(1) + n/2.0_wp)


               end subroutine convert

        !*************************************************************************** !> author: Jacob Williams !

               ! Convert spherical to cartesian coordinates.

               subroutine spherical_to_cartesian(r,theta,xy)


               implicit none


               real(wp),intent(in) :: r
               real(wp),intent(in) :: theta
               real(wp),dimension(2),intent(out) :: xy


               xy(1) = r * cos(theta)
               xy(2) = r * sin(theta)


               end subroutine spherical_to_cartesian

        !***************************************************************************

        end program circle_illusion !***************************************************************************

AUTHOR

    o Copyright (c) 2014-2015, Jacob Williams

LICENSE

    Copyright (c) 2014-2015, Jacob Williams. All rights reserved.

    Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following
    conditions are met:

    o    Redistributions of source code must retain the above copyright notice, this list of conditions and the following
         disclaimer.

    o    Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following
         disclaimer in the documentation and/or other materials provided with the distribution.

    o    Neither the name of the {organization} nor the names of its contributors may be used to endorse or promote products
         derived from this software without specific prior written permission.

    This software is provided by the copyright holders and contributors "AS IS" and any express or implied warranties, including,
    but not limited to, the implied warranties of merchantability and fitness for a particular purpose are disclaimed. In no event
    shall the copyright holder or contributors be liable for any direct, indirect, incidental, special, exemplary, or consequential
    damages (including, but not limited to, procurement of substitute goods or services; loss of use, data, or profits; or business
    interruption) However caused and on any theory of liability, whether in contract, strict liability, or tort (Including
    negligence or otherwise) arising in any way out of the use of this software, even if advised of the possibility of such damage.

-----------------------------------------------------------------------------------------------------------------------------------

                                                      write_animated_gif (3)                                          July 02, 2017

Generated by manServer 1.08 from 5c38e338-c89a-40c6-add8-8db785e6542c using man macros.
                                                           [write_anim]
