










!>
!!##NAME
!!    write_animated_gif(3f) - [M_writegif_animated] Codes pixel-maps with palette into animated GIF format. Optional transparent color
!!
!!##SYNOPSIS
!!
!!   subroutine write_animated_gif(filename,pixel,colormap,transparent,delay)
!!
!!    character(len=*),intent(in)         :: filename
!!    integer,intent(in),dimension(:,:,:) :: pixel
!!    integer,intent(in),dimension(:,0:)  :: colormap
!!    integer,intent(in),optional         :: transparent
!!    integer,intent(in),optional         :: delay
!!
!!##DESCRIPTION
!!    Writes gif89 image, given pixel array and color map.
!!    This version can create an animated gif.
!!
!!##OPTIONS
!!    FileName       file to create or replace
!!    Pixel          Pixel values 0 to ncol
!!    ColorMap       Color map (RGB 0:255 for colours 0:ncol)
!!    Transparent    Transparent color; optional
!!    Delay          Delay time [ 1/100 of seconds]; optional
!!
!!##EXAMPLE
!!
!!   Sample call:
!!
!!    program demo_write_animated_gif
!!    !***************************************************************************
!!    !> author: Jacob Williams
!!    !
!!    !  Use the gif module to create a sample animated gif.
!!    !
!!    !# See also
!!    !  * [Make a circle illusion animation](http://codegolf.stackexchange.com/questions/34887/make-a-circle-illusion-animation)
!!    !
!!    use, intrinsic :: iso_fortran_env, only: wp=>real64
!!    use M_writegif_animated, only : write_animated_gif
!!    implicit none
!!    !
!!    logical,parameter :: new = .true.
!!    !
!!    integer,parameter  :: n        = 200  !! size of image (square)
!!    real(wp),parameter :: rcircle  = n/2  !! radius of the big circle
!!    integer,parameter  :: time_sep = 5    !! deg
!!    !
!!    real(wp),parameter :: deg2rad = acos(-1.0_wp)/180.0_wp
!!    !
!!    integer,dimension(:,:,:),allocatable :: pixel    !! pixel values
!!    !
!!    real(wp),dimension(2) :: xy
!!    real(wp)              :: r,t
!!    integer               :: i,j,k,row,col,m,n_cases,ang_sep,iframe
!!    !
!!    integer,dimension(3,0:5)  :: colormap
!!    integer,parameter  :: white = 0
!!    integer,parameter  :: gray  = 1
!!    integer,parameter  :: red   = 2
!!    integer,parameter  :: green = 3
!!    integer,parameter  :: blue  = 4
!!    integer,parameter  :: black = 5
!!    !
!!    colormap(:,black) = [0,0,0]
!!    colormap(:,white) = [255,255,255]
!!    colormap(:,gray)  = [200,200,200]
!!    colormap(:,red)   = [255,0,0]
!!    colormap(:,green) = [0,255,0]
!!    colormap(:,blue)  = [0,0,255]
!!    !
!!    if (new) then
!!        ang_sep = 5
!!        n_cases = 3
!!    else
!!        ang_sep = 20
!!        n_cases = 0
!!    end if
!!    !
!!    !how many frames:
!!    iframe=0
!!    do k=0,355,time_sep
!!        iframe=iframe+1
!!    end do
!!    allocate(pixel(iframe,0:n,0:n))
!!    !
!!    iframe=0
!!    do k=0,355,time_sep
!!        !frame number:
!!        iframe=iframe+1
!!        !clear entire image:
!!        pixel(iframe,:,:) = white
!!        if (new) call draw_circle(n/2,n/2,red,n/2)
!!        !draw polar grid:
!!        do j=0,180-ang_sep,ang_sep
!!            do i=-n/2, n/2
!!                call spherical_to_cartesian(dble(i),dble(j)*deg2rad,xy)
!!                call convert(xy,row,col)
!!                if (new) then
!!                    pixel(iframe,row,col) = gray
!!                else
!!                    pixel(iframe,row,col) = black
!!                end if
!!            end do
!!        end do
!!        !draw dots:
!!        do m=0,n_cases
!!            do j=0,360-ang_sep,ang_sep
!!                r = sin(m*90.0_wp*deg2rad + (k + j)*deg2rad)*rcircle
!!                t = dble(j)*deg2rad
!!                call spherical_to_cartesian(r,t,xy)
!!                call convert(xy,row,col)
!!                if (new) then
!!                    !call draw_circle(row,col,black,10)    !v2
!!                    !call draw_circle(row,col,m,5)         !v2
!!                    call draw_circle(row,col,mod(j,3)+3,5) !v3
!!                else
!!                    call draw_square(row,col,red)          !v1
!!                end if
!!            end do
!!        end do
!!    end do
!!    !
!!    call write_animated_gif('circle_illusion.gif',pixel,colormap,delay=5)
!!    !
!!    deallocate(pixel)
!!    !
!!    contains
!!    !***************************************************************************
!!    !> author: Jacob Williams
!!    !
!!    !  Draw a square.
!!    !
!!    subroutine draw_square(r,c,icolor)
!!    implicit none
!!    integer,intent(in) :: r      !! row of center
!!    integer,intent(in) :: c      !! col of center
!!    integer,intent(in) :: icolor !! color value
!!    !
!!    integer,parameter :: d = 10 !square size
!!    !
!!        pixel(iframe,max(0,r-d):min(n,r+d),max(0,c-d):min(n,c+d)) = icolor
!!    !
!!    end subroutine draw_square
!!    !***************************************************************************
!!    !> author: Jacob Williams
!!    !
!!    !  Draw a circle.
!!    subroutine draw_circle(r,c,icolor,d)
!!    implicit none
!!    !
!!    integer,intent(in) :: r      !! row of center
!!    integer,intent(in) :: c      !! col of center
!!    integer,intent(in) :: icolor !! color value
!!    integer,intent(in) :: d      !! diameter
!!    !
!!    integer :: i,j
!!    !
!!        do i=max(0,r-d),min(n,r+d)
!!            do j=max(0,c-d),min(n,c+d)
!!                if (sqrt(dble(i-r)**2 + dble(j-c)**2)<=d) &
!!                    pixel(iframe,i,j) = icolor
!!            end do
!!        end do
!!    !
!!    end subroutine draw_circle
!!    !***************************************************************************
!!    !> author: Jacob Williams
!!    !
!!    !  Convert from x,y to row,col.
!!    subroutine convert(xy,row,col)
!!    implicit none
!!    !
!!    real(wp),dimension(2),intent(in) :: xy  !! coordinates
!!    integer,intent(out) :: row
!!    integer,intent(out) :: col
!!    !
!!       row = int(-xy(2) + n/2.0_wp)
!!       col = int( xy(1) + n/2.0_wp)
!!    !
!!    end subroutine convert
!!    !***************************************************************************
!!    !> author: Jacob Williams
!!    !
!!    !  Convert spherical to cartesian coordinates.
!!    subroutine spherical_to_cartesian(r,theta,xy)
!!    implicit none
!!    !
!!    real(wp),intent(in) :: r
!!    real(wp),intent(in) :: theta
!!    real(wp),dimension(2),intent(out) :: xy
!!    !
!!       xy(1) = r * cos(theta)
!!       xy(2) = r * sin(theta)
!!    !
!!    end subroutine spherical_to_cartesian
!!    !***************************************************************************
!!    end program demo_write_animated_gif
!!    !***************************************************************************
!!
!!##AUTHOR
!!    o Version 1.01, August 1999, Written by Jos Bergervoet
!!    o 2008 Jan 28: Modified by Clive Page to use stream I/O, array as colourmap.
!!    * Jacob Williams, 7/27/2014. Refactored, updated, added ability to export animated gifs.
!!    o Minor modifications to make more easily used with M_PIXEL(3f) module, 2017-July-06, John Urban
!!##LICENSE
!!   Copyright (c) 2014-2015, Jacob Williams.
!!   All rights reserved.
!!
!!   Redistribution and use in source and binary forms, with or without
!!   modification, are permitted provided that the following conditions are met:
!!
!!   * Redistributions of source code must retain the above copyright notice, this
!!     list of conditions and the following disclaimer.
!!
!!   * Redistributions in binary form must reproduce the above copyright notice,
!!     this list of conditions and the following disclaimer in the documentation
!!     and/or other materials provided with the distribution.
!!
!!   * Neither the name of the {organization} nor the names of its
!!     contributors may be used to endorse or promote products derived from
!!     this software without specific prior written permission.
!!
!!   This software is provided by the copyright holders and contributors "AS IS"
!!   and any express or implied warranties, including, but not limited to, the
!!   implied warranties of merchantability and fitness for a particular purpose are
!!   disclaimed. In no event shall the copyright holder or contributors be liable
!!   for any direct, indirect, incidental, special, exemplary, or consequential
!!   damages (including, but not limited to, procurement of substitute goods or
!!   services; loss of use, data, or profits; or business interruption) However
!!   caused and on any theory of liability, whether in contract, strict liability,
!!   or tort (Including negligence or otherwise) arising in any way out of the use
!!   of this software, even if advised of the possibility of such damage.
!>
!  Conversion of raster data to GIF89 format.
!
!# See also
!   * The original code (License: public domain) was from
!     [here](http://fortranwiki.org/fortran/show/writegif)
!
!# History
!   * Version 1.01, August 1999, Written by Jos Bergervoet
!   * 2008 Jan 28: Modified by Clive Page to use stream I/O, array as colourmap.
!   * Jacob Williams, 7/27/2014. Refactored, updated, added ability to export animated gifs.
!
!-----------------------------------------------------------------------------------------------------------------------------------
    module M_writegif_animated
    implicit none
    private

    public :: write_animated_gif

    contains
!-----------------------------------------------------------------------------------------------------------------------------------
!> author: Jacob Williams
!  date: 7/27/2014
!
!  Writes gif89 image, given pixel array and color map
!  This version can create an animated gif:
!
!  * The pixel matrix is rank 3: image i is pixel(i,:,:)
!  * If size(pixel,1) is 1, then a regular gif is produced.
!
!# See also
!   1. [writegif](http://fortranwiki.org/fortran/show/writegif)
!   2. [GIF format](http://www.onicos.com/staff/iz/formats/gif.html#aeb)
!   3. [GIF File Format Summary](http://www.fileformat.info/format/gif/egff.htm)

    subroutine write_animated_gif(filename,pixel,colormap,transparent,delay)

    character(len=*),intent(in)         :: filename    !! file to create or replace
    integer,intent(in),dimension(:,:,:) :: pixel       !! pixel values [0 to ncol]
    integer,intent(in),dimension(:,0:)  :: colormap    !! [r,g,b (0:255)] , [0:ncol] colors
    integer,intent(in),optional         :: transparent !! transparent color
    integer,intent(in),optional         :: delay       !! delay time [1/100 of seconds]

    integer,parameter     :: bufend=260
    character(len=bufend) :: buf
    integer               :: ibuf ! output buffer vars
    integer,parameter     :: maxcode = 4095
    integer,parameter     :: nocode = maxcode+1 !! definitions for lzw

    ! define lzw code tables for hashing:
    !
    ! for any code p, which codes for a sequence af pixel-values, endbyte(p)
    ! is the last pixel-value, follow(p) points to another code (if it exists)
    ! which codes for this same sequence, but with one more pixel-value
    ! appended.
    !   for each code p, next(p) points to another code which codes for a
    ! similar sequence with only the endbyte different. this is a hashing
    ! pointer, for fast look-up.
    !   all pointers are 'nocode' if they point to nothing
    !
    character(len=1),dimension(0:maxcode+1) :: endbyte
    integer,dimension(0:maxcode) :: follow, next
    integer :: ncod, curmaxcode, eoi, cc, p, k, child, &
                    maxbase, skip, slen, blen, accum, nout

    integer :: infobyte,nx,ny,cblen,hasmap,maxincol,istat,&
                maxgifcol,background,i,iunit,iframe,n,dt
    character(len=1),dimension(2) :: t

    !delay time:
    if (present(delay)) then
        dt = delay
    else
        dt = 1
    end if

    !transparency info:
    if (present(transparent)) then
        t(1) = char(1) !Reserved+Disposal Method+User Input Flag+Transparent Color Flag
        t(2) = char(transparent)
    else
        t(1) = char(0)
        t(2) = char(0)
    end if

    open(    newunit=iunit,&
            file=trim(filename),&
            access='STREAM',&
            status='REPLACE',&
            iostat=istat)

    if (istat==0) then

        n = size(pixel,1) !number of images
        nx = ubound(pixel, 2)
        ny = ubound(pixel, 3)
        maxincol = ubound(colormap,2)

        do i=1,8                            ! find the bitsize, blen, for pixels
            blen = i
            maxgifcol = 2**blen - 1         ! Number of colors has to be power of 2
            if (maxgifcol>=maxincol) exit   ! now blen and maxgifcol are correct
                                            ! [only up to 256 colors]
        end do

        !------------
        ! GIF Header
        !------------

        write(iunit) 'GIF89a'

        ! create information for screen descriptor
        if (present(transparent)) then
            background = transparent
        else
            background = 0
        end if
        hasmap = 1
        cblen = blen
        infobyte = hasmap * 128 + (cblen-1) * 16 + blen-1

        ! write the screen descriptor
        write(iunit)   char2(nx),&         ! logical screen width
                        char2(ny),&         ! logical screen height
                        char(infobyte),&    ! screen and color map information
                        char(background),&  ! background color index
                        char(0)             ! pixel aspect ratio

        ! write global colormap
        do i=0,maxgifcol
            write(iunit)  char(colormap(1,min(i,maxincol))), &
                          char(colormap(2,min(i,maxincol))), &
                          char(colormap(3,min(i,maxincol)))
        end do

        if (n>1) then    !it is an animated gif

            !-----------------------------
            ! Application Extension Block
            !-----------------------------

            ! See: http://odur.let.rug.nl/kleiweg/gif/netscape.html

            write(iunit)   '!',&           ! Extension Introducer (0x21)
                            char(255),&     ! GIF Extension code
                            char(11),&      ! Length of Application Block
                            'NETSCAPE',&    ! Application Identifier
                            '2.0',&         ! Application Authentication Code
                            char(3),&       ! Length of Data Sub-Block
                            char(1),&        ! 1 (0x01)
                            char(0),&       ! number of loop iterations
                            char(0),&       ! Data Sub-Block Terminator
                            char(0)         ! Block Terminator (0x00)

        end if

        !each frame of the animated gif:
        do iframe = 1,n

            !---------------------------------
            ! Graphic Control Extension Block
            !---------------------------------

            write(iunit)   '!',&           ! Extension Introducer (0x21)
                            char(249),&    ! Graphic Control Label (0xF9)
                            char(4),&      ! Block Size (0x04)
                            t(1),&         !
                            char2(dt),&    ! Delay Time
                            t(2),&         ! Transparent Color Index
                            char(0)        ! Block Terminator (0x00)

            !-------------
            ! Image Block
            !-------------

            ! now create and write image descriptor
            hasmap = 0
            infobyte = hasmap * 128 + blen-1    ! add 64, if interlaced

            write(iunit)   ',',&            ! Image Separator (0x2C)
                            char2(0),&      ! Image Left Position
                            char2(0),&      ! Image Top Position
                            char2(nx),&     ! Image Width
                            char2(ny),&     ! Image Height
                            char(infobyte)  ! Image and Color Table Data Information

            call giflzw(iunit,pixel(iframe,:,:))  ! now the raster data

            write(iunit) char(0)    ! Block Terminator (0x00)

        end do

        !---------
        ! Trailer
        !---------

        write(iunit) ';'

    else
        write(*,*) 'Error opening :'//trim(filename)
    end if

    !close the gif file:
    close(unit=iunit,iostat=istat)

contains
!-----------------------------------------------------------------------------------------------------------------------------------
    !>
    !  Convert the two least sig bytes of an integer to a 2-character string

function char2(ival) result(c)
integer, intent(in) :: ival
character(len=2)    :: c

   c = achar(mod(ival,256)) // achar(mod(ival/256,256))

end function char2
!-----------------------------------------------------------------------------------------------------------------------------------
    !>
    !  Flushes up to 255 bytes to output file if buffer contains data, keeping
    !  rest of data in buffer. If skip>0 there is a partially filled last byte
    !  in buf[ibuf]. This byte will be written only if ibuf<256. That should be
    !  the last call to flushbuffer.

subroutine flushbuffer(iunit)
integer, intent(in) :: iunit   !! i/o unit to use
integer :: bl   !! number of bytes to write (to be determined)

   if (ibuf > 255) then     ! we will write buf[1..255]
      bl = 255
   else if (skip /= 0) then ! buf[ibuf] is partially used, write buf[1..ibuf]
      bl = ibuf
   else if (ibuf > 1) then  ! write buf[1..ibuf-1], there is no partial byte
      bl = ibuf-1
   else                     ! nothing to write
      return
   end if

   write(iunit) char(bl)
   write(iunit) buf(1:bl)
   buf(1:ibuf-bl) = buf(bl+1:ibuf) ! shift down remaining data
   ibuf = ibuf - bl

end subroutine flushbuffer
!-----------------------------------------------------------------------------------------------------------------------------------
    !>
    !  routine for LZW coding

        subroutine giflzw(iunit, pixel)

        integer, intent(in)                 :: iunit
        integer, intent(in), dimension(:,:) :: pixel
        integer                             :: i
        integer                             :: j

        nout=0                        ! for counting the codes going out
        if (blen<2) then
            blen=2                    ! pixel code-length, 2 is minimum for gif
        end if
        write(iunit) char(blen)
        maxbase = 2**blen - 1
        call inittable()
        call slicewrite(iunit, cc)

        do j=1, ubound(pixel,2)
            do i=1, ubound(pixel,1)
                k = modulo(pixel(i,j), maxbase+1)    ! take next byte, prevent overflow
                if (i==1 .and. j==1) then
                    p = k                       ! first raster byte has one-byte code p
                    cycle                       ! for the first byte no further action
                end if
                                            ! now see if code exists for sequence [.p.]k
                child = follow(p)           ! [.p.]k is "string coded by p" followed by k
                childloop: do
                    if ((child == nocode) .or. (ichar(endbyte(child)) == k)) then
                        exit childloop
                    end if
                    child = next(child)
                end do childloop

                if (child /= nocode) then    ! if code for [.p.]k was found, store it in p
                    p = child
                else                         ! if not: output p and create code for [.p.]k
                    call slicewrite(iunit, p)
                    if (ncod > maxcode) then        ! check if a new code can be added
                        call slicewrite(iunit, cc)  ! if not: tell listener to clear table
                        call inittable()            ! and clear our own table
                    else
                        if (ncod > curmaxcode) then
                            slen = slen+1               ! new codes will be one bit longer
                            curmaxcode = curmaxcode * 2 + 1 ! and more codes are possible
                        end if
                        endbyte(ncod) = char(k)   ! ncod is the new code for [.p.]k
                        follow(ncod) = nocode
                        next(ncod) = follow(p)    ! include ncod in the hashing list
                        follow(p) = ncod          !     of codes with same start-sequence
                        ncod = ncod+1
                    end if
                    p = k
                end if
            end do
        end do

        call slicewrite(iunit, p)      ! send the last code to buffer
        call slicewrite(iunit, eoi)    ! send 'end of image' to buffer
        call flushbuffer(iunit)        ! extra flush, including partial last byte

        end subroutine giflzw
!-----------------------------------------------------------------------------------------------------------------------------------
    !>
    !  Initialize table.

        subroutine inittable()

        integer :: i

        do i=0,maxbase                  ! start with defining the codes 0..maxbase
            endbyte(i) = char(i)        ! for one-pixel sequences (code=pixelvalue)
        end do                          ! initially no multi-pixel codes exist
        follow(0:maxbase) = nocode
        next(0:maxbase) = nocode
        cc = maxbase+1                  ! 'clear code-tabel', a control code
        eoi = maxbase+2                 ! 'end of image', another control code
        ncod = cc + 2                   ! ncod = number of currently defined codes
        slen = blen + 1                 ! current number of bits to write one code
        curmaxcode = 2**slen - 1        ! currently the highest, until slen increases

        end subroutine inittable
!-----------------------------------------------------------------------------------------------------------------------------------
    !>
    !  add some bits (a 'slice') to output buffer

        subroutine slicewrite(iunit, code)

        integer, intent(in)  :: iunit
        integer, intent(in)  :: code

        if (nout == 0) then             ! initiate output buffer
            ibuf = 1
            skip = 0
            accum = 0
        end if
        nout = nout+1

        accum = accum + code * 2**skip  ! add bits at correct position in accum
        skip = skip + slen              ! slen is current slice length, in bits

        shiftout: do
            buf(ibuf:ibuf) = char(modulo(accum,256))
            if (skip<8) exit shiftout
            ibuf = ibuf+1               ! last written buffer-byte is now permanent
            accum = accum/256           ! remove that byte from accum
            skip = skip-8               ! skip points to next bit to write in accum
        end do shiftout

        if (ibuf>255) then
            call flushbuffer(iunit)    ! won't write unfinished byte in buf[ibuf]
        end if

        ! at most 255 bytes will be left in buffer

        end subroutine slicewrite
!-----------------------------------------------------------------------------------------------------------------------------------
    end subroutine write_animated_gif
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
    end module M_writegif_animated
!-----------------------------------------------------------------------------------------------------------------------------------
