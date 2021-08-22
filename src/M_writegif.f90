










!>
!!##NAME
!!    writegif(3f) - [M_writegif] Codes pixel-map with palette into GIF format. Optional transparent color
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!   subroutine writegif (FileName, Pixel, ColorMap, Transparent)
!!
!!    character(len=*), intent(in)         :: FileName
!!    integer, intent(in), dimension(:,:)  :: Pixel
!!    integer, intent(in), dimension(:,0:) :: ColorMap
!!    integer, intent(in), optional        :: Transparent
!!##DESCRIPTION
!!    Write GIF file from pixel array and color map.
!!
!!##OPTIONS
!!    FileName       file to create or replace
!!    Pixel          Pixel values 0 to ncol
!!    ColorMap       Color map (RGB 0:255 for colours 0:ncol)
!!    Transparent    Optional
!!##EXAMPLE
!!
!!   Sample call:
!!
!!    program demo_writegif
!!    use M_writegif, only : writegif
!!    integer  :: Pixel(100,100)
!!    integer  :: Transparent = 0
!!    integer  :: ColorMap (3,0:7)
!!    colormap(:,0)=[255,255,255]
!!    colormap(:,1)=[255,  0,  0]
!!    colormap(:,2)=[  0,255,  0]
!!    colormap(:,3)=[  0,  0,255]
!!    colormap(:,4)=[255,255,  0]
!!    colormap(:,5)=[255,  0,255]
!!    colormap(:,6)=[  0,255,255]
!!    colormap(:,7)=[  0,  0,  0]
!!
!!    ! put some colored boxes into pixmap
!!    pixel(:,:)=0
!!    pixel(1:80,1:80)=1
!!    pixel(11:20,11:20)=2
!!    pixel(21:40,21:40)=3
!!
!!    ! write gif with a transparent background
!!    call writegif('boxes_t.gif',pixel,ColorMap,Transparent)
!!
!!    ! change background color and write standard gif file
!!    where (pixel.eq.0) pixel=4
!!    call writegif('boxes.gif',pixel,ColorMap)
!!
!!    end program demo_writegif
!!
!!##AUTHOR
!!    o Version 1.01, 1999 August: Written by Jos Bergervoet
!!    o Version 2, 2008 Jan 28: Modified by Clive Page to use stream I/O, array as colourmap.
!!    o Version 3, 2017 July 6: Modified by John Urban to make it easier to use with M_PIXEL(3f) module.
!!##LICENSE
!!    Public Domain.
!>
!! PRODUCT:        CLI library utilities and examples
!! PROGRAM:        M_writegif(3f)
!! DESCRIPTION:    This module can write a GIF file in GIF89 format from raster data
!!##VERSION:        1.01, 19990808
!! AUTHOR:         Jos Bergervoet
!!##VERSION:        2.00, 20080128
!! AUTHOR:         version by [[Clive Page]] makes use of Fortran stream I/O, array as colourmap
!!##VERSION:        3.00, 20170706
!! AUTHOR:         Modified by John Urban to make it easier to use with M_PIXEL(3f) module.
!! LICENSE:        Public Domain. This is free software: you are free to change and redistribute it.
!!                 There is NO WARRANTY, to the extent permitted by law.
module M_writegif
implicit none         !  Check all declarations

! ident_1="@(#)This module can write a GIF file in GIF89 format from raster data"

private               !  bin_io is used private, no transfer to main program
public  :: writegif   !  Writes GIF89 image, given pixel array and color map

private :: giflzw, slicewrite, InitTable, flushbuffer
integer, parameter, private     :: Bufend=260
character(len=Bufend), private  :: buf
integer, private  ::  ibuf                            ! output buffer vars
integer, parameter, private  ::    maxcode = 4095
integer, parameter, private  ::    nocode = maxcode+1 ! definitions for LZW

! Define LZW code tables for hashing:
character(len=1), private, dimension(0:maxcode+1)  :: endbyte
integer, private, dimension(0:maxcode)             :: follow, next
  !
  ! For any code P, which codes for a sequence af pixel-values, endbyte(P)
  ! is the last pixel-value, follow(P) points to another code (if it exists)
  ! which codes for this same sequence, but with one more pixel-value
  ! appended.
  !   For each code P, next(P) points to another code which codes for a
  ! similar sequence with only the endbyte different. This is a hashing
  ! pointer, for fast look-up.
  !   All pointers are 'nocode' if they point to nothing
  !

integer, private :: ncod, curmaxcode, EOI, CC, P, K, child, &
                    maxbase, skip, slen, blen, accum, nout   ! local vars

contains
!-----------------------------------------------------------------------------

! CHAR2 Converts the two least sig bytes of an integer to a 2-character string
character(len=2) function char2(ival)
integer, intent(in) :: ival
char2 = achar(mod(ival,256)) // achar(mod(ival/256,256))
end function char2
!-----------------------------------------------------------------------------

subroutine flushbuffer(F_unit)
! Flushes up to 255 bytes to output file if buffer contains data, keeping
! rest of data in buffer. If skip>0 there is a partially filled last byte
! in buf[ibuf]. This byte will be written only if ibuf<256. That should be
! the last call to flushbuffer.
integer, intent(in) :: F_unit   ! I/O unit to use
  integer  :: bl    !   number of bytes to write (to be determined)

  if (ibuf > 255) then
    bl = 255        !   we will write buf[1..255]
  else if (skip /= 0) then
    bl = ibuf       !   buf[ibuf] is partially used, write buf[1..ibuf]
  else if (ibuf > 1) then
    bl = ibuf-1     !   write buf[1..ibuf-1], there is no partial byte
  else
    return          !   nothing to write
  end if

  write(F_unit) CHAR(bl)
  write(F_unit) buf(1:bl)
  buf(1:ibuf-bl) = buf(bl+1:ibuf) ! shift down remaining data
  ibuf = ibuf - bl
  return
end subroutine flushbuffer
!-----------------------------------------------------------------------------
subroutine giflzw(F_unit, Pixel)          ! routine for LZW coding
  integer, intent(in)                 :: F_unit
  integer, intent(in), dimension(:,:) :: Pixel
  integer                             :: i, j

  nout=0                          ! for counting the codes going out
  if (blen<2) then
    blen=2                        ! pixel code-length, 2 is minimum for GIF
  end if
  write(F_unit) CHAR(blen)
  maxbase = 2**blen - 1
  call InitTable()
  call slicewrite(F_unit, CC)

  do j=1, ubound(Pixel,2)
   do i=1, ubound(Pixel,1)
    K = modulo(Pixel(i,j), maxbase+1)    ! take next byte, prevent overflow
    if (i==1 .and. j==1) then
      P = K                       ! first raster byte has one-byte code P
      cycle                       ! for the first byte no further action
    end if
                                  ! Now see if code exists for sequence [.P.]K
    child = follow(P)             ! [.P.]K is "string coded by P" followed by K
    childloop: do
      if ((child == nocode) .or. (ichar(endbyte(child)) == K)) then
        exit childloop
      end if
      child = next(child)
    end do childloop

    if (child /= nocode) then     ! If code for [.P.]K was found, store it in P
      P = child
    else                          ! If not: output P and create code for [.P.]K
      call slicewrite(F_unit, P)
      if (ncod > maxcode) then    ! check if a new code can be added
        call slicewrite(F_unit, CC)       ! If not: tell listener to clear table
        call InitTable()          ! and clear our own table
      else
        if (ncod > curmaxcode) then
          slen = slen+1                     ! New codes will be one bit longer
          curmaxcode = curmaxcode * 2 + 1   ! and more codes are possible
        end if
        endbyte(ncod) = char(K)   ! ncod is the new code for [.P.]K
        follow(ncod) = nocode
        next(ncod) = follow(P)    ! include ncod in the hashing list
        follow(P) = ncod          !     of codes with same start-sequence
        ncod = ncod+1
      end if
      P = K
    end if
   end do
  end do
  call slicewrite(F_unit, P)              ! send the last code to buffer
  call slicewrite(F_unit, EOI)            ! send 'end of image' to buffer
  call flushbuffer(F_unit)        ! extra flush, including partial last byte
  return
end subroutine giflzw
!-----------------------------------------------------------------------------
subroutine InitTable()
  integer :: i

  do i=0,maxbase                  ! Start with defining the codes 0..maxbase
    endbyte(i) = char(i)          ! for one-pixel sequences (code=pixelvalue)
  end do                          ! Initially no multi-pixel codes exist
  follow(0:maxbase) = nocode
  next(0:maxbase) = nocode
  CC = maxbase+1                  ! `clear code-tabel', a control code
  EOI = maxbase+2                 ! `end of image', another control code
  ncod = CC + 2                   ! ncod = number of currently defined codes
  slen = blen + 1                 ! current number of bits to write one code
  curmaxcode = 2**slen - 1        ! currently the highest, until slen increases
  return
end subroutine InitTable
!-----------------------------------------------------------------------------
subroutine open_for_write(Fname, Funit)
! Creates a new Stream I/O file returning I/O unit used
! CGP 2009 Jan 28
character(len=*), intent(in)  :: Fname
integer, intent(out)          :: Funit
!
logical :: exists, open
! Get free I/O unit number
do Funit = 90, 7, -1
    inquire(unit=Funit, exist=exists, opened=open)
    if(exists .and. .not. open) EXIT
end do
if(Funit < 7) STOP 'open_for_write failed - no free I/O units'
open (unit=Funit, file=Fname, access="STREAM", status="REPLACE")
end subroutine open_for_write
!-----------------------------------------------------------------------------
subroutine slicewrite(F_unit, code)       ! add some bits (a 'slice') to output buffer
  integer, intent(in)  :: F_unit
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
    buf(ibuf:ibuf) = char(modulo(accum, 256))
    if (skip<8) then
      exit shiftout
    end if
    ibuf = ibuf+1                 ! last written buffer-byte is now permanent
    accum = accum / 256           ! remove that byte from accum
    skip = skip-8                 ! skip points to next bit to write in accum
  end do shiftout

  if (ibuf>255) then
    call flushbuffer(F_unit)            ! won't write unfinished byte in buf[ibuf]
  end if
  return                          ! at most 255 bytes will be left in buffer
end subroutine slicewrite
!-----------------------------------------------------------------------------
subroutine writegif (FileName, Pixel, ColorMap, Transparent)
!
! Codes pixel-map with palette into GIF format. Optional transparent color
!
character(len=*), intent(in)            :: FileName ! file to create or replace
integer, intent(in), dimension(:,:)     :: Pixel    ! Pixel values 0 to ncol
integer, intent(in), dimension(:,0:)    :: ColorMap ! RGB 0:255 for colours 0:ncol
integer, intent(in), optional           :: Transparent ! Optional

  character(len=256) :: s
  integer            :: InfoByte, nx, ny, Cblen, HasMap, maxincol,  &
                        maxgifcol, Background, i, F_unit

  call open_for_write (FileName, F_unit)
  nx = ubound(Pixel, 1)
  ny = ubound(Pixel, 2)
  maxincol = ubound(ColorMap,2)
!!  print *,'image size', nx, ny, ' colours', maxincol
  do i=1,8                           ! find the bitsize, blen, for pixels
    blen = i
    maxgifcol = 2**blen - 1          ! Number of colors has to be power of 2
    if (maxgifcol >= maxincol) then
      exit                           ! now blen and maxgifcol are correct
    end if                           ! only op to 256 colors can be
  end do
   write(F_unit) "GIF89a"
!  Create information for screen descriptor
  Background = 0
  if (present(Transparent)) then
    Background = Transparent
  end if
  HasMap = 1
  Cblen = blen
  InfoByte = HasMap * 128 + (Cblen-1) * 16 + blen-1
!  Write the screen descriptor
  write(F_unit) char2(nx), char2(ny), CHAR(InfoByte), CHAR(Background), CHAR(0)
  do i=0,maxgifcol                                 ! write global colormap
    write(F_unit) CHAR(colormap(1,min(i,maxincol))), &
                  CHAR(colormap(2,min(i,maxincol))), &
                  CHAR(colormap(3,min(i,maxincol)))
  end do
  if (present(Transparent)) then
    write(unit=*,fmt=*) "Transparent color: ", Transparent
    s = "!" // char(249) // char(4) // char(1) // char(0) // char(0)  &
            // char(Transparent) // char(0)
    write(F_unit) s(1:8)                            ! GIF transparent extension
  end if
   write(F_unit) ","                                ! Announce image
!    Now create and write image descriptor
  HasMap = 0
  InfoByte = HasMap * 128 + blen-1                 ! add 64, if interlaced
!    x_margin, y_margin (not used), image dimensions
  write(F_unit) char2(0), char2(0), char2(nx), char2(ny), CHAR(InfoByte)
  call giflzw (F_unit, Pixel)                       ! now the raster data
  write(F_unit) CHAR(0), ';'                    ! Terminating 0-block ; for GIF
  close(unit=F_unit)
  return
end subroutine writegif
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
end module M_writegif
