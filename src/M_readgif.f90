










!>
!!##NAME
!!    readgif(3f) - [M_readgif] read a GIF file
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!   subroutine readgif(filename, num_image, image, iostat, color_map, verbose)
!!
!!    character(len=*), intent(in) :: filename
!!    integer, intent(in)          :: num_image
!!    integer, intent(out), allocatable :: image(:,:)
!!    integer, intent(out)         :: iostat
!!    real   , allocatable, intent(out) :: color_map(:,:)
!!    logical, intent(in), optional :: verbose
!!
!!##DESCRIPTION
!!    read the num_image'th gif image from filename into arrays image and color_map
!!
!!##OPTIONS
!!    filename    input file
!!    num_image   number of image required
!!    image       Image data returned
!!    iostat      I/O error number, =0 if ok
!!    color_map   RGB for each level, range 0.0 to 1.0
!!    verbose     .true.for verbose output
!!
!!##EXAMPLE
!!
!!    Sample program:
!!
!!       program demo_readgif
!!       use M_readgif, only : readgif
!!       use M_writegif, only : writegif
!!       implicit none
!!       character(len=*),parameter :: filename='boxes.gif'
!!       integer                    :: num_image=1
!!       integer,allocatable        :: image(:,:)
!!       integer                    :: iostat=0
!!       real,allocatable           :: color_map(:,:)
!!       integer,allocatable        :: color_map2(:,:)
!!       logical                    :: verbose=.true.
!!       integer                    :: i,ii,jj
!!       call readgif(filename,num_image,image,iostat,color_map,verbose)
!!       if(iostat.ne.0)then
!!          write(*,*)'*demo_readgif* could not read GIF file ',trim(filename)
!!          stop
!!       endif
!!
!!       write(*,*)'SIZE OF IMAGE =',size(image)
!!       do i=1,rank(image)
!!          write(*,*)'RANK OF IMAGE=',i,size(image,dim=i)
!!       enddo
!!
!!       write(*,*)'SIZE OF COLORMAP=',size(color_map)
!!       do i=1,rank(color_map)
!!          write(*,*)'RANK OF COLORMAP=',i,size(color_map,dim=i)
!!       enddo
!!
!!       ! convert between colormap types
!!       ! writegif uses an integer colormap, values 0 to 255
!!       ! readgif  uses real values 0.0 to 1.0
!!       ii=size(color_map,dim=1)
!!       jj=size(color_map,dim=2)
!!       allocate(color_map2(ii,0:jj-1))
!!       color_map2=255*color_map
!!
!!       ! change color and write standard gif file
!!       where (image.eq.1) image=4
!!       call writegif('boxes_new.gif',image,color_map2)
!!
!!       end program demo_readgif
!!
!!##AUTHORS
!!    Jos Bergervoet, Van Snyder, Maurizio Cremonesi, Clive Page, and others
!!##LICENSE
!!    This module contains a subroutine readgif(3f) which can read GIF files
!!    of types Gif87a and Gif89 (and maybe others). The code comes from
!!    various authors, see comments below. This version was put together
!!    by Clive Page who has put it into the public domain.
module M_readgif
! readgif2.f90   cgp 2010 Aug 28
! Original code from: http://it.geocities.com/butonoj/doc/gif-io/gifio.htm (now a dead link)
public :: readgif
private
! *****     private stuff     ******************************************
integer,private, parameter :: interlace = 6    ! index of bit indicating interlaced
integer,private, parameter :: max_lzw_bits = 12
integer,private, parameter :: use_local_colormap = 7 ! bit indicating to use local color map
integer, public, parameter :: max_colormap_size = 256
integer,private, save      :: lun                    ! logical unit number
logical,private            :: zero_data_block

type, public :: gif_screen_type
    integer :: aspect_ratio, background
    integer :: bit_pixel                 ! size of colormap
    integer :: color_resolution
    integer :: height, width             ! shape(image) = (/width,height/)
    integer :: color_map_size           ! size of local_colormap
    logical :: use_local_colormap       ! .true. if local color map, else global
end type gif_screen_type

type(gif_screen_type), public :: gif_screen

type, public :: gif89_type
    integer :: transparent
    integer :: delaytime
    integer :: inputflag
    integer :: disposal
end type gif89_type

type(gif89_type), public :: gif89
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
CONTAINS
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine readgif(filename, num_image, image, iostat, color_map, verbose)
! read the num_image'th gif image from filename into arrays image and color_map
character(len=*), intent(in) :: filename                ! input file
integer, intent(in)          :: num_image               ! number of image required
integer, intent(out), allocatable :: image(:,:)         ! Image data returned
integer, intent(out)         :: iostat                  ! I/O error number, =0 if ok
real   , allocatable, intent(out) :: color_map(:,:)     ! RGB for each level, range 0.0 to 1.0
logical, intent(in), optional :: verbose                ! .true.for verbose output
! -----     local variables     ------------------------------------
character(len=16) :: buf             ! input buffer
character (len=1):: c                ! shorter input buffer
integer :: image_count               ! number of images processed so far
logical :: my_verbose
! -----     executable statements     ------------------------------
    zero_data_block = .false.
    gif89= gif89_type( -1, -1, -1, 0 )
    my_verbose = .false.
    if ( present(verbose) ) my_verbose = verbose
    call open_gif (filename, iostat, my_verbose, color_map )
    if (iostat /= 0) RETURN
    image_count = 0
    do ! forever
      call read_buf(c, iostat )
      if (iostat /= 0) then
        call io_error ( "reading file", iostat, filename )
        RETURN
      end if
      if ( c == ";" ) then ! gif image terminator
        if ( image_count < num_image ) then
          write (*,*) "only", image_count, "image(s) found in file"
          iostat = -1
        end if
        close ( unit=lun )
        RETURN
      end if
      if ( c == "!" ) then
   ! gif extension
        call do_extension (filename, iostat, my_verbose )
        if (iostat /= 0) RETURN
        CYCLE
      end if
      if ( c /= "," ) then
   ! not a valid start character
        write (*,*) "ignoring bogus character ", ichar(c)
        CYCLE
      end if
      image_count = image_count + 1
      if (image_count>num_image) RETURN
      call read_buf(buf(1:9), iostat )
      if (iostat /= 0) then
        call io_error("cannot read width/height", iostat, filename)
        RETURN
      end if
!
! If local colour map exists: read it
!
      gif_screen%use_local_colormap = btest(ichar(buf(9:9)),use_local_colormap)
      if ( gif_screen%use_local_colormap ) then
        gif_screen%color_map_size = 2**(modulo(ichar(buf(9:9)),8)+1)
        if(my_verbose) write(*,*)'readgif error in local colour map, size=', &
                        gif_screen%color_map_size
        allocate(color_map(3,gif_screen%color_map_size))
        call read_colormap(color_map, iostat )
        if (iostat /= 0) then
          call io_error ( " error reading local color map", iostat, filename )
          RETURN
        end if
        call read_image(bcint2b(buf(5:6)), bcint2b(buf(7:8)), &
                         btest(ichar(buf(9:9)),interlace), image_count /= num_image, &
                         my_verbose, filename,  iostat, image)
        if (iostat /= 0) RETURN
      else
        call read_image(bcint2b(buf(5:6)), bcint2b(buf(7:8)), &
                         btest(ichar(buf(9:9)),interlace), image_count /= num_image, &
                         my_verbose, filename,  iostat, image )
        if (iostat /= 0) RETURN
      end if
    end do
    close(unit=lun)
    print *,'closed unit', lun, ' in readgif'
  end subroutine readgif
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
! private module procedures
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
function bcint2b ( buf ) result (iresult)
! convert two bytes to an integer. The bytes are in little-endian
! order -- the first one is the low-order byte, and the second is
! the high-order byte
character(len=*), intent(in) :: buf
integer :: iresult
iresult = 256*ichar(buf(2:2)) + ichar(buf(1:1))
end function bcint2b
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine do_extension (filename, iostat, verbose )
character(len=*),intent(in) :: filename             ! in case it is needed in a message
integer, intent(out)        :: iostat
logical, intent(in)         :: verbose
!
    character(len=256), save :: buf      ! long input buffer
    character (len=1) :: c                       ! short input buffer
    integer :: countx                     ! length of a data block
    character(len=256) :: str            ! part of a message, if verbose
!
    call read_buf(c, iostat )
    if (iostat /= 0) then
      call io_error ( " error reading extension", iostat, filename )
      RETURN
    end if
    select case ( ichar(c) )
    case ( 1 )                           ! 0x01 -- plain text extension
      str = "plain text extension"
    case ( 249 )                         ! 0xf9 -- graphic control extension
      str = "graphic control extension"
      call get_data_block(buf, filename, countx, iostat )
      if (iostat /= 0)  then
         RETURN
      end if
      ! the gif89 structure isn't used. Why do we do this?
      gif89%disposal = modulo(ichar(buf(1:1))/4, 7)
      gif89%inputflag   = modulo(ichar(buf(1:1))/2, 1)
      gif89%delaytime   = bcint2b(buf(2:3))
      if ( modulo(ichar(buf(1:1)),2) /= 0 ) then
         gif89%transparent = ichar(buf(4:4))
      end if
      do
        call get_data_block(buf, filename, countx, iostat )
        if (iostat /= 0) then
          RETURN
        end if
        if ( countx == 0 ) then
          EXIT
        end if
      end do
    case ( 254 )                         ! 0xfe -- comment extension
      str = "comment extension"
      do
        call get_data_block(buf, filename, countx, iostat )
        if (iostat /= 0) then
          RETURN
        end if
        if ( countx == 0 ) then
           EXIT
        end if
        if (verbose) write (*,*) " gif comment: ", buf(:countx)
      end do
    case ( 255 )                         ! 0xff -- application extension
      str = "application extension"
    case default                         ! oops
      write (*,*) " unknown extension ", ichar(c), " label"
      str = buf
    end select
    if ( verbose ) write(*,*) 'readgif: extension ', trim(str)
end subroutine do_extension
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine get_code ( result,code_size, flag, filename, iostat )
integer, intent(out) :: result
integer, intent(in) :: code_size
logical, intent(in) :: flag          ! first-time flag
character (len=*), intent(in) :: filename        ! in case it is needed in a message
integer, intent(out) :: iostat
!
    integer :: bint                      ! bit an integer (not logical)
    character(len=280), save :: buf      ! input buffer
    integer :: countx
    integer, save :: curbit
    logical, save :: done
    integer :: i, j, k
    integer, save :: lastbit
    integer, save :: last_byte
!
    if ( flag ) then
      curbit = 0
      lastbit = 0
      last_byte = 2
      buf(1:2) = "  " ! so it has a value the first time around
      done = .false.
      result = 0
      RETURN
    end if
    if ( curbit + code_size >= lastbit ) then
      if ( done ) then
        if ( curbit >= lastbit ) write (*,*) " ran off the end of my bits"
        result = -1
        RETURN
      end if
      buf(1:2) = buf(last_byte-1:last_byte)
      call get_data_block(buf(3:), filename, countx, iostat )
      if (iostat /= 0) then
        result = -1
        RETURN
      end if
      if ( countx == 0 ) done = .true.
      curbit = (curbit - lastbit) + 16
      last_byte = 2 + countx
      lastbit = last_byte * 8
    end if
    result = 0
    i = curbit / 8 + 1
    k = modulo(curbit, 8)
    do j = 0, code_size-1
      bint = 0
      if (btest(ichar(buf(i:i)), k)) bint = 1
      result = ior( result, ishft(bint,j) )
      curbit = curbit + 1
      k = k + 1
      if ( k == 8 ) then
        k = 0
        i = i + 1
      end if
    end do
  end subroutine get_code
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine get_data_block (buf, filename, countx, iostat )
character(len=*), intent(out) :: buf
character(len=*), intent(in):: filename ! in case it is needed in a message
integer, intent(out) :: countx        ! size of data block
integer, intent(out) :: iostat
!
    character (len=1) :: c
    call read_buf(c, iostat )
    if (iostat /= 0) then
      call io_error ( " error in count for data block", iostat, filename )
      RETURN
    end if
    countx = ichar(c)
    zero_data_block = countx == 0
    if ( countx /= 0 ) then
      call read_buf(buf(1:countx), iostat )
      if (iostat /= 0) &
        call io_error (" error reading data block", iostat, filename )
    end if
  end subroutine get_data_block
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine get_lun ( lun )
integer, intent(out) :: lun    ! a free logical unit number
!
logical :: inuse, exists
!
do lun = 100, 6, -1
      inquire (unit=lun, opened=inuse, exist=exists )
      if (exists .and. .not. inuse) RETURN
end do
write(*,*) "get_lun: no free logical unit numbers"
lun = -1
end subroutine get_lun
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine io_error ( message, iostat, filename )
character(len=*), intent(in) :: message
integer, intent(in)          :: iostat
character(len=*), intent(in) :: filename
!
write (*,*) "readgif error ", trim(message), ' in ', trim(filename), &
            " code =", iostat
end subroutine io_error
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine lzw_read_byte ( result, input_code_size, flag, filename, iostat )
integer, intent (out) :: result
integer, intent(in) :: input_code_size
logical, intent(in) :: flag          ! first-time flag
character(len=*), intent(in) :: filename             ! in case it is needed in a message
integer, intent(out) :: iostat
!
    character(len=260) :: buf
    integer, save :: clear_code
    integer :: code
    integer, save :: code_size
    integer :: countx
    integer, save :: end_code
    integer, save :: firstcode
    logical, save :: fresh = .false.
    integer :: i
    integer :: incode
    integer, save :: max_code, max_code_size
    integer, parameter :: max_max_code = 2**max_lzw_bits
    integer, parameter :: max_stack = 2*max_max_code
    integer, save :: oldcode
    integer, save :: set_code_size
    integer, dimension(0:1,0:max_max_code-1), save :: table
    integer,dimension(max_stack), save :: stack
    integer, save :: stack_ptr
    result = 0
    if ( flag ) then                     ! setup
        set_code_size = input_code_size
        clear_code = 2 ** set_code_size
        if ( set_code_size > max_lzw_bits ) then
           result= -1
           RETURN
         end if
         end_code = clear_code + 1
         code_size = set_code_size + 1
         max_code = clear_code + 2
         max_code_size = 2 * clear_code
         stack_ptr = 1
         table(0,:) = 0
         do i = 0, clear_code-1
            table(1,i) = i
         end do
         table(1,clear_code:) = 0
         call get_code(i, code_size, .true., filename, iostat )   ! initialize
         fresh = .true.
         RETURN
    end if
    if ( fresh ) then
      fresh = .false.
      do
         call get_code(oldcode, code_size, .false., filename, iostat )
         firstcode = oldcode
         if ( firstcode /= clear_code ) EXIT
      end do
      result = firstcode
      RETURN
    end if
    if ( stack_ptr > 1 ) then
      stack_ptr = stack_ptr - 1
      result = stack(stack_ptr)
      RETURN
    end if
    do
      call get_code(code, code_size, .false., filename, iostat )
      if ( code < 0 ) EXIT
      if ( code == clear_code ) then
        code_size = set_code_size + 1
        max_code = clear_code + 2
        max_code_size = 2 * clear_code
        stack_ptr = 1
        table(0,:) = 0
        do i = 0, clear_code-1
          table(1,i) = i
        end do
        table(1,clear_code:) = 0
        call get_code( oldcode, code_size, .false., filename, iostat )
        firstcode = oldcode
        result = firstcode
        RETURN
      end if
      if ( code == end_code ) then
        result = -2
        if ( zero_data_block ) RETURN
        do
          call get_data_block(buf, filename, countx, iostat )
          if (iostat /= 0) RETURN
          if ( countx <= 0 ) EXIT
        end do
        if ( countx /= 0 ) then
          write (unit=*, fmt="(a)") "missing eod in data stream in file"
          write (unit=*, fmt="(a)") trim(filename)
          write (unit=*, fmt="(a)") "(this is not unusual)"
        end if
        RETURN
      end if
      incode = code
      if ( code >= max_code ) then
        stack(stack_ptr) = firstcode
        stack_ptr = stack_ptr + 1
        code = oldcode
      end if
      do
        if ( code >= clear_code ) then
           stack(stack_ptr) = table(1,code)
           stack_ptr = stack_ptr + 1
           if ( code == table(0,code) ) then
             write (unit=*, fmt="(a,i6,a,i6)") "code =", code,", table(0,code) =", table(0,code)
             write (unit=*, fmt="(a)") "circular table entry in file"
             write (unit=*, fmt="(a)") trim(filename)
             write (unit=*, fmt="(a)") "this is a serious error."
             result = -2
             RETURN
           end if
           code = table(0,code)
        else
          EXIT
        end if
      end do
      firstcode = table(1,code)
      stack(stack_ptr) = firstcode
      stack_ptr = stack_ptr + 1
      code = max_code
      if ( code < max_max_code ) then
        table(0,code) = oldcode
        table(1,code) = firstcode
        max_code = max_code + 1
        if ( max_code >= max_code_size .and. max_code_size < max_max_code ) then
          max_code_size = max_code_size * 2
          code_size = code_size + 1
        end if
      end if
      oldcode = incode
      if ( stack_ptr > 1 ) then
        stack_ptr = stack_ptr - 1
        result = stack(stack_ptr)
        RETURN
      end if
    end do
    result = code
  end subroutine lzw_read_byte
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine open_gif (filename, iostat, verbose, color_map)
! open a gif file, verify it is either gif87a or gif89a
! fill the gif_screen structure, including reading the color map if
! necessary.
! iostat > 0 is status from open or read, iostat = -1 means wrong format.
! a message will be printed if iostat /= 0.
character(len=*), intent(in) :: filename
integer, intent(out) :: iostat
logical, intent(in) :: verbose
real, allocatable, intent(out) :: color_map(:,:)
!
character(len=7) :: buf              ! input buffer
!
call get_lun ( lun )
if (lun<1) then
   iostat = -1
   RETURN
end if
if ( verbose ) write (unit=*, fmt="(a,a)") "opening ", trim(filename)
open(unit=lun, file=filename, access="stream", status="old", iostat=iostat)
if (iostat /= 0) then
    call io_error ( " failed to open", iostat, filename )
    RETURN
else
    end if
    call read_buf (buf(1:6), iostat )
    if (iostat /= 0) then
      call io_error ( " error reading 'magic number'", iostat, filename )
      RETURN
    end if
    if ( buf(1:6) /= "GIF87a" .and. buf(1:6) /= "GIF89a" ) then
      write (*,*) " invalid GIF format", buf(1:6), " in ", trim(filename),&
         " expected GIF87a or GIF89a"
      iostat = -1
      close ( unit=lun )
      RETURN
    end if
    call read_buf (buf(1:7), iostat )
    if (iostat /= 0) then
      RETURN
    end if
    gif_screen%width = bcint2b(buf(1:2))
    gif_screen%height = bcint2b(buf(3:4))
    gif_screen%bit_pixel = ishft(2,iand(ichar(buf(5:5)),7))
    gif_screen%color_resolution = iand(ishft(ichar(buf(5:5)),-3),14)+1
    gif_screen%background = ichar(buf(6:6))
    gif_screen%aspect_ratio = ichar(buf(7:7))
    if ( btest(ichar(buf(5:5)), use_local_colormap) ) then
        if(verbose) write(*,*) &
           'readgif error in global colour map size', gif_screen%bit_pixel
        allocate(color_map(3,gif_screen%bit_pixel))
        call read_colormap(color_map, iostat )
        if (iostat /= 0) then
           call io_error ( "error reading global colormap", iostat, filename )
           RETURN
        end if
    end if
    if ( gif_screen%aspect_ratio /= 0 .and. gif_screen%aspect_ratio /= 49 ) then
      write (*,*) "readgif warning: non-square pixels, ratio=", &
                 ( gif_screen%aspect_ratio + 15.0 ) / 64.0
    end if
end subroutine open_gif
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine read_buf (buf, iostat )
! read the next len(buf) characters from the gif file opened by open_gif.
character(len=*), intent(out) :: buf
integer, intent(out) :: iostat   ! I/O status, =0 if no error
read(unit=lun, iostat=iostat) buf
end subroutine read_buf
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine read_colormap (colormap, iostat )
! read the colormap.
real, dimension(:), intent(out) :: colormap(:,:)  ! 1st dimension: red/green/blue
integer, intent(out) :: iostat
!
integer :: j
character :: triplet*3
!
do j = 1, size(colormap,2)
    call read_buf(triplet, iostat )
    if (iostat /= 0) then
        write(*,*) 'readgif: error reading colormap at j=', j, iostat
        RETURN
    end if
    colormap(1,j) = ichar(triplet(1:1)) / 255.0
    colormap(2,j) = ichar(triplet(2:2)) / 255.0
    colormap(3,j) = ichar(triplet(3:3)) / 255.0
end do
end subroutine read_colormap
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
subroutine read_image (length, height, interlace, ignore, &
                       verbose, filename, iostat, image)
integer, intent(in) :: length
integer, intent(in) :: height
logical, intent(in) :: interlace
logical, intent(in) :: ignore
character(len=*),intent(in) :: filename             ! so it can appear in messages
logical, intent(in) :: verbose
!    character (len=*),dimension(:,:), pointer :: image
integer, intent(out) :: iostat
integer, intent(out), allocatable :: image(:,:)
!
character (len=1) :: c              ! input buffer
integer :: pass                     ! pass number, for interlaced images
integer :: vbyte                    ! output from lzw_read_byte
integer :: xpos, ypos               ! coordinates of pixel
integer :: rslt
!
    call read_buf(c, iostat )
    if (iostat /= 0) then
      call io_error ( "error reading code size byte", iostat, filename )
      RETURN
    end if
    call lzw_read_byte( rslt, ichar(c), .true., filename, iostat )
    if ( rslt < 0 ) then
      call io_error ( "reading image prefix", iostat, filename )
      RETURN
    end if
    if ( ignore ) then                   ! skip an uninteresting image
      if ( verbose ) then
         write (unit=*, fmt="(a)") "skipping an image"
      end if
      call lzw_read_byte (rslt, ichar(c), .false., filename, iostat )
      do
         if ( rslt >= 0 ) then
            call lzw_read_byte (rslt, ichar(c), .false., filename, iostat )
         else
            EXIT
         end if
      end do
      RETURN
    end if
 !
    if(verbose) write(unit=*,fmt=*) "Image size:",length,height
    allocate ( image(length,height), stat=iostat )
 !
    if (iostat /= 0) then
      call io_error ( "while allocating image", iostat, filename )
      RETURN
    end if
    if ( verbose ) write (*,*) " reading", length," by", height, " GIF image from ", &
                   trim(filename), "  ", merge("interlaced", "          ", interlace)
    pass = 0
    xpos = 1
    ypos = 1
    do
      call lzw_read_byte (vbyte, ichar(c), .false., filename, iostat )
      if (vbyte < 0) EXIT
      image(xpos,ypos) = vbyte
      xpos = xpos + 1
      if ( xpos > length ) then
        xpos = 1
        if ( interlace ) then
          select case ( pass )
          case ( 0, 1 )
            ypos = ypos + 8
          case ( 2 )
            ypos = ypos + 4
          case ( 3 )
            ypos = ypos + 2
          end select
          if ( ypos > height ) then
            pass = pass + 1
            select case ( pass )
            case ( 1 )
              ypos = 5
            case ( 2 )
              ypos = 3
            case ( 3 )
              ypos = 2
            case default
              EXIT
            end select
          end if
        else
          ypos = ypos + 1
        end if
      end if
      if ( ypos > height ) EXIT
    end do
      call lzw_read_byte ( rslt,ichar(c), .false., filename, iostat )
    if ( rslt >= 0 ) then
      write (*,*) "readgif: too much input data, ignoring extra..."
      do
        if ( rslt >= 0 ) then
          call lzw_read_byte ( rslt, ichar(c), .false., filename, iostat )
        else
          EXIT
        end if
      end do
    end if
  end subroutine read_image
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
end module M_readgif
