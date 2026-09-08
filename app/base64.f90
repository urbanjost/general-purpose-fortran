program demo_base64
! base64-encode/decode data to RFC-4648 and print to standard output
! usage: base64 inputfile > outputfile
use,intrinsic :: iso_fortran_env, only : int8, int16, int32, real32, stderr=>ERROR_UNIT, stdin=>INPUT_UNIT, stdout=>OUTPUT_UNIT
implicit none
integer(kind=int32)          :: i, column, sz, pad, iostat
character(len=1),allocatable :: text(:) ! array to hold file in memory
character(len=:),allocatable :: infile
character(len=4)             :: chunk
integer,parameter            :: rfc4648_linelength=76
character(len=1),parameter   :: rfc4648_padding='='
   infile=get_arg(1)
   call slurp(infile,text) ! allocate character array and copy file into it and pad with two characters at end
   sz=size(text)-2
   pad=3-mod(sz,3)
   column=0
   ! place three bytes and zero into 32bit integer
   ! take sets of 6 bits from integer and place into every 8 bits
   do i=1,sz,3
      chunk=three2four(text(i:i+2))
      if(i.gt.sz-3)then
         if(pad.gt.0.and.pad.lt.3)then
            chunk(4-pad+1:)=repeat(rfc4648_padding,pad)
         endif
      endif
      if(column.ge.rfc4648_linelength)then
         write(stdout,'(a)')
         flush(unit=stdout,iostat=iostat)
         column=0
      endif
      write(stdout,'(a)',advance='no')chunk
      column=column+4
   enddo
   if(column.ne.0)write(stdout,'(a)')
contains

function three2four(tri) result(quad)
character(len=1),intent(in) :: tri(3)
character(len=4)            :: quad
integer(kind=int32)         :: i32, i, j, iout(4)
character(len=*),parameter  :: rfc4648_alphabet='ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/'
character(len=1),parameter  :: rfc4648_chars(*)=[(rfc4648_alphabet(i:i),i=1,len(rfc4648_alphabet))]
   i32 = transfer([(tri(j),j=3,1,-1),achar(0)], i32 )
   iout = 0
   ! The bits are numbered 0 to BIT_SIZE(I)-1, from right to left.
   do j=1,4
      call  mvbits(i32, (j-1)*6, 6, iout(4-j+1), 0)
      quad(4-j+1:4-j+1)=rfc4648_chars(iout(4-j+1)+1)
   enddo
end function three2four

function get_arg(iarg) result(value)
integer,intent(in)           :: iarg
character(len=:),allocatable :: value
integer                      :: argument_length, istat
   call get_command_argument(number=iarg,length=argument_length)
   if(allocated(value))deallocate(value)
   allocate(character(len=argument_length) :: value)
   value(:)=''
   call get_command_argument(iarg, value, status=istat)
end function get_arg

subroutine slurp(filename,text)
use iso_fortran_env, only : iostat_eor, iostat_end
!@(#) M_io::slurp(3f): allocate text array and read file filename into it, padding on two characters
character(len=*),intent(in)              :: filename    ! filename to slurp
character(len=1),allocatable,intent(out) :: text(:)     ! array to hold file
integer            :: nchars=0, igetunit, iostat=0, i, icount
character(len=256) :: iomsg
character(len=1)   :: byte
   text=''
   if(filename /= '') then
      open(newunit=igetunit, file=trim(filename), action="read", iomsg=iomsg,&
      &form="unformatted", access="stream",status='old',iostat=iostat)
   else ! copy stdin to a scratch file
      open(newunit=igetunit, iomsg=iomsg,&
      &form="unformatted", access="stream",status='scratch',iostat=iostat)
      open(unit=stdin,pad='yes')
      INFINITE: do
         read(stdin,'(a)',iostat=iostat,advance='no')byte
         if(is_iostat_eor(iostat)) then
            byte=new_line('a')
         elseif(is_iostat_end(iostat)) then
            exit
         elseif(iostat.ne.0)then
            exit
         endif
         write(igetunit)byte
      enddo INFINITE
      rewind(igetunit,iostat=iostat,iomsg=iomsg)
   endif
   if(iostat == 0)then  ! if file was successfully opened
      inquire(unit=igetunit, size=nchars)
      if(nchars <= 0)then
         write(stderr,'(a)')'*slurp* empty file '//trim(filename)
         return
      endif
      ! read file into text array
      if(allocated(text))deallocate(text)  ! make sure text array not allocated
      allocate( text(nchars+2) )           ! make enough storage to hold file and two padding characters
      read(igetunit,iostat=iostat,iomsg=iomsg) text(:nchars)      ! load input file -> text array
      if(iostat /= 0)then
         write(stderr,'(a)') '*slurp* bad read of '//trim(filename)//':'//trim(iomsg)
      endif
      text(size(text)-1:)=repeat(achar(0),2) ! add padding characters
   else
      write(stderr,'(a)') '*slurp* '//iomsg
   endif
   close(iostat=iostat,unit=igetunit)            ! close if opened successfully or not
end subroutine slurp

end program demo_base64
