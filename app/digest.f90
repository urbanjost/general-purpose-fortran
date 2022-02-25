! NAME
!    digest(1f) - [FUNIX] compute SHA256 message digest
!    (LICENSE:PD)
! 
! SYNOPSIS
!    digest FILE...
! 
! DESCRIPTION
!    digest(1f) prints SHA256 (256-bit) checksums. The sums are computed
!    as described in FIPS-180-2. For each regular file listed on the
!    command line a line is printed with the checksum, a byte count and
!    the name of the file.
! 
!    It exercises the sha256(3f) routine. Note that it reads the files
!    one at a time into dynamically allocated memory.
! 
! EXAMPLES
!    Sample commands
! 
!     digest  *
!      FF1A6FB5327CBCEB6E32BCAB543FA71C3033C196EB84E122722C3FD6208360FF     1049831 c1-1-tirupathi.pdf
!      F61B2FF27B5268B9FB6DAD7BBA76D1072D046349D7E767BEEFC3AB2CA72CA95B      109119 in-1
!      B01911225398AC1371DF8F1D72CBE8AA2E677EDD855E830B2E073439057202EA        4591 newbugs.f90
!      52E4A0D9ACDD801D62AD497D57B143E5A2E0A56EAD0EA587D14F299EDBC3C8C9        4505 record.sh
! 
! AUTHOR
!    John S. Urban
! 
! LICENSE
! Public Domain
program demo_sha256
use,intrinsic :: iso_fortran_env, only : ERROR_UNIT
use M_hashkeys,                   only : sha256, test_suite_sha256
use M_io,                         only : slurp
use M_strings,                    only : switch
use M_system,                     only : system_isreg
implicit none

! ident_1="@(#)sha256(1f): read file into memory and generate SHA-256 digest value"

character(len=1),allocatable :: text(:)                      ! array to hold file in memory
character(len=:),allocatable :: string
integer                      :: i=0
character(len=4096)          :: filename
   do i=1,command_argument_count()                           ! step through filenames on command line
      call get_command_argument(i, filename)
      if(.not.system_isreg(filename))cycle                   ! ignore anything except regular files
      call slurp(filename,text)                              ! allocate character array and copy file into it
      if(.not.allocated(text))then
         write(ERROR_UNIT,*)'*sha256* ERROR: failed to load file '//trim(filename)
      else
         string=switch(text)                                 ! switch array to a single character variable
         deallocate(text)                                    ! release memory
         write(*,*)sha256(string),len(string),trim(filename) ! write digest value
      endif
   enddo
   if(i.le.1) call test_suite_sha256()
end program demo_sha256
