program demo_sha256

! ident_1="@(#) sha256(1f) read file into memory and generate SHA-256 digest value"

use,intrinsic :: iso_fortran_env, only : ERROR_UNIT
use M_hashkeys,                   only : sha256, test_suite_sha256
use M_io,                         only : slurp
use M_strings,                    only : switch
use M_system,                     only : system_isreg
use M_CLI2,                       only : set_args, lget, files=>unnamed
implicit none
character(len=1),allocatable :: text(:)                      ! array to hold file in memory
character(len=:),allocatable :: help_text(:),version_text(:)
character(len=:),allocatable :: string
integer                      :: i=0
character(len=:),allocatable :: filename
   call setup()
   call set_args(' --auto_test:a F',help_text,version_text)
   if (lget('auto_test'))then
      call test_suite_sha256()
   endif
   do i=1,size(files)                                        ! step through filenames on command line
      filename=files(i)
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
contains
subroutine setup()
help_text=[ CHARACTER(LEN=128) :: &
'NAME',&
'   digest(1f) - [FUNIX] compute SHA256 message digest',&
'   (LICENSE:PD)',&
'',&
'SYNOPSIS',&
'   digest FILE...',&
'',&
'DESCRIPTION',&
'   digest(1f) prints SHA256 (256-bit) checksums. The sums are computed as',&
'   described in FIPS-180-2. For each regular file listed on the command',&
'   line a line is printed with the checksum, a byte count and the name',&
'   of the file.',&
'',&
'   It exercises the sha256(3f) routine. Note that it reads the files',&
'   one at a time into dynamically allocated memory.',&
'',&
'OPTIONS',&
'    --auto_test,a    run internal tests of routines in M_sha3(3fm) module',&
'',&
'EXAMPLES',&
'   Sample commands',&
'',&
'    digest  *',&
'     FF1A6FB532 .... 22C3D6208360FF     1049831 c1-1-tirupathi.pdf',&
'     F61B2FF27B .... C3AB2CA72CA95B      109119 in-1',&
'     B019112253 .... 30B239057202EA        4591 newbugs.f90',&
'     52E4A0D9AE .... 4F299EDBC3C8C9        4505 record.sh',&
'',&
'AUTHOR',&
'   John S. Urban',&
'',&
'LICENSE',&
'Public Domain',&
'']
version_text=[ CHARACTER(LEN=128) :: &
'@(#)PRODUCT:        GPF library utilities and examples>',&
'@(#)PROGRAM:        digest(1)>',&
'@(#)DESCRIPTION:    generate SHA digest values for specified files>',&
'@(#)VERSION:        1.0-20220715>',&
'@(#)AUTHOR:         John S. Urban>',&
'']
end subroutine setup
end program demo_sha256
