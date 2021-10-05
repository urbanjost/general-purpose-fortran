










!>
!! generate_uuid(3f) was originally derived from the xmlf90 codebase, (c)
!! Alberto Garcia & Jon Wakelin, 2003-2004.  It also calls RNG routines from
!! Scott Ladd <scott.ladd@coyotegulch.com>, and the libFoX modules. Although
!! some sections have been replaced, generate_uuid(3f) was originally based
!! on the libFoX version, with licensing as follows:
!!
!!     (c) 2005-2009 Toby White <tow@uszla.me.uk>
!!     (c) 2007-2009 Gen-Tao Chiang <gtc25@cam.ac.uk>
!!     (c) 2008-2012 Andrew Walker <a.walker@ucl.ac.uk>
!!
!! All rights reserved.
!!
!!  + Redistribution and use in source and binary forms, with or without
!!    modification, are permitted provided that the following conditions
!!    are met:
!!
!!  + Redistributions of source code must retain the above copyright notice,
!!    this list of conditions and the following disclaimer.
!!
!!  + Redistributions in binary form must reproduce the above copyright
!!    notice, this list of conditions and the following disclaimer in the
!!    documentation and/or other materials provided with the distribution.
!!
!!  + Neither the name of the copyright holder nor the names of its
!!    contributors may be used to endorse or promote products derived from
!!    this software without specific prior written permission.
!!
!!    This software is provided by the copyright holders and contributors
!!    "AS IS" and any express or implied warranties, including, but not
!!    limited to, the implied warranties of merchantability and fitness for
!!    a particular purpose are disclaimed. in no event shall the copyright
!!    owner or contributors be liable for any direct, indirect, incidental,
!!    special, exemplary, or consequential damages (including, but not
!!    limited to, procurement of substitute goods or services; loss of use,
!!    data, or profits; or business interruption) however caused and on any
!!    theory of liability, whether in contract, strict liability, or tort
!!    (including negligence or otherwise) arising in any way out of the use
!!    of this software, even if advised of the possibility of such damage.
module M_uuid
!>
!!##NAME
!!    M_uuid(3f) - [M_uuid] a module of UUID (Universally Unique IDentifier) procedures
!!    (LICENSE:CUSTOM OPEN)
!!
!!##SYNOPSIS
!!
!!    use m_uuid, only : generate_uuid
!!
!!##QUOTE
!!    Remember you are unique, just like everyone else.
!!
!!##DESCRIPTION
!!
!!    A universally unique identifier (UUID) is a 128-bit number used to
!!    identify information in computer systems.
!!
!!    When generated according to the standard methods, UUIDs are for
!!    practical purposes unique, without depending for their uniqueness
!!    on a central registration authority or coordination between the
!!    parties generating them, unlike most other numbering schemes. While
!!    the probability that a UUID will be duplicated is not zero, it is
!!    close enough to zero to be negligible.
!!
!!    Thus, anyone can create a UUID and use it to identify something with
!!    near certainty that the identifier does not duplicate one that has
!!    already been or will be created to identify something else. Information
!!    labeled with UUIDs by independent parties can therefore be later
!!    combined into a single database, or transmitted on the same channel,
!!    without needing to resolve conflicts between identifiers.
!!
!!    Adoption of UUIDs and GUIDs is widespread. Many computing platforms
!!    provide support for generating them, and for parsing their textual
!!    representation.
!!
!!    RFC 4122 defines a Uniform Resource Name (URN) namespace for UUIDs.
!!    A UUID presented as a URN appears as follows:
!!
!!       urn:uuid:123e4567-e89b-12d3-a456-426655440000
!!
!! -- Wikipedia
!!
!!##PROCEDURES
!!
!!    generate_uuid(version)   generate 36-character UUID string
!===================================================================================================================================
use, intrinsic :: iso_fortran_env, only : int8, int16, int32, int64, real32, real64, dp=>real128
use M_random, only : mtprng_state, mtprng_init, mtprng_rand64
use M_time, only : date_to_unix, realtime
implicit none
!===================================================================================================================================
private

! ident_1="@(#)M_uuid::M_uid(3fm): generate UUIDs according to RFC 4122"

! Only versions  0(Nil), 1 (time-based) and 4 (pseudo-RNG-based) are implemented.

integer, parameter       :: i4b = selected_int_kind(9)
integer, parameter       :: i8b = selected_int_kind(18)
type(mtprng_state), save :: rng_state
logical, save            :: initialized = .false.
integer, save            :: values_save              ! must be default for date_and_time
integer(kind=i4b), save  :: hires_count = 0
integer, save            :: clock_seq = 0 ! a random number constant for the lifetime of the process. best we can do per S 4.1.5

public                   :: generate_uuid

contains
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
!>
!!##NAME
!!    generate_uuid(3f) - [M_uuid] generate a UUID (Universally Unique IDentifier) string per RFC 4122
!!    (LICENSE:CUSTOM OPEN)
!!
!!##SYNOPSIS
!!
!!    function generate_uuid(version) result(uuid)
!!
!!     integer, intent(in), optional :: version
!!     character(len=36) :: uuid
!!
!!##DESCRIPTION
!!    A universally unique identifier (UUID) is a 128-bit number used to
!!    identify information in computer systems. When generated according
!!    to standard methods UUIDs are for practical purposes unique.
!!    generate_uuid(3f) converts the UUID to a standard string format
!!    per RFC 4122.
!!
!!##AUTHORS
!!    based on previous work from Alberto Garcia & Jon Wakelin, 2003-2004.
!!    RNG routines from Scott Ladd <scott.ladd@coyotegulch.com>, and
!!    the libFoX library( Toby White <tow@uszla.me.uk>, Gen-Tao Chiang
!!    <gtc25@cam.ac.uk>, Andrew Walker <a.walker@ucl.ac.uk>).
!!
!!##OPTIONS
!!    version  Indicates which standard method as described in RFC 4122
!!             is used to generate the string. Versions 0,1, and 4 are supported.
!!
!!             0.  Nil UUID (ie. '00000000-0000-0000-0000-000000000000')
!!             1.  time-based UUID
!!             2.  Not implemented
!!             3.  Not implemented
!!             4.  pseudo-RNG(Random Number Generator) based
!!             5.  Not implemented
!!
!!##EXAMPLE
!!
!!   Sample usage:
!!
!!    program demo_generate_uuid
!!    use M_uuid, only : generate_uuid
!!    implicit none
!!    character(len=36) :: uuid
!!       !
!!       uuid=generate_uuid(1)  ! version 1 (time-based UUID)
!!       write(*,'(a36)')uuid
!!       !
!!       uuid=generate_uuid(4)  ! version 4 (pseudo-RNG-based), default
!!       !
!!       ! RFC 4122 defines a Uniform Resource Name (URN) namespace for UUIDs.
!!       write(*,'("urn:uuid:",a36)')uuid
!!       !
!!       ! a good scratch file name
!!       open(file='/tmp/scratch_'//uuid,unit=10)
!!       !
!!    end program demo_generate_uuid
!!
!!   Typical output:
!!
!!     e769adf4-4af7-11e8-7421-3c9dfbfe9aab
!!     urn:uuid:5b0946b8-0eb4-4966-619d-047b7f7e2056
function generate_uuid(version) result(uuid)

! ident_2="@(#)M_uuid::generate_uuid(3f): generate(approximately) a UUID (Universally Unique IDentifier) string per RFC 4122"

integer, intent(in), optional :: version
character(len=36) :: uuid

integer(kind=i8b) :: timestamp, node
integer(kind=i4b) :: clock_sequence

integer(kind=i4b) :: time_low, time_mid, time_hi_and_version
integer(kind=i4b) :: clk_seq_hi_res, clk_seq_low

integer :: values(8) ! must be default for date_and_time
integer(kind=i4b) :: variant, v

   if (.not.initialized) then
      ! Use the current date and time to init mtprng but this gives limited variability, so mix the result up.
      ! Can we do better? In any case, this gets passed through a quick generator inside mtprng_init.
      call date_and_time(values=values)
      values(7) = values(7)*1000+values(5)*100+values(3)*10+values(1)
      values(8) = values(2)*1000+values(4)*100+values(6)*10+values(8)
      call mtprng_init(int(values(7)*10000+values(8), i4b), rng_state)
      clock_seq = int(mtprng_rand64(rng_state), i4b)
      initialized = .true.
   endif

   variant = 1

   if (present(version)) then
      v = version
   else
      v = 4
   endif

   select case (v)
   case (0)
      uuid='00000000-0000-0000-0000-000000000000' ! Nil UUID
      return
   case(1)                                                                           !  version 1(time-based)
      call date_and_time(values=values)
      ! In case of too-frequent requests, we will replace time_low with the count below ...
      if (all(values==values_save)) then
         hires_count = hires_count + 1
      else
         hires_count = 0
      endif
      timestamp = get_utc_since_1582(values)
      clock_sequence = clock_seq                                                      ! clock sequence (14 bits)
      node = ior(mtprng_rand64(rng_state), ishft(mtprng_rand64(rng_state), 16))       ! node ( 48 bits)
      ! No MAC address accessible - see section 4.5 !FIXME
   case(2-3,5) ! Unimplemented
      uuid = ''
      return
   case(4)                                                                           ! version 4 (pseudo-RNG-based)
      timestamp = ior(mtprng_rand64(rng_state), ishft(mtprng_rand64(rng_state), 28))
      clock_sequence = int(mtprng_rand64(rng_state), i4b)                             ! clock sequence (14 bits)
      node = ior(mtprng_rand64(rng_state), ishft(mtprng_rand64(rng_state), 16))       ! node ( 48 bits)
   case default ! Unspecified
      uuid = ''
      return
   end select

   time_low = ibits(timestamp, 0, 32)
   time_mid = ibits(timestamp, 32, 16)

   if (hires_count==0) then
      time_hi_and_version = ior(int(ibits(timestamp, 48, 12), i4b), ishft(v, 12))
   else
      time_hi_and_version = ior(hires_count, ishft(v, 12))
   endif

   clk_seq_low = ibits(clock_sequence, 0, 8)
   clk_seq_hi_res = ior(ibits(clock_sequence, 8, 6), ishft(variant, 6))

   uuid = int32ToHexOctets(time_low, 4)//"-"// &
      int32ToHexOctets(time_mid, 2)//"-"// &
      int32ToHexOctets(time_hi_and_version, 2)//"-"// &
      int32ToHexOctets(clk_seq_hi_res, 1)// &
      int32ToHexOctets(clk_seq_low, 1)//"-"// &
      int64ToHexOctets(node, 6)

contains
!==================================================================================================================================!
function int32ToHexOctets(b, n) result(s)
integer(i4b), intent(in) :: b
integer, intent(in)      :: n ! number of octets to print
character(len=2*n)       :: s
character, parameter  :: hexdigits(0:15) = ['0','1','2','3','4','5','6','7','8','9','a','b','c','d','e','f']
integer               :: i

   do i = 0, 2*n-1
      s(2*n-i:2*n-i) = hexdigits(ibits(b, i*4, 4))
   enddo

end function int32ToHexOctets
!==================================================================================================================================!
function int64ToHexOctets(b, n) result(s)
integer(i8b), intent(in) :: b
integer, intent(in)      :: n ! number of octets to print
character(len=2*n)       :: s
character, parameter  :: hexdigits(0:15) = ['0','1','2','3','4','5','6','7','8','9','a','b','c','d','e','f']
integer               :: i

  do i = 0, 2*n-1
     s(2*n-i:2*n-i) = hexdigits(ibits(b, i*4, 4))
  enddo

end function int64ToHexOctets
!==================================================================================================================================!
end function generate_uuid
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
function get_utc_since_1582(values) result(ns)

! returns the number of 100-ns intervals since 1582-10-15T00:00:00-0

! Not really: Assuming only used as an internal routine for M_UUID(3fm)
!   Fortran date time arrays only report up to the millisecond,
!   and assuming any date given is after 2017 (because added leapseconds up till that date)
!   and not taking account of leapseconds after 2017, and assuming
!   if get same answer on multiple calls that caller will correct or try again, as goal is to generate unique values

integer,intent(in)  :: values(8)
integer(kind=i8b)   :: ns
real(kind=realtime) :: unixtime
real(kind=realtime) :: starttime
integer             :: ierr
integer             :: clicks,maxclicks
real                :: rate
real(kind=dp)       :: rate8,frac8
integer(kind=i8b)   :: frac
   call date_to_unix([1582,10,15,0,0,0,0,0,0],starttime,ierr)                  ! seconds from 1582-10-15-00-00-00 to Unix Epoch Time
   call date_to_unix(values,unixtime,ierr)                                     ! seconds from given date to Unix Epoch Time
   ! if system clock is higher resolution use it even though that makes fractional second wrong
   call system_clock(count=clicks,count_rate=rate,count_max=maxclicks)
   if(rate.gt.1000)then                                                        ! system clock available and higher resolution
      rate8=real(rate,kind=dp)
      frac8=mod(real(clicks,kind=dp),rate8)/rate8*10000000_i8b                 ! MOD(A,P) == A - INT (A/P) * P.
      frac=int(frac8)                                                          ! truncate to one remainder of one second
      ns=int((unixtime-starttime)*10000000_i8b,kind=i8b)+frac                  ! get date and time to nearest second and add frac
   else                                                                        ! use date even though accurate only to 1/1000 second
      ns=int(unixtime*10000000_i8b,kind=i8b)-int(starttime*10000000_i8b,kind=i8b)
   endif
   ns=ns+26_i8b                                                                ! leap seconds as of 2016 at 23:59:60 UTC
end function get_utc_since_1582
!==================================================================================================================================!
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()!
!==================================================================================================================================!
end module M_uuid
!>
!! UUID (Universally unique identifier)
!!
!!    From Wikipedia, the free encyclopedia
!!
!!    A universally unique identifier (UUID) is a 128-bit number used to
!!    identify information in computer systems.
!!
!!    When generated according to the standard methods, UUIDs are for practical
!!    purposes unique, without depending for their uniqueness on a central
!!    registration authority or coordination between the parties generating
!!    them, unlike most other numbering schemes. While the probability that
!!    a UUID will be duplicated is not zero, it is close enough to zero to
!!    be negligible.
!!
!!    Thus, anyone can create a UUID and use it to identify something with near
!!    certainty that the identifier does not duplicate one that has already
!!    been, or will be, created to identify something else. Information labeled
!!    with UUIDs by independent parties can therefore be later combined into
!!    a single database, or transmitted on the same channel, without needing
!!    to resolve conflicts between identifiers.
!!
!!    Adoption of UUIDs and GUIDs is widespread, with many computing platforms
!!    providing support for generating them, and for parsing their textual
!!    representation.
!!
!!    6   Versions 6.1 Nil UUID
!!    6.2 Version 1 (date-time and MAC address)
!!    6.3 Version 2 (date-time and MAC address, DCE security version)
!!    6.4 Versions 3 and 5 (namespace name-based)
!!    6.5 Version 4 (random)
!!
!!    7 Collisions
!!    8 Uses 8.1 In COM
!!    8.2 As database keys
!!
!! Standards
!!
!!    UUIDs are standardized by the Open Software Foundation (OSF) as part of
!!    the Distributed Computing Environment (DCE).[3][4]
!!
!!    UUIDs are documented as part of ISO/IEC 11578:1996 "Information technology
!!    – Open Systems Interconnection – Remote Procedure Call (RPC)" and
!!    more recently in ITU-T Rec. X.667 | ISO/IEC 9834-8:2005.[5]
!!
!!    The Internet Engineering Task Force (IETF) published the Standards-Track,
!!    RFC 4122,[2] technically equivalent to ITU-T Rec. X.667 | ISO/IEC 9834-8.
!!
!! Format
!!
!!    In its canonical textual representation, the sixteen octets of a UUID
!!    are represented as 32 hexadecimal (base 16) digits, displayed in five
!!    groups separated by hyphens, in the form 8-4-4-4-12 for a total of 36
!!    characters (32 alphanumeric characters and four hyphens).
!!
!!    For example:
!!
!!       123e4567-e89b-12d3-a456-426655440000
!!       xxxxxxxx-xxxx-Mxxx-Nxxx-xxxxxxxxxxxx
!!
!!    The four bits of digit M indicate the UUID version, and the one to
!!    three most significant bits of digit N indicate the UUID variant. In the
!!    example, M is 1 and N is a (10xx), meaning that the UUID is a variant 1,
!!    version 1 UUID; that is, a time-based DCE/RFC 4122 UUID.
!!
!!    The canonical 8-4-4-4-12 format string is based on the "record layout"
!!    for the 16 bytes of the UUID:[2]
!!
!!    UUID record layout
!!
!!    Name
!!
!!    Length (bytes)
!!
!!    Length (hex digits)
!!
!!    Contents
!!
!!       time_low 4 8 integer giving the low 32 bits of the time
!!       time_mid 2 4 integer giving the middle 16 bits of the time
!!       time_hi_and_version 2 4 4-bit "version" in the most significant bits, followed by the high 12 bits of the time
!!
!!       clock_seq_hi_and_res clock_seq_low 2 4 1-3 bit "variant" in the most
!!       significant bits, followed by the 13-15 bit clock sequence node 6 12 the 48-bit node id
!!
!!    These fields correspond to those in version 1 and 2 time-based UUIDs,
!!    but the same 8-4-4-4-12 representation is used for all UUIDs, even for
!!    UUIDs which are constructed differently.
!!
!!    RFC 4122 defines a Uniform Resource Name (URN) namespace for UUIDs. A
!!    UUID presented as a URN appears as follows:[2]
!!
!!       urn:uuid:123e4567-e89b-12d3-a456-426655440000
!!
!! Encoding
!!
!!    The binary encoding of UUIDs varies between systems. Many systems encode
!!    the UUID entirely in a big-endian format.
!!
!!    For example, 00112233-4455-6677-8899-aabbccddeeff is encoded as the
!!    bytes 00 11 22 33 44 55 66 77 88 99 aa bb cc dd ee ff.
!!
!!    Other systems, notably Microsoft's marshalling of UUIDs in their COM/OLE
!!    libraries, use a mixed-endian format, whereby the first three components
!!    of the UUID are little-endian, and the last two are big-endian.
!!
!!    For example,
!!
!!       00112233-4455-6677-8899-aabbccddeeff
!!
!!    is encoded as the bytes 33 22 11 00 55 44 77 66 88 99 aa bb cc dd ee ff.
!!
!! Variants
!!
!!    One of the variants defined by RFC 4122, variant 0 (indicated by the
!!    one-bit pattern 0xxx N=0..7), is for backwards compatibility with
!!    the circa 1988, now obsolete, Apollo Network Computing System 1.5
!!    UUID format. In this format, the first 6 octets of the UUID are a
!!    48-bit timestamp (the number of 4 microsecond units of time since 1
!!    Jan 1980 UTC); the next 2 octets are reserved; the next octet is the
!!    "address family"; and the final 7 octets are a 56-bit host ID in the
!!    form specified by the address family. Though different in detail, the
!!    similarity with modern version 1 UUIDs is evident. The variant bits in
!!    the current UUID specification coincide with the high bits of the address
!!    family octet in NCS UUIDs. Though the address family could hold values in
!!    the range 0..255, only the values 0..13 were ever defined. Accordingly,
!!    the variant 0 bit pattern 0xxx avoids conflicts with historical NCS UUIDs,
!!    should any still exist in databases.[7]
!!
!!    The 3-bit variant bit pattern 111x N=e..f is reserved for possible
!!    future variants.[2]
!!
!!    The other two variants, variants 1 and 2, are used by the current UUID
!!    specifications. Variant 1 UUIDs (10xx N=8..b, 2 bits) are referred to
!!    as RFC 4122/DCE 1.1 UUIDs, or "Leach-Salz" UUIDs, after the authors
!!    of the original Internet Draft. Variant 2 (110x N=c..d, 3 bits) is
!!    characterized in the RFC as "reserved, Microsoft Corporation backward
!!    compatibility", and was used for early GUIDs on the Microsoft Windows
!!    platform. Variant bits aside, the two variants are the same except that
!!    when reduced to a binary form for storage or transmission, variant 1
!!    UUIDs use "network" (big-endian) byte order, while variant 2 GUIDs use
!!    "native" (little-endian) byte order. In their textual representations,
!!    variants 1 and 2 are the same except for the variant bits.
!!
!!    When byte swapping is required to convert between the big-endian byte
!!    order of variant 1 and the little-endian byte order of variant 2, the
!!    fields above define the swapping. The first three fields are unsigned
!!    32- and 16-bit integers and are subject to swapping, while the last two
!!    fields consist of uninterpreted bytes, not subject to swapping. This
!!    byte swapping applies even for version 3, 4, and 5 UUID's where the
!!    canonical fields do not correspond to the content of the UUID.[2]
!!
!!    Note that while some important GUIDs, such as the identifier for the
!!    Component Object Model IUnknown interface, are nominally variant 2 UUIDs,
!!    many identifiers generated and used in Microsoft Windows software
!!    and referred to as "GUIDs" are standard variant 1 RFC 4122/DCE 1.1
!!    network byte-order UUIDs, rather than little-endian variant 2 UUIDs. The
!!    current version of the Microsoft guidgen tool produces standard variant
!!    1 UUIDs. Some Microsoft documentation states that "GUID" is a synonym
!!    for "UUID",[8] as standardized in RFC 4122. RFC 4122 itself states that
!!    UUIDs "are also known as GUIDs". All this suggests that "GUID", while
!!    originally referring to a variant of UUID used by Microsoft, has become
!!    simply an alternative name for UUID, with both variant 1 and variant 2
!!    GUIDs being extant.
!!
!! Versions
!!
!!    For both variants 1 and 2, five "versions" are defined in the standards,
!!    and each version may be more appropriate than the others in specific
!!    use cases. Version is indicated by the M in the string representation.
!!
!!    Version 1 UUIDs are generated from a time and a node id (usually the
!!    MAC address); version 2 UUIDs are generated from an identifier (usually
!!    a group or user id), time, and a node id; versions 3 and 5 produce
!!    deterministic UUIDs generated by hashing a namespace identifier and name;
!!    and version 4 UUIDs are generated using a random or pseudo-random number.
!!
!! Nil UUID
!!
!!    The "nil" UUID, a special case, is the UUID,
!!
!!       00000000-0000-0000-0000-000000000000; that is, all bits set to zero.[2]
!!
!! Version 1 (date-time and MAC address)[edit]
!!
!!    Version 1 concatenates the 48-bit MAC address of the "node" (that is, the
!!    computer generating the UUID), with a 60-bit timestamp, being the number
!!    of 100-nanosecond intervals since midnight 15 October 1582 Coordinated
!!    Universal Time (UTC), the date on which the Gregorian calendar was first
!!    adopted. RFC 4122 states that the time value rolls over around 3400 AD,[2]
!!    depending on the algorithm used, which implies that the 60-bit timestamp
!!    is a signed quantity. However some software, such as the libuuid library,
!!    treats the timestamp as unsigned, putting the rollover time in 5236 AD.[9]
!!
!!    A 13- or 14-bit "uniquifying" clock sequence extends the timestamp in
!!    order to handle cases where the processor clock does not advance fast
!!    enough, or where there are multiple processors and UUID generators per
!!    node. With each version 1 UUID corresponding to a single point in space
!!    (the node) and time (intervals and clock sequence), the chance of two
!!    properly-generated version 1 UUID's being unintentionally the same is
!!    practically nil. Since the time and clock sequence total 74 bits, 274
!!    (1.8x1022 or 18 sextillion) version 1 UUIDs can be generated per node id,
!!    at a maximum average rate of 163 billion per second per node id.[2]
!!
!!    In contrast to other UUID versions, version 1 and 2 UUIDs based on
!!    MAC addresses from network cards rely for their uniqueness in part on
!!    an identifier issued by a central registration authority, namely the
!!    Organizationally Unique Identifier (OUI) part of the MAC address, which
!!    is issued by the IEEE to manufacturers of networking equipment.[10] The
!!    uniqueness of version 1 and 2 UUIDs based on network card MAC addresses
!!    also depends on network card manufacturers properly assigning unique
!!    MAC addresses to their cards, which like other manufacturing processes
!!    is subject to error.
!!
!!    Usage of the node's network card MAC address for the node id means
!!    that a version 1 UUID can be tracked back to the computer that created
!!    it. Documents can sometimes be traced to the computers where they were
!!    created or edited through UUIDs embedded into them by word processing
!!    software. This privacy hole was used when locating the creator of the
!!    Melissa virus.[11]
!!
!!    RFC 4122 does allow the MAC address in a version 1 (or 2) UUID to be
!!    replaced by a random 48-bit node id, either because the node does not
!!    have a MAC address, or because it is not desirable to expose it. In that
!!    case, the RFC requires that the least significant bit of the first octet
!!    of the node id should be set to 1.[2] This corresponds to the multicast
!!    bit in MAC addresses and setting it serves to differentiate UUIDs where
!!    the node id is randomly-generated from those based on MAC addresses from
!!    network cards, which typically have unicast MAC addresses.[2]
!!
!! Version 2 (date-time and MAC address, DCE security version)[edit]
!!
!!    RFC 4122 reserves version 2 for "DCE security" UUIDs; but it does not
!!    provide any details. For this reason, many UUID implementations omit
!!    version 2. However, the specification of version 2 UUIDs is provided by
!!    the DCE 1.1 Authentication and Security Services specification.[4]
!!
!!    Version 2 UUIDs are similar to version 1, except that the least
!!    significant 8 bits of the clock sequence are replaced by a "local domain"
!!    number, and the least significant 32 bits of the timestamp are replaced
!!    by an integer identifier meaningful within the specified local domain. On
!!    POSIX systems, local domain numbers 0 and 1 are for user ids (UIDs),
!!    and group ids (GIDs), respectively, and other local domain numbers
!!    are site-defined.[4] On non-POSIX systems, all local domain numbers
!!    are site-defined.
!!
!!    The ability to include a 40-bit domain/identifier in the UUID comes
!!    with a tradeoff. On the one hand, 40 bits allow about 1 trillion
!!    domain/identifier values per node id. On the other hand, with the
!!    clock value truncated to the 28 most significant bits, compared to
!!    60 bits in version 1, the clock in a version 2 UUID will "tick" only
!!    once every 429.49 seconds, a little more than 7 minutes, as opposed
!!    to every 100 nanoseconds for version 1. And with a clock sequence of
!!    only 6 bits, compared to 14 bits in version 1, only 64 unique UUID's
!!    per node/domain/identifier can be generated per 7 minute clock tick,
!!    compared to 16,384 clock sequence values for version 1.[12] Thus,
!!    Version 2 may not be suitable for cases where UUIDs are required, per
!!    node/domain/identifier, at a rate exceeding about 1 per 7 seconds.
!!
!! Versions 3 and 5 (namespace name-based)
!!
!!    Version 3 and 5 UUIDs are generated by hashing a namespace identifier
!!    and name. Version 3 uses MD5 as the hashing algorithm, and version
!!    5 uses SHA1.[2]
!!
!!    The namespace identifier is itself a UUID. The specification provides
!!    UUIDs to represent the namespaces for URLs, fully qualified domain
!!    names, object identifiers, and X.500 distinguished names; but any
!!    desired UUID may be used as a namespace designator.
!!
!!    To determine the version 3 UUID corresponding to a given namespace and
!!    name, the UUID of the namespace is transformed to a string of bytes,
!!    concatenated with the input name, then hashed with MD5, yielding
!!    128 bits. Six or seven bits are then replaced by fixed values, the
!!    4-bit version (e.g. 0011 for version 3), and the 2- or 3-bit UUID
!!    "variant" (e.g. 10 indicating a RFC 4122 UUIDs, or 110 indicating
!!    a legacy Microsoft GUID). Since 6 or 7 bits are thus predetermined,
!!    only 121 or 122 bits contribute to the uniqueness of the UUID.
!!
!!    Version 5 UUIDs are similar, but SHA1 is used instead of MD5. Since
!!    SHA1 generates 160-bit digests, the digest is truncated to 128-bits
!!    before the version and variant bits are inserted.
!!
!!    Version 3 and 5 UUIDs have the property that the same namespace and
!!    name will map to the same UUID. However, neither the namespace nor
!!    name can be determined from the UUID, given the other, except by
!!    brute-force search.
!!
!!    RFC 4122 recommends version 5 (SHA1) over version 3 (MD5) and counsels
!!    against use of UUIDs of either version as security credentials.[2]
!!
!! Version 4 (random)
!!
!!    A version 4 UUID is randomly generated. As in other UUIDs, four bits
!!    are used to indicate version 4, and 2 or 3 bits to indicate the variant
!!    (10 or 110 for variants 1 and 2, respectively). Thus, for variant 1
!!    (that is, most UUIDs) a random version 4 UUID will have 6 predetermined
!!    variant and version bits, leaving 122 bits for the randomly-generated
!!    part, for a total of 2122, or 5.3x1036 (5.3 undecillion) possible
!!    version 4 variant 1 UUIDs. There are half as many possible version 4
!!    variant 2 UUIDs (legacy GUIDs) because there is one less random bit
!!    available, 3 bits being consumed for the variant.
!!
!!    Some pseudorandom number generators lack necessary entropy to produce
!!    sufficiently pseudorandom numbers. For example, the WinAPI GUID
!!    generator, which uses a pseudorandom number generator, has been shown
!!    to produce UUIDs which follow a predictable pattern.[citation needed]
!!    RFC 4122 advises that "distributed applications generating UUIDs at a
!!    variety of hosts must be willing to rely on the random number source
!!    at all hosts. If this is not feasible, the namespace variant should
!!    be used."
!!
!! Collisions
!!
!!    Collision occurs when the same UUID is generated more than once and
!!    assigned to different referents. In the case of standard version 1
!!    and 2 UUIDs using unique MAC addresses from network cards, collisions
!!    can occur only when an implementation varies from the standards,
!!    either inadvertently or intentionally.
!!
!!    In contrast with version 1 and 2 UUIDs using randomly-generated node
!!    ids, hash-based version 3 and 5 UUIDs, and random version 4 UUIDs,
!!    collisions can occur even without implementation problems, albeit
!!    with a probability so small that it can normally be ignored. This
!!    probability can be computed precisely based on analysis of the
!!    birthday problem.[13]
!!
!!    For example, the number of random version 4 UUIDs which need to be
!!    generated in order to have a 50% probability of at least one collision
!!    is 2.71 quintillion, computed as follows:
!!
!!    n ≈ 1 2   + 1 4   + 2 × ln ⁡ ( 2 ) × 2 122     ≈ 2.71 × 10 18     {\displaystyle n\approx {\frac {1}{2}}+{\sqrt {{\frac {1}{4}}+2\times \ln(2)\times 2^{122}}}\approx 2.71\times 10^{18}}  {\displaystyle n\approx {\frac {1}{2}}+{\sqrt {{\frac {1}{4}}+2\times \ln(2)\times 2^{122}}}\approx 2.71\times 10^{18}}[14]
!!
!!    This number is equivalent to generating 1 billion UUIDs per second
!!    for about 85 years, and a file containing this many UUIDs, at 16
!!    bytes per UUID, would be about 45 exabytes, many times larger than
!!    the largest databases currently in existence, which are on the order
!!    of hundreds of petabytes.[15][16]
!!
!!    The smallest number of version 4 UUIDs which must be generated for
!!    the probability of finding a collision to be p is approximated by
!!    the formula:
!!
!!       2 × 2 122   × ln ⁡ 1 1 − p        {\displaystyle {\sqrt {2\times 2^{122}\times \ln {\frac {1}{1-p}}}}}  {\displaystyle {\sqrt {2\times 2^{122}\times \ln {\frac {1}{1-p}}}}}
!!
!!    Thus, for there to be a one in a billion chance of duplication,
!!    103 trillion version 4 UUIDs must be generated.
!!
!! Uses
!!
!!    Significant uses include ext2/ext3/ext4 filesystem userspace tools
!!    (e2fsprogs uses libuuid provided by util-1), LUKS encrypted
!!    partitions, GNOME, KDE, and Mac OS X,[17] most of which are derived from
!!    the original implementation by Theodore Ts'o.[9]
!!
!!    One of the uses of UUIDs in Solaris (using Open Software Foundation
!!    implementation) is identification of a running operating system instance
!!    for the purpose of pairing crash dump data with Fault Management Event
!!    in the case of kernel panic.[18]
!!
!!
!! As database keys
!!
!!    UUIDs are commonly used as a unique key in database tables. The
!!    NEWID function in Microsoft SQL Server version 4 Transact-SQL returns
!!    standard random version 4 UUIDs, while the NEWSEQUENTIALID function
!!    returns 128-bit identifiers similar to UUIDs which are committed
!!    to ascend in sequence until the next system reboot.[22] The Oracle
!!    Database SYS_GUID function does not return a standard GUID, despite
!!    the name. Instead, it returns a 16-byte 128-bit RAW value based
!!    on a host identifier and a process or thread identifier, somewhat
!!    similar to a GUID.[23] PostgreSQL contains a UUID datatype,[24]
!!    and can generate most versions of UUIDs through the use of functions
!!    from modules.[25][26] MySQL provides a UUID function, which generates
!!    standard version 1 UUIDs.[27]
!!
!!    The random nature of standard version 3, 4, and 5 UUIDs and the
!!    ordering of the fields within standard version 1 and 2 UUIDs may
!!    create problems with database locality or performance when UUIDs
!!    are used as primary keys. For example, in 2002 Jimmy Nilsson
!!    reported a significant improvement in performance with Microsoft
!!    SQL Server when the version 4 UUIDs being used as keys were modified
!!    to include a non-random suffix based on system time. This so-called
!!    "COMB" (combined time-GUID) approach made the UUIDs non-standard and
!!    significantly more likely to be duplicated, as Nilsson acknowledged,
!!    but Nilsson only required uniqueness within the application.[28]
