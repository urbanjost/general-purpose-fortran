      program demo_selected_char_kind
      use iso_fortran_env, only: output_unit, CHARACTER_KINDS
      implicit none

      intrinsic date_and_time, selected_char_kind

      ! set some aliases for common character kinds
      ! as the numbers can vary from platform to platform

      integer, parameter :: default = selected_char_kind ("default")
      integer, parameter :: ascii =   selected_char_kind ("ascii")
      integer, parameter :: ucs4  =   selected_char_kind ('ISO_10646')
      integer, parameter :: utf8  =   selected_char_kind ('utf-8')

      ! assuming ASCII and UCS4 are supported (ie. not equal to -1)
      ! define some string variables
      character(len=26, kind=ascii ) :: alphabet
      character(len=30, kind=ucs4  ) :: hello_world
      character(len=30, kind=ucs4  ) :: string

         write(*,'(*(g0,1x))')'Available CHARACTER kind values:',CHARACTER_KINDS

         write(*,*)'ASCII     ',&
          & merge('Supported    ','Not Supported',ascii /= -1)
         write(*,*)'ISO_10646 ',&
          & merge('Supported    ','Not Supported',ucs4 /= -1)
         write(*,*)'UTF-8     ',&
          & merge('Supported    ','Not Supported',utf8 /= -1)

         if(default.eq.ascii)then
             write(*,*)'ASCII is the default on this processor'
         endif

        ! for constants the kind precedes the value, somewhat like a
        ! BOZ constant
         alphabet = ascii_"abcdefghijklmnopqrstuvwxyz"
         write (*,*) alphabet

         hello_world = ucs4_'Hello World and Ni Hao -- ' &
                       // char (int (z'4F60'), ucs4)     &
                       // char (int (z'597D'), ucs4)

        ! an encoding option is required on OPEN for non-default I/O
         if(ucs4 /= -1 )then
            open (output_unit, encoding='UTF-8')
            write (*,*) trim (hello_world)
         else
            write (*,*) 'cannot use utf-8'
         endif

         call create_date_string(string)
         write (*,*) trim (string)

      contains

      ! The following produces a Japanese date stamp.
      subroutine create_date_string(string)
      intrinsic date_and_time,selected_char_kind
      integer,parameter :: ucs4 = selected_char_kind("ISO_10646")
      character(len=1,kind=ucs4),parameter :: &
             nen =   char(int( z'5e74' ),ucs4), & ! year
             gatsu = char(int( z'6708' ),ucs4), & ! month
             nichi = char(int( z'65e5' ),ucs4)    ! day
      character(len= *, kind= ucs4) string
      integer values(8)
         call date_and_time(values=values)
         write(string,101) values(1),nen,values(2),gatsu,values(3),nichi
       101 format(*(i0,a))
      end subroutine create_date_string

      end program demo_selected_char_kind
