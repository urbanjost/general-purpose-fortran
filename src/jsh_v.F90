
!-----------------------------------------------------------------------
subroutine jsh_v(i) ! assume long routine names are supported

use iso_fortran_env
implicit none
integer,intent(in) :: i
      ! return version number in character variable version and print
      ! compile information to unit i
      if(i.ge.0)then
      write(i,'(1x,79("-"))')
      call trimit('@(#)File ................ jsh_v>')
      call trimit('@(#)Program Version ..... 0.0.0>')
      call trimit('@(#)Build Target ........ Linux_gfortran>')
      call trimit('@(#)Compiler Version .... '//trim(compiler_version())//'>')
      call trimit('@(#)Compiler Options .... '//trim(compiler_options())//'>')
      call trimit('@(#)Compile Date ........ '//&
     &'Mon 24 Aug 2026 12:06:56 AM EDT>')
     call trimit('@(#)Compiled on node:>')
      call trimit('@(#) Nodename ........... '// &
     &'mercury>')
      call trimit('@(#) System Type ........ '// &
     &'Linux>')
      call trimit('@(#) O.S. Release ....... '// &
     &'5.4.0-216-generic>')
      call trimit('@(#) O.S. Version ....... ' &
     &//'#236-Ubuntu ' &
     &//'SMP ' &
     &//'Fri ' &
     &//'Apr ' &
     &//'11 ' &
     &//'19:53:21 ' &
     &//'UTC ' &
     &//'2025 ' &
     &//'>')
      call trimit('@(#) Hardware Name ...... '//&
     &'x86_64>')
      write(i,'(1x,79("-"))')
      endif
      contains
      subroutine trimit(string) ! leave off metadata prefix
      character(len=*) :: string
         write(i,*)trim(string(5:len_trim(string)-1))
      end subroutine trimit
end subroutine jsh_v
!-----------------------------------------------------------------------
