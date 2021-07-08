
!-----------------------------------------------------------------------
subroutine jsh_v(i) ! assume long routine names are supported
implicit none
integer,intent(in) :: i
      ! return version number in character variable version and print
      ! compile information to unit i
      if(i.ge.0)then
      write(i,'(1x,79("-"))')
      call trimit('@(#)File ................ jsh_v>')
      call trimit('@(#)Program Version ..... 0.0.0>')
      call trimit('@(#)Build Target ........ Linux_ifort>')
      call trimit('@(#)Compile Date ........ '//&
     &'Thu 08 Jul 2021 06:13:59 PM EDT>')
     call trimit('@(#)Compiled on node:>')
      call trimit('@(#) Nodename ........... '// &
     &'venus>')
      call trimit('@(#) System Type ........ '// &
     &'Linux>')
      call trimit('@(#) O.S. Release ....... '// &
     &'5.4.0-66-generic>')
      call trimit('@(#) O.S. Version ....... ' &
     &//'#74-Ubuntu ' &
     &//'SMP ' &
     &//'Wed ' &
     &//'Jan ' &
     &//'27 ' &
     &//'22:54:38 ' &
     &//'UTC ' &
     &//'2021 ' &
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
