      program demo_ichar
      use,intrinsic :: iso_fortran_env, only : b=>int8
      implicit none
      integer,parameter  :: bytes=80
      character          :: string*(bytes),lets((bytes))*1
      integer(kind=b)    :: ilets(bytes)
      equivalence (string,lets)
      equivalence (string,ilets)
         write(*,*)ichar(['a','z','A','Z'])
         string='Do unto others'
         associate (a=>ichar(lets))
          ilets=merge(a-32,a,a>=97.and.a<=122) ! uppercase
          write(*,*)string
          ilets=merge(a+32,a,a>=65.and.a<=90)  ! lowercase
          write(*,*)string
         end associate
      end program demo_ichar
