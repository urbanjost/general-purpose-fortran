           program demo_verify
           implicit none
           character(len=12):: c1='Howdy There!'
           character(len=6) :: c2(2)=["Howdy ","there!"]
           character(len=2) :: c3(2)=["de","gh"]
           !=======================================================
           !! LOCATION OF FIRST NONBLANK CHARACTER
           write(*,*)'nonblank ',verify('  Hello World! ', ' ')
           !! SAME AS LEN_TRIM()
           write(*,*)'length ',verify('  Hello World!    ', ' ', back = .true.)
           !! ARRAYS
           write(*,*) verify(c1,'de')                  ! writes 1
           write(*,*) verify(c2,c3)                    ! writes 1 1
           write(*,*) verify(c1,'de',back=.true.)      ! writes 12
           write(*,*) verify(c2,c3,[.true.,.false.]) ! writes 6 1
           !=======================================================
           write(*,*) verify("fortran", "ao")           ! 1, found 'f'
           write(*,*) verify("fortran", "fo")           ! 3, found 'r'
           write(*,*) verify("fortran", "c++")          ! 1, found 'f'
           write(*,*) verify("fortran", "c++", .true.)  ! 7, found 'n'
           write(*,*) verify("fortran", "nartrof")      ! 0' found none
           !=======================================================
           !! CHECK IF STRING IS OF FORM NN-HHHHH
           check : block
           logical                    :: lout
           character(len=*),parameter :: int='0123456789'
           character(len=*),parameter :: hex='abcdef0123456789'
           character(len=80)          :: chars

           chars='32-af43d'
           lout=.true.
           lout = lout.and.(verify(chars(1:2), int) == 0)
           lout = lout.and.(verify(chars(3:3), '-') == 0)
           lout = lout.and.(verify(chars(4:8), hex) == 0)
           if(lout)then
              write(*,*)trim(chars),' passed'
           endif

           endblock check
           end program demo_verify
