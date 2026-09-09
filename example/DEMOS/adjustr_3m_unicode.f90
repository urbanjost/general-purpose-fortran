    program demo_adjustr
    use M_unicode, only : ut=>unicode_type
    use M_unicode, only : adjustr, len
    use M_unicode, only : write(formatted)
    use M_unicode, only : assignment(=)
    implicit none
    type(ut)                   :: str
    type(ut),allocatable       :: array(:)
    integer                    :: i
    character(len=*),parameter :: bracket='("[",DT,"]")'
        !
        call numberline(2)
        !
        ! basic usage
        str = '  sample string     '
        write(*,bracket) str
        str = adjustr(str)
        write(*,bracket) str
        !
        call numberline(5)
        !
        ! elemental
        array=ut([character(len=50) :: &
        '    एक (ek) ', &
        '       दो (do) ', &
        '          तीन(teen) ' ])
        !
        ! print array unadjusted
        write(*,bracket)array
        !do i=1,size(array)
        !   write(*,'(*(g0,1x))')array(i)%codepoint()
        !enddo
        ! note 50 bytes is not necessarily 50 glyphs
        write(*,'(*(g0,1x))')'length in glyphs=',len(array)
        write(*,'(*(g0,1x))')'length in bytes=',(len(array(i)%character()),i=1,size(array))
        !
        call numberline(5)
        !
        ! print array right-justified
        write(*,bracket)adjustr(array)
        !
        call numberline(5)
        !
        ! print array right-justified specifying number of glyphs
        write(*,*)'set to 50'
        write(*,bracket)adjustr(array,50)
        !
        write(*,*)'set to 60'
        call numberline(6)
        write(*,bracket)adjustr(array,60)
        write(*,*)'set to 40'
        call numberline(4)
        write(*,bracket)adjustr(array,40)
        write(*,*)'set to 10'
        call numberline(1)
        write(*,bracket)adjustr(array,10)
        write(*,*)'set to 5'
        write(*,bracket)adjustr(array,5)
        write(*,*)'set to 4'
        write(*,bracket)adjustr(array,4)
        write(*,*)'set to 1'
        write(*,bracket)adjustr(array,1)
     contains
        !
        subroutine numberline(ireps)
        integer,intent(in) :: ireps
           write(*,'(1x,a)')repeat('1234567890',ireps)
        end subroutine numberline
     end program demo_adjustr
