        program demo_bytes_to_anything
        use, intrinsic :: ISO_FORTRAN_ENV, only: &
             CSZ => CHARACTER_STORAGE_SIZE, &
             stderr => error_unit
        use :: M_anything, only : bytes_to_anything, anything_to_bytes
        implicit none
        character(len=1), allocatable :: chars(:)
        character(len=:), allocatable :: line
        character(len=:), allocatable :: lines(:)
        integer                       :: ints(10)
        integer                       :: i, int
        integer,allocatable           :: somesize(:)

        call header('integer array to bytes')
        chars = anything_to_bytes([(i*i, i=1, size(ints))])
        write (*, '(/,4(1x,z2.2))') chars
        call bytes_to_anything(chars, ints)
        write(*,*)'and bytes back to integer array'
        write (*, '(/,*(g0,1x))') ints

        call header('integer scalar to bytes')
        chars = anything_to_bytes(1234)
        write (*, '(/,"CHARS=",*(1x,z2.2))') chars
        call bytes_to_anything(chars, int)
        write(*,*)'and bytes back to integer scalar'
        write (*, '(/,"INT=",*(g0,1x))') int

        call header('a string')
        chars = anything_to_bytes('this is a string')
        write (*, '(/,"CHARS=",*(1x,z2.2))') chars
        write (*, '(/,"CHARS=",*(g0,1x))') chars
        ! string must be long enough to hold chars
        line=repeat(' ',size(chars))
        call bytes_to_anything(chars, line)
        write (*, '(/,"LINE=",*(g0,1x))') line

        call header(&
        'a string array (have to know length or size you wish to return to)')
        chars = anything_to_bytes([character(len=4) :: 'a', 'bb', 'ccc' ])
        write (*, '(/,"CHARS=",*(1x,z2.2))') chars
        write (*, '(/,"CHARS=",*(g0,1x))') chars
        ! string must be long enough to hold chars, and have enough elements
        ! can just return as a scalar string if unknown length
        lines=[repeat(' ',size(chars))]
        ! of for that matter just work with the chars(1) array,
        ! but assuming know length in this case
        lines=[(repeat('#',4),i=1,3)]
        call bytes_to_anything(chars, lines)
        write (*, '(/,"LINES=",*("[",g0,"]",1x:))') lines

        call header('calculating size to allocate for non-string types')
        ! make sure array is of sufficient size to hold results
        chars = anything_to_bytes([11,22,33,44])
        write (*, '(/,"CHARS=",*(1x,z2.2))') chars
        allocate(somesize(size(chars)/(storage_size(0)/CSZ)))
        call bytes_to_anything(chars, somesize)
        write (*, '(/,"SOMESIZE=",*("[",g0,"]",1x:))') somesize
        contains
        subroutine header(line)
        character(len=*),intent(in) :: line
        write(*,'(*(a))')'#',repeat('=',len(line)+2),'#'
        write(*,'("|",1x,a,1x,"|")') line
        write(*,'(*(a))')'#',repeat('=',len(line)+2),'#'
        end subroutine header
        end program demo_bytes_to_anything
