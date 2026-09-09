        program demo_is_hidden_file
        use M_io, only : is_hidden_file, basename
           call showit('.abc')
           call showit('./.')
           call showit('..')
           call showit('...')
           call showit('/abc/def/notes.txt')
           call showit('/abc/def/.hide')
        contains
        subroutine showit(path)
        character(len=*),intent(in) :: path
           write(*,*)is_hidden_file(path), &
            & ' ,path=',path
        end subroutine showit
        end program demo_is_hidden_file
