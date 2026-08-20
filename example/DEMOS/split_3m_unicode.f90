     program demo_split
     use iso_fortran_env, only : stdout => output_unit
     use M_unicode,       only : unicode_type, assignment(=)
     use M_unicode,       only : split, len, character
     use M_unicode,       only : ut=>unicode_type
     implicit none
     character(len=*),parameter :: g='(*(g0,1x))'
     type(ut)                   :: proverb
     type(ut)                   :: delims
     type(ut),allocatable       :: array(:)
     integer                    :: first
     integer                    :: last
     integer                    :: pos
     integer                    :: i
        !
        delims= '=|; '
        !
        proverb="Más vale pájaro en mano, que ciento volando."
        call printwords(proverb)

        ! there really are not spaces between these glyphs
        array=[ &
         ut("七転び八起き。"), &
         ut("転んでもまた立ち上がる。"), &
         ut("くじけずに前を向いて歩いていこう。")]
        call printwords(array)
        !
        write(stdout,g)'OOP'
        array=proverb%split(ut(' '))
        write(stdout,'(*(:"[",a,"]"))')(character(array(i)),i=1,size(array))
     contains
     impure elemental subroutine printwords(line)
     type(ut),intent(in) :: line
        pos = 0
        write(stdout,g)line%character(),len(line)
        do while (pos < len(line))
            first = pos + 1
            call split (line, delims, pos)
            last = pos - 1
            print g, line%character(first,last),first,last,pos
        end do
     end subroutine printwords
     end program demo_split
