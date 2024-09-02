     program demo_split
     use M_strings, only: split
     implicit none
     integer                      :: i
     character(len=*),parameter   :: title='(80("="),t1,a)'
     character(len=*),parameter   :: line=&
     '  aBcdef   ghijklmnop qrstuvwxyz  1:|:2     333|333 a B cc    '
     character(len=:),allocatable :: array(:) ! output array of tokens
        write(*,*)'INPUT LINE:['//line//']'
        !
        write(*,title)'typical call: '
        call split(line,array)
        call printme()
        !
        write(*,title)'custom delimiters=":|" : '
        call split(line,array,delimiters=':|',&
        & order='sequential',nulls='ignore')
        call printme()
        !
        write(*,title)&
        'delimiters=":|",reverse array order and count null fields:'
        call split(line,array,delimiters=':|',&
        & order='reverse',nulls='return')
        call printme()
        !
        write(*,title)&
        'default delimiters, reverse array order and return null fields:'
        call split(line,array,delimiters='',&
        & order='reverse',nulls='return')
        call printme()
     contains
     subroutine printme()
        write(*,'(i0," ==> ",a)')(i,trim(array(i)),i=1,size(array))
        write(*,*)'SIZE:',size(array)
     end subroutine printme
     end program demo_split
