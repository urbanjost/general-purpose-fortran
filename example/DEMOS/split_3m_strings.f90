          program demo_split
          use M_strings, only: split
          implicit none
          integer :: i
          character(len=*),parameter     :: line=&
          '  aBcdef   ghijklmnop qrstuvwxyz  1:|:2     333|333 a B cc    '
          character(len=:),allocatable :: array(:) ! output array of tokens
             write(*,*)'INPUT LINE:['//LINE//']'
             write(*,'(70("="))')
             write(*,*)'typical call:'
             CALL split(line,array)
             write(*,'(i0," ==> ",a)')(i,trim(array(i)),i=1,size(array))
             write(*,*)'SIZE:',SIZE(array)
             write(*,'(70("-"))')
             write(*,*)'custom list of delimiters (colon and vertical line):'
             CALL split(line,array,delimiters=':|',&
             & order='sequential',nulls='ignore')
             write(*,'(i0," ==> ",a)')(i,trim(array(i)),i=1,size(array))
             write(*,*)'SIZE:',SIZE(array)
             write(*,'(70("-"))')
             write(*,*) 'custom list of delimiters, &
             &reverse array order and count null fields:'
             CALL split(line,array,delimiters=':|',&
             &order='reverse',nulls='return')
             write(*,'(i0," ==> ",a)')(i,trim(array(i)),i=1,size(array))
             write(*,*)'SIZE:',SIZE(array)
             write(*,'(70("-"))')
             write(*,*)'INPUT LINE:['//LINE//']'
             write(*,*) 'default delimiters and reverse array order &
             &and return null fields:'
             CALL split(line,array,delimiters='',order='reverse',nulls='return')
             write(*,'(i0," ==> ",a)')(i,trim(array(i)),i=1,size(array))
             write(*,*)'SIZE:',SIZE(array)
          end program demo_split
