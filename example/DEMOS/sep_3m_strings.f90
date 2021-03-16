          program demo_sep
          use M_strings, only: sep
          character(len=*),parameter :: fo='(/,a,*(/,"[",g0,"]":,","))'
          character(len=*),parameter :: line=&
          '  aBcdef   ghijklmnop qrstuvwxyz  1:|:2     333|333 a B cc    '
             write(*,'(a)') 'INPUT LINE:['//LINE//']'
             write(*,fo) 'typical call:',sep(line)
             write(*,fo) 'delimiters ":|":',sep(line,':|')
             write(*,fo) 'count null fields ":|":',sep(line,':|','return')
          end program demo_sep
