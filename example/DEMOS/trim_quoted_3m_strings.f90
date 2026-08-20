     program demo_trim_quoted
     use M_strings, only: trim_quoted
     implicit none
     character(len=*),parameter   :: bracket='(*("[",g0,"]":,","))'
     character(len=*),parameter   :: uno='(/,*(g0:,/))'
     character(len=:),allocatable :: a,b

     a = 'Esto es    una   prueba a ver como sale y determinar si  &
     & realmente   funciona bien la ruutina para eliminar blancos  &
     & "intermedios  de  una    hola      cadena  de  caracteres"  &
     &  y ver ademas si "(respetamos     las       comillas) "     &
     &   realmente respeta las cadenas encerradas entre comillas.  &
     &vamos a ver como sale este negocio. que mas puedo decir.     &
     &probemos y veamos que pasa'

     print uno, 'Original tal y como se escribio (sin trim_quoted)',a
     print uno, 'reducir espacios a uno 1', trim_quoted (a, ' ')
     print uno, 'reducir espacios a dos 2', trim_quoted (a, '  ')
     print uno, 'reducir espacios a cero 0', trim_quoted (a, '')

     a = "This is a    test to see how it turns out and to determine if the&
     & routine to eliminate 'intermediate    spaces   from a text string'  &
     & really works well, and also to see if '(we respect       quotation  &
     & marks)'   really respects strings enclosed   in quotes. Let's see   &
     & how this business turns out. What else can I say. Let's test and see&
     & what       happens."

     print uno, "Original exactly as it was written (without trim_quoted)",a
     print uno, 'reduce spaces to one 1', trim_quoted (a, ' ')
     print uno, 'reduce spaces to two 2', trim_quoted (a, '  ')
     print uno, 'reduce spaces to zero 0', trim_quoted (a, '')

     b = trim_quoted(a, '')
     print *, b

     write(*,bracket)trim_quoted('this and    that','')

     write(*,bracket)trim_quoted(' a b  c  '),'a b c'
     write(*,bracket)trim_quoted('a','xxxxx'),'a'
     write(*,bracket)trim_quoted('','xxxxx'),''
     write(*,bracket)trim_quoted(' a b   c " don''t  touch " d   e',':'),&
     & 'a:b:c:" don''t  touch ":d:e'
     write(*,bracket)trim_quoted('  a ','xxxxx'),'a'
     write(*,bracket)trim_quoted("  a '  quoted   text '",'--'),&
     & "a--'  quoted  text '"
     write(*,bracket)trim_quoted("  a '  quoted   text ' abcd efg",'--'),&
     & "a--'  quoted  text '--abcd--efg"
     end program demo_trim_quoted
