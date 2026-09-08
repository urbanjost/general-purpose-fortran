     program demo_M_unicode
     use,intrinsic :: iso_fortran_env, only : stdout=>output_unit
     use M_unicode,only : tokenize, replace, character, upper, lower, len
     use M_unicode,only : unicode_type, assignment(=), operator(//)
     use M_unicode,only : ut => unicode_type, ch => character
     type(unicode_type)             :: string
     type(unicode_type)             :: numeric, uppercase, lowercase
     type(unicode_type),allocatable :: array(:)
     character(len=*),parameter     :: all='(g0)'
     !character(len=*),parameter     :: uni='(DT)'
     uppercase='АБВГҐДЕЄЖЗИІЇЙКЛМНОПРСТУФХЦЧШЩЬЮЯ'
     lowercase='абвгґдеєжзиіїйклмнопрстуфхцчшщьюя'
     numeric='0123456789'
      !
      string=uppercase // numeric // lowercase
      !
      print all, 'Original string:'
      print all, ch(string)
      print all, 'length in bytes :',len(string%character())
      print all, 'length in glyphs:',len(string)
      print all
      !
      print all, 'convert to all uppercase:'
      print all, ch( UPPER(string) )
      print all
      !
      print all, 'convert to all lowercase:'
      print all, ch( string%LOWER() )
      print all
      !
      print all, 'tokenize on spaces ... '
      call TOKENIZE(string,ut(' '),array)
      print all, '... writing with A or G format:',character(array)
      !print uni, ut('... writing with DT format'),array
      print all
      !
      print all, 'case-insensitive replace:'
      print all, ch( &
      & REPLACE(string, &
      & ut('клмнопрс'), &
      & ut('--------'), &
      & ignorecase=.true.) )
      !
      print all
      !
     end program demo_M_unicode
