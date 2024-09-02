       program demo_M_strings
       use M_strings,only : SPLIT, slice, sep, delim, chomp, strtok
       use M_strings,only : split2020, find_field
       use M_strings,only : substitute, change, modif, transliterate, &
               & reverse, squeeze
       use M_strings,only : REPLACE, join
       use M_strings,only : UPPER, LOWER, upper_quoted
       use M_strings,only : rotate13, percent_encode, percent_decode
       use M_strings,only : adjustc, compact, nospace, indent
       use M_strings,only : crop, clip, unquote, quote, matching_delimiter
       use M_strings,only : len_white, pad, lpad, cpad, rpad, zpad, &
               & stretch, lenset, merge_str
       use M_strings,only : switch, s2c, c2s
       use M_strings,only : noesc, notabs, dilate, expand, visible
       use M_strings,only : longest_common_substring
       use M_strings,only : string_to_value, string_to_values, s2v, s2vs
       use M_strings,only : int, real, dble, nint
       use M_strings,only : atoi, atol, aton
       use M_strings,only : value_to_string, v2s, str, fmt
       use M_strings,only : listout, getvals
       use M_strings,only : glob, ends_with
       use M_strings,only : paragraph
       use M_strings,only : base, decodebase, codebase, base2
       use M_strings,only : isalnum, isalpha, iscntrl, isdigit
       use M_strings,only : isgraph, islower, isprint, ispunct
       use M_strings,only : isspace, isupper, isascii, isblank, isxdigit
       use M_strings,only : isnumber
       use M_strings,only : fortran_name
       use M_strings,only : describe
       use M_strings,only : edit_distance
       use M_strings,only : bundle
       character(len=:),allocatable :: string
       character(len=:),allocatable :: array(:) ! output array of tokens
       character(len=*),parameter   :: gen='(*(g0))'
       character(len=*),parameter   :: genx='(*("[",g0,"] ":))'
       string='abcdefghijklmnopqrstuvwxyz ABCDEFGHIJKLMNOPQRSTUVWXYZ 01234567890'
       write(*,gen)string
       write(*,gen)upper(string)
       write(*,gen)lower(string)
       call split(string,array)
       write(*,genx)array
       write(*,gen)replace(string,'qrs','--RePlace--',ignorecase=.true.)
       end program demo_M_strings
