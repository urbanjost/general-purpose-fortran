!
! Downloads a dictionary entry, using libcurl via fortran-curl.
!
module callback
use M_curl,    only : c_f_str_ptr
implicit none
private
public :: response_callback
contains
   ! static size_t callback(char *ptr, size_t size, size_t nmemb, void *data)
function response_callback(ptr, size, nmemb, data) bind(c)
!! Callback function for `CURLOPT_WRITEFUNCTION` that appends the
!! response chunk in `ptr` to the given file with file name in pointer
!! `data`.
!!
!! This callback function might be called several times by libcurl,
!! passing in more chunks of the response.
use, intrinsic :: iso_c_binding, only: c_associated, c_f_pointer, c_ptr, c_size_t
type(c_ptr),            intent(in), value :: ptr               !! C pointer to a chunk of the response.
integer(kind=c_size_t), intent(in), value :: size              !! Always 1.
integer(kind=c_size_t), intent(in), value :: nmemb             !! Size of the response chunk.
type(c_ptr),            intent(in), value :: data              !! C pointer to argument passed by caller.
integer(kind=c_size_t)                    :: response_callback !! Function return value.
character(len=32), pointer                :: file_name         !! File to store response to.
character(len=:), allocatable             :: chunk             !! Response chunk.
integer(kind=8)                           :: fu, rc

   response_callback = int(0, kind=c_size_t)

   if (.not. c_associated(ptr)) return
   if (.not. c_associated(data)) return

   call c_f_pointer(data, file_name)
   if (len_trim(file_name) == 0) return

   allocate (character(len=nmemb) :: chunk)
   call c_f_str_ptr(ptr, chunk)

   if(file_name.ne.' ')then
      open (access   = 'stream', &
            action   = 'write', &
            file     = trim(file_name), &
            form     = 'unformatted', &
            iostat   = rc, &
            newunit  = fu, &
            position = 'append', &
            status   = 'unknown')
   endif
   if (rc /= 0) return
   !!write (fu) chunk
   write (*,'(*(a))') chunk
   close (fu)
   ! get rid of trash file. KLUDGE
   open(file='null.garbage',newunit=fu,iostat=rc)
   if(rc.eq.0) close(unit=fu,iostat=rc,status='delete')
   deallocate (chunk)
   response_callback = nmemb
end function response_callback
end module callback

program main
use, intrinsic :: iso_c_binding
use  M_CLI2, only  : set_args, sget, lget, specified, words=>unnamed
use :: M_curl
use :: callback
implicit none
character(len=*), parameter  :: DEFAULT_PROTOCOL = 'dict'
character(len=32),target     :: file_name
type(c_ptr)                  :: curl_ptr
integer                      :: rc
integer                      :: i
character(len=:),allocatable :: URL, SERVER, LIKE
logical                      :: VERBOSE
character(len=:),allocatable :: help_text(:)
character(len=:),allocatable :: version_text(:)

    call setup()
    call set_args(' --server www.dict.org --filename:o " " --url:U " " --like:l " "',help_text,version_text)
    SERVER=sget('server')
    file_name=sget('filename')
    url=sget('url')
    verbose=lget('verbose')
    like=sget('like')
    if (SERVER=='')SERVER='www.dict.org'

    curl_ptr = curl_easy_init()

    if (.not. c_associated(curl_ptr)) then
        stop 'Error: curl_easy_init() failed'
    endif

    ! Set curl options.
    rc = curl_easy_setopt(curl_ptr, CURLOPT_DEFAULT_PROTOCOL, DEFAULT_PROTOCOL // c_null_char)
    rc = curl_easy_setopt(curl_ptr, CURLOPT_FOLLOWLOCATION,   int( 1, kind=8))
    rc = curl_easy_setopt(curl_ptr, CURLOPT_TIMEOUT,          int(10, kind=8))
    rc = curl_easy_setopt(curl_ptr, CURLOPT_NOSIGNAL,         int( 1, kind=8))
    rc = curl_easy_setopt(curl_ptr, CURLOPT_CONNECTTIMEOUT,   int(10, kind=8))
    rc = curl_easy_setopt(curl_ptr, CURLOPT_WRITEFUNCTION,    c_funloc(response_callback))
    ! for some reason have to set this or get file created with garbage name
    if(file_name.eq.' ') file_name='null.garbage'
    rc = curl_easy_setopt(curl_ptr, CURLOPT_WRITEDATA,        c_loc(file_name))

    if(url.ne.' ')then
          rc = curl_easy_setopt(curl_ptr, CURLOPT_URL,URL// c_null_char)
          ! Send request.
          call showstat()
    else
       if(size(words).eq.0)words=[' ']
       do i=1,size(words)
          if(specified('like'))then
             if(words(i).eq.' ')then
                URL='dict://'//SERVER//'/show strat' ! if no words and blank --like show strategies
             elseif(like.eq.' ')then  ! if words and --like without a specific strategy use soundex and lev
                URL='dict://'//SERVER//'/m:'//trim(words(i))//'::soundex'
                call showstat()
                URL='dict://'//SERVER//'/m:'//trim(words(i))//'::lev'
             else  ! if strategy specified use it
                URL='dict://'//SERVER//'/m:'//trim(words(i))//'::'//like
             endif
          elseif(words(i).eq.'')then  ! if no strategy and no words list dictionaries
             URL='dict://'//SERVER//'/show:db' ! list all the dictionaries
          elseif(index(words(i),':').ne.0)then  ! assume if colon present a dictionary was specified
             URL='dict://'//SERVER//'/d:'//trim(words(i))
          else
             URL='dict://'//SERVER//'/d:'//trim(words(i))//':all' ! default to all dictionaries
          endif
          call showstat()
       enddo
    endif

    call curl_easy_cleanup(curl_ptr)
contains

subroutine showstat()
integer  :: istat
   if(verbose) write(*,'(*(g0))')'URL=',url
   rc = curl_easy_setopt(curl_ptr, CURLOPT_URL,              URL// c_null_char)
   !print '(5a)', 'Saving "', URL, '" to file "', trim(file_name), '" ...'
   ! Send request.
   istat=curl_easy_perform(curl_ptr)
   if (istat /= CURLE_OK) then
      print '(a)', 'Error: curl_easy_perform() failed'
   endif
end subroutine showstat

subroutine setup()
help_text=[ CHARACTER(LEN=128) :: &
'NAME',&
'   dict(1sh) - Get definition of a word using DICT Protocol and libcurl(3c)',&
'   (LICENSE:PD)',&
'',&
'SYNOPSIS',&
'   fpm-dict(1) - [word(s) [--like [strategy]] [--server SERVER]]|',&
'             -url FULL_URL|',&
'             [--help|--version]',&
'',&
'DESCRIPTION',&
'   By default fpm-dict(1) looks up the definitions of the given words in',&
'   the dictionaries at dict://dict.org/ on port 2628.',&
'',&
'   You can also search for similar words, words by substring or regular',&
'   expression, as substrings ...',&
'',&
'   With no options a table of the available dictionaries is produced.',&
'',&
'   Note there are many dictionaries hosted on servers around the WWW.',&
'',&
'   fpm-dict(1) uses the DICT Protocol as described in RFC2229 and the',&
'   libcurl(3c) library via a clone of the fortran-curl(3f) interface.',&
'',&
'OPTIONS',&
'    word(s)       word to find and display the definition of. Can be of',&
'                  form word:dictionary. The default dictionary is "all".',&
'    --like strategy   look for words like the word specified instead of',&
'                      showing definitions, using an optional search',&
'                      strategy. defaults to trying to guess the spelling',&
'                      of the word(s) assuming it is misspelled. If no',&
'                      words are present defaults to showing available',&
'                      strategies.',&
'    --server      DICT server name. Default is "www.dict.org".',&
'    --url         directly specify url:',&
'',&
'                     dict://HOST:PORT/m:MATCH_STRING:DATABASE_NAME:STRATEGY',&
'                     dict://HOST:PORT/d:WORD:DATABASE_NAME',&
'    --help,-h     display this help and exit',&
'    --version,-v  output version information and exit',&
'',&
'EXAMPLE',&
'  Sample usage:',&
'',&
'',&
'      > fpm-dict              # list dictionary names and descriptions',&
'      > fpm-dict bash         # search all the dictionaries for specified word',&
'      > fpm-dict bash:        # use just the default dictionary',&
'      > # Find the meaning of "bash" in the "computer terms" dictionary:',&
'      > fpm-dict bash:foldoc',&
'      > fpm-dict gold:elements     # just use the "elements" dictionary',&
'      > fpm-dict slovakia:world02',&
'      >',&
'      > fpm-dict bash --like  # look for words like the one specified',&
'      > fpm-dict ''q[aeiou]'' --like RE # use RE (Regular Expression)',&
'      > fpm-dict urban --like substring',&
'      >',&
'      > # direct use of RFC 2229 syntax',&
'      > fpm-dict --url dict://www.dict.org/help',&
'      > fpm-dict --url ''dict://www.dict.org/show strat''',&
'      > fpm-dict --url dict://www.dict.org/m:urban:english:regexp',&
'',&
'SEE ALSO',&
'   look(1), spell(1), aspell(1), ispell(1), wnb(1WordNet)',&
'']
!>
!!##NAME
!!    dict(1sh) - Get definition of a word using DICT Protocol and libcurl(3c)
!!    (LICENSE:PD)
!!
!!##SYNOPSIS
!!
!!    fpm-dict(1) - [word(s) [--like [strategy]] [--server SERVER]]|
!!              -url FULL_URL|
!!              [--help|--version]
!!
!!##DESCRIPTION
!!    By default fpm-dict(1) looks up the definitions of the given words in
!!    the dictionaries at dict://dict.org/ on port 2628.
!!
!!    You can also search for similar words, words by substring or regular
!!    expression, as substrings ...
!!
!!    With no options a table of the available dictionaries is produced.
!!
!!    Note there are many dictionaries hosted on servers around the WWW.
!!
!!    fpm-dict(1) uses the DICT Protocol as described in RFC2229 and the
!!    libcurl(3c) library via a clone of the fortran-curl(3f) interface.
!!
!!##OPTIONS
!!     word(s)       word to find and display the definition of. Can be of
!!                   form word:dictionary. The default dictionary is "all".
!!     --like strategy   look for words like the word specified instead of
!!                       showing definitions, using an optional search
!!                       strategy. defaults to trying to guess the spelling
!!                       of the word(s) assuming it is misspelled. If no
!!                       words are present defaults to showing available
!!                       strategies.
!!     --server      DICT server name. Default is "www.dict.org".
!!     --url         directly specify url:
!!
!!                      dict://HOST:PORT/m:MATCH_STRING:DATABASE_NAME:STRATEGY
!!                      dict://HOST:PORT/d:WORD:DATABASE_NAME
!!     --help,-h     display this help and exit
!!     --version,-v  output version information and exit
!!
!!##EXAMPLE
!!
!!   Sample usage:
!!
!!
!!       > fpm-dict              # list dictionary names and descriptions
!!       > fpm-dict bash         # search all the dictionaries for specified word
!!       > fpm-dict bash:        # use just the default dictionary
!!       > # Find the meaning of "bash" in the "computer terms" dictionary:
!!       > fpm-dict bash:foldoc
!!       > fpm-dict gold:elements     # just use the "elements" dictionary
!!       > fpm-dict slovakia:world02
!!       >
!!       > fpm-dict bash --like  # look for words like the one specified
!!       > fpm-dict 'q[aeiou]' --like RE # use RE (Regular Expression)
!!       > fpm-dict urban --like substring
!!       >
!!       > # direct use of RFC 2229 syntax
!!       > fpm-dict --url dict://www.dict.org/help
!!       > fpm-dict --url 'dict://www.dict.org/show strat'
!!       > fpm-dict --url dict://www.dict.org/m:urban:english:regexp
!!
!!##SEE ALSO
!!    look(1), spell(1), aspell(1), ispell(1), wnb(1WordNet)

version_text=[ CHARACTER(LEN=128) :: &
'@(#)PRODUCT:         GPF (General Purpose Fortran) utilities and examples>',&
'@(#)PROGRAM:         fpm-dict(1)>',&
'@(#)DESCRIPTION:     Get definition of a word using DICT Protocol and libcurl(3c',&
'@(#)VERSION:         3.0, 20210127>',&
'@(#)AUTHOR:          John S. Urban>',&
'@(#)REPORTING BUGS:  urbanjost@comcast.net>',&
'@(#)HOME PAGE:       http://www.urbanjost.altervista.org/index.html>',&
'@(#)LICENSE:         Public Domain. This is free software: you are free to chang',&
'@(#)                 There is NO WARRANTY, to the extent permitted by law.>',&
'']
end subroutine setup
end program main
