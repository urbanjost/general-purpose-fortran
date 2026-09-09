     program demo_quote
     use M_strings, only : quote
     implicit none
     integer                      :: i
     character(len=*),parameter   :: f='(*(g0))'
     character(len=:),allocatable :: str
     character(len=80),parameter  :: data(3)=[character(len=80)::&
        'test string',&
        'quote="',&
        '"word1" "word2"']
        do i=1,size(data)
           ! the original string
           write(*,'(a)')'ORIGINAL      '//trim(data(i))

           ! the string processed by quote(3f)
           str=quote(data(i))
           write(*,'(a)')'QUOTED        '//str

           ! write the string list-directed to compare the results
           write(*,f,advance='no') 'LIST DIRECTED'
           ! default is often NONE or APOSTROPHE
           write(*,*,delim='quote') trim(data(i))
        enddo
     end program demo_quote
