       program demo_splitpath

          use m_io, only : splitpath
          implicit none
          integer,parameter :: maxlen=4096
          character(len=maxlen),parameter   :: file(*)=[&
             & 'dirs/name.ext  ', &
             & 'xx/IO/zz/NN.FF ', &
             & 'xx/IO/zz/NN    ', &
             & '/xx/IO/zz/NN   ', &
             & '/xx/IO/zz/     ', &
             & '/xx/IO/zz.A/   ', &
             & '/xx/IO/zz/.    ', &
             & '               ', &
             & './             ', &
             & '/              ', &
             & '/..            ', &
             & './..           ', &
             & 'name.          ', &
             & '.name          ', &
             & '.name.         ', &
             & '.              ', &
             & '..             ', &
             & '...            ']

          character(len=maxlen)  :: dir
          character(len=maxlen)  :: name
          character(len=maxlen)  :: basename
          character(len=maxlen)  :: ext
          integer                :: i
          integer                :: longest
          longest=maxval(len_trim(file)) ! find longest filename

          do i=1,size(file)
             call splitpath(file(i), dir, name, basename, ext)
             write(*,'(*("| ",a:))')  &
             & file(i)(:longest),     &
             & dir(:longest),         &
             & name(:longest),        &
             & basename(:longest),    &
             & ext(:longest)
          enddo
       end program demo_splitpath
