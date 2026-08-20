     program demo_readline
     use,intrinsic :: iso_fortran_env, only : stdin=>input_unit
     use,intrinsic :: iso_fortran_env, only : iostat_end
     use M_unicode, only : readline, len, trim
     use M_unicode, only : assignment(=), ch=>character, ut=>unicode_type
     implicit none
     type(ut)                     :: ln
     character(len=:),allocatable :: aline
     integer,allocatable          :: ints(:)
     integer                      :: iostat, lun, i
     character(len=*),parameter :: filedata(*)=[character(len=80) :: &
        'The famous Confucian expression:', &
        '', &
        ' "己所不欲，勿施於人"', &
        ' (jǐ suǒ bù yù, wù shī yú rén)', &
        'or', &
        ' "What you do not want done to yourself,', &
        ' do not do to others":']
        ! create a scratch file to read
        open(newunit=lun,status='scratch',pad='yes')
        write(lun,'(a)')(trim(filedata(i)),i=1,size(filedata))
        !----------------------------------------------------------------
        ! read back UTF-8 byte stream and show the lines read
        rewind(unit=lun)
        do
           ln=readline(lun,iostat=iostat)
           if(iostat.ne.0)exit
           ! write the glyph length, byte length, line in brackets
           write(*,'(i4,1x,i4,1x,a)')len(ln),len(ch(ln)),'['//ch(ln)//']'
        enddo
        call checkit()
        !----------------------------------------------------------------
        ! the same thing except convert to default intrinsic types
        rewind(unit=lun)
        do
           ln=readline(lun,iostat=iostat)
           if(iostat.ne.0)exit
           ! assign the string to an allocatable array of integers
           ints=ln
           ! and the string to a character variable
           aline=ln
           write(*,'(i4,1x,i4,1x,a)')len(ln),len(aline),'['//ch(ln)//']'
        enddo
        call checkit()
        !----------------------------------------------------------------
        ! again but this time show the lines as Unicode codepoints
        rewind(unit=lun)
        do
           ln=readline(lun,iostat=iostat)
           if(iostat.ne.0)exit
           if(len(ln).eq.0)then
              write(*,'(/,10(g0,1x)," ...")')[0,0]
           else
              write(*,'(/,10(g0,1x)," ...")')ln%codepoint()
           endif
        enddo
        call checkit()
        !----------------------------------------------------------------
        ! show the line as Unicode codepoints using default integer array
        rewind(unit=lun)
        do
           ln=readline(lun,iostat=iostat)
           if(iostat.ne.0)exit
           ! assign the string to an allocatable array of integers
           ints=ln
           if(size(ints).eq.0)ints=[0,0]
           write(*,'(/,10(g0,1x)," ...")')ints
        enddo
        call checkit()
        !----------------------------------------------------------------
     contains
     subroutine checkit()
        if(iostat /= iostat_end)then
           write(*,*)'error reading input:',ch(trim(ln))
        endif
        write(*,*)
     end subroutine checkit
     end program demo_readline
