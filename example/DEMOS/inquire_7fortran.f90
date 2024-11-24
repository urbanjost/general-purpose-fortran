      program demo_inquire
      implicit none
      character(len=4096)  :: filename
      character(len=20)    :: mode
      integer              :: ios
      character(len=256)   :: message
      integer              :: lun
      call print_inquire(lun,'')
      contains
      subroutine print_inquire(lun_in,namein_in)

      !@(#) print_inquire(3f) INQUIRE a file by name/number and print results

      ! if unit >= 0 then query by unit number, else by name
      integer,intent(in),optional             :: lun_in
      character(len=*),intent(in),optional    :: namein_in

      integer                        :: ios
      character(len=256)             :: message
      character(len=:),allocatable   :: namein
      integer                        :: lun

      ! STATUS=NEW|REPLACE|OLD|SCRATCH|UNKNOWN

      ! SEQUENTIAL | DIRECT | STREAM
      character(len=20) :: access         ; namelist/inquire/access

      ! FORMATTED | UNFORMATTED
      character(len=20) :: form           ; namelist/inquire/form

      ! ASIS | REWIND | APPEND
      character(len=20) :: position       ; namelist/inquire/position

      ! READ | WRITE | READWRITE
      character(len=20) :: action         ; namelist/inquire/action

      character(len=20) :: asynchronous   ; namelist/inquire/asynchronous
      character(len=20) :: blank          ; namelist/inquire/blank
      character(len=20) :: decimal        ; namelist/inquire/decimal
      character(len=20) :: delim          ; namelist/inquire/delim
      character(len=20) :: direct         ; namelist/inquire/direct
      character(len=20) :: encoding       ; namelist/inquire/encoding
      character(len=20) :: formatted      ; namelist/inquire/formatted
      character(len=20) :: name           ; namelist/inquire/name
      character(len=20) :: pad            ; namelist/inquire/pad
      character(len=20) :: read           ; namelist/inquire/read
      character(len=20) :: readwrite      ; namelist/inquire/readwrite
      character(len=20) :: round          ; namelist/inquire/round
      character(len=20) :: sequential     ; namelist/inquire/sequential
      character(len=20) :: sign           ; namelist/inquire/sign
      character(len=20) :: stream         ; namelist/inquire/stream
      character(len=20) :: unformatted    ; namelist/inquire/unformatted
      character(len=20) :: write          ; namelist/inquire/write
      integer           :: id             ; namelist/inquire/id
      integer           :: nextrec        ; namelist/inquire/nextrec
      integer           :: number         ; namelist/inquire/number
      integer           :: pos            ; namelist/inquire/pos
      integer           :: recl           ; namelist/inquire/recl
      integer           :: size           ; namelist/inquire/size
      logical           :: exist          ; namelist/inquire/exist
      logical           :: named          ; namelist/inquire/named
      logical           :: opened         ; namelist/inquire/opened
      logical           :: pending        ; namelist/inquire/pending

         if(present(namein_in))then
            namein=namein_in
         else
            namein=''
         endif
         if(present(lun_in))then
            lun=lun_in
         else
            lun=-1
         endif
         ! exist, opened, and named always become defined
         ! unless an error condition occurs.
         !!write(*,*)'LUN=',lun,' FILENAME=',namein
         name=''
         if(namein == ''.and.lun /= -1)then
            write(*,*) '*print_inquire* checking unit',lun
            inquire(unit=lun,                                          &
            & recl=recl,nextrec=nextrec,pos=pos,size=size,             &
            & position=position,                                       &
            & name=name,                                               &
            & form=form,formatted=formatted,unformatted=unformatted,   &
            & access=access,sequential=sequential,direct=direct,       &
            & stream=stream,                                           &
            & action=action,read=read,write=write,readwrite=readwrite, &
            & sign=sign,                                               &
            & round=round,                                             &
            & blank=blank,decimal=decimal,delim=delim,                 &
            & encoding=encoding,pad=pad,                               &
            & named=named,opened=opened,exist=exist,number=number,     &
            & pending=pending,asynchronous=asynchronous,               &
            & iostat=ios,err=999,iomsg=message)
         elseif(namein /= '')then
            write(*,*) '*print_inquire* checking file:'//namein
            inquire(file=namein,                                       &
            & recl=recl,nextrec=nextrec,pos=pos,size=size,             &
            & position=position,                                       &
            & name=name,                                               &
            & form=form,formatted=formatted,unformatted=unformatted,   &
            & access=access,sequential=sequential,direct=direct,       &
            & stream=stream,                                           &
            & action=action,read=read,write=write,readwrite=readwrite, &
            & sign=sign,                                               &
            & round=round,                                             &
            & blank=blank,decimal=decimal,delim=delim,                 &
            & encoding=encoding,pad=pad,                               &
            & named=named,opened=opened,exist=exist,number=number,     &
            & pending=pending,asynchronous=asynchronous,               &
            & iostat=ios,err=999,iomsg=message)
            if(name == '')name=namein
         else
            write(*,*) &
            &'*print_inquire* must specify either filename or unit number'
         endif
         write(*,nml=inquire,delim='none')
         return

      999   continue
         write(*,*)'*print_inquire* bad inquire'
      !  If an error condition occurs during execution of an INQUIRE statement,
      !  all of the inquiry identifiers except ios become undefined.
         write(*,*) &
         &'*print_inquire* inquire call failed,iostat=',ios,'message=',message
      end subroutine print_inquire
      end program demo_inquire
