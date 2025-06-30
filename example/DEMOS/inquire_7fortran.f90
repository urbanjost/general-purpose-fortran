      program demo_inquire
      implicit none
      integer :: lun=40
      integer :: iostat
         write(*,*)'is it open or predefined?'
         call print_inquire(lun,'')
         write(*,*)'what are the defaults?'
         open(unit=lun)
         call print_inquire(lun,'')
         close(unit=lun,status='delete',iostat=iostat)
      contains
      subroutine print_inquire(lun_in,filename)

      ! @(#) print_inquire(3f) print INQUIRE of file by name/number

      integer,intent(in),optional           ::  lun_in
      character(len=*),intent(in),optional  ::  filename
      integer                               ::  iostat
      character(len=256)                    ::  message
      character(len=:),allocatable          ::  filename_
      integer                               ::  lun
      ! STATUS=NEW|REPLACE|OLD|SCRATCH|UNKNOWN
      ! SEQUENTIAL | DIRECT | STREAM | UNDEFINED
      character(len=20)  ::  access        ;  namelist/inquire/access
      character(len=20)  ::  asynchronous  ;  namelist/inquire/asynchronous
      character(len=20)  ::  blank         ;  namelist/inquire/blank
      character(len=20)  ::  decimal       ;  namelist/inquire/decimal
      character(len=20)  ::  delim         ;  namelist/inquire/delim
      character(len=20)  ::  direct        ;  namelist/inquire/direct
      character(len=20)  ::  encoding      ;  namelist/inquire/encoding
      !  FORMATTED   |  UNFORMATTED
      character(len=20)  ::  form          ;  namelist/inquire/form
      character(len=20)  ::  formatted     ;  namelist/inquire/formatted
      character(len=20)  ::  unformatted   ;  namelist/inquire/unformatted
      character(len=20)  ::  name          ;  namelist/inquire/name
      character(len=20)  ::  pad           ;  namelist/inquire/pad
      !  ASIS        |  REWIND       |  APPEND
      character(len=20)  ::  position      ;  namelist/inquire/position
      !  READ        |  WRITE        |  READWRITE
      character(len=20)  ::  action        ;  namelist/inquire/action
      character(len=20)  ::  read          ;  namelist/inquire/read
      character(len=20)  ::  readwrite     ;  namelist/inquire/readwrite
      character(len=20)  ::  write         ;  namelist/inquire/write
      character(len=20)  ::  round         ;  namelist/inquire/round
      character(len=20)  ::  sequential    ;  namelist/inquire/sequential
      character(len=20)  ::  sign          ;  namelist/inquire/sign
      character(len=20)  ::  stream        ;  namelist/inquire/stream
      integer            ::  id            ;  namelist/inquire/id
      integer            ::  nextrec       ;  namelist/inquire/nextrec
      integer            ::  number        ;  namelist/inquire/number
      integer            ::  pos           ;  namelist/inquire/pos
      integer            ::  recl          ;  namelist/inquire/recl
      integer            ::  size          ;  namelist/inquire/size
      logical            ::  exist         ;  namelist/inquire/exist
      logical            ::  named         ;  namelist/inquire/named
      logical            ::  opened        ;  namelist/inquire/opened
      logical            ::  pending       ;  namelist/inquire/pending

         if(present(filename))then
            filename_ =filename
         else
            filename_ =''
         endif
         lun=merge(lun_in,-1,present(lun_in))
         ! exist, opened, and named always become defined
         ! unless an error condition occurs.
         if(filename_  == ''.and.lun /= -1)then
           write(*,*)'*print_inquire* checking unit',lun
           inquire(unit=lun,recl=recl,nextrec=nextrec,pos=pos,size=size,      &
           & position=position,name=name,form=form,formatted=formatted,       &
           & unformatted=unformatted,access=access,sequential=sequential,     &
           & direct=direct,stream=stream,action=action,read=read,write=write, &
           & readwrite=readwrite,sign=sign,round=round,blank=blank,           &
           & decimal=decimal,delim=delim,encoding=encoding,pad=pad,           &
           & named=named,opened=opened,exist=exist,number=number,             &
           !bug & pending=pending,                                            &
           & asynchronous=asynchronous,                                       &
           & iostat=iostat,err=999,iomsg=message)
         elseif(filename_  /= '')then
           write(*,*)'*print_inquire* checking file:'//filename_
           inquire(file=filename_,                                            &
           & recl=recl,nextrec=nextrec,pos=pos,                               &
           & size=size,position=position,name=name,                           &
           & form=form,formatted=formatted,unformatted=unformatted,           &
           & access=access,sequential=sequential,direct=direct,stream=stream, &
           & action=action,read=read,write=write,readwrite=readwrite,         &
           & sign=sign,round=round,blank=blank,decimal=decimal,delim=delim,   &
           & encoding=encoding,pad=pad,named=named,opened=opened,exist=exist, &
           & number=number,pending=pending,asynchronous=asynchronous,         &
           & iostat=iostat,err=999,iomsg=message)
         else
            write(*,*) &
            & '*print_inquire* must specify either filename or unit number'
         endif
         write(*,nml=inquire,delim='none')
         return
      999   continue
         write(*,*)'*print_inquire* bad inquire'
      !  If an error condition occurs during execution of an INQUIRE statement,
      !  all of the inquiry identifiers except iostat become undefined.
         write(*,*) '*print_inquire* inquire call failed,iostat=',iostat, &
         & 'message=',message
      end subroutine print_inquire
      end program demo_inquire
