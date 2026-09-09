      program demo_namelist
         use, intrinsic :: iso_fortran_env, only : &
         & stderr=>ERROR_UNIT,&
         & stdin=>INPUT_UNIT,&
         & stdout=>OUTPUT_UNIT
         implicit none
         integer           :: lun

         ! create a namelist and initialize the values
         logical           :: l=.true.
         character(len=10) :: c='XXXXXXXXXX'
         real              :: r=12.3456
         integer           :: i=789
         complex           :: x=(12345.6789,9876.54321)
         doubleprecision   :: d= 123456789.123456789d0
         integer           :: a(5)=[1,2,3,4,5]
         type point
            integer           :: x=0
            integer           :: y=0
            character(len=10) :: color='red'
         endtype point
         type(point) :: dot
         namelist /nlist/ l,c,r,i,x,d,a,dot
         integer            :: iostat
         character(len=256) :: iomsg

         open(file='_tmp_',newunit=lun,action='readwrite')

         write(stdout,*)'initial nlist'
         write(stdout,nlist,delim='quote')
         flush(stdout)
         write(lun,nlist,delim='quote')

         write(stdout,*)'change values and print nlist again'
         flush(stdout)
         a=[10,20,30,40,50]
         dot%color='orange'
         write(lun,nlist,delim='quote')

         write(stdout,*)'read back values. Can have multiple sets in a file'
         rewind(lun)
         read(lun,nlist,iostat=iostat,iomsg=iomsg)
         if(iostat.ne.0)then
            write(stdout,*)'<ERROR> first read:',trim(iomsg)
         endif
         read(lun,nlist,iostat=iostat,iomsg=iomsg)
         if(iostat.ne.0)then
            write(stdout,*)'<ERROR> second read:',trim(iomsg)
         endif
         write(stdout,*)'values after reads(default delim):'
         write(stdout,nlist)
         flush(stdout)

      end program demo_namelist
