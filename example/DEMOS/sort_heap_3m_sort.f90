     program demo_sort_heap
     use M_sort, only : sort_heap
     implicit none
     integer,parameter            :: isz=10000
     real                         :: rr(isz)
     integer                      :: ii(isz)
     character(len=63)            :: cc(isz)
     integer                      :: indx(isz)
     integer                      :: i
     write(*,*)'initializing array with ',isz,' random numbers'
     CALL RANDOM_NUMBER(RR)
     rr=rr*450000.0
     ii=rr
     do i=1,size(cc)
        cc(i)=random_string(&
        & 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789 ', &
        & len(cc))
     enddo

     write(*,*)'checking if real values are sorted(3f)'
     call sort_heap(rr,indx)
     ! use the index array to actually move the input array into a sorted order
     rr=rr(indx)
     do i=1,isz-1
        if(rr(i).gt.rr(i+1))then
           write(*,*)'Error in sorting reals small to large ',i,rr(i),rr(i+1)
        endif
     enddo
     write(*,*)'test of real sort_heap(3f) complete'

     write(*,*)'checking if integer values are sorted(3f)'
     call sort_heap(ii,indx)
     ! use the index array to actually move the input array into a sorted order
     ii=ii(indx)
     do i=1,isz-1
        if(ii(i).gt.ii(i+1))then
           write(*,*)'Error sorting integers small to large ',i,ii(i),ii(i+1)
        endif
     enddo
     write(*,*)'test of integer sort_heap(3f) complete'

     write(*,*)'checking if character values are sorted(3f)'
     call sort_heap(cc,indx)
     ! use the index array to actually move the input array into a sorted order
     cc=cc(indx)
     do i=1,isz-1
        if(cc(i).gt.cc(i+1))then
           write(*,*)'Error sorting characters small to large ',i,cc(i),cc(i+1)
        endif
     enddo
     write(*,*)'test of character sort_heap(3f) complete'

     contains

     function random_string(chars,length) result(out)

     ! create random string from provided chars

     character(len=*),intent(in)     :: chars
     integer,intent(in)              :: length
     character(len=:),allocatable    :: out
        real                         :: x
        integer                      :: ilen   ! length of list of characters
        integer                      :: which
        integer                      :: i
        ilen=len(chars)
        out=''
        if(ilen.gt.0)then
           do i=1,length
              call random_number(x)
              which=nint(real(ilen-1)*x)+1
              out=out//chars(which:which)
           enddo
        endif
     end function random_string

     end program demo_sort_heap
