     program demo_perturb
     ! generate a random perturbation of an array
     use M_orderpack, only : perturb
     implicit none
     character(len=*),parameter :: g='(*(g0,1x))'
     integer,allocatable :: xout(:,:)
     integer          :: isz, i
     isz=200
        ! randomly perturb location of values
        !
        ! make an array with three initially identical rows of
        ! values perturbed by different amounts
        if(allocated(xout))deallocate(xout)
        allocate(xout(3,isz))
        xout(1,:)=[(i,i=isz,1,-1)]*10
        xout(2,:)=xout(1,:)
        xout(3,:)=xout(1,:)
        ! perturb each row a different amount
        call perturb(xout(1,:),0.0)
        call perturb(xout(2,:),0.1)
        call perturb(xout(3,:),1.0)
        ! show values
        write(*,'(a)')'count    unchanged  perturbed  random'
        do i=1,size(xout,dim=2)
           write(*,'(*(i8,1x))')i,xout(:,i)
        enddo
     char: block
     character(len=:),allocatable :: cdont(:)
        cdont=[character(len=20) :: 'a', 'be', 'car', 'dam','fan','gas','egg']
        isz=size(cdont)
        write(*,g)'Original.................:',(trim(cdont(i)),i=1,isz)
        call perturb(cdont,1.0)
        write(*,g)'Perturbed ...............:',(trim(cdont(i)),i=1,isz)
        write(*,g)
     endblock char

     end program demo_perturb
