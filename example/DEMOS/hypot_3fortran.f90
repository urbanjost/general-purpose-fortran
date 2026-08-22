         program demo_hypot
         use, intrinsic :: iso_fortran_env, only : real32, real64, real128
         implicit none
         real(kind=real32)             :: x, y
         real(kind=real32),allocatable :: xs(:), ys(:)
         integer                       :: i
         character(len=*),parameter    :: f='(a,/,SP,*(3x,g0,1x,g0:,/))'

           ! basics
            write(*,*)hypot(3.0,4.0)
            write(*,*)hypot(1.0,0.25)
            write(*,*)hypot(1.0,0.5)

            x=3.0
            y=4.0
            ! all equivalent
            write(*,*)sqrt(x**2+y**2), hypot(x,y), abs(cmplx(x,y))

            ! a common use is to determine the distance of a point
            ! from the origin
            x = 1.e0_real32
            y = 0.5e0_real32

            write(*,*)
            write(*,'(*(g0))')'point <',x,',',y,'> is ',hypot(x,y)
            write(*,'(*(g0))')'units away from the origin'
            write(*,*)

           ! elemental
            xs=[  x,  x**2,  x*10.0,  x*15.0, -x**2  ]
            ys=[  y,  y**2, -y*20.0,  y**2,   -y**2  ]

            write(*,f)"the points",(xs(i),ys(i),i=1,size(xs))
            write(*,f)"have distances from the origin of ",hypot(xs,ys)
            write(*,f)"the closest is",minval(hypot(xs,ys))

         ! Finding primitive Euclidean triple sets, which are pairs of whole
         ! numbers that form the sides of a right triangle with a hypotenuse
         ! whose length is also a whole number (like [3,4,5]).
         !
         EUCLIDEAN: block
         ! Euclid's formula is a fundamental formula for generating Pythagorean
         ! triples given an arbitrary pair of integers m and n with m > n > 0.
         ! The formula states that the integers
         !
         !    a = m**2 − n**2
         !    b = 2*m*n
         !    c = m**2 + n**2
         !
         ! form a Pythagorean triple.
            integer :: i,j
            real    :: m,n, a,b,c
            integer,parameter :: maxside=100
            ! find all primitive Euclidean triplets with sides a and b <= maxside
            do i=1,maxside
               do j=1,maxside
                  m=i
                  n=j
                  ! skip values unless m > 2
                  if(m.le.n)cycle
                  a=m**2-n**2
                  b=2*m*n
                  c=m**2+n**2
                  ! skip writing it if it is not a primitive Euclidean triplet
                  if (gcd_vector(nint([a,b,c])) > 1)cycle
                  if(a>maxside.or.b>maxside)cycle
                  ! c should be hypot(a,b) or equivalently abs(cmplx(a,b))
                  write(*,*) a, b, c, hypot(a,b), c==hypot(a,b)
               enddo
            enddo
         endblock EUCLIDEAN
         contains
         function gcd(m,n) result(answer) ! greatest common denominator
         integer,intent(in) :: m, n
         integer            :: answer
         integer            :: irest
         intrinsic          :: mod,abs
         integer            :: ifirst
            ifirst=abs(m)
            answer=abs(n)
            if(answer.eq.0)then
               answer=ifirst
            else
               do
                  irest = mod(ifirst,answer)
                  if(irest == 0)  exit
                  ifirst = answer
                  answer = irest
               enddo
               answer= iabs(answer)
            endif
         end function gcd
         integer function gcd_vector(m)
         integer,intent(in) :: m(:)
         integer            :: vsize
         integer            :: i
            vsize=size(m)
            if(vsize.gt.0)then
               gcd_vector = m(1)
               TILLONE: do i=1,vsize
                  gcd_vector = gcd(gcd_vector,iabs(m(i)))
                  if (gcd_vector.eq.1) exit TILLONE
               enddo TILLONE
            else
               gcd_vector=0
            endif
         end function gcd_vector

         end program demo_hypot
