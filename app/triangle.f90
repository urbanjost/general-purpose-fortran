!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
subroutine help_usage(l_help)
implicit none
! @(#)help_usage(3f): prints help information
logical,intent(in)             :: l_help
character(len=:),allocatable :: help_text(:)
integer                        :: i
if(l_help)then
help_text=[ CHARACTER(LEN=128) :: &
'NAME                                                                            ',&
'   triangle(1f) - resolution of a triangle                                      ',&
'   (LICENSE:PD)                                                                 ',&
'SYNOPSIS                                                                        ',&
'   triangle -a NN -b NN -c NN -ta NN -tb NN -tc NN -radians -degrees            ',&
'DESCRIPTION                                                                     ',&
'   Given three side or angle elements of a triangle out of six,                 ',&
'   this program will determine the missing elements and calculate the           ',&
'   surface. The not-given elements must be put to zero.                         ',&
'AUTHOR                                                                          ',&
'   Based on an example F90 version by J-P Moreau (www.jpmoreau.fr).             ',&
'                                                                                ',&
'   Ref.: "Mathématiques en Turbo-Pascal By M. Ducamp                            ',&
'   and A. Reverchon (vol 2), Eyrolles, Paris, 1988"                             ',&
'   [BIBLI 05].                                                                  ',&
'OPTIONS                                                                         ',&
'   a   length of side A                                                         ',&
'   b   length of side B                                                         ',&
'   c   length of side C                                                         ',&
'   ta  angle opposite to side A                                                 ',&
'   tb  angle opposite to side B                                                 ',&
'   tc  angle opposite to side C                                                 ',&
'EXAMPLE                                                                         ',&
'   sample run                                                                   ',&
'                                                                                ',&
'     $triangle -a 18 -tb 110 -tc 52.2                                           ',&
'                                                                                ',&
'     Angle unit: PI = 180                                                       ',&
'                                                                                ',&
'     A  =    18.0000000000000                                                   ',&
'     B  =    55.3311291183785                                                   ',&
'     C  =    46.5260338924988                                                   ',&
'                                                                                ',&
'     TA =    17.8000000000000                                                   ',&
'     TB =   110.0000000000000                                                   ',&
'     TC =    52.2000000000000                                                   ',&
'                                                                                ',&
'     Surface:    393.481559106180                                               ',&
'']
   WRITE(*,'(a)')(trim(help_text(i)),i=1,size(help_text))
   stop ! if -help was specified, stop
endif
end subroutine help_usage
!-----------------------------------------------------------------------------------------------------------------------------------
!>
!!##NAME
!!    triangle(1f) - resolution of a triangle
!!    (LICENSE:PD)
!!##SYNOPSIS
!!
!!    triangle -a NN -b NN -c NN -ta NN -tb NN -tc NN -radians -degrees
!!##DESCRIPTION
!!    Given three side or angle elements of a triangle out of six,
!!    this program will determine the missing elements and calculate the
!!    surface. The not-given elements must be put to zero.
!!##AUTHOR
!!    Based on an example F90 version by J-P Moreau (www.jpmoreau.fr).
!!
!!    Ref.: "Mathématiques en Turbo-Pascal By M. Ducamp
!!    and A. Reverchon (vol 2), Eyrolles, Paris, 1988"
!!    [BIBLI 05].
!!##OPTIONS
!!    a   length of side A
!!    b   length of side B
!!    c   length of side C
!!    ta  angle opposite to side A
!!    tb  angle opposite to side B
!!    tc  angle opposite to side C
!!##EXAMPLE
!!
!!    sample run
!!
!!      $triangle -a 18 -tb 110 -tc 52.2
!!
!!      Angle unit: PI = 180
!!
!!      A  =    18.0000000000000
!!      B  =    55.3311291183785
!!      C  =    46.5260338924988
!!
!!      TA =    17.8000000000000
!!      TB =   110.0000000000000
!!      TC =    52.2000000000000
!!
!!      Surface:    393.481559106180
subroutine help_version(l_version)
implicit none
! @(#)help_version(3f): prints version information
logical,intent(in)             :: l_version
character(len=:),allocatable   :: help_text(:)
integer                        :: i
if(l_version)then
help_text=[ CHARACTER(LEN=128) :: &
'@(#)PRODUCT:        GPF (General Purpose Fortran) utilities and examples>',&
'@(#)PROGRAM:        triangle(1)>',&
'@(#)DESCRIPTION:    resolution of a triangle>',&
'@(#)VERSION:        1.0, 20190315>',&
'@(#)AUTHOR:         John S. Urban>',&
'@(#)COMPILED:       Mon, Mar 15th, 2021 12:51:15 AM>',&
'']
   WRITE(*,'(a)')(trim(help_text(i)(5:len_trim(help_text(i),kind=kind(1))-1)),i=1,size(help_text))
   stop ! if -version was specified, stop
endif
end subroutine help_version
!-----------------------------------------------------------------------------------------------------------------------------------
Program Triangle
use M_kracken, only : kracken, dget, lget
implicit none
doubleprecision,parameter :: PI=3.14159265359d0
doubleprecision           :: a,b,c,ta,tb,tc,one_pi
integer                   :: two

   call kracken('triangle','-a 0 -b 0 -c 0 -ta 0 -tb 0 -tc 0 -radians .f. -degrees .f. -help .f. -version .f.')
   call help_usage(lget("triangle_help"))      ! call routine generated by $DOCUMENT HELP
   call help_version(lget("triangle_version")) ! call routine generated by $DOCUMENT VERSION

   one_pi=180.0d0
   if(lget('triangle_degrees'))then
      one_pi=180.0d0
   elseif(lget('triangle_radians'))then
      one_pi=PI
   endif
   a=dget('triangle_a')
   b=dget('triangle_b')
   c=dget('triangle_c')
   ta=dget('triangle_ta')
   tb=dget('triangle_tb')
   tc=dget('triangle_tc')

   ta=ta*PI/one_pi
   tb=tb*PI/one_pi
   tc=tc*PI/one_pi
   two=0

   call DisplayTriangle(a,b,c,ta,tb,tc,one_pi,two)
   if (two.eq.1) then
      print *, ' There is a second triangle solution:'
      call DisplayTriangle(a,b,c,ta,tb,tc,one_pi,two)
   endif

   stop

contains
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
   doubleprecision Function SQR(x)
   doubleprecision       :: x
      SQR=x*x
   end Function SQR
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
   Subroutine Exchange(i,j,angle,indx,side)
      integer,intent(in) :: i
      integer,intent(in) :: j
      integer            :: n
      doubleprecision    :: angle(3)
      doubleprecision    :: side(3)
      integer            :: indx(3)
      doubleprecision    :: r
      n=indx (i); indx (i)=indx (j); indx (j)=n
      r=angle(i); angle(i)=angle(j); angle(j)=r
      r=side (i); side (i)=side (j); side (j)=r
   end Subroutine Exchange
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
   Subroutine CalculateAngle(i,angle,side)
      integer                   :: i
      integer                   :: j
      integer                   :: k
      doubleprecision,parameter :: PI=3.14159265359d0
      doubleprecision angle(3), side(3)
      doubleprecision r
      j=1+MOD(i,3)
      k=1+MOD(j,3)
      r=(SQR(side(j))+SQR(side(k))-SQR(side(i)))/2.d0/side(j)/side(k)
      angle(i)=PI/2.d0-DATAN(r/DSQRT(1.d0-r*r))
   end Subroutine CalculateAngle
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
   integer Function Triangle1(a,b,c,ta,tb,tc,s,two)
      doubleprecision,parameter :: PI=3.14159265359d0
      doubleprecision  a,b,c,ta,tb,tc,s
      integer two
      doubleprecision  side(3), angle(3)
      integer indx(3)
      doubleprecision    :: r
      integer            :: nbside
      integer            :: nbangle
      integer            :: i
      !------------ prepare indx tables -------------
      side(1)=a; angle(1)=ta
      side(2)=b; angle(2)=tb
      side(3)=c; angle(3)=tc
      do i=1, 3
         indx(i)=i
      enddo
      !------------ verify consistency of data -------
      Triangle1=0; nbside=0; nbangle=0
      !angles must be in 0,PI and sides > 0
      do i=1, 3
         if (angle(i)<0.or.angle(i)>PI.or.side(i)<0) then
            return
         endif
      enddo
      do i=1, 3
         if (side(i)>0) nbside=nbside+1
      enddo
      do i=1, 3
         if (angle(i)>0) nbangle=nbangle+1
      enddo
      if (nbside.eq.0.or.nbside+nbangle.ne.3.or.angle(1)+angle(2)+angle(3)>PI) then
         return
      endif
      !end verify consistency of data
      Select Case(nbside)
         !-------------------- 1 side given -----------
       case(1)
         do i=1, 3
            if (angle(i).eq.0) then  !calculate missing angle
               angle(i)=PI-angle(1)-angle(2)-angle(3)
            endif
         enddo
         if (side(2)>0)  call Exchange(1,2,angle,indx,side)   !put given side
         if (side(3)>0)  call Exchange(1,3,angle,indx,side)   !in position 1
         side(2)=side(1)*dsin(angle(2))/dsin(angle(1))
         side(3)=side(1)*dsin(angle(3))/dsin(angle(1))
         !-------------------- 2 sides given ----------
       case(2)
         if (side(3)>0) then     !put unknown side in position 3
            if (side(1)>0) then
               call Exchange(2,3,angle,indx,side)
            else
               call Exchange(1,3,angle,indx,side)
            endif
         endif
         if (angle(3)>0) then  !a, b, tc given
            side(3)=DSQRT(a*a+b*b-2.d0*a*b*dcos(angle(3)))
            do i=1, 2
               call CalculateAngle(i,angle,side)
            enddo
         else                  !a, b, ta given
            if (angle(1).eq.0) call Exchange(1,2,angle,indx,side)
            r=side(2)*dsin(angle(1))
            if (r>side(1)) return
            r=DSQRT(side(1)*side(1)-r*r)
            if (angle(1) >= PI/2.and.side(1) <= side(2)) return
            if (angle(1) < PI/2.and.side(1) < side(2)) then
               if (two.eq.1) then
                  side(3)=side(2)*dcos(angle(1)) - r
               else
                  side(3)=side(2)*dcos(angle(1)) + r
                  two=1
               endif
            else
               side(3)=side(2)*dcos(angle(1)) + r
            endif
            do i=2, 3
               call CalculateAngle(i,angle,side)
            enddo
         endif
         !-------------------- 3 sides given ----------
       case(3)
         if (c<=DABS(a-b).or.c>=a+b)  return
         do i=1, 3
            call CalculateAngle(i,angle,side)
         enddo
      End Select
      !----------------------- desindex ---------------
      a=side(indx(1)); ta=angle(indx(1))
      b=side(indx(2)); tb=angle(indx(2))
      c=side(indx(3)); tc=angle(indx(3))
      !---------------------- calculate surface -------
      r=(a+b+c)/2
      s=DSQRT(r*(r-a)*(r-b)*(r-c))
      Triangle1=1   !success
   End Function Triangle1
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
   Subroutine DisplayTriangle(a,b,c,ta,tb,tc,one_pi,two)
      doubleprecision,parameter :: PI=3.14159265359d0
      doubleprecision  a,b,c,ta,tb,tc,one_pi
      doubleprecision  xa,xb,xc,xta,xtb,xtc,s
      integer ii,two
      xa=a; xb=b; xc=c
      xta=ta; xtb=tb; xtc=tc
      ii=Triangle1(xa,xb,xc,xta,xtb,xtc,s,two)
      if (ii.eq.1) then
         print *,'  A  = ', xa
         print *,'  B  = ', xb
         print *,'  C  = ', xc
         print *,''
         print *,'  TA = ', xta*(one_pi/PI)
         print *,'  TB = ', xtb*(one_pi/PI)
         print *,'  TC = ', xtc*(one_pi/PI)
         print *,''
         print *,'  Surface: ', s
      else
         print *,' Wrong data or no solution found.'
      endif
      print *,''
   End Subroutine DisplayTriangle
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
END Program Triangle
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
