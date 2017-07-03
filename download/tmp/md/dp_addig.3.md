[UP]

-----------------------------------------------------------------------------------------------------------------------------------
                                              Manual Reference Pages  - dp_addig (3)
-----------------------------------------------------------------------------------------------------------------------------------
                                                                 
NAME

    dp_addig(3f) - [M_math] compare two DOUBLEPRECISION numbers only up to a specified number of digits

CONTENTS

    Synopsis
    Description
    Example
    Notes
    References
    Dependencies
    Files
    Legal Restrictions
    Qa

SYNOPSIS

    subroutine dp_addig(x,y,rdgits,acurcy,ind)


           doubleprecision,intent(in)     :: X
           doubleprecision,intent(in)     :: Y
           doubleprecision,intent(in)     :: DIGI0
           doubleprecision,intent(out)    :: acurcy
           integer,intent(out) :: ind



DESCRIPTION

    This procedure is used to check how closely two numbers agree.

          call dp_addig(X,Y,DIGI0,ACURCY,IND)



    the values X and Y are the numbers to compare, and DIGI0 is the threshold number of digits to consider significant in returning
    IND. If X and Y are considered equal within DIGI0 relative tolerance,

           IND    = 0, if tolerance is     satisfied.
                  = 1, if tolerance is not satisified.



    The result ACURCY gives a measure of the number of leading digits in X which are the same as the number of leading digits in Y.

               ACURCY=-log10((X-Y)/Y)   if X != Y and Y != 0
               ACURCY=-log10(X-Y)       if X != Y and Y = 0
               ACURCY=8                 if X=Y


               ACURCY is never less than -8 or greater than 8



    TOLERANCE ... X and Y are considered equal within DIGI0 relative tolerance, if ACURCY is greater than DIGI0.

         For example, Take some numbers and compare then to 1.2345678 ...

          ================================================
          A number     |    ACURCY       |   ACURCY
                       |    1.2345678=Y  |   1.2345678=X
          ================================================
           1.234680    |    3.7900571    |   3.7901275
           1.2345378   |    4.6144510    |   4.6144404
           2.2234568   |    0.096367393  |   0.35188114
           1.2345678   |    8.0000000    |   8.0000000
           1.2345679   |    7.0732967    |   7.0731968
          -1.2345678   |   -0.30103000   |  -0.30103000
          76.234567    |   -1.7835463    |   0.0070906729
           2.4691356   |    0.0          |   0.3010300
           0.0         |    0.0          |  -0.91514942.



    Due to the typical limits of the log function, the number of significant digits in the result is best considered to be three.

    Notice that 1.2345678=Y produces different values than 1.2345678=X

    A negative result indicates the two values being compared either do not agree in the first digit or they differ with respect to
    sign. An example of two numbers which do not agree in their leading digit (and actually differ in order of magnitude) is given
    above by X=76.234567 and Y=1.2345678; the accuracy reported is -1.7835463. An example of two numbers which do not agree in sign
    in X=-1.2345678 and Y=1.2345678; here the accuracy reported is -0.30103000.

EXAMPLE

    Example program:

       program demo_dp_addig ! fortran 90 example
       use M_math, only : dp_addig
       integer digi
       real vals(9)
       data vals/ &
         &1.234680,   1.2345378,  2.2234568, 1.2345678, &
         &1.2345679, -1.2345678, 76.234567,  2.4691356, &
         &0.0/
          write(*,*) ========================= 
          do i10=0,16
             a=1.0
             b=a+1.0/(10**i10)
             call dp_addig(a,b,8.0,acurcy,ind)
             write(*,*)i10,a,b,acurcy,ind
          enddo
          write(*,*) ========================= 
          digi=16
          do i20=0,digi
             a=1.0
             b=a+1.0/(10**i20)
             call dp_addig(a,b,dble(digi),acurcy,ind)
             write(*,*)i20,a,b,acurcy,ind
          enddo
          write(*,*) ========================= 
          do i30=1,9
             call dp_addig(1.2345678,vals(i30),8.0,acurcy1,ind1)
             call dp_addig(vals(i30),1.2345678,8.0,acurcy2,ind2)
             write(*,*)i30,vals(i30),acurcy1,acurcy2,ind1,ind2
          enddo
       end program demo_dp_addig



NOTES

REFERENCES

    based on ...

       NBS OMNITAB 1980 VERSION 6.01  1/ 1/81. dp_addig V 7.00  2/14/90. **
          David Hogben,
          Statistical Engineering Division,
          Center for Computing and Applied Mathematics,
          A337 Administration Building,
          National Institute of Standards and Technology,
          Gaithersburg, MD 20899
                         TELEPHONE 301-975-2845
              ORIGINAL VERSION -  October, 1969.
               CURRENT VERSION - February, 1990.
               JSU     VERSION - February, 1991.



DEPENDENCIES

    o M_journal(),log10(), abs(1)

FILES

    o libjust4.a

LEGAL RESTRICTIONS

    none

QA

    o Authors: David Hogben, John S. Urban

-----------------------------------------------------------------------------------------------------------------------------------

                                                           dp_addig (3)                                               July 02, 2017

Generated by manServer 1.08 from 0b7b6924-08d5-4b13-b668-5ee726f55d85 using man macros.
                                                            [dp_addig]
