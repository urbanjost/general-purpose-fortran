[CLI Home Page]

NAME
    M_calculator_plus(3fm) - [M_calculator_plus] convenience routines for calling the M_calculator(3fm) module
SYNOPSIS
    Public entities:

        use M_calculator_plus, only : inum0, rnum0, dnum0, snum0, jucalcx, strgarr, strgar2

    DESCRIPTION

    The M_calculator_plus(3f) module is a collection of convenience routines that simplify calling the expression parser module
    M_calculator.

       inum0     resolve a calculator string into a whole integer number
       rnum0     resolve a calculator string into a real number
       dnum0     resolve a calculator string into a doubleprecision number
       snum0     resolve a calculator expression into a string
       jucalcx   call jucalc() calculator and display messages
       strgarr   read a string of numeric expressions into an array USING CALCULATOR
       strgar2   read a string of numeric or string expressions into an array USING CALCULATOR

    EXAMPLE

    Sample program


        program demo_m_calculator_plus
        use M_calculator_plus, only : rnum0
        implicit none
        real :: rval
        character(len=80) :: string
        string='A=sind(30)'
        rval=rnum0(string,ierr)
        if(ierr.eq.0)then
           write(*,*) rval
        else
           write(*,*) 'error evaluating '//trim(string)
        endif
        rval=rnum0('A',ierr)
        write(*,*) rval
        end program demo_m_calculator_plus

