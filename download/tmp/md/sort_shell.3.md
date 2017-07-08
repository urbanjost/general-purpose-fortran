[UP]

-----------------------------------------------------------------------------------------------------------------------------------
                                             Manual Reference Pages  - sort_shell (3)
-----------------------------------------------------------------------------------------------------------------------------------
                                                                 
NAME

    sort_shell(3f) - [M_sort]Generic subroutine sorts the array X using Shell s method

CONTENTS

    Synopsis
    Description
    Options
    Example
    Reference
    Author

SYNOPSIS

    Usage:


        real, integer data:
           sort_shell(X,ORDER= A|D )


        X          input/output numeric array
        order      Ascending (a-z) or Descending (z-a) sort order


        complex, complex(kind=kind(0.0d0)) data:
           sort_shell(X,order= A|D ,type= R|I|S )


        X          input/output complex array
        order      Ascending (a-z) or Descending (z-a) sort order
        type       Sort by Real component, Imaginary component, or Sqrt(R**2+I**2)


        character data:
           sort_shell(X,order= A|D ,[startcol=NN,endcol=MM])


        X          input/output character array
        order      Ascending (a-z) or Descending (z-a) sort order
        startcol   character position in strings which starts search field
        endcol     character position in strings which ends search field



DESCRIPTION

    subroutine sort_shell(3f) sorts an array over a specified field in numeric or alphanumeric order.

    From Wikipedia, the free encyclopedia:

    The step-by-step process of replacing pairs of items during the shell sorting algorithm. Shellsort, also known as Shell sort or
    Shell s method, is an in-place comparison sort. It can be seen as either a generalization of sorting by exchange (bubble sort)
    or sorting by insertion (insertion sort).[3] The method starts by sorting pairs of elements far apart from each other, then
    progressively reducing the gap between elements to be compared. Starting with far apart elements, it can move some out-of-place
    elements into position faster than a simple nearest neighbor exchange. Donald Shell published the first version of this sort in
    1959.[4][5] The running time of Shellsort is heavily dependent on the gap sequence it uses. For many practical variants,
    determining their time complexity remains an open problem.

    Shellsort is a generalization of insertion sort that allows the exchange of items that are far apart. The idea is to arrange
    the list of elements so that, starting anywhere, considering every hth element gives a sorted list. Such a list is said to be
    h-sorted. Equivalently, it can be thought of as h interleaved lists, each individually sorted.[6] Beginning with large values
    of h, this rearrangement allows elements to move long distances in the original list, reducing large amounts of disorder
    quickly, and leaving less work for smaller h-sort steps to do. If the file is then k-sorted for some smaller integer k, then
    the file remains h-sorted. Following this idea for a decreasing sequence of h values ending in 1 is guaranteed to leave a
    sorted list in the end.

F90 NOTES:

            o procedure names are declared private in this module so they are not accessible except by their generic name

            o procedures must include a "use M_sort" to access the generic name SORT_SHELL

            o if these routines are recompiled, routines with the use statement should then be recompiled and reloaded.

OPTIONS

             X is a vector or integer, real, complex, doubleprecision, character, or doubleprecision complex values to be sorted

             order sort order

             o    A for ascending

             o    D for descending (default)

             type Sort by Real component, Imaginary component, or Sqrt(R**2+I**2) Only applies to complex types.

             startcol character position in strings which starts sort field. Only applies to character values. Defaults to 1.
             Optional.

             endcol character position in strings which ends sort field Only applies to character values. Defaults to end of
             string. Optional.

EXAMPLE

    Sample program

          program demo_sort_shell
          use M_sort, only : sort_shell
          character(len=:),allocatable :: array(:)


          array= [ character(len=20) ::                               &
          &  red ,     green ,  blue ,  yellow ,  orange ,    black , &
          &  white ,   brown ,  gray ,  cyan ,    magenta ,           &
          &  purple ]


          write(*, (a,*(a:,",")) ) BEFORE  ,(trim(array(i)),i=1,size(array))
          call sort_shell(array,order= a )
          write(*, (a,*(a:,",")) ) A-Z     ,(trim(array(i)),i=1,size(array))
          do i=1,size(array)-1
             if(array(i).gt.array(i+1))then
                write(*,*) Error in sorting strings a-z 
             endif
          enddo


          array= [ character(len=20) ::                               &
          &  RED ,     GREEN ,  BLUE ,  YELLOW ,  ORANGE ,    BLACK , &
          &  WHITE ,   BROWN ,  GRAY ,  CYAN ,    MAGENTA ,           &
          &  PURPLE ]


          write(*, (a,*(a:,",")) ) BEFORE  ,(trim(array(i)),i=1,size(array))
          call sort_shell(array,order= d )
          write(*, (a,*(a:,",")) ) Z-A     ,(trim(array(i)),i=1,size(array))
          do i=1,size(array)-1
             if(array(i).lt.array(i+1))then
                write(*,*) Error in sorting strings z-a 
             endif
          enddo


          end program demo_sort_shell



    Expected output

          BEFORE red,green,blue,yellow,orange,black,white,brown,gray,cyan,magenta,purple
          A-Z    black,blue,brown,cyan,gray,green,magenta,orange,purple,red,white,yellow
          BEFORE RED,GREEN,BLUE,YELLOW,ORANGE,BLACK,WHITE,BROWN,GRAY,CYAN,MAGENTA,PURPLE
          Z-A    YELLOW,WHITE,RED,PURPLE,ORANGE,MAGENTA,GREEN,GRAY,CYAN,BROWN,BLUE,BLACK



REFERENCE

          1. ALGORITHM 201, SHELLSORT, J. BOOTHROYD, CACM VOL. 6, NO. 8, P 445, (1963)

          2. D. L. SHELL, CACM, VOL. 2, P. 30, (1959)

AUTHOR

    John S. Urban, 19970201

-----------------------------------------------------------------------------------------------------------------------------------

                                                          sort_shell (3)                                              July 02, 2017

Generated by manServer 1.08 from db2f607b-c6cb-4fe6-b2c6-1dd36fc463aa using man macros.
                                                           [sort_shell]
