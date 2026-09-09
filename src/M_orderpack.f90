module M_orderpack
use M_orderpack__refsor, only : sort=>refsor         ! [SORT] Sorts array (Quick-sort)
use M_orderpack__inssor, only : sort_special=>inssor ! [SORT] Sorts array (Insertion sort, generally for small or nearly sorted
                                                    !        arrays)
use M_orderpack__inspar, only : psort=>inspar        ! [SORT:PARTIAL] partially sorts an array

use M_orderpack__mrgrnk, only : rank=>mrgrnk             ! [RANK] ranks array (optimized merge-sort)
use M_orderpack__mrgref, only : rank_basic=>mrgref       ! [RANK] ranks array (basic merge-sort)
use M_orderpack__uniinv, only : rank_decreasing=>uniinv  ! [RANK:UNIQUE] inverse ranks an array,
use M_orderpack__unirnk, only : rank_unique=>unirnk      ! [RANK:UNIQUE] ranks an array, with removal of duplicate entries.(MergSort)
                                                        ! with duplicate entries assigned the same rank.(MergSort-like)

use M_orderpack__rnkpar, only : prank=>rnkpar            ! [RANK:PARTIAL] partially ranks array (Optimized Quick-Sort)
use M_orderpack__refpar, only : prank_basic=>refpar      ! [RANK:PARTIAL] partially ranks array (Quick-Sort)
use M_orderpack__rinpar, only : prank_special=>rinpar    ! [RANK:PARTIAL] partially ranks array
                                                        ! PRANK_DECREASING_UNIQUE
use M_orderpack__rapknr, only : prank_decreasing=>rapknr ! [RANK:PARTIAL] partially rank array in DECREASING order and equal ranks
                                                        ! for a value

use M_orderpack__unipar, only : prank_unique=>unipar ! [RANK:PARTIAL:UNIQUE] partially rank an array removing duplicates

use M_orderpack__median, only : median               ! [MEDIAN] Calculate median value. If number of elements is even, return average
                                                    !          of the central values
use M_orderpack__valmed, only : medianval=>valmed    ! [MEDIAN] Find VALUE of median element.
use M_orderpack__indmed, only : medianloc=>indmed    ! [MEDIAN] Find INDEX of median element.

use M_orderpack__indnth, only : orderloc=>indnth     ! [FRACTILE] Return INDEX of Nth ordered element ,
                                                    !            i.e fractile of order N/SIZE(array) (Quick-Sort-like)
use M_orderpack__valnth, only : orderval=>valnth     ! [FRACTILE] Return VALUE of Nth element of array,
                                                    !            i.e fractile of order N/SIZE(array) (Quick-Sort-like)
use M_orderpack__fndnth, only : orderval_special=>fndnth ! [FRACTILE] Return VALUE of Nth ordered elements ,
                                                    !            i.e. fractile of order N/SIZE(array) (Insert-like)

use M_orderpack__unista, only : unique=>unista       ! [UNIQUE] Removes duplicates from an array otherwise retaining original order
use M_orderpack__mulcnt, only : occurrences=>mulcnt  ! [MULTIPLICITY] gives number of times that each value appears in the input
use M_orderpack__ctrper, only : perturb=>ctrper      ! [PERMUTATION] perturbs an array leaving elements close to initial locations

! BUG: without explicitly adding this section ifort(1) fails
private
public sort, sort_special
public psort
public rank, rank_basic, rank_decreasing, rank_unique
public prank, prank_basic, prank_special, prank_decreasing, prank_unique
public median, medianval, medianloc
public orderloc, orderval, orderval_special
public unique
public occurrences
public perturb
!>
!!##NAME
!!    M_orderpack(3f) - [M_orderpack::INTRO]General and Specialized Ranking
!!                      and Sorting Routines
!!##SYNOPSIS
!!
!!    Procedure names and syntax:
!!
!!     use M_orderpack, only : &
!!      Sort,             & ! Subroutine Sort             (INOUTVALS)
!!      Sort_Special,     & ! Subroutine Sort_Special     (INOUTVALS)
!!      Psort,            & ! Subroutine Psort            (INOUTVALS, nord)
!!
!!      Rank,             & ! Subroutine Rank             (INVALS, imult)
!!      Rank_Basic,       & ! Subroutine Rank_Basic       (INVALS, irngt)
!!      Rank_Decreasing,  & ! Subroutine Rank_Decreasing  (INVALS, igoest)
!!      Rank_Unique,      & ! Subroutine Rank_Unique      (INVALS, irngt, nuni)
!!
!!      Prank,            & ! Subroutine Prank            (INVALS, irngt, nord)
!!      Prank_Basic,      & ! Subroutine Prank_Basic      (INVALS, irngt, nord)
!!      Prank_Decreasing, & ! Subroutine Prank_Decreasing (INVALS, irngt, nord)
!!      Prank_Special,    & ! Subroutine Prank_Special,   (INVALS, irngt, nord)
!!      Prank_Unique,     & ! Subroutine Prank_Unique     (INVALS, irngt, nord)
!!
!!      Median,           & ! Function Median             (INVALS)
!!      MedianVal,        & ! Function MedianVal          (INVALS)
!!      MedianLoc,        & ! Subroutine MedianLoc        (INVALS, indm)
!!
!!      Orderval,         & ! Function OrderVal           (INVALS, nord)
!!      OrderLoc,         & ! Integer Function OrderLoc   (INVALS, nord)
!!      Orderval_Special, & ! Function OrderVal_Special   (INVALS, nord)
!!
!!      Occurrences,      & ! Subroutine Occurrences      (INVALS, imult)
!!      Unique,           & ! Subroutine Unique           (INOUTVALS, nuni)
!!      Perturb             ! Subroutine Perturb          (INOUTVALS, CLOSENESS)
!!
!!    The procedures may be accessed via their original names in ORDERPACK2.0
!!    as well, one per module:
!!
!!     ! previous ORDERPACK2.0 name ! ORDERPACK 2.1 name
!!     use M_orderpack__refsor, only : refsor  ! Sort
!!     use M_orderpack__inssor, only : inssor  ! Sort_special
!!     use M_orderpack__inspar, only : inspar  ! psort
!!     use M_orderpack__mrgrnk, only : mrgrnk  ! rank
!!     use M_orderpack__mrgref, only : mrgref  ! rank_basic
!!     use M_orderpack__uniinv, only : uniinv  ! rank_decreasing
!!     use M_orderpack__unirnk, only : unirnk  ! rank_unique
!!     use M_orderpack__rnkpar, only : rnkpar  ! prank
!!     use M_orderpack__refpar, only : refpar  ! prank_basic
!!     use M_orderpack__rapknr, only : rapknr  ! prank_decreasing
!!     use M_orderpack__rinpar, only : rinpar  ! prank_special
!!     use M_orderpack__unipar, only : unipar  ! prank_unique
!!     use M_orderpack__median, only : median  ! median
!!     use M_orderpack__valmed, only : valmed  ! medianval
!!     use M_orderpack__indmed, only : indmed  ! medianloc
!!     use M_orderpack__valnth, only : valnth  ! orderval
!!     use M_orderpack__indnth, only : indnth  ! orderloc
!!     use M_orderpack__fndnth, only : fndnth  ! orderval_special
!!     use M_orderpack__mulcnt, only : mulcnt  ! occurrences
!!     use M_orderpack__unista, only : unista  ! unique
!!     use M_orderpack__ctrper, only : ctrper  ! perturb
!!
!!##DESCRIPTION
!!    M_ORDERPACK 2.1 - Unconditional, Unique and Partial Ranking, Sorting,
!!                    and Permutation
!!
!!    M_ORDERPACK 2.1 performs both conventional sorting and ranking as well as
!!    the rarer specialized ordering tasks such as partial sorting, partial
!!    ranking, unique sorting, unique ranking, inverse unique ranking, and
!!    more. These partial sort and ranking routines can greatly accelerate
!!    many computations when users need only the M largest or smallest
!!    elements out of a N-element vector.
!!
!!    All the specialized procedures have a range over which they far
!!    outperform a basic sort, and most have a range where they dramatically
!!    underperform. If you are not limited by memory requirements or have no
!!    issues with runtimes the simplest solution may be just to use SORT(3f)
!!    and RANK(3f).
!!
!!    Otherwise, your solution method may very well depend on the size of
!!    the input arrays, whether the data is already close to the required
!!    order, or how costly it is to create work arrays or an index array.
!!
!!    So, if you want the smallest value in an array call the intrinsic
!!    MINVAL(3f), not ORDERVAL(3f).
!!
!!##SORTING
!!     FULL SORTING
!!        Sort          Sorts array into ascending order (Quick-sort)
!!        Sort_Special  Sorts array into ascending order (Insertion sort,
!!                      generally for small or nearly sorted arrays)
!!     PARTIAL SORTING
!!        Psort             partially sorts an array
!!        Orderval          Return VALUE of Nth lowest value of array
!!                          (Quick-Sort)
!!        Orderval_Special  Return Nth lowest value of an array
!!                          (Insert-sort, generally for small or nearly
!!                          sorted arrays))
!!        MedianVal         finds the median of an array
!!        Median            Return median value of array. If number of elements
!!                          is even, return average of the two "medians"
!!##RANKING
!!     UNCONDITIONAL RANKING
!!        Rank        ranks array (optimized merge-sort)
!!        Rank_Basic  ranks array (basic merge-sort)
!!     PARTIAL RANKING
!!        Prank             partially ranks array (Optimized Quick-Sort)
!!        Prank_Basic       partially ranks array
!!        Prank_Decreasing  partially ranks array in DECREASING order
!!        Prank_Special     partially ranks array (Basic Insert-Sort)
!!        Orderloc          Return INDEX of Nth value of array (Quick-Sort-like)
!!        MedianLoc         Returns INDEX of median value of an array.
!!     UNIQUE RANKING
!!        Rank_Unique       performs a Merge-Sort ranking of an array,
!!                          with removal of duplicate entries.
!!        Rank_Decreasing   an inverse ranking of an array,
!!                          with duplicate entries assigned the same rank.
!!        Prank_Unique      partially rank an array removing duplicates
!!##UNIQUE
!!        Unique        Removes duplicates from an array
!!                      otherwise retaining original order
!!##MULTIPLICITY
!!        Occurrences   Give the multiplicity for each array value
!!##PERMUTATION
!!        Perturb  a random permutation of an array, optionally leaving
!!                 elements close to initial locations
!!
!!##RATIONALE
!!
!!    While Fortran 90 and later variants have made life much easier for
!!    scientific programmers than Fortran 77, the language still lacks
!!    depth in public domain utilities. The following package, M_ORDERPACK
!!    2.1, provides important but uncommon routines needed to complete the
!!    Fortran programming environment.
!!
!!##INTRODUCTION
!!
!!    The existing fortran code base provides many conventional ranking
!!    or sorting routines, but very few specialized ranking or sorting
!!    routines. Specifically, we know of no other Fortran code which sorts
!!    or ranks only a small proportion of an array (partial ordering). Such
!!    partial ranking routines have applications in statistics for rapidly
!!    computing extreme order statistics, finding nearest neighbors, and
!!    other clustering operations. In addition, many applications need to
!!    work with only the unique values in an array (unique ordering). Such
!!    unique ranking routines allow users to isolate individual cases out
!!    of a mass of discrete data. Many times the frequency of the unique
!!    values proves interesting (e.g., empirical distributions).
!!
!!    M_ORDERPACK handles all of these ordering needs.
!!
!!    Also, M_ORDERPACK contains a partial unique ranking routine. Such a
!!    routine would prove useful in finding a limited number of unique
!!    values in an array.
!!
!!    Inversion of orderings becomes difficult when duplicates exist (not
!!    a one-to-one relation). The M_ORDERPACK inverse ranking routine handles
!!    this difficult case.
!!
!!    As an added bonus M_ORDERPACK provides an unusual routine which allows
!!    user controllable partial random permutation of arrays.
!!
!!    M_ORDERPACK of course contains conventional or unconditional sorting
!!    routines as well.
!!
!!    Finally, many Fortran sorting or ranking routines do not take advantage
!!    of available memory and cache to maximize performance. The routines
!!    in M_ORDERPACK have been designed to take advantage of modern machines.
!!
!!##RANKING VERSUS SORTING
!!
!!    Ranking consists in finding, for each element of a set, its order
!!    (rank) in the sorted set, without effectively changing the initial
!!    order (or disorder! ) of the set. In many instances, it better suits
!!    the actual need of the user than sorting, as the ranks can then be
!!    used to order other related sets or components of a user type.
!!
!!    Ranking is especially needed when the sizes of the elements are large,
!!    and therefore moving them around is resource-consuming.
!!
!!##RANKING
!!
!!    In some instances, one is not actually interested in modifying the
!!    order of the elements in a set, but only in knowing how to access them
!!    in increasing -- or decreasing -- order. Ranking, as it is called,
!!    provides the index array I(:) such as the set S(I(:)) is ordered. One
!!    of the advantages of carrying out ranking rather than sorting is that
!!    the index array can be computed without the performance penalty of
!!    moving the elements around when they are of large sizes. A similar
!!    point is that the index array can be used to index other data.
!!
!!##OPTIMIZATION CHOICES
!!
!!    We tried to take into account the recent trends in computing to make
!!    our compromise choices. Of course, no two problems are the same, and
!!    for some of them the following decisions may happen to be wrong. We
!!    just hope that for most cases, they will be right.
!!
!!      * Make extensive use of work arrays: Memory can be extended,
!!        time cannot.
!!      * Try to reduce the number of operations in the inner loops, even
!!        if it increases code size.
!!      * Assume that cache size is relatively small, and try to maximize
!!        cache hits.
!!
!!##INTERFACE
!!
!!    Robust routines make their interface known to the calling
!!    program. There are three main ways to implement this in Fortran:
!!
!!      * Explicit interfaces, either included in the body of the calling
!!        routine, or gathered in an 'interface module'. An example of
!!        including an interface block in the calling program can be found
!!        in the sample program sort7.f90.
!!      * Embedding the routine of interest as a "contained routine" into
!!        the calling procedure. An example of such way can be found in
!!        the follow.f90 program, that rebuilds a curve from a set of X,
!!        Y coordinates.
!!      * Embedding the routine of interest into a MODULE, and USEing that
!!        module in the procedure that calls the routine. This creates
!!        order dependencies when compiling code, generally resulting in
!!        requiring such tools as Makefiles but has many other benefits,
!!        such as most easily allowing for generic versions of the routines,
!!        This is the way we used here. An example of use is provided as
!!        the test program tstvalnth.f90.
!!
!!##A WORD OF APOLOGY
!!
!!    When one looks at the description of a sorting algorithm, the
!!    process seems pretty simple, and can usually be held in 10 to 20
!!    lines of pseudo-code. But if one wants an optimized program, one
!!    takes this simple implementation, and looks for redundant operations,
!!    investigates runs with sample data sets with a profiling tool, and
!!    is led to duplicate code with slight modifications rather than use
!!    tests in inner loops, to process differently the first and the last
!!    iterations, or to take into account some special cases that are only
!!    special in that they can be done faster.
!!
!!    In the end, the number of lines of source code may be
!!    multiplied tenfold, and the readability decreased in a similar
!!    proportion. Unfortunately, this is the price to pay for speed of
!!    execution. It was that way when I started programming more than 20
!!    years ago, and I have forsaken any hope that it might become otherwise
!!    before I return to dust. So please accept my apologies that this code
!!    is often complex and difficult to read.
!!
!!##AUTHORS
!!    Michel Olagnon IFREMER Brest / Michel.Olagnon@ifremer.fr
!!
!!    2000- 2013/11/06
!!##MAINTAINERS
!!    John S. Urban, 2022-04-16
!!
!!##LICENSE
!!    CC0-1.0
end module M_orderpack
