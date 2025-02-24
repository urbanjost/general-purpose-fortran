program main
use, intrinsic :: iso_fortran_env, only : stderr=>ERROR_UNIT
use M_sets, only: unique, intersect, union, setdiff, ismember, setxor, issorted
use M_CLI2, only: set_args, igets, sget, lget, sgets, rgets, dgets, unnamed
implicit none
character(len=*),parameter   :: g='(*(g0,1x))'
integer,allocatable          :: A(:)
integer,allocatable          :: B(:)
character(len=:),allocatable :: version_text(:), help_text(:)
character(len=:),allocatable :: setorder
character(len=:),allocatable :: datatype
character(len=:),allocatable :: strA(:),strB(:)
real,allocatable             :: fltA(:),fltB(:)
doubleprecision,allocatable  :: dblA(:),dblB(:)
logical :: verbose
   call setup()
   call set_args('--seta:a , --setb:b , --setorder:o "sorted" --type:t "character"',help_text,version_text)
   datatype=sget('type')
   do
      select case(datatype)

      case('integer')
      a=igets('a')
      b=igets('b')
      setorder=sget('setorder')
      verbose=lget('verbose')
      if(size(a) == 0.and.size(b) == 0)then
         write(*,g)repeat('TEST OF SETTHEORY ',4)
         a = [7,23,14,15,9,12,8,24,35]
         b = [ 2,5,7,8,14,16,25,35,27]
         verbose=.true.
      endif
      if(verbose)write(*,g) '-------------: ', 'Given the sets'
      write(*,g) 'A            : ', a
      write(*,g) 'B            : ', b
      if(verbose)write(*,g) '-------------: ', 'Find the unique elements of set A, then of B'
      write(*,g) 'UNIQUE A     : ', unique(a,setorder=setorder)
      write(*,g) 'UNIQUE B     : ', unique(b,setorder=setorder)
      if(verbose)write(*,g) '-------------: ', 'Find all the unique values in sets A and B combined.'
      write(*,g) 'UNION        : ', union(a, b, setorder=setorder)
      if(verbose)write(*,g) '-------------: ', 'Find the values common to both A and B.'
      write(*,g) 'INTERSECT    : ', intersect(a, b, setorder=setorder)
      if(verbose)write(*,g) '-------------: ', 'Find the values in A that are not in B.'
      write(*,g) 'SETDIFF      : ', setdiff(a, b, setorder=setorder)
      if(verbose)write(*,g) '-------------: ', 'Find positions of values in A that are also in B (1=matched).'
      write(*,g) 'ISMEMBER     : ', ismember(a, b)
      if(verbose)write(*,g) '-------------: ', 'Find values in A or B but not both'
      write(*,g) 'SETXOR       : ', setxor(a, b, setorder=setorder)

      case('real')
      flta=rgets('a')
      fltb=rgets('b')
      setorder=sget('setorder')
      verbose=lget('verbose')
      if(size(flta) == 0.and.size(fltb) == 0)then
         datatype='integer'
         cycle
      endif
      if(verbose)write(*,g) '-------------: ', 'Given the sets'
      write(*,g) 'A            : ', flta
      write(*,g) 'B            : ', fltb
      if(verbose)write(*,g) '-------------: ', 'Find the unique elements of set A, then of B'
      write(*,g) 'UNIQUE A     : ', unique(flta,setorder=setorder)
      write(*,g) 'UNIQUE B     : ', unique(fltb,setorder=setorder)
      if(verbose)write(*,g) '-------------: ', 'Find all the unique values in sets A and B combined.'
      write(*,g) 'UNION        : ', union(flta, fltb, setorder=setorder)
      if(verbose)write(*,g) '-------------: ', 'Find the values common to both A and B.'
      write(*,g) 'INTERSECT    : ', intersect(flta, fltb, setorder=setorder)
      if(verbose)write(*,g) '-------------: ', 'Find the values in A that are not in B.'
      write(*,g) 'SETDIFF      : ', setdiff(flta, fltb, setorder=setorder)
      if(verbose)write(*,g) '-------------: ', 'Find positions of values in A that are also in B (1=matched).'
      write(*,g) 'ISMEMBER     : ', ismember(flta, fltb)
      if(verbose)write(*,g) '-------------: ', 'Find values in A or B but not both'
      write(*,g) 'SETXOR       : ', setxor(flta, fltb, setorder=setorder)

      case('double','doubleprecision')
      dbla=dgets('a')
      dblb=dgets('b')
      setorder=sget('setorder')
      verbose=lget('verbose')
      if(size(dbla) == 0.and.size(dblb) == 0)then
         datatype='integer'
         cycle
      endif
      if(verbose)write(*,g) '-------------: ', 'Given the sets'
      write(*,g) 'A            : ', dbla
      write(*,g) 'B            : ', dblb
      if(verbose)write(*,g) '-------------: ', 'Find the unique elements of set A, then of B'
      write(*,g) 'UNIQUE A     : ', unique(dbla,setorder=setorder)
      write(*,g) 'UNIQUE B     : ', unique(dblb,setorder=setorder)
      if(verbose)write(*,g) '-------------: ', 'Find all the unique values in sets A and B combined.'
      write(*,g) 'UNION        : ', union(dbla, dblb, setorder=setorder)
      if(verbose)write(*,g) '-------------: ', 'Find the values common to both A and B.'
      write(*,g) 'INTERSECT    : ', intersect(dbla, dblb, setorder=setorder)
      if(verbose)write(*,g) '-------------: ', 'Find the values in A that are not in B.'
      write(*,g) 'SETDIFF      : ', setdiff(dbla, dblb, setorder=setorder)
      if(verbose)write(*,g) '-------------: ', 'Determine which elements of A are also in B by position.'
      write(*,g) 'ISMEMBER     : ', ismember(dbla, dblb)
      if(verbose)write(*,g) '-------------: ', 'Find values of A and B not in their intersection.'
      write(*,g) 'SETXOR       : ', setxor(dbla, dblb, setorder=setorder)

      case('character')
      stra=sgets('a')
      strb=sgets('b')
      setorder=sget('setorder')
      verbose=lget('verbose')
      if(size(stra) == 0.and.size(strb) == 0)then
         datatype='integer'
         cycle
      endif
      if(verbose)write(*,g) '-------------: ', 'Given the sets'
      write(*,g) 'A            : ', stra
      write(*,g) 'B            : ', strb
      if(verbose)write(*,g) '-------------: ', 'Find the unique elements of set A, then of B'
      write(*,g) 'UNIQUE A     : ', unique(stra,setorder=setorder)
      write(*,g) 'UNIQUE B     : ', unique(strb,setorder=setorder)
      if(verbose)write(*,g) '-------------: ', 'Find all the unique values in sets A and B combined.'
      write(*,g) 'UNION        : ', union(stra, strb, setorder=setorder)
      if(verbose)write(*,g) '-------------: ', 'Find the values common to both A and B.'
      write(*,g) 'INTERSECT    : ', intersect(stra, strb, setorder=setorder)
      if(verbose)write(*,g) '-------------: ', 'Find the values in A that are not in B.'
      write(*,g) 'SETDIFF      : ', setdiff(stra, strb, setorder=setorder)
      if(verbose)write(*,g) '-------------: ', 'Determine which elements of A are also in B by position.'
      write(*,g) 'ISMEMBER     : ', ismember(stra, strb)
      if(verbose)write(*,g) '-------------: ', 'Find values of A and B not in their intersection.'
      write(*,g) 'SETXOR       : ', setxor(stra, strb, setorder=setorder)
      case default
              write(*,g)'<ERROR> type=',datatype,' allowed values are integer,character,real,double'
      end select
      exit
   enddo
   if(size(unnamed)>0)then
      write(stderr,g)'<WARNING> *settheory* unused values : ',unnamed,';are the lists of values quoted?'
   endif
contains
subroutine setup()
help_text=[ CHARACTER(LEN=128) :: &
'NAME',&
'   settheory(1f) - [M_sets:SETTHEORY] combine two sets of values',&
'   and display union, intersection, values found in both sets,  ',&
'   values found only in one set, ...                            ',&
'   (LICENSE:MIT)                                                ',&
'                                                                ',&
'SYNOPSIS                                                        ',&
'    settheory -a SET_ONE -b SET_TWO [--type DATATYPE]           ',&
'    [--setorder ORDERTYPE] [--verbose]                          ',&
'                                                                ',&
'DESCRIPTION                                                     ',&
'   settheory(1f) finds the union, intersection, and set differences of',&
'   two small sets of numbers or strings and displays them             ',&
'                                                                      ',&
'   If both sets are empty a simple example is run.                    ',&
'                                                                      ',&
'OPTIONS                                                               ',&
'    --seta,-a SET_ONE   vector of numbers or strings comprising       ',&
'                        set A. May be delimited by commas, spaces,    ',&
'                        or colons. If spaces are used the set needs   ',&
'                        quoted.                                       ',&
'    --setb,-b SET_TWO   vector of numbers or strings comprising       ',&
'                        set B. May be delimited by commas, spaces,    ',&
'                        or colons. If spaces are used the set needs   ',&
'                        quoted.                                       ',&
'    --type,-t DATATYPE       May be "integer", "character", "real" or ',&
'                             "double". Defaults to "character".       ',&
'    --setorder,-o ORDERTYPE  "sorted" or "stable". If "stable" the values',&
'                             remain in the order input.                  ',&
'    --verbose,-V             add additional descriptive text             ',&
'                                                                         ',&
'    --usage,-u            show table of options and their current values ',&
'    --version,-v          Print version information on standard output   ',&
'                          then exit successfully.                        ',&
'    --help,-h             Print usage information on standard output     ',&
'                          then exit successfully.                        ',&
'RESULTS                                                                  ',&
'                                                                         ',&
'  Outputs the results from the following calls to the M_set(3f) module   ',&
'                                                                         ',&
'   * unique(A,setOrder);unique(B,setOrder) - Unique values in each array ',&
'   * union(A,B,setOrder) - Set union of two arrays                       ',&
'   * intersect(A,B,setOrder) - Set intersection of two arrays            ',&
'   * setdiff(A,B,setOrder) - Set difference of two arrays                ',&
'   * ismember(A,B) - Array elements of set B that are members            ',&
'                     of set A array                                      ',&
'   * setxor(A,B,setOrder) - Set exclusive OR of two arrays               ',&
'EXAMPLES                                                                 ',&
'   Sample commands                                                       ',&
'                                                                         ',&
'    settheory -a one,two,three -b four,two,five,three                    ',&
'    A            :  one   two   three                                    ',&
'    B            :  four  two   five  three                              ',&
'    UNIQUE A     :  one   three two                                      ',&
'    UNIQUE B     :  five  four  three two                                ',&
'    UNION        :  five  four  one   three two                          ',&
'    INTERSECT    :  three two                                            ',&
'    SETDIFF      :  one                                                  ',&
'    ISMEMBER     :  0 1 1                                                ',&
'    SETXOR       :  five  four  one                                      ',&
'                                                                         ',&
'    settheory -a 7,23,14,15,9,12,8,24,35 -b 2,5,7,8,14,16,25,35,27 \     ',&
'              --type integer                                             ',&
'    A            :  7 23 14 15 9 12 8 24 35                              ',&
'    B            :  2 5 7 8 14 16 25 35 27                               ',&
'    UNIQUE A     :  7 8 9 12 14 15 23 24 35                              ',&
'    UNIQUE B     :  2 5 7 8 14 16 25 27 35                               ',&
'    UNION        :  2 5 7 8 9 12 14 15 16 23 24 25 27 35                 ',&
'    INTERSECT    :  7 8 14 35                                            ',&
'    SETDIFF      :  9 12 15 23 24                                        ',&
'    ISMEMBER     :  1 0 1 0 0 0 1 0 1                                    ',&
'    SETXOR       :  2 5 9 12 15 16 23 24 25 27                           ',&
'    # or                                                                 ',&
'    settheory --type integer \                                           ',&
'    -a ''7 23 14 15 9 12 8 24 35'' \                                     ',&
'    -b ''2 5 7 8 14 16 25 35 27''                                        ',&
'    # or                                                                 ',&
'    settheory --type integer \                                           ',&
'    -a 7:23:14:15:9:12:8:24:35 \                                         ',&
'    -b 2:5:7:8:14:16:25:35:27                                            ',&
'                                                                         ',&
'SEE ALSO                                                                 ',&
'    diff(1),uniq(1),sort(1),comm(1),join(1)                              ',&
'']
version_text=[ CHARACTER(LEN=128) :: &
'PRODUCT:        GPF (General Purpose Fortran) utilities and examples',&
'PROGRAM:        settheory(1f)                                       ',&
'DESCRIPTION:    given two small vectors of intrinsic type find union, intersection, and set differences',&
'VERSION:        1.0, 2024-10-04                                                                        ',&
'AUTHOR:         John S. Urban                                                                          ',&
'LICENSE:        MIT                                                                                    ',&
'']
end subroutine setup
end program main
