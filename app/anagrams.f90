!>
!!##NAME
!!    anagrams(1f) - print anagrams of strings
!!##SYNOPSIS
!!
!!    anagrams string(s)
!!##DESCRIPTION
!!    Generation in lexicographic order
!!
!!    There are many ways to systematically generate all permutations of
!!    a given sequence. One classic, simple, and flexible algorithm is
!!    based upon finding the next permutation in lexicographic ordering, if it
!!    exists. It can handle repeated values, for which case it generates each
!!    distinct multiset permutation once. Even for ordinary permutations it is
!!    significantly more efficient than generating values for the Lehmer code
!!    in lexicographic order (possibly using the factorial number system) and
!!    converting those to permutations. It begins by sorting the sequence in
!!    (weakly) increasing order (which gives its lexicographically minimal
!!    permutation), and then repeats advancing to the next permutation as
!!    long as one is found. The method goes back to Narayana Pandita in 14th
!!    century India, and has been rediscovered frequently.
!!
!!    The following algorithm generates the next permutation lexicographically
!!    after a given permutation. It changes the given permutation in-place.
!!
!!     * Find the largest index k such that a[k] < a[k + 1]. If no such index
!!       exists, the permutation is the last permutation.
!!     * Find the largest index l greater than k such that a[k] < a[l].
!!     * Swap the value of a[k] with that of a[l].
!!     * Reverse the sequence from a[k + 1] up to and including the final
!!       element a[n].
!!
!!    For example, given the sequence [10, 20, 30, 40] (which is in increasing
!!    order), and given that the index is one-based, the steps are as follows:
!!
!!    1) Index k = 3, because 30 is placed at an index that satisfies condition
!!       of being the largest index that is still less than a[k + 1] which is 40.
!!       if no such index exists no more permutations exist
!!
!!    2) Index l = 4, because 40 is the only value in the sequence that is greater
!!       than 30 in order to satisfy the condition a[k] < a[l].
!!
!!    3) The values of a[3] and a[4] are swapped to form the new sequence
!!       [10,20,40,30].
!!
!!    4) The sequence after k-index a[3] to the final element is reversed. Because
!!       only one value lies after this index (the 30), the sequence remains
!!       unchanged in this instance. Thus the lexicographic successor of the
!!       initial state is permuted: [10,20,40,30].
!!
!!    5) Following this algorithm, the next lexicographic permutation will be
!!       [10,30,20,40], and the 24th permutation will be [40,30,20,10] at which point a[k]
!!       < a[k + 1] does not exist, indicating that this is the last permutation.
!!
!!    This method uses about 3 comparisons and 1.5 swaps per permutation,
!!    amortized over the whole sequence, not counting the initial sort.
!!##REFERENCES
!!    Wikipedia
!!##EXAMPLES
!!
!!    anagrams hello world
program testit
use M_sort, only : sort_shell
implicit none
integer                      :: argument_length
integer,allocatable          :: array(:)
character(len=:),allocatable :: string
integer                      :: i,j
logical                      :: done
   do i=1, command_argument_count() ! get number of arguments
      call get_command_argument(number=i,length=argument_length)
      if(allocated(string))deallocate(string)
      allocate(character(len=argument_length) :: string)
      call get_command_argument(i, string)
      array=[(j,j=1,len(string))]
      do
         write(*,'(*(a))')(string(array(j):array(j)),j=1,len(string))
         call permutation(array,done)
         if(done)exit
      enddo
   enddo
contains
subroutine permutation(values,done)
! suitable for this basic use but would be much faster if redundant calculations were removed
use M_sort, only : swap
implicit none
integer             :: values(:)
logical,intent(out) :: done
integer             :: sign_of_diff(size(values)-1)
integer             :: i,k,l,n
   done=.false.
   n=size(array) ! assuming sorted from smallest to largest with no duplicates
   ! Find the largest index k such that a[k] < a[k + 1]. If no such index exists, the permutation is the last permutation.
   ! so find the delta between values(k+1) and values(k) and return 1 if positive and 0 if negative
   sign_of_diff=[(sign(1,values(i+1)-values(i)),i=1,n-1)]
   k=findlast(sign_of_diff, value=1) ! Location of the last element of ARRAY along dimension DIM having value of 1
   if(k.ne.0)then
      ! Find the largest index l greater than k such that a[k] < a[l].
      l=findlast([(sign(1,values(i)-values(k)),i=k+1,n)],value=1)+k
      ! Swap the value of a[k] with that of a[l].
      call swap(values(k),values(l))
      ! Reverse the sequence from a[k + 1] up to and including the final element a[n].
      values(k+1:n)= values(n:k+1:-1)
   else
      done=.true.
   endif
end subroutine permutation
function findlast(array,value)
integer,intent(in) :: array(:)
integer,intent(in) :: value
integer            :: findlast
integer            :: i
! findloc(array,value,back=.true.)
   findlast=0
   do i=size(array),1,-1
     if(array(i).eq.value)then
        findlast=i
        exit
     endif
   enddo
end function findlast
end program testit


