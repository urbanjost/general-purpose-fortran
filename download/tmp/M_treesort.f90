module m_treesort  ! @(#) simple tree sort of integers as they are added to the sort
   TYPE NODE
      INTEGER :: VALUE
      TYPE (NODE), POINTER :: LEFT, RIGHT
   END TYPE NODE
contains
 
   RECURSIVE SUBROUTINE INSERT (T, NUMBER)
 
      IMPLICIT NONE
      TYPE (NODE), POINTER :: T  ! A tree
      INTEGER, INTENT (IN) :: NUMBER
 
      ! If (sub)tree is empty, put number at root
      IF (.NOT. ASSOCIATED (T)) THEN
         ALLOCATE (T)
         T % VALUE = NUMBER
         NULLIFY (T % LEFT)
         NULLIFY (T % RIGHT)
      ! Otherwise, insert into correct subtree
      ELSE IF (NUMBER < T % VALUE) THEN
         CALL INSERT (T % LEFT, NUMBER)
      ELSE
         CALL INSERT (T % RIGHT, NUMBER)
      END IF
 
   END SUBROUTINE INSERT
 
   RECURSIVE SUBROUTINE PRINT_TREE (T)
   ! Print tree in infix order
 
      IMPLICIT NONE
      TYPE (NODE), POINTER :: T  ! A tree
 
      IF (ASSOCIATED (T)) THEN
         CALL PRINT_TREE (T % LEFT)
         PRINT *, T % VALUE
         CALL PRINT_TREE (T % RIGHT)
      END IF
 
   END SUBROUTINE PRINT_TREE
 
end module m_treesort
