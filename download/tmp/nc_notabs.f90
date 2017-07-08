!-----------------------------------------------------------------------------------------------------------------------------------
!<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>-
!-----------------------------------------------------------------------------------------------------------------------------------
subroutine nc_notabs(INSTR,OUTSTR,ILEN)
! @(#) convert tabs in input to spaces in output while maintaining columns, assuming a tab is set every 8 characters
! given input string INSTR
! return output string OUTSTR
!  o with tabs expanded assuming tabs are set every 8 characters
!  o carriage return and line feed characters are replaced with a space
!  o ILEN holds the position of the last non-blank character in OUTSTR
!
!
! USES:
!       It is often useful to expand tabs in input files to simplify further processing such as tokenizing an input line.
!       Some FORTRAN compilers hate tabs in input files; some printers; some editors will have problems with tabs.
! AUTHOR:
!       John S. Urban
!
! SEE ALSO:
!       GNU/Unix commands expand(1) and unexpand(1)
!
   use M_journal, only : journal
   IMPLICIT NONE
   CHARACTER(LEN=*),INTENT(IN)   :: instr     ! input line to scan for tab characters
   CHARACTER(LEN=*),INTENT(OUT)  :: outstr    ! tab-expanded version of INSTR produced
   INTEGER,INTENT(OUT)           :: ilen      ! column position of last character put into output string

   INTEGER,PARAMETER             :: tabsize=8 ! assume a tab stop is set every 8th column
   INTEGER                       :: ipos      ! position in OUTSTR to put next character of INSTR
   INTEGER                       :: lenin     ! length of input string trimmed of trailing spaces
   INTEGER                       :: lenout    ! number of characters output string can hold
   INTEGER                       :: istep     ! counter that advances thru input string INSTR one character at a time
   CHARACTER(LEN=1)              :: c         ! character in input line being processed
   INTEGER                       :: iade      ! ADE (ASCII Decimal Equivalent) of character being tested
!===================================================================================================================================
   IPOS=1                                     ! where to put next character in output string OUTSTR
   lenin=LEN(instr)                           ! length of character variable INSTR
   lenin=LEN_TRIM(instr(1:lenin))             ! length of INSTR trimmed of trailing spaces
   lenout=LEN(outstr)                         ! number of characters output string OUTSTR can hold
   OUTSTR=" "                                 ! this SHOULD blank-fill string, a buggy machine required a loop to set all characters
!===================================================================================================================================
      SCAN_LINE: DO istep=1,lenin             ! look through input string one character at a time
         c=instr(istep:istep)                 ! get next character
         iade=ICHAR(c)                        ! get ADE of the character
         expand_tabs : SELECT CASE (iade)     ! take different actions depending on which character was found
         CASE(9)                              ! test if character is a tab and move pointer out to appropriate column
            ipos = ipos + (tabsize - (MOD(ipos-1,tabsize)))
         CASE(10,13)                          ! convert carriage-return and new-line to space ,typically to handle DOS-format files
            ipos=ipos+1
         CASE DEFAULT                         ! c is anything else other than a tab,newline,or return  insert it in output string
            IF(ipos > lenout)THEN
               CALL journal("*nc_notabs* output string overflow")
               EXIT
            ELSE
               outstr(ipos:ipos)=c
               ipos=ipos+1
            ENDIF
         END SELECT expand_tabs
      enddo SCAN_LINE
!===================================================================================================================================
      ipos=MIN(ipos,lenout)                   ! tabs or newline or return characters or last character might have gone too far
      ilen=LEN_TRIM(outstr(:ipos))            ! trim trailing spaces
!===================================================================================================================================
END SUBROUTINE nc_notabs
