     program demo_transliterate

      use M_unicode, only : transliterate,ut=>unicode_type
      use M_unicode, only : write(formatted),ch=>character
      use M_unicode, only : assignment(=)
      implicit none
      character(len=*),parameter :: u='(DT)'
      type(ut)  :: STRING, UPPER, LOWER
      type(ut)  :: MIDDLE_DOT

         STRING='aAbBcCdDeEfFgGhHiIjJkKlLmMnNoOpPqQrRsStTuUvVwWxXyYzZ'
         LOWER='abcdefghijklmnopqrstuvwxyz'
         UPPER='ABCDEFGHIJKLMNOPQRSTUVWXYZ'
         call callit()

         print u
         print u,ut('Greek')
         !
         ! | Α α | Β β | Γ γ | Δ δ | Ε ε | Ζ ζ       |
         ! | Η η | Θ θ | Ι ι | Κ κ | Λ λ | Μ μ       |
         ! | Ν ν | Ξ ξ | Ο ο | Π π | Ρ ρ | Σ σ ς |
         ! | Τ τ | Υ υ | Φ φ | Χ χ | Ψ ψ | Ω ω       |
         !
         STRING='ΑαΒβΓγΔδΕεΖζΗηΘθΙιΚκΛλΜμΝνΞξΟοΠπΡρΣσςΤτΥυΦφΧχΨψΩω'
         ! ignoring ς for simplicity
         UPPER='ΑΒΓΔΕΖΗΘΙΚΛΜΝΞΟΠΡΣΤΥΦΧΨΩ'
         LOWER='αβγδεζηθικλμνξοπρστυφχψω'
         call callit()

         ! OOP
         print u
         print u,ut('OOP!')
         print u,STRING%TRANSLITERATE(UPPER,'_')

         ! U+00B7 Middle Dot Unicode Character
         print u,STRING%TRANSLITERATE(LOWER,'·') ! ASCII bytes
         print u,STRING%TRANSLITERATE(LOWER,ut('·')) ! cast
         MIDDLE_DOT=int(z'00B7')
         print u,STRING%TRANSLITERATE(LOWER,MIDDLE_DOT) ! hexadecimal

      contains
      subroutine callit()
           print u, STRING

           ! convert -7 string to uppercase:
           print u, TRANSLITERATE(STRING , LOWER, UPPER )

           ! change all miniscule letters to a colon (":"):
           print u, TRANSLITERATE(STRING, LOWER, ':')

           ! delete all miniscule letters
           print u, TRANSLITERATE(STRING, LOWER, '')

           end subroutine callit

      end program demo_transliterate
