       program demo_add_backslash
       ! filter to replace all but printable ASCII-7 characters
       ! with C-like escape sequences
       use iso_fortran_env, only : stdout => output_unit
       use M_unicode,       only : add_backslash
       use M_unicode,       only : assignment(=)
       use M_unicode,       only : ut => unicode_type
       implicit none
       character(len=:),allocatable :: poem(:)
       type(ut)                     :: uline
       character(len=:),allocatable :: aline
       integer                      :: i
          !
          ! “The Crow and the Fox” by Jean de la Fontaine
          !
          poem=[character(len=255) :: &
          'Le Corbeau et le Renard                               ',&
          '                                                      ',&
          'Maître Corbeau, sur un arbre perché,                ',&
          'Tenait en son bec un fromage.                         ',&
          'Maître Renard, par l’odeur alléché,                      ',&
          'Lui tint à peu près ce langage :                    ',&
          '«Hé ! bonjour, Monsieur du Corbeau.                         ',&
          'Que vous êtes joli ! que vous me semblez beau !      ',&
          'Sans mentir, si votre ramage                          ',&
          'Se rapporte à votre plumage,                         ',&
          'Vous êtes le Phénix des hôtes de ces bois.»               ',&
          'A ces mots le Corbeau ne se sent pas de joie ;        ',&
          'Et pour montrer sa belle voix,                        ',&
          'Il ouvre un large bec, laisse tomber sa proie.        ',&
          'Le Renard s’en saisit, et dit : «Mon bon Monsieur,         ',&
          'Apprenez que tout flatteur                            ',&
          'Vit aux dépens de celui qui l’écoute :            ',&
          'Cette leçon vaut bien un fromage, sans doute.»      ',&
          'Le Corbeau, honteux et confus,                        ',&
          'Jura, mais un peu tard, qu’on ne l’y prendrait plus.  ',&
          ' -- Jean de la Fontaine                               ']

          do i=1,size(poem)
             ! convert UTF-8 to UNICODE_TYPE for demonstration purposes
             uline=poem(i)
             aline=add_backslash(uline)
             write(stdout,'(g0)')trim(aline)
          enddo

          do i=1,size(poem)
             aline=add_backslash(poem(i))
             write(stdout,'(g0)')trim(aline)
          enddo

       end program demo_add_backslash
