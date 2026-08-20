     program demo_escape
     ! demonstrate filter to expand C-like escape sequences in input lines
     use iso_fortran_env, only : stdout => output_unit
     use M_unicode,       only : ut=>unicode_type,ch=>character,len
     use M_unicode,       only : assignment(=), trim, remove_backslash
     implicit none
     type(ut),allocatable  :: poem(:)
     type(ut)              :: test(5)
     integer               :: i
        !
        ! “The Crow and the Fox” by Jean de la Fontaine
        write(stdout,'(a,/)') &
        'Le Corbeau et le Renard -- Jean de la Fontaine'
        !
        poem=[&
        ut( 'Le Corbeau et le Renard'                                   ),&
        ut( ''                                                          ),&
        ut( 'Ma\u00EEtre Corbeau, sur un arbre perch\u00E9,'            ),&
        ut( 'Tenait en son bec un fromage.'                             ),&
        ut( 'Ma\u00EEtre Renard, par l\u2019odeur all\u00E9ch\u00E9,'   ),&
        ut( 'Lui tint \U000000E0 peu pr\U000000E8s ce langage :'        ),&
        ut( '\U000000ABH\U000000E9 ! bonjour, Monsieur du Corbeau.'     ),&
        ut( 'Que vous \U000000EAtes joli ! que vous me semblez beau !'  ),&
        ut( 'Sans mentir, si votre ramage'                              ),&
        ut( 'Se rapporte \U000000E0 votre plumage,'                     ),&
        ut( 'Vous \xEAtes le Ph\xE9nix des h\xF4tes de ces bois.\xBB'   ),&
        ut( 'A ces mots le Corbeau ne se sent pas de joie ;'            ),&
        ut( 'Et pour montrer sa belle voix,'                            ),&
        ut( 'Il ouvre un large bec, laisse tomber sa proie.'            ),&
        ut( 'Le Renard s\u2019en saisit, et dit : \xABMon bon Monsieur,'),&
        ut( 'Apprenez que tout flatteur'                                ),&
        ut( 'Vit aux d\xE9pens de celui qui l\U00002019\u00E9coute :'   ),&
        ut( 'Cette le\xE7on vaut bien un fromage, sans doute.\xBB'      ),&
        ut( 'Le Corbeau, honteux et confus,'                            ),&
        ut( &
        'Jura, mais un peu tard, qu\u2019on ne l\u2019y prendrait plus.'),&
        ut( ' -- Jean de la Fontaine')]
        !
        poem=remove_backslash(poem)
        write(stdout,'(g0)')ch(poem)
        !
        test=[ &
         '\e[H\e[2J           ',& ! home cursor and clear screen
                                  ! on ANSI terminals
         '\tABC\tabc          ',& ! write some tabs in the output
         '\tA\a               ',& ! ring bell at end if supported
         '\nONE\nTWO\nTHREE   ',& ! place one word per line
         '\\                  ']
        test=trim(remove_backslash(test))
        write(*,'(a)')(test(i)%character(),i=1,size(test))
        !
     end program demo_escape
