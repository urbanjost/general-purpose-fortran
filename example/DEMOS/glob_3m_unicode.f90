     program demo_glob
     use M_unicode, only : glob, trim, unicode_type, len
     use M_unicode, only : escape
     use M_unicode, only : assignment(=)
     implicit none
     integer :: i
     type(unicode_type),allocatable :: ufiles(:)
     type(unicode_type),allocatable :: matched(:)
     character(len=*),parameter :: &
      filenames(*)= [character(len=256) :: &
     & 'My_favorite_file.F90',    & ! English
     & '我最喜欢的文档.c',         & ! Mandarin_Chinese
     & 'मरी_पसदीदा_फाइल.f90',       & ! Hindu
     & 'Mi_archivo_favorito.c',   & ! Spanish
     & 'ملفي_المفضل.h',         & ! Modern_Standard_Arabic
     & 'Mon_fichier_préféré.f90', & ! French
     & 'আমার_পরিয_ফাইল',          & ! Bengali
     & 'Meu_arquivo_favorito',    & ! Portuguese
     & 'Мой_любимый_файл',          & ! Russian
     & 'میری_پسندیدہ_فائل.pdf',   & ! Urdu
     & 'src/M_modules.F90',       &
     & 'src/subset.inc',          &
     & 'test/check.f90 ',         &
     & 'app/main.f90 ']
     character(len=*),parameter :: &
      encoded(*)= [character(len=256) :: &
     & 'My_favorite_file.F90',                    & ! English
     & '\u6211\u6700\u559C\u6B22\u7684\u6587\u6863.c', & ! Mandarin_Chinese
     & '\u092E\u0947\u0930\u0940_&
     &\u092A\u0938\u0902\u0926\u0940\u0926\u093E_&
     &\u092B\u093C\u093E\u0907\u0932.f90',        & ! Hindu
     & 'Mi_archivo_favorito.c',                   & ! Spanish
     & '\u0645\u0644\u0641\u064A_&
     &\u0627\u0644\u0645\u0641\u0636\u0644.h ',   & ! Modern_Standard_Arabic
     & 'Mon_fichier_pr\xE9f\xE9r\xE9.f90',        & ! French
     & '\u0986\u09AE\u09BE\u09B0_\u09AA\u09CD\u09B0\u09BF\u09AF\u09BC_&
     &\u09AB\u09BE\u0987\u09B2',                  & ! Bengali
     & 'Meu_arquivo_favorito',                    & ! Portuguese
     & '\u041C\u043E\u0439_\u043B\u044E\u0431\u0438\u043C\u044B\u0439_&
     &\u0444\u0430\u0439\u043B',                  & ! Russian
     & '\u0645\u06CC\u0631\u06CC_&
     &\u067E\u0633\u0646\u062F\u06CC\u062F\u06C1_&
     &\u0641\u0627\u0626\u0644.pdf',              & ! Urdu
     & 'src/M_modules.F90', &
     & 'src/subset.inc', &
     & 'test/check.f90 ', &
     & 'app/main.f90 ']
     character(len=*),parameter :: &
       g='(*(g0))', g1='(*(g0,1x))', comma='(*(g0:,", ",/))'

        ! some basic usage
        write(*,g)merge('PASSED','FAILED',glob("mississipPI", "*issip*PI"))
        write(*,g)merge('PASSED','FAILED',glob("bLah", "bL?h"))
        write(*,g)merge('PASSED','FAILED',glob("bLaH", "?LaH"))

        ! create a list of trimmed filenames
        ufiles=unicode_type(filenames)
        ufiles=trim(ufiles)
        write(*,g)'FILENAMES:'
        call show_filenames(ufiles)

        ! create a list of trimmed filenames from encoded names
        ufiles=escape(encoded)
        ufiles=trim(ufiles)
        write(*,g)'ENCODED FILENAMES:'
        call show_filenames(ufiles)

        ! get filenames ending in ".f90"
        matched=pack(ufiles,glob(ufiles,'*.f90'))
        write(*,g)'MATCHED *.f90:'
        call show_filenames(matched)

        ! get filenames ending in ".c"
        matched=pack(ufiles,glob(ufiles,'*.c'))
        write(*,g)'MATCHED *.c:'
        call show_filenames(matched)

     contains
     subroutine show_filenames(names)
     type(unicode_type),allocatable :: names(:)
        write(*,g1)':SIZE:',size(names),':LEN:',len(names)
        write(*,comma)(names(i)%character(),i=1,size(names))
     end subroutine show_filenames

     end program demo_glob
