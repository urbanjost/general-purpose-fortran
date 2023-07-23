     program demo_journal
     use M_framework__journal, only : journal
     !! BASIC USAGE
     call journal(&
     & 'write to standard output as-is, and trail file as a comment if open')
     ! since trail file is not yet open, only stdout will display output
     call journal('c','ignored, as trail file is not open')
     ! now open trail file "trail"
     call journal('o','trail')
     call journal('sc','same thing except now trail file is open')
     ! only write to trail file if open
     call journal('c',&
     & 'not ignored, as trail file is open. Written with # suffix')
     call journal('t',&
     & 'not ignored, as trail file is open. Written as-is')
     ! turn off trail file
     call journal('o','')
     end program demo_journal
