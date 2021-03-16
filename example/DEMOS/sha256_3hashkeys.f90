          program demo_sha256
          use M_hashkeys, only : sha256, dirty_sha256
          implicit none
          character(len=:),allocatable :: str
          character(len=64)            :: ref

          ! Test the sha256 function with a set of reference strings.

          str=""
          ref="E3B0C44298FC1C149AFBF4C8996FB92427AE41E4649B934CA495991B7852B855"
          call unit_check('sha256',sha256(str)==ref,'test sha256 1')

          str="abc"
          ref="BA7816BF8F01CFEA414140DE5DAE2223B00361A396177A9CB410FF61F20015AD"
          call unit_check('sha256',sha256(str)==ref,'test sha256 2')

          str="abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq"
          ref="248D6A61D20638B8E5C026930C3E6039A33CE45964FF2167F6ECEDD419DB06C1"
          call unit_check('sha256',sha256(str)==ref,'test sha256 3')

          str="abcdefghbcdefghicdefghijdefghijkefghijklfghi&
               &jklmghijklmnhijklmnoijklmnopjklmnopqklmnopqrlmnopqrsmnopqrstnopqrstu"
          ref="CF5B16A778AF8380036CE59E7B0492370B249B11E8F07A51AFAC45037AFEE9D1"
          call unit_check('sha256',sha256(str)==ref,'test sha256 4')

          str=repeat("a",1000000)
          ref="CDC76E5C9914FB9281A1C7E284D73E67F1809A48A497200E046D39CCC7112CD0"
          call unit_check('sha256',sha256(str)==ref,'test sha256 5')

          str="message digest"
          ref="F7846F55CF23E14EEBEAB5B4E1550CAD5B509E3348FBC4EFA3A1413D393CB650"
          call unit_check('sha256',sha256(str)==ref,'test sha256 6')

          str="secure hash algorithm"
          ref="F30CEB2BB2829E79E4CA9753D35A8ECC00262D164CC077080295381CBD643F0D"
          call unit_check('sha256',sha256(str)==ref,'test sha256 7')

          str="SHA256 is considered to be safe"
          ref="6819D915C73F4D1E77E4E1B52D1FA0F9CF9BEAEAD3939F15874BD988E2A23630"
          call unit_check('sha256',sha256(str)==ref,'test sha256 8')

          str="For this sample, this 63-byte string will be used as input data"
          ref="F08A78CBBAEE082B052AE0708F32FA1E50C5C421AA772BA5DBB406A2EA6BE342"
          call unit_check('sha256',sha256(str)==ref,'test sha256 9')

          str="This is exactly 64 bytes long, not counting the terminating byte"
          ref="AB64EFF7E88E2E46165E29F2BCE41826BD4C7B3552F6B382A9E7D3AF47C245F8"
          call unit_check('sha256',sha256(str)==ref,'test sha256 10')

          ! Check the quick and dirty implementation as well.
          ref="69E3FACD5F08321F78117BD53476E5321845433356F106E7013E68EC367F3017"
          call unit_check('sha256',dirty_sha256(str)==ref,'test dirtysha256 1')

          !!str=repeat("abcdefghbcdefghicdefghijdefghijkefghijklfghijklmghijklmnhijklmno",16777216)
          !!ref="50E72A0E26442FE2552DC3938AC58658228C0CBFB1D2CA872AE435266FCD055E"
          !!call unit_check('sha256',sha256(str)==ref,'test sha256 11 -- long test')

          contains
          subroutine unit_check(name,test,message)
          character(len=*),intent(in) :: name
          logical,intent(in)          :: test
          character(len=*),intent(in) :: message
             write(*,'(a)') repeat("=",64)
             write(*,'(a)') sha256(str)
             write(*,'(a)') ref
             if(test)then
                write(*,*)trim(name)," PASSED: ",trim(message)
             else
                write(*,*)trim(name)," FAILED: ",trim(message)
             endif
          end subroutine unit_check
          !
          end program demo_sha256
