program license
use, intrinsic :: iso_fortran_env, only: stderr => ERROR_UNIT, stdout => OUTPUT_UNIT, stdin => INPUT_UNIT
use M_strings, only: lower, replace, stretch, transliterate
use M_CLI2, only: set_args, lget, sget, sgets, spdx_names => unnamed, specified
use M_io, only: fileread, get_env

! https://spdx.org/licenses/
! lists many of the common licenses, their abbreviations, and a canononical web page for the license.
character(len=:), allocatable   :: textblock(:)
character(len=:), allocatable   :: configblock(:)
character(len=:), allocatable   :: spdx(:)
character(len=:), allocatable   :: filename
character(len=:), allocatable   :: config
character(len=20)               :: cnum
character(len=8)                :: date
character(len=:), allocatable   :: help(:), version(:)
integer :: istart
integer :: i
integer :: j
integer :: iostat
integer :: width
logical :: all
logical :: verbose
   istart=2
   call setup()
   call set_args(' --all F --fortran F --file " " --config "'//get_env("FPM_LICENSE_CONFIG")//'"', help, version)
   all = lget('all')
   verbose = lget('verbose')
   if(verbose)then
      istart=1
   endif
   filename = sget('file')

   config = sget('config')
   if (config .ne. '') then
      call fileread(config, configblock)
   else
      configblock = [character(len=0)::]
   end if
   call date_and_time(date)
   ! add predefined substitutions
   configblock = [character(len=max(12, len(configblock))):: configblock, &  ! add some predefined conversion strings
                  '@YEAR@=>'//date(1:4), &
                  '@MONTH@=>'//date(5:6), &
                  '@DAY@=>'//date(7:8)] ! widen file is neccessary

   if (specified('file')) then
      if (filename .eq. '') then
         call fileread(stdin, textblock)
      else
         call fileread(filename, textblock)
      end if
      if (size(textblock) > 0) then
         textblock = [character(len=max(len(filename), len(textblock))):: filename, textblock]
         call replaceit()
         call codeit()
      end if
   elseif (all) then
      do i = 1, huge(i) - 1
         write (cnum, '(i0)') i
         textblock = show_one(cnum)
         if (textblock(1) == 'UNKNOWN') exit
         call replaceit()
         if (lget('fortran')) then
            call codeit()
         else
            !write(*,'(at)')textblock
            write (*, '(a)') (trim(textblock(j)), j=min(istart, size(textblock)), size(textblock))
         end if
      end do
   elseif (size(spdx_names) /= 0) then
      do i = 1, size(spdx_names)
         write (cnum, '(i0)') i
         textblock = show_one(lower(spdx_names(i)))
         if (textblock(1) == 'UNKNOWN') then
            write (stderr, '(*(g0))') '*fpm-license*<ERROR> no match for SPDX license name ', trim(spdx_names(i))
            cycle
         end if
         call replaceit()
         if (lget('fortran')) then
            call codeit()
         else
            !write(*,'(at)')textblock
            write (*, '(a)') (trim(textblock(j)), j=min(istart, size(textblock)), size(textblock))
         end if
      end do
   else
      spdx = [character(len=0) ::]
      do i = 1, huge(i) - 1
         write (cnum, '(i0)') i
         textblock = show_one(cnum, topic_only=.true.)
         if (textblock(1) == 'UNKNOWN') exit
         width = max(len(trim(textblock(1))), len(spdx))
         spdx = [character(len=width) :: spdx, textblock]
      end do
      write (*, '(4(a,1x))') (spdx(j), j=1, size(spdx))
   end if
   if(verbose)then
      REFER: block
      character(len=*),parameter :: verbage(*)=[ CHARACTER(LEN=128) :: &
         'SEE ALSO:',&
         ' + https://spdx.org/licenses/',&
         ' + http://choosealicense.com/',&
         ' + https://opensource.guide/legal/',&
         ' + https://docs.github.com/en/repositories/',&
         '']
         flush (unit=stderr,iostat=iostat)
         flush (unit=stdout,iostat=iostat)
         write (stderr, '(a)') (trim(verbage(j)), j=1, size(verbage))
      endblock REFER
   endif
contains
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
function show_one(name,topic_only) result (textblock)
character(len=*),intent(in)      :: name
logical,intent(in),optional      :: topic_only
character(len=256),allocatable   :: textblock(:)
integer                          :: i
select case(name)
case('1','0bsd')
textblock=[ CHARACTER(LEN=128) :: &
'0bsd',&
'    ',&
'BSD Zero Clause License',&
'                       ',&
'Copyright (c) @YEAR@ @FULLNAME@',&
'                               ',&
'Permission to use, copy, modify, and/or distribute this software for any',&
'purpose with or without fee is hereby granted.                          ',&
'                                                                        ',&
'THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES WITH',&
'REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY  ',&
'AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT, ',&
'INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM  ',&
'LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR',&
'OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR       ',&
'PERFORMANCE OF THIS SOFTWARE.                                                ',&
'']
if(present(topic_only))then
   if(size(textblock)>0)then
      if(topic_only) textblock=[character(len=len(textblock(1))) :: textblock(1)]
   else
      textblock=['']
   endif
endif
case('2','afl-3.0')
textblock=[ CHARACTER(LEN=128) :: &
'afl-3.0',&
'       ',&
'Academic Free License ("AFL") v. 3.0',&
'                                    ',&
'This Academic Free License (the "License") applies to any original work of',&
'authorship (the "Original Work") whose owner (the "Licensor") has placed the',&
'following licensing notice adjacent to the copyright notice for the Original',&
'Work:                                                                       ',&
'                                                                            ',&
'  Licensed under the Academic Free License version 3.0                      ',&
'                                                                            ',&
'1) Grant of Copyright License. Licensor grants You a worldwide, royalty-free,',&
'non-exclusive, sublicensable license, for the duration of the copyright, to do',&
'the following:                                                                ',&
'                                                                              ',&
'  a) to reproduce the Original Work in copies, either alone or as part of a   ',&
'  collective work;                                                            ',&
'                                                                              ',&
'  b) to translate, adapt, alter, transform, modify, or arrange the Original   ',&
'  Work, thereby creating derivative works ("Derivative Works") based upon the ',&
'  Original Work;                                                              ',&
'                                                                              ',&
'  c) to distribute or communicate copies of the Original Work and Derivative  ',&
'  Works to the public, under any license of your choice that does not         ',&
'  contradict the terms and conditions, including Licensor''s reserved rights  ',&
'  and remedies, in this Academic Free License;                                ',&
'                                                                              ',&
'  d) to perform the Original Work publicly; and                               ',&
'                                                                              ',&
'  e) to display the Original Work publicly.                                   ',&
'                                                                              ',&
'2) Grant of Patent License. Licensor grants You a worldwide, royalty-free,    ',&
'non-exclusive, sublicensable license, under patent claims owned or controlled ',&
'by the Licensor that are embodied in the Original Work as furnished by the    ',&
'Licensor, for the duration of the patents, to make, use, sell, offer for sale,',&
'have made, and import the Original Work and Derivative Works.                 ',&
'                                                                              ',&
'3) Grant of Source Code License. The term "Source Code" means the preferred   ',&
'form of the Original Work for making modifications to it and all available    ',&
'documentation describing how to modify the Original Work. Licensor agrees to  ',&
'provide a machine-readable copy of the Source Code of the Original Work along ',&
'with each copy of the Original Work that Licensor distributes. Licensor       ',&
'reserves the right to satisfy this obligation by placing a machine-readable   ',&
'copy of the Source Code in an information repository reasonably calculated to ',&
'permit inexpensive and convenient access by You for as long as Licensor       ',&
'continues to distribute the Original Work.                                    ',&
'                                                                              ',&
'4) Exclusions From License Grant. Neither the names of Licensor, nor the names',&
'of any contributors to the Original Work, nor any of their trademarks or      ',&
'service marks, may be used to endorse or promote products derived from this   ',&
'Original Work without express prior permission of the Licensor. Except as     ',&
'expressly stated herein, nothing in this License grants any license to        ',&
'Licensor''s trademarks, copyrights, patents, trade secrets or any other       ',&
'intellectual property. No patent license is granted to make, use, sell, offer ',&
'for sale, have made, or import embodiments of any patent claims other than the',&
'licensed claims defined in Section 2. No license is granted to the trademarks ',&
'of Licensor even if such marks are included in the Original Work. Nothing in  ',&
'this License shall be interpreted to prohibit Licensor from licensing under   ',&
'terms different from this License any Original Work that Licensor otherwise   ',&
'would have a right to license.                                                ',&
'                                                                              ',&
'5) External Deployment. The term "External Deployment" means the use,         ',&
'distribution, or communication of the Original Work or Derivative Works in any',&
'way such that the Original Work or Derivative Works may be used by anyone     ',&
'other than You, whether those works are distributed or communicated to those  ',&
'persons or made available as an application intended for use over a network.  ',&
'As an express condition for the grants of license hereunder, You must treat   ',&
'any External Deployment by You of the Original Work or a Derivative Work as a ',&
'distribution under section 1(c).                                              ',&
'                                                                              ',&
'6) Attribution Rights. You must retain, in the Source Code of any Derivative  ',&
'Works that You create, all copyright, patent, or trademark notices from the   ',&
'Source Code of the Original Work, as well as any notices of licensing and any ',&
'descriptive text identified therein as an "Attribution Notice." You must cause',&
'the Source Code for any Derivative Works that You create to carry a prominent ',&
'Attribution Notice reasonably calculated to inform recipients that You have   ',&
'modified the Original Work.                                                   ',&
'                                                                              ',&
'7) Warranty of Provenance and Disclaimer of Warranty. Licensor warrants that  ',&
'the copyright in and to the Original Work and the patent rights granted herein',&
'by Licensor are owned by the Licensor or are sublicensed to You under the     ',&
'terms of this License with the permission of the contributor(s) of those      ',&
'copyrights and patent rights. Except as expressly stated in the immediately   ',&
'preceding sentence, the Original Work is provided under this License on an "AS',&
'IS" BASIS and WITHOUT WARRANTY, either express or implied, including, without ',&
'limitation, the warranties of non-infringement, merchantability or fitness for',&
'a particular purpose. THE ENTIRE RISK AS TO THE QUALITY OF THE ORIGINAL WORK  ',&
'IS WITH YOU. This DISCLAIMER OF WARRANTY constitutes an essential part of this',&
'License. No license to the Original Work is granted by this License except    ',&
'under this disclaimer.                                                        ',&
'                                                                              ',&
'8) Limitation of Liability. Under no circumstances and under no legal theory, ',&
'whether in tort (including negligence), contract, or otherwise, shall the     ',&
'Licensor be liable to anyone for any indirect, special, incidental, or        ',&
'consequential damages of any character arising as a result of this License or ',&
'the use of the Original Work including, without limitation, damages for loss  ',&
'of goodwill, work stoppage, computer failure or malfunction, or any and all   ',&
'other commercial damages or losses. This limitation of liability shall not    ',&
'apply to the extent applicable law prohibits such limitation.                 ',&
'                                                                              ',&
'9) Acceptance and Termination. If, at any time, You expressly assented to this',&
'License, that assent indicates your clear and irrevocable acceptance of this  ',&
'License and all of its terms and conditions. If You distribute or communicate ',&
'copies of the Original Work or a Derivative Work, You must make a reasonable  ',&
'effort under the circumstances to obtain the express assent of recipients to  ',&
'the terms of this License. This License conditions your rights to undertake   ',&
'the activities listed in Section 1, including your right to create Derivative ',&
'Works based upon the Original Work, and doing so without honoring these terms ',&
'and conditions is prohibited by copyright law and international treaty.       ',&
'Nothing in this License is intended to affect copyright exceptions and        ',&
'limitations (including "fair use" or "fair dealing"). This License shall      ',&
'terminate immediately and You may no longer exercise any of the rights granted',&
'to You by this License upon your failure to honor the conditions in Section   ',&
'1(c).                                                                         ',&
'                                                                              ',&
'10) Termination for Patent Action. This License shall terminate automatically ',&
'and You may no longer exercise any of the rights granted to You by this       ',&
'License as of the date You commence an action, including a cross-claim or     ',&
'counterclaim, against Licensor or any licensee alleging that the Original Work',&
'infringes a patent. This termination provision shall not apply for an action  ',&
'alleging patent infringement by combinations of the Original Work with other  ',&
'software or hardware.                                                         ',&
'                                                                              ',&
'11) Jurisdiction, Venue and Governing Law. Any action or suit relating to this',&
'License may be brought only in the courts of a jurisdiction wherein the       ',&
'Licensor resides or in which Licensor conducts its primary business, and under',&
'the laws of that jurisdiction excluding its conflict-of-law provisions. The   ',&
'application of the United Nations Convention on Contracts for the             ',&
'International Sale of Goods is expressly excluded. Any use of the Original    ',&
'Work outside the scope of this License or after its termination shall be      ',&
'subject to the requirements and penalties of copyright or patent law in the   ',&
'appropriate jurisdiction. This section shall survive the termination of this  ',&
'License.                                                                      ',&
'                                                                              ',&
'12) Attorneys'' Fees. In any action to enforce the terms of this License or   ',&
'seeking damages relating thereto, the prevailing party shall be entitled to   ',&
'recover its costs and expenses, including, without limitation, reasonable     ',&
'attorneys'' fees and costs incurred in connection with such action, including ',&
'any appeal of such action. This section shall survive the termination of this ',&
'License.                                                                      ',&
'                                                                              ',&
'13) Miscellaneous. If any provision of this License is held to be             ',&
'unenforceable, such provision shall be reformed only to the extent necessary  ',&
'to make it enforceable.                                                       ',&
'                                                                              ',&
'14) Definition of "You" in This License. "You" throughout this License,       ',&
'whether in upper or lower case, means an individual or a legal entity         ',&
'exercising rights under, and complying with all of the terms of, this License.',&
'For legal entities, "You" includes any entity that controls, is controlled by,',&
'or is under common control with you. For purposes of this definition,         ',&
'"control" means (i) the power, direct or indirect, to cause the direction or  ',&
'management of such entity, whether by contract or otherwise, or (ii) ownership',&
'of fifty percent (50%) or more of the outstanding shares, or (iii) beneficial ',&
'ownership of such entity.                                                     ',&
'                                                                              ',&
'15) Right to Use. You may use the Original Work in all ways not otherwise     ',&
'restricted or conditioned by this License or by law, and Licensor promises not',&
'to interfere with or be responsible for such uses by You.                     ',&
'                                                                              ',&
'16) Modification of This License. This License is Copyright © 2005 Lawrence  ',&
'Rosen. Permission is granted to copy, distribute, or communicate this License ',&
'without modification. Nothing in this License permits You to modify this      ',&
'License as applied to the Original Work or to Derivative Works. However, You  ',&
'may modify the text of this License and copy, distribute or communicate your  ',&
'modified version (the "Modified License") and apply it to other original works',&
'of authorship subject to the following conditions: (i) You may not indicate in',&
'any way that your Modified License is the "Academic Free License" or "AFL" and',&
'you may not use those names in the name of your Modified License; (ii) You    ',&
'must replace the notice specified in the first paragraph above with the notice',&
'"Licensed under <insert your license name here>" or with a notice of your own ',&
'that is not confusingly similar to the notice in this License; and (iii) You  ',&
'may not claim that your original works are open source software unless your   ',&
'Modified License has been approved by Open Source Initiative (OSI) and You    ',&
'comply with its license review and certification process.                     ',&
'']
if(present(topic_only))then
   if(size(textblock)>0)then
      if(topic_only) textblock=[character(len=len(textblock(1))) :: textblock(1)]
   else
      textblock=['']
   endif
endif
case('3','agpl-3.0')
textblock=[ CHARACTER(LEN=128) :: &
'agpl-3.0',&
'        ',&
'                    GNU AFFERO GENERAL PUBLIC LICENSE',&
'                       Version 3, 19 November 2007   ',&
'                                                     ',&
' Copyright (C) 2007 Free Software Foundation, Inc. <https://fsf.org/>',&
' Everyone is permitted to copy and distribute verbatim copies        ',&
' of this license document, but changing it is not allowed.           ',&
'                                                                     ',&
'                            Preamble                                 ',&
'                                                                     ',&
'  The GNU Affero General Public License is a free, copyleft license for',&
'software and other kinds of works, specifically designed to ensure     ',&
'cooperation with the community in the case of network server software. ',&
'                                                                       ',&
'  The licenses for most software and other practical works are designed',&
'to take away your freedom to share and change the works.  By contrast, ',&
'our General Public Licenses are intended to guarantee your freedom to  ',&
'share and change all versions of a program--to make sure it remains free',&
'software for all its users.                                             ',&
'                                                                        ',&
'  When we speak of free software, we are referring to freedom, not      ',&
'price.  Our General Public Licenses are designed to make sure that you  ',&
'have the freedom to distribute copies of free software (and charge for  ',&
'them if you wish), that you receive source code or can get it if you    ',&
'want it, that you can change the software or use pieces of it in new    ',&
'free programs, and that you know you can do these things.               ',&
'                                                                        ',&
'  Developers that use our General Public Licenses protect your rights   ',&
'with two steps: (1) assert copyright on the software, and (2) offer     ',&
'you this License which gives you legal permission to copy, distribute   ',&
'and/or modify the software.                                             ',&
'                                                                        ',&
'  A secondary benefit of defending all users'' freedom is that          ',&
'improvements made in alternate versions of the program, if they         ',&
'receive widespread use, become available for other developers to        ',&
'incorporate.  Many developers of free software are heartened and        ',&
'encouraged by the resulting cooperation.  However, in the case of       ',&
'software used on network servers, this result may fail to come about.   ',&
'The GNU General Public License permits making a modified version and    ',&
'letting the public access it on a server without ever releasing its     ',&
'source code to the public.                                              ',&
'                                                                        ',&
'  The GNU Affero General Public License is designed specifically to     ',&
'ensure that, in such cases, the modified source code becomes available  ',&
'to the community.  It requires the operator of a network server to      ',&
'provide the source code of the modified version running there to the    ',&
'users of that server.  Therefore, public use of a modified version, on  ',&
'a publicly accessible server, gives the public access to the source     ',&
'code of the modified version.                                           ',&
'                                                                        ',&
'  An older license, called the Affero General Public License and        ',&
'published by Affero, was designed to accomplish similar goals.  This is ',&
'a different license, not a version of the Affero GPL, but Affero has    ',&
'released a new version of the Affero GPL which permits relicensing under',&
'this license.                                                           ',&
'                                                                        ',&
'  The precise terms and conditions for copying, distribution and        ',&
'modification follow.                                                    ',&
'                                                                        ',&
'                       TERMS AND CONDITIONS                             ',&
'                                                                        ',&
'  0. Definitions.                                                       ',&
'                                                                        ',&
'  "This License" refers to version 3 of the GNU Affero General Public License.',&
'                                                                              ',&
'  "Copyright" also means copyright-like laws that apply to other kinds of     ',&
'works, such as semiconductor masks.                                           ',&
'                                                                              ',&
'  "The Program" refers to any copyrightable work licensed under this          ',&
'License.  Each licensee is addressed as "you".  "Licensees" and               ',&
'"recipients" may be individuals or organizations.                             ',&
'                                                                              ',&
'  To "modify" a work means to copy from or adapt all or part of the work      ',&
'in a fashion requiring copyright permission, other than the making of an      ',&
'exact copy.  The resulting work is called a "modified version" of the         ',&
'earlier work or a work "based on" the earlier work.                           ',&
'                                                                              ',&
'  A "covered work" means either the unmodified Program or a work based        ',&
'on the Program.                                                               ',&
'                                                                              ',&
'  To "propagate" a work means to do anything with it that, without            ',&
'permission, would make you directly or secondarily liable for                 ',&
'infringement under applicable copyright law, except executing it on a         ',&
'computer or modifying a private copy.  Propagation includes copying,          ',&
'distribution (with or without modification), making available to the          ',&
'public, and in some countries other activities as well.                       ',&
'                                                                              ',&
'  To "convey" a work means any kind of propagation that enables other         ',&
'parties to make or receive copies.  Mere interaction with a user through      ',&
'a computer network, with no transfer of a copy, is not conveying.             ',&
'                                                                              ',&
'  An interactive user interface displays "Appropriate Legal Notices"          ',&
'to the extent that it includes a convenient and prominently visible           ',&
'feature that (1) displays an appropriate copyright notice, and (2)            ',&
'tells the user that there is no warranty for the work (except to the          ',&
'extent that warranties are provided), that licensees may convey the           ',&
'work under this License, and how to view a copy of this License.  If          ',&
'the interface presents a list of user commands or options, such as a          ',&
'menu, a prominent item in the list meets this criterion.                      ',&
'                                                                              ',&
'  1. Source Code.                                                             ',&
'                                                                              ',&
'  The "source code" for a work means the preferred form of the work           ',&
'for making modifications to it.  "Object code" means any non-source           ',&
'form of a work.                                                               ',&
'                                                                              ',&
'  A "Standard Interface" means an interface that either is an official        ',&
'standard defined by a recognized standards body, or, in the case of           ',&
'interfaces specified for a particular programming language, one that          ',&
'is widely used among developers working in that language.                     ',&
'                                                                              ',&
'  The "System Libraries" of an executable work include anything, other        ',&
'than the work as a whole, that (a) is included in the normal form of          ',&
'packaging a Major Component, but which is not part of that Major              ',&
'Component, and (b) serves only to enable use of the work with that            ',&
'Major Component, or to implement a Standard Interface for which an            ',&
'implementation is available to the public in source code form.  A             ',&
'"Major Component", in this context, means a major essential component         ',&
'(kernel, window system, and so on) of the specific operating system           ',&
'(if any) on which the executable work runs, or a compiler used to             ',&
'produce the work, or an object code interpreter used to run it.               ',&
'                                                                              ',&
'  The "Corresponding Source" for a work in object code form means all         ',&
'the source code needed to generate, install, and (for an executable           ',&
'work) run the object code and to modify the work, including scripts to        ',&
'control those activities.  However, it does not include the work''s           ',&
'System Libraries, or general-purpose tools or generally available free        ',&
'programs which are used unmodified in performing those activities but         ',&
'which are not part of the work.  For example, Corresponding Source            ',&
'includes interface definition files associated with source files for          ',&
'the work, and the source code for shared libraries and dynamically            ',&
'linked subprograms that the work is specifically designed to require,         ',&
'such as by intimate data communication or control flow between those          ',&
'subprograms and other parts of the work.                                      ',&
'                                                                              ',&
'  The Corresponding Source need not include anything that users               ',&
'can regenerate automatically from other parts of the Corresponding            ',&
'Source.                                                                       ',&
'                                                                              ',&
'  The Corresponding Source for a work in source code form is that             ',&
'same work.                                                                    ',&
'                                                                              ',&
'  2. Basic Permissions.                                                       ',&
'                                                                              ',&
'  All rights granted under this License are granted for the term of           ',&
'copyright on the Program, and are irrevocable provided the stated             ',&
'conditions are met.  This License explicitly affirms your unlimited           ',&
'permission to run the unmodified Program.  The output from running a          ',&
'covered work is covered by this License only if the output, given its         ',&
'content, constitutes a covered work.  This License acknowledges your          ',&
'rights of fair use or other equivalent, as provided by copyright law.         ',&
'                                                                              ',&
'  You may make, run and propagate covered works that you do not               ',&
'convey, without conditions so long as your license otherwise remains          ',&
'in force.  You may convey covered works to others for the sole purpose        ',&
'of having them make modifications exclusively for you, or provide you         ',&
'with facilities for running those works, provided that you comply with        ',&
'the terms of this License in conveying all material for which you do          ',&
'not control copyright.  Those thus making or running the covered works        ',&
'for you must do so exclusively on your behalf, under your direction           ',&
'and control, on terms that prohibit them from making any copies of            ',&
'your copyrighted material outside their relationship with you.                ',&
'                                                                              ',&
'  Conveying under any other circumstances is permitted solely under           ',&
'the conditions stated below.  Sublicensing is not allowed; section 10         ',&
'makes it unnecessary.                                                         ',&
'                                                                              ',&
'  3. Protecting Users'' Legal Rights From Anti-Circumvention Law.             ',&
'                                                                              ',&
'  No covered work shall be deemed part of an effective technological          ',&
'measure under any applicable law fulfilling obligations under article         ',&
'11 of the WIPO copyright treaty adopted on 20 December 1996, or               ',&
'similar laws prohibiting or restricting circumvention of such                 ',&
'measures.                                                                     ',&
'                                                                              ',&
'  When you convey a covered work, you waive any legal power to forbid         ',&
'circumvention of technological measures to the extent such circumvention      ',&
'is effected by exercising rights under this License with respect to           ',&
'the covered work, and you disclaim any intention to limit operation or        ',&
'modification of the work as a means of enforcing, against the work''s         ',&
'users, your or third parties'' legal rights to forbid circumvention of        ',&
'technological measures.                                                       ',&
'                                                                              ',&
'  4. Conveying Verbatim Copies.                                               ',&
'                                                                              ',&
'  You may convey verbatim copies of the Program''s source code as you         ',&
'receive it, in any medium, provided that you conspicuously and                ',&
'appropriately publish on each copy an appropriate copyright notice;           ',&
'keep intact all notices stating that this License and any                     ',&
'non-permissive terms added in accord with section 7 apply to the code;        ',&
'keep intact all notices of the absence of any warranty; and give all          ',&
'recipients a copy of this License along with the Program.                     ',&
'                                                                              ',&
'  You may charge any price or no price for each copy that you convey,         ',&
'and you may offer support or warranty protection for a fee.                   ',&
'                                                                              ',&
'  5. Conveying Modified Source Versions.                                      ',&
'                                                                              ',&
'  You may convey a work based on the Program, or the modifications to         ',&
'produce it from the Program, in the form of source code under the             ',&
'terms of section 4, provided that you also meet all of these conditions:      ',&
'                                                                              ',&
'    a) The work must carry prominent notices stating that you modified        ',&
'    it, and giving a relevant date.                                           ',&
'                                                                              ',&
'    b) The work must carry prominent notices stating that it is               ',&
'    released under this License and any conditions added under section        ',&
'    7.  This requirement modifies the requirement in section 4 to             ',&
'    "keep intact all notices".                                                ',&
'                                                                              ',&
'    c) You must license the entire work, as a whole, under this               ',&
'    License to anyone who comes into possession of a copy.  This              ',&
'    License will therefore apply, along with any applicable section 7         ',&
'    additional terms, to the whole of the work, and all its parts,            ',&
'    regardless of how they are packaged.  This License gives no               ',&
'    permission to license the work in any other way, but it does not          ',&
'    invalidate such permission if you have separately received it.            ',&
'                                                                              ',&
'    d) If the work has interactive user interfaces, each must display         ',&
'    Appropriate Legal Notices; however, if the Program has interactive        ',&
'    interfaces that do not display Appropriate Legal Notices, your            ',&
'    work need not make them do so.                                            ',&
'                                                                              ',&
'  A compilation of a covered work with other separate and independent         ',&
'works, which are not by their nature extensions of the covered work,          ',&
'and which are not combined with it such as to form a larger program,          ',&
'in or on a volume of a storage or distribution medium, is called an           ',&
'"aggregate" if the compilation and its resulting copyright are not            ',&
'used to limit the access or legal rights of the compilation''s users          ',&
'beyond what the individual works permit.  Inclusion of a covered work         ',&
'in an aggregate does not cause this License to apply to the other             ',&
'parts of the aggregate.                                                       ',&
'                                                                              ',&
'  6. Conveying Non-Source Forms.                                              ',&
'                                                                              ',&
'  You may convey a covered work in object code form under the terms           ',&
'of sections 4 and 5, provided that you also convey the                        ',&
'machine-readable Corresponding Source under the terms of this License,        ',&
'in one of these ways:                                                         ',&
'                                                                              ',&
'    a) Convey the object code in, or embodied in, a physical product          ',&
'    (including a physical distribution medium), accompanied by the            ',&
'    Corresponding Source fixed on a durable physical medium                   ',&
'    customarily used for software interchange.                                ',&
'                                                                              ',&
'    b) Convey the object code in, or embodied in, a physical product          ',&
'    (including a physical distribution medium), accompanied by a              ',&
'    written offer, valid for at least three years and valid for as            ',&
'    long as you offer spare parts or customer support for that product        ',&
'    model, to give anyone who possesses the object code either (1) a          ',&
'    copy of the Corresponding Source for all the software in the              ',&
'    product that is covered by this License, on a durable physical            ',&
'    medium customarily used for software interchange, for a price no          ',&
'    more than your reasonable cost of physically performing this              ',&
'    conveying of source, or (2) access to copy the                            ',&
'    Corresponding Source from a network server at no charge.                  ',&
'                                                                              ',&
'    c) Convey individual copies of the object code with a copy of the         ',&
'    written offer to provide the Corresponding Source.  This                  ',&
'    alternative is allowed only occasionally and noncommercially, and         ',&
'    only if you received the object code with such an offer, in accord        ',&
'    with subsection 6b.                                                       ',&
'                                                                              ',&
'    d) Convey the object code by offering access from a designated            ',&
'    place (gratis or for a charge), and offer equivalent access to the        ',&
'    Corresponding Source in the same way through the same place at no         ',&
'    further charge.  You need not require recipients to copy the              ',&
'    Corresponding Source along with the object code.  If the place to         ',&
'    copy the object code is a network server, the Corresponding Source        ',&
'    may be on a different server (operated by you or a third party)           ',&
'    that supports equivalent copying facilities, provided you maintain        ',&
'    clear directions next to the object code saying where to find the         ',&
'    Corresponding Source.  Regardless of what server hosts the                ',&
'    Corresponding Source, you remain obligated to ensure that it is           ',&
'    available for as long as needed to satisfy these requirements.            ',&
'                                                                              ',&
'    e) Convey the object code using peer-to-peer transmission, provided       ',&
'    you inform other peers where the object code and Corresponding            ',&
'    Source of the work are being offered to the general public at no          ',&
'    charge under subsection 6d.                                               ',&
'                                                                              ',&
'  A separable portion of the object code, whose source code is excluded       ',&
'from the Corresponding Source as a System Library, need not be                ',&
'included in conveying the object code work.                                   ',&
'                                                                              ',&
'  A "User Product" is either (1) a "consumer product", which means any        ',&
'tangible personal property which is normally used for personal, family,       ',&
'or household purposes, or (2) anything designed or sold for incorporation     ',&
'into a dwelling.  In determining whether a product is a consumer product,     ',&
'doubtful cases shall be resolved in favor of coverage.  For a particular      ',&
'product received by a particular user, "normally used" refers to a            ',&
'typical or common use of that class of product, regardless of the status      ',&
'of the particular user or of the way in which the particular user             ',&
'actually uses, or expects or is expected to use, the product.  A product      ',&
'is a consumer product regardless of whether the product has substantial       ',&
'commercial, industrial or non-consumer uses, unless such uses represent       ',&
'the only significant mode of use of the product.                              ',&
'                                                                              ',&
'  "Installation Information" for a User Product means any methods,            ',&
'procedures, authorization keys, or other information required to install      ',&
'and execute modified versions of a covered work in that User Product from     ',&
'a modified version of its Corresponding Source.  The information must         ',&
'suffice to ensure that the continued functioning of the modified object       ',&
'code is in no case prevented or interfered with solely because                ',&
'modification has been made.                                                   ',&
'                                                                              ',&
'  If you convey an object code work under this section in, or with, or        ',&
'specifically for use in, a User Product, and the conveying occurs as          ',&
'part of a transaction in which the right of possession and use of the         ',&
'User Product is transferred to the recipient in perpetuity or for a           ',&
'fixed term (regardless of how the transaction is characterized), the          ',&
'Corresponding Source conveyed under this section must be accompanied          ',&
'by the Installation Information.  But this requirement does not apply         ',&
'if neither you nor any third party retains the ability to install             ',&
'modified object code on the User Product (for example, the work has           ',&
'been installed in ROM).                                                       ',&
'                                                                              ',&
'  The requirement to provide Installation Information does not include a      ',&
'requirement to continue to provide support service, warranty, or updates      ',&
'for a work that has been modified or installed by the recipient, or for       ',&
'the User Product in which it has been modified or installed.  Access to a     ',&
'network may be denied when the modification itself materially and             ',&
'adversely affects the operation of the network or violates the rules and      ',&
'protocols for communication across the network.                               ',&
'                                                                              ',&
'  Corresponding Source conveyed, and Installation Information provided,       ',&
'in accord with this section must be in a format that is publicly              ',&
'documented (and with an implementation available to the public in             ',&
'source code form), and must require no special password or key for            ',&
'unpacking, reading or copying.                                                ',&
'                                                                              ',&
'  7. Additional Terms.                                                        ',&
'                                                                              ',&
'  "Additional permissions" are terms that supplement the terms of this        ',&
'License by making exceptions from one or more of its conditions.              ',&
'Additional permissions that are applicable to the entire Program shall        ',&
'be treated as though they were included in this License, to the extent        ',&
'that they are valid under applicable law.  If additional permissions          ',&
'apply only to part of the Program, that part may be used separately           ',&
'under those permissions, but the entire Program remains governed by           ',&
'this License without regard to the additional permissions.                    ',&
'                                                                              ',&
'  When you convey a copy of a covered work, you may at your option            ',&
'remove any additional permissions from that copy, or from any part of         ',&
'it.  (Additional permissions may be written to require their own              ',&
'removal in certain cases when you modify the work.)  You may place            ',&
'additional permissions on material, added by you to a covered work,           ',&
'for which you have or can give appropriate copyright permission.              ',&
'                                                                              ',&
'  Notwithstanding any other provision of this License, for material you       ',&
'add to a covered work, you may (if authorized by the copyright holders of     ',&
'that material) supplement the terms of this License with terms:               ',&
'                                                                              ',&
'    a) Disclaiming warranty or limiting liability differently from the        ',&
'    terms of sections 15 and 16 of this License; or                           ',&
'                                                                              ',&
'    b) Requiring preservation of specified reasonable legal notices or        ',&
'    author attributions in that material or in the Appropriate Legal          ',&
'    Notices displayed by works containing it; or                              ',&
'                                                                              ',&
'    c) Prohibiting misrepresentation of the origin of that material, or       ',&
'    requiring that modified versions of such material be marked in            ',&
'    reasonable ways as different from the original version; or                ',&
'                                                                              ',&
'    d) Limiting the use for publicity purposes of names of licensors or       ',&
'    authors of the material; or                                               ',&
'                                                                              ',&
'    e) Declining to grant rights under trademark law for use of some          ',&
'    trade names, trademarks, or service marks; or                             ',&
'                                                                              ',&
'    f) Requiring indemnification of licensors and authors of that             ',&
'    material by anyone who conveys the material (or modified versions of      ',&
'    it) with contractual assumptions of liability to the recipient, for       ',&
'    any liability that these contractual assumptions directly impose on       ',&
'    those licensors and authors.                                              ',&
'                                                                              ',&
'  All other non-permissive additional terms are considered "further           ',&
'restrictions" within the meaning of section 10.  If the Program as you        ',&
'received it, or any part of it, contains a notice stating that it is          ',&
'governed by this License along with a term that is a further                  ',&
'restriction, you may remove that term.  If a license document contains        ',&
'a further restriction but permits relicensing or conveying under this         ',&
'License, you may add to a covered work material governed by the terms         ',&
'of that license document, provided that the further restriction does          ',&
'not survive such relicensing or conveying.                                    ',&
'                                                                              ',&
'  If you add terms to a covered work in accord with this section, you         ',&
'must place, in the relevant source files, a statement of the                  ',&
'additional terms that apply to those files, or a notice indicating            ',&
'where to find the applicable terms.                                           ',&
'                                                                              ',&
'  Additional terms, permissive or non-permissive, may be stated in the        ',&
'form of a separately written license, or stated as exceptions;                ',&
'the above requirements apply either way.                                      ',&
'                                                                              ',&
'  8. Termination.                                                             ',&
'                                                                              ',&
'  You may not propagate or modify a covered work except as expressly          ',&
'provided under this License.  Any attempt otherwise to propagate or           ',&
'modify it is void, and will automatically terminate your rights under         ',&
'this License (including any patent licenses granted under the third           ',&
'paragraph of section 11).                                                     ',&
'                                                                              ',&
'  However, if you cease all violation of this License, then your              ',&
'license from a particular copyright holder is reinstated (a)                  ',&
'provisionally, unless and until the copyright holder explicitly and           ',&
'finally terminates your license, and (b) permanently, if the copyright        ',&
'holder fails to notify you of the violation by some reasonable means          ',&
'prior to 60 days after the cessation.                                         ',&
'                                                                              ',&
'  Moreover, your license from a particular copyright holder is                ',&
'reinstated permanently if the copyright holder notifies you of the            ',&
'violation by some reasonable means, this is the first time you have           ',&
'received notice of violation of this License (for any work) from that         ',&
'copyright holder, and you cure the violation prior to 30 days after           ',&
'your receipt of the notice.                                                   ',&
'                                                                              ',&
'  Termination of your rights under this section does not terminate the        ',&
'licenses of parties who have received copies or rights from you under         ',&
'this License.  If your rights have been terminated and not permanently        ',&
'reinstated, you do not qualify to receive new licenses for the same           ',&
'material under section 10.                                                    ',&
'                                                                              ',&
'  9. Acceptance Not Required for Having Copies.                               ',&
'                                                                              ',&
'  You are not required to accept this License in order to receive or          ',&
'run a copy of the Program.  Ancillary propagation of a covered work           ',&
'occurring solely as a consequence of using peer-to-peer transmission          ',&
'to receive a copy likewise does not require acceptance.  However,             ',&
'nothing other than this License grants you permission to propagate or         ',&
'modify any covered work.  These actions infringe copyright if you do          ',&
'not accept this License.  Therefore, by modifying or propagating a            ',&
'covered work, you indicate your acceptance of this License to do so.          ',&
'                                                                              ',&
'  10. Automatic Licensing of Downstream Recipients.                           ',&
'                                                                              ',&
'  Each time you convey a covered work, the recipient automatically            ',&
'receives a license from the original licensors, to run, modify and            ',&
'propagate that work, subject to this License.  You are not responsible        ',&
'for enforcing compliance by third parties with this License.                  ',&
'                                                                              ',&
'  An "entity transaction" is a transaction transferring control of an         ',&
'organization, or substantially all assets of one, or subdividing an           ',&
'organization, or merging organizations.  If propagation of a covered          ',&
'work results from an entity transaction, each party to that                   ',&
'transaction who receives a copy of the work also receives whatever            ',&
'licenses to the work the party''s predecessor in interest had or could        ',&
'give under the previous paragraph, plus a right to possession of the          ',&
'Corresponding Source of the work from the predecessor in interest, if         ',&
'the predecessor has it or can get it with reasonable efforts.                 ',&
'                                                                              ',&
'  You may not impose any further restrictions on the exercise of the          ',&
'rights granted or affirmed under this License.  For example, you may          ',&
'not impose a license fee, royalty, or other charge for exercise of            ',&
'rights granted under this License, and you may not initiate litigation        ',&
'(including a cross-claim or counterclaim in a lawsuit) alleging that          ',&
'any patent claim is infringed by making, using, selling, offering for         ',&
'sale, or importing the Program or any portion of it.                          ',&
'                                                                              ',&
'  11. Patents.                                                                ',&
'                                                                              ',&
'  A "contributor" is a copyright holder who authorizes use under this         ',&
'License of the Program or a work on which the Program is based.  The          ',&
'work thus licensed is called the contributor''s "contributor version".        ',&
'                                                                              ',&
'  A contributor''s "essential patent claims" are all patent claims            ',&
'owned or controlled by the contributor, whether already acquired or           ',&
'hereafter acquired, that would be infringed by some manner, permitted         ',&
'by this License, of making, using, or selling its contributor version,        ',&
'but do not include claims that would be infringed only as a                   ',&
'consequence of further modification of the contributor version.  For          ',&
'purposes of this definition, "control" includes the right to grant            ',&
'patent sublicenses in a manner consistent with the requirements of            ',&
'this License.                                                                 ',&
'                                                                              ',&
'  Each contributor grants you a non-exclusive, worldwide, royalty-free        ',&
'patent license under the contributor''s essential patent claims, to           ',&
'make, use, sell, offer for sale, import and otherwise run, modify and         ',&
'propagate the contents of its contributor version.                            ',&
'                                                                              ',&
'  In the following three paragraphs, a "patent license" is any express        ',&
'agreement or commitment, however denominated, not to enforce a patent         ',&
'(such as an express permission to practice a patent or covenant not to        ',&
'sue for patent infringement).  To "grant" such a patent license to a          ',&
'party means to make such an agreement or commitment not to enforce a          ',&
'patent against the party.                                                     ',&
'                                                                              ',&
'  If you convey a covered work, knowingly relying on a patent license,        ',&
'and the Corresponding Source of the work is not available for anyone          ',&
'to copy, free of charge and under the terms of this License, through a        ',&
'publicly available network server or other readily accessible means,          ',&
'then you must either (1) cause the Corresponding Source to be so              ',&
'available, or (2) arrange to deprive yourself of the benefit of the           ',&
'patent license for this particular work, or (3) arrange, in a manner          ',&
'consistent with the requirements of this License, to extend the patent        ',&
'license to downstream recipients.  "Knowingly relying" means you have         ',&
'actual knowledge that, but for the patent license, your conveying the         ',&
'covered work in a country, or your recipient''s use of the covered work       ',&
'in a country, would infringe one or more identifiable patents in that         ',&
'country that you have reason to believe are valid.                            ',&
'                                                                              ',&
'  If, pursuant to or in connection with a single transaction or               ',&
'arrangement, you convey, or propagate by procuring conveyance of, a           ',&
'covered work, and grant a patent license to some of the parties               ',&
'receiving the covered work authorizing them to use, propagate, modify         ',&
'or convey a specific copy of the covered work, then the patent license        ',&
'you grant is automatically extended to all recipients of the covered          ',&
'work and works based on it.                                                   ',&
'                                                                              ',&
'  A patent license is "discriminatory" if it does not include within          ',&
'the scope of its coverage, prohibits the exercise of, or is                   ',&
'conditioned on the non-exercise of one or more of the rights that are         ',&
'specifically granted under this License.  You may not convey a covered        ',&
'work if you are a party to an arrangement with a third party that is          ',&
'in the business of distributing software, under which you make payment        ',&
'to the third party based on the extent of your activity of conveying          ',&
'the work, and under which the third party grants, to any of the               ',&
'parties who would receive the covered work from you, a discriminatory         ',&
'patent license (a) in connection with copies of the covered work              ',&
'conveyed by you (or copies made from those copies), or (b) primarily          ',&
'for and in connection with specific products or compilations that             ',&
'contain the covered work, unless you entered into that arrangement,           ',&
'or that patent license was granted, prior to 28 March 2007.                   ',&
'                                                                              ',&
'  Nothing in this License shall be construed as excluding or limiting         ',&
'any implied license or other defenses to infringement that may                ',&
'otherwise be available to you under applicable patent law.                    ',&
'                                                                              ',&
'  12. No Surrender of Others'' Freedom.                                       ',&
'                                                                              ',&
'  If conditions are imposed on you (whether by court order, agreement or      ',&
'otherwise) that contradict the conditions of this License, they do not        ',&
'excuse you from the conditions of this License.  If you cannot convey a       ',&
'covered work so as to satisfy simultaneously your obligations under this      ',&
'License and any other pertinent obligations, then as a consequence you may    ',&
'not convey it at all.  For example, if you agree to terms that obligate you   ',&
'to collect a royalty for further conveying from those to whom you convey      ',&
'the Program, the only way you could satisfy both those terms and this         ',&
'License would be to refrain entirely from conveying the Program.              ',&
'                                                                              ',&
'  13. Remote Network Interaction; Use with the GNU General Public License.    ',&
'                                                                              ',&
'  Notwithstanding any other provision of this License, if you modify the      ',&
'Program, your modified version must prominently offer all users               ',&
'interacting with it remotely through a computer network (if your version      ',&
'supports such interaction) an opportunity to receive the Corresponding        ',&
'Source of your version by providing access to the Corresponding Source        ',&
'from a network server at no charge, through some standard or customary        ',&
'means of facilitating copying of software.  This Corresponding Source         ',&
'shall include the Corresponding Source for any work covered by version 3      ',&
'of the GNU General Public License that is incorporated pursuant to the        ',&
'following paragraph.                                                          ',&
'                                                                              ',&
'  Notwithstanding any other provision of this License, you have               ',&
'permission to link or combine any covered work with a work licensed           ',&
'under version 3 of the GNU General Public License into a single               ',&
'combined work, and to convey the resulting work.  The terms of this           ',&
'License will continue to apply to the part which is the covered work,         ',&
'but the work with which it is combined will remain governed by version        ',&
'3 of the GNU General Public License.                                          ',&
'                                                                              ',&
'  14. Revised Versions of this License.                                       ',&
'                                                                              ',&
'  The Free Software Foundation may publish revised and/or new versions of     ',&
'the GNU Affero General Public License from time to time.  Such new versions   ',&
'will be similar in spirit to the present version, but may differ in detail to ',&
'address new problems or concerns.                                             ',&
'                                                                              ',&
'  Each version is given a distinguishing version number.  If the              ',&
'Program specifies that a certain numbered version of the GNU Affero General   ',&
'Public License "or any later version" applies to it, you have the             ',&
'option of following the terms and conditions either of that numbered          ',&
'version or of any later version published by the Free Software                ',&
'Foundation.  If the Program does not specify a version number of the          ',&
'GNU Affero General Public License, you may choose any version ever published  ',&
'by the Free Software Foundation.                                              ',&
'                                                                              ',&
'  If the Program specifies that a proxy can decide which future               ',&
'versions of the GNU Affero General Public License can be used, that proxy''s  ',&
'public statement of acceptance of a version permanently authorizes you        ',&
'to choose that version for the Program.                                       ',&
'                                                                              ',&
'  Later license versions may give you additional or different                 ',&
'permissions.  However, no additional obligations are imposed on any           ',&
'author or copyright holder as a result of your choosing to follow a           ',&
'later version.                                                                ',&
'                                                                              ',&
'  15. Disclaimer of Warranty.                                                 ',&
'                                                                              ',&
'  THERE IS NO WARRANTY FOR THE PROGRAM, TO THE EXTENT PERMITTED BY            ',&
'APPLICABLE LAW.  EXCEPT WHEN OTHERWISE STATED IN WRITING THE COPYRIGHT        ',&
'HOLDERS AND/OR OTHER PARTIES PROVIDE THE PROGRAM "AS IS" WITHOUT WARRANTY     ',&
'OF ANY KIND, EITHER EXPRESSED OR IMPLIED, INCLUDING, BUT NOT LIMITED TO,      ',&
'THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR        ',&
'PURPOSE.  THE ENTIRE RISK AS TO THE QUALITY AND PERFORMANCE OF THE PROGRAM    ',&
'IS WITH YOU.  SHOULD THE PROGRAM PROVE DEFECTIVE, YOU ASSUME THE COST OF      ',&
'ALL NECESSARY SERVICING, REPAIR OR CORRECTION.                                ',&
'                                                                              ',&
'  16. Limitation of Liability.                                                ',&
'                                                                              ',&
'  IN NO EVENT UNLESS REQUIRED BY APPLICABLE LAW OR AGREED TO IN WRITING       ',&
'WILL ANY COPYRIGHT HOLDER, OR ANY OTHER PARTY WHO MODIFIES AND/OR CONVEYS     ',&
'THE PROGRAM AS PERMITTED ABOVE, BE LIABLE TO YOU FOR DAMAGES, INCLUDING ANY   ',&
'GENERAL, SPECIAL, INCIDENTAL OR CONSEQUENTIAL DAMAGES ARISING OUT OF THE      ',&
'USE OR INABILITY TO USE THE PROGRAM (INCLUDING BUT NOT LIMITED TO LOSS OF     ',&
'DATA OR DATA BEING RENDERED INACCURATE OR LOSSES SUSTAINED BY YOU OR THIRD    ',&
'PARTIES OR A FAILURE OF THE PROGRAM TO OPERATE WITH ANY OTHER PROGRAMS),      ',&
'EVEN IF SUCH HOLDER OR OTHER PARTY HAS BEEN ADVISED OF THE POSSIBILITY OF     ',&
'SUCH DAMAGES.                                                                 ',&
'                                                                              ',&
'  17. Interpretation of Sections 15 and 16.                                   ',&
'                                                                              ',&
'  If the disclaimer of warranty and limitation of liability provided          ',&
'above cannot be given local legal effect according to their terms,            ',&
'reviewing courts shall apply local law that most closely approximates         ',&
'an absolute waiver of all civil liability in connection with the              ',&
'Program, unless a warranty or assumption of liability accompanies a           ',&
'copy of the Program in return for a fee.                                      ',&
'                                                                              ',&
'                     END OF TERMS AND CONDITIONS                              ',&
'                                                                              ',&
'            How to Apply These Terms to Your New Programs                     ',&
'                                                                              ',&
'  If you develop a new program, and you want it to be of the greatest         ',&
'possible use to the public, the best way to achieve this is to make it        ',&
'free software which everyone can redistribute and change under these terms.   ',&
'                                                                              ',&
'  To do so, attach the following notices to the program.  It is safest        ',&
'to attach them to the start of each source file to most effectively           ',&
'state the exclusion of warranty; and each file should have at least           ',&
'the "copyright" line and a pointer to where the full notice is found.         ',&
'                                                                              ',&
'    <one line to give the program''s name and a brief idea of what it does.>  ',&
'    Copyright (C) @YEAR@  @NAME_OF_AUTHOR@                                    ',&
'                                                                              ',&
'    This program is free software: you can redistribute it and/or modify      ',&
'    it under the terms of the GNU Affero General Public License as published  ',&
'    by the Free Software Foundation, either version 3 of the License, or      ',&
'    (at your option) any later version.                                       ',&
'                                                                              ',&
'    This program is distributed in the hope that it will be useful,           ',&
'    but WITHOUT ANY WARRANTY; without even the implied warranty of            ',&
'    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the             ',&
'    GNU Affero General Public License for more details.                       ',&
'                                                                              ',&
'    You should have received a copy of the GNU Affero General Public License  ',&
'    along with this program.  If not, see <https://www.gnu.org/licenses/>.    ',&
'                                                                              ',&
'Also add information on how to contact you by electronic and paper mail.      ',&
'                                                                              ',&
'  If your software can interact with users remotely through a computer        ',&
'network, you should also make sure that it provides a way for users to        ',&
'get its source.  For example, if your program is a web application, its       ',&
'interface could display a "Source" link that leads users to an archive        ',&
'of the code.  There are many ways you could offer source, and different       ',&
'solutions will be better for different programs; see section 13 for the       ',&
'specific requirements.                                                        ',&
'                                                                              ',&
'  You should also get your employer (if you work as a programmer) or school,  ',&
'if any, to sign a "copyright disclaimer" for the program, if necessary.       ',&
'For more information on this, and how to apply and follow the GNU AGPL, see   ',&
'<https://www.gnu.org/licenses/>.                                              ',&
'                                                                              ',&
'']
if(present(topic_only))then
   if(size(textblock)>0)then
      if(topic_only) textblock=[character(len=len(textblock(1))) :: textblock(1)]
   else
      textblock=['']
   endif
endif
case('4','apache-2.0')
textblock=[ CHARACTER(LEN=128) :: &
'apache-2.0',&
'          ',&
'          ',&
'                                 Apache License',&
'                           Version 2.0, January 2004',&
'                        http://www.apache.org/licenses/',&
'                                                       ',&
'   TERMS AND CONDITIONS FOR USE, REPRODUCTION, AND DISTRIBUTION',&
'                                                               ',&
'   1. Definitions.                                             ',&
'                                                               ',&
'      "License" shall mean the terms and conditions for use, reproduction,',&
'      and distribution as defined by Sections 1 through 9 of this document.',&
'                                                                           ',&
'      "Licensor" shall mean the copyright owner or entity authorized by    ',&
'      the copyright owner that is granting the License.                    ',&
'                                                                           ',&
'      "Legal Entity" shall mean the union of the acting entity and all     ',&
'      other entities that control, are controlled by, or are under common  ',&
'      control with that entity. For the purposes of this definition,       ',&
'      "control" means (i) the power, direct or indirect, to cause the      ',&
'      direction or management of such entity, whether by contract or       ',&
'      otherwise, or (ii) ownership of fifty percent (50%) or more of the   ',&
'      outstanding shares, or (iii) beneficial ownership of such entity.    ',&
'                                                                           ',&
'      "You" (or "Your") shall mean an individual or Legal Entity           ',&
'      exercising permissions granted by this License.                      ',&
'                                                                           ',&
'      "Source" form shall mean the preferred form for making modifications,',&
'      including but not limited to software source code, documentation     ',&
'      source, and configuration files.                                     ',&
'                                                                           ',&
'      "Object" form shall mean any form resulting from mechanical          ',&
'      transformation or translation of a Source form, including but        ',&
'      not limited to compiled object code, generated documentation,        ',&
'      and conversions to other media types.                                ',&
'                                                                           ',&
'      "Work" shall mean the work of authorship, whether in Source or       ',&
'      Object form, made available under the License, as indicated by a     ',&
'      copyright notice that is included in or attached to the work         ',&
'      (an example is provided in the Appendix below).                      ',&
'                                                                           ',&
'      "Derivative Works" shall mean any work, whether in Source or Object  ',&
'      form, that is based on (or derived from) the Work and for which the  ',&
'      editorial revisions, annotations, elaborations, or other modifications',&
'      represent, as a whole, an original work of authorship. For the purposes',&
'      of this License, Derivative Works shall not include works that remain  ',&
'      separable from, or merely link (or bind by name) to the interfaces of, ',&
'      the Work and Derivative Works thereof.                                 ',&
'                                                                             ',&
'      "Contribution" shall mean any work of authorship, including            ',&
'      the original version of the Work and any modifications or additions    ',&
'      to that Work or Derivative Works thereof, that is intentionally        ',&
'      submitted to Licensor for inclusion in the Work by the copyright owner ',&
'      or by an individual or Legal Entity authorized to submit on behalf of  ',&
'      the copyright owner. For the purposes of this definition, "submitted"  ',&
'      means any form of electronic, verbal, or written communication sent    ',&
'      to the Licensor or its representatives, including but not limited to   ',&
'      communication on electronic mailing lists, source code control systems,',&
'      and issue tracking systems that are managed by, or on behalf of, the   ',&
'      Licensor for the purpose of discussing and improving the Work, but     ',&
'      excluding communication that is conspicuously marked or otherwise      ',&
'      designated in writing by the copyright owner as "Not a Contribution."  ',&
'                                                                             ',&
'      "Contributor" shall mean Licensor and any individual or Legal Entity   ',&
'      on behalf of whom a Contribution has been received by Licensor and     ',&
'      subsequently incorporated within the Work.                             ',&
'                                                                             ',&
'   2. Grant of Copyright License. Subject to the terms and conditions of     ',&
'      this License, each Contributor hereby grants to You a perpetual,       ',&
'      worldwide, non-exclusive, no-charge, royalty-free, irrevocable         ',&
'      copyright license to reproduce, prepare Derivative Works of,           ',&
'      publicly display, publicly perform, sublicense, and distribute the     ',&
'      Work and such Derivative Works in Source or Object form.               ',&
'                                                                             ',&
'   3. Grant of Patent License. Subject to the terms and conditions of        ',&
'      this License, each Contributor hereby grants to You a perpetual,       ',&
'      worldwide, non-exclusive, no-charge, royalty-free, irrevocable         ',&
'      (except as stated in this section) patent license to make, have made,  ',&
'      use, offer to sell, sell, import, and otherwise transfer the Work,     ',&
'      where such license applies only to those patent claims licensable      ',&
'      by such Contributor that are necessarily infringed by their            ',&
'      Contribution(s) alone or by combination of their Contribution(s)       ',&
'      with the Work to which such Contribution(s) was submitted. If You      ',&
'      institute patent litigation against any entity (including a            ',&
'      cross-claim or counterclaim in a lawsuit) alleging that the Work       ',&
'      or a Contribution incorporated within the Work constitutes direct      ',&
'      or contributory patent infringement, then any patent licenses          ',&
'      granted to You under this License for that Work shall terminate        ',&
'      as of the date such litigation is filed.                               ',&
'                                                                             ',&
'   4. Redistribution. You may reproduce and distribute copies of the         ',&
'      Work or Derivative Works thereof in any medium, with or without        ',&
'      modifications, and in Source or Object form, provided that You         ',&
'      meet the following conditions:                                         ',&
'                                                                             ',&
'      (a) You must give any other recipients of the Work or                  ',&
'          Derivative Works a copy of this License; and                       ',&
'                                                                             ',&
'      (b) You must cause any modified files to carry prominent notices       ',&
'          stating that You changed the files; and                            ',&
'                                                                             ',&
'      (c) You must retain, in the Source form of any Derivative Works        ',&
'          that You distribute, all copyright, patent, trademark, and         ',&
'          attribution notices from the Source form of the Work,              ',&
'          excluding those notices that do not pertain to any part of         ',&
'          the Derivative Works; and                                          ',&
'                                                                             ',&
'      (d) If the Work includes a "NOTICE" text file as part of its           ',&
'          distribution, then any Derivative Works that You distribute must   ',&
'          include a readable copy of the attribution notices contained       ',&
'          within such NOTICE file, excluding those notices that do not       ',&
'          pertain to any part of the Derivative Works, in at least one       ',&
'          of the following places: within a NOTICE text file distributed     ',&
'          as part of the Derivative Works; within the Source form or         ',&
'          documentation, if provided along with the Derivative Works; or,    ',&
'          within a display generated by the Derivative Works, if and         ',&
'          wherever such third-party notices normally appear. The contents    ',&
'          of the NOTICE file are for informational purposes only and         ',&
'          do not modify the License. You may add Your own attribution        ',&
'          notices within Derivative Works that You distribute, alongside     ',&
'          or as an addendum to the NOTICE text from the Work, provided       ',&
'          that such additional attribution notices cannot be construed       ',&
'          as modifying the License.                                          ',&
'                                                                             ',&
'      You may add Your own copyright statement to Your modifications and     ',&
'      may provide additional or different license terms and conditions       ',&
'      for use, reproduction, or distribution of Your modifications, or       ',&
'      for any such Derivative Works as a whole, provided Your use,           ',&
'      reproduction, and distribution of the Work otherwise complies with     ',&
'      the conditions stated in this License.                                 ',&
'                                                                             ',&
'   5. Submission of Contributions. Unless You explicitly state otherwise,    ',&
'      any Contribution intentionally submitted for inclusion in the Work     ',&
'      by You to the Licensor shall be under the terms and conditions of      ',&
'      this License, without any additional terms or conditions.              ',&
'      Notwithstanding the above, nothing herein shall supersede or modify    ',&
'      the terms of any separate license agreement you may have executed      ',&
'      with Licensor regarding such Contributions.                            ',&
'                                                                             ',&
'   6. Trademarks. This License does not grant permission to use the trade    ',&
'      names, trademarks, service marks, or product names of the Licensor,    ',&
'      except as required for reasonable and customary use in describing the  ',&
'      origin of the Work and reproducing the content of the NOTICE file.     ',&
'                                                                             ',&
'   7. Disclaimer of Warranty. Unless required by applicable law or           ',&
'      agreed to in writing, Licensor provides the Work (and each             ',&
'      Contributor provides its Contributions) on an "AS IS" BASIS,           ',&
'      WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or        ',&
'      implied, including, without limitation, any warranties or conditions   ',&
'      of TITLE, NON-INFRINGEMENT, MERCHANTABILITY, or FITNESS FOR A          ',&
'      PARTICULAR PURPOSE. You are solely responsible for determining the     ',&
'      appropriateness of using or redistributing the Work and assume any     ',&
'      risks associated with Your exercise of permissions under this License. ',&
'                                                                             ',&
'   8. Limitation of Liability. In no event and under no legal theory,        ',&
'      whether in tort (including negligence), contract, or otherwise,        ',&
'      unless required by applicable law (such as deliberate and grossly      ',&
'      negligent acts) or agreed to in writing, shall any Contributor be      ',&
'      liable to You for damages, including any direct, indirect, special,    ',&
'      incidental, or consequential damages of any character arising as a     ',&
'      result of this License or out of the use or inability to use the       ',&
'      Work (including but not limited to damages for loss of goodwill,       ',&
'      work stoppage, computer failure or malfunction, or any and all         ',&
'      other commercial damages or losses), even if such Contributor          ',&
'      has been advised of the possibility of such damages.                   ',&
'                                                                             ',&
'   9. Accepting Warranty or Additional Liability. While redistributing       ',&
'      the Work or Derivative Works thereof, You may choose to offer,         ',&
'      and charge a fee for, acceptance of support, warranty, indemnity,      ',&
'      or other liability obligations and/or rights consistent with this      ',&
'      License. However, in accepting such obligations, You may act only      ',&
'      on Your own behalf and on Your sole responsibility, not on behalf      ',&
'      of any other Contributor, and only if You agree to indemnify,          ',&
'      defend, and hold each Contributor harmless for any liability           ',&
'      incurred by, or claims asserted against, such Contributor by reason    ',&
'      of your accepting any such warranty or additional liability.           ',&
'                                                                             ',&
'   END OF TERMS AND CONDITIONS                                               ',&
'                                                                             ',&
'   APPENDIX: How to apply the Apache License to your work.                   ',&
'                                                                             ',&
'      To apply the Apache License to your work, attach the following         ',&
'      boilerplate notice, with the fields enclosed by at signs "@"           ',&
'      replaced with your own identifying information. (Don''t include        ',&
'      the at signs!)  The text should be enclosed in the appropriate         ',&
'      comment syntax for the file format. We also recommend that a           ',&
'      file or class name and description of purpose be included on the       ',&
'      same "printed page" as the copyright notice for easier                 ',&
'      identification within third-party archives.                            ',&
'                                                                             ',&
'   Copyright @YEAR@ @name of copyright owner@                                ',&
'                                                                             ',&
'   Licensed under the Apache License, Version 2.0 (the "License");           ',&
'   you may not use this file except in compliance with the License.          ',&
'   You may obtain a copy of the License at                                   ',&
'                                                                             ',&
'       http://www.apache.org/licenses/LICENSE-2.0                            ',&
'                                                                             ',&
'   Unless required by applicable law or agreed to in writing, software       ',&
'   distributed under the License is distributed on an "AS IS" BASIS,         ',&
'   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.  ',&
'   See the License for the specific language governing permissions and       ',&
'   limitations under the License.                                            ',&
'                                                                             ',&
'']
if(present(topic_only))then
   if(size(textblock)>0)then
      if(topic_only) textblock=[character(len=len(textblock(1))) :: textblock(1)]
   else
      textblock=['']
   endif
endif
case('5','artistic-2.0')
textblock=[ CHARACTER(LEN=128) :: &
'artistic-2.0',&
'            ',&
'                       The Artistic License 2.0',&
'                                               ',&
'            Copyright (c) 2000-2006, The Perl Foundation.',&
'                                                         ',&
'     Everyone is permitted to copy and distribute verbatim copies',&
'      of this license document, but changing it is not allowed.  ',&
'                                                                 ',&
'Preamble                                                         ',&
'                                                                 ',&
'This license establishes the terms under which a given free software',&
'Package may be copied, modified, distributed, and/or redistributed. ',&
'The intent is that the Copyright Holder maintains some artistic     ',&
'control over the development of that Package while still keeping the',&
'Package available as open source and free software.                 ',&
'                                                                    ',&
'You are always permitted to make arrangements wholly outside of this',&
'license directly with the Copyright Holder of a given Package.  If the',&
'terms of this license do not permit the full use that you propose to  ',&
'make of the Package, you should contact the Copyright Holder and seek ',&
'a different licensing arrangement.                                    ',&
'                                                                      ',&
'Definitions                                                           ',&
'                                                                      ',&
'    "Copyright Holder" means the individual(s) or organization(s)     ',&
'    named in the copyright notice for the entire Package.             ',&
'                                                                      ',&
'    "Contributor" means any party that has contributed code or other  ',&
'    material to the Package, in accordance with the Copyright Holder''s',&
'    procedures.                                                        ',&
'                                                                       ',&
'    "You" and "your" means any person who would like to copy,          ',&
'    distribute, or modify the Package.                                 ',&
'                                                                       ',&
'    "Package" means the collection of files distributed by the         ',&
'    Copyright Holder, and derivatives of that collection and/or of     ',&
'    those files. A given Package may consist of either the Standard    ',&
'    Version, or a Modified Version.                                    ',&
'                                                                       ',&
'    "Distribute" means providing a copy of the Package or making it    ',&
'    accessible to anyone else, or in the case of a company or          ',&
'    organization, to others outside of your company or organization.   ',&
'                                                                       ',&
'    "Distributor Fee" means any fee that you charge for Distributing   ',&
'    this Package or providing support for this Package to another      ',&
'    party.  It does not mean licensing fees.                           ',&
'                                                                       ',&
'    "Standard Version" refers to the Package if it has not been        ',&
'    modified, or has been modified only in ways explicitly requested   ',&
'    by the Copyright Holder.                                           ',&
'                                                                       ',&
'    "Modified Version" means the Package, if it has been changed, and  ',&
'    such changes were not explicitly requested by the Copyright        ',&
'    Holder.                                                            ',&
'                                                                       ',&
'    "Original License" means this Artistic License as Distributed with ',&
'    the Standard Version of the Package, in its current version or as  ',&
'    it may be modified by The Perl Foundation in the future.           ',&
'                                                                       ',&
'    "Source" form means the source code, documentation source, and     ',&
'    configuration files for the Package.                               ',&
'                                                                       ',&
'    "Compiled" form means the compiled bytecode, object code, binary,  ',&
'    or any other form resulting from mechanical transformation or      ',&
'    translation of the Source form.                                    ',&
'                                                                       ',&
'                                                                       ',&
'Permission for Use and Modification Without Distribution               ',&
'                                                                       ',&
'(1)  You are permitted to use the Standard Version and create and use  ',&
'Modified Versions for any purpose without restriction, provided that   ',&
'you do not Distribute the Modified Version.                            ',&
'                                                                       ',&
'                                                                       ',&
'Permissions for Redistribution of the Standard Version                 ',&
'                                                                       ',&
'(2)  You may Distribute verbatim copies of the Source form of the      ',&
'Standard Version of this Package in any medium without restriction,    ',&
'either gratis or for a Distributor Fee, provided that you duplicate    ',&
'all of the original copyright notices and associated disclaimers.  At  ',&
'your discretion, such verbatim copies may or may not include a         ',&
'Compiled form of the Package.                                          ',&
'                                                                       ',&
'(3)  You may apply any bug fixes, portability changes, and other       ',&
'modifications made available from the Copyright Holder.  The resulting ',&
'Package will still be considered the Standard Version, and as such     ',&
'will be subject to the Original License.                               ',&
'                                                                       ',&
'                                                                       ',&
'Distribution of Modified Versions of the Package as Source             ',&
'                                                                       ',&
'(4)  You may Distribute your Modified Version as Source (either gratis ',&
'or for a Distributor Fee, and with or without a Compiled form of the   ',&
'Modified Version) provided that you clearly document how it differs    ',&
'from the Standard Version, including, but not limited to, documenting  ',&
'any non-standard features, executables, or modules, and provided that  ',&
'you do at least ONE of the following:                                  ',&
'                                                                       ',&
'    (a)  make the Modified Version available to the Copyright Holder   ',&
'    of the Standard Version, under the Original License, so that the   ',&
'    Copyright Holder may include your modifications in the Standard    ',&
'    Version.                                                           ',&
'                                                                       ',&
'    (b)  ensure that installation of your Modified Version does not    ',&
'    prevent the user installing or running the Standard Version. In    ',&
'    addition, the Modified Version must bear a name that is different  ',&
'    from the name of the Standard Version.                             ',&
'                                                                       ',&
'    (c)  allow anyone who receives a copy of the Modified Version to   ',&
'    make the Source form of the Modified Version available to others   ',&
'    under                                                              ',&
'                                                                       ',&
'        (i)  the Original License or                                   ',&
'                                                                       ',&
'        (ii)  a license that permits the licensee to freely copy,      ',&
'        modify and redistribute the Modified Version using the same    ',&
'        licensing terms that apply to the copy that the licensee       ',&
'        received, and requires that the Source form of the Modified    ',&
'        Version, and of any works derived from it, be made freely      ',&
'        available in that license fees are prohibited but Distributor  ',&
'        Fees are allowed.                                              ',&
'                                                                       ',&
'                                                                       ',&
'Distribution of Compiled Forms of the Standard Version                 ',&
'or Modified Versions without the Source                                ',&
'                                                                       ',&
'(5)  You may Distribute Compiled forms of the Standard Version without ',&
'the Source, provided that you include complete instructions on how to  ',&
'get the Source of the Standard Version.  Such instructions must be     ',&
'valid at the time of your distribution.  If these instructions, at any ',&
'time while you are carrying out such distribution, become invalid, you ',&
'must provide new instructions on demand or cease further distribution. ',&
'If you provide valid instructions or cease distribution within thirty  ',&
'days after you become aware that the instructions are invalid, then    ',&
'you do not forfeit any of your rights under this license.              ',&
'                                                                       ',&
'(6)  You may Distribute a Modified Version in Compiled form without    ',&
'the Source, provided that you comply with Section 4 with respect to    ',&
'the Source of the Modified Version.                                    ',&
'                                                                       ',&
'                                                                       ',&
'Aggregating or Linking the Package                                     ',&
'                                                                       ',&
'(7)  You may aggregate the Package (either the Standard Version or     ',&
'Modified Version) with other packages and Distribute the resulting     ',&
'aggregation provided that you do not charge a licensing fee for the    ',&
'Package.  Distributor Fees are permitted, and licensing fees for other ',&
'components in the aggregation are permitted. The terms of this license ',&
'apply to the use and Distribution of the Standard or Modified Versions ',&
'as included in the aggregation.                                        ',&
'                                                                       ',&
'(8) You are permitted to link Modified and Standard Versions with      ',&
'other works, to embed the Package in a larger work of your own, or to  ',&
'build stand-alone binary or bytecode versions of applications that     ',&
'include the Package, and Distribute the result without restriction,    ',&
'provided the result does not expose a direct interface to the Package. ',&
'                                                                       ',&
'                                                                       ',&
'Items That are Not Considered Part of a Modified Version               ',&
'                                                                       ',&
'(9) Works (including, but not limited to, modules and scripts) that    ',&
'merely extend or make use of the Package, do not, by themselves, cause ',&
'the Package to be a Modified Version.  In addition, such works are not ',&
'considered parts of the Package itself, and are not subject to the     ',&
'terms of this license.                                                 ',&
'                                                                       ',&
'                                                                       ',&
'General Provisions                                                     ',&
'                                                                       ',&
'(10)  Any use, modification, and distribution of the Standard or       ',&
'Modified Versions is governed by this Artistic License. By using,      ',&
'modifying or distributing the Package, you accept this license. Do not ',&
'use, modify, or distribute the Package, if you do not accept this      ',&
'license.                                                               ',&
'                                                                       ',&
'(11)  If your Modified Version has been derived from a Modified        ',&
'Version made by someone other than you, you are nevertheless required  ',&
'to ensure that your Modified Version complies with the requirements of ',&
'this license.                                                          ',&
'                                                                       ',&
'(12)  This license does not grant you the right to use any trademark,  ',&
'service mark, tradename, or logo of the Copyright Holder.              ',&
'                                                                       ',&
'(13)  This license includes the non-exclusive, worldwide,              ',&
'free-of-charge patent license to make, have made, use, offer to sell,  ',&
'sell, import and otherwise transfer the Package with respect to any    ',&
'patent claims licensable by the Copyright Holder that are necessarily  ',&
'infringed by the Package. If you institute patent litigation           ',&
'(including a cross-claim or counterclaim) against any party alleging   ',&
'that the Package constitutes direct or contributory patent             ',&
'infringement, then this Artistic License to you shall terminate on the ',&
'date that such litigation is filed.                                    ',&
'                                                                       ',&
'(14)  Disclaimer of Warranty:                                          ',&
'THE PACKAGE IS PROVIDED BY THE COPYRIGHT HOLDER AND CONTRIBUTORS "AS   ',&
'IS'' AND WITHOUT ANY EXPRESS OR IMPLIED WARRANTIES. THE IMPLIED        ',&
'WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE, OR    ',&
'NON-INFRINGEMENT ARE DISCLAIMED TO THE EXTENT PERMITTED BY YOUR LOCAL  ',&
'LAW. UNLESS REQUIRED BY LAW, NO COPYRIGHT HOLDER OR CONTRIBUTOR WILL   ',&
'BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, OR CONSEQUENTIAL       ',&
'DAMAGES ARISING IN ANY WAY OUT OF THE USE OF THE PACKAGE, EVEN IF      ',&
'ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.                             ',&
'                                                                       ',&
'']
if(present(topic_only))then
   if(size(textblock)>0)then
      if(topic_only) textblock=[character(len=len(textblock(1))) :: textblock(1)]
   else
      textblock=['']
   endif
endif
case('6','blueoak-1.0.0')
textblock=[ CHARACTER(LEN=128) :: &
'blueoak-1.0.0',&
'             ',&
'# Blue Oak Model License',&
'                        ',&
'Version 1.0.0           ',&
'                        ',&
'## Purpose              ',&
'                        ',&
'This license gives everyone as much permission to work with',&
'this software as possible, while protecting contributors   ',&
'from liability.                                            ',&
'                                                           ',&
'## Acceptance                                              ',&
'                                                           ',&
'In order to receive this license, you must agree to its    ',&
'rules.  The rules of this license are both obligations     ',&
'under that agreement and conditions to your license.       ',&
'You must not do anything with this software that triggers  ',&
'a rule that you cannot or will not follow.                 ',&
'                                                           ',&
'## Copyright                                               ',&
'                                                           ',&
'Each contributor licenses you to do everything with this   ',&
'software that would otherwise infringe that contributor''s ',&
'copyright in it.                                           ',&
'                                                           ',&
'## Notices                                                 ',&
'                                                           ',&
'You must ensure that everyone who gets a copy of           ',&
'any part of this software from you, with or without        ',&
'changes, also gets the text of this license or a link to   ',&
'<https://blueoakcouncil.org/license/1.0.0>.                ',&
'                                                           ',&
'## Excuse                                                  ',&
'                                                           ',&
'If anyone notifies you in writing that you have not        ',&
'complied with [Notices](#notices), you can keep your       ',&
'license by taking all practical steps to comply within 30  ',&
'days after the notice.  If you do not do so, your license  ',&
'ends immediately.                                          ',&
'                                                           ',&
'## Patent                                                  ',&
'                                                           ',&
'Each contributor licenses you to do everything with this   ',&
'software that would otherwise infringe any patent claims   ',&
'they can license or become able to license.                ',&
'                                                           ',&
'## Reliability                                             ',&
'                                                           ',&
'No contributor can revoke this license.                    ',&
'                                                           ',&
'## No Liability                                            ',&
'                                                           ',&
'***As far as the law allows, this software comes as is,    ',&
'without any warranty or condition, and no contributor      ',&
'will be liable to anyone for any damages related to this   ',&
'software or this license, under any kind of legal claim.***',&
'                                                           ',&
'']
if(present(topic_only))then
   if(size(textblock)>0)then
      if(topic_only) textblock=[character(len=len(textblock(1))) :: textblock(1)]
   else
      textblock=['']
   endif
endif
case('7','bsd-2-clause')
textblock=[ CHARACTER(LEN=128) :: &
'bsd-2-clause',&
'            ',&
'BSD 2-Clause License',&
'                    ',&
'Copyright (c) @YEAR@, @FULLNAME@',&
'                                ',&
'Redistribution and use in source and binary forms, with or without',&
'modification, are permitted provided that the following conditions are met:',&
'                                                                           ',&
'1. Redistributions of source code must retain the above copyright notice, this',&
'   list of conditions and the following disclaimer.                           ',&
'                                                                              ',&
'2. Redistributions in binary form must reproduce the above copyright notice,  ',&
'   this list of conditions and the following disclaimer in the documentation  ',&
'   and/or other materials provided with the distribution.                     ',&
'                                                                              ',&
'THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"   ',&
'AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE     ',&
'IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE',&
'DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE  ',&
'FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL    ',&
'DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR    ',&
'SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER    ',&
'CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, ',&
'OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE ',&
'OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.          ',&
'                                                                              ',&
'']
if(present(topic_only))then
   if(size(textblock)>0)then
      if(topic_only) textblock=[character(len=len(textblock(1))) :: textblock(1)]
   else
      textblock=['']
   endif
endif
case('8','bsd-2-clause-patent')
textblock=[ CHARACTER(LEN=128) :: &
'bsd-2-clause-patent',&
'                   ',&
'Copyright (c) @YEAR@ @FULLNAME@',&
'                               ',&
'Redistribution and use in source and binary forms, with or without',&
'modification, are permitted provided that the following conditions are met:',&
'                                                                           ',&
'1. Redistributions of source code must retain the above copyright notice,  ',&
'this list of conditions and the following disclaimer.                      ',&
'                                                                           ',&
'2. Redistributions in binary form must reproduce the above copyright notice,',&
'this list of conditions and the following disclaimer in the documentation   ',&
'and/or other materials provided with the distribution.                      ',&
'                                                                            ',&
'Subject to the terms and conditions of this license, each copyright holder  ',&
'and contributor hereby grants to those receiving rights under this license  ',&
'a perpetual, worldwide, non-exclusive, no-charge, royalty-free, irrevocable ',&
'(except for failure to satisfy the conditions of this license) patent license',&
'to make, have made, use, offer to sell, sell, import, and otherwise transfer ',&
'this software, where such license applies only to those patent claims, already',&
'acquired or hereafter acquired, licensable by such copyright holder or        ',&
'contributor that are necessarily infringed by:                                ',&
'                                                                              ',&
'(a) their Contribution(s) (the licensed copyrights of copyright holders and   ',&
'non-copyrightable additions of contributors, in source or binary form) alone; ',&
'or                                                                            ',&
'                                                                              ',&
'(b) combination of their Contribution(s) with the work of authorship to which ',&
'such Contribution(s) was added by such copyright holder or contributor, if,   ',&
'at the time the Contribution is added, such addition causes such combination  ',&
'to be necessarily infringed. The patent license shall not apply to any other  ',&
'combinations which include the Contribution.                                  ',&
'                                                                              ',&
'Except as expressly stated above, no rights or licenses from any copyright    ',&
'holder or contributor is granted under this license, whether expressly, by    ',&
'implication, estoppel or otherwise.                                           ',&
'                                                                              ',&
'DISCLAIMER                                                                    ',&
'                                                                              ',&
'THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"   ',&
'AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE     ',&
'IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE',&
'DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDERS OR CONTRIBUTORS BE LIABLE ',&
'FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL    ',&
'DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR    ',&
'SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER    ',&
'CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, ',&
'OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE ',&
'OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.          ',&
'                                                                              ',&
'']
if(present(topic_only))then
   if(size(textblock)>0)then
      if(topic_only) textblock=[character(len=len(textblock(1))) :: textblock(1)]
   else
      textblock=['']
   endif
endif
case('9','bsd-3-clause-clear')
textblock=[ CHARACTER(LEN=128) :: &
'bsd-3-clause-clear',&
'                  ',&
'The Clear BSD License',&
'                     ',&
'Copyright (c) @YEAR@ @FULLNAME@',&
'All rights reserved.           ',&
'                               ',&
'Redistribution and use in source and binary forms, with or without',&
'modification, are permitted (subject to the limitations in the disclaimer',&
'below) provided that the following conditions are met:                   ',&
'                                                                         ',&
'     * Redistributions of source code must retain the above copyright notice,',&
'     this list of conditions and the following disclaimer.                   ',&
'                                                                             ',&
'     * Redistributions in binary form must reproduce the above copyright     ',&
'     notice, this list of conditions and the following disclaimer in the     ',&
'     documentation and/or other materials provided with the distribution.    ',&
'                                                                             ',&
'     * Neither the name of the copyright holder nor the names of its         ',&
'     contributors may be used to endorse or promote products derived from this',&
'     software without specific prior written permission.                      ',&
'                                                                              ',&
'NO EXPRESS OR IMPLIED LICENSES TO ANY PARTY''S PATENT RIGHTS ARE GRANTED BY   ',&
'THIS LICENSE. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND          ',&
'CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT',&
'LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A       ',&
'PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR  ',&
'CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,         ',&
'EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,           ',&
'PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR',&
'BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER ',&
'IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)    ',&
'ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE    ',&
'POSSIBILITY OF SUCH DAMAGE.                                                   ',&
'                                                                              ',&
'']
if(present(topic_only))then
   if(size(textblock)>0)then
      if(topic_only) textblock=[character(len=len(textblock(1))) :: textblock(1)]
   else
      textblock=['']
   endif
endif
case('10','bsd-3-clause')
textblock=[ CHARACTER(LEN=128) :: &
'bsd-3-clause',&
'            ',&
'BSD 3-Clause License',&
'                    ',&
'Copyright (c) @YEAR@, @FULLNAME@',&
'                                ',&
'Redistribution and use in source and binary forms, with or without',&
'modification, are permitted provided that the following conditions are met:',&
'                                                                           ',&
'1. Redistributions of source code must retain the above copyright notice, this',&
'   list of conditions and the following disclaimer.                           ',&
'                                                                              ',&
'2. Redistributions in binary form must reproduce the above copyright notice,  ',&
'   this list of conditions and the following disclaimer in the documentation  ',&
'   and/or other materials provided with the distribution.                     ',&
'                                                                              ',&
'3. Neither the name of the copyright holder nor the names of its              ',&
'   contributors may be used to endorse or promote products derived from       ',&
'   this software without specific prior written permission.                   ',&
'                                                                              ',&
'THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"   ',&
'AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE     ',&
'IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE',&
'DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE  ',&
'FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL    ',&
'DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR    ',&
'SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER    ',&
'CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, ',&
'OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE ',&
'OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.          ',&
'                                                                              ',&
'']
if(present(topic_only))then
   if(size(textblock)>0)then
      if(topic_only) textblock=[character(len=len(textblock(1))) :: textblock(1)]
   else
      textblock=['']
   endif
endif
case('11','bsd-4-clause')
textblock=[ CHARACTER(LEN=128) :: &
'bsd-4-clause',&
'            ',&
'BSD 4-Clause License',&
'                    ',&
'Copyright (c) @YEAR@, @FULLNAME@',&
'All rights reserved.            ',&
'                                ',&
'Redistribution and use in source and binary forms, with or without',&
'modification, are permitted provided that the following conditions are met:',&
'                                                                           ',&
'1. Redistributions of source code must retain the above copyright notice, this',&
'   list of conditions and the following disclaimer.                           ',&
'                                                                              ',&
'2. Redistributions in binary form must reproduce the above copyright notice,  ',&
'   this list of conditions and the following disclaimer in the documentation  ',&
'   and/or other materials provided with the distribution.                     ',&
'                                                                              ',&
'3. All advertising materials mentioning features or use of this software must ',&
'   display the following acknowledgement:                                     ',&
'     This product includes software developed by @PROJECT@.                   ',&
'                                                                              ',&
'4. Neither the name of the copyright holder nor the names of its              ',&
'   contributors may be used to endorse or promote products derived from       ',&
'   this software without specific prior written permission.                   ',&
'                                                                              ',&
'THIS SOFTWARE IS PROVIDED BY COPYRIGHT HOLDER "AS IS" AND ANY EXPRESS OR      ',&
'IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF  ',&
'MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO    ',&
'EVENT SHALL COPYRIGHT HOLDER BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,  ',&
'SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,  ',&
'PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS;   ',&
'OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,      ',&
'WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR       ',&
'OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF        ',&
'ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.                                    ',&
'                                                                              ',&
'']
if(present(topic_only))then
   if(size(textblock)>0)then
      if(topic_only) textblock=[character(len=len(textblock(1))) :: textblock(1)]
   else
      textblock=['']
   endif
endif
case('12','bsl-1.0')
textblock=[ CHARACTER(LEN=128) :: &
'bsl-1.0',&
'       ',&
'Boost Software License - Version 1.0 - August 17th, 2003',&
'                                                        ',&
'Permission is hereby granted, free of charge, to any person or organization',&
'obtaining a copy of the software and accompanying documentation covered by ',&
'this license (the "Software") to use, reproduce, display, distribute,      ',&
'execute, and transmit the Software, and to prepare derivative works of the ',&
'Software, and to permit third-parties to whom the Software is furnished to ',&
'do so, all subject to the following:                                       ',&
'                                                                           ',&
'The copyright notices in the Software and this entire statement, including ',&
'the above license grant, this restriction and the following disclaimer,    ',&
'must be included in all copies of the Software, in whole or in part, and   ',&
'all derivative works of the Software, unless such copies or derivative     ',&
'works are solely in the form of machine-executable object code generated by',&
'a source language processor.                                               ',&
'                                                                           ',&
'THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR ',&
'IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,   ',&
'FITNESS FOR A PARTICULAR PURPOSE, TITLE AND NON-INFRINGEMENT. IN NO EVENT  ',&
'SHALL THE COPYRIGHT HOLDERS OR ANYONE DISTRIBUTING THE SOFTWARE BE LIABLE  ',&
'FOR ANY DAMAGES OR OTHER LIABILITY, WHETHER IN CONTRACT, TORT OR OTHERWISE,',&
'ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER',&
'DEALINGS IN THE SOFTWARE.                                                  ',&
'                                                                           ',&
'']
if(present(topic_only))then
   if(size(textblock)>0)then
      if(topic_only) textblock=[character(len=len(textblock(1))) :: textblock(1)]
   else
      textblock=['']
   endif
endif
case('13','cc0-1.0')
textblock=[ CHARACTER(LEN=128) :: &
'cc0-1.0',&
'       ',&
'Creative Commons Legal Code',&
'                           ',&
'CC0 1.0 Universal          ',&
'                           ',&
'    CREATIVE COMMONS CORPORATION IS NOT A LAW FIRM AND DOES NOT PROVIDE',&
'    LEGAL SERVICES. DISTRIBUTION OF THIS DOCUMENT DOES NOT CREATE AN   ',&
'    ATTORNEY-CLIENT RELATIONSHIP. CREATIVE COMMONS PROVIDES THIS       ',&
'    INFORMATION ON AN "AS-IS" BASIS. CREATIVE COMMONS MAKES NO WARRANTIES',&
'    REGARDING THE USE OF THIS DOCUMENT OR THE INFORMATION OR WORKS       ',&
'    PROVIDED HEREUNDER, AND DISCLAIMS LIABILITY FOR DAMAGES RESULTING FROM',&
'    THE USE OF THIS DOCUMENT OR THE INFORMATION OR WORKS PROVIDED         ',&
'    HEREUNDER.                                                            ',&
'                                                                          ',&
'Statement of Purpose                                                      ',&
'                                                                          ',&
'The laws of most jurisdictions throughout the world automatically confer  ',&
'exclusive Copyright and Related Rights (defined below) upon the creator   ',&
'and subsequent owner(s) (each and all, an "owner") of an original work of ',&
'authorship and/or a database (each, a "Work").                            ',&
'                                                                          ',&
'Certain owners wish to permanently relinquish those rights to a Work for  ',&
'the purpose of contributing to a commons of creative, cultural and        ',&
'scientific works ("Commons") that the public can reliably and without fear',&
'of later claims of infringement build upon, modify, incorporate in other  ',&
'works, reuse and redistribute as freely as possible in any form whatsoever',&
'and for any purposes, including without limitation commercial purposes.   ',&
'These owners may contribute to the Commons to promote the ideal of a free ',&
'culture and the further production of creative, cultural and scientific   ',&
'works, or to gain reputation or greater distribution for their Work in    ',&
'part through the use and efforts of others.                               ',&
'                                                                          ',&
'For these and/or other purposes and motivations, and without any          ',&
'expectation of additional consideration or compensation, the person       ',&
'associating CC0 with a Work (the "Affirmer"), to the extent that he or she',&
'is an owner of Copyright and Related Rights in the Work, voluntarily      ',&
'elects to apply CC0 to the Work and publicly distribute the Work under its',&
'terms, with knowledge of his or her Copyright and Related Rights in the   ',&
'Work and the meaning and intended legal effect of CC0 on those rights.    ',&
'                                                                          ',&
'1. Copyright and Related Rights. A Work made available under CC0 may be   ',&
'protected by copyright and related or neighboring rights ("Copyright and  ',&
'Related Rights"). Copyright and Related Rights include, but are not       ',&
'limited to, the following:                                                ',&
'                                                                          ',&
'  i. the right to reproduce, adapt, distribute, perform, display,         ',&
'     communicate, and translate a Work;                                   ',&
' ii. moral rights retained by the original author(s) and/or performer(s); ',&
'iii. publicity and privacy rights pertaining to a person''s image or      ',&
'     likeness depicted in a Work;                                         ',&
' iv. rights protecting against unfair competition in regards to a Work,   ',&
'     subject to the limitations in paragraph 4(a), below;                 ',&
'  v. rights protecting the extraction, dissemination, use and reuse of data',&
'     in a Work;                                                            ',&
' vi. database rights (such as those arising under Directive 96/9/EC of the ',&
'     European Parliament and of the Council of 11 March 1996 on the legal  ',&
'     protection of databases, and under any national implementation        ',&
'     thereof, including any amended or successor version of such           ',&
'     directive); and                                                       ',&
'vii. other similar, equivalent or corresponding rights throughout the      ',&
'     world based on applicable law or treaty, and any national             ',&
'     implementations thereof.                                              ',&
'                                                                           ',&
'2. Waiver. To the greatest extent permitted by, but not in contravention   ',&
'of, applicable law, Affirmer hereby overtly, fully, permanently,           ',&
'irrevocably and unconditionally waives, abandons, and surrenders all of    ',&
'Affirmer''s Copyright and Related Rights and associated claims and causes  ',&
'of action, whether now known or unknown (including existing as well as     ',&
'future claims and causes of action), in the Work (i) in all territories    ',&
'worldwide, (ii) for the maximum duration provided by applicable law or     ',&
'treaty (including future time extensions), (iii) in any current or future  ',&
'medium and for any number of copies, and (iv) for any purpose whatsoever,  ',&
'including without limitation commercial, advertising or promotional        ',&
'purposes (the "Waiver"). Affirmer makes the Waiver for the benefit of each ',&
'member of the public at large and to the detriment of Affirmer''s heirs and',&
'successors, fully intending that such Waiver shall not be subject to       ',&
'revocation, rescission, cancellation, termination, or any other legal or   ',&
'equitable action to disrupt the quiet enjoyment of the Work by the public  ',&
'as contemplated by Affirmer''s express Statement of Purpose.               ',&
'                                                                           ',&
'3. Public License Fallback. Should any part of the Waiver for any reason   ',&
'be judged legally invalid or ineffective under applicable law, then the    ',&
'Waiver shall be preserved to the maximum extent permitted taking into      ',&
'account Affirmer''s express Statement of Purpose. In addition, to the      ',&
'extent the Waiver is so judged Affirmer hereby grants to each affected     ',&
'person a royalty-free, non transferable, non sublicensable, non exclusive, ',&
'irrevocable and unconditional license to exercise Affirmer''s Copyright and',&
'Related Rights in the Work (i) in all territories worldwide, (ii) for the  ',&
'maximum duration provided by applicable law or treaty (including future    ',&
'time extensions), (iii) in any current or future medium and for any number ',&
'of copies, and (iv) for any purpose whatsoever, including without          ',&
'limitation commercial, advertising or promotional purposes (the            ',&
'"License"). The License shall be deemed effective as of the date CC0 was   ',&
'applied by Affirmer to the Work. Should any part of the License for any    ',&
'reason be judged legally invalid or ineffective under applicable law, such ',&
'partial invalidity or ineffectiveness shall not invalidate the remainder   ',&
'of the License, and in such case Affirmer hereby affirms that he or she    ',&
'will not (i) exercise any of his or her remaining Copyright and Related    ',&
'Rights in the Work or (ii) assert any associated claims and causes of      ',&
'action with respect to the Work, in either case contrary to Affirmer''s    ',&
'express Statement of Purpose.                                              ',&
'                                                                           ',&
'4. Limitations and Disclaimers.                                            ',&
'                                                                           ',&
' a. No trademark or patent rights held by Affirmer are waived, abandoned,  ',&
'    surrendered, licensed or otherwise affected by this document.          ',&
' b. Affirmer offers the Work as-is and makes no representations or         ',&
'    warranties of any kind concerning the Work, express, implied,          ',&
'    statutory or otherwise, including without limitation warranties of     ',&
'    title, merchantability, fitness for a particular purpose, non          ',&
'    infringement, or the absence of latent or other defects, accuracy, or  ',&
'    the present or absence of errors, whether or not discoverable, all to  ',&
'    the greatest extent permissible under applicable law.                  ',&
' c. Affirmer disclaims responsibility for clearing rights of other persons ',&
'    that may apply to the Work or any use thereof, including without       ',&
'    limitation any person''s Copyright and Related Rights in the Work.     ',&
'    Further, Affirmer disclaims responsibility for obtaining any necessary ',&
'    consents, permissions or other rights required for any use of the      ',&
'    Work.                                                                  ',&
' d. Affirmer understands and acknowledges that Creative Commons is not a   ',&
'    party to this document and has no duty or obligation with respect to   ',&
'    this CC0 or use of the Work.                                           ',&
'                                                                           ',&
'']
if(present(topic_only))then
   if(size(textblock)>0)then
      if(topic_only) textblock=[character(len=len(textblock(1))) :: textblock(1)]
   else
      textblock=['']
   endif
endif
case('14','cc-by-4.0')
textblock=[ CHARACTER(LEN=128) :: &
'cc-by-4.0',&
'         ',&
'Attribution 4.0 International',&
'                             ',&
'=======================================================================',&
'                                                                       ',&
'Creative Commons Corporation ("Creative Commons") is not a law firm and',&
'does not provide legal services or legal advice. Distribution of       ',&
'Creative Commons public licenses does not create a lawyer-client or    ',&
'other relationship. Creative Commons makes its licenses and related    ',&
'information available on an "as-is" basis. Creative Commons gives no   ',&
'warranties regarding its licenses, any material licensed under their   ',&
'terms and conditions, or any related information. Creative Commons     ',&
'disclaims all liability for damages resulting from their use to the    ',&
'fullest extent possible.                                               ',&
'                                                                       ',&
'Using Creative Commons Public Licenses                                 ',&
'                                                                       ',&
'Creative Commons public licenses provide a standard set of terms and   ',&
'conditions that creators and other rights holders may use to share     ',&
'original works of authorship and other material subject to copyright   ',&
'and certain other rights specified in the public license below. The    ',&
'following considerations are for informational purposes only, are not  ',&
'exhaustive, and do not form part of our licenses.                      ',&
'                                                                       ',&
'     Considerations for licensors: Our public licenses are             ',&
'     intended for use by those authorized to give the public           ',&
'     permission to use material in ways otherwise restricted by        ',&
'     copyright and certain other rights. Our licenses are              ',&
'     irrevocable. Licensors should read and understand the terms       ',&
'     and conditions of the license they choose before applying it.     ',&
'     Licensors should also secure all rights necessary before          ',&
'     applying our licenses so that the public can reuse the            ',&
'     material as expected. Licensors should clearly mark any           ',&
'     material not subject to the license. This includes other CC-      ',&
'     licensed material, or material used under an exception or         ',&
'     limitation to copyright. More considerations for licensors:       ',&
'     wiki.creativecommons.org/Considerations_for_licensors             ',&
'                                                                       ',&
'     Considerations for the public: By using one of our public         ',&
'     licenses, a licensor grants the public permission to use the      ',&
'     licensed material under specified terms and conditions. If        ',&
'     the licensor''s permission is not necessary for any reason--for   ',&
'     example, because of any applicable exception or limitation to     ',&
'     copyright--then that use is not regulated by the license. Our     ',&
'     licenses grant only permissions under copyright and certain       ',&
'     other rights that a licensor has authority to grant. Use of       ',&
'     the licensed material may still be restricted for other           ',&
'     reasons, including because others have copyright or other         ',&
'     rights in the material. A licensor may make special requests,     ',&
'     such as asking that all changes be marked or described.           ',&
'     Although not required by our licenses, you are encouraged to      ',&
'     respect those requests where reasonable. More considerations      ',&
'     for the public:                                                   ',&
'     wiki.creativecommons.org/Considerations_for_licensees             ',&
'                                                                       ',&
'=======================================================================',&
'                                                                       ',&
'Creative Commons Attribution 4.0 International Public License          ',&
'                                                                       ',&
'By exercising the Licensed Rights (defined below), You accept and agree',&
'to be bound by the terms and conditions of this Creative Commons       ',&
'Attribution 4.0 International Public License ("Public License"). To the',&
'extent this Public License may be interpreted as a contract, You are   ',&
'granted the Licensed Rights in consideration of Your acceptance of     ',&
'these terms and conditions, and the Licensor grants You such rights in ',&
'consideration of benefits the Licensor receives from making the        ',&
'Licensed Material available under these terms and conditions.          ',&
'                                                                       ',&
'                                                                       ',&
'Section 1 -- Definitions.                                              ',&
'                                                                       ',&
'  a. Adapted Material means material subject to Copyright and Similar  ',&
'     Rights that is derived from or based upon the Licensed Material   ',&
'     and in which the Licensed Material is translated, altered,        ',&
'     arranged, transformed, or otherwise modified in a manner requiring',&
'     permission under the Copyright and Similar Rights held by the     ',&
'     Licensor. For purposes of this Public License, where the Licensed ',&
'     Material is a musical work, performance, or sound recording,      ',&
'     Adapted Material is always produced where the Licensed Material is',&
'     synched in timed relation with a moving image.                    ',&
'                                                                       ',&
'  b. Adapter''s License means the license You apply to Your Copyright  ',&
'     and Similar Rights in Your contributions to Adapted Material in   ',&
'     accordance with the terms and conditions of this Public License.  ',&
'                                                                       ',&
'  c. Copyright and Similar Rights means copyright and/or similar rights',&
'     closely related to copyright including, without limitation,       ',&
'     performance, broadcast, sound recording, and Sui Generis Database ',&
'     Rights, without regard to how the rights are labeled or           ',&
'     categorized. For purposes of this Public License, the rights      ',&
'     specified in Section 2(b)(1)-(2) are not Copyright and Similar    ',&
'     Rights.                                                           ',&
'                                                                       ',&
'  d. Effective Technological Measures means those measures that, in the',&
'     absence of proper authority, may not be circumvented under laws   ',&
'     fulfilling obligations under Article 11 of the WIPO Copyright     ',&
'     Treaty adopted on December 20, 1996, and/or similar international ',&
'     agreements.                                                       ',&
'                                                                       ',&
'  e. Exceptions and Limitations means fair use, fair dealing, and/or   ',&
'     any other exception or limitation to Copyright and Similar Rights ',&
'     that applies to Your use of the Licensed Material.                ',&
'                                                                       ',&
'  f. Licensed Material means the artistic or literary work, database,  ',&
'     or other material to which the Licensor applied this Public       ',&
'     License.                                                          ',&
'                                                                       ',&
'  g. Licensed Rights means the rights granted to You subject to the    ',&
'     terms and conditions of this Public License, which are limited to ',&
'     all Copyright and Similar Rights that apply to Your use of the    ',&
'     Licensed Material and that the Licensor has authority to license. ',&
'                                                                       ',&
'  h. Licensor means the individual(s) or entity(ies) granting rights   ',&
'     under this Public License.                                        ',&
'                                                                       ',&
'  i. Share means to provide material to the public by any means or     ',&
'     process that requires permission under the Licensed Rights, such  ',&
'     as reproduction, public display, public performance, distribution,',&
'     dissemination, communication, or importation, and to make material',&
'     available to the public including in ways that members of the     ',&
'     public may access the material from a place and at a time         ',&
'     individually chosen by them.                                      ',&
'                                                                       ',&
'  j. Sui Generis Database Rights means rights other than copyright     ',&
'     resulting from Directive 96/9/EC of the European Parliament and of',&
'     the Council of 11 March 1996 on the legal protection of databases,',&
'     as amended and/or succeeded, as well as other essentially         ',&
'     equivalent rights anywhere in the world.                          ',&
'                                                                       ',&
'  k. You means the individual or entity exercising the Licensed Rights ',&
'     under this Public License. Your has a corresponding meaning.      ',&
'                                                                       ',&
'                                                                       ',&
'Section 2 -- Scope.                                                    ',&
'                                                                       ',&
'  a. License grant.                                                    ',&
'                                                                       ',&
'       1. Subject to the terms and conditions of this Public License,  ',&
'          the Licensor hereby grants You a worldwide, royalty-free,    ',&
'          non-sublicensable, non-exclusive, irrevocable license to     ',&
'          exercise the Licensed Rights in the Licensed Material to:    ',&
'                                                                       ',&
'            a. reproduce and Share the Licensed Material, in whole or  ',&
'               in part; and                                            ',&
'                                                                       ',&
'            b. produce, reproduce, and Share Adapted Material.         ',&
'                                                                       ',&
'       2. Exceptions and Limitations. For the avoidance of doubt, where',&
'          Exceptions and Limitations apply to Your use, this Public    ',&
'          License does not apply, and You do not need to comply with   ',&
'          its terms and conditions.                                    ',&
'                                                                       ',&
'       3. Term. The term of this Public License is specified in Section',&
'          6(a).                                                        ',&
'                                                                       ',&
'       4. Media and formats; technical modifications allowed. The      ',&
'          Licensor authorizes You to exercise the Licensed Rights in   ',&
'          all media and formats whether now known or hereafter created,',&
'          and to make technical modifications necessary to do so. The  ',&
'          Licensor waives and/or agrees not to assert any right or     ',&
'          authority to forbid You from making technical modifications  ',&
'          necessary to exercise the Licensed Rights, including         ',&
'          technical modifications necessary to circumvent Effective    ',&
'          Technological Measures. For purposes of this Public License, ',&
'          simply making modifications authorized by this Section 2(a)  ',&
'          (4) never produces Adapted Material.                         ',&
'                                                                       ',&
'       5. Downstream recipients.                                       ',&
'                                                                       ',&
'            a. Offer from the Licensor -- Licensed Material. Every     ',&
'               recipient of the Licensed Material automatically        ',&
'               receives an offer from the Licensor to exercise the     ',&
'               Licensed Rights under the terms and conditions of this  ',&
'               Public License.                                         ',&
'                                                                       ',&
'            b. No downstream restrictions. You may not offer or impose ',&
'               any additional or different terms or conditions on, or  ',&
'               apply any Effective Technological Measures to, the      ',&
'               Licensed Material if doing so restricts exercise of the ',&
'               Licensed Rights by any recipient of the Licensed        ',&
'               Material.                                               ',&
'                                                                       ',&
'       6. No endorsement. Nothing in this Public License constitutes or',&
'          may be construed as permission to assert or imply that You   ',&
'          are, or that Your use of the Licensed Material is, connected ',&
'          with, or sponsored, endorsed, or granted official status by, ',&
'          the Licensor or others designated to receive attribution as  ',&
'          provided in Section 3(a)(1)(A)(i).                           ',&
'                                                                       ',&
'  b. Other rights.                                                     ',&
'                                                                       ',&
'       1. Moral rights, such as the right of integrity, are not        ',&
'          licensed under this Public License, nor are publicity,       ',&
'          privacy, and/or other similar personality rights; however, to',&
'          the extent possible, the Licensor waives and/or agrees not to',&
'          assert any such rights held by the Licensor to the limited   ',&
'          extent necessary to allow You to exercise the Licensed       ',&
'          Rights, but not otherwise.                                   ',&
'                                                                       ',&
'       2. Patent and trademark rights are not licensed under this      ',&
'          Public License.                                              ',&
'                                                                       ',&
'       3. To the extent possible, the Licensor waives any right to     ',&
'          collect royalties from You for the exercise of the Licensed  ',&
'          Rights, whether directly or through a collecting society     ',&
'          under any voluntary or waivable statutory or compulsory      ',&
'          licensing scheme. In all other cases the Licensor expressly  ',&
'          reserves any right to collect such royalties.                ',&
'                                                                       ',&
'                                                                       ',&
'Section 3 -- License Conditions.                                       ',&
'                                                                       ',&
'Your exercise of the Licensed Rights is expressly made subject to the  ',&
'following conditions.                                                  ',&
'                                                                       ',&
'  a. Attribution.                                                      ',&
'                                                                       ',&
'       1. If You Share the Licensed Material (including in modified    ',&
'          form), You must:                                             ',&
'                                                                       ',&
'            a. retain the following if it is supplied by the Licensor  ',&
'               with the Licensed Material:                             ',&
'                                                                       ',&
'                 i. identification of the creator(s) of the Licensed   ',&
'                    Material and any others designated to receive      ',&
'                    attribution, in any reasonable manner requested by ',&
'                    the Licensor (including by pseudonym if            ',&
'                    designated);                                       ',&
'                                                                       ',&
'                ii. a copyright notice;                                ',&
'                                                                       ',&
'               iii. a notice that refers to this Public License;       ',&
'                                                                       ',&
'                iv. a notice that refers to the disclaimer of          ',&
'                    warranties;                                        ',&
'                                                                       ',&
'                 v. a URI or hyperlink to the Licensed Material to the ',&
'                    extent reasonably practicable;                     ',&
'                                                                       ',&
'            b. indicate if You modified the Licensed Material and      ',&
'               retain an indication of any previous modifications; and ',&
'                                                                       ',&
'            c. indicate the Licensed Material is licensed under this   ',&
'               Public License, and include the text of, or the URI or  ',&
'               hyperlink to, this Public License.                      ',&
'                                                                       ',&
'       2. You may satisfy the conditions in Section 3(a)(1) in any     ',&
'          reasonable manner based on the medium, means, and context in ',&
'          which You Share the Licensed Material. For example, it may be',&
'          reasonable to satisfy the conditions by providing a URI or   ',&
'          hyperlink to a resource that includes the required           ',&
'          information.                                                 ',&
'                                                                       ',&
'       3. If requested by the Licensor, You must remove any of the     ',&
'          information required by Section 3(a)(1)(A) to the extent     ',&
'          reasonably practicable.                                      ',&
'                                                                       ',&
'       4. If You Share Adapted Material You produce, the Adapter''s    ',&
'          License You apply must not prevent recipients of the Adapted ',&
'          Material from complying with this Public License.            ',&
'                                                                       ',&
'                                                                       ',&
'Section 4 -- Sui Generis Database Rights.                              ',&
'                                                                       ',&
'Where the Licensed Rights include Sui Generis Database Rights that     ',&
'apply to Your use of the Licensed Material:                            ',&
'                                                                       ',&
'  a. for the avoidance of doubt, Section 2(a)(1) grants You the right  ',&
'     to extract, reuse, reproduce, and Share all or a substantial      ',&
'     portion of the contents of the database;                          ',&
'                                                                       ',&
'  b. if You include all or a substantial portion of the database       ',&
'     contents in a database in which You have Sui Generis Database     ',&
'     Rights, then the database in which You have Sui Generis Database  ',&
'     Rights (but not its individual contents) is Adapted Material; and ',&
'                                                                       ',&
'  c. You must comply with the conditions in Section 3(a) if You Share  ',&
'     all or a substantial portion of the contents of the database.     ',&
'                                                                       ',&
'For the avoidance of doubt, this Section 4 supplements and does not    ',&
'replace Your obligations under this Public License where the Licensed  ',&
'Rights include other Copyright and Similar Rights.                     ',&
'                                                                       ',&
'                                                                       ',&
'Section 5 -- Disclaimer of Warranties and Limitation of Liability.     ',&
'                                                                       ',&
'  a. UNLESS OTHERWISE SEPARATELY UNDERTAKEN BY THE LICENSOR, TO THE    ',&
'     EXTENT POSSIBLE, THE LICENSOR OFFERS THE LICENSED MATERIAL AS-IS  ',&
'     AND AS-AVAILABLE, AND MAKES NO REPRESENTATIONS OR WARRANTIES OF   ',&
'     ANY KIND CONCERNING THE LICENSED MATERIAL, WHETHER EXPRESS,       ',&
'     IMPLIED, STATUTORY, OR OTHER. THIS INCLUDES, WITHOUT LIMITATION,  ',&
'     WARRANTIES OF TITLE, MERCHANTABILITY, FITNESS FOR A PARTICULAR    ',&
'     PURPOSE, NON-INFRINGEMENT, ABSENCE OF LATENT OR OTHER DEFECTS,    ',&
'     ACCURACY, OR THE PRESENCE OR ABSENCE OF ERRORS, WHETHER OR NOT    ',&
'     KNOWN OR DISCOVERABLE. WHERE DISCLAIMERS OF WARRANTIES ARE NOT    ',&
'     ALLOWED IN FULL OR IN PART, THIS DISCLAIMER MAY NOT APPLY TO YOU. ',&
'                                                                       ',&
'  b. TO THE EXTENT POSSIBLE, IN NO EVENT WILL THE LICENSOR BE LIABLE   ',&
'     TO YOU ON ANY LEGAL THEORY (INCLUDING, WITHOUT LIMITATION,        ',&
'     NEGLIGENCE) OR OTHERWISE FOR ANY DIRECT, SPECIAL, INDIRECT,       ',&
'     INCIDENTAL, CONSEQUENTIAL, PUNITIVE, EXEMPLARY, OR OTHER LOSSES,  ',&
'     COSTS, EXPENSES, OR DAMAGES ARISING OUT OF THIS PUBLIC LICENSE OR ',&
'     USE OF THE LICENSED MATERIAL, EVEN IF THE LICENSOR HAS BEEN       ',&
'     ADVISED OF THE POSSIBILITY OF SUCH LOSSES, COSTS, EXPENSES, OR    ',&
'     DAMAGES. WHERE A LIMITATION OF LIABILITY IS NOT ALLOWED IN FULL OR',&
'     IN PART, THIS LIMITATION MAY NOT APPLY TO YOU.                    ',&
'                                                                       ',&
'  c. The disclaimer of warranties and limitation of liability provided ',&
'     above shall be interpreted in a manner that, to the extent        ',&
'     possible, most closely approximates an absolute disclaimer and    ',&
'     waiver of all liability.                                          ',&
'                                                                       ',&
'                                                                       ',&
'Section 6 -- Term and Termination.                                     ',&
'                                                                       ',&
'  a. This Public License applies for the term of the Copyright and     ',&
'     Similar Rights licensed here. However, if You fail to comply with ',&
'     this Public License, then Your rights under this Public License   ',&
'     terminate automatically.                                          ',&
'                                                                       ',&
'  b. Where Your right to use the Licensed Material has terminated under',&
'     Section 6(a), it reinstates:                                      ',&
'                                                                       ',&
'       1. automatically as of the date the violation is cured, provided',&
'          it is cured within 30 days of Your discovery of the          ',&
'          violation; or                                                ',&
'                                                                       ',&
'       2. upon express reinstatement by the Licensor.                  ',&
'                                                                       ',&
'     For the avoidance of doubt, this Section 6(b) does not affect any ',&
'     right the Licensor may have to seek remedies for Your violations  ',&
'     of this Public License.                                           ',&
'                                                                       ',&
'  c. For the avoidance of doubt, the Licensor may also offer the       ',&
'     Licensed Material under separate terms or conditions or stop      ',&
'     distributing the Licensed Material at any time; however, doing so ',&
'     will not terminate this Public License.                           ',&
'                                                                       ',&
'  d. Sections 1, 5, 6, 7, and 8 survive termination of this Public     ',&
'     License.                                                          ',&
'                                                                       ',&
'                                                                       ',&
'Section 7 -- Other Terms and Conditions.                               ',&
'                                                                       ',&
'  a. The Licensor shall not be bound by any additional or different    ',&
'     terms or conditions communicated by You unless expressly agreed.  ',&
'                                                                       ',&
'  b. Any arrangements, understandings, or agreements regarding the     ',&
'     Licensed Material not stated herein are separate from and         ',&
'     independent of the terms and conditions of this Public License.   ',&
'                                                                       ',&
'                                                                       ',&
'Section 8 -- Interpretation.                                           ',&
'                                                                       ',&
'  a. For the avoidance of doubt, this Public License does not, and     ',&
'     shall not be interpreted to, reduce, limit, restrict, or impose   ',&
'     conditions on any use of the Licensed Material that could lawfully',&
'     be made without permission under this Public License.             ',&
'                                                                       ',&
'  b. To the extent possible, if any provision of this Public License is',&
'     deemed unenforceable, it shall be automatically reformed to the   ',&
'     minimum extent necessary to make it enforceable. If the provision ',&
'     cannot be reformed, it shall be severed from this Public License  ',&
'     without affecting the enforceability of the remaining terms and   ',&
'     conditions.                                                       ',&
'                                                                       ',&
'  c. No term or condition of this Public License will be waived and no ',&
'     failure to comply consented to unless expressly agreed to by the  ',&
'     Licensor.                                                         ',&
'                                                                       ',&
'  d. Nothing in this Public License constitutes or may be interpreted  ',&
'     as a limitation upon, or waiver of, any privileges and immunities ',&
'     that apply to the Licensor or You, including from the legal       ',&
'     processes of any jurisdiction or authority.                       ',&
'                                                                       ',&
'                                                                       ',&
'=======================================================================',&
'                                                                       ',&
'Creative Commons is not a party to its public licenses.                ',&
'Notwithstanding, Creative Commons may elect to apply one of its public ',&
'licenses to material it publishes and in those instances will be       ',&
'considered the “Licensor.” The text of the Creative Commons public ',&
'licenses is dedicated to the public domain under the CC0 Public Domain ',&
'Dedication. Except for the limited purpose of indicating that material ',&
'is shared under a Creative Commons public license or as otherwise      ',&
'permitted by the Creative Commons policies published at                ',&
'creativecommons.org/policies, Creative Commons does not authorize the  ',&
'use of the trademark "Creative Commons" or any other trademark or logo ',&
'of Creative Commons without its prior written consent including,       ',&
'without limitation, in connection with any unauthorized modifications  ',&
'to any of its public licenses or any other arrangements,               ',&
'understandings, or agreements concerning use of licensed material. For ',&
'the avoidance of doubt, this paragraph does not form part of the public',&
'licenses.                                                              ',&
'                                                                       ',&
'Creative Commons may be contacted at creativecommons.org.              ',&
'                                                                       ',&
'']
if(present(topic_only))then
   if(size(textblock)>0)then
      if(topic_only) textblock=[character(len=len(textblock(1))) :: textblock(1)]
   else
      textblock=['']
   endif
endif
case('15','cc-by-sa-4.0')
textblock=[ CHARACTER(LEN=128) :: &
'cc-by-sa-4.0',&
'            ',&
'Attribution-ShareAlike 4.0 International',&
'                                        ',&
'=======================================================================',&
'                                                                       ',&
'Creative Commons Corporation ("Creative Commons") is not a law firm and',&
'does not provide legal services or legal advice. Distribution of       ',&
'Creative Commons public licenses does not create a lawyer-client or    ',&
'other relationship. Creative Commons makes its licenses and related    ',&
'information available on an "as-is" basis. Creative Commons gives no   ',&
'warranties regarding its licenses, any material licensed under their   ',&
'terms and conditions, or any related information. Creative Commons     ',&
'disclaims all liability for damages resulting from their use to the    ',&
'fullest extent possible.                                               ',&
'                                                                       ',&
'Using Creative Commons Public Licenses                                 ',&
'                                                                       ',&
'Creative Commons public licenses provide a standard set of terms and   ',&
'conditions that creators and other rights holders may use to share     ',&
'original works of authorship and other material subject to copyright   ',&
'and certain other rights specified in the public license below. The    ',&
'following considerations are for informational purposes only, are not  ',&
'exhaustive, and do not form part of our licenses.                      ',&
'                                                                       ',&
'     Considerations for licensors: Our public licenses are             ',&
'     intended for use by those authorized to give the public           ',&
'     permission to use material in ways otherwise restricted by        ',&
'     copyright and certain other rights. Our licenses are              ',&
'     irrevocable. Licensors should read and understand the terms       ',&
'     and conditions of the license they choose before applying it.     ',&
'     Licensors should also secure all rights necessary before          ',&
'     applying our licenses so that the public can reuse the            ',&
'     material as expected. Licensors should clearly mark any           ',&
'     material not subject to the license. This includes other CC-      ',&
'     licensed material, or material used under an exception or         ',&
'     limitation to copyright. More considerations for licensors:       ',&
'     wiki.creativecommons.org/Considerations_for_licensors             ',&
'                                                                       ',&
'     Considerations for the public: By using one of our public         ',&
'     licenses, a licensor grants the public permission to use the      ',&
'     licensed material under specified terms and conditions. If        ',&
'     the licensor''s permission is not necessary for any reason--for   ',&
'     example, because of any applicable exception or limitation to     ',&
'     copyright--then that use is not regulated by the license. Our     ',&
'     licenses grant only permissions under copyright and certain       ',&
'     other rights that a licensor has authority to grant. Use of       ',&
'     the licensed material may still be restricted for other           ',&
'     reasons, including because others have copyright or other         ',&
'     rights in the material. A licensor may make special requests,     ',&
'     such as asking that all changes be marked or described.           ',&
'     Although not required by our licenses, you are encouraged to      ',&
'     respect those requests where reasonable. More considerations      ',&
'     for the public:                                                   ',&
'     wiki.creativecommons.org/Considerations_for_licensees             ',&
'                                                                       ',&
'=======================================================================',&
'                                                                       ',&
'Creative Commons Attribution-ShareAlike 4.0 International Public       ',&
'License                                                                ',&
'                                                                       ',&
'By exercising the Licensed Rights (defined below), You accept and agree',&
'to be bound by the terms and conditions of this Creative Commons       ',&
'Attribution-ShareAlike 4.0 International Public License ("Public       ',&
'License"). To the extent this Public License may be interpreted as a   ',&
'contract, You are granted the Licensed Rights in consideration of Your ',&
'acceptance of these terms and conditions, and the Licensor grants You  ',&
'such rights in consideration of benefits the Licensor receives from    ',&
'making the Licensed Material available under these terms and           ',&
'conditions.                                                            ',&
'                                                                       ',&
'                                                                       ',&
'Section 1 -- Definitions.                                              ',&
'                                                                       ',&
'  a. Adapted Material means material subject to Copyright and Similar  ',&
'     Rights that is derived from or based upon the Licensed Material   ',&
'     and in which the Licensed Material is translated, altered,        ',&
'     arranged, transformed, or otherwise modified in a manner requiring',&
'     permission under the Copyright and Similar Rights held by the     ',&
'     Licensor. For purposes of this Public License, where the Licensed ',&
'     Material is a musical work, performance, or sound recording,      ',&
'     Adapted Material is always produced where the Licensed Material is',&
'     synched in timed relation with a moving image.                    ',&
'                                                                       ',&
'  b. Adapter''s License means the license You apply to Your Copyright  ',&
'     and Similar Rights in Your contributions to Adapted Material in   ',&
'     accordance with the terms and conditions of this Public License.  ',&
'                                                                       ',&
'  c. BY-SA Compatible License means a license listed at                ',&
'     creativecommons.org/compatiblelicenses, approved by Creative      ',&
'     Commons as essentially the equivalent of this Public License.     ',&
'                                                                       ',&
'  d. Copyright and Similar Rights means copyright and/or similar rights',&
'     closely related to copyright including, without limitation,       ',&
'     performance, broadcast, sound recording, and Sui Generis Database ',&
'     Rights, without regard to how the rights are labeled or           ',&
'     categorized. For purposes of this Public License, the rights      ',&
'     specified in Section 2(b)(1)-(2) are not Copyright and Similar    ',&
'     Rights.                                                           ',&
'                                                                       ',&
'  e. Effective Technological Measures means those measures that, in the',&
'     absence of proper authority, may not be circumvented under laws   ',&
'     fulfilling obligations under Article 11 of the WIPO Copyright     ',&
'     Treaty adopted on December 20, 1996, and/or similar international ',&
'     agreements.                                                       ',&
'                                                                       ',&
'  f. Exceptions and Limitations means fair use, fair dealing, and/or   ',&
'     any other exception or limitation to Copyright and Similar Rights ',&
'     that applies to Your use of the Licensed Material.                ',&
'                                                                       ',&
'  g. License Elements means the license attributes listed in the name  ',&
'     of a Creative Commons Public License. The License Elements of this',&
'     Public License are Attribution and ShareAlike.                    ',&
'                                                                       ',&
'  h. Licensed Material means the artistic or literary work, database,  ',&
'     or other material to which the Licensor applied this Public       ',&
'     License.                                                          ',&
'                                                                       ',&
'  i. Licensed Rights means the rights granted to You subject to the    ',&
'     terms and conditions of this Public License, which are limited to ',&
'     all Copyright and Similar Rights that apply to Your use of the    ',&
'     Licensed Material and that the Licensor has authority to license. ',&
'                                                                       ',&
'  j. Licensor means the individual(s) or entity(ies) granting rights   ',&
'     under this Public License.                                        ',&
'                                                                       ',&
'  k. Share means to provide material to the public by any means or     ',&
'     process that requires permission under the Licensed Rights, such  ',&
'     as reproduction, public display, public performance, distribution,',&
'     dissemination, communication, or importation, and to make material',&
'     available to the public including in ways that members of the     ',&
'     public may access the material from a place and at a time         ',&
'     individually chosen by them.                                      ',&
'                                                                       ',&
'  l. Sui Generis Database Rights means rights other than copyright     ',&
'     resulting from Directive 96/9/EC of the European Parliament and of',&
'     the Council of 11 March 1996 on the legal protection of databases,',&
'     as amended and/or succeeded, as well as other essentially         ',&
'     equivalent rights anywhere in the world.                          ',&
'                                                                       ',&
'  m. You means the individual or entity exercising the Licensed Rights ',&
'     under this Public License. Your has a corresponding meaning.      ',&
'                                                                       ',&
'                                                                       ',&
'Section 2 -- Scope.                                                    ',&
'                                                                       ',&
'  a. License grant.                                                    ',&
'                                                                       ',&
'       1. Subject to the terms and conditions of this Public License,  ',&
'          the Licensor hereby grants You a worldwide, royalty-free,    ',&
'          non-sublicensable, non-exclusive, irrevocable license to     ',&
'          exercise the Licensed Rights in the Licensed Material to:    ',&
'                                                                       ',&
'            a. reproduce and Share the Licensed Material, in whole or  ',&
'               in part; and                                            ',&
'                                                                       ',&
'            b. produce, reproduce, and Share Adapted Material.         ',&
'                                                                       ',&
'       2. Exceptions and Limitations. For the avoidance of doubt, where',&
'          Exceptions and Limitations apply to Your use, this Public    ',&
'          License does not apply, and You do not need to comply with   ',&
'          its terms and conditions.                                    ',&
'                                                                       ',&
'       3. Term. The term of this Public License is specified in Section',&
'          6(a).                                                        ',&
'                                                                       ',&
'       4. Media and formats; technical modifications allowed. The      ',&
'          Licensor authorizes You to exercise the Licensed Rights in   ',&
'          all media and formats whether now known or hereafter created,',&
'          and to make technical modifications necessary to do so. The  ',&
'          Licensor waives and/or agrees not to assert any right or     ',&
'          authority to forbid You from making technical modifications  ',&
'          necessary to exercise the Licensed Rights, including         ',&
'          technical modifications necessary to circumvent Effective    ',&
'          Technological Measures. For purposes of this Public License, ',&
'          simply making modifications authorized by this Section 2(a)  ',&
'          (4) never produces Adapted Material.                         ',&
'                                                                       ',&
'       5. Downstream recipients.                                       ',&
'                                                                       ',&
'            a. Offer from the Licensor -- Licensed Material. Every     ',&
'               recipient of the Licensed Material automatically        ',&
'               receives an offer from the Licensor to exercise the     ',&
'               Licensed Rights under the terms and conditions of this  ',&
'               Public License.                                         ',&
'                                                                       ',&
'            b. Additional offer from the Licensor -- Adapted Material. ',&
'               Every recipient of Adapted Material from You            ',&
'               automatically receives an offer from the Licensor to    ',&
'               exercise the Licensed Rights in the Adapted Material    ',&
'               under the conditions of the Adapter''s License You apply.',&
'                                                                        ',&
'            c. No downstream restrictions. You may not offer or impose  ',&
'               any additional or different terms or conditions on, or   ',&
'               apply any Effective Technological Measures to, the       ',&
'               Licensed Material if doing so restricts exercise of the  ',&
'               Licensed Rights by any recipient of the Licensed         ',&
'               Material.                                                ',&
'                                                                        ',&
'       6. No endorsement. Nothing in this Public License constitutes or ',&
'          may be construed as permission to assert or imply that You    ',&
'          are, or that Your use of the Licensed Material is, connected  ',&
'          with, or sponsored, endorsed, or granted official status by,  ',&
'          the Licensor or others designated to receive attribution as   ',&
'          provided in Section 3(a)(1)(A)(i).                            ',&
'                                                                        ',&
'  b. Other rights.                                                      ',&
'                                                                        ',&
'       1. Moral rights, such as the right of integrity, are not         ',&
'          licensed under this Public License, nor are publicity,        ',&
'          privacy, and/or other similar personality rights; however, to ',&
'          the extent possible, the Licensor waives and/or agrees not to ',&
'          assert any such rights held by the Licensor to the limited    ',&
'          extent necessary to allow You to exercise the Licensed        ',&
'          Rights, but not otherwise.                                    ',&
'                                                                        ',&
'       2. Patent and trademark rights are not licensed under this       ',&
'          Public License.                                               ',&
'                                                                        ',&
'       3. To the extent possible, the Licensor waives any right to      ',&
'          collect royalties from You for the exercise of the Licensed   ',&
'          Rights, whether directly or through a collecting society      ',&
'          under any voluntary or waivable statutory or compulsory       ',&
'          licensing scheme. In all other cases the Licensor expressly   ',&
'          reserves any right to collect such royalties.                 ',&
'                                                                        ',&
'                                                                        ',&
'Section 3 -- License Conditions.                                        ',&
'                                                                        ',&
'Your exercise of the Licensed Rights is expressly made subject to the   ',&
'following conditions.                                                   ',&
'                                                                        ',&
'  a. Attribution.                                                       ',&
'                                                                        ',&
'       1. If You Share the Licensed Material (including in modified     ',&
'          form), You must:                                              ',&
'                                                                        ',&
'            a. retain the following if it is supplied by the Licensor   ',&
'               with the Licensed Material:                              ',&
'                                                                        ',&
'                 i. identification of the creator(s) of the Licensed    ',&
'                    Material and any others designated to receive       ',&
'                    attribution, in any reasonable manner requested by  ',&
'                    the Licensor (including by pseudonym if             ',&
'                    designated);                                        ',&
'                                                                        ',&
'                ii. a copyright notice;                                 ',&
'                                                                        ',&
'               iii. a notice that refers to this Public License;        ',&
'                                                                        ',&
'                iv. a notice that refers to the disclaimer of           ',&
'                    warranties;                                         ',&
'                                                                        ',&
'                 v. a URI or hyperlink to the Licensed Material to the  ',&
'                    extent reasonably practicable;                      ',&
'                                                                        ',&
'            b. indicate if You modified the Licensed Material and       ',&
'               retain an indication of any previous modifications; and  ',&
'                                                                        ',&
'            c. indicate the Licensed Material is licensed under this    ',&
'               Public License, and include the text of, or the URI or   ',&
'               hyperlink to, this Public License.                       ',&
'                                                                        ',&
'       2. You may satisfy the conditions in Section 3(a)(1) in any      ',&
'          reasonable manner based on the medium, means, and context in  ',&
'          which You Share the Licensed Material. For example, it may be ',&
'          reasonable to satisfy the conditions by providing a URI or    ',&
'          hyperlink to a resource that includes the required            ',&
'          information.                                                  ',&
'                                                                        ',&
'       3. If requested by the Licensor, You must remove any of the      ',&
'          information required by Section 3(a)(1)(A) to the extent      ',&
'          reasonably practicable.                                       ',&
'                                                                        ',&
'  b. ShareAlike.                                                        ',&
'                                                                        ',&
'     In addition to the conditions in Section 3(a), if You Share        ',&
'     Adapted Material You produce, the following conditions also apply. ',&
'                                                                        ',&
'       1. The Adapter''s License You apply must be a Creative Commons   ',&
'          license with the same License Elements, this version or       ',&
'          later, or a BY-SA Compatible License.                         ',&
'                                                                        ',&
'       2. You must include the text of, or the URI or hyperlink to, the ',&
'          Adapter''s License You apply. You may satisfy this condition  ',&
'          in any reasonable manner based on the medium, means, and      ',&
'          context in which You Share Adapted Material.                  ',&
'                                                                        ',&
'       3. You may not offer or impose any additional or different terms ',&
'          or conditions on, or apply any Effective Technological        ',&
'          Measures to, Adapted Material that restrict exercise of the   ',&
'          rights granted under the Adapter''s License You apply.        ',&
'                                                                        ',&
'                                                                        ',&
'Section 4 -- Sui Generis Database Rights.                               ',&
'                                                                        ',&
'Where the Licensed Rights include Sui Generis Database Rights that      ',&
'apply to Your use of the Licensed Material:                             ',&
'                                                                        ',&
'  a. for the avoidance of doubt, Section 2(a)(1) grants You the right   ',&
'     to extract, reuse, reproduce, and Share all or a substantial       ',&
'     portion of the contents of the database;                           ',&
'                                                                        ',&
'  b. if You include all or a substantial portion of the database        ',&
'     contents in a database in which You have Sui Generis Database      ',&
'     Rights, then the database in which You have Sui Generis Database   ',&
'     Rights (but not its individual contents) is Adapted Material,      ',&
'     including for purposes of Section 3(b); and                        ',&
'                                                                        ',&
'  c. You must comply with the conditions in Section 3(a) if You Share   ',&
'     all or a substantial portion of the contents of the database.      ',&
'                                                                        ',&
'For the avoidance of doubt, this Section 4 supplements and does not     ',&
'replace Your obligations under this Public License where the Licensed   ',&
'Rights include other Copyright and Similar Rights.                      ',&
'                                                                        ',&
'                                                                        ',&
'Section 5 -- Disclaimer of Warranties and Limitation of Liability.      ',&
'                                                                        ',&
'  a. UNLESS OTHERWISE SEPARATELY UNDERTAKEN BY THE LICENSOR, TO THE     ',&
'     EXTENT POSSIBLE, THE LICENSOR OFFERS THE LICENSED MATERIAL AS-IS   ',&
'     AND AS-AVAILABLE, AND MAKES NO REPRESENTATIONS OR WARRANTIES OF    ',&
'     ANY KIND CONCERNING THE LICENSED MATERIAL, WHETHER EXPRESS,        ',&
'     IMPLIED, STATUTORY, OR OTHER. THIS INCLUDES, WITHOUT LIMITATION,   ',&
'     WARRANTIES OF TITLE, MERCHANTABILITY, FITNESS FOR A PARTICULAR     ',&
'     PURPOSE, NON-INFRINGEMENT, ABSENCE OF LATENT OR OTHER DEFECTS,     ',&
'     ACCURACY, OR THE PRESENCE OR ABSENCE OF ERRORS, WHETHER OR NOT     ',&
'     KNOWN OR DISCOVERABLE. WHERE DISCLAIMERS OF WARRANTIES ARE NOT     ',&
'     ALLOWED IN FULL OR IN PART, THIS DISCLAIMER MAY NOT APPLY TO YOU.  ',&
'                                                                        ',&
'  b. TO THE EXTENT POSSIBLE, IN NO EVENT WILL THE LICENSOR BE LIABLE    ',&
'     TO YOU ON ANY LEGAL THEORY (INCLUDING, WITHOUT LIMITATION,         ',&
'     NEGLIGENCE) OR OTHERWISE FOR ANY DIRECT, SPECIAL, INDIRECT,        ',&
'     INCIDENTAL, CONSEQUENTIAL, PUNITIVE, EXEMPLARY, OR OTHER LOSSES,   ',&
'     COSTS, EXPENSES, OR DAMAGES ARISING OUT OF THIS PUBLIC LICENSE OR  ',&
'     USE OF THE LICENSED MATERIAL, EVEN IF THE LICENSOR HAS BEEN        ',&
'     ADVISED OF THE POSSIBILITY OF SUCH LOSSES, COSTS, EXPENSES, OR     ',&
'     DAMAGES. WHERE A LIMITATION OF LIABILITY IS NOT ALLOWED IN FULL OR ',&
'     IN PART, THIS LIMITATION MAY NOT APPLY TO YOU.                     ',&
'                                                                        ',&
'  c. The disclaimer of warranties and limitation of liability provided  ',&
'     above shall be interpreted in a manner that, to the extent         ',&
'     possible, most closely approximates an absolute disclaimer and     ',&
'     waiver of all liability.                                           ',&
'                                                                        ',&
'                                                                        ',&
'Section 6 -- Term and Termination.                                      ',&
'                                                                        ',&
'  a. This Public License applies for the term of the Copyright and      ',&
'     Similar Rights licensed here. However, if You fail to comply with  ',&
'     this Public License, then Your rights under this Public License    ',&
'     terminate automatically.                                           ',&
'                                                                        ',&
'  b. Where Your right to use the Licensed Material has terminated under ',&
'     Section 6(a), it reinstates:                                       ',&
'                                                                        ',&
'       1. automatically as of the date the violation is cured, provided ',&
'          it is cured within 30 days of Your discovery of the           ',&
'          violation; or                                                 ',&
'                                                                        ',&
'       2. upon express reinstatement by the Licensor.                   ',&
'                                                                        ',&
'     For the avoidance of doubt, this Section 6(b) does not affect any  ',&
'     right the Licensor may have to seek remedies for Your violations   ',&
'     of this Public License.                                            ',&
'                                                                        ',&
'  c. For the avoidance of doubt, the Licensor may also offer the        ',&
'     Licensed Material under separate terms or conditions or stop       ',&
'     distributing the Licensed Material at any time; however, doing so  ',&
'     will not terminate this Public License.                            ',&
'                                                                        ',&
'  d. Sections 1, 5, 6, 7, and 8 survive termination of this Public      ',&
'     License.                                                           ',&
'                                                                        ',&
'                                                                        ',&
'Section 7 -- Other Terms and Conditions.                                ',&
'                                                                        ',&
'  a. The Licensor shall not be bound by any additional or different     ',&
'     terms or conditions communicated by You unless expressly agreed.   ',&
'                                                                        ',&
'  b. Any arrangements, understandings, or agreements regarding the      ',&
'     Licensed Material not stated herein are separate from and          ',&
'     independent of the terms and conditions of this Public License.    ',&
'                                                                        ',&
'                                                                        ',&
'Section 8 -- Interpretation.                                            ',&
'                                                                        ',&
'  a. For the avoidance of doubt, this Public License does not, and      ',&
'     shall not be interpreted to, reduce, limit, restrict, or impose    ',&
'     conditions on any use of the Licensed Material that could lawfully ',&
'     be made without permission under this Public License.              ',&
'                                                                        ',&
'  b. To the extent possible, if any provision of this Public License is ',&
'     deemed unenforceable, it shall be automatically reformed to the    ',&
'     minimum extent necessary to make it enforceable. If the provision  ',&
'     cannot be reformed, it shall be severed from this Public License   ',&
'     without affecting the enforceability of the remaining terms and    ',&
'     conditions.                                                        ',&
'                                                                        ',&
'  c. No term or condition of this Public License will be waived and no  ',&
'     failure to comply consented to unless expressly agreed to by the   ',&
'     Licensor.                                                          ',&
'                                                                        ',&
'  d. Nothing in this Public License constitutes or may be interpreted   ',&
'     as a limitation upon, or waiver of, any privileges and immunities  ',&
'     that apply to the Licensor or You, including from the legal        ',&
'     processes of any jurisdiction or authority.                        ',&
'                                                                        ',&
'                                                                        ',&
'======================================================================= ',&
'                                                                        ',&
'Creative Commons is not a party to its public licenses.                 ',&
'Notwithstanding, Creative Commons may elect to apply one of its public  ',&
'licenses to material it publishes and in those instances will be        ',&
'considered the “Licensor.” The text of the Creative Commons public  ',&
'licenses is dedicated to the public domain under the CC0 Public Domain  ',&
'Dedication. Except for the limited purpose of indicating that material  ',&
'is shared under a Creative Commons public license or as otherwise       ',&
'permitted by the Creative Commons policies published at                 ',&
'creativecommons.org/policies, Creative Commons does not authorize the   ',&
'use of the trademark "Creative Commons" or any other trademark or logo  ',&
'of Creative Commons without its prior written consent including,        ',&
'without limitation, in connection with any unauthorized modifications   ',&
'to any of its public licenses or any other arrangements,                ',&
'understandings, or agreements concerning use of licensed material. For  ',&
'the avoidance of doubt, this paragraph does not form part of the public ',&
'licenses.                                                               ',&
'                                                                        ',&
'Creative Commons may be contacted at creativecommons.org.               ',&
'                                                                        ',&
'']
if(present(topic_only))then
   if(size(textblock)>0)then
      if(topic_only) textblock=[character(len=len(textblock(1))) :: textblock(1)]
   else
      textblock=['']
   endif
endif
case('16','cecill-2.1')
textblock=[ CHARACTER(LEN=128) :: &
'cecill-2.1',&
'          ',&
'  CONTRAT DE LICENCE DE LOGICIEL LIBRE CeCILL',&
'                                             ',&
'Version 2.1 du 2013-06-21                    ',&
'                                             ',&
'                                             ',&
'    Avertissement                            ',&
'                                             ',&
'Ce contrat est une licence de logiciel libre issue d''une concertation',&
'entre ses auteurs afin que le respect de deux grands principes préside à',&
'sa rédaction:                                                            ',&
'                                                                          ',&
'  * d''une part, le respect des principes de diffusion des logiciels      ',&
'    libres: accès au code source, droits étendus conférés aux utilisateurs,',&
'  * d''autre part, la désignation d''un droit applicable, le droit            ',&
'    français, auquel elle est conforme, tant au regard du droit de la         ',&
'    responsabilité civile que du droit de la propriété intellectuelle et    ',&
'    de la protection qu''il offre aux auteurs et titulaires des droits         ',&
'    patrimoniaux sur un logiciel.                                              ',&
'                                                                               ',&
'Les auteurs de la licence CeCILL (Ce[a] C[nrs] I[nria] L[ogiciel] L[ibre])     ',&
'sont:                                                                          ',&
'                                                                               ',&
'Commissariat à l''énergie atomique et aux énergies alternatives - CEA,      ',&
'établissement public de recherche à caractère scientifique, technique et    ',&
'industriel, dont le siège est situé 25 rue Leblanc, immeuble Le Ponant       ',&
'D, 75015 Paris.                                                                ',&
'                                                                               ',&
'Centre National de la Recherche Scientifique - CNRS, établissement            ',&
'public à caractère scientifique et technologique, dont le siège est         ',&
'situé 3 rue Michel-Ange, 75794 Paris cedex 16.                                ',&
'                                                                               ',&
'Institut National de Recherche en Informatique et en Automatique -             ',&
'Inria, établissement public à caractère scientifique et technologique,      ',&
'dont le siège est situé Domaine de Voluceau, Rocquencourt, BP 105, 78153     ',&
'Le Chesnay cedex.                                                              ',&
'                                                                               ',&
'                                                                               ',&
'    Préambule                                                                 ',&
'                                                                               ',&
'Ce contrat est une licence de logiciel libre dont l''objectif est de           ',&
'conférer aux utilisateurs la liberté de modification et de                   ',&
'redistribution du logiciel régi par cette licence dans le cadre d''un         ',&
'modèle de diffusion en logiciel libre.                                        ',&
'                                                                               ',&
'L''exercice de ces libertés est assorti de certains devoirs à la charge      ',&
'des utilisateurs afin de préserver ce statut au cours des                     ',&
'redistributions ultérieures.                                                  ',&
'                                                                               ',&
'L''accessibilité au code source et les droits de copie, de modification       ',&
'et de redistribution qui en découlent ont pour contrepartie de n''offrir      ',&
'aux utilisateurs qu''une garantie limitée et de ne faire peser sur            ',&
'l''auteur du logiciel, le titulaire des droits patrimoniaux et les             ',&
'concédants successifs qu''une responsabilité restreinte.                     ',&
'                                                                               ',&
'A cet égard l''attention de l''utilisateur est attirée sur les risques       ',&
'associés au chargement, à l''utilisation, à la modification et/ou au        ',&
'développement et à la reproduction du logiciel par l''utilisateur étant     ',&
'donné sa spécificité de logiciel libre, qui peut le rendre complexe à      ',&
'manipuler et qui le réserve donc à des développeurs ou des                  ',&
'professionnels avertis possédant des connaissances informatiques              ',&
'approfondies. Les utilisateurs sont donc invités à charger et tester         ',&
'l''adéquation du logiciel à leurs besoins dans des conditions permettant     ',&
'd''assurer la sécurité de leurs systèmes et/ou de leurs données et, plus   ',&
'généralement, à l''utiliser et l''exploiter dans les mêmes conditions de   ',&
'sécurité. Ce contrat peut être reproduit et diffusé librement, sous        ',&
'réserve de le conserver en l''état, sans ajout ni suppression de clauses.    ',&
'                                                                               ',&
'Ce contrat est susceptible de s''appliquer à tout logiciel dont le            ',&
'titulaire des droits patrimoniaux décide de soumettre l''exploitation aux     ',&
'dispositions qu''il contient.                                                  ',&
'                                                                               ',&
'Une liste de questions fréquemment posées se trouve sur le site web          ',&
'officiel de la famille des licences CeCILL                                     ',&
'(http://www.cecill.info/index.fr.html) pour toute clarification qui            ',&
'serait nécessaire.                                                            ',&
'                                                                               ',&
'                                                                               ',&
'    Article 1 - DEFINITIONS                                                    ',&
'                                                                               ',&
'Dans ce contrat, les termes suivants, lorsqu''ils seront écrits avec une      ',&
'lettre capitale, auront la signification suivante:                             ',&
'                                                                               ',&
'Contrat: désigne le présent contrat de licence, ses éventuelles versions    ',&
'postérieures et annexes.                                                      ',&
'                                                                               ',&
'Logiciel: désigne le logiciel sous sa forme de Code Objet et/ou de Code       ',&
'Source et le cas échéant sa documentation, dans leur état au moment de      ',&
'l''acceptation du Contrat par le Licencié.                                    ',&
'                                                                               ',&
'Logiciel Initial: désigne le Logiciel sous sa forme de Code Source et         ',&
'éventuellement de Code Objet et le cas échéant sa documentation, dans       ',&
'leur état au moment de leur première diffusion sous les termes du Contrat.   ',&
'                                                                               ',&
'Logiciel Modifié: désigne le Logiciel modifié par au moins une              ',&
'Contribution.                                                                  ',&
'                                                                               ',&
'Code Source: désigne l''ensemble des instructions et des lignes de            ',&
'programme du Logiciel et auquel l''accès est nécessaire en vue de            ',&
'modifier le Logiciel.                                                          ',&
'                                                                               ',&
'Code Objet: désigne les fichiers binaires issus de la compilation du          ',&
'Code Source.                                                                   ',&
'                                                                               ',&
'Titulaire: désigne le ou les détenteurs des droits patrimoniaux d''auteur    ',&
'sur le Logiciel Initial.                                                       ',&
'                                                                               ',&
'Licencié: désigne le ou les utilisateurs du Logiciel ayant accepté le       ',&
'Contrat.                                                                       ',&
'                                                                               ',&
'Contributeur: désigne le Licencié auteur d''au moins une Contribution.       ',&
'                                                                               ',&
'Concédant: désigne le Titulaire ou toute personne physique ou morale         ',&
'distribuant le Logiciel sous le Contrat.                                       ',&
'                                                                               ',&
'Contribution: désigne l''ensemble des modifications, corrections,             ',&
'traductions, adaptations et/ou nouvelles fonctionnalités intégrées dans     ',&
'le Logiciel par tout Contributeur, ainsi que tout Module Interne.              ',&
'                                                                               ',&
'Module: désigne un ensemble de fichiers sources y compris leur                ',&
'documentation qui permet de réaliser des fonctionnalités ou services         ',&
'supplémentaires à ceux fournis par le Logiciel.                              ',&
'                                                                               ',&
'Module Externe: désigne tout Module, non dérivé du Logiciel, tel que ce     ',&
'Module et le Logiciel s''exécutent dans des espaces d''adressage              ',&
'différents, l''un appelant l''autre au moment de leur exécution.             ',&
'                                                                               ',&
'Module Interne: désigne tout Module lié au Logiciel de telle sorte           ',&
'qu''ils s''exécutent dans le même espace d''adressage.                       ',&
'                                                                               ',&
'GNU GPL: désigne la GNU General Public License dans sa version 2 ou           ',&
'toute version ultérieure, telle que publiée par Free Software Foundation     ',&
'Inc.                                                                           ',&
'                                                                               ',&
'GNU Affero GPL: désigne la GNU Affero General Public License dans sa          ',&
'version 3 ou toute version ultérieure, telle que publiée par Free            ',&
'Software Foundation Inc.                                                       ',&
'                                                                               ',&
'EUPL: désigne la Licence Publique de l''Union européenne dans sa version     ',&
'1.1 ou toute version ultérieure, telle que publiée par la Commission         ',&
'Européenne.                                                                   ',&
'                                                                               ',&
'Parties: désigne collectivement le Licencié et le Concédant.                ',&
'                                                                               ',&
'Ces termes s''entendent au singulier comme au pluriel.                         ',&
'                                                                               ',&
'                                                                               ',&
'    Article 2 - OBJET                                                          ',&
'                                                                               ',&
'Le Contrat a pour objet la concession par le Concédant au Licencié d''une    ',&
'licence non exclusive, cessible et mondiale du Logiciel telle que              ',&
'définie ci-après à l''article 5 <#etendue> pour toute la durée de          ',&
'protection des droits portant sur ce Logiciel.                                 ',&
'                                                                               ',&
'                                                                               ',&
'    Article 3 - ACCEPTATION                                                    ',&
'                                                                               ',&
'3.1 L''acceptation par le Licencié des termes du Contrat est réputée        ',&
'acquise du fait du premier des faits suivants:                                 ',&
'                                                                               ',&
'  * (i) le chargement du Logiciel par tout moyen notamment par                 ',&
'    téléchargement à partir d''un serveur distant ou par chargement à      ',&
'    partir d''un support physique;                                             ',&
'  * (ii) le premier exercice par le Licencié de l''un quelconque des          ',&
'    droits concédés par le Contrat.                                          ',&
'                                                                               ',&
'3.2 Un exemplaire du Contrat, contenant notamment un avertissement             ',&
'relatif aux spécificités du Logiciel, à la restriction de garantie et à    ',&
'la limitation à un usage par des utilisateurs expérimentés a été mis à   ',&
'disposition du Licencié préalablement à son acceptation telle que           ',&
'définie à l''article 3.1 <#acceptation-acquise> ci dessus et le Licencié    ',&
'reconnaît en avoir pris connaissance.                                         ',&
'                                                                               ',&
'                                                                               ',&
'    Article 4 - ENTREE EN VIGUEUR ET DUREE                                     ',&
'                                                                               ',&
'                                                                               ',&
'      4.1 ENTREE EN VIGUEUR                                                    ',&
'                                                                               ',&
'Le Contrat entre en vigueur à la date de son acceptation par le Licencié     ',&
'telle que définie en 3.1 <#acceptation-acquise>.                              ',&
'                                                                               ',&
'                                                                               ',&
'      4.2 DUREE                                                                ',&
'                                                                               ',&
'Le Contrat produira ses effets pendant toute la durée légale de              ',&
'protection des droits patrimoniaux portant sur le Logiciel.                    ',&
'                                                                               ',&
'                                                                               ',&
'    Article 5 - ETENDUE DES DROITS CONCEDES                                    ',&
'                                                                               ',&
'Le Concédant concède au Licencié, qui accepte, les droits suivants sur      ',&
'le Logiciel pour toutes destinations et pour la durée du Contrat dans         ',&
'les conditions ci-après détaillées.                                         ',&
'                                                                               ',&
'Par ailleurs, si le Concédant détient ou venait à détenir un ou            ',&
'plusieurs brevets d''invention protégeant tout ou partie des                  ',&
'fonctionnalités du Logiciel ou de ses composants, il s''engage à ne pas      ',&
'opposer les éventuels droits conférés par ces brevets aux Licenciés        ',&
'successifs qui utiliseraient, exploiteraient ou modifieraient le               ',&
'Logiciel. En cas de cession de ces brevets, le Concédant s''engage à         ',&
'faire reprendre les obligations du présent alinéa aux cessionnaires.         ',&
'                                                                               ',&
'                                                                               ',&
'      5.1 DROIT D''UTILISATION                                                 ',&
'                                                                               ',&
'Le Licencié est autorisé à utiliser le Logiciel, sans restriction quant     ',&
'aux domaines d''application, étant ci-après précisé que cela comporte:     ',&
'                                                                               ',&
' 1.                                                                            ',&
'                                                                               ',&
'    la reproduction permanente ou provisoire du Logiciel en tout ou            ',&
'    partie par tout moyen et sous toute forme.                                 ',&
'                                                                               ',&
' 2.                                                                            ',&
'                                                                               ',&
'    le chargement, l''affichage, l''exécution, ou le stockage du Logiciel     ',&
'    sur tout support.                                                          ',&
'                                                                               ',&
' 3.                                                                            ',&
'                                                                               ',&
'    la possibilité d''en observer, d''en étudier, ou d''en tester le         ',&
'    fonctionnement afin de déterminer les idées et principes qui sont à     ',&
'    la base de n''importe quel élément de ce Logiciel; et ceci, lorsque      ',&
'    le Licencié effectue toute opération de chargement, d''affichage,        ',&
'    d''exécution, de transmission ou de stockage du Logiciel qu''il est en    ',&
'    droit d''effectuer en vertu du Contrat.                                    ',&
'                                                                               ',&
'                                                                               ',&
'      5.2 DROIT D''APPORTER DES CONTRIBUTIONS                                  ',&
'                                                                               ',&
'Le droit d''apporter des Contributions comporte le droit de traduire,          ',&
'd''adapter, d''arranger ou d''apporter toute autre modification au Logiciel    ',&
'et le droit de reproduire le logiciel en résultant.                           ',&
'                                                                               ',&
'Le Licencié est autorisé à apporter toute Contribution au Logiciel sous     ',&
'réserve de mentionner, de façon explicite, son nom en tant qu''auteur de     ',&
'cette Contribution et la date de création de celle-ci.                        ',&
'                                                                               ',&
'                                                                               ',&
'      5.3 DROIT DE DISTRIBUTION                                                ',&
'                                                                               ',&
'Le droit de distribution comporte notamment le droit de diffuser, de           ',&
'transmettre et de communiquer le Logiciel au public sur tout support et        ',&
'par tout moyen ainsi que le droit de mettre sur le marché à titre            ',&
'onéreux ou gratuit, un ou des exemplaires du Logiciel par tout procédé.     ',&
'                                                                               ',&
'Le Licencié est autorisé à distribuer des copies du Logiciel, modifié ou   ',&
'non, à des tiers dans les conditions ci-après détaillées.                  ',&
'                                                                               ',&
'                                                                               ',&
'        5.3.1 DISTRIBUTION DU LOGICIEL SANS MODIFICATION                       ',&
'                                                                               ',&
'Le Licencié est autorisé à distribuer des copies conformes du Logiciel,     ',&
'sous forme de Code Source ou de Code Objet, à condition que cette             ',&
'distribution respecte les dispositions du Contrat dans leur totalité et       ',&
'soit accompagnée:                                                             ',&
'                                                                               ',&
' 1.                                                                            ',&
'                                                                               ',&
'    d''un exemplaire du Contrat,                                               ',&
'                                                                               ',&
' 2.                                                                            ',&
'                                                                               ',&
'    d''un avertissement relatif à la restriction de garantie et de            ',&
'    responsabilité du Concédant telle que prévue aux articles 8             ',&
'    <#responsabilite> et 9 <#garantie>,                                        ',&
'                                                                               ',&
'et que, dans le cas où seul le Code Objet du Logiciel est redistribué,       ',&
'le Licencié permette un accès effectif au Code Source complet du             ',&
'Logiciel pour une durée d''au moins 3 ans à compter de la distribution du    ',&
'logiciel, étant entendu que le coût additionnel d''acquisition du Code       ',&
'Source ne devra pas excéder le simple coût de transfert des données.        ',&
'                                                                               ',&
'                                                                               ',&
'        5.3.2 DISTRIBUTION DU LOGICIEL MODIFIE                                 ',&
'                                                                               ',&
'Lorsque le Licencié apporte une Contribution au Logiciel, les conditions      ',&
'de distribution du Logiciel Modifié en résultant sont alors soumises à      ',&
'l''intégralité des dispositions du Contrat.                                  ',&
'                                                                               ',&
'Le Licencié est autorisé à distribuer le Logiciel Modifié, sous forme de   ',&
'code source ou de code objet, à condition que cette distribution              ',&
'respecte les dispositions du Contrat dans leur totalité et soit               ',&
'accompagnée:                                                                  ',&
'                                                                               ',&
' 1.                                                                            ',&
'                                                                               ',&
'    d''un exemplaire du Contrat,                                               ',&
'                                                                               ',&
' 2.                                                                            ',&
'                                                                               ',&
'    d''un avertissement relatif à la restriction de garantie et de            ',&
'    responsabilité du Concédant telle que prévue aux articles 8             ',&
'    <#responsabilite> et 9 <#garantie>,                                        ',&
'                                                                               ',&
'et, dans le cas où seul le code objet du Logiciel Modifié est redistribué,  ',&
'                                                                               ',&
' 3.                                                                            ',&
'                                                                               ',&
'    d''une note précisant les conditions d''accès effectif au code source    ',&
'    complet du Logiciel Modifié, pendant une période d''au moins 3 ans à    ',&
'    compter de la distribution du Logiciel Modifié, étant entendu que le     ',&
'    coût additionnel d''acquisition du code source ne devra pas excéder      ',&
'    le simple coût de transfert des données.                                 ',&
'                                                                               ',&
'                                                                               ',&
'        5.3.3 DISTRIBUTION DES MODULES EXTERNES                                ',&
'                                                                               ',&
'Lorsque le Licencié a développé un Module Externe les conditions du         ',&
'Contrat ne s''appliquent pas à ce Module Externe, qui peut être distribué   ',&
'sous un contrat de licence différent.                                         ',&
'                                                                               ',&
'                                                                               ',&
'        5.3.4 COMPATIBILITE AVEC D''AUTRES LICENCES                            ',&
'                                                                               ',&
'Le Licencié peut inclure un code soumis aux dispositions d''une des           ',&
'versions de la licence GNU GPL, GNU Affero GPL et/ou EUPL dans le              ',&
'Logiciel modifié ou non et distribuer l''ensemble sous les conditions de      ',&
'la même version de la licence GNU GPL, GNU Affero GPL et/ou EUPL.             ',&
'                                                                               ',&
'Le Licencié peut inclure le Logiciel modifié ou non dans un code soumis      ',&
'aux dispositions d''une des versions de la licence GNU GPL, GNU Affero         ',&
'GPL et/ou EUPL et distribuer l''ensemble sous les conditions de la même       ',&
'version de la licence GNU GPL, GNU Affero GPL et/ou EUPL.                      ',&
'                                                                               ',&
'                                                                               ',&
'    Article 6 - PROPRIETE INTELLECTUELLE                                       ',&
'                                                                               ',&
'                                                                               ',&
'      6.1 SUR LE LOGICIEL INITIAL                                              ',&
'                                                                               ',&
'Le Titulaire est détenteur des droits patrimoniaux sur le Logiciel            ',&
'Initial. Toute utilisation du Logiciel Initial est soumise au respect          ',&
'des conditions dans lesquelles le Titulaire a choisi de diffuser son           ',&
'oeuvre et nul autre n''a la faculté de modifier les conditions de             ',&
'diffusion de ce Logiciel Initial.                                              ',&
'                                                                               ',&
'Le Titulaire s''engage à ce que le Logiciel Initial reste au moins régi      ',&
'par le Contrat et ce, pour la durée visée à l''article 4.2 <#duree>.        ',&
'                                                                               ',&
'                                                                               ',&
'      6.2 SUR LES CONTRIBUTIONS                                                ',&
'                                                                               ',&
'Le Licencié qui a développé une Contribution est titulaire sur celle-ci     ',&
'des droits de propriété intellectuelle dans les conditions définies par     ',&
'la législation applicable.                                                    ',&
'                                                                               ',&
'                                                                               ',&
'      6.3 SUR LES MODULES EXTERNES                                             ',&
'                                                                               ',&
'Le Licencié qui a développé un Module Externe est titulaire sur celui-ci    ',&
'des droits de propriété intellectuelle dans les conditions définies par     ',&
'la législation applicable et reste libre du choix du contrat régissant       ',&
'sa diffusion.                                                                  ',&
'                                                                               ',&
'                                                                               ',&
'      6.4 DISPOSITIONS COMMUNES                                                ',&
'                                                                               ',&
'Le Licencié s''engage expressément:                                          ',&
'                                                                               ',&
' 1.                                                                            ',&
'                                                                               ',&
'    à ne pas supprimer ou modifier de quelque manière que ce soit les        ',&
'    mentions de propriété intellectuelle apposées sur le Logiciel;          ',&
'                                                                               ',&
' 2.                                                                            ',&
'                                                                               ',&
'    à reproduire à l''identique lesdites mentions de propriété             ',&
'    intellectuelle sur les copies du Logiciel modifié ou non.                 ',&
'                                                                               ',&
'Le Licencié s''engage à ne pas porter atteinte, directement ou               ',&
'indirectement, aux droits de propriété intellectuelle du Titulaire et/ou     ',&
'des Contributeurs sur le Logiciel et à prendre, le cas échéant, à          ',&
'l''égard de son personnel toutes les mesures nécessaires pour assurer le     ',&
'respect des dits droits de propriété intellectuelle du Titulaire et/ou       ',&
'des Contributeurs.                                                             ',&
'                                                                               ',&
'                                                                               ',&
'    Article 7 - SERVICES ASSOCIES                                              ',&
'                                                                               ',&
'7.1 Le Contrat n''oblige en aucun cas le Concédant à la réalisation de      ',&
'prestations d''assistance technique ou de maintenance du Logiciel.             ',&
'                                                                               ',&
'Cependant le Concédant reste libre de proposer ce type de services. Les       ',&
'termes et conditions d''une telle assistance technique et/ou d''une telle      ',&
'maintenance seront alors déterminés dans un acte séparé. Ces actes de      ',&
'maintenance et/ou assistance technique n''engageront que la seule              ',&
'responsabilité du Concédant qui les propose.                                 ',&
'                                                                               ',&
'7.2 De même, tout Concédant est libre de proposer, sous sa seule             ',&
'responsabilité, à ses licenciés une garantie, qui n''engagera que lui,      ',&
'lors de la redistribution du Logiciel et/ou du Logiciel Modifié et ce,        ',&
'dans les conditions qu''il souhaite. Cette garantie et les modalités          ',&
'financières de son application feront l''objet d''un acte séparé entre le   ',&
'Concédant et le Licencié.                                                    ',&
'                                                                               ',&
'                                                                               ',&
'    Article 8 - RESPONSABILITE                                                 ',&
'                                                                               ',&
'8.1 Sous réserve des dispositions de l''article 8.2                           ',&
'<#limite-responsabilite>, le Licencié a la faculté, sous réserve de         ',&
'prouver la faute du Concédant concerné, de solliciter la réparation du      ',&
'préjudice direct qu''il subirait du fait du Logiciel et dont il apportera     ',&
'la preuve.                                                                     ',&
'                                                                               ',&
'8.2 La responsabilité du Concédant est limitée aux engagements pris en      ',&
'application du Contrat et ne saurait être engagée en raison notamment:       ',&
'(i) des dommages dus à l''inexécution, totale ou partielle, de ses           ',&
'obligations par le Licencié, (ii) des dommages directs ou indirects           ',&
'découlant de l''utilisation ou des performances du Logiciel subis par le      ',&
'Licencié et (iii) plus généralement d''un quelconque dommage indirect. En   ',&
'particulier, les Parties conviennent expressément que tout préjudice         ',&
'financier ou commercial (par exemple perte de données, perte de               ',&
'bénéfices, perte d''exploitation, perte de clientèle ou de commandes,       ',&
'manque à gagner, trouble commercial quelconque) ou toute action dirigée      ',&
'contre le Licencié par un tiers, constitue un dommage indirect et             ',&
'n''ouvre pas droit à réparation par le Concédant.                           ',&
'                                                                               ',&
'                                                                               ',&
'    Article 9 - GARANTIE                                                       ',&
'                                                                               ',&
'9.1 Le Licencié reconnaît que l''état actuel des connaissances              ',&
'scientifiques et techniques au moment de la mise en circulation du             ',&
'Logiciel ne permet pas d''en tester et d''en vérifier toutes les              ',&
'utilisations ni de détecter l''existence d''éventuels défauts. L''attention ',&
'du Licencié a été attirée sur ce point sur les risques associés au        ',&
'chargement, à l''utilisation, la modification et/ou au développement et à   ',&
'la reproduction du Logiciel qui sont réservés à des utilisateurs avertis.   ',&
'                                                                               ',&
'Il relève de la responsabilité du Licencié de contrôler, par tous          ',&
'moyens, l''adéquation du produit à ses besoins, son bon fonctionnement et    ',&
'de s''assurer qu''il ne causera pas de dommages aux personnes et aux biens.    ',&
'                                                                               ',&
'9.2 Le Concédant déclare de bonne foi être en droit de concéder            ',&
'l''ensemble des droits attachés au Logiciel (comprenant notamment les         ',&
'droits visés à l''article 5 <#etendue>).                                     ',&
'                                                                               ',&
'9.3 Le Licencié reconnaît que le Logiciel est fourni "en l''état" par le    ',&
'Concédant sans autre garantie, expresse ou tacite, que celle prévue à       ',&
'l''article 9.2 <#bonne-foi> et notamment sans aucune garantie sur sa           ',&
'valeur commerciale, son caractère sécurisé, innovant ou pertinent.          ',&
'                                                                               ',&
'En particulier, le Concédant ne garantit pas que le Logiciel est exempt       ',&
'd''erreur, qu''il fonctionnera sans interruption, qu''il sera compatible       ',&
'avec l''équipement du Licencié et sa configuration logicielle ni qu''il      ',&
'remplira les besoins du Licencié.                                             ',&
'                                                                               ',&
'9.4 Le Concédant ne garantit pas, de manière expresse ou tacite, que le      ',&
'Logiciel ne porte pas atteinte à un quelconque droit de propriété           ',&
'intellectuelle d''un tiers portant sur un brevet, un logiciel ou sur tout      ',&
'autre droit de propriété. Ainsi, le Concédant exclut toute garantie au      ',&
'profit du Licencié contre les actions en contrefaçon qui pourraient être    ',&
'diligentées au titre de l''utilisation, de la modification, et de la          ',&
'redistribution du Logiciel. Néanmoins, si de telles actions sont              ',&
'exercées contre le Licencié, le Concédant lui apportera son expertise       ',&
'technique et juridique pour sa défense. Cette expertise technique et          ',&
'juridique est déterminée au cas par cas entre le Concédant concerné et     ',&
'le Licencié dans le cadre d''un protocole d''accord. Le Concédant dégage    ',&
'toute responsabilité quant à l''utilisation de la dénomination du           ',&
'Logiciel par le Licencié. Aucune garantie n''est apportée quant à           ',&
'l''existence de droits antérieurs sur le nom du Logiciel et sur               ',&
'l''existence d''une marque.                                                    ',&
'                                                                               ',&
'                                                                               ',&
'    Article 10 - RESILIATION                                                   ',&
'                                                                               ',&
'10.1 En cas de manquement par le Licencié aux obligations mises à sa         ',&
'charge par le Contrat, le Concédant pourra résilier de plein droit le        ',&
'Contrat trente (30) jours après notification adressée au Licencié et        ',&
'restée sans effet.                                                            ',&
'                                                                               ',&
'10.2 Le Licencié dont le Contrat est résilié n''est plus autorisé à       ',&
'utiliser, modifier ou distribuer le Logiciel. Cependant, toutes les            ',&
'licences qu''il aura concédées antérieurement à la résiliation du Contrat ',&
'resteront valides sous réserve qu''elles aient été effectuées en           ',&
'conformité avec le Contrat.                                                   ',&
'                                                                               ',&
'                                                                               ',&
'    Article 11 - DISPOSITIONS DIVERSES                                         ',&
'                                                                               ',&
'                                                                               ',&
'      11.1 CAUSE EXTERIEURE                                                    ',&
'                                                                               ',&
'Aucune des Parties ne sera responsable d''un retard ou d''une défaillance     ',&
'd''exécution du Contrat qui serait dû à un cas de force majeure, un cas     ',&
'fortuit ou une cause extérieure, telle que, notamment, le mauvais             ',&
'fonctionnement ou les interruptions du réseau électrique ou de               ',&
'télécommunication, la paralysie du réseau liée à une attaque              ',&
'informatique, l''intervention des autorités gouvernementales, les             ',&
'catastrophes naturelles, les dégâts des eaux, les tremblements de terre,     ',&
'le feu, les explosions, les grèves et les conflits sociaux, l''état de       ',&
'guerre...                                                                      ',&
'                                                                               ',&
'11.2 Le fait, par l''une ou l''autre des Parties, d''omettre en une ou         ',&
'plusieurs occasions de se prévaloir d''une ou plusieurs dispositions du       ',&
'Contrat, ne pourra en aucun cas impliquer renonciation par la Partie           ',&
'intéressée à s''en prévaloir ultérieurement.                              ',&
'                                                                               ',&
'11.3 Le Contrat annule et remplace toute convention antérieure, écrite       ',&
'ou orale, entre les Parties sur le même objet et constitue l''accord          ',&
'entier entre les Parties sur cet objet. Aucune addition ou modification        ',&
'aux termes du Contrat n''aura d''effet à l''égard des Parties à moins       ',&
'd''être faite par écrit et signée par leurs représentants dûment habilités.',&
'                                                                                 ',&
'11.4 Dans l''hypothèse où une ou plusieurs des dispositions du Contrat         ',&
's''avèrerait contraire à une loi ou à un texte applicable, existants ou       ',&
'futurs, cette loi ou ce texte prévaudrait, et les Parties feraient les          ',&
'amendements nécessaires pour se conformer à cette loi ou à ce texte.          ',&
'Toutes les autres dispositions resteront en vigueur. De même, la                ',&
'nullité, pour quelque raison que ce soit, d''une des dispositions du            ',&
'Contrat ne saurait entraîner la nullité de l''ensemble du Contrat.             ',&
'                                                                                 ',&
'                                                                                 ',&
'      11.5 LANGUE                                                                ',&
'                                                                                 ',&
'Le Contrat est rédigé en langue française et en langue anglaise, ces          ',&
'deux versions faisant également foi.                                            ',&
'                                                                                 ',&
'                                                                                 ',&
'    Article 12 - NOUVELLES VERSIONS DU CONTRAT                                   ',&
'                                                                                 ',&
'12.1 Toute personne est autorisée à copier et distribuer des copies de         ',&
'ce Contrat.                                                                      ',&
'                                                                                 ',&
'12.2 Afin d''en préserver la cohérence, le texte du Contrat est protégé      ',&
'et ne peut être modifié que par les auteurs de la licence, lesquels se         ',&
'réservent le droit de publier périodiquement des mises à jour ou de           ',&
'nouvelles versions du Contrat, qui posséderont chacune un numéro               ',&
'distinct. Ces versions ultérieures seront susceptibles de prendre en            ',&
'compte de nouvelles problématiques rencontrées par les logiciels libres.       ',&
'                                                                                 ',&
'12.3 Tout Logiciel diffusé sous une version donnée du Contrat ne pourra        ',&
'faire l''objet d''une diffusion ultérieure que sous la même version du         ',&
'Contrat ou une version postérieure, sous réserve des dispositions de           ',&
'l''article 5.3.4 <#compatibilite>.                                               ',&
'                                                                                 ',&
'                                                                                 ',&
'    Article 13 - LOI APPLICABLE ET COMPETENCE TERRITORIALE                       ',&
'                                                                                 ',&
'13.1 Le Contrat est régi par la loi française. Les Parties conviennent         ',&
'de tenter de régler à l''amiable les différends ou litiges qui                ',&
'viendraient à se produire par suite ou à l''occasion du Contrat.               ',&
'                                                                                 ',&
'13.2 A défaut d''accord amiable dans un délai de deux (2) mois à compter      ',&
'de leur survenance et sauf situation relevant d''une procédure d''urgence,      ',&
'les différends ou litiges seront portés par la Partie la plus diligente        ',&
'devant les Tribunaux compétents de Paris.                                       ',&
'                                                                                 ',&
'']
if(present(topic_only))then
   if(size(textblock)>0)then
      if(topic_only) textblock=[character(len=len(textblock(1))) :: textblock(1)]
   else
      textblock=['']
   endif
endif
case('17','cern-ohl-p-2.0')
textblock=[ CHARACTER(LEN=128) :: &
'cern-ohl-p-2.0',&
'              ',&
'CERN Open Hardware Licence Version 2 - Permissive',&
'                                                 ',&
'                                                 ',&
'Preamble                                         ',&
'                                                 ',&
'CERN has developed this licence to promote collaboration among hardware',&
'designers and to provide a legal tool which supports the freedom to use,',&
'study, modify, share and distribute hardware designs and products based on',&
'those designs. Version 2 of the CERN Open Hardware Licence comes in three ',&
'variants: this licence, CERN-OHL-P (permissive); and two reciprocal licences:',&
'CERN-OHL-W (weakly reciprocal) and CERN-OHL-S (strongly reciprocal).         ',&
'                                                                             ',&
'The CERN-OHL-P is copyright CERN 2020. Anyone is welcome to use it, in       ',&
'unmodified form only.                                                        ',&
'                                                                             ',&
'Use of this Licence does not imply any endorsement by CERN of any Licensor or',&
'their designs nor does it imply any involvement by CERN in their development.',&
'                                                                             ',&
'                                                                             ',&
'1 Definitions                                                                ',&
'                                                                             ',&
'  1.1 ''Licence'' means this CERN-OHL-P.                                     ',&
'                                                                             ',&
'  1.2 ''Source'' means information such as design materials or digital code  ',&
'      which can be applied to Make or test a Product or to prepare a Product ',&
'      for use, Conveyance or sale, regardless of its medium or how it is     ',&
'      expressed. It may include Notices.                                     ',&
'                                                                             ',&
'  1.3 ''Covered Source'' means Source that is explicitly made available under',&
'      this Licence.                                                          ',&
'                                                                             ',&
'  1.4 ''Product'' means any device, component, work or physical object, whether',&
'      in finished or intermediate form, arising from the use, application or   ',&
'      processing of Covered Source.                                            ',&
'                                                                               ',&
'  1.5 ''Make'' means to create or configure something, whether by manufacture, ',&
'      assembly, compiling, loading or applying Covered Source or another       ',&
'      Product or otherwise.                                                    ',&
'                                                                               ',&
'  1.6 ''Notice'' means copyright, acknowledgement and trademark notices,       ',&
'      references to the location of any Notices, modification notices          ',&
'      (subsection 3.3(b)) and all notices that refer to this Licence and to    ',&
'      the disclaimer of warranties that are included in the Covered Source.    ',&
'                                                                               ',&
'  1.7 ''Licensee'' or ''You'' means any person exercising rights under this    ',&
'      Licence.                                                                 ',&
'                                                                               ',&
'  1.8 ''Licensor'' means a person who creates Source or modifies Covered Source',&
'      and subsequently Conveys the resulting Covered Source under the terms    ',&
'      and conditions of this Licence. A person may be a Licensee and a         ',&
'      Licensor at the same time.                                               ',&
'                                                                               ',&
'  1.9 ''Convey'' means to communicate to the public or distribute.             ',&
'                                                                               ',&
'                                                                               ',&
'2 Applicability                                                                ',&
'                                                                               ',&
'  2.1 This Licence governs the use, copying, modification, Conveying of        ',&
'      Covered Source and Products, and the Making of Products. By exercising   ',&
'      any right granted under this Licence, You irrevocably accept these terms ',&
'      and conditions.                                                          ',&
'                                                                               ',&
'  2.2 This Licence is granted by the Licensor directly to You, and shall apply ',&
'      worldwide and without limitation in time.                                ',&
'                                                                               ',&
'  2.3 You shall not attempt to restrict by contract or otherwise the rights    ',&
'      granted under this Licence to other Licensees.                           ',&
'                                                                               ',&
'  2.4 This Licence is not intended to restrict fair use, fair dealing, or any  ',&
'      other similar right.                                                     ',&
'                                                                               ',&
'                                                                               ',&
'3 Copying, Modifying and Conveying Covered Source                              ',&
'                                                                               ',&
'  3.1 You may copy and Convey verbatim copies of Covered Source, in any        ',&
'      medium, provided You retain all Notices.                                 ',&
'                                                                               ',&
'  3.2 You may modify Covered Source, other than Notices.                       ',&
'                                                                               ',&
'      You may only delete Notices if they are no longer applicable to the      ',&
'      corresponding Covered Source as modified by You and You may add          ',&
'      additional Notices applicable to Your modifications.                     ',&
'                                                                               ',&
'  3.3 You may Convey modified Covered Source (with the effect that You shall   ',&
'      also become a Licensor) provided that You:                               ',&
'                                                                               ',&
'       a) retain Notices as required in subsection 3.2; and                    ',&
'                                                                               ',&
'       b) add a Notice to the modified Covered Source stating that You have    ',&
'          modified it, with the date and brief description of how You have     ',&
'          modified it.                                                         ',&
'                                                                               ',&
'  3.4 You may Convey Covered Source or modified Covered Source under licence   ',&
'      terms which differ from the terms of this Licence provided that You:     ',&
'                                                                               ',&
'       a) comply at all times with subsection 3.3; and                         ',&
'                                                                               ',&
'       b) provide a copy of this Licence to anyone to whom You Convey Covered  ',&
'          Source or modified Covered Source.                                   ',&
'                                                                               ',&
'                                                                               ',&
'4 Making and Conveying Products                                                ',&
'                                                                               ',&
'You may Make Products, and/or Convey them, provided that You ensure that the   ',&
'recipient of the Product has access to any Notices applicable to the Product.  ',&
'                                                                               ',&
'                                                                               ',&
'5 DISCLAIMER AND LIABILITY                                                     ',&
'                                                                               ',&
'  5.1 DISCLAIMER OF WARRANTY -- The Covered Source and any Products are        ',&
'      provided ''as is'' and any express or implied warranties, including, but ',&
'      not limited to, implied warranties of merchantability, of satisfactory   ',&
'      quality, non-infringement of third party rights, and fitness for a       ',&
'      particular purpose or use are disclaimed in respect of any Source or     ',&
'      Product to the maximum extent permitted by law. The Licensor makes no    ',&
'      representation that any Source or Product does not or will not infringe  ',&
'      any patent, copyright, trade secret or other proprietary right. The      ',&
'      entire risk as to the use, quality, and performance of any Source or     ',&
'      Product shall be with You and not the Licensor. This disclaimer of       ',&
'      warranty is an essential part of this Licence and a condition for the    ',&
'      grant of any rights granted under this Licence.                          ',&
'                                                                               ',&
'  5.2 EXCLUSION AND LIMITATION OF LIABILITY -- The Licensor shall, to the      ',&
'      maximum extent permitted by law, have no liability for direct, indirect, ',&
'      special, incidental, consequential, exemplary, punitive or other damages ',&
'      of any character including, without limitation, procurement of           ',&
'      substitute goods or services, loss of use, data or profits, or business  ',&
'      interruption, however caused and on any theory of contract, warranty,    ',&
'      tort (including negligence), product liability or otherwise, arising in  ',&
'      any way in relation to the Covered Source, modified Covered Source       ',&
'      and/or the Making or Conveyance of a Product, even if advised of the     ',&
'      possibility of such damages, and You shall hold the Licensor(s) free and ',&
'      harmless from any liability, costs, damages, fees and expenses,          ',&
'      including claims by third parties, in relation to such use.              ',&
'                                                                               ',&
'                                                                               ',&
'6 Patents                                                                      ',&
'                                                                               ',&
'  6.1 Subject to the terms and conditions of this Licence, each Licensor       ',&
'      hereby grants to You a perpetual, worldwide, non-exclusive, no-charge,   ',&
'      royalty-free, irrevocable (except as stated in this section 6, or where  ',&
'      terminated by the Licensor for cause) patent licence to Make, have Made, ',&
'      use, offer to sell, sell, import, and otherwise transfer the Covered     ',&
'      Source and Products, where such licence applies only to those patent     ',&
'      claims licensable by such Licensor that are necessarily infringed by     ',&
'      exercising rights under the Covered Source as Conveyed by that Licensor. ',&
'                                                                               ',&
'  6.2 If You institute patent litigation against any entity (including a       ',&
'      cross-claim or counterclaim in a lawsuit) alleging that the Covered      ',&
'      Source or a Product constitutes direct or contributory patent            ',&
'      infringement, or You seek any declaration that a patent licensed to You  ',&
'      under this Licence is invalid or unenforceable then any rights granted   ',&
'      to You under this Licence shall terminate as of the date such process is ',&
'      initiated.                                                               ',&
'                                                                               ',&
'                                                                               ',&
'7 General                                                                      ',&
'                                                                               ',&
'  7.1 If any provisions of this Licence are or subsequently become invalid or  ',&
'      unenforceable for any reason, the remaining provisions shall remain      ',&
'      effective.                                                               ',&
'                                                                               ',&
'  7.2 You shall not use any of the name (including acronyms and                ',&
'      abbreviations), image, or logo by which the Licensor or CERN is known,   ',&
'      except where needed to comply with section 3, or where the use is        ',&
'      otherwise allowed by law. Any such permitted use shall be factual and    ',&
'      shall not be made so as to suggest any kind of endorsement or            ',&
'      implication of involvement by the Licensor or its personnel.             ',&
'                                                                               ',&
'  7.3 CERN may publish updated versions and variants of this Licence which it  ',&
'      considers to be in the spirit of this version, but may differ in detail  ',&
'      to address new problems or concerns. New versions will be published with ',&
'      a unique version number and a variant identifier specifying the variant. ',&
'      If the Licensor has specified that a given variant applies to the        ',&
'      Covered Source without specifying a version, You may treat that Covered  ',&
'      Source as being released under any version of the CERN-OHL with that     ',&
'      variant. If no variant is specified, the Covered Source shall be treated ',&
'      as being released under CERN-OHL-S. The Licensor may also specify that   ',&
'      the Covered Source is subject to a specific version of the CERN-OHL or   ',&
'      any later version in which case You may apply this or any later version  ',&
'      of CERN-OHL with the same variant identifier published by CERN.          ',&
'                                                                               ',&
'  7.4 This Licence shall not be enforceable except by a Licensor acting as     ',&
'      such, and third party beneficiary rights are specifically excluded.      ',&
'                                                                               ',&
'']
if(present(topic_only))then
   if(size(textblock)>0)then
      if(topic_only) textblock=[character(len=len(textblock(1))) :: textblock(1)]
   else
      textblock=['']
   endif
endif
case('18','cern-ohl-s-2.0')
textblock=[ CHARACTER(LEN=128) :: &
'cern-ohl-s-2.0',&
'              ',&
'CERN Open Hardware Licence Version 2 - Strongly Reciprocal',&
'                                                          ',&
'                                                          ',&
'Preamble                                                  ',&
'                                                          ',&
'CERN has developed this licence to promote collaboration among hardware',&
'designers and to provide a legal tool which supports the freedom to use,',&
'study, modify, share and distribute hardware designs and products based on',&
'those designs. Version 2 of the CERN Open Hardware Licence comes in three ',&
'variants: CERN-OHL-P (permissive); and two reciprocal licences: CERN-OHL-W',&
'(weakly reciprocal) and this licence, CERN-OHL-S (strongly reciprocal).   ',&
'                                                                          ',&
'The CERN-OHL-S is copyright CERN 2020. Anyone is welcome to use it, in    ',&
'unmodified form only.                                                     ',&
'                                                                          ',&
'Use of this Licence does not imply any endorsement by CERN of any Licensor or',&
'their designs nor does it imply any involvement by CERN in their development.',&
'                                                                             ',&
'                                                                             ',&
'1 Definitions                                                                ',&
'                                                                             ',&
'  1.1 ''Licence'' means this CERN-OHL-S.                                     ',&
'                                                                             ',&
'  1.2 ''Compatible Licence'' means                                           ',&
'                                                                             ',&
'       a) any earlier version of the CERN Open Hardware licence, or          ',&
'                                                                             ',&
'       b) any version of the CERN-OHL-S, or                                  ',&
'                                                                             ',&
'       c) any licence which permits You to treat the Source to which it      ',&
'          applies as licensed under CERN-OHL-S provided that on Conveyance of',&
'          any such Source, or any associated Product You treat the Source in ',&
'          question as being licensed under CERN-OHL-S.                       ',&
'                                                                             ',&
'  1.3 ''Source'' means information such as design materials or digital code  ',&
'      which can be applied to Make or test a Product or to prepare a Product ',&
'      for use, Conveyance or sale, regardless of its medium or how it is     ',&
'      expressed. It may include Notices.                                     ',&
'                                                                             ',&
'  1.4 ''Covered Source'' means Source that is explicitly made available under',&
'      this Licence.                                                          ',&
'                                                                             ',&
'  1.5 ''Product'' means any device, component, work or physical object, whether',&
'      in finished or intermediate form, arising from the use, application or   ',&
'      processing of Covered Source.                                            ',&
'                                                                               ',&
'  1.6 ''Make'' means to create or configure something, whether by manufacture, ',&
'      assembly, compiling, loading or applying Covered Source or another       ',&
'      Product or otherwise.                                                    ',&
'                                                                               ',&
'  1.7 ''Available Component'' means any part, sub-assembly, library or code    ',&
'      which:                                                                   ',&
'                                                                               ',&
'       a) is licensed to You as Complete Source under a Compatible Licence; or ',&
'                                                                               ',&
'       b) is available, at the time a Product or the Source containing it is   ',&
'          first Conveyed, to You and any other prospective licensees           ',&
'                                                                               ',&
'            i) as a physical part with sufficient rights and information       ',&
'               (including any configuration and programming files and          ',&
'               information about its characteristics and interfaces) to enable ',&
'               it either to be Made itself, or to be sourced and used to Make  ',&
'               the Product; or                                                 ',&
'           ii) as part of the normal distribution of a tool used to design or  ',&
'               Make the Product.                                               ',&
'                                                                               ',&
'  1.8 ''Complete Source'' means the set of all Source necessary to Make a      ',&
'      Product, in the preferred form for making modifications, including       ',&
'      necessary installation and interfacing information both for the Product, ',&
'      and for any included Available Components.  If the format is             ',&
'      proprietary, it must also be made available in a format (if the          ',&
'      proprietary tool can create it) which is viewable with a tool available  ',&
'      to potential licensees and licensed under a licence approved by the Free ',&
'      Software Foundation or the Open Source Initiative. Complete Source need  ',&
'      not include the Source of any Available Component, provided that You     ',&
'      include in the Complete Source sufficient information to enable a        ',&
'      recipient to Make or source and use the Available Component to Make the  ',&
'      Product.                                                                 ',&
'                                                                               ',&
'  1.9 ''Source Location'' means a location where a Licensor has placed Covered ',&
'      Source, and which that Licensor reasonably believes will remain easily   ',&
'      accessible for at least three years for anyone to obtain a digital copy. ',&
'                                                                               ',&
' 1.10 ''Notice'' means copyright, acknowledgement and trademark notices, Source',&
'      Location references, modification notices (subsection 3.3(b)) and all    ',&
'      notices that refer to this Licence and to the disclaimer of warranties   ',&
'      that are included in the Covered Source.                                 ',&
'                                                                               ',&
' 1.11 ''Licensee'' or ''You'' means any person exercising rights under this    ',&
'      Licence.                                                                 ',&
'                                                                               ',&
' 1.12 ''Licensor'' means a natural or legal person who creates or modifies     ',&
'      Covered Source. A person may be a Licensee and a Licensor at the same    ',&
'      time.                                                                    ',&
'                                                                               ',&
' 1.13 ''Convey'' means to communicate to the public or distribute.             ',&
'                                                                               ',&
'                                                                               ',&
'2 Applicability                                                                ',&
'                                                                               ',&
'  2.1 This Licence governs the use, copying, modification, Conveying of        ',&
'      Covered Source and Products, and the Making of Products. By exercising   ',&
'      any right granted under this Licence, You irrevocably accept these terms ',&
'      and conditions.                                                          ',&
'                                                                               ',&
'  2.2 This Licence is granted by the Licensor directly to You, and shall apply ',&
'      worldwide and without limitation in time.                                ',&
'                                                                               ',&
'  2.3 You shall not attempt to restrict by contract or otherwise the rights    ',&
'      granted under this Licence to other Licensees.                           ',&
'                                                                               ',&
'  2.4 This Licence is not intended to restrict fair use, fair dealing, or any  ',&
'      other similar right.                                                     ',&
'                                                                               ',&
'                                                                               ',&
'3 Copying, Modifying and Conveying Covered Source                              ',&
'                                                                               ',&
'  3.1 You may copy and Convey verbatim copies of Covered Source, in any        ',&
'      medium, provided You retain all Notices.                                 ',&
'                                                                               ',&
'  3.2 You may modify Covered Source, other than Notices, provided that You     ',&
'      irrevocably undertake to make that modified Covered Source available     ',&
'      from a Source Location should You Convey a Product in circumstances      ',&
'      where the recipient does not otherwise receive a copy of the modified    ',&
'      Covered Source. In each case subsection 3.3 shall apply.                 ',&
'                                                                               ',&
'      You may only delete Notices if they are no longer applicable to the      ',&
'      corresponding Covered Source as modified by You and You may add          ',&
'      additional Notices applicable to Your modifications.  Including Covered  ',&
'      Source in a larger work is modifying the Covered Source, and the larger  ',&
'      work becomes modified Covered Source.                                    ',&
'                                                                               ',&
'  3.3 You may Convey modified Covered Source (with the effect that You shall   ',&
'      also become a Licensor) provided that You:                               ',&
'                                                                               ',&
'       a) retain Notices as required in subsection 3.2;                        ',&
'                                                                               ',&
'       b) add a Notice to the modified Covered Source stating that You have    ',&
'          modified it, with the date and brief description of how You have     ',&
'          modified it;                                                         ',&
'                                                                               ',&
'       c) add a Source Location Notice for the modified Covered Source if You  ',&
'          Convey in circumstances where the recipient does not otherwise       ',&
'          receive a copy of the modified Covered Source; and                   ',&
'                                                                               ',&
'       d) license the modified Covered Source under the terms and conditions   ',&
'          of this Licence (or, as set out in subsection 8.3, a later version,  ',&
'          if permitted by the licence of the original Covered Source). Such    ',&
'          modified Covered Source must be licensed as a whole, but excluding   ',&
'          Available Components contained in it, which remain licensed under    ',&
'          their own applicable licences.                                       ',&
'                                                                               ',&
'                                                                               ',&
'4 Making and Conveying Products                                                ',&
'                                                                               ',&
'You may Make Products, and/or Convey them, provided that You either provide    ',&
'each recipient with a copy of the Complete Source or ensure that each          ',&
'recipient is notified of the Source Location of the Complete Source. That      ',&
'Complete Source is Covered Source, and You must accordingly satisfy Your       ',&
'obligations set out in subsection 3.3. If specified in a Notice, the Product   ',&
'must visibly and securely display the Source Location on it or its packaging   ',&
'or documentation in the manner specified in that Notice.                       ',&
'                                                                               ',&
'                                                                               ',&
'5 Research and Development                                                     ',&
'                                                                               ',&
'You may Convey Covered Source, modified Covered Source or Products to a legal  ',&
'entity carrying out development, testing or quality assurance work on Your     ',&
'behalf provided that the work is performed on terms which prevent the entity   ',&
'from both using the Source or Products for its own internal purposes and       ',&
'Conveying the Source or Products or any modifications to them to any person    ',&
'other than You. Any modifications made by the entity shall be deemed to be     ',&
'made by You pursuant to subsection 3.2.                                        ',&
'                                                                               ',&
'                                                                               ',&
'6 DISCLAIMER AND LIABILITY                                                     ',&
'                                                                               ',&
'  6.1 DISCLAIMER OF WARRANTY -- The Covered Source and any Products are        ',&
'      provided ''as is'' and any express or implied warranties, including, but ',&
'      not limited to, implied warranties of merchantability, of satisfactory   ',&
'      quality, non-infringement of third party rights, and fitness for a       ',&
'      particular purpose or use are disclaimed in respect of any Source or     ',&
'      Product to the maximum extent permitted by law. The Licensor makes no    ',&
'      representation that any Source or Product does not or will not infringe  ',&
'      any patent, copyright, trade secret or other proprietary right. The      ',&
'      entire risk as to the use, quality, and performance of any Source or     ',&
'      Product shall be with You and not the Licensor. This disclaimer of       ',&
'      warranty is an essential part of this Licence and a condition for the    ',&
'      grant of any rights granted under this Licence.                          ',&
'                                                                               ',&
'  6.2 EXCLUSION AND LIMITATION OF LIABILITY -- The Licensor shall, to the      ',&
'      maximum extent permitted by law, have no liability for direct, indirect, ',&
'      special, incidental, consequential, exemplary, punitive or other damages ',&
'      of any character including, without limitation, procurement of           ',&
'      substitute goods or services, loss of use, data or profits, or business  ',&
'      interruption, however caused and on any theory of contract, warranty,    ',&
'      tort (including negligence), product liability or otherwise, arising in  ',&
'      any way in relation to the Covered Source, modified Covered Source       ',&
'      and/or the Making or Conveyance of a Product, even if advised of the     ',&
'      possibility of such damages, and You shall hold the Licensor(s) free and ',&
'      harmless from any liability, costs, damages, fees and expenses,          ',&
'      including claims by third parties, in relation to such use.              ',&
'                                                                               ',&
'                                                                               ',&
'7 Patents                                                                      ',&
'                                                                               ',&
'  7.1 Subject to the terms and conditions of this Licence, each Licensor       ',&
'      hereby grants to You a perpetual, worldwide, non-exclusive, no-charge,   ',&
'      royalty-free, irrevocable (except as stated in subsections 7.2 and 8.4)  ',&
'      patent licence to Make, have Made, use, offer to sell, sell, import, and ',&
'      otherwise transfer the Covered Source and Products, where such licence   ',&
'      applies only to those patent claims licensable by such Licensor that are ',&
'      necessarily infringed by exercising rights under the Covered Source as   ',&
'      Conveyed by that Licensor.                                               ',&
'                                                                               ',&
'  7.2 If You institute patent litigation against any entity (including a       ',&
'      cross-claim or counterclaim in a lawsuit) alleging that the Covered      ',&
'      Source or a Product constitutes direct or contributory patent            ',&
'      infringement, or You seek any declaration that a patent licensed to You  ',&
'      under this Licence is invalid or unenforceable then any rights granted   ',&
'      to You under this Licence shall terminate as of the date such process is ',&
'      initiated.                                                               ',&
'                                                                               ',&
'                                                                               ',&
'8 General                                                                      ',&
'                                                                               ',&
'  8.1 If any provisions of this Licence are or subsequently become invalid or  ',&
'      unenforceable for any reason, the remaining provisions shall remain      ',&
'      effective.                                                               ',&
'                                                                               ',&
'  8.2 You shall not use any of the name (including acronyms and                ',&
'      abbreviations), image, or logo by which the Licensor or CERN is known,   ',&
'      except where needed to comply with section 3, or where the use is        ',&
'      otherwise allowed by law. Any such permitted use shall be factual and    ',&
'      shall not be made so as to suggest any kind of endorsement or            ',&
'      implication of involvement by the Licensor or its personnel.             ',&
'                                                                               ',&
'  8.3 CERN may publish updated versions and variants of this Licence which it  ',&
'      considers to be in the spirit of this version, but may differ in detail  ',&
'      to address new problems or concerns. New versions will be published with ',&
'      a unique version number and a variant identifier specifying the variant. ',&
'      If the Licensor has specified that a given variant applies to the        ',&
'      Covered Source without specifying a version, You may treat that Covered  ',&
'      Source as being released under any version of the CERN-OHL with that     ',&
'      variant. If no variant is specified, the Covered Source shall be treated ',&
'      as being released under CERN-OHL-S. The Licensor may also specify that   ',&
'      the Covered Source is subject to a specific version of the CERN-OHL or   ',&
'      any later version in which case You may apply this or any later version  ',&
'      of CERN-OHL with the same variant identifier published by CERN.          ',&
'                                                                               ',&
'  8.4 This Licence shall terminate with immediate effect if You fail to comply ',&
'      with any of its terms and conditions.                                    ',&
'                                                                               ',&
'  8.5 However, if You cease all breaches of this Licence, then Your Licence    ',&
'      from any Licensor is reinstated unless such Licensor has terminated this ',&
'      Licence by giving You, while You remain in breach, a notice specifying   ',&
'      the breach and requiring You to cure it within 30 days, and You have     ',&
'      failed to come into compliance in all material respects by the end of    ',&
'      the 30 day period. Should You repeat the breach after receipt of a cure  ',&
'      notice and subsequent reinstatement, this Licence will terminate         ',&
'      immediately and permanently. Section 6 shall continue to apply after any ',&
'      termination.                                                             ',&
'                                                                               ',&
'  8.6 This Licence shall not be enforceable except by a Licensor acting as     ',&
'      such, and third party beneficiary rights are specifically excluded.      ',&
'                                                                               ',&
'']
if(present(topic_only))then
   if(size(textblock)>0)then
      if(topic_only) textblock=[character(len=len(textblock(1))) :: textblock(1)]
   else
      textblock=['']
   endif
endif
case('19','cern-ohl-w-2.0')
textblock=[ CHARACTER(LEN=128) :: &
'cern-ohl-w-2.0',&
'              ',&
'CERN Open Hardware Licence Version 2 - Weakly Reciprocal',&
'                                                        ',&
'                                                        ',&
'Preamble                                                ',&
'                                                        ',&
'CERN has developed this licence to promote collaboration among hardware',&
'designers and to provide a legal tool which supports the freedom to use,',&
'study, modify, share and distribute hardware designs and products based on',&
'those designs. Version 2 of the CERN Open Hardware Licence comes in three ',&
'variants: CERN-OHL-P (permissive); and two reciprocal licences: this licence,',&
'CERN-OHL-W (weakly reciprocal) and CERN-OHL-S (strongly reciprocal).         ',&
'                                                                             ',&
'The CERN-OHL-W is copyright CERN 2020. Anyone is welcome to use it, in       ',&
'unmodified form only.                                                        ',&
'                                                                             ',&
'Use of this Licence does not imply any endorsement by CERN of any Licensor or',&
'their designs nor does it imply any involvement by CERN in their development.',&
'                                                                             ',&
'                                                                             ',&
'1 Definitions                                                                ',&
'                                                                             ',&
'  1.1 ''Licence'' means this CERN-OHL-W.                                     ',&
'                                                                             ',&
'  1.2 ''Compatible Licence'' means                                           ',&
'                                                                             ',&
'       a) any earlier version of the CERN Open Hardware licence, or          ',&
'                                                                             ',&
'       b) any version of the CERN-OHL-S or the CERN-OHL-W, or                ',&
'                                                                             ',&
'       c) any licence which permits You to treat the Source to which it      ',&
'          applies as licensed under CERN-OHL-S or CERN-OHL-W provided that on',&
'          Conveyance of any such Source, or any associated Product You treat ',&
'          the Source in question as being licensed under CERN-OHL-S or       ',&
'          CERN-OHL-W as appropriate.                                         ',&
'                                                                             ',&
'  1.3 ''Source'' means information such as design materials or digital code  ',&
'      which can be applied to Make or test a Product or to prepare a Product ',&
'      for use, Conveyance or sale, regardless of its medium or how it is     ',&
'      expressed. It may include Notices.                                     ',&
'                                                                             ',&
'  1.4 ''Covered Source'' means Source that is explicitly made available under',&
'      this Licence.                                                          ',&
'                                                                             ',&
'  1.5 ''Product'' means any device, component, work or physical object, whether',&
'      in finished or intermediate form, arising from the use, application or   ',&
'      processing of Covered Source.                                            ',&
'                                                                               ',&
'  1.6 ''Make'' means to create or configure something, whether by manufacture, ',&
'      assembly, compiling, loading or applying Covered Source or another       ',&
'      Product or otherwise.                                                    ',&
'                                                                               ',&
'  1.7 ''Available Component'' means any part, sub-assembly, library or code    ',&
'      which:                                                                   ',&
'                                                                               ',&
'       a) is licensed to You as Complete Source under a Compatible Licence; or ',&
'                                                                               ',&
'       b) is available, at the time a Product or the Source containing it is   ',&
'          first Conveyed, to You and any other prospective licensees           ',&
'                                                                               ',&
'           i) with sufficient rights and information (including any            ',&
'              configuration and programming files and information about its    ',&
'              characteristics and interfaces) to enable it either to be Made   ',&
'              itself, or to be sourced and used to Make the Product; or        ',&
'          ii) as part of the normal distribution of a tool used to design or   ',&
'              Make the Product.                                                ',&
'                                                                               ',&
'  1.8 ''External Material'' means anything (including Source) which:           ',&
'                                                                               ',&
'       a) is only combined with Covered Source in such a way that it           ',&
'          interfaces with the Covered Source using a documented interface      ',&
'          which is described in the Covered Source; and                        ',&
'                                                                               ',&
'       b) is not a derivative of or contains Covered Source, or, if it is, it  ',&
'          is solely to the extent necessary to facilitate such interfacing.    ',&
'                                                                               ',&
'  1.9 ''Complete Source'' means the set of all Source necessary to Make a      ',&
'      Product, in the preferred form for making modifications, including       ',&
'      necessary installation and interfacing information both for the Product, ',&
'      and for any included Available Components.  If the format is             ',&
'      proprietary, it must also be made available in a format (if the          ',&
'      proprietary tool can create it) which is viewable with a tool available  ',&
'      to potential licensees and licensed under a licence approved by the Free ',&
'      Software Foundation or the Open Source Initiative. Complete Source need  ',&
'      not include the Source of any Available Component, provided that You     ',&
'      include in the Complete Source sufficient information to enable a        ',&
'      recipient to Make or source and use the Available Component to Make the  ',&
'      Product.                                                                 ',&
'                                                                               ',&
' 1.10 ''Source Location'' means a location where a Licensor has placed Covered ',&
'      Source, and which that Licensor reasonably believes will remain easily   ',&
'      accessible for at least three years for anyone to obtain a digital copy. ',&
'                                                                               ',&
' 1.11 ''Notice'' means copyright, acknowledgement and trademark notices, Source',&
'      Location references, modification notices (subsection 3.3(b)) and all    ',&
'      notices that refer to this Licence and to the disclaimer of warranties   ',&
'      that are included in the Covered Source.                                 ',&
'                                                                               ',&
' 1.12 ''Licensee'' or ''You'' means any person exercising rights under this    ',&
'      Licence.                                                                 ',&
'                                                                               ',&
' 1.13 ''Licensor'' means a natural or legal person who creates or modifies     ',&
'      Covered Source. A person may be a Licensee and a Licensor at the same    ',&
'      time.                                                                    ',&
'                                                                               ',&
' 1.14 ''Convey'' means to communicate to the public or distribute.             ',&
'                                                                               ',&
'                                                                               ',&
'2 Applicability                                                                ',&
'                                                                               ',&
'  2.1 This Licence governs the use, copying, modification, Conveying of        ',&
'      Covered Source and Products, and the Making of Products. By exercising   ',&
'      any right granted under this Licence, You irrevocably accept these terms ',&
'      and conditions.                                                          ',&
'                                                                               ',&
'  2.2 This Licence is granted by the Licensor directly to You, and shall apply ',&
'      worldwide and without limitation in time.                                ',&
'                                                                               ',&
'  2.3 You shall not attempt to restrict by contract or otherwise the rights    ',&
'      granted under this Licence to other Licensees.                           ',&
'                                                                               ',&
'  2.4 This Licence is not intended to restrict fair use, fair dealing, or any  ',&
'      other similar right.                                                     ',&
'                                                                               ',&
'                                                                               ',&
'3 Copying, Modifying and Conveying Covered Source                              ',&
'                                                                               ',&
'  3.1 You may copy and Convey verbatim copies of Covered Source, in any        ',&
'      medium, provided You retain all Notices.                                 ',&
'                                                                               ',&
'  3.2 You may modify Covered Source, other than Notices, provided that You     ',&
'      irrevocably undertake to make that modified Covered Source available     ',&
'      from a Source Location should You Convey a Product in circumstances      ',&
'      where the recipient does not otherwise receive a copy of the modified    ',&
'      Covered Source. In each case subsection 3.3 shall apply.                 ',&
'                                                                               ',&
'      You may only delete Notices if they are no longer applicable to the      ',&
'      corresponding Covered Source as modified by You and You may add          ',&
'      additional Notices applicable to Your modifications.                     ',&
'                                                                               ',&
'  3.3 You may Convey modified Covered Source (with the effect that You shall   ',&
'      also become a Licensor) provided that You:                               ',&
'                                                                               ',&
'       a) retain Notices as required in subsection 3.2;                        ',&
'                                                                               ',&
'       b) add a Notice to the modified Covered Source stating that You have    ',&
'          modified it, with the date and brief description of how You have     ',&
'          modified it;                                                         ',&
'                                                                               ',&
'       c) add a Source Location Notice for the modified Covered Source if You  ',&
'          Convey in circumstances where the recipient does not otherwise       ',&
'          receive a copy of the modified Covered Source; and                   ',&
'                                                                               ',&
'       d) license the modified Covered Source under the terms and conditions   ',&
'          of this Licence (or, as set out in subsection 8.3, a later version,  ',&
'          if permitted by the licence of the original Covered Source). Such    ',&
'          modified Covered Source must be licensed as a whole, but excluding   ',&
'          Available Components contained in it or External Material to which   ',&
'          it is interfaced, which remain licensed under their own applicable   ',&
'          licences.                                                            ',&
'                                                                               ',&
'                                                                               ',&
'4 Making and Conveying Products                                                ',&
'                                                                               ',&
'  4.1 You may Make Products, and/or Convey them, provided that You either      ',&
'      provide each recipient with a copy of the Complete Source or ensure that ',&
'      each recipient is notified of the Source Location of the Complete        ',&
'      Source. That Complete Source includes Covered Source and You must        ',&
'      accordingly satisfy Your obligations set out in subsection 3.3. If       ',&
'      specified in a Notice, the Product must visibly and securely display the ',&
'      Source Location on it or its packaging or documentation in the manner    ',&
'      specified in that Notice.                                                ',&
'                                                                               ',&
'  4.2 Where You Convey a Product which incorporates External Material, the     ',&
'      Complete Source for that Product which You are required to provide under ',&
'      subsection 4.1 need not include any Source for the External Material.    ',&
'                                                                               ',&
'  4.3 You may license Products under terms of Your choice, provided that such  ',&
'      terms do not restrict or attempt to restrict any recipients'' rights     ',&
'      under this Licence to the Covered Source.                                ',&
'                                                                               ',&
'                                                                               ',&
'5 Research and Development                                                     ',&
'                                                                               ',&
'You may Convey Covered Source, modified Covered Source or Products to a legal  ',&
'entity carrying out development, testing or quality assurance work on Your     ',&
'behalf provided that the work is performed on terms which prevent the entity   ',&
'from both using the Source or Products for its own internal purposes and       ',&
'Conveying the Source or Products or any modifications to them to any person    ',&
'other than You. Any modifications made by the entity shall be deemed to be     ',&
'made by You pursuant to subsection 3.2.                                        ',&
'                                                                               ',&
'                                                                               ',&
'6 DISCLAIMER AND LIABILITY                                                     ',&
'                                                                               ',&
'  6.1 DISCLAIMER OF WARRANTY -- The Covered Source and any Products are        ',&
'      provided ''as is'' and any express or implied warranties, including, but ',&
'      not limited to, implied warranties of merchantability, of satisfactory   ',&
'      quality, non-infringement of third party rights, and fitness for a       ',&
'      particular purpose or use are disclaimed in respect of any Source or     ',&
'      Product to the maximum extent permitted by law. The Licensor makes no    ',&
'      representation that any Source or Product does not or will not infringe  ',&
'      any patent, copyright, trade secret or other proprietary right. The      ',&
'      entire risk as to the use, quality, and performance of any Source or     ',&
'      Product shall be with You and not the Licensor. This disclaimer of       ',&
'      warranty is an essential part of this Licence and a condition for the    ',&
'      grant of any rights granted under this Licence.                          ',&
'                                                                               ',&
'  6.2 EXCLUSION AND LIMITATION OF LIABILITY -- The Licensor shall, to the      ',&
'      maximum extent permitted by law, have no liability for direct, indirect, ',&
'      special, incidental, consequential, exemplary, punitive or other damages ',&
'      of any character including, without limitation, procurement of           ',&
'      substitute goods or services, loss of use, data or profits, or business  ',&
'      interruption, however caused and on any theory of contract, warranty,    ',&
'      tort (including negligence), product liability or otherwise, arising in  ',&
'      any way in relation to the Covered Source, modified Covered Source       ',&
'      and/or the Making or Conveyance of a Product, even if advised of the     ',&
'      possibility of such damages, and You shall hold the Licensor(s) free and ',&
'      harmless from any liability, costs, damages, fees and expenses,          ',&
'      including claims by third parties, in relation to such use.              ',&
'                                                                               ',&
'                                                                               ',&
'7 Patents                                                                      ',&
'                                                                               ',&
'  7.1 Subject to the terms and conditions of this Licence, each Licensor       ',&
'      hereby grants to You a perpetual, worldwide, non-exclusive, no-charge,   ',&
'      royalty-free, irrevocable (except as stated in subsections 7.2 and 8.4)  ',&
'      patent licence to Make, have Made, use, offer to sell, sell, import, and ',&
'      otherwise transfer the Covered Source and Products, where such licence   ',&
'      applies only to those patent claims licensable by such Licensor that are ',&
'      necessarily infringed by exercising rights under the Covered Source as   ',&
'      Conveyed by that Licensor.                                               ',&
'                                                                               ',&
'  7.2 If You institute patent litigation against any entity (including a       ',&
'      cross-claim or counterclaim in a lawsuit) alleging that the Covered      ',&
'      Source or a Product constitutes direct or contributory patent            ',&
'      infringement, or You seek any declaration that a patent licensed to You  ',&
'      under this Licence is invalid or unenforceable then any rights granted   ',&
'      to You under this Licence shall terminate as of the date such process is ',&
'      initiated.                                                               ',&
'                                                                               ',&
'                                                                               ',&
'8 General                                                                      ',&
'                                                                               ',&
'  8.1 If any provisions of this Licence are or subsequently become invalid or  ',&
'      unenforceable for any reason, the remaining provisions shall remain      ',&
'      effective.                                                               ',&
'                                                                               ',&
'  8.2 You shall not use any of the name (including acronyms and                ',&
'      abbreviations), image, or logo by which the Licensor or CERN is known,   ',&
'      except where needed to comply with section 3, or where the use is        ',&
'      otherwise allowed by law. Any such permitted use shall be factual and    ',&
'      shall not be made so as to suggest any kind of endorsement or            ',&
'      implication of involvement by the Licensor or its personnel.             ',&
'                                                                               ',&
'  8.3 CERN may publish updated versions and variants of this Licence which it  ',&
'      considers to be in the spirit of this version, but may differ in detail  ',&
'      to address new problems or concerns. New versions will be published with ',&
'      a unique version number and a variant identifier specifying the variant. ',&
'      If the Licensor has specified that a given variant applies to the        ',&
'      Covered Source without specifying a version, You may treat that Covered  ',&
'      Source as being released under any version of the CERN-OHL with that     ',&
'      variant. If no variant is specified, the Covered Source shall be treated ',&
'      as being released under CERN-OHL-S. The Licensor may also specify that   ',&
'      the Covered Source is subject to a specific version of the CERN-OHL or   ',&
'      any later version in which case You may apply this or any later version  ',&
'      of CERN-OHL with the same variant identifier published by CERN.          ',&
'                                                                               ',&
'      You may treat Covered Source licensed under CERN-OHL-W as licensed under ',&
'      CERN-OHL-S if and only if all Available Components referenced in the     ',&
'      Covered Source comply with the corresponding definition of Available     ',&
'      Component for CERN-OHL-S.                                                ',&
'                                                                               ',&
'  8.4 This Licence shall terminate with immediate effect if You fail to comply ',&
'      with any of its terms and conditions.                                    ',&
'                                                                               ',&
'  8.5 However, if You cease all breaches of this Licence, then Your Licence    ',&
'      from any Licensor is reinstated unless such Licensor has terminated this ',&
'      Licence by giving You, while You remain in breach, a notice specifying   ',&
'      the breach and requiring You to cure it within 30 days, and You have     ',&
'      failed to come into compliance in all material respects by the end of    ',&
'      the 30 day period. Should You repeat the breach after receipt of a cure  ',&
'      notice and subsequent reinstatement, this Licence will terminate         ',&
'      immediately and permanently. Section 6 shall continue to apply after any ',&
'      termination.                                                             ',&
'                                                                               ',&
'  8.6 This Licence shall not be enforceable except by a Licensor acting as     ',&
'       such, and third party beneficiary rights are specifically excluded.     ',&
'                                                                               ',&
'']
if(present(topic_only))then
   if(size(textblock)>0)then
      if(topic_only) textblock=[character(len=len(textblock(1))) :: textblock(1)]
   else
      textblock=['']
   endif
endif
case('20','ecl-2.0')
textblock=[ CHARACTER(LEN=128) :: &
'ecl-2.0',&
'       ',&
'Educational Community License',&
'                             ',&
'Version 2.0, April 2007      ',&
'                             ',&
'http://opensource.org/licenses/ECL-2.0',&
'                                      ',&
'The Educational Community License version 2.0 ("ECL") consists of the Apache',&
'2.0 license, modified to change the scope of the patent grant in section 3 to',&
'be specific to the needs of the education communities using this license. The',&
'original Apache 2.0 license can be found at:                                 ',&
'http://www.apache.org/licenses/LICENSE-2.0                                   ',&
'                                                                             ',&
'TERMS AND CONDITIONS FOR USE, REPRODUCTION, AND DISTRIBUTION                 ',&
'                                                                             ',&
'1. Definitions.                                                              ',&
'                                                                             ',&
'"License" shall mean the terms and conditions for use, reproduction, and     ',&
'distribution as defined by Sections 1 through 9 of this document.            ',&
'                                                                             ',&
'"Licensor" shall mean the copyright owner or entity authorized by the        ',&
'copyright owner that is granting the License.                                ',&
'                                                                             ',&
'"Legal Entity" shall mean the union of the acting entity and all other       ',&
'entities that control, are controlled by, or are under common control with   ',&
'that entity. For the purposes of this definition, "control" means (i) the    ',&
'power, direct or indirect, to cause the direction or management of such      ',&
'entity, whether by contract or otherwise, or (ii) ownership of fifty percent ',&
'(50%) or more of the outstanding shares, or (iii) beneficial ownership of such',&
'entity.                                                                       ',&
'                                                                              ',&
'"You" (or "Your") shall mean an individual or Legal Entity exercising         ',&
'permissions granted by this License.                                          ',&
'                                                                              ',&
'"Source" form shall mean the preferred form for making modifications,         ',&
'including but not limited to software source code, documentation source, and  ',&
'configuration files.                                                          ',&
'                                                                              ',&
'"Object" form shall mean any form resulting from mechanical transformation or ',&
'translation of a Source form, including but not limited to compiled object    ',&
'code, generated documentation, and conversions to other media types.          ',&
'                                                                              ',&
'"Work" shall mean the work of authorship, whether in Source or Object form,   ',&
'made available under the License, as indicated by a copyright notice that is  ',&
'included in or attached to the work (an example is provided in the Appendix   ',&
'below).                                                                       ',&
'                                                                              ',&
'"Derivative Works" shall mean any work, whether in Source or Object form, that',&
'is based on (or derived from) the Work and for which the editorial revisions, ',&
'annotations, elaborations, or other modifications represent, as a whole, an   ',&
'original work of authorship. For the purposes of this License, Derivative     ',&
'Works shall not include works that remain separable from, or merely link (or  ',&
'bind by name) to the interfaces of, the Work and Derivative Works thereof.    ',&
'                                                                              ',&
'"Contribution" shall mean any work of authorship, including the original      ',&
'version of the Work and any modifications or additions to that Work or        ',&
'Derivative Works thereof, that is intentionally submitted to Licensor for     ',&
'inclusion in the Work by the copyright owner or by an individual or Legal     ',&
'Entity authorized to submit on behalf of the copyright owner. For the purposes',&
'of this definition, "submitted" means any form of electronic, verbal, or      ',&
'written communication sent to the Licensor or its representatives, including  ',&
'but not limited to communication on electronic mailing lists, source code     ',&
'control systems, and issue tracking systems that are managed by, or on behalf ',&
'of, the Licensor for the purpose of discussing and improving the Work, but    ',&
'excluding communication that is conspicuously marked or otherwise designated  ',&
'in writing by the copyright owner as "Not a Contribution."                    ',&
'                                                                              ',&
'"Contributor" shall mean Licensor and any individual or Legal Entity on behalf',&
'of whom a Contribution has been received by Licensor and subsequently         ',&
'incorporated within the Work.                                                 ',&
'                                                                              ',&
'2. Grant of Copyright License.                                                ',&
'                                                                              ',&
'Subject to the terms and conditions of this License, each Contributor hereby  ',&
'grants to You a perpetual, worldwide, non-exclusive, no-charge, royalty-free, ',&
'irrevocable copyright license to reproduce, prepare Derivative Works of,      ',&
'publicly display, publicly perform, sublicense, and distribute the Work and   ',&
'such Derivative Works in Source or Object form.                               ',&
'                                                                              ',&
'3. Grant of Patent License.                                                   ',&
'                                                                              ',&
'Subject to the terms and conditions of this License, each Contributor hereby  ',&
'grants to You a perpetual, worldwide, non-exclusive, no-charge, royalty-free, ',&
'irrevocable (except as stated in this section) patent license to make, have   ',&
'made, use, offer to sell, sell, import, and otherwise transfer the Work, where',&
'such license applies only to those patent claims licensable by such           ',&
'Contributor that are necessarily infringed by their Contribution(s) alone or  ',&
'by combination of their Contribution(s) with the Work to which such           ',&
'Contribution(s) was submitted. If You institute patent litigation against any ',&
'entity (including a cross-claim or counterclaim in a lawsuit) alleging that   ',&
'the Work or a Contribution incorporated within the Work constitutes direct or ',&
'contributory patent infringement, then any patent licenses granted to You     ',&
'under this License for that Work shall terminate as of the date such          ',&
'litigation is filed. Any patent license granted hereby with respect to        ',&
'contributions by an individual employed by an institution or organization is  ',&
'limited to patent claims where the individual that is the author of the Work  ',&
'is also the inventor of the patent claims licensed, and where the organization',&
'or institution has the right to grant such license under applicable grant and ',&
'research funding agreements. No other express or implied licenses are granted.',&
'                                                                              ',&
'4. Redistribution.                                                            ',&
'                                                                              ',&
'You may reproduce and distribute copies of the Work or Derivative Works       ',&
'thereof in any medium, with or without modifications, and in Source or Object ',&
'form, provided that You meet the following conditions:                        ',&
'                                                                              ',&
'You must give any other recipients of the Work or Derivative Works a copy of  ',&
'this License; and You must cause any modified files to carry prominent notices',&
'stating that You changed the files; and You must retain, in the Source form of',&
'any Derivative Works that You distribute, all copyright, patent, trademark,   ',&
'and attribution notices from the Source form of the Work, excluding those     ',&
'notices that do not pertain to any part of the Derivative Works; and If the   ',&
'Work includes a "NOTICE" text file as part of its distribution, then any      ',&
'Derivative Works that You distribute must include a readable copy of the      ',&
'attribution notices contained within such NOTICE file, excluding those notices',&
'that do not pertain to any part of the Derivative Works, in at least one of   ',&
'the following places: within a NOTICE text file distributed as part of the    ',&
'Derivative Works; within the Source form or documentation, if provided along  ',&
'with the Derivative Works; or, within a display generated by the Derivative   ',&
'Works, if and wherever such third-party notices normally appear. The contents ',&
'of the NOTICE file are for informational purposes only and do not modify the  ',&
'License. You may add Your own attribution notices within Derivative Works that',&
'You distribute, alongside or as an addendum to the NOTICE text from the Work, ',&
'provided that such additional attribution notices cannot be construed as      ',&
'modifying the License. You may add Your own copyright statement to Your       ',&
'modifications and may provide additional or different license terms and       ',&
'conditions for use, reproduction, or distribution of Your modifications, or   ',&
'for any such Derivative Works as a whole, provided Your use, reproduction, and',&
'distribution of the Work otherwise complies with the conditions stated in this',&
'License.                                                                      ',&
'                                                                              ',&
'5. Submission of Contributions.                                               ',&
'                                                                              ',&
'Unless You explicitly state otherwise, any Contribution intentionally         ',&
'submitted for inclusion in the Work by You to the Licensor shall be under the ',&
'terms and conditions of this License, without any additional terms or         ',&
'conditions. Notwithstanding the above, nothing herein shall supersede or      ',&
'modify the terms of any separate license agreement you may have executed with ',&
'Licensor regarding such Contributions.                                        ',&
'                                                                              ',&
'6. Trademarks.                                                                ',&
'                                                                              ',&
'This License does not grant permission to use the trade names, trademarks,    ',&
'service marks, or product names of the Licensor, except as required for       ',&
'reasonable and customary use in describing the origin of the Work and         ',&
'reproducing the content of the NOTICE file.                                   ',&
'                                                                              ',&
'7. Disclaimer of Warranty.                                                    ',&
'                                                                              ',&
'Unless required by applicable law or agreed to in writing, Licensor provides  ',&
'the Work (and each Contributor provides its Contributions) on an "AS IS"      ',&
'BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or        ',&
'implied, including, without limitation, any warranties or conditions of TITLE,',&
'NON-INFRINGEMENT, MERCHANTABILITY, or FITNESS FOR A PARTICULAR PURPOSE. You   ',&
'are solely responsible for determining the appropriateness of using or        ',&
'redistributing the Work and assume any risks associated with Your exercise of ',&
'permissions under this License.                                               ',&
'                                                                              ',&
'8. Limitation of Liability.                                                   ',&
'                                                                              ',&
'In no event and under no legal theory, whether in tort (including negligence),',&
'contract, or otherwise, unless required by applicable law (such as deliberate ',&
'and grossly negligent acts) or agreed to in writing, shall any Contributor be ',&
'liable to You for damages, including any direct, indirect, special,           ',&
'incidental, or consequential damages of any character arising as a result of  ',&
'this License or out of the use or inability to use the Work (including but not',&
'limited to damages for loss of goodwill, work stoppage, computer failure or   ',&
'malfunction, or any and all other commercial damages or losses), even if such ',&
'Contributor has been advised of the possibility of such damages.              ',&
'                                                                              ',&
'9. Accepting Warranty or Additional Liability.                                ',&
'                                                                              ',&
'While redistributing the Work or Derivative Works thereof, You may choose to  ',&
'offer, and charge a fee for, acceptance of support, warranty, indemnity, or   ',&
'other liability obligations and/or rights consistent with this License.       ',&
'However, in accepting such obligations, You may act only on Your own behalf   ',&
'and on Your sole responsibility, not on behalf of any other Contributor, and  ',&
'only if You agree to indemnify, defend, and hold each Contributor harmless for',&
'any liability incurred by, or claims asserted against, such Contributor by    ',&
'reason of your accepting any such warranty or additional liability.           ',&
'                                                                              ',&
'END OF TERMS AND CONDITIONS                                                   ',&
'                                                                              ',&
'APPENDIX: How to apply the Educational Community License to your work         ',&
'                                                                              ',&
'To apply the Educational Community License to your work, attach the following ',&
'boilerplate notice, with the fields enclosed by brackets "[]" replaced with   ',&
'your own identifying information. (Don''t include the brackets!) The text     ',&
'should be enclosed in the appropriate comment syntax for the file format. We  ',&
'also recommend that a file or class name and description of purpose be        ',&
'included on the same "printed page" as the copyright notice for easier        ',&
'identification within third-party archives.                                   ',&
'                                                                              ',&
'Copyright @YEAR@ [name of copyright owner] Licensed under the Educational     ',&
'Community License, Version 2.0 (the "License"); you may not use this file     ',&
'except in compliance with the License. You may obtain a copy of the License at',&
'                                                                              ',&
'http://opensource.org/licenses/ECL-2.0                                        ',&
'                                                                              ',&
' Unless required by applicable law or agreed to in writing, software          ',&
'distributed under the License is distributed on an "AS IS" BASIS, WITHOUT     ',&
'WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the      ',&
'License for the specific language governing permissions and limitations under ',&
'the License.                                                                  ',&
'                                                                              ',&
'']
if(present(topic_only))then
   if(size(textblock)>0)then
      if(topic_only) textblock=[character(len=len(textblock(1))) :: textblock(1)]
   else
      textblock=['']
   endif
endif
case('21','epl-1.0')
textblock=[ CHARACTER(LEN=128) :: &
'epl-1.0',&
'       ',&
'Eclipse Public License - v 1.0',&
'                              ',&
'THE ACCOMPANYING PROGRAM IS PROVIDED UNDER THE TERMS OF THIS ECLIPSE PUBLIC',&
'LICENSE ("AGREEMENT"). ANY USE, REPRODUCTION OR DISTRIBUTION OF THE PROGRAM',&
'CONSTITUTES RECIPIENT''S ACCEPTANCE OF THIS AGREEMENT.                     ',&
'                                                                           ',&
'1. DEFINITIONS                                                             ',&
'                                                                           ',&
'"Contribution" means:                                                      ',&
'     a) in the case of the initial Contributor, the initial code and       ',&
'     documentation distributed under this Agreement, and                   ',&
'     b) in the case of each subsequent Contributor:                        ',&
'          i) changes to the Program, and                                   ',&
'          ii) additions to the Program;                                    ',&
'                                                                           ',&
'where such changes and/or additions to the Program originate from and are  ',&
'distributed by that particular Contributor. A Contribution ''originates'' from a',&
'Contributor if it was added to the Program by such Contributor itself or        ',&
'anyone acting on such Contributor''s behalf. Contributions do not include       ',&
'additions to the Program which: (i) are separate modules of software            ',&
'distributed in conjunction with the Program under their own license agreement,  ',&
'and (ii) are not derivative works of the Program.                               ',&
'"Contributor" means any person or entity that distributes the Program.          ',&
'                                                                                ',&
'"Licensed Patents" mean patent claims licensable by a Contributor which are     ',&
'necessarily infringed by the use or sale of its Contribution alone or when      ',&
'combined with the Program.                                                      ',&
'                                                                                ',&
'"Program" means the Contributions distributed in accordance with this           ',&
'Agreement.                                                                      ',&
'                                                                                ',&
'"Recipient" means anyone who receives the Program under this Agreement,         ',&
'including all Contributors.                                                     ',&
'                                                                                ',&
'2. GRANT OF RIGHTS                                                              ',&
'                                                                                ',&
'     a) Subject to the terms of this Agreement, each Contributor hereby grants  ',&
'     Recipient a non-exclusive, worldwide, royalty-free copyright license to    ',&
'     reproduce, prepare derivative works of, publicly display, publicly         ',&
'     perform, distribute and sublicense the Contribution of such Contributor,   ',&
'     if any, and such derivative works, in source code and object code form.    ',&
'                                                                                ',&
'     b) Subject to the terms of this Agreement, each Contributor hereby grants  ',&
'     Recipient a non-exclusive, worldwide, royalty-free patent license under    ',&
'     Licensed Patents to make, use, sell, offer to sell, import and otherwise   ',&
'     transfer the Contribution of such Contributor, if any, in source code and  ',&
'     object code form. This patent license shall apply to the combination of    ',&
'     the Contribution and the Program if, at the time the Contribution is       ',&
'     added by the Contributor, such addition of the Contribution causes such    ',&
'     combination to be covered by the Licensed Patents. The patent license      ',&
'     shall not apply to any other combinations which include the Contribution.  ',&
'     No hardware per se is licensed hereunder.                                  ',&
'                                                                                ',&
'     c) Recipient understands that although each Contributor grants the         ',&
'     licenses to its Contributions set forth herein, no assurances are          ',&
'     provided by any Contributor that the Program does not infringe the patent  ',&
'     or other intellectual property rights of any other entity. Each            ',&
'     Contributor disclaims any liability to Recipient for claims brought by     ',&
'     any other entity based on infringement of intellectual property rights or  ',&
'     otherwise. As a condition to exercising the rights and licenses granted    ',&
'     hereunder, each Recipient hereby assumes sole responsibility to secure     ',&
'     any other intellectual property rights needed, if any. For example, if a   ',&
'     third party patent license is required to allow Recipient to distribute    ',&
'     the Program, it is Recipient''s responsibility to acquire that license     ',&
'     before distributing the Program.                                           ',&
'                                                                                ',&
'     d) Each Contributor represents that to its knowledge it has sufficient     ',&
'     copyright rights in its Contribution, if any, to grant the copyright       ',&
'     license set forth in this Agreement.                                       ',&
'                                                                                ',&
'3. REQUIREMENTS                                                                 ',&
'A Contributor may choose to distribute the Program in object code form under    ',&
'its own license agreement, provided that:                                       ',&
'                                                                                ',&
'     a) it complies with the terms and conditions of this Agreement; and        ',&
'                                                                                ',&
'     b) its license agreement:                                                  ',&
'          i) effectively disclaims on behalf of all Contributors all            ',&
'          warranties and conditions, express and implied, including warranties  ',&
'          or conditions of title and non-infringement, and implied warranties   ',&
'          or conditions of merchantability and fitness for a particular         ',&
'          purpose;                                                              ',&
'          ii) effectively excludes on behalf of all Contributors all liability  ',&
'          for damages, including direct, indirect, special, incidental and      ',&
'          consequential damages, such as lost profits;                          ',&
'          iii) states that any provisions which differ from this Agreement are  ',&
'          offered by that Contributor alone and not by any other party; and     ',&
'          iv) states that source code for the Program is available from such    ',&
'          Contributor, and informs licensees how to obtain it in a reasonable   ',&
'          manner on or through a medium customarily used for software           ',&
'          exchange.                                                             ',&
'                                                                                ',&
'When the Program is made available in source code form:                         ',&
'                                                                                ',&
'     a) it must be made available under this Agreement; and                     ',&
'                                                                                ',&
'     b) a copy of this Agreement must be included with each copy of the         ',&
'     Program.                                                                   ',&
'Contributors may not remove or alter any copyright notices contained within     ',&
'the Program.                                                                    ',&
'                                                                                ',&
'Each Contributor must identify itself as the originator of its Contribution,    ',&
'if any, in a manner that reasonably allows subsequent Recipients to identify    ',&
'the originator of the Contribution.                                             ',&
'                                                                                ',&
'4. COMMERCIAL DISTRIBUTION                                                      ',&
'Commercial distributors of software may accept certain responsibilities with    ',&
'respect to end users, business partners and the like. While this license is     ',&
'intended to facilitate the commercial use of the Program, the Contributor who   ',&
'includes the Program in a commercial product offering should do so in a manner  ',&
'which does not create potential liability for other Contributors. Therefore,    ',&
'if a Contributor includes the Program in a commercial product offering, such    ',&
'Contributor ("Commercial Contributor") hereby agrees to defend and indemnify    ',&
'every other Contributor ("Indemnified Contributor") against any losses,         ',&
'damages and costs (collectively "Losses") arising from claims, lawsuits and     ',&
'other legal actions brought by a third party against the Indemnified            ',&
'Contributor to the extent caused by the acts or omissions of such Commercial    ',&
'Contributor in connection with its distribution of the Program in a commercial  ',&
'product offering. The obligations in this section do not apply to any claims    ',&
'or Losses relating to any actual or alleged intellectual property               ',&
'infringement. In order to qualify, an Indemnified Contributor must: a)          ',&
'promptly notify the Commercial Contributor in writing of such claim, and b)     ',&
'allow the Commercial Contributor to control, and cooperate with the Commercial  ',&
'Contributor in, the defense and any related settlement negotiations. The        ',&
'Indemnified Contributor may participate in any such claim at its own expense.   ',&
'                                                                                ',&
'For example, a Contributor might include the Program in a commercial product    ',&
'offering, Product X. That Contributor is then a Commercial Contributor. If      ',&
'that Commercial Contributor then makes performance claims, or offers            ',&
'warranties related to Product X, those performance claims and warranties are    ',&
'such Commercial Contributor''s responsibility alone. Under this section, the    ',&
'Commercial Contributor would have to defend claims against the other            ',&
'Contributors related to those performance claims and warranties, and if a       ',&
'court requires any other Contributor to pay any damages as a result, the        ',&
'Commercial Contributor must pay those damages.                                  ',&
'                                                                                ',&
'5. NO WARRANTY                                                                  ',&
'EXCEPT AS EXPRESSLY SET FORTH IN THIS AGREEMENT, THE PROGRAM IS PROVIDED ON AN  ',&
'"AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, EITHER EXPRESS OR  ',&
'IMPLIED INCLUDING, WITHOUT LIMITATION, ANY WARRANTIES OR CONDITIONS OF TITLE,   ',&
'NON-INFRINGEMENT, MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. Each     ',&
'Recipient is solely responsible for determining the appropriateness of using    ',&
'and distributing the Program and assumes all risks associated with its          ',&
'exercise of rights under this Agreement , including but not limited to the      ',&
'risks and costs of program errors, compliance with applicable laws, damage to   ',&
'or loss of data, programs or equipment, and unavailability or interruption of   ',&
'operations.                                                                     ',&
'                                                                                ',&
'6. DISCLAIMER OF LIABILITY                                                      ',&
'EXCEPT AS EXPRESSLY SET FORTH IN THIS AGREEMENT, NEITHER RECIPIENT NOR ANY      ',&
'CONTRIBUTORS SHALL HAVE ANY LIABILITY FOR ANY DIRECT, INDIRECT, INCIDENTAL,     ',&
'SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING WITHOUT LIMITATION      ',&
'LOST PROFITS), HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN        ',&
'CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)         ',&
'ARISING IN ANY WAY OUT OF THE USE OR DISTRIBUTION OF THE PROGRAM OR THE         ',&
'EXERCISE OF ANY RIGHTS GRANTED HEREUNDER, EVEN IF ADVISED OF THE POSSIBILITY    ',&
'OF SUCH DAMAGES.                                                                ',&
'                                                                                ',&
'7. GENERAL                                                                      ',&
'                                                                                ',&
'If any provision of this Agreement is invalid or unenforceable under            ',&
'applicable law, it shall not affect the validity or enforceability of the       ',&
'remainder of the terms of this Agreement, and without further action by the     ',&
'parties hereto, such provision shall be reformed to the minimum extent          ',&
'necessary to make such provision valid and enforceable.                         ',&
'                                                                                ',&
'If Recipient institutes patent litigation against any entity (including a       ',&
'cross-claim or counterclaim in a lawsuit) alleging that the Program itself      ',&
'(excluding combinations of the Program with other software or hardware)         ',&
'infringes such Recipient''s patent(s), then such Recipient''s rights granted    ',&
'under Section 2(b) shall terminate as of the date such litigation is filed.     ',&
'                                                                                ',&
'All Recipient''s rights under this Agreement shall terminate if it fails to     ',&
'comply with any of the material terms or conditions of this Agreement and does  ',&
'not cure such failure in a reasonable period of time after becoming aware of    ',&
'such noncompliance. If all Recipient''s rights under this Agreement terminate,  ',&
'Recipient agrees to cease use and distribution of the Program as soon as        ',&
'reasonably practicable. However, Recipient''s obligations under this Agreement  ',&
'and any licenses granted by Recipient relating to the Program shall continue    ',&
'and survive.                                                                    ',&
'                                                                                ',&
'Everyone is permitted to copy and distribute copies of this Agreement, but in   ',&
'order to avoid inconsistency the Agreement is copyrighted and may only be       ',&
'modified in the following manner. The Agreement Steward reserves the right to   ',&
'publish new versions (including revisions) of this Agreement from time to       ',&
'time. No one other than the Agreement Steward has the right to modify this      ',&
'Agreement. The Eclipse Foundation is the initial Agreement Steward. The         ',&
'Eclipse Foundation may assign the responsibility to serve as the Agreement      ',&
'Steward to a suitable separate entity. Each new version of the Agreement will   ',&
'be given a distinguishing version number. The Program (including                ',&
'Contributions) may always be distributed subject to the version of the          ',&
'Agreement under which it was received. In addition, after a new version of the  ',&
'Agreement is published, Contributor may elect to distribute the Program         ',&
'(including its Contributions) under the new version. Except as expressly        ',&
'stated in Sections 2(a) and 2(b) above, Recipient receives no rights or         ',&
'licenses to the intellectual property of any Contributor under this Agreement,  ',&
'whether expressly, by implication, estoppel or otherwise. All rights in the     ',&
'Program not expressly granted under this Agreement are reserved.                ',&
'                                                                                ',&
'This Agreement is governed by the laws of the State of New York and the         ',&
'intellectual property laws of the United States of America. No party to this    ',&
'Agreement will bring a legal action under this Agreement more than one year     ',&
'after the cause of action arose. Each party waives its rights to a jury trial   ',&
'in any resulting litigation.                                                    ',&
'                                                                                ',&
'']
if(present(topic_only))then
   if(size(textblock)>0)then
      if(topic_only) textblock=[character(len=len(textblock(1))) :: textblock(1)]
   else
      textblock=['']
   endif
endif
case('22','epl-2.0')
textblock=[ CHARACTER(LEN=128) :: &
'epl-2.0',&
'       ',&
'Eclipse Public License - v 2.0',&
'                              ',&
'    THE ACCOMPANYING PROGRAM IS PROVIDED UNDER THE TERMS OF THIS ECLIPSE',&
'    PUBLIC LICENSE ("AGREEMENT"). ANY USE, REPRODUCTION OR DISTRIBUTION ',&
'    OF THE PROGRAM CONSTITUTES RECIPIENT''S ACCEPTANCE OF THIS AGREEMENT.',&
'                                                                         ',&
'1. DEFINITIONS                                                           ',&
'                                                                         ',&
'"Contribution" means:                                                    ',&
'                                                                         ',&
'  a) in the case of the initial Contributor, the initial content         ',&
'     Distributed under this Agreement, and                               ',&
'                                                                         ',&
'  b) in the case of each subsequent Contributor:                         ',&
'     i) changes to the Program, and                                      ',&
'     ii) additions to the Program;                                       ',&
'  where such changes and/or additions to the Program originate from      ',&
'  and are Distributed by that particular Contributor. A Contribution     ',&
'  "originates" from a Contributor if it was added to the Program by      ',&
'  such Contributor itself or anyone acting on such Contributor''s behalf.',&
'  Contributions do not include changes or additions to the Program that  ',&
'  are not Modified Works.                                                ',&
'                                                                         ',&
'"Contributor" means any person or entity that Distributes the Program.   ',&
'                                                                         ',&
'"Licensed Patents" mean patent claims licensable by a Contributor which  ',&
'are necessarily infringed by the use or sale of its Contribution alone   ',&
'or when combined with the Program.                                       ',&
'                                                                         ',&
'"Program" means the Contributions Distributed in accordance with this    ',&
'Agreement.                                                               ',&
'                                                                         ',&
'"Recipient" means anyone who receives the Program under this Agreement   ',&
'or any Secondary License (as applicable), including Contributors.        ',&
'                                                                         ',&
'"Derivative Works" shall mean any work, whether in Source Code or other  ',&
'form, that is based on (or derived from) the Program and for which the   ',&
'editorial revisions, annotations, elaborations, or other modifications   ',&
'represent, as a whole, an original work of authorship.                   ',&
'                                                                         ',&
'"Modified Works" shall mean any work in Source Code or other form that   ',&
'results from an addition to, deletion from, or modification of the       ',&
'contents of the Program, including, for purposes of clarity any new file ',&
'in Source Code form that contains any contents of the Program. Modified  ',&
'Works shall not include works that contain only declarations,            ',&
'interfaces, types, classes, structures, or files of the Program solely   ',&
'in each case in order to link to, bind by name, or subclass the Program  ',&
'or Modified Works thereof.                                               ',&
'                                                                         ',&
'"Distribute" means the acts of a) distributing or b) making available    ',&
'in any manner that enables the transfer of a copy.                       ',&
'                                                                         ',&
'"Source Code" means the form of a Program preferred for making           ',&
'modifications, including but not limited to software source code,        ',&
'documentation source, and configuration files.                           ',&
'                                                                         ',&
'"Secondary License" means either the GNU General Public License,         ',&
'Version 2.0, or any later versions of that license, including any        ',&
'exceptions or additional permissions as identified by the initial        ',&
'Contributor.                                                             ',&
'                                                                         ',&
'2. GRANT OF RIGHTS                                                       ',&
'                                                                         ',&
'  a) Subject to the terms of this Agreement, each Contributor hereby     ',&
'  grants Recipient a non-exclusive, worldwide, royalty-free copyright    ',&
'  license to reproduce, prepare Derivative Works of, publicly display,   ',&
'  publicly perform, Distribute and sublicense the Contribution of such   ',&
'  Contributor, if any, and such Derivative Works.                        ',&
'                                                                         ',&
'  b) Subject to the terms of this Agreement, each Contributor hereby     ',&
'  grants Recipient a non-exclusive, worldwide, royalty-free patent       ',&
'  license under Licensed Patents to make, use, sell, offer to sell,      ',&
'  import and otherwise transfer the Contribution of such Contributor,    ',&
'  if any, in Source Code or other form. This patent license shall        ',&
'  apply to the combination of the Contribution and the Program if, at    ',&
'  the time the Contribution is added by the Contributor, such addition   ',&
'  of the Contribution causes such combination to be covered by the       ',&
'  Licensed Patents. The patent license shall not apply to any other      ',&
'  combinations which include the Contribution. No hardware per se is     ',&
'  licensed hereunder.                                                    ',&
'                                                                         ',&
'  c) Recipient understands that although each Contributor grants the     ',&
'  licenses to its Contributions set forth herein, no assurances are      ',&
'  provided by any Contributor that the Program does not infringe the     ',&
'  patent or other intellectual property rights of any other entity.      ',&
'  Each Contributor disclaims any liability to Recipient for claims       ',&
'  brought by any other entity based on infringement of intellectual      ',&
'  property rights or otherwise. As a condition to exercising the         ',&
'  rights and licenses granted hereunder, each Recipient hereby           ',&
'  assumes sole responsibility to secure any other intellectual           ',&
'  property rights needed, if any. For example, if a third party          ',&
'  patent license is required to allow Recipient to Distribute the        ',&
'  Program, it is Recipient''s responsibility to acquire that license     ',&
'  before distributing the Program.                                       ',&
'                                                                         ',&
'  d) Each Contributor represents that to its knowledge it has            ',&
'  sufficient copyright rights in its Contribution, if any, to grant      ',&
'  the copyright license set forth in this Agreement.                     ',&
'                                                                         ',&
'  e) Notwithstanding the terms of any Secondary License, no              ',&
'  Contributor makes additional grants to any Recipient (other than       ',&
'  those set forth in this Agreement) as a result of such Recipient''s    ',&
'  receipt of the Program under the terms of a Secondary License          ',&
'  (if permitted under the terms of Section 3).                           ',&
'                                                                         ',&
'3. REQUIREMENTS                                                          ',&
'                                                                         ',&
'3.1 If a Contributor Distributes the Program in any form, then:          ',&
'                                                                         ',&
'  a) the Program must also be made available as Source Code, in          ',&
'  accordance with section 3.2, and the Contributor must accompany        ',&
'  the Program with a statement that the Source Code for the Program      ',&
'  is available under this Agreement, and informs Recipients how to       ',&
'  obtain it in a reasonable manner on or through a medium customarily    ',&
'  used for software exchange; and                                        ',&
'                                                                         ',&
'  b) the Contributor may Distribute the Program under a license          ',&
'  different than this Agreement, provided that such license:             ',&
'     i) effectively disclaims on behalf of all other Contributors all    ',&
'     warranties and conditions, express and implied, including           ',&
'     warranties or conditions of title and non-infringement, and         ',&
'     implied warranties or conditions of merchantability and fitness     ',&
'     for a particular purpose;                                           ',&
'                                                                         ',&
'     ii) effectively excludes on behalf of all other Contributors all    ',&
'     liability for damages, including direct, indirect, special,         ',&
'     incidental and consequential damages, such as lost profits;         ',&
'                                                                         ',&
'     iii) does not attempt to limit or alter the recipients'' rights     ',&
'     in the Source Code under section 3.2; and                           ',&
'                                                                         ',&
'     iv) requires any subsequent distribution of the Program by any      ',&
'     party to be under a license that satisfies the requirements         ',&
'     of this section 3.                                                  ',&
'                                                                         ',&
'3.2 When the Program is Distributed as Source Code:                      ',&
'                                                                         ',&
'  a) it must be made available under this Agreement, or if the           ',&
'  Program (i) is combined with other material in a separate file or      ',&
'  files made available under a Secondary License, and (ii) the initial   ',&
'  Contributor attached to the Source Code the notice described in        ',&
'  Exhibit A of this Agreement, then the Program may be made available    ',&
'  under the terms of such Secondary Licenses, and                        ',&
'                                                                         ',&
'  b) a copy of this Agreement must be included with each copy of         ',&
'  the Program.                                                           ',&
'                                                                         ',&
'3.3 Contributors may not remove or alter any copyright, patent,          ',&
'trademark, attribution notices, disclaimers of warranty, or limitations  ',&
'of liability ("notices") contained within the Program from any copy of   ',&
'the Program which they Distribute, provided that Contributors may add    ',&
'their own appropriate notices.                                           ',&
'                                                                         ',&
'4. COMMERCIAL DISTRIBUTION                                               ',&
'                                                                         ',&
'Commercial distributors of software may accept certain responsibilities  ',&
'with respect to end users, business partners and the like. While this    ',&
'license is intended to facilitate the commercial use of the Program,     ',&
'the Contributor who includes the Program in a commercial product         ',&
'offering should do so in a manner which does not create potential        ',&
'liability for other Contributors. Therefore, if a Contributor includes   ',&
'the Program in a commercial product offering, such Contributor           ',&
'("Commercial Contributor") hereby agrees to defend and indemnify every   ',&
'other Contributor ("Indemnified Contributor") against any losses,        ',&
'damages and costs (collectively "Losses") arising from claims, lawsuits  ',&
'and other legal actions brought by a third party against the Indemnified ',&
'Contributor to the extent caused by the acts or omissions of such        ',&
'Commercial Contributor in connection with its distribution of the Program',&
'in a commercial product offering. The obligations in this section do not ',&
'apply to any claims or Losses relating to any actual or alleged          ',&
'intellectual property infringement. In order to qualify, an Indemnified  ',&
'Contributor must: a) promptly notify the Commercial Contributor in       ',&
'writing of such claim, and b) allow the Commercial Contributor to control,',&
'and cooperate with the Commercial Contributor in, the defense and any     ',&
'related settlement negotiations. The Indemnified Contributor may          ',&
'participate in any such claim at its own expense.                         ',&
'                                                                          ',&
'For example, a Contributor might include the Program in a commercial      ',&
'product offering, Product X. That Contributor is then a Commercial        ',&
'Contributor. If that Commercial Contributor then makes performance        ',&
'claims, or offers warranties related to Product X, those performance      ',&
'claims and warranties are such Commercial Contributor''s responsibility   ',&
'alone. Under this section, the Commercial Contributor would have to       ',&
'defend claims against the other Contributors related to those performance ',&
'claims and warranties, and if a court requires any other Contributor to   ',&
'pay any damages as a result, the Commercial Contributor must pay          ',&
'those damages.                                                            ',&
'                                                                          ',&
'5. NO WARRANTY                                                            ',&
'                                                                          ',&
'EXCEPT AS EXPRESSLY SET FORTH IN THIS AGREEMENT, AND TO THE EXTENT        ',&
'PERMITTED BY APPLICABLE LAW, THE PROGRAM IS PROVIDED ON AN "AS IS"        ',&
'BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, EITHER EXPRESS OR    ',&
'IMPLIED INCLUDING, WITHOUT LIMITATION, ANY WARRANTIES OR CONDITIONS OF    ',&
'TITLE, NON-INFRINGEMENT, MERCHANTABILITY OR FITNESS FOR A PARTICULAR      ',&
'PURPOSE. Each Recipient is solely responsible for determining the         ',&
'appropriateness of using and distributing the Program and assumes all     ',&
'risks associated with its exercise of rights under this Agreement,        ',&
'including but not limited to the risks and costs of program errors,       ',&
'compliance with applicable laws, damage to or loss of data, programs      ',&
'or equipment, and unavailability or interruption of operations.           ',&
'                                                                          ',&
'6. DISCLAIMER OF LIABILITY                                                ',&
'                                                                          ',&
'EXCEPT AS EXPRESSLY SET FORTH IN THIS AGREEMENT, AND TO THE EXTENT        ',&
'PERMITTED BY APPLICABLE LAW, NEITHER RECIPIENT NOR ANY CONTRIBUTORS       ',&
'SHALL HAVE ANY LIABILITY FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,   ',&
'EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING WITHOUT LIMITATION LOST    ',&
'PROFITS), HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN       ',&
'CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)   ',&
'ARISING IN ANY WAY OUT OF THE USE OR DISTRIBUTION OF THE PROGRAM OR THE   ',&
'EXERCISE OF ANY RIGHTS GRANTED HEREUNDER, EVEN IF ADVISED OF THE          ',&
'POSSIBILITY OF SUCH DAMAGES.                                              ',&
'                                                                          ',&
'7. GENERAL                                                                ',&
'                                                                          ',&
'If any provision of this Agreement is invalid or unenforceable under      ',&
'applicable law, it shall not affect the validity or enforceability of     ',&
'the remainder of the terms of this Agreement, and without further         ',&
'action by the parties hereto, such provision shall be reformed to the     ',&
'minimum extent necessary to make such provision valid and enforceable.    ',&
'                                                                          ',&
'If Recipient institutes patent litigation against any entity              ',&
'(including a cross-claim or counterclaim in a lawsuit) alleging that the  ',&
'Program itself (excluding combinations of the Program with other software ',&
'or hardware) infringes such Recipient''s patent(s), then such Recipient''s',&
'rights granted under Section 2(b) shall terminate as of the date such     ',&
'litigation is filed.                                                      ',&
'                                                                          ',&
'All Recipient''s rights under this Agreement shall terminate if it        ',&
'fails to comply with any of the material terms or conditions of this      ',&
'Agreement and does not cure such failure in a reasonable period of        ',&
'time after becoming aware of such noncompliance. If all Recipient''s      ',&
'rights under this Agreement terminate, Recipient agrees to cease use      ',&
'and distribution of the Program as soon as reasonably practicable.        ',&
'However, Recipient''s obligations under this Agreement and any licenses   ',&
'granted by Recipient relating to the Program shall continue and survive.  ',&
'                                                                          ',&
'Everyone is permitted to copy and distribute copies of this Agreement,    ',&
'but in order to avoid inconsistency the Agreement is copyrighted and      ',&
'may only be modified in the following manner. The Agreement Steward       ',&
'reserves the right to publish new versions (including revisions) of       ',&
'this Agreement from time to time. No one other than the Agreement         ',&
'Steward has the right to modify this Agreement. The Eclipse Foundation    ',&
'is the initial Agreement Steward. The Eclipse Foundation may assign the   ',&
'responsibility to serve as the Agreement Steward to a suitable separate   ',&
'entity. Each new version of the Agreement will be given a distinguishing  ',&
'version number. The Program (including Contributions) may always be       ',&
'Distributed subject to the version of the Agreement under which it was    ',&
'received. In addition, after a new version of the Agreement is published, ',&
'Contributor may elect to Distribute the Program (including its            ',&
'Contributions) under the new version.                                     ',&
'                                                                          ',&
'Except as expressly stated in Sections 2(a) and 2(b) above, Recipient     ',&
'receives no rights or licenses to the intellectual property of any        ',&
'Contributor under this Agreement, whether expressly, by implication,      ',&
'estoppel or otherwise. All rights in the Program not expressly granted    ',&
'under this Agreement are reserved. Nothing in this Agreement is intended  ',&
'to be enforceable by any entity that is not a Contributor or Recipient.   ',&
'No third-party beneficiary rights are created under this Agreement.       ',&
'                                                                          ',&
'Exhibit A - Form of Secondary Licenses Notice                             ',&
'                                                                          ',&
'"This Source Code may also be made available under the following          ',&
'Secondary Licenses when the conditions for such availability set forth    ',&
'in the Eclipse Public License, v. 2.0 are satisfied: {name license(s),    ',&
'version(s), and exceptions or additional permissions here}."              ',&
'                                                                          ',&
'  Simply including a copy of this Agreement, including this Exhibit A     ',&
'  is not sufficient to license the Source Code under Secondary Licenses.  ',&
'                                                                          ',&
'  If it is not possible or desirable to put the notice in a particular    ',&
'  file, then You may include the notice in a location (such as a LICENSE  ',&
'  file in a relevant directory) where a recipient would be likely to      ',&
'  look for such a notice.                                                 ',&
'                                                                          ',&
'  You may add additional accurate notices of copyright ownership.         ',&
'                                                                          ',&
'']
if(present(topic_only))then
   if(size(textblock)>0)then
      if(topic_only) textblock=[character(len=len(textblock(1))) :: textblock(1)]
   else
      textblock=['']
   endif
endif
case('23','eupl-1.1')
textblock=[ CHARACTER(LEN=128) :: &
'eupl-1.1',&
'        ',&
'European Union Public Licence',&
'V. 1.1                       ',&
'                             ',&
'                             ',&
'EUPL © the European Community 2007',&
'                                   ',&
'                                   ',&
'This European Union Public Licence (the “EUPL”) applies to the',&
'Work or Software (as defined below) which is provided under the terms of this',&
'Licence. Any use of the Work, other than as authorised under this Licence is ',&
'prohibited (to the extent such use is covered by a right of the copyright    ',&
'holder of the Work).                                                         ',&
'                                                                             ',&
'The Original Work is provided under the terms of this                        ',&
'Licence when the Licensor (as defined below) has placed the following notice ',&
'immediately following the copyright notice for the Original Work:            ',&
'                                                                             ',&
'Licensed under the EUPL V.1.1                                                ',&
'                                                                             ',&
'or has expressed by any other mean his willingness to license under the EUPL.',&
'                                                                             ',&
'                                                                             ',&
'1. Definitions                                                               ',&
'                                                                             ',&
'In this Licence, the                                                         ',&
'following terms have the following meaning:                                  ',&
'                                                                             ',&
'- The Licence: this Licence.                                                 ',&
'                                                                             ',&
'- The Original Work or the Software: the software distributed                ',&
'and/or communicated by the Licensor under this Licence, available as Source  ',&
'Code and also as Executable Code as the case may be.                         ',&
'                                                                             ',&
'- Derivative Works:                                                          ',&
'the works or software that could be created by the Licensee, based upon the  ',&
'Original Work or modifications thereof. This Licence does not define the     ',&
'extent of modification or dependence on the Original Work required in order to',&
'classify a work as a Derivative Work; this extent is determined by copyright  ',&
'law applicable in the country mentioned in Article 15.                        ',&
'                                                                              ',&
'- The Work: the Original Work and/or its Derivative Works.                    ',&
'                                                                              ',&
'- The Source Code: the human-readable form of the Work which is the most      ',&
'convenient for people to study and modify.                                    ',&
'                                                                              ',&
'- The Executable Code: any code which has generally been compiled and which   ',&
'is meant to be interpreted by a computer as a program.                        ',&
'                                                                              ',&
'- The Licensor: the natural or legal person that distributes and/or           ',&
'communicates the Work under the Licence.                                      ',&
'                                                                              ',&
'- Contributor(s): any natural or legal person who modifies the Work under the ',&
'Licence, or otherwise contributes to the creation of a Derivative Work.       ',&
'                                                                              ',&
'- The Licensee or “You”: any natural or legal person who makes any usage of',&
'the Software under the terms of the Licence.                                   ',&
'                                                                               ',&
'- Distribution and/or Communication: any act of selling, giving, lending,      ',&
'renting, distributing, communicating, transmitting, or otherwise               ',&
'making available, on-line or off-line, copies of the Work or providing access  ',&
'to its essential functionalities at the disposal of any other natural or legal ',&
'person.                                                                        ',&
'                                                                               ',&
'                                                                               ',&
'2. Scope of the rights granted by the Licence                                  ',&
'                                                                               ',&
'The Licensor hereby grants You a world-wide, royalty-free, non-exclusive,      ',&
'sub-licensable licence to do the following, for the duration of copyright      ',&
'vested in the Original Work:                                                   ',&
'                                                                               ',&
'- use the Work in any circumstance and for all usage,                          ',&
'- reproduce the Work,                                                          ',&
'- modify the Original Work, and make Derivative Works                          ',&
'based upon the Work,                                                           ',&
'- communicate to the public, including the right to make available or display  ',&
'the Work or copies thereof to the public and perform publicly, as the case     ',&
'may be, the Work,                                                              ',&
'- distribute the Work or copies thereof,                                       ',&
'- lend and rent the Work or copies thereof,                                    ',&
'- sub-license rights in the Work or copies thereof.                            ',&
'                                                                               ',&
'Those rights can be exercised on any media, supports and formats, whether now  ',&
'known or later invented, as far as the applicable law permits so.              ',&
'                                                                               ',&
'In the countries where moral rights apply, the Licensor waives his right to    ',&
'exercise his moral right to the extent allowed by law in order to make         ',&
'effective the licence of the economic rights here above listed.                ',&
'                                                                               ',&
'The Licensor grants to the Licensee royalty-free, non exclusive usage rights   ',&
'to any patents held by the Licensor, to the extent necessary to make use of    ',&
'the rights granted on the Work under this Licence.                             ',&
'                                                                               ',&
'                                                                               ',&
'3. Communication of the Source Code                                            ',&
'                                                                               ',&
'The Licensor may provide the Work either                                       ',&
'in its Source Code form, or as Executable Code. If the Work is provided as     ',&
'Executable Code, the Licensor provides in addition a machine-readable copy of  ',&
'the Source Code of the Work along with each copy of the Work that the Licensor ',&
'distributes or indicates, in a notice following the copyright notice attached  ',&
'to the Work, a repository where the Source Code is easily and freely           ',&
'accessible for as long as the Licensor continues to distribute and/or          ',&
'communicate the Work.                                                          ',&
'                                                                               ',&
'                                                                               ',&
'4. Limitations on copyright                                                    ',&
'                                                                               ',&
'Nothing in this Licence is intended to deprive the Licensee of the benefits    ',&
'from any exception or limitation to the exclusive rights of the rights owners  ',&
'in the Original Work or Software, of the exhaustion of those rights or of      ',&
'other applicable limitations thereto.                                          ',&
'                                                                               ',&
'                                                                               ',&
'5. Obligations of the Licensee                                                 ',&
'                                                                               ',&
'The grant of the rights mentioned above is subject to some restrictions and    ',&
'obligations imposed on the Licensee. Those obligations are the following:      ',&
'                                                                               ',&
'Attribution right:                                                             ',&
'the Licensee shall keep intact all copyright, patent or trademarks notices and ',&
'all notices that refer to the Licence and to the disclaimer of warranties. The ',&
'Licensee must include a copy of such notices and a copy of the Licence with    ',&
'every copy of the Work he/she distributes and/or communicates. The Licensee    ',&
'must cause any Derivative Work to carry prominent notices stating that the     ',&
'Work has been modified and the date of modification.                           ',&
'                                                                               ',&
'Copyleft clause:                                                               ',&
'If the Licensee distributes and/or communicates copies of the Original Works   ',&
'or Derivative Works based upon the Original Work, this Distribution and/or     ',&
'Communication will be done under the terms of this Licence or of a later       ',&
'version of this Licence unless the Original Work is expressly distributed only ',&
'under this version of the Licence. The Licensee (becoming Licensor) cannot     ',&
'offer or impose any additional terms or conditions on the Work or Derivative   ',&
'Work that alter or restrict the terms of the Licence.                          ',&
'                                                                               ',&
'Compatibility clause:                                                          ',&
'If the Licensee Distributes and/or Communicates Derivative Works or copies     ',&
'thereof based upon both the Original Work and another work  licensed under a   ',&
'Compatible Licence, this Distribution and/or Communication can be done under   ',&
'the terms of this Compatible Licence. For the sake of this clause,             ',&
'“Compatible Licence” refers to the licences listed in the appendix         ',&
'attached to this Licence. Should the Licensee’s obligations under the        ',&
'Compatible Licence conflict with his/her obligations under this Licence, the   ',&
'obligations of the Compatible Licence shall prevail.                           ',&
'                                                                               ',&
'Provision of Source Code:                                                      ',&
'When distributing and/or communicating copies of the Work, the Licensee        ',&
'will provide a machine-readable copy of the Source Code or indicate a          ',&
'repository where this Source will be easily and freely available for as long   ',&
'as the Licensee continues to distribute and/or communicate the Work.           ',&
'                                                                               ',&
'Legal Protection:                                                              ',&
'This Licence does not grant permission to use the trade names,                 ',&
'trademarks, service marks, or names of the Licensor, except as required for    ',&
'reasonable and customary use in describing the origin of the Work and          ',&
'reproducing the content of the copyright notice.                               ',&
'                                                                               ',&
'                                                                               ',&
'6. Chain of Authorship                                                         ',&
'                                                                               ',&
'The original Licensor warrants that the copyright in the Original Work         ',&
'granted hereunder is owned by him/her or licensed to him/her and               ',&
'that he/she has the power and authority to grant the Licence.                  ',&
'                                                                               ',&
'Each Contributor warrants that the copyright in the modifications he/she       ',&
'brings to the Work are owned by him/her or licensed to him/her and that        ',&
'he/she has the power and authority to grant the Licence.                       ',&
'                                                                               ',&
'Each time You accept the Licence, the original Licensor and subsequent         ',&
'Contributors grant You a licence to their contributions to the Work, under     ',&
'the terms of this Licence.                                                     ',&
'                                                                               ',&
'                                                                               ',&
'7. Disclaimer of Warranty                                                      ',&
'                                                                               ',&
'The Work is a work in progress, which is continuously improved by numerous     ',&
'contributors. It is not a finished work and may therefore contain defects or   ',&
'“bugs” inherent to this type of software development.                      ',&
'                                                                               ',&
'For the above reason, the Work is provided under the Licence on an “as is” ',&
'basis and without warranties of any kind concerning the Work, including        ',&
'without limitation merchantability, fitness for a particular purpose, absence  ',&
'of defects or errors, accuracy, non-infringement of intellectual property      ',&
'rights other than copyright as stated in Article 6 of this Licence.            ',&
'                                                                               ',&
'This disclaimer of warranty is an essential part of the Licence and a          ',&
'condition for the grant of any rights to the Work.                             ',&
'                                                                               ',&
'                                                                               ',&
'8. Disclaimer of Liability                                                     ',&
'                                                                               ',&
'Except in the cases of wilful misconduct or damages directly caused to         ',&
'natural persons, the Licensor will in no event be liable for any direct or     ',&
'indirect, material or moral, damages of any kind, arising out of the Licence   ',&
'or of the use of the Work, including without limitation,                       ',&
'damages for loss of goodwill, work stoppage, computer failure or malfunction,  ',&
'loss of data or any commercial damage, even if the Licensor has been advised   ',&
'of the possibility of such damage. However, the Licensor will be liable under  ',&
'statutory product liability laws as far such laws apply to the Work.           ',&
'                                                                               ',&
'                                                                               ',&
'9. Additional agreements                                                       ',&
'                                                                               ',&
'While distributing the Original Work or Derivative Works, You may choose       ',&
'to conclude an additional agreement to offer, and charge a fee for,            ',&
'acceptance of support, warranty, indemnity, or other liability                 ',&
'obligations and/or services consistent with this Licence. However, in          ',&
'accepting such obligations, You may act only on your own behalf and on your    ',&
'sole responsibility, not on behalf of the original Licensor or any other       ',&
'Contributor, and only if You agree to indemnify, defend, and hold each         ',&
'Contributor harmless for any liability incurred by, or claims asserted against ',&
'such Contributor by the fact You have accepted any such warranty or additional ',&
'liability.                                                                     ',&
'                                                                               ',&
'                                                                               ',&
'10. Acceptance of the Licence                                                  ',&
'                                                                               ',&
'The provisions of this Licence can be accepted by clicking on                  ',&
'an icon “I agree” placed under the bottom of a window displaying the text of',&
'this Licence or by affirming consent in any other similar way, in accordance    ',&
'with the rules of applicable law. Clicking on that icon indicates your clear    ',&
'and irrevocable acceptance of this Licence and                                  ',&
'all of its terms and conditions.                                                ',&
'                                                                                ',&
'Similarly, you irrevocably accept this Licence and                              ',&
'all of its terms and conditions by exercising any rights granted to You         ',&
'by Article 2 of this Licence, such as the use of the Work,                      ',&
'the creation by You of a Derivative Work or the Distribution and/or             ',&
'Communication by You of the Work or copies thereof.                             ',&
'                                                                                ',&
'                                                                                ',&
'11. Information to the public                                                   ',&
'                                                                                ',&
'In case of any Distribution and/or Communication of the Work by means of        ',&
'electronic communication by You (for example, by offering to download           ',&
'the Work from a remote location) the distribution channel or media (for         ',&
'example, a website) must at least provide to the public the information         ',&
'requested by the applicable law regarding the Licensor, the Licence and the     ',&
'way it may be accessible, concluded, stored and reproduced by the               ',&
'Licensee.                                                                       ',&
'                                                                                ',&
'                                                                                ',&
'12. Termination of the Licence                                                  ',&
'                                                                                ',&
'The Licence and the rights granted hereunder will terminate automatically       ',&
'upon any breach by the Licensee of the terms of the Licence.                    ',&
'                                                                                ',&
'Such a termination will not terminate the licences of any person who has        ',&
'received the Work from the Licensee under the Licence, provided such persons    ',&
'remain in full compliance with the Licence.                                     ',&
'                                                                                ',&
'                                                                                ',&
'13. Miscellaneous                                                               ',&
'                                                                                ',&
'Without prejudice of Article 9 above, the Licence represents the complete       ',&
'agreement between the Parties as to the Work licensed hereunder.                ',&
'                                                                                ',&
'If any provision of the Licence is invalid or unenforceable under applicable    ',&
'law, this will not affect the validity or enforceability of the Licence as a    ',&
'whole. Such provision will be construed and/or reformed so as necessary         ',&
'to make it valid and enforceable.                                               ',&
'                                                                                ',&
'The European Commission may publish other linguistic versions and/or new        ',&
'versions of this Licence, so far this is required and reasonable, without       ',&
'reducing the scope of the rights granted by the Licence.                        ',&
'New versions of the Licence will be published with a unique version number.     ',&
'                                                                                ',&
'All linguistic versions of this Licence, approved by the European Commission,   ',&
'have identical value. Parties can take advantage of the linguistic version      ',&
'of their choice.                                                                ',&
'                                                                                ',&
'                                                                                ',&
'14. Jurisdiction                                                                ',&
'                                                                                ',&
'Any litigation resulting from the interpretation of this License, arising       ',&
'between the European Commission, as a Licensor, and any Licensee,               ',&
'will be subject to the jurisdiction of the Court of Justice of the              ',&
'European Communities, as laid down in article 238 of the Treaty establishing    ',&
'the European Community.                                                         ',&
'                                                                                ',&
'Any litigation arising between Parties, other than the European Commission,     ',&
'and resulting from the interpretation of this License, will be subject to the   ',&
'exclusive jurisdiction of the competent court where the Licensor resides or     ',&
'conducts its primary business.                                                  ',&
'                                                                                ',&
'                                                                                ',&
'15. Applicable Law                                                              ',&
'                                                                                ',&
'This Licence shall be governed by the law of the European Union country where   ',&
'the Licensor resides or has his registered office.                              ',&
'                                                                                ',&
'This licence shall be governed by the Belgian law if:                           ',&
'                                                                                ',&
'- a litigation arises between the European Commission, as a Licensor, and any   ',&
'Licensee;                                                                       ',&
'- the Licensor, other than the European Commission, has no residence or         ',&
'registered office inside a European Union country.                              ',&
'                                                                                ',&
'                                                                                ',&
'===                                                                             ',&
'                                                                                ',&
'                                                                                ',&
'Appendix                                                                        ',&
'                                                                                ',&
'                                                                                ',&
'“Compatible Licences” according to article 5 EUPL are:                      ',&
'- GNU General Public License (GNU GPL) v. 2                                     ',&
'- Open Software License (OSL) v. 2.1, v. 3.0                                    ',&
'- Common Public License v. 1.0                                                  ',&
'- Eclipse Public License v. 1.0                                                 ',&
'- Cecill v. 2.0                                                                 ',&
'                                                                                ',&
'']
if(present(topic_only))then
   if(size(textblock)>0)then
      if(topic_only) textblock=[character(len=len(textblock(1))) :: textblock(1)]
   else
      textblock=['']
   endif
endif
case('24','eupl-1.2')
textblock=[ CHARACTER(LEN=128) :: &
'eupl-1.2',&
'        ',&
'                      EUROPEAN UNION PUBLIC LICENCE v. 1.2',&
'                      EUPL © the European Union 2007, 2016',&
'                                                           ',&
'This European Union Public Licence (the ‘EUPL’) applies to the Work (as',&
'defined below) which is provided under the terms of this Licence. Any use of',&
'the Work, other than as authorised under this Licence is prohibited (to the ',&
'extent such use is covered by a right of the copyright holder of the Work). ',&
'                                                                            ',&
'The Work is provided under the terms of this Licence when the Licensor (as  ',&
'defined below) has placed the following notice immediately following the    ',&
'copyright notice for the Work:                                              ',&
'                                                                            ',&
'        Licensed under the EUPL                                             ',&
'                                                                            ',&
'or has expressed by any other means his willingness to license under the EUPL.',&
'                                                                              ',&
'1. Definitions                                                                ',&
'                                                                              ',&
'In this Licence, the following terms have the following meaning:              ',&
'                                                                              ',&
'- ‘The Licence’: this Licence.                                            ',&
'                                                                              ',&
'- ‘The Original Work’: the work or software distributed or communicated by the',&
'  Licensor under this Licence, available as Source Code and also as Executable    ',&
'  Code as the case may be.                                                        ',&
'                                                                                  ',&
'- ‘Derivative Works’: the works or software that could be created by the      ',&
'  Licensee, based upon the Original Work or modifications thereof. This           ',&
'  Licence does not define the extent of modification or dependence on the         ',&
'  Original Work required in order to classify a work as a Derivative Work;        ',&
'  this extent is determined by copyright law applicable in the country            ',&
'  mentioned in Article 15.                                                        ',&
'                                                                                  ',&
'- ‘The Work’: the Original Work or its Derivative Works.                      ',&
'                                                                                  ',&
'- ‘The Source Code’: the human-readable form of the Work which is the most    ',&
'  convenient for people to study and modify.                                      ',&
'                                                                                  ',&
'- ‘The Executable Code’: any code which has generally been compiled and which ',&
'  is meant to be interpreted by a computer as a program.                          ',&
'                                                                                  ',&
'- ‘The Licensor’: the natural or legal person that distributes or communicates',&
'  the Work under the Licence.                                                     ',&
'                                                                                  ',&
'- ‘Contributor(s)’: any natural or legal person who modifies the Work under   ',&
'  the Licence, or otherwise contributes to the creation of a Derivative Work.     ',&
'                                                                                  ',&
'- ‘The Licensee’ or ‘You’: any natural or legal person who makes any usage of',&
'  the Work under the terms of the Licence.                                           ',&
'                                                                                     ',&
'- ‘Distribution’ or ‘Communication’: any act of selling, giving, lending,    ',&
'  renting, distributing, communicating, transmitting, or otherwise making            ',&
'  available, online or offline, copies of the Work or providing access to its        ',&
'  essential functionalities at the disposal of any other natural or legal            ',&
'  person.                                                                            ',&
'                                                                                     ',&
'2. Scope of the rights granted by the Licence                                        ',&
'                                                                                     ',&
'The Licensor hereby grants You a worldwide, royalty-free, non-exclusive,             ',&
'sublicensable licence to do the following, for the duration of copyright             ',&
'vested in the Original Work:                                                         ',&
'                                                                                     ',&
'- use the Work in any circumstance and for all usage,                                ',&
'- reproduce the Work,                                                                ',&
'- modify the Work, and make Derivative Works based upon the Work,                    ',&
'- communicate to the public, including the right to make available or display        ',&
'  the Work or copies thereof to the public and perform publicly, as the case         ',&
'  may be, the Work,                                                                  ',&
'- distribute the Work or copies thereof,                                             ',&
'- lend and rent the Work or copies thereof,                                          ',&
'- sublicense rights in the Work or copies thereof.                                   ',&
'                                                                                     ',&
'Those rights can be exercised on any media, supports and formats, whether now        ',&
'known or later invented, as far as the applicable law permits so.                    ',&
'                                                                                     ',&
'In the countries where moral rights apply, the Licensor waives his right to          ',&
'exercise his moral right to the extent allowed by law in order to make               ',&
'effective the licence of the economic rights here above listed.                      ',&
'                                                                                     ',&
'The Licensor grants to the Licensee royalty-free, non-exclusive usage rights         ',&
'to any patents held by the Licensor, to the extent necessary to make use of          ',&
'the rights granted on the Work under this Licence.                                   ',&
'                                                                                     ',&
'3. Communication of the Source Code                                                  ',&
'                                                                                     ',&
'The Licensor may provide the Work either in its Source Code form, or as              ',&
'Executable Code. If the Work is provided as Executable Code, the Licensor            ',&
'provides in addition a machine-readable copy of the Source Code of the Work          ',&
'along with each copy of the Work that the Licensor distributes or indicates,         ',&
'in a notice following the copyright notice attached to the Work, a repository        ',&
'where the Source Code is easily and freely accessible for as long as the             ',&
'Licensor continues to distribute or communicate the Work.                            ',&
'                                                                                     ',&
'4. Limitations on copyright                                                          ',&
'                                                                                     ',&
'Nothing in this Licence is intended to deprive the Licensee of the benefits          ',&
'from any exception or limitation to the exclusive rights of the rights owners        ',&
'in the Work, of the exhaustion of those rights or of other applicable                ',&
'limitations thereto.                                                                 ',&
'                                                                                     ',&
'5. Obligations of the Licensee                                                       ',&
'                                                                                     ',&
'The grant of the rights mentioned above is subject to some restrictions and          ',&
'obligations imposed on the Licensee. Those obligations are the following:            ',&
'                                                                                     ',&
'Attribution right: The Licensee shall keep intact all copyright, patent or           ',&
'trademarks notices and all notices that refer to the Licence and to the              ',&
'disclaimer of warranties. The Licensee must include a copy of such notices and       ',&
'a copy of the Licence with every copy of the Work he/she distributes or              ',&
'communicates. The Licensee must cause any Derivative Work to carry prominent         ',&
'notices stating that the Work has been modified and the date of modification.        ',&
'                                                                                     ',&
'Copyleft clause: If the Licensee distributes or communicates copies of the           ',&
'Original Works or Derivative Works, this Distribution or Communication will be       ',&
'done under the terms of this Licence or of a later version of this Licence           ',&
'unless the Original Work is expressly distributed only under this version of         ',&
'the Licence — for example by communicating ‘EUPL v. 1.2 only’. The Licensee    ',&
'(becoming Licensor) cannot offer or impose any additional terms or conditions        ',&
'on the Work or Derivative Work that alter or restrict the terms of the               ',&
'Licence.                                                                             ',&
'                                                                                     ',&
'Compatibility clause: If the Licensee Distributes or Communicates Derivative         ',&
'Works or copies thereof based upon both the Work and another work licensed           ',&
'under a Compatible Licence, this Distribution or Communication can be done           ',&
'under the terms of this Compatible Licence. For the sake of this clause,             ',&
'‘Compatible Licence’ refers to the licences listed in the appendix attached to   ',&
'this Licence. Should the Licensee''s obligations under the Compatible Licence        ',&
'conflict with his/her obligations under this Licence, the obligations of the         ',&
'Compatible Licence shall prevail.                                                    ',&
'                                                                                     ',&
'Provision of Source Code: When distributing or communicating copies of the           ',&
'Work, the Licensee will provide a machine-readable copy of the Source Code or        ',&
'indicate a repository where this Source will be easily and freely available          ',&
'for as long as the Licensee continues to distribute or communicate the Work.         ',&
'                                                                                     ',&
'Legal Protection: This Licence does not grant permission to use the trade            ',&
'names, trademarks, service marks, or names of the Licensor, except as required       ',&
'for reasonable and customary use in describing the origin of the Work and            ',&
'reproducing the content of the copyright notice.                                     ',&
'                                                                                     ',&
'6. Chain of Authorship                                                               ',&
'                                                                                     ',&
'The original Licensor warrants that the copyright in the Original Work granted       ',&
'hereunder is owned by him/her or licensed to him/her and that he/she has the         ',&
'power and authority to grant the Licence.                                            ',&
'                                                                                     ',&
'Each Contributor warrants that the copyright in the modifications he/she             ',&
'brings to the Work are owned by him/her or licensed to him/her and that he/she       ',&
'has the power and authority to grant the Licence.                                    ',&
'                                                                                     ',&
'Each time You accept the Licence, the original Licensor and subsequent               ',&
'Contributors grant You a licence to their contributions to the Work, under the       ',&
'terms of this Licence.                                                               ',&
'                                                                                     ',&
'7. Disclaimer of Warranty                                                            ',&
'                                                                                     ',&
'The Work is a work in progress, which is continuously improved by numerous           ',&
'Contributors. It is not a finished work and may therefore contain defects or         ',&
'‘bugs’ inherent to this type of development.                                     ',&
'                                                                                     ',&
'For the above reason, the Work is provided under the Licence on an ‘as is’       ',&
'basis and without warranties of any kind concerning the Work, including              ',&
'without limitation merchantability, fitness for a particular purpose, absence        ',&
'of defects or errors, accuracy, non-infringement of intellectual property            ',&
'rights other than copyright as stated in Article 6 of this Licence.                  ',&
'                                                                                     ',&
'This disclaimer of warranty is an essential part of the Licence and a                ',&
'condition for the grant of any rights to the Work.                                   ',&
'                                                                                     ',&
'8. Disclaimer of Liability                                                           ',&
'                                                                                     ',&
'Except in the cases of wilful misconduct or damages directly caused to natural       ',&
'persons, the Licensor will in no event be liable for any direct or indirect,         ',&
'material or moral, damages of any kind, arising out of the Licence or of the         ',&
'use of the Work, including without limitation, damages for loss of goodwill,         ',&
'work stoppage, computer failure or malfunction, loss of data or any commercial       ',&
'damage, even if the Licensor has been advised of the possibility of such             ',&
'damage. However, the Licensor will be liable under statutory product liability       ',&
'laws as far such laws apply to the Work.                                             ',&
'                                                                                     ',&
'9. Additional agreements                                                             ',&
'                                                                                     ',&
'While distributing the Work, You may choose to conclude an additional                ',&
'agreement, defining obligations or services consistent with this Licence.            ',&
'However, if accepting obligations, You may act only on your own behalf and on        ',&
'your sole responsibility, not on behalf of the original Licensor or any other        ',&
'Contributor, and only if You agree to indemnify, defend, and hold each               ',&
'Contributor harmless for any liability incurred by, or claims asserted against       ',&
'such Contributor by the fact You have accepted any warranty or additional            ',&
'liability.                                                                           ',&
'                                                                                     ',&
'10. Acceptance of the Licence                                                        ',&
'                                                                                     ',&
'The provisions of this Licence can be accepted by clicking on an icon ‘I           ',&
'agree’ placed under the bottom of a window displaying the text of this Licence     ',&
'or by affirming consent in any other similar way, in accordance with the rules       ',&
'of applicable law. Clicking on that icon indicates your clear and irrevocable        ',&
'acceptance of this Licence and all of its terms and conditions.                      ',&
'                                                                                     ',&
'Similarly, you irrevocably accept this Licence and all of its terms and              ',&
'conditions by exercising any rights granted to You by Article 2 of this              ',&
'Licence, such as the use of the Work, the creation by You of a Derivative Work       ',&
'or the Distribution or Communication by You of the Work or copies thereof.           ',&
'                                                                                     ',&
'11. Information to the public                                                        ',&
'                                                                                     ',&
'In case of any Distribution or Communication of the Work by means of                 ',&
'electronic communication by You (for example, by offering to download the Work       ',&
'from a remote location) the distribution channel or media (for example, a            ',&
'website) must at least provide to the public the information requested by the        ',&
'applicable law regarding the Licensor, the Licence and the way it may be             ',&
'accessible, concluded, stored and reproduced by the Licensee.                        ',&
'                                                                                     ',&
'12. Termination of the Licence                                                       ',&
'                                                                                     ',&
'The Licence and the rights granted hereunder will terminate automatically upon       ',&
'any breach by the Licensee of the terms of the Licence.                              ',&
'                                                                                     ',&
'Such a termination will not terminate the licences of any person who has             ',&
'received the Work from the Licensee under the Licence, provided such persons         ',&
'remain in full compliance with the Licence.                                          ',&
'                                                                                     ',&
'13. Miscellaneous                                                                    ',&
'                                                                                     ',&
'Without prejudice of Article 9 above, the Licence represents the complete            ',&
'agreement between the Parties as to the Work.                                        ',&
'                                                                                     ',&
'If any provision of the Licence is invalid or unenforceable under applicable         ',&
'law, this will not affect the validity or enforceability of the Licence as a         ',&
'whole. Such provision will be construed or reformed so as necessary to make it       ',&
'valid and enforceable.                                                               ',&
'                                                                                     ',&
'The European Commission may publish other linguistic versions or new versions        ',&
'of this Licence or updated versions of the Appendix, so far this is required         ',&
'and reasonable, without reducing the scope of the rights granted by the              ',&
'Licence. New versions of the Licence will be published with a unique version         ',&
'number.                                                                              ',&
'                                                                                     ',&
'All linguistic versions of this Licence, approved by the European Commission,        ',&
'have identical value. Parties can take advantage of the linguistic version of        ',&
'their choice.                                                                        ',&
'                                                                                     ',&
'14. Jurisdiction                                                                     ',&
'                                                                                     ',&
'Without prejudice to specific agreement between parties,                             ',&
'                                                                                     ',&
'- any litigation resulting from the interpretation of this License, arising          ',&
'  between the European Union institutions, bodies, offices or agencies, as a         ',&
'  Licensor, and any Licensee, will be subject to the jurisdiction of the Court       ',&
'  of Justice of the European Union, as laid down in article 272 of the Treaty        ',&
'  on the Functioning of the European Union,                                          ',&
'                                                                                     ',&
'- any litigation arising between other parties and resulting from the                ',&
'  interpretation of this License, will be subject to the exclusive                   ',&
'  jurisdiction of the competent court where the Licensor resides or conducts         ',&
'  its primary business.                                                              ',&
'                                                                                     ',&
'15. Applicable Law                                                                   ',&
'                                                                                     ',&
'Without prejudice to specific agreement between parties,                             ',&
'                                                                                     ',&
'- this Licence shall be governed by the law of the European Union Member State       ',&
'  where the Licensor has his seat, resides or has his registered office,             ',&
'                                                                                     ',&
'- this licence shall be governed by Belgian law if the Licensor has no seat,         ',&
'  residence or registered office inside a European Union Member State.               ',&
'                                                                                     ',&
'Appendix                                                                             ',&
'                                                                                     ',&
'‘Compatible Licences’ according to Article 5 EUPL are:                           ',&
'                                                                                     ',&
'- GNU General Public License (GPL) v. 2, v. 3                                        ',&
'- GNU Affero General Public License (AGPL) v. 3                                      ',&
'- Open Software License (OSL) v. 2.1, v. 3.0                                         ',&
'- Eclipse Public License (EPL) v. 1.0                                                ',&
'- CeCILL v. 2.0, v. 2.1                                                              ',&
'- Mozilla Public Licence (MPL) v. 2                                                  ',&
'- GNU Lesser General Public Licence (LGPL) v. 2.1, v. 3                              ',&
'- Creative Commons Attribution-ShareAlike v. 3.0 Unported (CC BY-SA 3.0) for         ',&
'  works other than software                                                          ',&
'- European Union Public Licence (EUPL) v. 1.1, v. 1.2                                ',&
'- Québec Free and Open-Source Licence — Reciprocity (LiLiQ-R) or Strong           ',&
'  Reciprocity (LiLiQ-R+).                                                            ',&
'                                                                                     ',&
'The European Commission may update this Appendix to later versions of the            ',&
'above licences without producing a new version of the EUPL, as long as they          ',&
'provide the rights granted in Article 2 of this Licence and protect the              ',&
'covered Source Code from exclusive appropriation.                                    ',&
'                                                                                     ',&
'All other changes or additions to this Appendix require the production of a          ',&
'new EUPL version.                                                                    ',&
'                                                                                     ',&
'']
if(present(topic_only))then
   if(size(textblock)>0)then
      if(topic_only) textblock=[character(len=len(textblock(1))) :: textblock(1)]
   else
      textblock=['']
   endif
endif
case('25','gfdl-1.3')
textblock=[ CHARACTER(LEN=128) :: &
'gfdl-1.3',&
'        ',&
'                GNU Free Documentation License',&
'                 Version 1.3, 3 November 2008 ',&
'                                              ',&
'                                              ',&
' Copyright (C) 2000, 2001, 2002, 2007, 2008 Free Software Foundation, Inc.',&
'     <https://fsf.org/>                                                   ',&
' Everyone is permitted to copy and distribute verbatim copies             ',&
' of this license document, but changing it is not allowed.                ',&
'                                                                          ',&
'0. PREAMBLE                                                               ',&
'                                                                          ',&
'The purpose of this License is to make a manual, textbook, or other       ',&
'functional and useful document "free" in the sense of freedom: to         ',&
'assure everyone the effective freedom to copy and redistribute it,        ',&
'with or without modifying it, either commercially or noncommercially.     ',&
'Secondarily, this License preserves for the author and publisher a way    ',&
'to get credit for their work, while not being considered responsible      ',&
'for modifications made by others.                                         ',&
'                                                                          ',&
'This License is a kind of "copyleft", which means that derivative         ',&
'works of the document must themselves be free in the same sense.  It      ',&
'complements the GNU General Public License, which is a copyleft           ',&
'license designed for free software.                                       ',&
'                                                                          ',&
'We have designed this License in order to use it for manuals for free     ',&
'software, because free software needs free documentation: a free          ',&
'program should come with manuals providing the same freedoms that the     ',&
'software does.  But this License is not limited to software manuals;      ',&
'it can be used for any textual work, regardless of subject matter or      ',&
'whether it is published as a printed book.  We recommend this License     ',&
'principally for works whose purpose is instruction or reference.          ',&
'                                                                          ',&
'                                                                          ',&
'1. APPLICABILITY AND DEFINITIONS                                          ',&
'                                                                          ',&
'This License applies to any manual or other work, in any medium, that     ',&
'contains a notice placed by the copyright holder saying it can be         ',&
'distributed under the terms of this License.  Such a notice grants a      ',&
'world-wide, royalty-free license, unlimited in duration, to use that      ',&
'work under the conditions stated herein.  The "Document", below,          ',&
'refers to any such manual or work.  Any member of the public is a         ',&
'licensee, and is addressed as "you".  You accept the license if you       ',&
'copy, modify or distribute the work in a way requiring permission         ',&
'under copyright law.                                                      ',&
'                                                                          ',&
'A "Modified Version" of the Document means any work containing the        ',&
'Document or a portion of it, either copied verbatim, or with              ',&
'modifications and/or translated into another language.                    ',&
'                                                                          ',&
'A "Secondary Section" is a named appendix or a front-matter section of    ',&
'the Document that deals exclusively with the relationship of the          ',&
'publishers or authors of the Document to the Document''s overall          ',&
'subject (or to related matters) and contains nothing that could fall      ',&
'directly within that overall subject.  (Thus, if the Document is in       ',&
'part a textbook of mathematics, a Secondary Section may not explain       ',&
'any mathematics.)  The relationship could be a matter of historical       ',&
'connection with the subject or with related matters, or of legal,         ',&
'commercial, philosophical, ethical or political position regarding        ',&
'them.                                                                     ',&
'                                                                          ',&
'The "Invariant Sections" are certain Secondary Sections whose titles      ',&
'are designated, as being those of Invariant Sections, in the notice       ',&
'that says that the Document is released under this License.  If a         ',&
'section does not fit the above definition of Secondary then it is not     ',&
'allowed to be designated as Invariant.  The Document may contain zero     ',&
'Invariant Sections.  If the Document does not identify any Invariant      ',&
'Sections then there are none.                                             ',&
'                                                                          ',&
'The "Cover Texts" are certain short passages of text that are listed,     ',&
'as Front-Cover Texts or Back-Cover Texts, in the notice that says that    ',&
'the Document is released under this License.  A Front-Cover Text may      ',&
'be at most 5 words, and a Back-Cover Text may be at most 25 words.        ',&
'                                                                          ',&
'A "Transparent" copy of the Document means a machine-readable copy,       ',&
'represented in a format whose specification is available to the           ',&
'general public, that is suitable for revising the document                ',&
'straightforwardly with generic text editors or (for images composed of    ',&
'pixels) generic paint programs or (for drawings) some widely available    ',&
'drawing editor, and that is suitable for input to text formatters or      ',&
'for automatic translation to a variety of formats suitable for input      ',&
'to text formatters.  A copy made in an otherwise Transparent file         ',&
'format whose markup, or absence of markup, has been arranged to thwart    ',&
'or discourage subsequent modification by readers is not Transparent.      ',&
'An image format is not Transparent if used for any substantial amount     ',&
'of text.  A copy that is not "Transparent" is called "Opaque".            ',&
'                                                                          ',&
'Examples of suitable formats for Transparent copies include plain         ',&
'ASCII without markup, Texinfo input format, LaTeX input format, SGML      ',&
'or XML using a publicly available DTD, and standard-conforming simple     ',&
'HTML, PostScript or PDF designed for human modification.  Examples of     ',&
'transparent image formats include PNG, XCF and JPG.  Opaque formats       ',&
'include proprietary formats that can be read and edited only by           ',&
'proprietary word processors, SGML or XML for which the DTD and/or         ',&
'processing tools are not generally available, and the                     ',&
'machine-generated HTML, PostScript or PDF produced by some word           ',&
'processors for output purposes only.                                      ',&
'                                                                          ',&
'The "Title Page" means, for a printed book, the title page itself,        ',&
'plus such following pages as are needed to hold, legibly, the material    ',&
'this License requires to appear in the title page.  For works in          ',&
'formats which do not have any title page as such, "Title Page" means      ',&
'the text near the most prominent appearance of the work''s title,         ',&
'preceding the beginning of the body of the text.                          ',&
'                                                                          ',&
'The "publisher" means any person or entity that distributes copies of     ',&
'the Document to the public.                                               ',&
'                                                                          ',&
'A section "Entitled XYZ" means a named subunit of the Document whose      ',&
'title either is precisely XYZ or contains XYZ in parentheses following    ',&
'text that translates XYZ in another language.  (Here XYZ stands for a     ',&
'specific section name mentioned below, such as "Acknowledgements",        ',&
'"Dedications", "Endorsements", or "History".)  To "Preserve the Title"    ',&
'of such a section when you modify the Document means that it remains a    ',&
'section "Entitled XYZ" according to this definition.                      ',&
'                                                                          ',&
'The Document may include Warranty Disclaimers next to the notice which    ',&
'states that this License applies to the Document.  These Warranty         ',&
'Disclaimers are considered to be included by reference in this            ',&
'License, but only as regards disclaiming warranties: any other            ',&
'implication that these Warranty Disclaimers may have is void and has      ',&
'no effect on the meaning of this License.                                 ',&
'                                                                          ',&
'2. VERBATIM COPYING                                                       ',&
'                                                                          ',&
'You may copy and distribute the Document in any medium, either            ',&
'commercially or noncommercially, provided that this License, the          ',&
'copyright notices, and the license notice saying this License applies     ',&
'to the Document are reproduced in all copies, and that you add no         ',&
'other conditions whatsoever to those of this License.  You may not use    ',&
'technical measures to obstruct or control the reading or further          ',&
'copying of the copies you make or distribute.  However, you may accept    ',&
'compensation in exchange for copies.  If you distribute a large enough    ',&
'number of copies you must also follow the conditions in section 3.        ',&
'                                                                          ',&
'You may also lend copies, under the same conditions stated above, and     ',&
'you may publicly display copies.                                          ',&
'                                                                          ',&
'                                                                          ',&
'3. COPYING IN QUANTITY                                                    ',&
'                                                                          ',&
'If you publish printed copies (or copies in media that commonly have      ',&
'printed covers) of the Document, numbering more than 100, and the         ',&
'Document''s license notice requires Cover Texts, you must enclose the     ',&
'copies in covers that carry, clearly and legibly, all these Cover         ',&
'Texts: Front-Cover Texts on the front cover, and Back-Cover Texts on      ',&
'the back cover.  Both covers must also clearly and legibly identify       ',&
'you as the publisher of these copies.  The front cover must present       ',&
'the full title with all words of the title equally prominent and          ',&
'visible.  You may add other material on the covers in addition.           ',&
'Copying with changes limited to the covers, as long as they preserve      ',&
'the title of the Document and satisfy these conditions, can be treated    ',&
'as verbatim copying in other respects.                                    ',&
'                                                                          ',&
'If the required texts for either cover are too voluminous to fit          ',&
'legibly, you should put the first ones listed (as many as fit             ',&
'reasonably) on the actual cover, and continue the rest onto adjacent      ',&
'pages.                                                                    ',&
'                                                                          ',&
'If you publish or distribute Opaque copies of the Document numbering      ',&
'more than 100, you must either include a machine-readable Transparent     ',&
'copy along with each Opaque copy, or state in or with each Opaque copy    ',&
'a computer-network location from which the general network-using          ',&
'public has access to download using public-standard network protocols     ',&
'a complete Transparent copy of the Document, free of added material.      ',&
'If you use the latter option, you must take reasonably prudent steps,     ',&
'when you begin distribution of Opaque copies in quantity, to ensure       ',&
'that this Transparent copy will remain thus accessible at the stated      ',&
'location until at least one year after the last time you distribute an    ',&
'Opaque copy (directly or through your agents or retailers) of that        ',&
'edition to the public.                                                    ',&
'                                                                          ',&
'It is requested, but not required, that you contact the authors of the    ',&
'Document well before redistributing any large number of copies, to        ',&
'give them a chance to provide you with an updated version of the          ',&
'Document.                                                                 ',&
'                                                                          ',&
'                                                                          ',&
'4. MODIFICATIONS                                                          ',&
'                                                                          ',&
'You may copy and distribute a Modified Version of the Document under      ',&
'the conditions of sections 2 and 3 above, provided that you release       ',&
'the Modified Version under precisely this License, with the Modified      ',&
'Version filling the role of the Document, thus licensing distribution     ',&
'and modification of the Modified Version to whoever possesses a copy      ',&
'of it.  In addition, you must do these things in the Modified Version:    ',&
'                                                                          ',&
'A. Use in the Title Page (and on the covers, if any) a title distinct     ',&
'   from that of the Document, and from those of previous versions         ',&
'   (which should, if there were any, be listed in the History section     ',&
'   of the Document).  You may use the same title as a previous version    ',&
'   if the original publisher of that version gives permission.            ',&
'B. List on the Title Page, as authors, one or more persons or entities    ',&
'   responsible for authorship of the modifications in the Modified        ',&
'   Version, together with at least five of the principal authors of the   ',&
'   Document (all of its principal authors, if it has fewer than five),    ',&
'   unless they release you from this requirement.                         ',&
'C. State on the Title page the name of the publisher of the               ',&
'   Modified Version, as the publisher.                                    ',&
'D. Preserve all the copyright notices of the Document.                    ',&
'E. Add an appropriate copyright notice for your modifications             ',&
'   adjacent to the other copyright notices.                               ',&
'F. Include, immediately after the copyright notices, a license notice     ',&
'   giving the public permission to use the Modified Version under the     ',&
'   terms of this License, in the form shown in the Addendum below.        ',&
'G. Preserve in that license notice the full lists of Invariant Sections   ',&
'   and required Cover Texts given in the Document''s license notice.      ',&
'H. Include an unaltered copy of this License.                             ',&
'I. Preserve the section Entitled "History", Preserve its Title, and add   ',&
'   to it an item stating at least the title, year, new authors, and       ',&
'   publisher of the Modified Version as given on the Title Page.  If      ',&
'   there is no section Entitled "History" in the Document, create one     ',&
'   stating the title, year, authors, and publisher of the Document as     ',&
'   given on its Title Page, then add an item describing the Modified      ',&
'   Version as stated in the previous sentence.                            ',&
'J. Preserve the network location, if any, given in the Document for       ',&
'   public access to a Transparent copy of the Document, and likewise      ',&
'   the network locations given in the Document for previous versions      ',&
'   it was based on.  These may be placed in the "History" section.        ',&
'   You may omit a network location for a work that was published at       ',&
'   least four years before the Document itself, or if the original        ',&
'   publisher of the version it refers to gives permission.                ',&
'K. For any section Entitled "Acknowledgements" or "Dedications",          ',&
'   Preserve the Title of the section, and preserve in the section all     ',&
'   the substance and tone of each of the contributor acknowledgements     ',&
'   and/or dedications given therein.                                      ',&
'L. Preserve all the Invariant Sections of the Document,                   ',&
'   unaltered in their text and in their titles.  Section numbers          ',&
'   or the equivalent are not considered part of the section titles.       ',&
'M. Delete any section Entitled "Endorsements".  Such a section            ',&
'   may not be included in the Modified Version.                           ',&
'N. Do not retitle any existing section to be Entitled "Endorsements"      ',&
'   or to conflict in title with any Invariant Section.                    ',&
'O. Preserve any Warranty Disclaimers.                                     ',&
'                                                                          ',&
'If the Modified Version includes new front-matter sections or             ',&
'appendices that qualify as Secondary Sections and contain no material     ',&
'copied from the Document, you may at your option designate some or all    ',&
'of these sections as invariant.  To do this, add their titles to the      ',&
'list of Invariant Sections in the Modified Version''s license notice.     ',&
'These titles must be distinct from any other section titles.              ',&
'                                                                          ',&
'You may add a section Entitled "Endorsements", provided it contains       ',&
'nothing but endorsements of your Modified Version by various              ',&
'parties--for example, statements of peer review or that the text has      ',&
'been approved by an organization as the authoritative definition of a     ',&
'standard.                                                                 ',&
'                                                                          ',&
'You may add a passage of up to five words as a Front-Cover Text, and a    ',&
'passage of up to 25 words as a Back-Cover Text, to the end of the list    ',&
'of Cover Texts in the Modified Version.  Only one passage of              ',&
'Front-Cover Text and one of Back-Cover Text may be added by (or           ',&
'through arrangements made by) any one entity.  If the Document already    ',&
'includes a cover text for the same cover, previously added by you or      ',&
'by arrangement made by the same entity you are acting on behalf of,       ',&
'you may not add another; but you may replace the old one, on explicit     ',&
'permission from the previous publisher that added the old one.            ',&
'                                                                          ',&
'The author(s) and publisher(s) of the Document do not by this License     ',&
'give permission to use their names for publicity for or to assert or      ',&
'imply endorsement of any Modified Version.                                ',&
'                                                                          ',&
'                                                                          ',&
'5. COMBINING DOCUMENTS                                                    ',&
'                                                                          ',&
'You may combine the Document with other documents released under this     ',&
'License, under the terms defined in section 4 above for modified          ',&
'versions, provided that you include in the combination all of the         ',&
'Invariant Sections of all of the original documents, unmodified, and      ',&
'list them all as Invariant Sections of your combined work in its          ',&
'license notice, and that you preserve all their Warranty Disclaimers.     ',&
'                                                                          ',&
'The combined work need only contain one copy of this License, and         ',&
'multiple identical Invariant Sections may be replaced with a single       ',&
'copy.  If there are multiple Invariant Sections with the same name but    ',&
'different contents, make the title of each such section unique by         ',&
'adding at the end of it, in parentheses, the name of the original         ',&
'author or publisher of that section if known, or else a unique number.    ',&
'Make the same adjustment to the section titles in the list of             ',&
'Invariant Sections in the license notice of the combined work.            ',&
'                                                                          ',&
'In the combination, you must combine any sections Entitled "History"      ',&
'in the various original documents, forming one section Entitled           ',&
'"History"; likewise combine any sections Entitled "Acknowledgements",     ',&
'and any sections Entitled "Dedications".  You must delete all sections    ',&
'Entitled "Endorsements".                                                  ',&
'                                                                          ',&
'                                                                          ',&
'6. COLLECTIONS OF DOCUMENTS                                               ',&
'                                                                          ',&
'You may make a collection consisting of the Document and other            ',&
'documents released under this License, and replace the individual         ',&
'copies of this License in the various documents with a single copy        ',&
'that is included in the collection, provided that you follow the rules    ',&
'of this License for verbatim copying of each of the documents in all      ',&
'other respects.                                                           ',&
'                                                                          ',&
'You may extract a single document from such a collection, and             ',&
'distribute it individually under this License, provided you insert a      ',&
'copy of this License into the extracted document, and follow this         ',&
'License in all other respects regarding verbatim copying of that          ',&
'document.                                                                 ',&
'                                                                          ',&
'                                                                          ',&
'7. AGGREGATION WITH INDEPENDENT WORKS                                     ',&
'                                                                          ',&
'A compilation of the Document or its derivatives with other separate      ',&
'and independent documents or works, in or on a volume of a storage or     ',&
'distribution medium, is called an "aggregate" if the copyright            ',&
'resulting from the compilation is not used to limit the legal rights      ',&
'of the compilation''s users beyond what the individual works permit.      ',&
'When the Document is included in an aggregate, this License does not      ',&
'apply to the other works in the aggregate which are not themselves        ',&
'derivative works of the Document.                                         ',&
'                                                                          ',&
'If the Cover Text requirement of section 3 is applicable to these         ',&
'copies of the Document, then if the Document is less than one half of     ',&
'the entire aggregate, the Document''s Cover Texts may be placed on        ',&
'covers that bracket the Document within the aggregate, or the             ',&
'electronic equivalent of covers if the Document is in electronic form.    ',&
'Otherwise they must appear on printed covers that bracket the whole       ',&
'aggregate.                                                                ',&
'                                                                          ',&
'                                                                          ',&
'8. TRANSLATION                                                            ',&
'                                                                          ',&
'Translation is considered a kind of modification, so you may              ',&
'distribute translations of the Document under the terms of section 4.     ',&
'Replacing Invariant Sections with translations requires special           ',&
'permission from their copyright holders, but you may include              ',&
'translations of some or all Invariant Sections in addition to the         ',&
'original versions of these Invariant Sections.  You may include a         ',&
'translation of this License, and all the license notices in the           ',&
'Document, and any Warranty Disclaimers, provided that you also include    ',&
'the original English version of this License and the original versions    ',&
'of those notices and disclaimers.  In case of a disagreement between      ',&
'the translation and the original version of this License or a notice      ',&
'or disclaimer, the original version will prevail.                         ',&
'                                                                          ',&
'If a section in the Document is Entitled "Acknowledgements",              ',&
'"Dedications", or "History", the requirement (section 4) to Preserve      ',&
'its Title (section 1) will typically require changing the actual          ',&
'title.                                                                    ',&
'                                                                          ',&
'                                                                          ',&
'9. TERMINATION                                                            ',&
'                                                                          ',&
'You may not copy, modify, sublicense, or distribute the Document          ',&
'except as expressly provided under this License.  Any attempt             ',&
'otherwise to copy, modify, sublicense, or distribute it is void, and      ',&
'will automatically terminate your rights under this License.              ',&
'                                                                          ',&
'However, if you cease all violation of this License, then your license    ',&
'from a particular copyright holder is reinstated (a) provisionally,       ',&
'unless and until the copyright holder explicitly and finally              ',&
'terminates your license, and (b) permanently, if the copyright holder     ',&
'fails to notify you of the violation by some reasonable means prior to    ',&
'60 days after the cessation.                                              ',&
'                                                                          ',&
'Moreover, your license from a particular copyright holder is              ',&
'reinstated permanently if the copyright holder notifies you of the        ',&
'violation by some reasonable means, this is the first time you have       ',&
'received notice of violation of this License (for any work) from that     ',&
'copyright holder, and you cure the violation prior to 30 days after       ',&
'your receipt of the notice.                                               ',&
'                                                                          ',&
'Termination of your rights under this section does not terminate the      ',&
'licenses of parties who have received copies or rights from you under     ',&
'this License.  If your rights have been terminated and not permanently    ',&
'reinstated, receipt of a copy of some or all of the same material does    ',&
'not give you any rights to use it.                                        ',&
'                                                                          ',&
'                                                                          ',&
'10. FUTURE REVISIONS OF THIS LICENSE                                      ',&
'                                                                          ',&
'The Free Software Foundation may publish new, revised versions of the     ',&
'GNU Free Documentation License from time to time.  Such new versions      ',&
'will be similar in spirit to the present version, but may differ in       ',&
'detail to address new problems or concerns.  See                          ',&
'https://www.gnu.org/licenses/.                                            ',&
'                                                                          ',&
'Each version of the License is given a distinguishing version number.     ',&
'If the Document specifies that a particular numbered version of this      ',&
'License "or any later version" applies to it, you have the option of      ',&
'following the terms and conditions either of that specified version or    ',&
'of any later version that has been published (not as a draft) by the      ',&
'Free Software Foundation.  If the Document does not specify a version     ',&
'number of this License, you may choose any version ever published (not    ',&
'as a draft) by the Free Software Foundation.  If the Document             ',&
'specifies that a proxy can decide which future versions of this           ',&
'License can be used, that proxy''s public statement of acceptance of a    ',&
'version permanently authorizes you to choose that version for the         ',&
'Document.                                                                 ',&
'                                                                          ',&
'11. RELICENSING                                                           ',&
'                                                                          ',&
'"Massive Multiauthor Collaboration Site" (or "MMC Site") means any        ',&
'World Wide Web server that publishes copyrightable works and also         ',&
'provides prominent facilities for anybody to edit those works.  A         ',&
'public wiki that anybody can edit is an example of such a server.  A      ',&
'"Massive Multiauthor Collaboration" (or "MMC") contained in the site      ',&
'means any set of copyrightable works thus published on the MMC site.      ',&
'                                                                          ',&
'"CC-BY-SA" means the Creative Commons Attribution-Share Alike 3.0         ',&
'license published by Creative Commons Corporation, a not-for-profit       ',&
'corporation with a principal place of business in San Francisco,          ',&
'California, as well as future copyleft versions of that license           ',&
'published by that same organization.                                      ',&
'                                                                          ',&
'"Incorporate" means to publish or republish a Document, in whole or in    ',&
'part, as part of another Document.                                        ',&
'                                                                          ',&
'An MMC is "eligible for relicensing" if it is licensed under this         ',&
'License, and if all works that were first published under this License    ',&
'somewhere other than this MMC, and subsequently incorporated in whole or  ',&
'in part into the MMC, (1) had no cover texts or invariant sections, and   ',&
'(2) were thus incorporated prior to November 1, 2008.                     ',&
'                                                                          ',&
'The operator of an MMC Site may republish an MMC contained in the site    ',&
'under CC-BY-SA on the same site at any time before August 1, 2009,        ',&
'provided the MMC is eligible for relicensing.                             ',&
'                                                                          ',&
'                                                                          ',&
'ADDENDUM: How to use this License for your documents                      ',&
'                                                                          ',&
'To use this License in a document you have written, include a copy of     ',&
'the License in the document and put the following copyright and           ',&
'license notices just after the title page:                                ',&
'                                                                          ',&
'    Copyright (c)  YEAR  YOUR NAME.                                       ',&
'    Permission is granted to copy, distribute and/or modify this document ',&
'    under the terms of the GNU Free Documentation License, Version 1.3    ',&
'    or any later version published by the Free Software Foundation;       ',&
'    with no Invariant Sections, no Front-Cover Texts, and no Back-Cover Texts.',&
'    A copy of the license is included in the section entitled "GNU            ',&
'    Free Documentation License".                                              ',&
'                                                                              ',&
'If you have Invariant Sections, Front-Cover Texts and Back-Cover Texts,       ',&
'replace the "with...Texts." line with this:                                   ',&
'                                                                              ',&
'    with the Invariant Sections being LIST THEIR TITLES, with the             ',&
'    Front-Cover Texts being LIST, and with the Back-Cover Texts being LIST.   ',&
'                                                                              ',&
'If you have Invariant Sections without Cover Texts, or some other             ',&
'combination of the three, merge those two alternatives to suit the            ',&
'situation.                                                                    ',&
'                                                                              ',&
'If your document contains nontrivial examples of program code, we             ',&
'recommend releasing these examples in parallel under your choice of           ',&
'free software license, such as the GNU General Public License,                ',&
'to permit their use in free software.                                         ',&
'                                                                              ',&
'']
if(present(topic_only))then
   if(size(textblock)>0)then
      if(topic_only) textblock=[character(len=len(textblock(1))) :: textblock(1)]
   else
      textblock=['']
   endif
endif
case('26','gpl-2.0')
textblock=[ CHARACTER(LEN=128) :: &
'gpl-2.0',&
'       ',&
'                    GNU GENERAL PUBLIC LICENSE',&
'                       Version 2, June 1991   ',&
'                                              ',&
' Copyright (C) 1989, 1991 Free Software Foundation, Inc.,',&
' 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA',&
' Everyone is permitted to copy and distribute verbatim copies',&
' of this license document, but changing it is not allowed.   ',&
'                                                             ',&
'                            Preamble                         ',&
'                                                             ',&
'  The licenses for most software are designed to take away your',&
'freedom to share and change it.  By contrast, the GNU General Public',&
'License is intended to guarantee your freedom to share and change free',&
'software--to make sure the software is free for all its users.  This  ',&
'General Public License applies to most of the Free Software           ',&
'Foundation''s software and to any other program whose authors commit to',&
'using it.  (Some other Free Software Foundation software is covered by ',&
'the GNU Lesser General Public License instead.)  You can apply it to   ',&
'your programs, too.                                                    ',&
'                                                                       ',&
'  When we speak of free software, we are referring to freedom, not     ',&
'price.  Our General Public Licenses are designed to make sure that you ',&
'have the freedom to distribute copies of free software (and charge for ',&
'this service if you wish), that you receive source code or can get it  ',&
'if you want it, that you can change the software or use pieces of it   ',&
'in new free programs; and that you know you can do these things.       ',&
'                                                                       ',&
'  To protect your rights, we need to make restrictions that forbid     ',&
'anyone to deny you these rights or to ask you to surrender the rights. ',&
'These restrictions translate to certain responsibilities for you if you',&
'distribute copies of the software, or if you modify it.                ',&
'                                                                       ',&
'  For example, if you distribute copies of such a program, whether     ',&
'gratis or for a fee, you must give the recipients all the rights that  ',&
'you have.  You must make sure that they, too, receive or can get the   ',&
'source code.  And you must show them these terms so they know their    ',&
'rights.                                                                ',&
'                                                                       ',&
'  We protect your rights with two steps: (1) copyright the software, and',&
'(2) offer you this license which gives you legal permission to copy,    ',&
'distribute and/or modify the software.                                  ',&
'                                                                        ',&
'  Also, for each author''s protection and ours, we want to make certain ',&
'that everyone understands that there is no warranty for this free       ',&
'software.  If the software is modified by someone else and passed on, we',&
'want its recipients to know that what they have is not the original, so ',&
'that any problems introduced by others will not reflect on the original ',&
'authors'' reputations.                                                  ',&
'                                                                        ',&
'  Finally, any free program is threatened constantly by software        ',&
'patents.  We wish to avoid the danger that redistributors of a free     ',&
'program will individually obtain patent licenses, in effect making the  ',&
'program proprietary.  To prevent this, we have made it clear that any   ',&
'patent must be licensed for everyone''s free use or not licensed at all.',&
'                                                                        ',&
'  The precise terms and conditions for copying, distribution and        ',&
'modification follow.                                                    ',&
'                                                                        ',&
'                    GNU GENERAL PUBLIC LICENSE                          ',&
'   TERMS AND CONDITIONS FOR COPYING, DISTRIBUTION AND MODIFICATION      ',&
'                                                                        ',&
'  0. This License applies to any program or other work which contains   ',&
'a notice placed by the copyright holder saying it may be distributed    ',&
'under the terms of this General Public License.  The "Program", below,  ',&
'refers to any such program or work, and a "work based on the Program"   ',&
'means either the Program or any derivative work under copyright law:    ',&
'that is to say, a work containing the Program or a portion of it,       ',&
'either verbatim or with modifications and/or translated into another    ',&
'language.  (Hereinafter, translation is included without limitation in  ',&
'the term "modification".)  Each licensee is addressed as "you".         ',&
'                                                                        ',&
'Activities other than copying, distribution and modification are not    ',&
'covered by this License; they are outside its scope.  The act of        ',&
'running the Program is not restricted, and the output from the Program  ',&
'is covered only if its contents constitute a work based on the          ',&
'Program (independent of having been made by running the Program).       ',&
'Whether that is true depends on what the Program does.                  ',&
'                                                                        ',&
'  1. You may copy and distribute verbatim copies of the Program''s      ',&
'source code as you receive it, in any medium, provided that you         ',&
'conspicuously and appropriately publish on each copy an appropriate     ',&
'copyright notice and disclaimer of warranty; keep intact all the        ',&
'notices that refer to this License and to the absence of any warranty;  ',&
'and give any other recipients of the Program a copy of this License     ',&
'along with the Program.                                                 ',&
'                                                                        ',&
'You may charge a fee for the physical act of transferring a copy, and   ',&
'you may at your option offer warranty protection in exchange for a fee. ',&
'                                                                        ',&
'  2. You may modify your copy or copies of the Program or any portion   ',&
'of it, thus forming a work based on the Program, and copy and           ',&
'distribute such modifications or work under the terms of Section 1      ',&
'above, provided that you also meet all of these conditions:             ',&
'                                                                        ',&
'    a) You must cause the modified files to carry prominent notices     ',&
'    stating that you changed the files and the date of any change.      ',&
'                                                                        ',&
'    b) You must cause any work that you distribute or publish, that in  ',&
'    whole or in part contains or is derived from the Program or any     ',&
'    part thereof, to be licensed as a whole at no charge to all third   ',&
'    parties under the terms of this License.                            ',&
'                                                                        ',&
'    c) If the modified program normally reads commands interactively    ',&
'    when run, you must cause it, when started running for such          ',&
'    interactive use in the most ordinary way, to print or display an    ',&
'    announcement including an appropriate copyright notice and a        ',&
'    notice that there is no warranty (or else, saying that you provide  ',&
'    a warranty) and that users may redistribute the program under       ',&
'    these conditions, and telling the user how to view a copy of this   ',&
'    License.  (Exception: if the Program itself is interactive but      ',&
'    does not normally print such an announcement, your work based on    ',&
'    the Program is not required to print an announcement.)              ',&
'                                                                        ',&
'These requirements apply to the modified work as a whole.  If           ',&
'identifiable sections of that work are not derived from the Program,    ',&
'and can be reasonably considered independent and separate works in      ',&
'themselves, then this License, and its terms, do not apply to those     ',&
'sections when you distribute them as separate works.  But when you      ',&
'distribute the same sections as part of a whole which is a work based   ',&
'on the Program, the distribution of the whole must be on the terms of   ',&
'this License, whose permissions for other licensees extend to the       ',&
'entire whole, and thus to each and every part regardless of who wrote it.',&
'                                                                         ',&
'Thus, it is not the intent of this section to claim rights or contest    ',&
'your rights to work written entirely by you; rather, the intent is to    ',&
'exercise the right to control the distribution of derivative or          ',&
'collective works based on the Program.                                   ',&
'                                                                         ',&
'In addition, mere aggregation of another work not based on the Program   ',&
'with the Program (or with a work based on the Program) on a volume of    ',&
'a storage or distribution medium does not bring the other work under     ',&
'the scope of this License.                                               ',&
'                                                                         ',&
'  3. You may copy and distribute the Program (or a work based on it,     ',&
'under Section 2) in object code or executable form under the terms of    ',&
'Sections 1 and 2 above provided that you also do one of the following:   ',&
'                                                                         ',&
'    a) Accompany it with the complete corresponding machine-readable     ',&
'    source code, which must be distributed under the terms of Sections   ',&
'    1 and 2 above on a medium customarily used for software interchange; or,',&
'                                                                            ',&
'    b) Accompany it with a written offer, valid for at least three          ',&
'    years, to give any third party, for a charge no more than your          ',&
'    cost of physically performing source distribution, a complete           ',&
'    machine-readable copy of the corresponding source code, to be           ',&
'    distributed under the terms of Sections 1 and 2 above on a medium       ',&
'    customarily used for software interchange; or,                          ',&
'                                                                            ',&
'    c) Accompany it with the information you received as to the offer       ',&
'    to distribute corresponding source code.  (This alternative is          ',&
'    allowed only for noncommercial distribution and only if you             ',&
'    received the program in object code or executable form with such        ',&
'    an offer, in accord with Subsection b above.)                           ',&
'                                                                            ',&
'The source code for a work means the preferred form of the work for         ',&
'making modifications to it.  For an executable work, complete source        ',&
'code means all the source code for all modules it contains, plus any        ',&
'associated interface definition files, plus the scripts used to             ',&
'control compilation and installation of the executable.  However, as a      ',&
'special exception, the source code distributed need not include             ',&
'anything that is normally distributed (in either source or binary           ',&
'form) with the major components (compiler, kernel, and so on) of the        ',&
'operating system on which the executable runs, unless that component        ',&
'itself accompanies the executable.                                          ',&
'                                                                            ',&
'If distribution of executable or object code is made by offering            ',&
'access to copy from a designated place, then offering equivalent            ',&
'access to copy the source code from the same place counts as                ',&
'distribution of the source code, even though third parties are not          ',&
'compelled to copy the source along with the object code.                    ',&
'                                                                            ',&
'  4. You may not copy, modify, sublicense, or distribute the Program        ',&
'except as expressly provided under this License.  Any attempt               ',&
'otherwise to copy, modify, sublicense or distribute the Program is          ',&
'void, and will automatically terminate your rights under this License.      ',&
'However, parties who have received copies, or rights, from you under        ',&
'this License will not have their licenses terminated so long as such        ',&
'parties remain in full compliance.                                          ',&
'                                                                            ',&
'  5. You are not required to accept this License, since you have not        ',&
'signed it.  However, nothing else grants you permission to modify or        ',&
'distribute the Program or its derivative works.  These actions are          ',&
'prohibited by law if you do not accept this License.  Therefore, by         ',&
'modifying or distributing the Program (or any work based on the             ',&
'Program), you indicate your acceptance of this License to do so, and        ',&
'all its terms and conditions for copying, distributing or modifying         ',&
'the Program or works based on it.                                           ',&
'                                                                            ',&
'  6. Each time you redistribute the Program (or any work based on the       ',&
'Program), the recipient automatically receives a license from the           ',&
'original licensor to copy, distribute or modify the Program subject to      ',&
'these terms and conditions.  You may not impose any further                 ',&
'restrictions on the recipients'' exercise of the rights granted herein.     ',&
'You are not responsible for enforcing compliance by third parties to        ',&
'this License.                                                               ',&
'                                                                            ',&
'  7. If, as a consequence of a court judgment or allegation of patent       ',&
'infringement or for any other reason (not limited to patent issues),        ',&
'conditions are imposed on you (whether by court order, agreement or         ',&
'otherwise) that contradict the conditions of this License, they do not      ',&
'excuse you from the conditions of this License.  If you cannot              ',&
'distribute so as to satisfy simultaneously your obligations under this      ',&
'License and any other pertinent obligations, then as a consequence you      ',&
'may not distribute the Program at all.  For example, if a patent            ',&
'license would not permit royalty-free redistribution of the Program by      ',&
'all those who receive copies directly or indirectly through you, then       ',&
'the only way you could satisfy both it and this License would be to         ',&
'refrain entirely from distribution of the Program.                          ',&
'                                                                            ',&
'If any portion of this section is held invalid or unenforceable under       ',&
'any particular circumstance, the balance of the section is intended to      ',&
'apply and the section as a whole is intended to apply in other              ',&
'circumstances.                                                              ',&
'                                                                            ',&
'It is not the purpose of this section to induce you to infringe any         ',&
'patents or other property right claims or to contest validity of any        ',&
'such claims; this section has the sole purpose of protecting the            ',&
'integrity of the free software distribution system, which is                ',&
'implemented by public license practices.  Many people have made             ',&
'generous contributions to the wide range of software distributed            ',&
'through that system in reliance on consistent application of that           ',&
'system; it is up to the author/donor to decide if he or she is willing      ',&
'to distribute software through any other system and a licensee cannot       ',&
'impose that choice.                                                         ',&
'                                                                            ',&
'This section is intended to make thoroughly clear what is believed to       ',&
'be a consequence of the rest of this License.                               ',&
'                                                                            ',&
'  8. If the distribution and/or use of the Program is restricted in         ',&
'certain countries either by patents or by copyrighted interfaces, the       ',&
'original copyright holder who places the Program under this License         ',&
'may add an explicit geographical distribution limitation excluding          ',&
'those countries, so that distribution is permitted only in or among         ',&
'countries not thus excluded.  In such case, this License incorporates       ',&
'the limitation as if written in the body of this License.                   ',&
'                                                                            ',&
'  9. The Free Software Foundation may publish revised and/or new versions   ',&
'of the General Public License from time to time.  Such new versions will    ',&
'be similar in spirit to the present version, but may differ in detail to    ',&
'address new problems or concerns.                                           ',&
'                                                                            ',&
'Each version is given a distinguishing version number.  If the Program      ',&
'specifies a version number of this License which applies to it and "any     ',&
'later version", you have the option of following the terms and conditions   ',&
'either of that version or of any later version published by the Free        ',&
'Software Foundation.  If the Program does not specify a version number of   ',&
'this License, you may choose any version ever published by the Free Software',&
'Foundation.                                                                 ',&
'                                                                            ',&
'  10. If you wish to incorporate parts of the Program into other free       ',&
'programs whose distribution conditions are different, write to the author   ',&
'to ask for permission.  For software which is copyrighted by the Free       ',&
'Software Foundation, write to the Free Software Foundation; we sometimes    ',&
'make exceptions for this.  Our decision will be guided by the two goals     ',&
'of preserving the free status of all derivatives of our free software and   ',&
'of promoting the sharing and reuse of software generally.                   ',&
'                                                                            ',&
'                            NO WARRANTY                                     ',&
'                                                                            ',&
'  11. BECAUSE THE PROGRAM IS LICENSED FREE OF CHARGE, THERE IS NO WARRANTY  ',&
'FOR THE PROGRAM, TO THE EXTENT PERMITTED BY APPLICABLE LAW.  EXCEPT WHEN    ',&
'OTHERWISE STATED IN WRITING THE COPYRIGHT HOLDERS AND/OR OTHER PARTIES      ',&
'PROVIDE THE PROGRAM "AS IS" WITHOUT WARRANTY OF ANY KIND, EITHER EXPRESSED  ',&
'OR IMPLIED, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF        ',&
'MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.  THE ENTIRE RISK AS   ',&
'TO THE QUALITY AND PERFORMANCE OF THE PROGRAM IS WITH YOU.  SHOULD THE      ',&
'PROGRAM PROVE DEFECTIVE, YOU ASSUME THE COST OF ALL NECESSARY SERVICING,    ',&
'REPAIR OR CORRECTION.                                                       ',&
'                                                                            ',&
'  12. IN NO EVENT UNLESS REQUIRED BY APPLICABLE LAW OR AGREED TO IN WRITING ',&
'WILL ANY COPYRIGHT HOLDER, OR ANY OTHER PARTY WHO MAY MODIFY AND/OR         ',&
'REDISTRIBUTE THE PROGRAM AS PERMITTED ABOVE, BE LIABLE TO YOU FOR DAMAGES,  ',&
'INCLUDING ANY GENERAL, SPECIAL, INCIDENTAL OR CONSEQUENTIAL DAMAGES ARISING ',&
'OUT OF THE USE OR INABILITY TO USE THE PROGRAM (INCLUDING BUT NOT LIMITED   ',&
'TO LOSS OF DATA OR DATA BEING RENDERED INACCURATE OR LOSSES SUSTAINED BY    ',&
'YOU OR THIRD PARTIES OR A FAILURE OF THE PROGRAM TO OPERATE WITH ANY OTHER  ',&
'PROGRAMS), EVEN IF SUCH HOLDER OR OTHER PARTY HAS BEEN ADVISED OF THE       ',&
'POSSIBILITY OF SUCH DAMAGES.                                                ',&
'                                                                            ',&
'                     END OF TERMS AND CONDITIONS                            ',&
'                                                                            ',&
'            How to Apply These Terms to Your New Programs                   ',&
'                                                                            ',&
'  If you develop a new program, and you want it to be of the greatest       ',&
'possible use to the public, the best way to achieve this is to make it      ',&
'free software which everyone can redistribute and change under these terms. ',&
'                                                                            ',&
'  To do so, attach the following notices to the program.  It is safest      ',&
'to attach them to the start of each source file to most effectively         ',&
'convey the exclusion of warranty; and each file should have at least        ',&
'the "copyright" line and a pointer to where the full notice is found.       ',&
'                                                                            ',&
'    <one line to give the program''s name and a brief idea of what it does.>',&
'    Copyright (C) @YEAR@  @NAME_OF_AUTHOR@                                  ',&
'                                                                            ',&
'    This program is free software; you can redistribute it and/or modify    ',&
'    it under the terms of the GNU General Public License as published by    ',&
'    the Free Software Foundation; either version 2 of the License, or       ',&
'    (at your option) any later version.                                     ',&
'                                                                            ',&
'    This program is distributed in the hope that it will be useful,         ',&
'    but WITHOUT ANY WARRANTY; without even the implied warranty of          ',&
'    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the           ',&
'    GNU General Public License for more details.                            ',&
'                                                                            ',&
'    You should have received a copy of the GNU General Public License along ',&
'    with this program; if not, write to the Free Software Foundation, Inc., ',&
'    51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.             ',&
'                                                                            ',&
'Also add information on how to contact you by electronic and paper mail.    ',&
'                                                                            ',&
'If the program is interactive, make it output a short notice like this      ',&
'when it starts in an interactive mode:                                      ',&
'                                                                            ',&
'    Gnomovision version 69, Copyright (C) year name of author               ',&
'    Gnomovision comes with ABSOLUTELY NO WARRANTY; for details type `show w''.',&
'    This is free software, and you are welcome to redistribute it             ',&
'    under certain conditions; type `show c'' for details.                     ',&
'                                                                              ',&
'The hypothetical commands `show w'' and `show c'' should show the appropriate ',&
'parts of the General Public License.  Of course, the commands you use may     ',&
'be called something other than `show w'' and `show c''; they could even be    ',&
'mouse-clicks or menu items--whatever suits your program.                      ',&
'                                                                              ',&
'You should also get your employer (if you work as a programmer) or your       ',&
'school, if any, to sign a "copyright disclaimer" for the program, if          ',&
'necessary.  Here is a sample; alter the names:                                ',&
'                                                                              ',&
'  Yoyodyne, Inc., hereby disclaims all copyright interest in the program      ',&
'  `Gnomovision'' (which makes passes at compilers) written by James Hacker.   ',&
'                                                                              ',&
'  <signature of Ty Coon>, 1 April 1989                                        ',&
'  Ty Coon, President of Vice                                                  ',&
'                                                                              ',&
'This General Public License does not permit incorporating your program into   ',&
'proprietary programs.  If your program is a subroutine library, you may       ',&
'consider it more useful to permit linking proprietary applications with the   ',&
'library.  If this is what you want to do, use the GNU Lesser General          ',&
'Public License instead of this License.                                       ',&
'                                                                              ',&
'']
if(present(topic_only))then
   if(size(textblock)>0)then
      if(topic_only) textblock=[character(len=len(textblock(1))) :: textblock(1)]
   else
      textblock=['']
   endif
endif
case('27','gpl-3.0')
textblock=[ CHARACTER(LEN=128) :: &
'gpl-3.0',&
'       ',&
'                    GNU GENERAL PUBLIC LICENSE',&
'                       Version 3, 29 June 2007',&
'                                              ',&
' Copyright (C) 2007 Free Software Foundation, Inc. <https://fsf.org/>',&
' Everyone is permitted to copy and distribute verbatim copies        ',&
' of this license document, but changing it is not allowed.           ',&
'                                                                     ',&
'                            Preamble                                 ',&
'                                                                     ',&
'  The GNU General Public License is a free, copyleft license for     ',&
'software and other kinds of works.                                   ',&
'                                                                     ',&
'  The licenses for most software and other practical works are designed',&
'to take away your freedom to share and change the works.  By contrast, ',&
'the GNU General Public License is intended to guarantee your freedom to',&
'share and change all versions of a program--to make sure it remains free',&
'software for all its users.  We, the Free Software Foundation, use the  ',&
'GNU General Public License for most of our software; it applies also to ',&
'any other work released this way by its authors.  You can apply it to   ',&
'your programs, too.                                                     ',&
'                                                                        ',&
'  When we speak of free software, we are referring to freedom, not      ',&
'price.  Our General Public Licenses are designed to make sure that you  ',&
'have the freedom to distribute copies of free software (and charge for  ',&
'them if you wish), that you receive source code or can get it if you    ',&
'want it, that you can change the software or use pieces of it in new    ',&
'free programs, and that you know you can do these things.               ',&
'                                                                        ',&
'  To protect your rights, we need to prevent others from denying you    ',&
'these rights or asking you to surrender the rights.  Therefore, you have',&
'certain responsibilities if you distribute copies of the software, or if',&
'you modify it: responsibilities to respect the freedom of others.       ',&
'                                                                        ',&
'  For example, if you distribute copies of such a program, whether      ',&
'gratis or for a fee, you must pass on to the recipients the same        ',&
'freedoms that you received.  You must make sure that they, too, receive ',&
'or can get the source code.  And you must show them these terms so they ',&
'know their rights.                                                      ',&
'                                                                        ',&
'  Developers that use the GNU GPL protect your rights with two steps:   ',&
'(1) assert copyright on the software, and (2) offer you this License    ',&
'giving you legal permission to copy, distribute and/or modify it.       ',&
'                                                                        ',&
'  For the developers'' and authors'' protection, the GPL clearly explains',&
'that there is no warranty for this free software.  For both users'' and  ',&
'authors'' sake, the GPL requires that modified versions be marked as     ',&
'changed, so that their problems will not be attributed erroneously to    ',&
'authors of previous versions.                                            ',&
'                                                                         ',&
'  Some devices are designed to deny users access to install or run       ',&
'modified versions of the software inside them, although the manufacturer ',&
'can do so.  This is fundamentally incompatible with the aim of           ',&
'protecting users'' freedom to change the software.  The systematic       ',&
'pattern of such abuse occurs in the area of products for individuals to  ',&
'use, which is precisely where it is most unacceptable.  Therefore, we    ',&
'have designed this version of the GPL to prohibit the practice for those ',&
'products.  If such problems arise substantially in other domains, we     ',&
'stand ready to extend this provision to those domains in future versions ',&
'of the GPL, as needed to protect the freedom of users.                   ',&
'                                                                         ',&
'  Finally, every program is threatened constantly by software patents.   ',&
'States should not allow patents to restrict development and use of       ',&
'software on general-purpose computers, but in those that do, we wish to  ',&
'avoid the special danger that patents applied to a free program could    ',&
'make it effectively proprietary.  To prevent this, the GPL assures that  ',&
'patents cannot be used to render the program non-free.                   ',&
'                                                                         ',&
'  The precise terms and conditions for copying, distribution and         ',&
'modification follow.                                                     ',&
'                                                                         ',&
'                       TERMS AND CONDITIONS                              ',&
'                                                                         ',&
'  0. Definitions.                                                        ',&
'                                                                         ',&
'  "This License" refers to version 3 of the GNU General Public License.  ',&
'                                                                         ',&
'  "Copyright" also means copyright-like laws that apply to other kinds of',&
'works, such as semiconductor masks.                                      ',&
'                                                                         ',&
'  "The Program" refers to any copyrightable work licensed under this     ',&
'License.  Each licensee is addressed as "you".  "Licensees" and          ',&
'"recipients" may be individuals or organizations.                        ',&
'                                                                         ',&
'  To "modify" a work means to copy from or adapt all or part of the work ',&
'in a fashion requiring copyright permission, other than the making of an ',&
'exact copy.  The resulting work is called a "modified version" of the    ',&
'earlier work or a work "based on" the earlier work.                      ',&
'                                                                         ',&
'  A "covered work" means either the unmodified Program or a work based   ',&
'on the Program.                                                          ',&
'                                                                         ',&
'  To "propagate" a work means to do anything with it that, without       ',&
'permission, would make you directly or secondarily liable for            ',&
'infringement under applicable copyright law, except executing it on a    ',&
'computer or modifying a private copy.  Propagation includes copying,     ',&
'distribution (with or without modification), making available to the     ',&
'public, and in some countries other activities as well.                  ',&
'                                                                         ',&
'  To "convey" a work means any kind of propagation that enables other    ',&
'parties to make or receive copies.  Mere interaction with a user through ',&
'a computer network, with no transfer of a copy, is not conveying.        ',&
'                                                                         ',&
'  An interactive user interface displays "Appropriate Legal Notices"     ',&
'to the extent that it includes a convenient and prominently visible      ',&
'feature that (1) displays an appropriate copyright notice, and (2)       ',&
'tells the user that there is no warranty for the work (except to the     ',&
'extent that warranties are provided), that licensees may convey the      ',&
'work under this License, and how to view a copy of this License.  If     ',&
'the interface presents a list of user commands or options, such as a     ',&
'menu, a prominent item in the list meets this criterion.                 ',&
'                                                                         ',&
'  1. Source Code.                                                        ',&
'                                                                         ',&
'  The "source code" for a work means the preferred form of the work      ',&
'for making modifications to it.  "Object code" means any non-source      ',&
'form of a work.                                                          ',&
'                                                                         ',&
'  A "Standard Interface" means an interface that either is an official   ',&
'standard defined by a recognized standards body, or, in the case of      ',&
'interfaces specified for a particular programming language, one that     ',&
'is widely used among developers working in that language.                ',&
'                                                                         ',&
'  The "System Libraries" of an executable work include anything, other   ',&
'than the work as a whole, that (a) is included in the normal form of     ',&
'packaging a Major Component, but which is not part of that Major         ',&
'Component, and (b) serves only to enable use of the work with that       ',&
'Major Component, or to implement a Standard Interface for which an       ',&
'implementation is available to the public in source code form.  A        ',&
'"Major Component", in this context, means a major essential component    ',&
'(kernel, window system, and so on) of the specific operating system      ',&
'(if any) on which the executable work runs, or a compiler used to        ',&
'produce the work, or an object code interpreter used to run it.          ',&
'                                                                         ',&
'  The "Corresponding Source" for a work in object code form means all    ',&
'the source code needed to generate, install, and (for an executable      ',&
'work) run the object code and to modify the work, including scripts to   ',&
'control those activities.  However, it does not include the work''s      ',&
'System Libraries, or general-purpose tools or generally available free   ',&
'programs which are used unmodified in performing those activities but    ',&
'which are not part of the work.  For example, Corresponding Source       ',&
'includes interface definition files associated with source files for     ',&
'the work, and the source code for shared libraries and dynamically       ',&
'linked subprograms that the work is specifically designed to require,    ',&
'such as by intimate data communication or control flow between those     ',&
'subprograms and other parts of the work.                                 ',&
'                                                                         ',&
'  The Corresponding Source need not include anything that users          ',&
'can regenerate automatically from other parts of the Corresponding       ',&
'Source.                                                                  ',&
'                                                                         ',&
'  The Corresponding Source for a work in source code form is that        ',&
'same work.                                                               ',&
'                                                                         ',&
'  2. Basic Permissions.                                                  ',&
'                                                                         ',&
'  All rights granted under this License are granted for the term of      ',&
'copyright on the Program, and are irrevocable provided the stated        ',&
'conditions are met.  This License explicitly affirms your unlimited      ',&
'permission to run the unmodified Program.  The output from running a     ',&
'covered work is covered by this License only if the output, given its    ',&
'content, constitutes a covered work.  This License acknowledges your     ',&
'rights of fair use or other equivalent, as provided by copyright law.    ',&
'                                                                         ',&
'  You may make, run and propagate covered works that you do not          ',&
'convey, without conditions so long as your license otherwise remains     ',&
'in force.  You may convey covered works to others for the sole purpose   ',&
'of having them make modifications exclusively for you, or provide you    ',&
'with facilities for running those works, provided that you comply with   ',&
'the terms of this License in conveying all material for which you do     ',&
'not control copyright.  Those thus making or running the covered works   ',&
'for you must do so exclusively on your behalf, under your direction      ',&
'and control, on terms that prohibit them from making any copies of       ',&
'your copyrighted material outside their relationship with you.           ',&
'                                                                         ',&
'  Conveying under any other circumstances is permitted solely under      ',&
'the conditions stated below.  Sublicensing is not allowed; section 10    ',&
'makes it unnecessary.                                                    ',&
'                                                                         ',&
'  3. Protecting Users'' Legal Rights From Anti-Circumvention Law.        ',&
'                                                                         ',&
'  No covered work shall be deemed part of an effective technological     ',&
'measure under any applicable law fulfilling obligations under article    ',&
'11 of the WIPO copyright treaty adopted on 20 December 1996, or          ',&
'similar laws prohibiting or restricting circumvention of such            ',&
'measures.                                                                ',&
'                                                                         ',&
'  When you convey a covered work, you waive any legal power to forbid    ',&
'circumvention of technological measures to the extent such circumvention ',&
'is effected by exercising rights under this License with respect to      ',&
'the covered work, and you disclaim any intention to limit operation or   ',&
'modification of the work as a means of enforcing, against the work''s    ',&
'users, your or third parties'' legal rights to forbid circumvention of   ',&
'technological measures.                                                  ',&
'                                                                         ',&
'  4. Conveying Verbatim Copies.                                          ',&
'                                                                         ',&
'  You may convey verbatim copies of the Program''s source code as you    ',&
'receive it, in any medium, provided that you conspicuously and           ',&
'appropriately publish on each copy an appropriate copyright notice;      ',&
'keep intact all notices stating that this License and any                ',&
'non-permissive terms added in accord with section 7 apply to the code;   ',&
'keep intact all notices of the absence of any warranty; and give all     ',&
'recipients a copy of this License along with the Program.                ',&
'                                                                         ',&
'  You may charge any price or no price for each copy that you convey,    ',&
'and you may offer support or warranty protection for a fee.              ',&
'                                                                         ',&
'  5. Conveying Modified Source Versions.                                 ',&
'                                                                         ',&
'  You may convey a work based on the Program, or the modifications to    ',&
'produce it from the Program, in the form of source code under the        ',&
'terms of section 4, provided that you also meet all of these conditions: ',&
'                                                                         ',&
'    a) The work must carry prominent notices stating that you modified   ',&
'    it, and giving a relevant date.                                      ',&
'                                                                         ',&
'    b) The work must carry prominent notices stating that it is          ',&
'    released under this License and any conditions added under section   ',&
'    7.  This requirement modifies the requirement in section 4 to        ',&
'    "keep intact all notices".                                           ',&
'                                                                         ',&
'    c) You must license the entire work, as a whole, under this          ',&
'    License to anyone who comes into possession of a copy.  This         ',&
'    License will therefore apply, along with any applicable section 7    ',&
'    additional terms, to the whole of the work, and all its parts,       ',&
'    regardless of how they are packaged.  This License gives no          ',&
'    permission to license the work in any other way, but it does not     ',&
'    invalidate such permission if you have separately received it.       ',&
'                                                                         ',&
'    d) If the work has interactive user interfaces, each must display    ',&
'    Appropriate Legal Notices; however, if the Program has interactive   ',&
'    interfaces that do not display Appropriate Legal Notices, your       ',&
'    work need not make them do so.                                       ',&
'                                                                         ',&
'  A compilation of a covered work with other separate and independent    ',&
'works, which are not by their nature extensions of the covered work,     ',&
'and which are not combined with it such as to form a larger program,     ',&
'in or on a volume of a storage or distribution medium, is called an      ',&
'"aggregate" if the compilation and its resulting copyright are not       ',&
'used to limit the access or legal rights of the compilation''s users     ',&
'beyond what the individual works permit.  Inclusion of a covered work    ',&
'in an aggregate does not cause this License to apply to the other        ',&
'parts of the aggregate.                                                  ',&
'                                                                         ',&
'  6. Conveying Non-Source Forms.                                         ',&
'                                                                         ',&
'  You may convey a covered work in object code form under the terms      ',&
'of sections 4 and 5, provided that you also convey the                   ',&
'machine-readable Corresponding Source under the terms of this License,   ',&
'in one of these ways:                                                    ',&
'                                                                         ',&
'    a) Convey the object code in, or embodied in, a physical product     ',&
'    (including a physical distribution medium), accompanied by the       ',&
'    Corresponding Source fixed on a durable physical medium              ',&
'    customarily used for software interchange.                           ',&
'                                                                         ',&
'    b) Convey the object code in, or embodied in, a physical product     ',&
'    (including a physical distribution medium), accompanied by a         ',&
'    written offer, valid for at least three years and valid for as       ',&
'    long as you offer spare parts or customer support for that product   ',&
'    model, to give anyone who possesses the object code either (1) a     ',&
'    copy of the Corresponding Source for all the software in the         ',&
'    product that is covered by this License, on a durable physical       ',&
'    medium customarily used for software interchange, for a price no     ',&
'    more than your reasonable cost of physically performing this         ',&
'    conveying of source, or (2) access to copy the                       ',&
'    Corresponding Source from a network server at no charge.             ',&
'                                                                         ',&
'    c) Convey individual copies of the object code with a copy of the    ',&
'    written offer to provide the Corresponding Source.  This             ',&
'    alternative is allowed only occasionally and noncommercially, and    ',&
'    only if you received the object code with such an offer, in accord   ',&
'    with subsection 6b.                                                  ',&
'                                                                         ',&
'    d) Convey the object code by offering access from a designated       ',&
'    place (gratis or for a charge), and offer equivalent access to the   ',&
'    Corresponding Source in the same way through the same place at no    ',&
'    further charge.  You need not require recipients to copy the         ',&
'    Corresponding Source along with the object code.  If the place to    ',&
'    copy the object code is a network server, the Corresponding Source   ',&
'    may be on a different server (operated by you or a third party)      ',&
'    that supports equivalent copying facilities, provided you maintain   ',&
'    clear directions next to the object code saying where to find the    ',&
'    Corresponding Source.  Regardless of what server hosts the           ',&
'    Corresponding Source, you remain obligated to ensure that it is      ',&
'    available for as long as needed to satisfy these requirements.       ',&
'                                                                         ',&
'    e) Convey the object code using peer-to-peer transmission, provided  ',&
'    you inform other peers where the object code and Corresponding       ',&
'    Source of the work are being offered to the general public at no     ',&
'    charge under subsection 6d.                                          ',&
'                                                                         ',&
'  A separable portion of the object code, whose source code is excluded  ',&
'from the Corresponding Source as a System Library, need not be           ',&
'included in conveying the object code work.                              ',&
'                                                                         ',&
'  A "User Product" is either (1) a "consumer product", which means any   ',&
'tangible personal property which is normally used for personal, family,  ',&
'or household purposes, or (2) anything designed or sold for incorporation',&
'into a dwelling.  In determining whether a product is a consumer product,',&
'doubtful cases shall be resolved in favor of coverage.  For a particular ',&
'product received by a particular user, "normally used" refers to a       ',&
'typical or common use of that class of product, regardless of the status ',&
'of the particular user or of the way in which the particular user        ',&
'actually uses, or expects or is expected to use, the product.  A product ',&
'is a consumer product regardless of whether the product has substantial  ',&
'commercial, industrial or non-consumer uses, unless such uses represent  ',&
'the only significant mode of use of the product.                         ',&
'                                                                         ',&
'  "Installation Information" for a User Product means any methods,       ',&
'procedures, authorization keys, or other information required to install ',&
'and execute modified versions of a covered work in that User Product from',&
'a modified version of its Corresponding Source.  The information must    ',&
'suffice to ensure that the continued functioning of the modified object  ',&
'code is in no case prevented or interfered with solely because           ',&
'modification has been made.                                              ',&
'                                                                         ',&
'  If you convey an object code work under this section in, or with, or   ',&
'specifically for use in, a User Product, and the conveying occurs as     ',&
'part of a transaction in which the right of possession and use of the    ',&
'User Product is transferred to the recipient in perpetuity or for a      ',&
'fixed term (regardless of how the transaction is characterized), the     ',&
'Corresponding Source conveyed under this section must be accompanied     ',&
'by the Installation Information.  But this requirement does not apply    ',&
'if neither you nor any third party retains the ability to install        ',&
'modified object code on the User Product (for example, the work has      ',&
'been installed in ROM).                                                  ',&
'                                                                         ',&
'  The requirement to provide Installation Information does not include a ',&
'requirement to continue to provide support service, warranty, or updates ',&
'for a work that has been modified or installed by the recipient, or for  ',&
'the User Product in which it has been modified or installed.  Access to a',&
'network may be denied when the modification itself materially and        ',&
'adversely affects the operation of the network or violates the rules and ',&
'protocols for communication across the network.                          ',&
'                                                                         ',&
'  Corresponding Source conveyed, and Installation Information provided,  ',&
'in accord with this section must be in a format that is publicly         ',&
'documented (and with an implementation available to the public in        ',&
'source code form), and must require no special password or key for       ',&
'unpacking, reading or copying.                                           ',&
'                                                                         ',&
'  7. Additional Terms.                                                   ',&
'                                                                         ',&
'  "Additional permissions" are terms that supplement the terms of this   ',&
'License by making exceptions from one or more of its conditions.         ',&
'Additional permissions that are applicable to the entire Program shall   ',&
'be treated as though they were included in this License, to the extent   ',&
'that they are valid under applicable law.  If additional permissions     ',&
'apply only to part of the Program, that part may be used separately      ',&
'under those permissions, but the entire Program remains governed by      ',&
'this License without regard to the additional permissions.               ',&
'                                                                         ',&
'  When you convey a copy of a covered work, you may at your option       ',&
'remove any additional permissions from that copy, or from any part of    ',&
'it.  (Additional permissions may be written to require their own         ',&
'removal in certain cases when you modify the work.)  You may place       ',&
'additional permissions on material, added by you to a covered work,      ',&
'for which you have or can give appropriate copyright permission.         ',&
'                                                                         ',&
'  Notwithstanding any other provision of this License, for material you  ',&
'add to a covered work, you may (if authorized by the copyright holders of',&
'that material) supplement the terms of this License with terms:          ',&
'                                                                         ',&
'    a) Disclaiming warranty or limiting liability differently from the   ',&
'    terms of sections 15 and 16 of this License; or                      ',&
'                                                                         ',&
'    b) Requiring preservation of specified reasonable legal notices or   ',&
'    author attributions in that material or in the Appropriate Legal     ',&
'    Notices displayed by works containing it; or                         ',&
'                                                                         ',&
'    c) Prohibiting misrepresentation of the origin of that material, or  ',&
'    requiring that modified versions of such material be marked in       ',&
'    reasonable ways as different from the original version; or           ',&
'                                                                         ',&
'    d) Limiting the use for publicity purposes of names of licensors or  ',&
'    authors of the material; or                                          ',&
'                                                                         ',&
'    e) Declining to grant rights under trademark law for use of some     ',&
'    trade names, trademarks, or service marks; or                        ',&
'                                                                         ',&
'    f) Requiring indemnification of licensors and authors of that        ',&
'    material by anyone who conveys the material (or modified versions of ',&
'    it) with contractual assumptions of liability to the recipient, for  ',&
'    any liability that these contractual assumptions directly impose on  ',&
'    those licensors and authors.                                         ',&
'                                                                         ',&
'  All other non-permissive additional terms are considered "further      ',&
'restrictions" within the meaning of section 10.  If the Program as you   ',&
'received it, or any part of it, contains a notice stating that it is     ',&
'governed by this License along with a term that is a further             ',&
'restriction, you may remove that term.  If a license document contains   ',&
'a further restriction but permits relicensing or conveying under this    ',&
'License, you may add to a covered work material governed by the terms    ',&
'of that license document, provided that the further restriction does     ',&
'not survive such relicensing or conveying.                               ',&
'                                                                         ',&
'  If you add terms to a covered work in accord with this section, you    ',&
'must place, in the relevant source files, a statement of the             ',&
'additional terms that apply to those files, or a notice indicating       ',&
'where to find the applicable terms.                                      ',&
'                                                                         ',&
'  Additional terms, permissive or non-permissive, may be stated in the   ',&
'form of a separately written license, or stated as exceptions;           ',&
'the above requirements apply either way.                                 ',&
'                                                                         ',&
'  8. Termination.                                                        ',&
'                                                                         ',&
'  You may not propagate or modify a covered work except as expressly     ',&
'provided under this License.  Any attempt otherwise to propagate or      ',&
'modify it is void, and will automatically terminate your rights under    ',&
'this License (including any patent licenses granted under the third      ',&
'paragraph of section 11).                                                ',&
'                                                                         ',&
'  However, if you cease all violation of this License, then your         ',&
'license from a particular copyright holder is reinstated (a)             ',&
'provisionally, unless and until the copyright holder explicitly and      ',&
'finally terminates your license, and (b) permanently, if the copyright   ',&
'holder fails to notify you of the violation by some reasonable means     ',&
'prior to 60 days after the cessation.                                    ',&
'                                                                         ',&
'  Moreover, your license from a particular copyright holder is           ',&
'reinstated permanently if the copyright holder notifies you of the       ',&
'violation by some reasonable means, this is the first time you have      ',&
'received notice of violation of this License (for any work) from that    ',&
'copyright holder, and you cure the violation prior to 30 days after      ',&
'your receipt of the notice.                                              ',&
'                                                                         ',&
'  Termination of your rights under this section does not terminate the   ',&
'licenses of parties who have received copies or rights from you under    ',&
'this License.  If your rights have been terminated and not permanently   ',&
'reinstated, you do not qualify to receive new licenses for the same      ',&
'material under section 10.                                               ',&
'                                                                         ',&
'  9. Acceptance Not Required for Having Copies.                          ',&
'                                                                         ',&
'  You are not required to accept this License in order to receive or     ',&
'run a copy of the Program.  Ancillary propagation of a covered work      ',&
'occurring solely as a consequence of using peer-to-peer transmission     ',&
'to receive a copy likewise does not require acceptance.  However,        ',&
'nothing other than this License grants you permission to propagate or    ',&
'modify any covered work.  These actions infringe copyright if you do     ',&
'not accept this License.  Therefore, by modifying or propagating a       ',&
'covered work, you indicate your acceptance of this License to do so.     ',&
'                                                                         ',&
'  10. Automatic Licensing of Downstream Recipients.                      ',&
'                                                                         ',&
'  Each time you convey a covered work, the recipient automatically       ',&
'receives a license from the original licensors, to run, modify and       ',&
'propagate that work, subject to this License.  You are not responsible   ',&
'for enforcing compliance by third parties with this License.             ',&
'                                                                         ',&
'  An "entity transaction" is a transaction transferring control of an    ',&
'organization, or substantially all assets of one, or subdividing an      ',&
'organization, or merging organizations.  If propagation of a covered     ',&
'work results from an entity transaction, each party to that              ',&
'transaction who receives a copy of the work also receives whatever       ',&
'licenses to the work the party''s predecessor in interest had or could   ',&
'give under the previous paragraph, plus a right to possession of the     ',&
'Corresponding Source of the work from the predecessor in interest, if    ',&
'the predecessor has it or can get it with reasonable efforts.            ',&
'                                                                         ',&
'  You may not impose any further restrictions on the exercise of the     ',&
'rights granted or affirmed under this License.  For example, you may     ',&
'not impose a license fee, royalty, or other charge for exercise of       ',&
'rights granted under this License, and you may not initiate litigation   ',&
'(including a cross-claim or counterclaim in a lawsuit) alleging that     ',&
'any patent claim is infringed by making, using, selling, offering for    ',&
'sale, or importing the Program or any portion of it.                     ',&
'                                                                         ',&
'  11. Patents.                                                           ',&
'                                                                         ',&
'  A "contributor" is a copyright holder who authorizes use under this    ',&
'License of the Program or a work on which the Program is based.  The     ',&
'work thus licensed is called the contributor''s "contributor version".   ',&
'                                                                         ',&
'  A contributor''s "essential patent claims" are all patent claims       ',&
'owned or controlled by the contributor, whether already acquired or      ',&
'hereafter acquired, that would be infringed by some manner, permitted    ',&
'by this License, of making, using, or selling its contributor version,   ',&
'but do not include claims that would be infringed only as a              ',&
'consequence of further modification of the contributor version.  For     ',&
'purposes of this definition, "control" includes the right to grant       ',&
'patent sublicenses in a manner consistent with the requirements of       ',&
'this License.                                                            ',&
'                                                                         ',&
'  Each contributor grants you a non-exclusive, worldwide, royalty-free   ',&
'patent license under the contributor''s essential patent claims, to      ',&
'make, use, sell, offer for sale, import and otherwise run, modify and    ',&
'propagate the contents of its contributor version.                       ',&
'                                                                         ',&
'  In the following three paragraphs, a "patent license" is any express   ',&
'agreement or commitment, however denominated, not to enforce a patent    ',&
'(such as an express permission to practice a patent or covenant not to   ',&
'sue for patent infringement).  To "grant" such a patent license to a     ',&
'party means to make such an agreement or commitment not to enforce a     ',&
'patent against the party.                                                ',&
'                                                                         ',&
'  If you convey a covered work, knowingly relying on a patent license,   ',&
'and the Corresponding Source of the work is not available for anyone     ',&
'to copy, free of charge and under the terms of this License, through a   ',&
'publicly available network server or other readily accessible means,     ',&
'then you must either (1) cause the Corresponding Source to be so         ',&
'available, or (2) arrange to deprive yourself of the benefit of the      ',&
'patent license for this particular work, or (3) arrange, in a manner     ',&
'consistent with the requirements of this License, to extend the patent   ',&
'license to downstream recipients.  "Knowingly relying" means you have    ',&
'actual knowledge that, but for the patent license, your conveying the    ',&
'covered work in a country, or your recipient''s use of the covered work  ',&
'in a country, would infringe one or more identifiable patents in that    ',&
'country that you have reason to believe are valid.                       ',&
'                                                                         ',&
'  If, pursuant to or in connection with a single transaction or          ',&
'arrangement, you convey, or propagate by procuring conveyance of, a      ',&
'covered work, and grant a patent license to some of the parties          ',&
'receiving the covered work authorizing them to use, propagate, modify    ',&
'or convey a specific copy of the covered work, then the patent license   ',&
'you grant is automatically extended to all recipients of the covered     ',&
'work and works based on it.                                              ',&
'                                                                         ',&
'  A patent license is "discriminatory" if it does not include within     ',&
'the scope of its coverage, prohibits the exercise of, or is              ',&
'conditioned on the non-exercise of one or more of the rights that are    ',&
'specifically granted under this License.  You may not convey a covered   ',&
'work if you are a party to an arrangement with a third party that is     ',&
'in the business of distributing software, under which you make payment   ',&
'to the third party based on the extent of your activity of conveying     ',&
'the work, and under which the third party grants, to any of the          ',&
'parties who would receive the covered work from you, a discriminatory    ',&
'patent license (a) in connection with copies of the covered work         ',&
'conveyed by you (or copies made from those copies), or (b) primarily     ',&
'for and in connection with specific products or compilations that        ',&
'contain the covered work, unless you entered into that arrangement,      ',&
'or that patent license was granted, prior to 28 March 2007.              ',&
'                                                                         ',&
'  Nothing in this License shall be construed as excluding or limiting    ',&
'any implied license or other defenses to infringement that may           ',&
'otherwise be available to you under applicable patent law.               ',&
'                                                                         ',&
'  12. No Surrender of Others'' Freedom.                                  ',&
'                                                                         ',&
'  If conditions are imposed on you (whether by court order, agreement or ',&
'otherwise) that contradict the conditions of this License, they do not   ',&
'excuse you from the conditions of this License.  If you cannot convey a  ',&
'covered work so as to satisfy simultaneously your obligations under this ',&
'License and any other pertinent obligations, then as a consequence you may',&
'not convey it at all.  For example, if you agree to terms that obligate you',&
'to collect a royalty for further conveying from those to whom you convey   ',&
'the Program, the only way you could satisfy both those terms and this      ',&
'License would be to refrain entirely from conveying the Program.           ',&
'                                                                           ',&
'  13. Use with the GNU Affero General Public License.                      ',&
'                                                                           ',&
'  Notwithstanding any other provision of this License, you have            ',&
'permission to link or combine any covered work with a work licensed        ',&
'under version 3 of the GNU Affero General Public License into a single     ',&
'combined work, and to convey the resulting work.  The terms of this        ',&
'License will continue to apply to the part which is the covered work,      ',&
'but the special requirements of the GNU Affero General Public License,     ',&
'section 13, concerning interaction through a network will apply to the     ',&
'combination as such.                                                       ',&
'                                                                           ',&
'  14. Revised Versions of this License.                                    ',&
'                                                                           ',&
'  The Free Software Foundation may publish revised and/or new versions of  ',&
'the GNU General Public License from time to time.  Such new versions will  ',&
'be similar in spirit to the present version, but may differ in detail to   ',&
'address new problems or concerns.                                          ',&
'                                                                           ',&
'  Each version is given a distinguishing version number.  If the           ',&
'Program specifies that a certain numbered version of the GNU General       ',&
'Public License "or any later version" applies to it, you have the          ',&
'option of following the terms and conditions either of that numbered       ',&
'version or of any later version published by the Free Software             ',&
'Foundation.  If the Program does not specify a version number of the       ',&
'GNU General Public License, you may choose any version ever published      ',&
'by the Free Software Foundation.                                           ',&
'                                                                           ',&
'  If the Program specifies that a proxy can decide which future            ',&
'versions of the GNU General Public License can be used, that proxy''s      ',&
'public statement of acceptance of a version permanently authorizes you     ',&
'to choose that version for the Program.                                    ',&
'                                                                           ',&
'  Later license versions may give you additional or different              ',&
'permissions.  However, no additional obligations are imposed on any        ',&
'author or copyright holder as a result of your choosing to follow a        ',&
'later version.                                                             ',&
'                                                                           ',&
'  15. Disclaimer of Warranty.                                              ',&
'                                                                           ',&
'  THERE IS NO WARRANTY FOR THE PROGRAM, TO THE EXTENT PERMITTED BY         ',&
'APPLICABLE LAW.  EXCEPT WHEN OTHERWISE STATED IN WRITING THE COPYRIGHT     ',&
'HOLDERS AND/OR OTHER PARTIES PROVIDE THE PROGRAM "AS IS" WITHOUT WARRANTY  ',&
'OF ANY KIND, EITHER EXPRESSED OR IMPLIED, INCLUDING, BUT NOT LIMITED TO,   ',&
'THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR     ',&
'PURPOSE.  THE ENTIRE RISK AS TO THE QUALITY AND PERFORMANCE OF THE PROGRAM ',&
'IS WITH YOU.  SHOULD THE PROGRAM PROVE DEFECTIVE, YOU ASSUME THE COST OF   ',&
'ALL NECESSARY SERVICING, REPAIR OR CORRECTION.                             ',&
'                                                                           ',&
'  16. Limitation of Liability.                                             ',&
'                                                                           ',&
'  IN NO EVENT UNLESS REQUIRED BY APPLICABLE LAW OR AGREED TO IN WRITING    ',&
'WILL ANY COPYRIGHT HOLDER, OR ANY OTHER PARTY WHO MODIFIES AND/OR CONVEYS  ',&
'THE PROGRAM AS PERMITTED ABOVE, BE LIABLE TO YOU FOR DAMAGES, INCLUDING ANY',&
'GENERAL, SPECIAL, INCIDENTAL OR CONSEQUENTIAL DAMAGES ARISING OUT OF THE   ',&
'USE OR INABILITY TO USE THE PROGRAM (INCLUDING BUT NOT LIMITED TO LOSS OF  ',&
'DATA OR DATA BEING RENDERED INACCURATE OR LOSSES SUSTAINED BY YOU OR THIRD ',&
'PARTIES OR A FAILURE OF THE PROGRAM TO OPERATE WITH ANY OTHER PROGRAMS),   ',&
'EVEN IF SUCH HOLDER OR OTHER PARTY HAS BEEN ADVISED OF THE POSSIBILITY OF  ',&
'SUCH DAMAGES.                                                              ',&
'                                                                           ',&
'  17. Interpretation of Sections 15 and 16.                                ',&
'                                                                           ',&
'  If the disclaimer of warranty and limitation of liability provided       ',&
'above cannot be given local legal effect according to their terms,         ',&
'reviewing courts shall apply local law that most closely approximates      ',&
'an absolute waiver of all civil liability in connection with the           ',&
'Program, unless a warranty or assumption of liability accompanies a        ',&
'copy of the Program in return for a fee.                                   ',&
'                                                                           ',&
'                     END OF TERMS AND CONDITIONS                           ',&
'                                                                           ',&
'            How to Apply These Terms to Your New Programs                  ',&
'                                                                           ',&
'  If you develop a new program, and you want it to be of the greatest      ',&
'possible use to the public, the best way to achieve this is to make it     ',&
'free software which everyone can redistribute and change under these terms.',&
'                                                                           ',&
'  To do so, attach the following notices to the program.  It is safest     ',&
'to attach them to the start of each source file to most effectively        ',&
'state the exclusion of warranty; and each file should have at least        ',&
'the "copyright" line and a pointer to where the full notice is found.      ',&
'                                                                           ',&
'    <one line to give the program''s name and a brief idea of what it does.>',&
'    Copyright (C) @YEAR@  @NAME_OF_AUTHOR@                                  ',&
'                                                                            ',&
'    This program is free software: you can redistribute it and/or modify    ',&
'    it under the terms of the GNU General Public License as published by    ',&
'    the Free Software Foundation, either version 3 of the License, or       ',&
'    (at your option) any later version.                                     ',&
'                                                                            ',&
'    This program is distributed in the hope that it will be useful,         ',&
'    but WITHOUT ANY WARRANTY; without even the implied warranty of          ',&
'    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the           ',&
'    GNU General Public License for more details.                            ',&
'                                                                            ',&
'    You should have received a copy of the GNU General Public License       ',&
'    along with this program.  If not, see <https://www.gnu.org/licenses/>.  ',&
'                                                                            ',&
'Also add information on how to contact you by electronic and paper mail.    ',&
'                                                                            ',&
'  If the program does terminal interaction, make it output a short          ',&
'notice like this when it starts in an interactive mode:                     ',&
'                                                                            ',&
'    <program>  Copyright (C) @YEAR@  @NAME_OF_AUTHOR@                       ',&
'    This program comes with ABSOLUTELY NO WARRANTY; for details type `show w''.',&
'    This is free software, and you are welcome to redistribute it              ',&
'    under certain conditions; type `show c'' for details.                      ',&
'                                                                               ',&
'The hypothetical commands `show w'' and `show c'' should show the appropriate  ',&
'parts of the General Public License.  Of course, your program''s commands      ',&
'might be different; for a GUI interface, you would use an "about box".         ',&
'                                                                               ',&
'  You should also get your employer (if you work as a programmer) or school,   ',&
'if any, to sign a "copyright disclaimer" for the program, if necessary.        ',&
'For more information on this, and how to apply and follow the GNU GPL, see     ',&
'<https://www.gnu.org/licenses/>.                                               ',&
'                                                                               ',&
'  The GNU General Public License does not permit incorporating your program    ',&
'into proprietary programs.  If your program is a subroutine library, you       ',&
'may consider it more useful to permit linking proprietary applications with    ',&
'the library.  If this is what you want to do, use the GNU Lesser General       ',&
'Public License instead of this License.  But first, please read                ',&
'<https://www.gnu.org/licenses/why-not-lgpl.html>.                              ',&
'                                                                               ',&
'']
if(present(topic_only))then
   if(size(textblock)>0)then
      if(topic_only) textblock=[character(len=len(textblock(1))) :: textblock(1)]
   else
      textblock=['']
   endif
endif
case('28','isc')
textblock=[ CHARACTER(LEN=128) :: &
'isc',&
'   ',&
'ISC License',&
'           ',&
'Copyright (c) @YEAR@ @FULLNAME@',&
'                               ',&
'Permission to use, copy, modify, and/or distribute this software for any',&
'purpose with or without fee is hereby granted, provided that the above  ',&
'copyright notice and this permission notice appear in all copies.       ',&
'                                                                        ',&
'THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES WITH',&
'REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY  ',&
'AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT, ',&
'INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM  ',&
'LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR',&
'OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR       ',&
'PERFORMANCE OF THIS SOFTWARE.                                                ',&
'                                                                             ',&
'']
if(present(topic_only))then
   if(size(textblock)>0)then
      if(topic_only) textblock=[character(len=len(textblock(1))) :: textblock(1)]
   else
      textblock=['']
   endif
endif
case('29','lgpl-2.1')
textblock=[ CHARACTER(LEN=128) :: &
'lgpl-2.1',&
'        ',&
'                  GNU LESSER GENERAL PUBLIC LICENSE',&
'                       Version 2.1, February 1999  ',&
'                                                   ',&
' Copyright (C) 1991, 1999 Free Software Foundation, Inc.',&
' 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA',&
' Everyone is permitted to copy and distribute verbatim copies',&
' of this license document, but changing it is not allowed.   ',&
'                                                             ',&
'[This is the first released version of the Lesser GPL.  It also counts',&
' as the successor of the GNU Library Public License, version 2, hence ',&
' the version number 2.1.]                                             ',&
'                                                                      ',&
'                            Preamble                                  ',&
'                                                                      ',&
'  The licenses for most software are designed to take away your       ',&
'freedom to share and change it.  By contrast, the GNU General Public  ',&
'Licenses are intended to guarantee your freedom to share and change   ',&
'free software--to make sure the software is free for all its users.   ',&
'                                                                      ',&
'  This license, the Lesser General Public License, applies to some    ',&
'specially designated software packages--typically libraries--of the   ',&
'Free Software Foundation and other authors who decide to use it.  You ',&
'can use it too, but we suggest you first think carefully about whether',&
'this license or the ordinary General Public License is the better     ',&
'strategy to use in any particular case, based on the explanations below.',&
'                                                                        ',&
'  When we speak of free software, we are referring to freedom of use,   ',&
'not price.  Our General Public Licenses are designed to make sure that  ',&
'you have the freedom to distribute copies of free software (and charge  ',&
'for this service if you wish); that you receive source code or can get  ',&
'it if you want it; that you can change the software and use pieces of   ',&
'it in new free programs; and that you are informed that you can do      ',&
'these things.                                                           ',&
'                                                                        ',&
'  To protect your rights, we need to make restrictions that forbid      ',&
'distributors to deny you these rights or to ask you to surrender these  ',&
'rights.  These restrictions translate to certain responsibilities for   ',&
'you if you distribute copies of the library or if you modify it.        ',&
'                                                                        ',&
'  For example, if you distribute copies of the library, whether gratis  ',&
'or for a fee, you must give the recipients all the rights that we gave  ',&
'you.  You must make sure that they, too, receive or can get the source  ',&
'code.  If you link other code with the library, you must provide        ',&
'complete object files to the recipients, so that they can relink them   ',&
'with the library after making changes to the library and recompiling    ',&
'it.  And you must show them these terms so they know their rights.      ',&
'                                                                        ',&
'  We protect your rights with a two-step method: (1) we copyright the   ',&
'library, and (2) we offer you this license, which gives you legal       ',&
'permission to copy, distribute and/or modify the library.               ',&
'                                                                        ',&
'  To protect each distributor, we want to make it very clear that       ',&
'there is no warranty for the free library.  Also, if the library is     ',&
'modified by someone else and passed on, the recipients should know      ',&
'that what they have is not the original version, so that the original   ',&
'author''s reputation will not be affected by problems that might be     ',&
'introduced by others.                                                   ',&
'                                                                        ',&
'  Finally, software patents pose a constant threat to the existence of  ',&
'any free program.  We wish to make sure that a company cannot           ',&
'effectively restrict the users of a free program by obtaining a         ',&
'restrictive license from a patent holder.  Therefore, we insist that    ',&
'any patent license obtained for a version of the library must be        ',&
'consistent with the full freedom of use specified in this license.      ',&
'                                                                        ',&
'  Most GNU software, including some libraries, is covered by the        ',&
'ordinary GNU General Public License.  This license, the GNU Lesser      ',&
'General Public License, applies to certain designated libraries, and    ',&
'is quite different from the ordinary General Public License.  We use    ',&
'this license for certain libraries in order to permit linking those     ',&
'libraries into non-free programs.                                       ',&
'                                                                        ',&
'  When a program is linked with a library, whether statically or using  ',&
'a shared library, the combination of the two is legally speaking a      ',&
'combined work, a derivative of the original library.  The ordinary      ',&
'General Public License therefore permits such linking only if the       ',&
'entire combination fits its criteria of freedom.  The Lesser General    ',&
'Public License permits more lax criteria for linking other code with    ',&
'the library.                                                            ',&
'                                                                        ',&
'  We call this license the "Lesser" General Public License because it   ',&
'does Less to protect the user''s freedom than the ordinary General      ',&
'Public License.  It also provides other free software developers Less   ',&
'of an advantage over competing non-free programs.  These disadvantages  ',&
'are the reason we use the ordinary General Public License for many      ',&
'libraries.  However, the Lesser license provides advantages in certain  ',&
'special circumstances.                                                  ',&
'                                                                        ',&
'  For example, on rare occasions, there may be a special need to        ',&
'encourage the widest possible use of a certain library, so that it becomes',&
'a de-facto standard.  To achieve this, non-free programs must be          ',&
'allowed to use the library.  A more frequent case is that a free          ',&
'library does the same job as widely used non-free libraries.  In this     ',&
'case, there is little to gain by limiting the free library to free        ',&
'software only, so we use the Lesser General Public License.               ',&
'                                                                          ',&
'  In other cases, permission to use a particular library in non-free      ',&
'programs enables a greater number of people to use a large body of        ',&
'free software.  For example, permission to use the GNU C Library in       ',&
'non-free programs enables many more people to use the whole GNU           ',&
'operating system, as well as its variant, the GNU/Linux operating         ',&
'system.                                                                   ',&
'                                                                          ',&
'  Although the Lesser General Public License is Less protective of the    ',&
'users'' freedom, it does ensure that the user of a program that is        ',&
'linked with the Library has the freedom and the wherewithal to run        ',&
'that program using a modified version of the Library.                     ',&
'                                                                          ',&
'  The precise terms and conditions for copying, distribution and          ',&
'modification follow.  Pay close attention to the difference between a     ',&
'"work based on the library" and a "work that uses the library".  The      ',&
'former contains code derived from the library, whereas the latter must    ',&
'be combined with the library in order to run.                             ',&
'                                                                          ',&
'                  GNU LESSER GENERAL PUBLIC LICENSE                       ',&
'   TERMS AND CONDITIONS FOR COPYING, DISTRIBUTION AND MODIFICATION        ',&
'                                                                          ',&
'  0. This License Agreement applies to any software library or other      ',&
'program which contains a notice placed by the copyright holder or         ',&
'other authorized party saying it may be distributed under the terms of    ',&
'this Lesser General Public License (also called "this License").          ',&
'Each licensee is addressed as "you".                                      ',&
'                                                                          ',&
'  A "library" means a collection of software functions and/or data        ',&
'prepared so as to be conveniently linked with application programs        ',&
'(which use some of those functions and data) to form executables.         ',&
'                                                                          ',&
'  The "Library", below, refers to any such software library or work       ',&
'which has been distributed under these terms.  A "work based on the       ',&
'Library" means either the Library or any derivative work under            ',&
'copyright law: that is to say, a work containing the Library or a         ',&
'portion of it, either verbatim or with modifications and/or translated    ',&
'straightforwardly into another language.  (Hereinafter, translation is    ',&
'included without limitation in the term "modification".)                  ',&
'                                                                          ',&
'  "Source code" for a work means the preferred form of the work for       ',&
'making modifications to it.  For a library, complete source code means    ',&
'all the source code for all modules it contains, plus any associated      ',&
'interface definition files, plus the scripts used to control compilation  ',&
'and installation of the library.                                          ',&
'                                                                          ',&
'  Activities other than copying, distribution and modification are not    ',&
'covered by this License; they are outside its scope.  The act of          ',&
'running a program using the Library is not restricted, and output from    ',&
'such a program is covered only if its contents constitute a work based    ',&
'on the Library (independent of the use of the Library in a tool for       ',&
'writing it).  Whether that is true depends on what the Library does       ',&
'and what the program that uses the Library does.                          ',&
'                                                                          ',&
'  1. You may copy and distribute verbatim copies of the Library''s        ',&
'complete source code as you receive it, in any medium, provided that      ',&
'you conspicuously and appropriately publish on each copy an               ',&
'appropriate copyright notice and disclaimer of warranty; keep intact      ',&
'all the notices that refer to this License and to the absence of any      ',&
'warranty; and distribute a copy of this License along with the            ',&
'Library.                                                                  ',&
'                                                                          ',&
'  You may charge a fee for the physical act of transferring a copy,       ',&
'and you may at your option offer warranty protection in exchange for a    ',&
'fee.                                                                      ',&
'                                                                          ',&
'  2. You may modify your copy or copies of the Library or any portion     ',&
'of it, thus forming a work based on the Library, and copy and             ',&
'distribute such modifications or work under the terms of Section 1        ',&
'above, provided that you also meet all of these conditions:               ',&
'                                                                          ',&
'    a) The modified work must itself be a software library.               ',&
'                                                                          ',&
'    b) You must cause the files modified to carry prominent notices       ',&
'    stating that you changed the files and the date of any change.        ',&
'                                                                          ',&
'    c) You must cause the whole of the work to be licensed at no          ',&
'    charge to all third parties under the terms of this License.          ',&
'                                                                          ',&
'    d) If a facility in the modified Library refers to a function or a    ',&
'    table of data to be supplied by an application program that uses      ',&
'    the facility, other than as an argument passed when the facility      ',&
'    is invoked, then you must make a good faith effort to ensure that,    ',&
'    in the event an application does not supply such function or          ',&
'    table, the facility still operates, and performs whatever part of     ',&
'    its purpose remains meaningful.                                       ',&
'                                                                          ',&
'    (For example, a function in a library to compute square roots has     ',&
'    a purpose that is entirely well-defined independent of the            ',&
'    application.  Therefore, Subsection 2d requires that any              ',&
'    application-supplied function or table used by this function must     ',&
'    be optional: if the application does not supply it, the square        ',&
'    root function must still compute square roots.)                       ',&
'                                                                          ',&
'These requirements apply to the modified work as a whole.  If             ',&
'identifiable sections of that work are not derived from the Library,      ',&
'and can be reasonably considered independent and separate works in        ',&
'themselves, then this License, and its terms, do not apply to those       ',&
'sections when you distribute them as separate works.  But when you        ',&
'distribute the same sections as part of a whole which is a work based     ',&
'on the Library, the distribution of the whole must be on the terms of     ',&
'this License, whose permissions for other licensees extend to the         ',&
'entire whole, and thus to each and every part regardless of who wrote     ',&
'it.                                                                       ',&
'                                                                          ',&
'Thus, it is not the intent of this section to claim rights or contest     ',&
'your rights to work written entirely by you; rather, the intent is to     ',&
'exercise the right to control the distribution of derivative or           ',&
'collective works based on the Library.                                    ',&
'                                                                          ',&
'In addition, mere aggregation of another work not based on the Library    ',&
'with the Library (or with a work based on the Library) on a volume of     ',&
'a storage or distribution medium does not bring the other work under      ',&
'the scope of this License.                                                ',&
'                                                                          ',&
'  3. You may opt to apply the terms of the ordinary GNU General Public    ',&
'License instead of this License to a given copy of the Library.  To do    ',&
'this, you must alter all the notices that refer to this License, so       ',&
'that they refer to the ordinary GNU General Public License, version 2,    ',&
'instead of to this License.  (If a newer version than version 2 of the    ',&
'ordinary GNU General Public License has appeared, then you can specify    ',&
'that version instead if you wish.)  Do not make any other change in       ',&
'these notices.                                                            ',&
'                                                                          ',&
'  Once this change is made in a given copy, it is irreversible for        ',&
'that copy, so the ordinary GNU General Public License applies to all      ',&
'subsequent copies and derivative works made from that copy.               ',&
'                                                                          ',&
'  This option is useful when you wish to copy part of the code of         ',&
'the Library into a program that is not a library.                         ',&
'                                                                          ',&
'  4. You may copy and distribute the Library (or a portion or             ',&
'derivative of it, under Section 2) in object code or executable form      ',&
'under the terms of Sections 1 and 2 above provided that you accompany     ',&
'it with the complete corresponding machine-readable source code, which    ',&
'must be distributed under the terms of Sections 1 and 2 above on a        ',&
'medium customarily used for software interchange.                         ',&
'                                                                          ',&
'  If distribution of object code is made by offering access to copy       ',&
'from a designated place, then offering equivalent access to copy the      ',&
'source code from the same place satisfies the requirement to              ',&
'distribute the source code, even though third parties are not             ',&
'compelled to copy the source along with the object code.                  ',&
'                                                                          ',&
'  5. A program that contains no derivative of any portion of the          ',&
'Library, but is designed to work with the Library by being compiled or    ',&
'linked with it, is called a "work that uses the Library".  Such a         ',&
'work, in isolation, is not a derivative work of the Library, and          ',&
'therefore falls outside the scope of this License.                        ',&
'                                                                          ',&
'  However, linking a "work that uses the Library" with the Library        ',&
'creates an executable that is a derivative of the Library (because it     ',&
'contains portions of the Library), rather than a "work that uses the      ',&
'library".  The executable is therefore covered by this License.           ',&
'Section 6 states terms for distribution of such executables.              ',&
'                                                                          ',&
'  When a "work that uses the Library" uses material from a header file    ',&
'that is part of the Library, the object code for the work may be a        ',&
'derivative work of the Library even though the source code is not.        ',&
'Whether this is true is especially significant if the work can be         ',&
'linked without the Library, or if the work is itself a library.  The      ',&
'threshold for this to be true is not precisely defined by law.            ',&
'                                                                          ',&
'  If such an object file uses only numerical parameters, data             ',&
'structure layouts and accessors, and small macros and small inline        ',&
'functions (ten lines or less in length), then the use of the object       ',&
'file is unrestricted, regardless of whether it is legally a derivative    ',&
'work.  (Executables containing this object code plus portions of the      ',&
'Library will still fall under Section 6.)                                 ',&
'                                                                          ',&
'  Otherwise, if the work is a derivative of the Library, you may          ',&
'distribute the object code for the work under the terms of Section 6.     ',&
'Any executables containing that work also fall under Section 6,           ',&
'whether or not they are linked directly with the Library itself.          ',&
'                                                                          ',&
'  6. As an exception to the Sections above, you may also combine or       ',&
'link a "work that uses the Library" with the Library to produce a         ',&
'work containing portions of the Library, and distribute that work         ',&
'under terms of your choice, provided that the terms permit                ',&
'modification of the work for the customer''s own use and reverse          ',&
'engineering for debugging such modifications.                             ',&
'                                                                          ',&
'  You must give prominent notice with each copy of the work that the      ',&
'Library is used in it and that the Library and its use are covered by     ',&
'this License.  You must supply a copy of this License.  If the work       ',&
'during execution displays copyright notices, you must include the         ',&
'copyright notice for the Library among them, as well as a reference       ',&
'directing the user to the copy of this License.  Also, you must do one    ',&
'of these things:                                                          ',&
'                                                                          ',&
'    a) Accompany the work with the complete corresponding                 ',&
'    machine-readable source code for the Library including whatever       ',&
'    changes were used in the work (which must be distributed under        ',&
'    Sections 1 and 2 above); and, if the work is an executable linked     ',&
'    with the Library, with the complete machine-readable "work that       ',&
'    uses the Library", as object code and/or source code, so that the     ',&
'    user can modify the Library and then relink to produce a modified     ',&
'    executable containing the modified Library.  (It is understood        ',&
'    that the user who changes the contents of definitions files in the    ',&
'    Library will not necessarily be able to recompile the application     ',&
'    to use the modified definitions.)                                     ',&
'                                                                          ',&
'    b) Use a suitable shared library mechanism for linking with the       ',&
'    Library.  A suitable mechanism is one that (1) uses at run time a     ',&
'    copy of the library already present on the user''s computer system,   ',&
'    rather than copying library functions into the executable, and (2)    ',&
'    will operate properly with a modified version of the library, if      ',&
'    the user installs one, as long as the modified version is             ',&
'    interface-compatible with the version that the work was made with.    ',&
'                                                                          ',&
'    c) Accompany the work with a written offer, valid for at              ',&
'    least three years, to give the same user the materials                ',&
'    specified in Subsection 6a, above, for a charge no more               ',&
'    than the cost of performing this distribution.                        ',&
'                                                                          ',&
'    d) If distribution of the work is made by offering access to copy     ',&
'    from a designated place, offer equivalent access to copy the above    ',&
'    specified materials from the same place.                              ',&
'                                                                          ',&
'    e) Verify that the user has already received a copy of these          ',&
'    materials or that you have already sent this user a copy.             ',&
'                                                                          ',&
'  For an executable, the required form of the "work that uses the         ',&
'Library" must include any data and utility programs needed for            ',&
'reproducing the executable from it.  However, as a special exception,     ',&
'the materials to be distributed need not include anything that is         ',&
'normally distributed (in either source or binary form) with the major     ',&
'components (compiler, kernel, and so on) of the operating system on       ',&
'which the executable runs, unless that component itself accompanies       ',&
'the executable.                                                           ',&
'                                                                          ',&
'  It may happen that this requirement contradicts the license             ',&
'restrictions of other proprietary libraries that do not normally          ',&
'accompany the operating system.  Such a contradiction means you cannot    ',&
'use both them and the Library together in an executable that you          ',&
'distribute.                                                               ',&
'                                                                          ',&
'  7. You may place library facilities that are a work based on the        ',&
'Library side-by-side in a single library together with other library      ',&
'facilities not covered by this License, and distribute such a combined    ',&
'library, provided that the separate distribution of the work based on     ',&
'the Library and of the other library facilities is otherwise              ',&
'permitted, and provided that you do these two things:                     ',&
'                                                                          ',&
'    a) Accompany the combined library with a copy of the same work        ',&
'    based on the Library, uncombined with any other library               ',&
'    facilities.  This must be distributed under the terms of the          ',&
'    Sections above.                                                       ',&
'                                                                          ',&
'    b) Give prominent notice with the combined library of the fact        ',&
'    that part of it is a work based on the Library, and explaining        ',&
'    where to find the accompanying uncombined form of the same work.      ',&
'                                                                          ',&
'  8. You may not copy, modify, sublicense, link with, or distribute       ',&
'the Library except as expressly provided under this License.  Any         ',&
'attempt otherwise to copy, modify, sublicense, link with, or              ',&
'distribute the Library is void, and will automatically terminate your     ',&
'rights under this License.  However, parties who have received copies,    ',&
'or rights, from you under this License will not have their licenses       ',&
'terminated so long as such parties remain in full compliance.             ',&
'                                                                          ',&
'  9. You are not required to accept this License, since you have not      ',&
'signed it.  However, nothing else grants you permission to modify or      ',&
'distribute the Library or its derivative works.  These actions are        ',&
'prohibited by law if you do not accept this License.  Therefore, by       ',&
'modifying or distributing the Library (or any work based on the           ',&
'Library), you indicate your acceptance of this License to do so, and      ',&
'all its terms and conditions for copying, distributing or modifying       ',&
'the Library or works based on it.                                         ',&
'                                                                          ',&
'  10. Each time you redistribute the Library (or any work based on the    ',&
'Library), the recipient automatically receives a license from the         ',&
'original licensor to copy, distribute, link with or modify the Library    ',&
'subject to these terms and conditions.  You may not impose any further    ',&
'restrictions on the recipients'' exercise of the rights granted herein.   ',&
'You are not responsible for enforcing compliance by third parties with    ',&
'this License.                                                             ',&
'                                                                          ',&
'  11. If, as a consequence of a court judgment or allegation of patent    ',&
'infringement or for any other reason (not limited to patent issues),      ',&
'conditions are imposed on you (whether by court order, agreement or       ',&
'otherwise) that contradict the conditions of this License, they do not    ',&
'excuse you from the conditions of this License.  If you cannot            ',&
'distribute so as to satisfy simultaneously your obligations under this    ',&
'License and any other pertinent obligations, then as a consequence you    ',&
'may not distribute the Library at all.  For example, if a patent          ',&
'license would not permit royalty-free redistribution of the Library by    ',&
'all those who receive copies directly or indirectly through you, then     ',&
'the only way you could satisfy both it and this License would be to       ',&
'refrain entirely from distribution of the Library.                        ',&
'                                                                          ',&
'If any portion of this section is held invalid or unenforceable under any ',&
'particular circumstance, the balance of the section is intended to apply, ',&
'and the section as a whole is intended to apply in other circumstances.   ',&
'                                                                          ',&
'It is not the purpose of this section to induce you to infringe any       ',&
'patents or other property right claims or to contest validity of any      ',&
'such claims; this section has the sole purpose of protecting the          ',&
'integrity of the free software distribution system which is               ',&
'implemented by public license practices.  Many people have made           ',&
'generous contributions to the wide range of software distributed          ',&
'through that system in reliance on consistent application of that         ',&
'system; it is up to the author/donor to decide if he or she is willing    ',&
'to distribute software through any other system and a licensee cannot     ',&
'impose that choice.                                                       ',&
'                                                                          ',&
'This section is intended to make thoroughly clear what is believed to     ',&
'be a consequence of the rest of this License.                             ',&
'                                                                          ',&
'  12. If the distribution and/or use of the Library is restricted in      ',&
'certain countries either by patents or by copyrighted interfaces, the     ',&
'original copyright holder who places the Library under this License may add',&
'an explicit geographical distribution limitation excluding those countries,',&
'so that distribution is permitted only in or among countries not thus      ',&
'excluded.  In such case, this License incorporates the limitation as if    ',&
'written in the body of this License.                                       ',&
'                                                                           ',&
'  13. The Free Software Foundation may publish revised and/or new          ',&
'versions of the Lesser General Public License from time to time.           ',&
'Such new versions will be similar in spirit to the present version,        ',&
'but may differ in detail to address new problems or concerns.              ',&
'                                                                           ',&
'Each version is given a distinguishing version number.  If the Library     ',&
'specifies a version number of this License which applies to it and         ',&
'"any later version", you have the option of following the terms and        ',&
'conditions either of that version or of any later version published by     ',&
'the Free Software Foundation.  If the Library does not specify a           ',&
'license version number, you may choose any version ever published by       ',&
'the Free Software Foundation.                                              ',&
'                                                                           ',&
'  14. If you wish to incorporate parts of the Library into other free      ',&
'programs whose distribution conditions are incompatible with these,        ',&
'write to the author to ask for permission.  For software which is          ',&
'copyrighted by the Free Software Foundation, write to the Free             ',&
'Software Foundation; we sometimes make exceptions for this.  Our           ',&
'decision will be guided by the two goals of preserving the free status     ',&
'of all derivatives of our free software and of promoting the sharing       ',&
'and reuse of software generally.                                           ',&
'                                                                           ',&
'                            NO WARRANTY                                    ',&
'                                                                           ',&
'  15. BECAUSE THE LIBRARY IS LICENSED FREE OF CHARGE, THERE IS NO          ',&
'WARRANTY FOR THE LIBRARY, TO THE EXTENT PERMITTED BY APPLICABLE LAW.       ',&
'EXCEPT WHEN OTHERWISE STATED IN WRITING THE COPYRIGHT HOLDERS AND/OR       ',&
'OTHER PARTIES PROVIDE THE LIBRARY "AS IS" WITHOUT WARRANTY OF ANY          ',&
'KIND, EITHER EXPRESSED OR IMPLIED, INCLUDING, BUT NOT LIMITED TO, THE      ',&
'IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR         ',&
'PURPOSE.  THE ENTIRE RISK AS TO THE QUALITY AND PERFORMANCE OF THE         ',&
'LIBRARY IS WITH YOU.  SHOULD THE LIBRARY PROVE DEFECTIVE, YOU ASSUME       ',&
'THE COST OF ALL NECESSARY SERVICING, REPAIR OR CORRECTION.                 ',&
'                                                                           ',&
'  16. IN NO EVENT UNLESS REQUIRED BY APPLICABLE LAW OR AGREED TO IN        ',&
'WRITING WILL ANY COPYRIGHT HOLDER, OR ANY OTHER PARTY WHO MAY MODIFY       ',&
'AND/OR REDISTRIBUTE THE LIBRARY AS PERMITTED ABOVE, BE LIABLE TO YOU       ',&
'FOR DAMAGES, INCLUDING ANY GENERAL, SPECIAL, INCIDENTAL OR                 ',&
'CONSEQUENTIAL DAMAGES ARISING OUT OF THE USE OR INABILITY TO USE THE       ',&
'LIBRARY (INCLUDING BUT NOT LIMITED TO LOSS OF DATA OR DATA BEING           ',&
'RENDERED INACCURATE OR LOSSES SUSTAINED BY YOU OR THIRD PARTIES OR A       ',&
'FAILURE OF THE LIBRARY TO OPERATE WITH ANY OTHER SOFTWARE), EVEN IF        ',&
'SUCH HOLDER OR OTHER PARTY HAS BEEN ADVISED OF THE POSSIBILITY OF SUCH     ',&
'DAMAGES.                                                                   ',&
'                                                                           ',&
'                     END OF TERMS AND CONDITIONS                           ',&
'                                                                           ',&
'           How to Apply These Terms to Your New Libraries                  ',&
'                                                                           ',&
'  If you develop a new library, and you want it to be of the greatest      ',&
'possible use to the public, we recommend making it free software that      ',&
'everyone can redistribute and change.  You can do so by permitting         ',&
'redistribution under these terms (or, alternatively, under the terms of the',&
'ordinary General Public License).                                          ',&
'                                                                           ',&
'  To apply these terms, attach the following notices to the library.  It is',&
'safest to attach them to the start of each source file to most effectively ',&
'convey the exclusion of warranty; and each file should have at least the   ',&
'"copyright" line and a pointer to where the full notice is found.          ',&
'                                                                           ',&
'    <one line to give the library''s name and a brief idea of what it does.>',&
'    Copyright (C) @YEAR@  @NAME_OF_AUTHOR@                                  ',&
'                                                                            ',&
'    This library is free software; you can redistribute it and/or           ',&
'    modify it under the terms of the GNU Lesser General Public              ',&
'    License as published by the Free Software Foundation; either            ',&
'    version 2.1 of the License, or (at your option) any later version.      ',&
'                                                                            ',&
'    This library is distributed in the hope that it will be useful,         ',&
'    but WITHOUT ANY WARRANTY; without even the implied warranty of          ',&
'    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU       ',&
'    Lesser General Public License for more details.                         ',&
'                                                                            ',&
'    You should have received a copy of the GNU Lesser General Public        ',&
'    License along with this library; if not, write to the Free Software     ',&
'    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301',&
'    USA                                                                      ',&
'                                                                             ',&
'Also add information on how to contact you by electronic and paper mail.     ',&
'                                                                             ',&
'You should also get your employer (if you work as a programmer) or your      ',&
'school, if any, to sign a "copyright disclaimer" for the library, if         ',&
'necessary.  Here is a sample; alter the names:                               ',&
'                                                                             ',&
'  Yoyodyne, Inc., hereby disclaims all copyright interest in the             ',&
'  library `Frob'' (a library for tweaking knobs) written by James Random     ',&
'  Hacker.                                                                    ',&
'                                                                             ',&
'  <signature of Ty Coon>, 1 April 1990                                       ',&
'  Ty Coon, President of Vice                                                 ',&
'                                                                             ',&
'That''s all there is to it!                                                  ',&
'                                                                             ',&
'']
if(present(topic_only))then
   if(size(textblock)>0)then
      if(topic_only) textblock=[character(len=len(textblock(1))) :: textblock(1)]
   else
      textblock=['']
   endif
endif
case('30','lgpl-3.0')
textblock=[ CHARACTER(LEN=128) :: &
'lgpl-3.0',&
'        ',&
'                   GNU LESSER GENERAL PUBLIC LICENSE',&
'                       Version 3, 29 June 2007      ',&
'                                                    ',&
' Copyright (C) 2007 Free Software Foundation, Inc. <https://fsf.org/>',&
' Everyone is permitted to copy and distribute verbatim copies        ',&
' of this license document, but changing it is not allowed.           ',&
'                                                                     ',&
'                                                                     ',&
'  This version of the GNU Lesser General Public License incorporates ',&
'the terms and conditions of version 3 of the GNU General Public      ',&
'License, supplemented by the additional permissions listed below.    ',&
'                                                                     ',&
'  0. Additional Definitions.                                         ',&
'                                                                     ',&
'  As used herein, "this License" refers to version 3 of the GNU Lesser',&
'General Public License, and the "GNU GPL" refers to version 3 of the GNU',&
'General Public License.                                                 ',&
'                                                                        ',&
'  "The Library" refers to a covered work governed by this License,      ',&
'other than an Application or a Combined Work as defined below.          ',&
'                                                                        ',&
'  An "Application" is any work that makes use of an interface provided  ',&
'by the Library, but which is not otherwise based on the Library.        ',&
'Defining a subclass of a class defined by the Library is deemed a mode  ',&
'of using an interface provided by the Library.                          ',&
'                                                                        ',&
'  A "Combined Work" is a work produced by combining or linking an       ',&
'Application with the Library.  The particular version of the Library    ',&
'with which the Combined Work was made is also called the "Linked        ',&
'Version".                                                               ',&
'                                                                        ',&
'  The "Minimal Corresponding Source" for a Combined Work means the      ',&
'Corresponding Source for the Combined Work, excluding any source code   ',&
'for portions of the Combined Work that, considered in isolation, are    ',&
'based on the Application, and not on the Linked Version.                ',&
'                                                                        ',&
'  The "Corresponding Application Code" for a Combined Work means the    ',&
'object code and/or source code for the Application, including any data  ',&
'and utility programs needed for reproducing the Combined Work from the  ',&
'Application, but excluding the System Libraries of the Combined Work.   ',&
'                                                                        ',&
'  1. Exception to Section 3 of the GNU GPL.                             ',&
'                                                                        ',&
'  You may convey a covered work under sections 3 and 4 of this License  ',&
'without being bound by section 3 of the GNU GPL.                        ',&
'                                                                        ',&
'  2. Conveying Modified Versions.                                       ',&
'                                                                        ',&
'  If you modify a copy of the Library, and, in your modifications, a    ',&
'facility refers to a function or data to be supplied by an Application  ',&
'that uses the facility (other than as an argument passed when the       ',&
'facility is invoked), then you may convey a copy of the modified        ',&
'version:                                                                ',&
'                                                                        ',&
'   a) under this License, provided that you make a good faith effort to ',&
'   ensure that, in the event an Application does not supply the         ',&
'   function or data, the facility still operates, and performs          ',&
'   whatever part of its purpose remains meaningful, or                  ',&
'                                                                        ',&
'   b) under the GNU GPL, with none of the additional permissions of     ',&
'   this License applicable to that copy.                                ',&
'                                                                        ',&
'  3. Object Code Incorporating Material from Library Header Files.      ',&
'                                                                        ',&
'  The object code form of an Application may incorporate material from  ',&
'a header file that is part of the Library.  You may convey such object  ',&
'code under terms of your choice, provided that, if the incorporated     ',&
'material is not limited to numerical parameters, data structure         ',&
'layouts and accessors, or small macros, inline functions and templates  ',&
'(ten or fewer lines in length), you do both of the following:           ',&
'                                                                        ',&
'   a) Give prominent notice with each copy of the object code that the  ',&
'   Library is used in it and that the Library and its use are           ',&
'   covered by this License.                                             ',&
'                                                                        ',&
'   b) Accompany the object code with a copy of the GNU GPL and this license',&
'   document.                                                               ',&
'                                                                           ',&
'  4. Combined Works.                                                       ',&
'                                                                           ',&
'  You may convey a Combined Work under terms of your choice that,          ',&
'taken together, effectively do not restrict modification of the            ',&
'portions of the Library contained in the Combined Work and reverse         ',&
'engineering for debugging such modifications, if you also do each of       ',&
'the following:                                                             ',&
'                                                                           ',&
'   a) Give prominent notice with each copy of the Combined Work that       ',&
'   the Library is used in it and that the Library and its use are          ',&
'   covered by this License.                                                ',&
'                                                                           ',&
'   b) Accompany the Combined Work with a copy of the GNU GPL and this license',&
'   document.                                                                 ',&
'                                                                             ',&
'   c) For a Combined Work that displays copyright notices during             ',&
'   execution, include the copyright notice for the Library among             ',&
'   these notices, as well as a reference directing the user to the           ',&
'   copies of the GNU GPL and this license document.                          ',&
'                                                                             ',&
'   d) Do one of the following:                                               ',&
'                                                                             ',&
'       0) Convey the Minimal Corresponding Source under the terms of this    ',&
'       License, and the Corresponding Application Code in a form             ',&
'       suitable for, and under terms that permit, the user to                ',&
'       recombine or relink the Application with a modified version of        ',&
'       the Linked Version to produce a modified Combined Work, in the        ',&
'       manner specified by section 6 of the GNU GPL for conveying            ',&
'       Corresponding Source.                                                 ',&
'                                                                             ',&
'       1) Use a suitable shared library mechanism for linking with the       ',&
'       Library.  A suitable mechanism is one that (a) uses at run time       ',&
'       a copy of the Library already present on the user''s computer         ',&
'       system, and (b) will operate properly with a modified version         ',&
'       of the Library that is interface-compatible with the Linked           ',&
'       Version.                                                              ',&
'                                                                             ',&
'   e) Provide Installation Information, but only if you would otherwise      ',&
'   be required to provide such information under section 6 of the            ',&
'   GNU GPL, and only to the extent that such information is                  ',&
'   necessary to install and execute a modified version of the                ',&
'   Combined Work produced by recombining or relinking the                    ',&
'   Application with a modified version of the Linked Version. (If            ',&
'   you use option 4d0, the Installation Information must accompany           ',&
'   the Minimal Corresponding Source and Corresponding Application            ',&
'   Code. If you use option 4d1, you must provide the Installation            ',&
'   Information in the manner specified by section 6 of the GNU GPL           ',&
'   for conveying Corresponding Source.)                                      ',&
'                                                                             ',&
'  5. Combined Libraries.                                                     ',&
'                                                                             ',&
'  You may place library facilities that are a work based on the              ',&
'Library side by side in a single library together with other library         ',&
'facilities that are not Applications and are not covered by this             ',&
'License, and convey such a combined library under terms of your              ',&
'choice, if you do both of the following:                                     ',&
'                                                                             ',&
'   a) Accompany the combined library with a copy of the same work based      ',&
'   on the Library, uncombined with any other library facilities,             ',&
'   conveyed under the terms of this License.                                 ',&
'                                                                             ',&
'   b) Give prominent notice with the combined library that part of it        ',&
'   is a work based on the Library, and explaining where to find the          ',&
'   accompanying uncombined form of the same work.                            ',&
'                                                                             ',&
'  6. Revised Versions of the GNU Lesser General Public License.              ',&
'                                                                             ',&
'  The Free Software Foundation may publish revised and/or new versions       ',&
'of the GNU Lesser General Public License from time to time. Such new         ',&
'versions will be similar in spirit to the present version, but may           ',&
'differ in detail to address new problems or concerns.                        ',&
'                                                                             ',&
'  Each version is given a distinguishing version number. If the              ',&
'Library as you received it specifies that a certain numbered version         ',&
'of the GNU Lesser General Public License "or any later version"              ',&
'applies to it, you have the option of following the terms and                ',&
'conditions either of that published version or of any later version          ',&
'published by the Free Software Foundation. If the Library as you             ',&
'received it does not specify a version number of the GNU Lesser              ',&
'General Public License, you may choose any version of the GNU Lesser         ',&
'General Public License ever published by the Free Software Foundation.       ',&
'                                                                             ',&
'  If the Library as you received it specifies that a proxy can decide        ',&
'whether future versions of the GNU Lesser General Public License shall       ',&
'apply, that proxy''s public statement of acceptance of any version is        ',&
'permanent authorization for you to choose that version for the               ',&
'Library.                                                                     ',&
'                                                                             ',&
'']
if(present(topic_only))then
   if(size(textblock)>0)then
      if(topic_only) textblock=[character(len=len(textblock(1))) :: textblock(1)]
   else
      textblock=['']
   endif
endif
case('31','lppl-1.3c')
textblock=[ CHARACTER(LEN=128) :: &
'lppl-1.3c',&
'         ',&
'The LaTeX Project Public License',&
'=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-',&
'                                ',&
'LPPL Version 1.3c  2008-05-04   ',&
'                                ',&
'Copyright 1999 2002-2008 LaTeX3 Project',&
'    Everyone is allowed to distribute verbatim copies of this',&
'    license document, but modification of it is not allowed. ',&
'                                                             ',&
'                                                             ',&
'PREAMBLE                                                     ',&
'========                                                     ',&
'                                                             ',&
'The LaTeX Project Public License (LPPL) is the primary license under',&
'which the LaTeX kernel and the base LaTeX packages are distributed. ',&
'                                                                    ',&
'You may use this license for any work of which you hold the copyright',&
'and which you wish to distribute.  This license may be particularly  ',&
'suitable if your work is TeX-related (such as a LaTeX package), but  ',&
'it is written in such a way that you can use it even if your work is ',&
'unrelated to TeX.                                                    ',&
'                                                                     ',&
'The section `WHETHER AND HOW TO DISTRIBUTE WORKS UNDER THIS LICENSE'',',&
'below, gives instructions, examples, and recommendations for authors  ',&
'who are considering distributing their works under this license.      ',&
'                                                                      ',&
'This license gives conditions under which a work may be distributed   ',&
'and modified, as well as conditions under which modified versions of  ',&
'that work may be distributed.                                         ',&
'                                                                      ',&
'We, the LaTeX3 Project, believe that the conditions below give you    ',&
'the freedom to make and distribute modified versions of your work     ',&
'that conform with whatever technical specifications you wish while    ',&
'maintaining the availability, integrity, and reliability of           ',&
'that work.  If you do not see how to achieve your goal while          ',&
'meeting these conditions, then read the document `cfgguide.tex''      ',&
'and `modguide.tex'' in the base LaTeX distribution for suggestions.   ',&
'                                                                      ',&
'                                                                      ',&
'DEFINITIONS                                                           ',&
'===========                                                           ',&
'                                                                      ',&
'In this license document the following terms are used:                ',&
'                                                                      ',&
'   `Work''                                                            ',&
'    Any work being distributed under this License.                    ',&
'                                                                      ',&
'   `Derived Work''                                                    ',&
'    Any work that under any applicable law is derived from the Work.  ',&
'                                                                      ',&
'   `Modification''                                                    ',&
'    Any procedure that produces a Derived Work under any applicable   ',&
'    law -- for example, the production of a file containing an        ',&
'    original file associated with the Work or a significant portion of',&
'    such a file, either verbatim or with modifications and/or         ',&
'    translated into another language.                                 ',&
'                                                                      ',&
'   `Modify''                                                          ',&
'    To apply any procedure that produces a Derived Work under any     ',&
'    applicable law.                                                   ',&
'                                                                      ',&
'   `Distribution''                                                    ',&
'    Making copies of the Work available from one person to another, in',&
'    whole or in part.  Distribution includes (but is not limited to)  ',&
'    making any electronic components of the Work accessible by        ',&
'    file transfer protocols such as FTP or HTTP or by shared file     ',&
'    systems such as Sun''s Network File System (NFS).                 ',&
'                                                                      ',&
'   `Compiled Work''                                                   ',&
'    A version of the Work that has been processed into a form where it',&
'    is directly usable on a computer system.  This processing may     ',&
'    include using installation facilities provided by the Work,       ',&
'    transformations of the Work, copying of components of the Work, or',&
'    other activities.  Note that modification of any installation     ',&
'    facilities provided by the Work constitutes modification of the Work.',&
'                                                                         ',&
'   `Current Maintainer''                                                 ',&
'    A person or persons nominated as such within the Work.  If there is  ',&
'    no such explicit nomination then it is the `Copyright Holder'' under ',&
'    any applicable law.                                                  ',&
'                                                                         ',&
'   `Base Interpreter''                                                   ',&
'    A program or process that is normally needed for running or          ',&
'    interpreting a part or the whole of the Work.                        ',&
'                                                                         ',&
'    A Base Interpreter may depend on external components but these       ',&
'    are not considered part of the Base Interpreter provided that each   ',&
'    external component clearly identifies itself whenever it is used     ',&
'    interactively.  Unless explicitly specified when applying the        ',&
'    license to the Work, the only applicable Base Interpreter is a       ',&
'    `LaTeX-Format'' or in the case of files belonging to the             ',&
'    `LaTeX-format'' a program implementing the `TeX language''.          ',&
'                                                                         ',&
'                                                                         ',&
'                                                                         ',&
'CONDITIONS ON DISTRIBUTION AND MODIFICATION                              ',&
'===========================================                              ',&
'                                                                         ',&
'1.  Activities other than distribution and/or modification of the Work   ',&
'are not covered by this license; they are outside its scope.  In         ',&
'particular, the act of running the Work is not restricted and no         ',&
'requirements are made concerning any offers of support for the Work.     ',&
'                                                                         ',&
'2.  You may distribute a complete, unmodified copy of the Work as you    ',&
'received it.  Distribution of only part of the Work is considered        ',&
'modification of the Work, and no right to distribute such a Derived      ',&
'Work may be assumed under the terms of this clause.                      ',&
'                                                                         ',&
'3.  You may distribute a Compiled Work that has been generated from a    ',&
'complete, unmodified copy of the Work as distributed under Clause 2      ',&
'above, as long as that Compiled Work is distributed in such a way that   ',&
'the recipients may install the Compiled Work on their system exactly     ',&
'as it would have been installed if they generated a Compiled Work        ',&
'directly from the Work.                                                  ',&
'                                                                         ',&
'4.  If you are the Current Maintainer of the Work, you may, without      ',&
'restriction, modify the Work, thus creating a Derived Work.  You may     ',&
'also distribute the Derived Work without restriction, including          ',&
'Compiled Works generated from the Derived Work.  Derived Works           ',&
'distributed in this manner by the Current Maintainer are considered to   ',&
'be updated versions of the Work.                                         ',&
'                                                                         ',&
'5.  If you are not the Current Maintainer of the Work, you may modify    ',&
'your copy of the Work, thus creating a Derived Work based on the Work,   ',&
'and compile this Derived Work, thus creating a Compiled Work based on    ',&
'the Derived Work.                                                        ',&
'                                                                         ',&
'6.  If you are not the Current Maintainer of the Work, you may           ',&
'distribute a Derived Work provided the following conditions are met      ',&
'for every component of the Work unless that component clearly states     ',&
'in the copyright notice that it is exempt from that condition.  Only     ',&
'the Current Maintainer is allowed to add such statements of exemption    ',&
'to a component of the Work.                                              ',&
'                                                                         ',&
'  a. If a component of this Derived Work can be a direct replacement     ',&
'     for a component of the Work when that component is used with the    ',&
'     Base Interpreter, then, wherever this component of the Work         ',&
'     identifies itself to the user when used interactively with that     ',&
'     Base Interpreter, the replacement component of this Derived Work    ',&
'     clearly and unambiguously identifies itself as a modified version   ',&
'     of this component to the user when used interactively with that     ',&
'     Base Interpreter.                                                   ',&
'                                                                         ',&
'  b. Every component of the Derived Work contains prominent notices      ',&
'     detailing the nature of the changes to that component, or a         ',&
'     prominent reference to another file that is distributed as part     ',&
'     of the Derived Work and that contains a complete and accurate log   ',&
'     of the changes.                                                     ',&
'                                                                         ',&
'  c. No information in the Derived Work implies that any persons,        ',&
'     including (but not limited to) the authors of the original version  ',&
'     of the Work, provide any support, including (but not limited to)    ',&
'     the reporting and handling of errors, to recipients of the          ',&
'     Derived Work unless those persons have stated explicitly that       ',&
'     they do provide such support for the Derived Work.                  ',&
'                                                                         ',&
'  d. You distribute at least one of the following with the Derived Work: ',&
'                                                                         ',&
'       1. A complete, unmodified copy of the Work;                       ',&
'          if your distribution of a modified component is made by        ',&
'          offering access to copy the modified component from a          ',&
'          designated place, then offering equivalent access to copy      ',&
'          the Work from the same or some similar place meets this        ',&
'          condition, even though third parties are not compelled to      ',&
'          copy the Work along with the modified component;               ',&
'                                                                         ',&
'       2. Information that is sufficient to obtain a complete,           ',&
'          unmodified copy of the Work.                                   ',&
'                                                                         ',&
'7.  If you are not the Current Maintainer of the Work, you may           ',&
'distribute a Compiled Work generated from a Derived Work, as long as     ',&
'the Derived Work is distributed to all recipients of the Compiled        ',&
'Work, and as long as the conditions of Clause 6, above, are met with     ',&
'regard to the Derived Work.                                              ',&
'                                                                         ',&
'8.  The conditions above are not intended to prohibit, and hence do not  ',&
'apply to, the modification, by any method, of any component so that it   ',&
'becomes identical to an updated version of that component of the Work as ',&
'it is distributed by the Current Maintainer under Clause 4, above.       ',&
'                                                                         ',&
'9.  Distribution of the Work or any Derived Work in an alternative       ',&
'format, where the Work or that Derived Work (in whole or in part) is     ',&
'then produced by applying some process to that format, does not relax or ',&
'nullify any sections of this license as they pertain to the results of   ',&
'applying that process.                                                   ',&
'                                                                         ',&
'10. a. A Derived Work may be distributed under a different license       ',&
'       provided that license itself honors the conditions listed in      ',&
'       Clause 6 above, in regard to the Work, though it does not have    ',&
'       to honor the rest of the conditions in this license.              ',&
'                                                                         ',&
'    b. If a Derived Work is distributed under a different license, that  ',&
'       Derived Work must provide sufficient documentation as part of     ',&
'       itself to allow each recipient of that Derived Work to honor the  ',&
'       restrictions in Clause 6 above, concerning changes from the Work. ',&
'                                                                         ',&
'11. This license places no restrictions on works that are unrelated to   ',&
'the Work, nor does this license place any restrictions on aggregating    ',&
'such works with the Work by any means.                                   ',&
'                                                                         ',&
'12.  Nothing in this license is intended to, or may be used to, prevent  ',&
'complete compliance by all parties with all applicable laws.             ',&
'                                                                         ',&
'                                                                         ',&
'NO WARRANTY                                                              ',&
'===========                                                              ',&
'                                                                         ',&
'There is no warranty for the Work.  Except when otherwise stated in      ',&
'writing, the Copyright Holder provides the Work `as is'', without        ',&
'warranty of any kind, either expressed or implied, including, but not    ',&
'limited to, the implied warranties of merchantability and fitness for a  ',&
'particular purpose.  The entire risk as to the quality and performance   ',&
'of the Work is with you.  Should the Work prove defective, you assume    ',&
'the cost of all necessary servicing, repair, or correction.              ',&
'                                                                         ',&
'In no event unless required by applicable law or agreed to in writing    ',&
'will The Copyright Holder, or any author named in the components of the  ',&
'Work, or any other party who may distribute and/or modify the Work as    ',&
'permitted above, be liable to you for damages, including any general,    ',&
'special, incidental or consequential damages arising out of any use of   ',&
'the Work or out of inability to use the Work (including, but not limited ',&
'to, loss of data, data being rendered inaccurate, or losses sustained by ',&
'anyone as a result of any failure of the Work to operate with any other  ',&
'programs), even if the Copyright Holder or said author or said other     ',&
'party has been advised of the possibility of such damages.               ',&
'                                                                         ',&
'                                                                         ',&
'MAINTENANCE OF THE WORK                                                  ',&
'=======================                                                  ',&
'                                                                         ',&
'The Work has the status `author-maintained'' if the Copyright Holder     ',&
'explicitly and prominently states near the primary copyright notice in   ',&
'the Work that the Work can only be maintained by the Copyright Holder    ',&
'or simply that it is `author-maintained''.                               ',&
'                                                                         ',&
'The Work has the status `maintained'' if there is a Current Maintainer   ',&
'who has indicated in the Work that they are willing to receive error     ',&
'reports for the Work (for example, by supplying a valid e-mail           ',&
'address). It is not required for the Current Maintainer to acknowledge   ',&
'or act upon these error reports.                                         ',&
'                                                                         ',&
'The Work changes from status `maintained'' to `unmaintained'' if there   ',&
'is no Current Maintainer, or the person stated to be Current             ',&
'Maintainer of the work cannot be reached through the indicated means     ',&
'of communication for a period of six months, and there are no other      ',&
'significant signs of active maintenance.                                 ',&
'                                                                         ',&
'You can become the Current Maintainer of the Work by agreement with      ',&
'any existing Current Maintainer to take over this role.                  ',&
'                                                                         ',&
'If the Work is unmaintained, you can become the Current Maintainer of    ',&
'the Work through the following steps:                                    ',&
'                                                                         ',&
' 1.  Make a reasonable attempt to trace the Current Maintainer (and      ',&
'     the Copyright Holder, if the two differ) through the means of       ',&
'     an Internet or similar search.                                      ',&
'                                                                         ',&
' 2.  If this search is successful, then enquire whether the Work         ',&
'     is still maintained.                                                ',&
'                                                                         ',&
'  a. If it is being maintained, then ask the Current Maintainer          ',&
'     to update their communication data within one month.                ',&
'                                                                         ',&
'  b. If the search is unsuccessful or no action to resume active         ',&
'     maintenance is taken by the Current Maintainer, then announce       ',&
'     within the pertinent community your intention to take over          ',&
'     maintenance.  (If the Work is a LaTeX work, this could be           ',&
'     done, for example, by posting to comp.text.tex.)                    ',&
'                                                                         ',&
' 3a. If the Current Maintainer is reachable and agrees to pass           ',&
'     maintenance of the Work to you, then this takes effect              ',&
'     immediately upon announcement.                                      ',&
'                                                                         ',&
'  b. If the Current Maintainer is not reachable and the Copyright        ',&
'     Holder agrees that maintenance of the Work be passed to you,        ',&
'     then this takes effect immediately upon announcement.               ',&
'                                                                         ',&
' 4.  If you make an `intention announcement'' as described in 2b. above  ',&
'     and after three months your intention is challenged neither by      ',&
'     the Current Maintainer nor by the Copyright Holder nor by other     ',&
'     people, then you may arrange for the Work to be changed so as       ',&
'     to name you as the (new) Current Maintainer.                        ',&
'                                                                         ',&
' 5.  If the previously unreachable Current Maintainer becomes            ',&
'     reachable once more within three months of a change completed       ',&
'     under the terms of 3b) or 4), then that Current Maintainer must     ',&
'     become or remain the Current Maintainer upon request provided       ',&
'     they then update their communication data within one month.         ',&
'                                                                         ',&
'A change in the Current Maintainer does not, of itself, alter the fact   ',&
'that the Work is distributed under the LPPL license.                     ',&
'                                                                         ',&
'If you become the Current Maintainer of the Work, you should             ',&
'immediately provide, within the Work, a prominent and unambiguous        ',&
'statement of your status as Current Maintainer.  You should also         ',&
'announce your new status to the same pertinent community as              ',&
'in 2b) above.                                                            ',&
'                                                                         ',&
'                                                                         ',&
'WHETHER AND HOW TO DISTRIBUTE WORKS UNDER THIS LICENSE                   ',&
'======================================================                   ',&
'                                                                         ',&
'This section contains important instructions, examples, and              ',&
'recommendations for authors who are considering distributing their       ',&
'works under this license.  These authors are addressed as `you'' in      ',&
'this section.                                                            ',&
'                                                                         ',&
'Choosing This License or Another License                                 ',&
'----------------------------------------                                 ',&
'                                                                         ',&
'If for any part of your work you want or need to use *distribution*      ',&
'conditions that differ significantly from those in this license, then    ',&
'do not refer to this license anywhere in your work but, instead,         ',&
'distribute your work under a different license.  You may use the text    ',&
'of this license as a model for your own license, but your license        ',&
'should not refer to the LPPL or otherwise give the impression that       ',&
'your work is distributed under the LPPL.                                 ',&
'                                                                         ',&
'The document `modguide.tex'' in the base LaTeX distribution explains     ',&
'the motivation behind the conditions of this license.  It explains,      ',&
'for example, why distributing LaTeX under the GNU General Public         ',&
'License (GPL) was considered inappropriate.  Even if your work is        ',&
'unrelated to LaTeX, the discussion in `modguide.tex'' may still be       ',&
'relevant, and authors intending to distribute their works under any      ',&
'license are encouraged to read it.                                       ',&
'                                                                         ',&
'A Recommendation on Modification Without Distribution                    ',&
'-----------------------------------------------------                    ',&
'                                                                         ',&
'It is wise never to modify a component of the Work, even for your own    ',&
'personal use, without also meeting the above conditions for              ',&
'distributing the modified component.  While you might intend that such   ',&
'modifications will never be distributed, often this will happen by       ',&
'accident -- you may forget that you have modified that component; or     ',&
'it may not occur to you when allowing others to access the modified      ',&
'version that you are thus distributing it and violating the conditions   ',&
'of this license in ways that could have legal implications and, worse,   ',&
'cause problems for the community.  It is therefore usually in your       ',&
'best interest to keep your copy of the Work identical with the public    ',&
'one.  Many works provide ways to control the behavior of that work       ',&
'without altering any of its licensed components.                         ',&
'                                                                         ',&
'How to Use This License                                                  ',&
'-----------------------                                                  ',&
'                                                                         ',&
'To use this license, place in each of the components of your work both   ',&
'an explicit copyright notice including your name and the year the work   ',&
'was authored and/or last substantially modified.  Include also a         ',&
'statement that the distribution and/or modification of that              ',&
'component is constrained by the conditions in this license.              ',&
'                                                                         ',&
'Here is an example of such a notice and statement:                       ',&
'                                                                         ',&
'  %% pig.dtx                                                             ',&
'  %% Copyright 2005 M. Y. Name                                           ',&
'  %                                                                      ',&
'  % This work may be distributed and/or modified under the               ',&
'  % conditions of the LaTeX Project Public License, either version 1.3   ',&
'  % of this license or (at your option) any later version.               ',&
'  % The latest version of this license is in                             ',&
'  %   http://www.latex-project.org/lppl.txt                              ',&
'  % and version 1.3 or later is part of all distributions of LaTeX       ',&
'  % version 2005/12/01 or later.                                         ',&
'  %                                                                      ',&
'  % This work has the LPPL maintenance status `maintained''.             ',&
'  %                                                                      ',&
'  % The Current Maintainer of this work is M. Y. Name.                   ',&
'  %                                                                      ',&
'  % This work consists of the files pig.dtx and pig.ins                  ',&
'  % and the derived file pig.sty.                                        ',&
'                                                                         ',&
'Given such a notice and statement in a file, the conditions              ',&
'given in this license document would apply, with the `Work'' referring   ',&
'to the three files `pig.dtx'', `pig.ins'', and `pig.sty'' (the last being',&
'generated from `pig.dtx'' using `pig.ins''), the `Base Interpreter''     ',&
'referring to any `LaTeX-Format'', and both `Copyright Holder'' and       ',&
'`Current Maintainer'' referring to the person `M. Y. Name''.             ',&
'                                                                         ',&
'If you do not want the Maintenance section of LPPL to apply to your      ',&
'Work, change `maintained'' above into `author-maintained''.              ',&
'However, we recommend that you use `maintained'', as the Maintenance     ',&
'section was added in order to ensure that your Work remains useful to    ',&
'the community even when you can no longer maintain and support it        ',&
'yourself.                                                                ',&
'                                                                         ',&
'Derived Works That Are Not Replacements                                  ',&
'---------------------------------------                                  ',&
'                                                                         ',&
'Several clauses of the LPPL specify means to provide reliability and     ',&
'stability for the user community. They therefore concern themselves      ',&
'with the case that a Derived Work is intended to be used as a            ',&
'(compatible or incompatible) replacement of the original Work. If        ',&
'this is not the case (e.g., if a few lines of code are reused for a      ',&
'completely different task), then clauses 6b and 6d shall not apply.      ',&
'                                                                         ',&
'                                                                         ',&
'Important Recommendations                                                ',&
'-------------------------                                                ',&
'                                                                         ',&
' Defining What Constitutes the Work                                      ',&
'                                                                         ',&
'   The LPPL requires that distributions of the Work contain all the      ',&
'   files of the Work.  It is therefore important that you provide a      ',&
'   way for the licensee to determine which files constitute the Work.    ',&
'   This could, for example, be achieved by explicitly listing all the    ',&
'   files of the Work near the copyright notice of each file or by        ',&
'   using a line such as:                                                 ',&
'                                                                         ',&
'    % This work consists of all files listed in manifest.txt.            ',&
'                                                                         ',&
'   in that place.  In the absence of an unequivocal list it might be     ',&
'   impossible for the licensee to determine what is considered by you    ',&
'   to comprise the Work and, in such a case, the licensee would be       ',&
'   entitled to make reasonable conjectures as to which files comprise    ',&
'   the Work.                                                             ',&
'                                                                         ',&
'']
if(present(topic_only))then
   if(size(textblock)>0)then
      if(topic_only) textblock=[character(len=len(textblock(1))) :: textblock(1)]
   else
      textblock=['']
   endif
endif
case('32','mit-0')
textblock=[ CHARACTER(LEN=128) :: &
'mit-0',&
'     ',&
'MIT No Attribution',&
'                  ',&
'Copyright @YEAR@ @FULLNAME@',&
'                           ',&
'Permission is hereby granted, free of charge, to any person obtaining a copy',&
'of this software and associated documentation files (the "Software"), to deal',&
'in the Software without restriction, including without limitation the rights ',&
'to use, copy, modify, merge, publish, distribute, sublicense, and/or sell    ',&
'copies of the Software, and to permit persons to whom the Software is        ',&
'furnished to do so.                                                          ',&
'                                                                             ',&
'THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR   ',&
'IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,     ',&
'FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE  ',&
'AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER       ',&
'LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,',&
'OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE',&
'SOFTWARE.                                                                    ',&
'                                                                             ',&
'']
if(present(topic_only))then
   if(size(textblock)>0)then
      if(topic_only) textblock=[character(len=len(textblock(1))) :: textblock(1)]
   else
      textblock=['']
   endif
endif
case('33','mit')
textblock=[ CHARACTER(LEN=128) :: &
'mit',&
'   ',&
'MIT License',&
'           ',&
'Copyright (c) @YEAR@ @FULLNAME@',&
'                               ',&
'Permission is hereby granted, free of charge, to any person obtaining a copy',&
'of this software and associated documentation files (the "Software"), to deal',&
'in the Software without restriction, including without limitation the rights ',&
'to use, copy, modify, merge, publish, distribute, sublicense, and/or sell    ',&
'copies of the Software, and to permit persons to whom the Software is        ',&
'furnished to do so, subject to the following conditions:                     ',&
'                                                                             ',&
'The above copyright notice and this permission notice shall be included in all',&
'copies or substantial portions of the Software.                               ',&
'                                                                              ',&
'THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR    ',&
'IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,      ',&
'FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE   ',&
'AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER        ',&
'LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, ',&
'OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE ',&
'SOFTWARE.                                                                     ',&
'']
if(present(topic_only))then
   if(size(textblock)>0)then
      if(topic_only) textblock=[character(len=len(textblock(1))) :: textblock(1)]
   else
      textblock=['']
   endif
endif
case('34','mpl-2.0')
textblock=[ CHARACTER(LEN=128) :: &
'mpl-2.0',&
'       ',&
'Mozilla Public License Version 2.0',&
'==================================',&
'                                  ',&
'1. Definitions                    ',&
'--------------                    ',&
'                                  ',&
'1.1. "Contributor"                ',&
'    means each individual or legal entity that creates, contributes to',&
'    the creation of, or owns Covered Software.                        ',&
'                                                                      ',&
'1.2. "Contributor Version"                                            ',&
'    means the combination of the Contributions of others (if any) used',&
'    by a Contributor and that particular Contributor''s Contribution. ',&
'                                                                      ',&
'1.3. "Contribution"                                                   ',&
'    means Covered Software of a particular Contributor.               ',&
'                                                                      ',&
'1.4. "Covered Software"                                               ',&
'    means Source Code Form to which the initial Contributor has attached',&
'    the notice in Exhibit A, the Executable Form of such Source Code    ',&
'    Form, and Modifications of such Source Code Form, in each case      ',&
'    including portions thereof.                                         ',&
'                                                                        ',&
'1.5. "Incompatible With Secondary Licenses"                             ',&
'    means                                                               ',&
'                                                                        ',&
'    (a) that the initial Contributor has attached the notice described  ',&
'        in Exhibit B to the Covered Software; or                        ',&
'                                                                        ',&
'    (b) that the Covered Software was made available under the terms of ',&
'        version 1.1 or earlier of the License, but not also under the   ',&
'        terms of a Secondary License.                                   ',&
'                                                                        ',&
'1.6. "Executable Form"                                                  ',&
'    means any form of the work other than Source Code Form.             ',&
'                                                                        ',&
'1.7. "Larger Work"                                                      ',&
'    means a work that combines Covered Software with other material, in ',&
'    a separate file or files, that is not Covered Software.             ',&
'                                                                        ',&
'1.8. "License"                                                          ',&
'    means this document.                                                ',&
'                                                                        ',&
'1.9. "Licensable"                                                       ',&
'    means having the right to grant, to the maximum extent possible,    ',&
'    whether at the time of the initial grant or subsequently, any and   ',&
'    all of the rights conveyed by this License.                         ',&
'                                                                        ',&
'1.10. "Modifications"                                                   ',&
'    means any of the following:                                         ',&
'                                                                        ',&
'    (a) any file in Source Code Form that results from an addition to,  ',&
'        deletion from, or modification of the contents of Covered       ',&
'        Software; or                                                    ',&
'                                                                        ',&
'    (b) any new file in Source Code Form that contains any Covered      ',&
'        Software.                                                       ',&
'                                                                        ',&
'1.11. "Patent Claims" of a Contributor                                  ',&
'    means any patent claim(s), including without limitation, method,    ',&
'    process, and apparatus claims, in any patent Licensable by such     ',&
'    Contributor that would be infringed, but for the grant of the       ',&
'    License, by the making, using, selling, offering for sale, having   ',&
'    made, import, or transfer of either its Contributions or its        ',&
'    Contributor Version.                                                ',&
'                                                                        ',&
'1.12. "Secondary License"                                               ',&
'    means either the GNU General Public License, Version 2.0, the GNU   ',&
'    Lesser General Public License, Version 2.1, the GNU Affero General  ',&
'    Public License, Version 3.0, or any later versions of those         ',&
'    licenses.                                                           ',&
'                                                                        ',&
'1.13. "Source Code Form"                                                ',&
'    means the form of the work preferred for making modifications.      ',&
'                                                                        ',&
'1.14. "You" (or "Your")                                                 ',&
'    means an individual or a legal entity exercising rights under this  ',&
'    License. For legal entities, "You" includes any entity that         ',&
'    controls, is controlled by, or is under common control with You. For',&
'    purposes of this definition, "control" means (a) the power, direct  ',&
'    or indirect, to cause the direction or management of such entity,   ',&
'    whether by contract or otherwise, or (b) ownership of more than     ',&
'    fifty percent (50%) of the outstanding shares or beneficial         ',&
'    ownership of such entity.                                           ',&
'                                                                        ',&
'2. License Grants and Conditions                                        ',&
'--------------------------------                                        ',&
'                                                                        ',&
'2.1. Grants                                                             ',&
'                                                                        ',&
'Each Contributor hereby grants You a world-wide, royalty-free,          ',&
'non-exclusive license:                                                  ',&
'                                                                        ',&
'(a) under intellectual property rights (other than patent or trademark) ',&
'    Licensable by such Contributor to use, reproduce, make available,   ',&
'    modify, display, perform, distribute, and otherwise exploit its     ',&
'    Contributions, either on an unmodified basis, with Modifications, or',&
'    as part of a Larger Work; and                                       ',&
'                                                                        ',&
'(b) under Patent Claims of such Contributor to make, use, sell, offer   ',&
'    for sale, have made, import, and otherwise transfer either its      ',&
'    Contributions or its Contributor Version.                           ',&
'                                                                        ',&
'2.2. Effective Date                                                     ',&
'                                                                        ',&
'The licenses granted in Section 2.1 with respect to any Contribution    ',&
'become effective for each Contribution on the date the Contributor first',&
'distributes such Contribution.                                          ',&
'                                                                        ',&
'2.3. Limitations on Grant Scope                                         ',&
'                                                                        ',&
'The licenses granted in this Section 2 are the only rights granted under',&
'this License. No additional rights or licenses will be implied from the ',&
'distribution or licensing of Covered Software under this License.       ',&
'Notwithstanding Section 2.1(b) above, no patent license is granted by a ',&
'Contributor:                                                            ',&
'                                                                        ',&
'(a) for any code that a Contributor has removed from Covered Software;  ',&
'    or                                                                  ',&
'                                                                        ',&
'(b) for infringements caused by: (i) Your and any other third party''s  ',&
'    modifications of Covered Software, or (ii) the combination of its   ',&
'    Contributions with other software (except as part of its Contributor',&
'    Version); or                                                        ',&
'                                                                        ',&
'(c) under Patent Claims infringed by Covered Software in the absence of ',&
'    its Contributions.                                                  ',&
'                                                                        ',&
'This License does not grant any rights in the trademarks, service marks,',&
'or logos of any Contributor (except as may be necessary to comply with  ',&
'the notice requirements in Section 3.4).                                ',&
'                                                                        ',&
'2.4. Subsequent Licenses                                                ',&
'                                                                        ',&
'No Contributor makes additional grants as a result of Your choice to    ',&
'distribute the Covered Software under a subsequent version of this      ',&
'License (see Section 10.2) or under the terms of a Secondary License (if',&
'permitted under the terms of Section 3.3).                              ',&
'                                                                        ',&
'2.5. Representation                                                     ',&
'                                                                        ',&
'Each Contributor represents that the Contributor believes its           ',&
'Contributions are its original creation(s) or it has sufficient rights  ',&
'to grant the rights to its Contributions conveyed by this License.      ',&
'                                                                        ',&
'2.6. Fair Use                                                           ',&
'                                                                        ',&
'This License is not intended to limit any rights You have under         ',&
'applicable copyright doctrines of fair use, fair dealing, or other      ',&
'equivalents.                                                            ',&
'                                                                        ',&
'2.7. Conditions                                                         ',&
'                                                                        ',&
'Sections 3.1, 3.2, 3.3, and 3.4 are conditions of the licenses granted  ',&
'in Section 2.1.                                                         ',&
'                                                                        ',&
'3. Responsibilities                                                     ',&
'-------------------                                                     ',&
'                                                                        ',&
'3.1. Distribution of Source Form                                        ',&
'                                                                        ',&
'All distribution of Covered Software in Source Code Form, including any ',&
'Modifications that You create or to which You contribute, must be under ',&
'the terms of this License. You must inform recipients that the Source   ',&
'Code Form of the Covered Software is governed by the terms of this      ',&
'License, and how they can obtain a copy of this License. You may not    ',&
'attempt to alter or restrict the recipients'' rights in the Source Code ',&
'Form.                                                                   ',&
'                                                                        ',&
'3.2. Distribution of Executable Form                                    ',&
'                                                                        ',&
'If You distribute Covered Software in Executable Form then:             ',&
'                                                                        ',&
'(a) such Covered Software must also be made available in Source Code    ',&
'    Form, as described in Section 3.1, and You must inform recipients of',&
'    the Executable Form how they can obtain a copy of such Source Code  ',&
'    Form by reasonable means in a timely manner, at a charge no more    ',&
'    than the cost of distribution to the recipient; and                 ',&
'                                                                        ',&
'(b) You may distribute such Executable Form under the terms of this     ',&
'    License, or sublicense it under different terms, provided that the  ',&
'    license for the Executable Form does not attempt to limit or alter  ',&
'    the recipients'' rights in the Source Code Form under this License. ',&
'                                                                        ',&
'3.3. Distribution of a Larger Work                                      ',&
'                                                                        ',&
'You may create and distribute a Larger Work under terms of Your choice, ',&
'provided that You also comply with the requirements of this License for ',&
'the Covered Software. If the Larger Work is a combination of Covered    ',&
'Software with a work governed by one or more Secondary Licenses, and the',&
'Covered Software is not Incompatible With Secondary Licenses, this      ',&
'License permits You to additionally distribute such Covered Software    ',&
'under the terms of such Secondary License(s), so that the recipient of  ',&
'the Larger Work may, at their option, further distribute the Covered    ',&
'Software under the terms of either this License or such Secondary       ',&
'License(s).                                                             ',&
'                                                                        ',&
'3.4. Notices                                                            ',&
'                                                                        ',&
'You may not remove or alter the substance of any license notices        ',&
'(including copyright notices, patent notices, disclaimers of warranty,  ',&
'or limitations of liability) contained within the Source Code Form of   ',&
'the Covered Software, except that You may alter any license notices to  ',&
'the extent required to remedy known factual inaccuracies.               ',&
'                                                                        ',&
'3.5. Application of Additional Terms                                    ',&
'                                                                        ',&
'You may choose to offer, and to charge a fee for, warranty, support,    ',&
'indemnity or liability obligations to one or more recipients of Covered ',&
'Software. However, You may do so only on Your own behalf, and not on    ',&
'behalf of any Contributor. You must make it absolutely clear that any   ',&
'such warranty, support, indemnity, or liability obligation is offered by',&
'You alone, and You hereby agree to indemnify every Contributor for any  ',&
'liability incurred by such Contributor as a result of warranty, support,',&
'indemnity or liability terms You offer. You may include additional      ',&
'disclaimers of warranty and limitations of liability specific to any    ',&
'jurisdiction.                                                           ',&
'                                                                        ',&
'4. Inability to Comply Due to Statute or Regulation                     ',&
'---------------------------------------------------                     ',&
'                                                                        ',&
'If it is impossible for You to comply with any of the terms of this     ',&
'License with respect to some or all of the Covered Software due to      ',&
'statute, judicial order, or regulation then You must: (a) comply with   ',&
'the terms of this License to the maximum extent possible; and (b)       ',&
'describe the limitations and the code they affect. Such description must',&
'be placed in a text file included with all distributions of the Covered ',&
'Software under this License. Except to the extent prohibited by statute ',&
'or regulation, such description must be sufficiently detailed for a     ',&
'recipient of ordinary skill to be able to understand it.                ',&
'                                                                        ',&
'5. Termination                                                          ',&
'--------------                                                          ',&
'                                                                        ',&
'5.1. The rights granted under this License will terminate automatically ',&
'if You fail to comply with any of its terms. However, if You become     ',&
'compliant, then the rights granted under this License from a particular ',&
'Contributor are reinstated (a) provisionally, unless and until such     ',&
'Contributor explicitly and finally terminates Your grants, and (b) on an',&
'ongoing basis, if such Contributor fails to notify You of the           ',&
'non-compliance by some reasonable means prior to 60 days after You have ',&
'come back into compliance. Moreover, Your grants from a particular      ',&
'Contributor are reinstated on an ongoing basis if such Contributor      ',&
'notifies You of the non-compliance by some reasonable means, this is the',&
'first time You have received notice of non-compliance with this License ',&
'from such Contributor, and You become compliant prior to 30 days after  ',&
'Your receipt of the notice.                                             ',&
'                                                                        ',&
'5.2. If You initiate litigation against any entity by asserting a patent',&
'infringement claim (excluding declaratory judgment actions,             ',&
'counter-claims, and cross-claims) alleging that a Contributor Version   ',&
'directly or indirectly infringes any patent, then the rights granted to ',&
'You by any and all Contributors for the Covered Software under Section  ',&
'2.1 of this License shall terminate.                                    ',&
'                                                                        ',&
'5.3. In the event of termination under Sections 5.1 or 5.2 above, all   ',&
'end user license agreements (excluding distributors and resellers) which',&
'have been validly granted by You or Your distributors under this License',&
'prior to termination shall survive termination.                         ',&
'                                                                        ',&
'************************************************************************',&
'*                                                                      *',&
'*  6. Disclaimer of Warranty                                           *',&
'*  -------------------------                                           *',&
'*                                                                      *',&
'*  Covered Software is provided under this License on an "as is"       *',&
'*  basis, without warranty of any kind, either expressed, implied, or  *',&
'*  statutory, including, without limitation, warranties that the       *',&
'*  Covered Software is free of defects, merchantable, fit for a        *',&
'*  particular purpose or non-infringing. The entire risk as to the     *',&
'*  quality and performance of the Covered Software is with You.        *',&
'*  Should any Covered Software prove defective in any respect, You     *',&
'*  (not any Contributor) assume the cost of any necessary servicing,   *',&
'*  repair, or correction. This disclaimer of warranty constitutes an   *',&
'*  essential part of this License. No use of any Covered Software is   *',&
'*  authorized under this License except under this disclaimer.         *',&
'*                                                                      *',&
'************************************************************************',&
'                                                                        ',&
'************************************************************************',&
'*                                                                      *',&
'*  7. Limitation of Liability                                          *',&
'*  --------------------------                                          *',&
'*                                                                      *',&
'*  Under no circumstances and under no legal theory, whether tort      *',&
'*  (including negligence), contract, or otherwise, shall any           *',&
'*  Contributor, or anyone who distributes Covered Software as          *',&
'*  permitted above, be liable to You for any direct, indirect,         *',&
'*  special, incidental, or consequential damages of any character      *',&
'*  including, without limitation, damages for lost profits, loss of    *',&
'*  goodwill, work stoppage, computer failure or malfunction, or any    *',&
'*  and all other commercial damages or losses, even if such party      *',&
'*  shall have been informed of the possibility of such damages. This   *',&
'*  limitation of liability shall not apply to liability for death or   *',&
'*  personal injury resulting from such party''s negligence to the       *',&
'*  extent applicable law prohibits such limitation. Some               * ',&
'*  jurisdictions do not allow the exclusion or limitation of           * ',&
'*  incidental or consequential damages, so this exclusion and          * ',&
'*  limitation may not apply to You.                                    * ',&
'*                                                                      * ',&
'************************************************************************ ',&
'                                                                         ',&
'8. Litigation                                                            ',&
'-------------                                                            ',&
'                                                                         ',&
'Any litigation relating to this License may be brought only in the       ',&
'courts of a jurisdiction where the defendant maintains its principal     ',&
'place of business and such litigation shall be governed by laws of that  ',&
'jurisdiction, without reference to its conflict-of-law provisions.       ',&
'Nothing in this Section shall prevent a party''s ability to bring        ',&
'cross-claims or counter-claims.                                          ',&
'                                                                         ',&
'9. Miscellaneous                                                         ',&
'----------------                                                         ',&
'                                                                         ',&
'This License represents the complete agreement concerning the subject    ',&
'matter hereof. If any provision of this License is held to be            ',&
'unenforceable, such provision shall be reformed only to the extent       ',&
'necessary to make it enforceable. Any law or regulation which provides   ',&
'that the language of a contract shall be construed against the drafter   ',&
'shall not be used to construe this License against a Contributor.        ',&
'                                                                         ',&
'10. Versions of the License                                              ',&
'---------------------------                                              ',&
'                                                                         ',&
'10.1. New Versions                                                       ',&
'                                                                         ',&
'Mozilla Foundation is the license steward. Except as provided in Section ',&
'10.3, no one other than the license steward has the right to modify or   ',&
'publish new versions of this License. Each version will be given a       ',&
'distinguishing version number.                                           ',&
'                                                                         ',&
'10.2. Effect of New Versions                                             ',&
'                                                                         ',&
'You may distribute the Covered Software under the terms of the version   ',&
'of the License under which You originally received the Covered Software, ',&
'or under the terms of any subsequent version published by the license    ',&
'steward.                                                                 ',&
'                                                                         ',&
'10.3. Modified Versions                                                  ',&
'                                                                         ',&
'If you create software not governed by this License, and you want to     ',&
'create a new license for such software, you may create and use a         ',&
'modified version of this License if you rename the license and remove    ',&
'any references to the name of the license steward (except to note that   ',&
'such modified license differs from this License).                        ',&
'                                                                         ',&
'10.4. Distributing Source Code Form that is Incompatible With Secondary  ',&
'Licenses                                                                 ',&
'                                                                         ',&
'If You choose to distribute Source Code Form that is Incompatible With   ',&
'Secondary Licenses under the terms of this version of the License, the   ',&
'notice described in Exhibit B of this License must be attached.          ',&
'                                                                         ',&
'Exhibit A - Source Code Form License Notice                              ',&
'-------------------------------------------                              ',&
'                                                                         ',&
'  This Source Code Form is subject to the terms of the Mozilla Public    ',&
'  License, v. 2.0. If a copy of the MPL was not distributed with this    ',&
'  file, You can obtain one at http://mozilla.org/MPL/2.0/.               ',&
'                                                                         ',&
'If it is not possible or desirable to put the notice in a particular     ',&
'file, then You may include the notice in a location (such as a LICENSE   ',&
'file in a relevant directory) where a recipient would be likely to look  ',&
'for such a notice.                                                       ',&
'                                                                         ',&
'You may add additional accurate notices of copyright ownership.          ',&
'                                                                         ',&
'Exhibit B - "Incompatible With Secondary Licenses" Notice                ',&
'---------------------------------------------------------                ',&
'                                                                         ',&
'  This Source Code Form is "Incompatible With Secondary Licenses", as    ',&
'  defined by the Mozilla Public License, v. 2.0.                         ',&
'                                                                         ',&
'']
if(present(topic_only))then
   if(size(textblock)>0)then
      if(topic_only) textblock=[character(len=len(textblock(1))) :: textblock(1)]
   else
      textblock=['']
   endif
endif
case('35','ms-pl')
textblock=[ CHARACTER(LEN=128) :: &
'ms-pl',&
'     ',&
'Microsoft Public License (Ms-PL)',&
'                                ',&
'This license governs use of the accompanying software. If you use the',&
'software, you accept this license. If you do not accept the license, do not',&
'use the software.                                                          ',&
'                                                                           ',&
'1.  Definitions                                                            ',&
'The terms "reproduce," "reproduction," "derivative works," and "distribution"',&
'have the same meaning here as under U.S. copyright law. A "contribution" is  ',&
'the original software, or any additions or changes to the software. A        ',&
'"contributor" is any person that distributes its contribution under this     ',&
'license. "Licensed patents" are a contributor''s patent claims that read     ',&
'directly on its contribution.                                                ',&
'                                                                             ',&
'2.  Grant of Rights                                                          ',&
'     (A) Copyright Grant- Subject to the terms of this license, including the',&
'     license conditions and limitations in section 3, each contributor grants',&
'     you a non-exclusive, worldwide, royalty-free copyright license to       ',&
'     reproduce its contribution, prepare derivative works of its contribution,',&
'     and distribute its contribution or any derivative works that you create. ',&
'                                                                              ',&
'     (B) Patent Grant- Subject to the terms of this license, including the    ',&
'     license conditions and limitations in section 3, each contributor grants ',&
'     you a non-exclusive, worldwide, royalty-free license under its licensed  ',&
'     patents to make, have made, use, sell, offer for sale, import, and/or    ',&
'     otherwise dispose of its contribution in the software or derivative works',&
'     of the contribution in the software.                                     ',&
'                                                                              ',&
'3.  Conditions and Limitations                                                ',&
'     (A) No Trademark License- This license does not grant you rights to use  ',&
'     any contributors'' name, logo, or trademarks.                            ',&
'                                                                              ',&
'     (B) If you bring a patent claim against any contributor over patents that',&
'     you claim are infringed by the software, your patent license from such   ',&
'     contributor to the software ends automatically.                          ',&
'                                                                              ',&
'     (C) If you distribute any portion of the software, you must retain all   ',&
'     copyright, patent, trademark, and attribution notices that are present in',&
'     the software.                                                            ',&
'                                                                              ',&
'     (D) If you distribute any portion of the software in source code form,   ',&
'     you may do so only under this license by including a complete copy of    ',&
'     this license with your distribution. If you distribute any portion of the',&
'     software in compiled or object code form, you may only do so under a     ',&
'     license that complies with this license.                                 ',&
'                                                                              ',&
'     (E) The software is licensed "as-is." You bear the risk of using it. The ',&
'     contributors give no express warranties, guarantees, or conditions. You  ',&
'     may have additional consumer rights under your local laws which this     ',&
'     license cannot change. To the extent permitted under your local laws, the',&
'     contributors exclude the implied warranties of merchantability, fitness  ',&
'     for a particular purpose and non-infringement.                           ',&
'                                                                              ',&
'']
if(present(topic_only))then
   if(size(textblock)>0)then
      if(topic_only) textblock=[character(len=len(textblock(1))) :: textblock(1)]
   else
      textblock=['']
   endif
endif
case('36','ms-rl')
textblock=[ CHARACTER(LEN=128) :: &
'ms-rl',&
'     ',&
'Microsoft Reciprocal License (Ms-RL)',&
'                                    ',&
'This license governs use of the accompanying software. If you use the',&
'software, you accept this license. If you do not accept the license, do not',&
'use the software.                                                          ',&
'                                                                           ',&
'1.  Definitions                                                            ',&
'The terms "reproduce," "reproduction," "derivative works," and "distribution"',&
'have the same meaning here as under U.S. copyright law.                      ',&
'                                                                             ',&
'A "contribution" is the original software, or any additions or changes to the',&
'software.                                                                    ',&
'                                                                             ',&
'A "contributor" is any person that distributes its contribution under this   ',&
'license.                                                                     ',&
'                                                                             ',&
'"Licensed patents" are a contributor''s patent claims that read directly on its',&
'contribution.                                                                  ',&
'                                                                               ',&
'2.  Grant of Rights                                                            ',&
'     (A) Copyright Grant- Subject to the terms of this license, including the  ',&
'     license conditions and limitations in section 3, each contributor grants  ',&
'     you a non-exclusive, worldwide, royalty-free copyright license to         ',&
'     reproduce its contribution, prepare derivative works of its contribution, ',&
'     and distribute its contribution or any derivative works that you create.  ',&
'                                                                               ',&
'     (B) Patent Grant- Subject to the terms of this license, including the     ',&
'     license conditions and limitations in section 3, each contributor grants  ',&
'     you a non-exclusive, worldwide, royalty-free license under its licensed   ',&
'     patents to make, have made, use, sell, offer for sale, import, and/or     ',&
'     otherwise dispose of its contribution in the software or derivative works ',&
'     of the contribution in the software.                                      ',&
'                                                                               ',&
'3.  Conditions and Limitations                                                 ',&
'     (A) Reciprocal Grants- For any file you distribute that contains code     ',&
'     from the software (in source code or binary format), you must provide     ',&
'     recipients the source code to that file along with a copy of this         ',&
'     license, which license will govern that file. You may license other files ',&
'     that are entirely your own work and do not contain code from the software ',&
'     under any terms you choose.                                               ',&
'                                                                               ',&
'     (B) No Trademark License- This license does not grant you rights to use   ',&
'     any contributors'' name, logo, or trademarks.                             ',&
'                                                                               ',&
'     (C) If you bring a patent claim against any contributor over patents that ',&
'     you claim are infringed by the software, your patent license from such    ',&
'     contributor to the software ends automatically.                           ',&
'                                                                               ',&
'     (D) If you distribute any portion of the software, you must retain all    ',&
'     copyright, patent, trademark, and attribution notices that are present in ',&
'     the software.                                                             ',&
'                                                                               ',&
'     (E) If you distribute any portion of the software in source code form,    ',&
'     you may do so only under this license by including a complete copy of     ',&
'     this license with your distribution. If you distribute any portion of the ',&
'     software in compiled or object code form, you may only do so under a      ',&
'     license that complies with this license.                                  ',&
'                                                                               ',&
'     (F) The software is licensed "as-is." You bear the risk of using it. The  ',&
'     contributors give no express warranties, guarantees, or conditions. You   ',&
'     may have additional consumer rights under your local laws which this      ',&
'     license cannot change. To the extent permitted under your local laws, the ',&
'     contributors exclude the implied warranties of merchantability, fitness   ',&
'     for a particular purpose and non-infringement.                            ',&
'                                                                               ',&
'']
if(present(topic_only))then
   if(size(textblock)>0)then
      if(topic_only) textblock=[character(len=len(textblock(1))) :: textblock(1)]
   else
      textblock=['']
   endif
endif
case('37','mulanpsl-2.0')
textblock=[ CHARACTER(LEN=128) :: &
'mulanpsl-2.0',&
'            ',&
'木兰宽松许可证, 第2版',&
'                              ',&
'木兰宽松许可证， 第2版',&
'                                ',&
'2020年1月 http://license.coscl.org.cn/MulanPSL2',&
'                                                 ',&
'您对“软件”的复制、使用、修改及分发受木兰宽松许可证，第2版（“本许可证”）的如下条款�',&
'                                                                                                                                ',&
'0.   定义                                                                                                                     ',&
'                                                                                                                                ',&
'“软件” 是指由“贡献”构成的许可在“本许可证”下的程序和相关文档的集合。                   ',&
'                                                                                                                                ',&
'“贡献” 是指由任一“贡献者”许可在“本许可证”下的受版权法保护的作品。                      ',&
'                                                                                                                                ',&
'“贡献者” 是指将受版权法保护的作品许可在“本许可证”下的自然人或“法人实体”。          ',&
'                                                                                                                                ',&
'“法人实体” 是指提交贡献的机构及其“关联实体”。                                                       ',&
'                                                                                                                                ',&
'“关联实体” 是指，对“本许可证”下的行为方而言，控制、受控制或与其共同受控制的机构，�',&
'指有受控方或共同受控方至少50%直接或间接的投票权、资金或其他有价证券。                          ',&
'                                                                                                                                ',&
'1.   授予版权许可                                                                                                         ',&
'                                                                                                                                ',&
'每个“贡献者”根据“本许可证”授予您永久性的、全球性的、免费的、非独占的、不可撤销的�',&
'以复制、使用、修改、分发其“贡献”，不论修改与否。                                                     ',&
'                                                                                                                                ',&
'2.   授予专利许可                                                                                                         ',&
'                                                                                                                                ',&
'每个“贡献者”根据“本许可证”授予您永久性的、全球性的、免费的、非独占的、不可撤销的�',&
'撤销除外）专利许可，供您制造、委托制造、使用、许诺销售、销售、进口其“贡献”或以其他�',&
'献”。前述专利许可仅限于“贡献者”现在或将来拥有或控制的其“贡献”本身或其“贡献”与�',&
'件”结合而将必然会侵犯的专利权利要求，不包括对“贡献”的修改或包含“贡献”的其他结合�',&
'关联实体”直接或间接地，就“软件”或其中的“贡献”对任何人发起专利侵权诉讼（包括反诉�',&
'其他专利维权行动，指控其侵犯专利权，则“本许可证”授予您对“软件”的专利许可自您提起�',&
'行动之日终止。                                                                                                           ',&
'                                                                                                                                ',&
'3.   无商标许可                                                                                                            ',&
'                                                                                                                                ',&
'“本许可证”不提供对“贡献者”的商品名称、商标、服务标志或产品名称的商标许可，但您为�',&
'的声明义务而必须使用除外。                                                                                         ',&
'                                                                                                                                ',&
'4.   分发限制                                                                                                               ',&
'                                                                                                                                ',&
'您可以在任何媒介中将“软件”以源程序形式或可执行形式重新分发，不论修改与否，但您必须�',&
'本许可证”的副本，并保留“软件”中的版权、商标、专利及免责声明。                                ',&
'                                                                                                                                ',&
'5.   免责声明与责任限制                                                                                                ',&
'                                                                                                                                ',&
'“软件”及其中的“贡献”在提供时不带任何明示或默示的担保。在任何情况下，“贡献者”或�',&
'任何人因使用“软件”或其中的“贡献”而引发的任何直接或间接损失承担责任，不论因何种原�',&
'何种法律理论，即使其曾被建议有此种损失的可能性。                                                        ',&
'                                                                                                                                ',&
'6.   语言                                                                                                                     ',&
'                                                                                                                                ',&
'“本许可证”以中英文双语表述，中英文版本具有同等法律效力。如果中英文版本存在任何冲突�',&
'版为准。                                                                                                                    ',&
'                                                                                                                                ',&
'条款结束                                                                                                                    ',&
'                                                                                                                                ',&
'如何将木兰宽松许可证，第2版，应用到您的软件                                                                ',&
'                                                                                                                                ',&
'如果您希望将木兰宽松许可证，第2版，应用到您的新软件，为了方便接收者查阅，建议您完成如�',&
'                                                                                                                                ',&
'1， 请您补充如下声明中的空白，包括软件名、软件的首次发表年份以及您作为版权人的名字；   ',&
'                                                                                                                                ',&
'2， 请您在软件包的一级目录下创建以“LICENSE”为名的文件，将整个许可证文本放入该文件中；  ',&
'                                                                                                                                ',&
'3， 请将如下声明文本放入每个源文件的头部注释中。                                                         ',&
'                                                                                                                                ',&
'Copyright (c) @YEAR@ [name of copyright holder]                                                                                 ',&
'[Software Name] is licensed under Mulan PSL v2.                                                                                 ',&
'You can use this software according to the terms and conditions of the Mulan                                                    ',&
'PSL v2.                                                                                                                         ',&
'You may obtain a copy of Mulan PSL v2 at:                                                                                       ',&
'         http://license.coscl.org.cn/MulanPSL2                                                                                  ',&
'THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY                                                        ',&
'KIND, EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO                                                                   ',&
'NON-INFRINGEMENT, MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.                                                              ',&
'See the Mulan PSL v2 for more details.                                                                                          ',&
'                                                                                                                                ',&
'Mulan Permissive Software License，Version 2                                                                                   ',&
'                                                                                                                                ',&
'Mulan Permissive Software License，Version 2 (Mulan PSL v2)                                                                    ',&
'                                                                                                                                ',&
'January 2020 http://license.coscl.org.cn/MulanPSL2                                                                              ',&
'                                                                                                                                ',&
'Your reproduction, use, modification and distribution of the Software shall                                                     ',&
'be subject to Mulan PSL v2 (this License) with the following terms and                                                          ',&
'conditions:                                                                                                                     ',&
'                                                                                                                                ',&
'0. Definition                                                                                                                   ',&
'                                                                                                                                ',&
'Software means the program and related documents which are licensed under                                                       ',&
'this License and comprise all Contribution(s).                                                                                  ',&
'                                                                                                                                ',&
'Contribution means the copyrightable work licensed by a particular                                                              ',&
'Contributor under this License.                                                                                                 ',&
'                                                                                                                                ',&
'Contributor means the Individual or Legal Entity who licenses its                                                               ',&
'copyrightable work under this License.                                                                                          ',&
'                                                                                                                                ',&
'Legal Entity means the entity making a Contribution and all its                                                                 ',&
'Affiliates.                                                                                                                     ',&
'                                                                                                                                ',&
'Affiliates means entities that control, are controlled by, or are under                                                         ',&
'common control with the acting entity under this License, ‘control’ means                                                   ',&
'direct or indirect ownership of at least fifty percent (50%) of the voting                                                      ',&
'power, capital or other securities of controlled or commonly controlled                                                         ',&
'entity.                                                                                                                         ',&
'                                                                                                                                ',&
'1. Grant of Copyright License                                                                                                   ',&
'                                                                                                                                ',&
'Subject to the terms and conditions of this License, each Contributor hereby                                                    ',&
'grants to you a perpetual, worldwide, royalty-free, non-exclusive,                                                              ',&
'irrevocable copyright license to reproduce, use, modify, or distribute its                                                      ',&
'Contribution, with modification or not.                                                                                         ',&
'                                                                                                                                ',&
'2. Grant of Patent License                                                                                                      ',&
'                                                                                                                                ',&
'Subject to the terms and conditions of this License, each Contributor hereby                                                    ',&
'grants to you a perpetual, worldwide, royalty-free, non-exclusive,                                                              ',&
'irrevocable (except for revocation under this Section) patent license to                                                        ',&
'make, have made, use, offer for sale, sell, import or otherwise transfer its                                                    ',&
'Contribution, where such patent license is only limited to the patent claims                                                    ',&
'owned or controlled by such Contributor now or in future which will be                                                          ',&
'necessarily infringed by its Contribution alone, or by combination of the                                                       ',&
'Contribution with the Software to which the Contribution was contributed.                                                       ',&
'The patent license shall not apply to any modification of the Contribution,                                                     ',&
'and any other combination which includes the Contribution. If you or your                                                       ',&
'Affiliates directly or indirectly institute patent litigation (including a                                                      ',&
'cross claim or counterclaim in a litigation) or other patent enforcement                                                        ',&
'activities against any individual or entity by alleging that the Software or                                                    ',&
'any Contribution in it infringes patents, then any patent license granted to                                                    ',&
'you under this License for the Software shall terminate as of the date such                                                     ',&
'litigation or activity is filed or taken.                                                                                       ',&
'                                                                                                                                ',&
'3. No Trademark License                                                                                                         ',&
'                                                                                                                                ',&
'No trademark license is granted to use the trade names, trademarks, service                                                     ',&
'marks, or product names of Contributor, except as required to fulfill notice                                                    ',&
'requirements in section 4.                                                                                                      ',&
'                                                                                                                                ',&
'4. Distribution Restriction                                                                                                     ',&
'                                                                                                                                ',&
'You may distribute the Software in any medium with or without modification,                                                     ',&
'whether in source or executable forms, provided that you provide recipients                                                     ',&
'with a copy of this License and retain copyright, patent, trademark and                                                         ',&
'disclaimer statements in the Software.                                                                                          ',&
'                                                                                                                                ',&
'5. Disclaimer of Warranty and Limitation of Liability                                                                           ',&
'                                                                                                                                ',&
'THE SOFTWARE AND CONTRIBUTION IN IT ARE PROVIDED WITHOUT WARRANTIES OF ANY                                                      ',&
'KIND, EITHER EXPRESS OR IMPLIED. IN NO EVENT SHALL ANY CONTRIBUTOR OR                                                           ',&
'COPYRIGHT HOLDER BE LIABLE TO YOU FOR ANY DAMAGES, INCLUDING, BUT NOT                                                           ',&
'LIMITED TO ANY DIRECT, OR INDIRECT, SPECIAL OR CONSEQUENTIAL DAMAGES ARISING                                                    ',&
'FROM YOUR USE OR INABILITY TO USE THE SOFTWARE OR THE CONTRIBUTION IN IT, NO                                                    ',&
'MATTER HOW IT’S CAUSED OR BASED ON WHICH LEGAL THEORY, EVEN IF ADVISED OF                                                     ',&
'THE POSSIBILITY OF SUCH DAMAGES.                                                                                                ',&
'                                                                                                                                ',&
'6. Language                                                                                                                     ',&
'                                                                                                                                ',&
'THIS LICENSE IS WRITTEN IN BOTH CHINESE AND ENGLISH, AND THE CHINESE VERSION                                                    ',&
'AND ENGLISH VERSION SHALL HAVE THE SAME LEGAL EFFECT. IN THE CASE OF                                                            ',&
'DIVERGENCE BETWEEN THE CHINESE AND ENGLISH VERSIONS, THE CHINESE VERSION                                                        ',&
'SHALL PREVAIL.                                                                                                                  ',&
'                                                                                                                                ',&
'END OF THE TERMS AND CONDITIONS                                                                                                 ',&
'                                                                                                                                ',&
'How to Apply the Mulan Permissive Software License，Version 2                                                                  ',&
'(Mulan PSL v2) to Your Software                                                                                                 ',&
'                                                                                                                                ',&
'To apply the Mulan PSL v2 to your work, for easy identification by                                                              ',&
'recipients, you are suggested to complete following three steps:                                                                ',&
'                                                                                                                                ',&
'i. Fill in the blanks in following statement, including insert your software                                                    ',&
'name, the year of the first publication of your software, and your name                                                         ',&
'identified as the copyright owner;                                                                                              ',&
'ii. Create a file named "LICENSE" which contains the whole context of this                                                      ',&
'License in the first directory of your software package;                                                                        ',&
'iii. Attach the statement to the appropriate annotated syntax at the                                                            ',&
'beginning of each source file.                                                                                                  ',&
'                                                                                                                                ',&
'Copyright (c) @YEAR@ [name of copyright holder]                                                                                 ',&
'[Software Name] is licensed under Mulan PSL v2.                                                                                 ',&
'You can use this software according to the terms and conditions of the Mulan                                                    ',&
'PSL v2.                                                                                                                         ',&
'You may obtain a copy of Mulan PSL v2 at:                                                                                       ',&
'         http://license.coscl.org.cn/MulanPSL2                                                                                  ',&
'THIS SOFTWARE IS PROVIDED ON AN "AS IS" BASIS, WITHOUT WARRANTIES OF ANY                                                        ',&
'KIND, EITHER EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO                                                                   ',&
'NON-INFRINGEMENT, MERCHANTABILITY OR FIT FOR A PARTICULAR PURPOSE.                                                              ',&
'See the Mulan PSL v2 for more details.                                                                                          ',&
'']
if(present(topic_only))then
   if(size(textblock)>0)then
      if(topic_only) textblock=[character(len=len(textblock(1))) :: textblock(1)]
   else
      textblock=['']
   endif
endif
case('38','ncsa')
textblock=[ CHARACTER(LEN=128) :: &
'ncsa',&
'    ',&
'University of Illinois/NCSA Open Source License',&
'                                               ',&
'Copyright (c) @YEAR@ @FULLNAME@. All rights reserved.',&
'                                                     ',&
'Developed by: @PROJECT@                              ',&
'              @FULLNAME@                             ',&
'              [projecturl]                           ',&
'                                                     ',&
'Permission is hereby granted, free of charge, to any person',&
'obtaining a copy of this software and associated documentation files',&
'(the "Software"), to deal with the Software without restriction,    ',&
'including without limitation the rights to use, copy, modify, merge,',&
'publish, distribute, sublicense, and/or sell copies of the Software,',&
'and to permit persons to whom the Software is furnished to do so,   ',&
'subject to the following conditions:                                ',&
'                                                                    ',&
'* Redistributions of source code must retain the above copyright notice,',&
'  this list of conditions and the following disclaimers.                ',&
'                                                                        ',&
'* Redistributions in binary form must reproduce the above copyright     ',&
'  notice, this list of conditions and the following disclaimers in the  ',&
'  documentation and/or other materials provided with the distribution.  ',&
'                                                                        ',&
'* Neither the names of @FULLNAME@, @PROJECT@ nor the names of its       ',&
'  contributors may be used to endorse or promote products derived from  ',&
'  this Software without specific prior written permission.              ',&
'                                                                        ',&
'THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS ',&
'OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,',&
'FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE',&
'CONTRIBUTORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER',&
'LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,',&
'OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS WITH  ',&
'THE SOFTWARE.                                                                ',&
'                                                                             ',&
'']
if(present(topic_only))then
   if(size(textblock)>0)then
      if(topic_only) textblock=[character(len=len(textblock(1))) :: textblock(1)]
   else
      textblock=['']
   endif
endif
case('39','odbl-1.0')
textblock=[ CHARACTER(LEN=128) :: &
'odbl-1.0',&
'        ',&
'## ODC Open Database License (ODbL)',&
'                                   ',&
'### Preamble                       ',&
'                                   ',&
'The Open Database License (ODbL) is a license agreement intended to',&
'allow users to freely share, modify, and use this Database while   ',&
'maintaining this same freedom for others. Many databases are covered by',&
'copyright, and therefore this document licenses these rights. Some     ',&
'jurisdictions, mainly in the European Union, have specific rights that ',&
'cover databases, and so the ODbL addresses these rights, too. Finally, ',&
'the ODbL is also an agreement in contract for users of this Database to',&
'act in certain ways in return for accessing this Database.             ',&
'                                                                       ',&
'Databases can contain a wide variety of types of content (images,      ',&
'audiovisual material, and sounds all in the same database, for example),',&
'and so the ODbL only governs the rights over the Database, and not the  ',&
'contents of the Database individually. Licensors should use the ODbL    ',&
'together with another license for the contents, if the contents have a  ',&
'single set of rights that uniformly covers all of the contents. If the  ',&
'contents have multiple sets of different rights, Licensors should       ',&
'describe what rights govern what contents together in the individual    ',&
'record or in some other way that clarifies what rights apply.           ',&
'                                                                        ',&
'Sometimes the contents of a database, or the database itself, can be    ',&
'covered by other rights not addressed here (such as private contracts,  ',&
'trade mark over the name, or privacy rights / data protection rights    ',&
'over information in the contents), and so you are advised that you may  ',&
'have to consult other documents or clear other rights before doing      ',&
'activities not covered by this License.                                 ',&
'                                                                        ',&
'------                                                                  ',&
'                                                                        ',&
'The Licensor (as defined below)                                         ',&
'                                                                        ',&
'and                                                                     ',&
'                                                                        ',&
'You (as defined below)                                                  ',&
'                                                                        ',&
'agree as follows:                                                       ',&
'                                                                        ',&
'### 1.0 Definitions of Capitalised Words                                ',&
'                                                                        ',&
'"Collective Database" – Means this Database in unmodified form as part',&
'of a collection of independent databases in themselves that together are',&
'assembled into a collective whole. A work that constitutes a Collective ',&
'Database will not be considered a Derivative Database.                  ',&
'                                                                        ',&
'"Convey" – As a verb, means Using the Database, a Derivative Database,',&
'or the Database as part of a Collective Database in any way that enables',&
'a Person to make or receive copies of the Database or a Derivative      ',&
'Database.  Conveying does not include interaction with a user through a ',&
'computer network, or creating and Using a Produced Work, where no       ',&
'transfer of a copy of the Database or a Derivative Database occurs.     ',&
'"Contents" – The contents of this Database, which includes the        ',&
'information, independent works, or other material collected into the    ',&
'Database. For example, the contents of the Database could be factual    ',&
'data or works such as images, audiovisual material, text, or sounds.    ',&
'                                                                        ',&
'"Database" – A collection of material (the Contents) arranged in a    ',&
'systematic or methodical way and individually accessible by electronic  ',&
'or other means offered under the terms of this License.                 ',&
'                                                                        ',&
'"Database Directive" – Means Directive 96/9/EC of the European        ',&
'Parliament and of the Council of 11 March 1996 on the legal protection  ',&
'of databases, as amended or succeeded.                                  ',&
'                                                                        ',&
'"Database Right" – Means rights resulting from the Chapter III ("sui  ',&
'generis") rights in the Database Directive (as amended and as transposed',&
'by member states), which includes the Extraction and Re-utilisation of  ',&
'the whole or a Substantial part of the Contents, as well as any similar ',&
'rights available in the relevant jurisdiction under Section 10.4.       ',&
'                                                                        ',&
'"Derivative Database" – Means a database based upon the Database, and ',&
'includes any translation, adaptation, arrangement, modification, or any ',&
'other alteration of the Database or of a Substantial part of the        ',&
'Contents. This includes, but is not limited to, Extracting or           ',&
'Re-utilising the whole or a Substantial part of the Contents in a new   ',&
'Database.                                                               ',&
'                                                                        ',&
'"Extraction" – Means the permanent or temporary transfer of all or a  ',&
'Substantial part of the Contents to another medium by any means or in   ',&
'any form.                                                               ',&
'                                                                        ',&
'"License" – Means this license agreement and is both a license of rights',&
'such as copyright and Database Rights and an agreement in contract.       ',&
'                                                                          ',&
'"Licensor" – Means the Person that offers the Database under the terms  ',&
'of this License.                                                          ',&
'                                                                          ',&
'"Person" – Means a natural or legal person or a body of persons         ',&
'corporate or incorporate.                                                 ',&
'                                                                          ',&
'"Produced Work" –  a work (such as an image, audiovisual material, text,',&
'or sounds) resulting from using the whole or a Substantial part of the    ',&
'Contents (via a search or other query) from this Database, a Derivative   ',&
'Database, or this Database as part of a Collective Database.              ',&
'                                                                          ',&
'"Publicly" – means to Persons other than You or under Your control by   ',&
'either more than 50% ownership or by the power to direct their            ',&
'activities (such as contracting with an independent consultant).          ',&
'                                                                          ',&
'"Re-utilisation" – means any form of making available to the public all ',&
'or a Substantial part of the Contents by the distribution of copies, by   ',&
'renting, by online or other forms of transmission.                        ',&
'                                                                          ',&
'"Substantial" – Means substantial in terms of quantity or quality or a  ',&
'combination of both. The repeated and systematic Extraction or            ',&
'Re-utilisation of insubstantial parts of the Contents may amount to the   ',&
'Extraction or Re-utilisation of a Substantial part of the Contents.       ',&
'                                                                          ',&
'"Use" – As a verb, means doing any act that is restricted by copyright  ',&
'or Database Rights whether in the original medium or any other; and       ',&
'includes without limitation distributing, copying, publicly performing,   ',&
'publicly displaying, and preparing derivative works of the Database, as   ',&
'well as modifying the Database as may be technically necessary to use it  ',&
'in a different mode or format.                                            ',&
'                                                                          ',&
'"You" – Means a Person exercising rights under this License who has not ',&
'previously violated the terms of this License with respect to the         ',&
'Database, or who has received express permission from the Licensor to     ',&
'exercise rights under this License despite a previous violation.          ',&
'                                                                          ',&
'Words in the singular include the plural and vice versa.                  ',&
'                                                                          ',&
'### 2.0 What this License covers                                          ',&
'                                                                          ',&
'2.1. Legal effect of this document. This License is:                      ',&
'                                                                          ',&
'  a. A license of applicable copyright and neighbouring rights;           ',&
'                                                                          ',&
'  b. A license of the Database Right; and                                 ',&
'                                                                          ',&
'  c. An agreement in contract between You and the Licensor.               ',&
'                                                                          ',&
'2.2 Legal rights covered. This License covers the legal rights in the     ',&
'Database, including:                                                      ',&
'                                                                          ',&
'  a. Copyright. Any copyright or neighbouring rights in the Database.     ',&
'  The copyright licensed includes any individual elements of the          ',&
'  Database, but does not cover the copyright over the Contents            ',&
'  independent of this Database. See Section 2.4 for details. Copyright    ',&
'  law varies between jurisdictions, but is likely to cover: the Database  ',&
'  model or schema, which is the structure, arrangement, and organisation  ',&
'  of the Database, and can also include the Database tables and table     ',&
'  indexes; the data entry and output sheets; and the Field names of       ',&
'  Contents stored in the Database;                                        ',&
'                                                                          ',&
'  b. Database Rights. Database Rights only extend to the Extraction and   ',&
'  Re-utilisation of the whole or a Substantial part of the Contents.      ',&
'  Database Rights can apply even when there is no copyright over the      ',&
'  Database. Database Rights can also apply when the Contents are removed  ',&
'  from the Database and are selected and arranged in a way that would     ',&
'  not infringe any applicable copyright; and                              ',&
'                                                                          ',&
'  c. Contract. This is an agreement between You and the Licensor for      ',&
'  access to the Database. In return you agree to certain conditions of    ',&
'  use on this access as outlined in this License.                         ',&
'                                                                          ',&
'2.3 Rights not covered.                                                   ',&
'                                                                          ',&
'  a. This License does not apply to computer programs used in the making  ',&
'  or operation of the Database;                                           ',&
'                                                                          ',&
'  b. This License does not cover any patents over the Contents or the     ',&
'  Database; and                                                           ',&
'                                                                          ',&
'  c. This License does not cover any trademarks associated with the       ',&
'  Database.                                                               ',&
'                                                                          ',&
'2.4 Relationship to Contents in the Database. The individual items of     ',&
'the Contents contained in this Database may be covered by other rights,   ',&
'including copyright, patent, data protection, privacy, or personality     ',&
'rights, and this License does not cover any rights (other than Database   ',&
'Rights or in contract) in individual Contents contained in the Database.  ',&
'For example, if used on a Database of images (the Contents), this         ',&
'License would not apply to copyright over individual images, which could  ',&
'have their own separate licenses, or one single license covering all of   ',&
'the rights over the images.                                               ',&
'                                                                          ',&
'### 3.0 Rights granted                                                    ',&
'                                                                          ',&
'3.1 Subject to the terms and conditions of this License, the Licensor     ',&
'grants to You a worldwide, royalty-free, non-exclusive, terminable (but   ',&
'only under Section 9) license to Use the Database for the duration of     ',&
'any applicable copyright and Database Rights. These rights explicitly     ',&
'include commercial use, and do not exclude any field of endeavour. To     ',&
'the extent possible in the relevant jurisdiction, these rights may be     ',&
'exercised in all media and formats whether now known or created in the    ',&
'future.                                                                   ',&
'                                                                          ',&
'The rights granted cover, for example:                                    ',&
'                                                                          ',&
'  a. Extraction and Re-utilisation of the whole or a Substantial part of  ',&
'  the Contents;                                                           ',&
'                                                                          ',&
'  b. Creation of Derivative Databases;                                    ',&
'                                                                          ',&
'  c. Creation of Collective Databases;                                    ',&
'                                                                          ',&
'  d. Creation of temporary or permanent reproductions by any means and    ',&
'  in any form, in whole or in part, including of any Derivative           ',&
'  Databases or as a part of Collective Databases; and                     ',&
'                                                                          ',&
'  e. Distribution, communication, display, lending, making available, or  ',&
'  performance to the public by any means and in any form, in whole or in  ',&
'  part, including of any Derivative Database or as a part of Collective   ',&
'  Databases.                                                              ',&
'                                                                          ',&
'3.2 Compulsory license schemes. For the avoidance of doubt:               ',&
'                                                                          ',&
'  a. Non-waivable compulsory license schemes. In those jurisdictions in   ',&
'  which the right to collect royalties through any statutory or           ',&
'  compulsory licensing scheme cannot be waived, the Licensor reserves     ',&
'  the exclusive right to collect such royalties for any exercise by You   ',&
'  of the rights granted under this License;                               ',&
'                                                                          ',&
'  b. Waivable compulsory license schemes. In those jurisdictions in       ',&
'  which the right to collect royalties through any statutory or           ',&
'  compulsory licensing scheme can be waived, the Licensor waives the      ',&
'  exclusive right to collect such royalties for any exercise by You of    ',&
'  the rights granted under this License; and,                             ',&
'                                                                          ',&
'  c. Voluntary license schemes. The Licensor waives the right to collect  ',&
'  royalties, whether individually or, in the event that the Licensor is   ',&
'  a member of a collecting society that administers voluntary licensing   ',&
'  schemes, via that society, from any exercise by You of the rights       ',&
'  granted under this License.                                             ',&
'                                                                          ',&
'3.3 The right to release the Database under different terms, or to stop   ',&
'distributing or making available the Database, is reserved. Note that     ',&
'this Database may be multiple-licensed, and so You may have the choice    ',&
'of using alternative licenses for this Database. Subject to Section       ',&
'10.4, all other rights not expressly granted by Licensor are reserved.    ',&
'                                                                          ',&
'### 4.0 Conditions of Use                                                 ',&
'                                                                          ',&
'4.1 The rights granted in Section 3 above are expressly made subject to   ',&
'Your complying with the following conditions of use. These are important  ',&
'conditions of this License, and if You fail to follow them, You will be   ',&
'in material breach of its terms.                                          ',&
'                                                                          ',&
'4.2 Notices. If You Publicly Convey this Database, any Derivative         ',&
'Database, or the Database as part of a Collective Database, then You      ',&
'must:                                                                     ',&
'                                                                          ',&
'  a. Do so only under the terms of this License or another license        ',&
'  permitted under Section 4.4;                                            ',&
'                                                                          ',&
'  b. Include a copy of this License (or, as applicable, a license         ',&
'  permitted under Section 4.4) or its Uniform Resource Identifier (URI)   ',&
'  with the Database or Derivative Database, including both in the         ',&
'  Database or Derivative Database and in any relevant documentation; and  ',&
'                                                                          ',&
'  c. Keep intact any copyright or Database Right notices and notices      ',&
'  that refer to this License.                                             ',&
'                                                                          ',&
'  d. If it is not possible to put the required notices in a particular    ',&
'  file due to its structure, then You must include the notices in a       ',&
'  location (such as a relevant directory) where users would be likely to  ',&
'  look for it.                                                            ',&
'                                                                          ',&
'4.3 Notice for using output (Contents). Creating and Using a Produced     ',&
'Work does not require the notice in Section 4.2. However, if you          ',&
'Publicly Use a Produced Work, You must include a notice associated with   ',&
'the Produced Work reasonably calculated to make any Person that uses,     ',&
'views, accesses, interacts with, or is otherwise exposed to the Produced  ',&
'Work aware that Content was obtained from the Database, Derivative        ',&
'Database, or the Database as part of a Collective Database, and that it   ',&
'is available under this License.                                          ',&
'                                                                          ',&
'  a. Example notice. The following text will satisfy notice under         ',&
'  Section 4.3:                                                            ',&
'                                                                          ',&
'        Contains information from DATABASE NAME, which is made available  ',&
'        here under the Open Database License (ODbL).                      ',&
'                                                                          ',&
'DATABASE NAME should be replaced with the name of the Database and a      ',&
'hyperlink to the URI of the Database. "Open Database License" should      ',&
'contain a hyperlink to the URI of the text of this License. If            ',&
'hyperlinks are not possible, You should include the plain text of the     ',&
'required URI''s with the above notice.                                    ',&
'                                                                          ',&
'4.4 Share alike.                                                          ',&
'                                                                          ',&
'  a. Any Derivative Database that You Publicly Use must be only under     ',&
'  the terms of:                                                           ',&
'                                                                          ',&
'    i. This License;                                                      ',&
'                                                                          ',&
'    ii. A later version of this License similar in spirit to this         ',&
'      License; or                                                         ',&
'                                                                          ',&
'    iii. A compatible license.                                            ',&
'                                                                          ',&
'  If You license the Derivative Database under one of the licenses        ',&
'  mentioned in (iii), You must comply with the terms of that license.     ',&
'                                                                          ',&
'  b. For the avoidance of doubt, Extraction or Re-utilisation of the      ',&
'  whole or a Substantial part of the Contents into a new database is a    ',&
'  Derivative Database and must comply with Section 4.4.                   ',&
'                                                                          ',&
'  c. Derivative Databases and Produced Works.  A Derivative Database is   ',&
'  Publicly Used and so must comply with Section 4.4. if a Produced Work   ',&
'  created from the Derivative Database is Publicly Used.                  ',&
'                                                                          ',&
'  d. Share Alike and additional Contents. For the avoidance of doubt,     ',&
'  You must not add Contents to Derivative Databases under Section 4.4 a   ',&
'  that are incompatible with the rights granted under this License.       ',&
'                                                                          ',&
'  e. Compatible licenses. Licensors may authorise a proxy to determine    ',&
'  compatible licenses under Section 4.4 a iii. If they do so, the         ',&
'  authorised proxy''s public statement of acceptance of a compatible      ',&
'  license grants You permission to use the compatible license.            ',&
'                                                                          ',&
'                                                                          ',&
'4.5 Limits of Share Alike.  The requirements of Section 4.4 do not apply  ',&
'in the following:                                                         ',&
'                                                                          ',&
'  a. For the avoidance of doubt, You are not required to license          ',&
'  Collective Databases under this License if You incorporate this         ',&
'  Database or a Derivative Database in the collection, but this License   ',&
'  still applies to this Database or a Derivative Database as a part of    ',&
'  the Collective Database;                                                ',&
'                                                                          ',&
'  b. Using this Database, a Derivative Database, or this Database as      ',&
'  part of a Collective Database to create a Produced Work does not        ',&
'  create a Derivative Database for purposes of  Section 4.4; and          ',&
'                                                                          ',&
'  c. Use of a Derivative Database internally within an organisation is    ',&
'  not to the public and therefore does not fall under the requirements    ',&
'  of Section 4.4.                                                         ',&
'                                                                          ',&
'4.6 Access to Derivative Databases. If You Publicly Use a Derivative      ',&
'Database or a Produced Work from a Derivative Database, You must also     ',&
'offer to recipients of the Derivative Database or Produced Work a copy    ',&
'in a machine readable form of:                                            ',&
'                                                                          ',&
'  a. The entire Derivative Database; or                                   ',&
'                                                                          ',&
'  b. A file containing all of the alterations made to the Database or     ',&
'  the method of making the alterations to the Database (such as an        ',&
'  algorithm), including any additional Contents, that make up all the     ',&
'  differences between the Database and the Derivative Database.           ',&
'                                                                          ',&
'The Derivative Database (under a.) or alteration file (under b.) must be  ',&
'available at no more than a reasonable production cost for physical       ',&
'distributions and free of charge if distributed over the internet.        ',&
'                                                                          ',&
'4.7 Technological measures and additional terms                           ',&
'                                                                          ',&
'  a. This License does not allow You to impose (except subject to         ',&
'  Section 4.7 b.)  any terms or any technological measures on the         ',&
'  Database, a Derivative Database, or the whole or a Substantial part of  ',&
'  the Contents that alter or restrict the terms of this License, or any   ',&
'  rights granted under it, or have the effect or intent of restricting    ',&
'  the ability of any person to exercise those rights.                     ',&
'                                                                          ',&
'  b. Parallel distribution. You may impose terms or technological         ',&
'  measures on the Database, a Derivative Database, or the whole or a      ',&
'  Substantial part of the Contents (a "Restricted Database") in           ',&
'  contravention of Section 4.74 a. only if You also make a copy of the    ',&
'  Database or a Derivative Database available to the recipient of the     ',&
'  Restricted Database:                                                    ',&
'                                                                          ',&
'    i. That is available without additional fee;                          ',&
'                                                                          ',&
'    ii. That is available in a medium that does not alter or restrict     ',&
'    the terms of this License, or any rights granted under it, or have    ',&
'    the effect or intent of restricting the ability of any person to      ',&
'    exercise those rights (an "Unrestricted Database"); and               ',&
'                                                                          ',&
'    iii. The Unrestricted Database is at least as accessible to the       ',&
'    recipient as a practical matter as the Restricted Database.           ',&
'                                                                          ',&
'  c. For the avoidance of doubt, You may place this Database or a         ',&
'  Derivative Database in an authenticated environment, behind a           ',&
'  password, or within a similar access control scheme provided that You   ',&
'  do not alter or restrict the terms of this License or any rights        ',&
'  granted under it or have the effect or intent of restricting the        ',&
'  ability of any person to exercise those rights.                         ',&
'                                                                          ',&
'4.8 Licensing of others. You may not sublicense the Database. Each time   ',&
'You communicate the Database, the whole or Substantial part of the        ',&
'Contents, or any Derivative Database to anyone else in any way, the       ',&
'Licensor offers to the recipient a license to the Database on the same    ',&
'terms and conditions as this License. You are not responsible for         ',&
'enforcing compliance by third parties with this License, but You may      ',&
'enforce any rights that You have over a Derivative Database. You are      ',&
'solely responsible for any modifications of a Derivative Database made    ',&
'by You or another Person at Your direction. You may not impose any        ',&
'further restrictions on the exercise of the rights granted or affirmed    ',&
'under this License.                                                       ',&
'                                                                          ',&
'### 5.0 Moral rights                                                      ',&
'                                                                          ',&
'5.1 Moral rights. This section covers moral rights, including any rights  ',&
'to be identified as the author of the Database or to object to treatment  ',&
'that would otherwise prejudice the author''s honour and reputation, or    ',&
'any other derogatory treatment:                                           ',&
'                                                                          ',&
'  a. For jurisdictions allowing waiver of moral rights, Licensor waives   ',&
'  all moral rights that Licensor may have in the Database to the fullest  ',&
'  extent possible by the law of the relevant jurisdiction under Section   ',&
'  10.4;                                                                   ',&
'                                                                          ',&
'  b. If waiver of moral rights under Section 5.1 a in the relevant        ',&
'  jurisdiction is not possible, Licensor agrees not to assert any moral   ',&
'  rights over the Database and waives all claims in moral rights to the   ',&
'  fullest extent possible by the law of the relevant jurisdiction under   ',&
'  Section 10.4; and                                                       ',&
'                                                                          ',&
'  c. For jurisdictions not allowing waiver or an agreement not to assert  ',&
'  moral rights under Section 5.1 a and b, the author may retain their     ',&
'  moral rights over certain aspects of the Database.                      ',&
'                                                                          ',&
'Please note that some jurisdictions do not allow for the waiver of moral  ',&
'rights, and so moral rights may still subsist over the Database in some   ',&
'jurisdictions.                                                            ',&
'                                                                          ',&
'### 6.0 Fair dealing, Database exceptions, and other rights not affected  ',&
'                                                                          ',&
'6.1 This License does not affect any rights that You or anyone else may   ',&
'independently have under any applicable law to make any use of this       ',&
'Database, including without limitation:                                   ',&
'                                                                          ',&
'  a. Exceptions to the Database Right including: Extraction of Contents   ',&
'  from non-electronic Databases for private purposes, Extraction for      ',&
'  purposes of illustration for teaching or scientific research, and       ',&
'  Extraction or Re-utilisation for public security or an administrative   ',&
'  or judicial procedure.                                                  ',&
'                                                                          ',&
'  b. Fair dealing, fair use, or any other legally recognised limitation   ',&
'  or exception to infringement of copyright or other applicable laws.     ',&
'                                                                          ',&
'6.2 This License does not affect any rights of lawful users to Extract    ',&
'and Re-utilise insubstantial parts of the Contents, evaluated             ',&
'quantitatively or qualitatively, for any purposes whatsoever, including   ',&
'creating a Derivative Database (subject to other rights over the          ',&
'Contents, see Section 2.4). The repeated and systematic Extraction or     ',&
'Re-utilisation of insubstantial parts of the Contents may however amount  ',&
'to the Extraction or Re-utilisation of a Substantial part of the          ',&
'Contents.                                                                 ',&
'                                                                          ',&
'### 7.0 Warranties and Disclaimer                                         ',&
'                                                                          ',&
'7.1 The Database is licensed by the Licensor "as is" and without any      ',&
'warranty of any kind, either express, implied, or arising by statute,     ',&
'custom, course of dealing, or trade usage. Licensor specifically          ',&
'disclaims any and all implied warranties or conditions of title,          ',&
'non-infringement, accuracy or completeness, the presence or absence of    ',&
'errors, fitness for a particular purpose, merchantability, or otherwise.  ',&
'Some jurisdictions do not allow the exclusion of implied warranties, so   ',&
'this exclusion may not apply to You.                                      ',&
'                                                                          ',&
'### 8.0 Limitation of liability                                           ',&
'                                                                          ',&
'8.1 Subject to any liability that may not be excluded or limited by law,  ',&
'the Licensor is not liable for, and expressly excludes, all liability     ',&
'for loss or damage however and whenever caused to anyone by any use       ',&
'under this License, whether by You or by anyone else, and whether caused  ',&
'by any fault on the part of the Licensor or not. This exclusion of        ',&
'liability includes, but is not limited to, any special, incidental,       ',&
'consequential, punitive, or exemplary damages such as loss of revenue,    ',&
'data, anticipated profits, and lost business. This exclusion applies      ',&
'even if the Licensor has been advised of the possibility of such          ',&
'damages.                                                                  ',&
'                                                                          ',&
'8.2 If liability may not be excluded by law, it is limited to actual and  ',&
'direct financial loss to the extent it is caused by proved negligence on  ',&
'the part of the Licensor.                                                 ',&
'                                                                          ',&
'### 9.0 Termination of Your rights under this License                     ',&
'                                                                          ',&
'9.1 Any breach by You of the terms and conditions of this License         ',&
'automatically terminates this License with immediate effect and without   ',&
'notice to You. For the avoidance of doubt, Persons who have received the  ',&
'Database, the whole or a Substantial part of the Contents, Derivative     ',&
'Databases, or the Database as part of a Collective Database from You      ',&
'under this License will not have their licenses terminated provided       ',&
'their use is in full compliance with this License or a license granted    ',&
'under Section 4.8 of this License.  Sections 1, 2, 7, 8, 9 and 10 will    ',&
'survive any termination of this License.                                  ',&
'                                                                          ',&
'9.2 If You are not in breach of the terms of this License, the Licensor   ',&
'will not terminate Your rights under it.                                  ',&
'                                                                          ',&
'9.3 Unless terminated under Section 9.1, this License is granted to You   ',&
'for the duration of applicable rights in the Database.                    ',&
'                                                                          ',&
'9.4 Reinstatement of rights. If you cease any breach of the terms and     ',&
'conditions of this License, then your full rights under this License      ',&
'will be reinstated:                                                       ',&
'                                                                          ',&
'  a. Provisionally and subject to permanent termination until the 60th    ',&
'  day after cessation of breach;                                          ',&
'                                                                          ',&
'  b. Permanently on the 60th day after cessation of breach unless         ',&
'  otherwise reasonably notified by the Licensor; or                       ',&
'                                                                          ',&
'  c.  Permanently if reasonably notified by the Licensor of the           ',&
'  violation, this is the first time You have received notice of           ',&
'  violation of this License from  the Licensor, and You cure the          ',&
'  violation prior to 30 days after your receipt of the notice.            ',&
'                                                                          ',&
'Persons subject to permanent termination of rights are not eligible to    ',&
'be a recipient and receive a license under Section 4.8.                   ',&
'                                                                          ',&
'9.5 Notwithstanding the above, Licensor reserves the right to release     ',&
'the Database under different license terms or to stop distributing or     ',&
'making available the Database. Releasing the Database under different     ',&
'license terms or stopping the distribution of the Database will not       ',&
'withdraw this License (or any other license that has been, or is          ',&
'required to be, granted under the terms of this License), and this        ',&
'License will continue in full force and effect unless terminated as       ',&
'stated above.                                                             ',&
'                                                                          ',&
'### 10.0 General                                                          ',&
'                                                                          ',&
'10.1 If any provision of this License is held to be invalid or            ',&
'unenforceable, that must not affect the validity or enforceability of     ',&
'the remainder of the terms and conditions of this License and each        ',&
'remaining provision of this License shall be valid and enforced to the    ',&
'fullest extent permitted by law.                                          ',&
'                                                                          ',&
'10.2 This License is the entire agreement between the parties with        ',&
'respect to the rights granted here over the Database. It replaces any     ',&
'earlier understandings, agreements or representations with respect to     ',&
'the Database.                                                             ',&
'                                                                          ',&
'10.3 If You are in breach of the terms of this License, You will not be   ',&
'entitled to rely on the terms of this License or to complain of any       ',&
'breach by the Licensor.                                                   ',&
'                                                                          ',&
'10.4 Choice of law. This License takes effect in and will be governed by  ',&
'the laws of the relevant jurisdiction in which the License terms are      ',&
'sought to be enforced. If the standard suite of rights granted under      ',&
'applicable copyright law and Database Rights in the relevant              ',&
'jurisdiction includes additional rights not granted under this License,   ',&
'these additional rights are granted in this License in order to meet the  ',&
'terms of this License.                                                    ',&
'                                                                          ',&
'']
if(present(topic_only))then
   if(size(textblock)>0)then
      if(topic_only) textblock=[character(len=len(textblock(1))) :: textblock(1)]
   else
      textblock=['']
   endif
endif
case('40','ofl-1.1')
textblock=[ CHARACTER(LEN=128) :: &
'ofl-1.1',&
'       ',&
'       ',&
'Copyright (c) @YEAR@ @FULLNAME@ (@EMAIL@)',&
'                                         ',&
'This Font Software is licensed under the SIL Open Font License, Version 1.1.',&
'This license is copied below, and is also available with a FAQ at:          ',&
'http://scripts.sil.org/OFL                                                  ',&
'                                                                            ',&
'-----------------------------------------------------------                 ',&
'SIL OPEN FONT LICENSE Version 1.1 - 26 February 2007                        ',&
'-----------------------------------------------------------                 ',&
'                                                                            ',&
'PREAMBLE                                                                    ',&
'The goals of the Open Font License (OFL) are to stimulate worldwide         ',&
'development of collaborative font projects, to support the font creation    ',&
'efforts of academic and linguistic communities, and to provide a free and   ',&
'open framework in which fonts may be shared and improved in partnership     ',&
'with others.                                                                ',&
'                                                                            ',&
'The OFL allows the licensed fonts to be used, studied, modified and         ',&
'redistributed freely as long as they are not sold by themselves. The        ',&
'fonts, including any derivative works, can be bundled, embedded,            ',&
'redistributed and/or sold with any software provided that any reserved      ',&
'names are not used by derivative works. The fonts and derivatives,          ',&
'however, cannot be released under any other type of license. The            ',&
'requirement for fonts to remain under this license does not apply           ',&
'to any document created using the fonts or their derivatives.               ',&
'                                                                            ',&
'DEFINITIONS                                                                 ',&
'"Font Software" refers to the set of files released by the Copyright        ',&
'Holder(s) under this license and clearly marked as such. This may           ',&
'include source files, build scripts and documentation.                      ',&
'                                                                            ',&
'"Reserved Font Name" refers to any names specified as such after the        ',&
'copyright statement(s).                                                     ',&
'                                                                            ',&
'"Original Version" refers to the collection of Font Software components as  ',&
'distributed by the Copyright Holder(s).                                     ',&
'                                                                            ',&
'"Modified Version" refers to any derivative made by adding to, deleting,    ',&
'or substituting -- in part or in whole -- any of the components of the      ',&
'Original Version, by changing formats or by porting the Font Software to a  ',&
'new environment.                                                            ',&
'                                                                            ',&
'"Author" refers to any designer, engineer, programmer, technical            ',&
'writer or other person who contributed to the Font Software.                ',&
'                                                                            ',&
'PERMISSION AND CONDITIONS                                                   ',&
'Permission is hereby granted, free of charge, to any person obtaining       ',&
'a copy of the Font Software, to use, study, copy, merge, embed, modify,     ',&
'redistribute, and sell modified and unmodified copies of the Font           ',&
'Software, subject to the following conditions:                              ',&
'                                                                            ',&
'1) Neither the Font Software nor any of its individual components,          ',&
'in Original or Modified Versions, may be sold by itself.                    ',&
'                                                                            ',&
'2) Original or Modified Versions of the Font Software may be bundled,       ',&
'redistributed and/or sold with any software, provided that each copy        ',&
'contains the above copyright notice and this license. These can be          ',&
'included either as stand-alone text files, human-readable headers or        ',&
'in the appropriate machine-readable metadata fields within text or          ',&
'binary files as long as those fields can be easily viewed by the user.      ',&
'                                                                            ',&
'3) No Modified Version of the Font Software may use the Reserved Font       ',&
'Name(s) unless explicit written permission is granted by the corresponding  ',&
'Copyright Holder. This restriction only applies to the primary font name as ',&
'presented to the users.                                                     ',&
'                                                                            ',&
'4) The name(s) of the Copyright Holder(s) or the Author(s) of the Font      ',&
'Software shall not be used to promote, endorse or advertise any             ',&
'Modified Version, except to acknowledge the contribution(s) of the          ',&
'Copyright Holder(s) and the Author(s) or with their explicit written        ',&
'permission.                                                                 ',&
'                                                                            ',&
'5) The Font Software, modified or unmodified, in part or in whole,          ',&
'must be distributed entirely under this license, and must not be            ',&
'distributed under any other license. The requirement for fonts to           ',&
'remain under this license does not apply to any document created            ',&
'using the Font Software.                                                    ',&
'                                                                            ',&
'TERMINATION                                                                 ',&
'This license becomes null and void if any of the above conditions are       ',&
'not met.                                                                    ',&
'                                                                            ',&
'DISCLAIMER                                                                  ',&
'THE FONT SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,        ',&
'EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO ANY WARRANTIES OF          ',&
'MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT       ',&
'OF COPYRIGHT, PATENT, TRADEMARK, OR OTHER RIGHT. IN NO EVENT SHALL THE      ',&
'COPYRIGHT HOLDER BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,       ',&
'INCLUDING ANY GENERAL, SPECIAL, INDIRECT, INCIDENTAL, OR CONSEQUENTIAL      ',&
'DAMAGES, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING       ',&
'FROM, OUT OF THE USE OR INABILITY TO USE THE FONT SOFTWARE OR FROM          ',&
'OTHER DEALINGS IN THE FONT SOFTWARE.                                        ',&
'                                                                            ',&
'']
if(present(topic_only))then
   if(size(textblock)>0)then
      if(topic_only) textblock=[character(len=len(textblock(1))) :: textblock(1)]
   else
      textblock=['']
   endif
endif
case('41','osl-3.0')
textblock=[ CHARACTER(LEN=128) :: &
'osl-3.0',&
'       ',&
'Open Software License ("OSL") v. 3.0',&
'                                    ',&
'This Open Software License (the "License") applies to any original work of',&
'authorship (the "Original Work") whose owner (the "Licensor") has placed the',&
'following licensing notice adjacent to the copyright notice for the Original',&
'Work:                                                                       ',&
'                                                                            ',&
'  Licensed under the Open Software License version 3.0                      ',&
'                                                                            ',&
'1) Grant of Copyright License. Licensor grants You a worldwide, royalty-free,',&
'non-exclusive, sublicensable license, for the duration of the copyright, to do',&
'the following:                                                                ',&
'                                                                              ',&
'  a) to reproduce the Original Work in copies, either alone or as part of a   ',&
'  collective work;                                                            ',&
'                                                                              ',&
'  b) to translate, adapt, alter, transform, modify, or arrange the Original   ',&
'  Work, thereby creating derivative works ("Derivative Works") based upon the ',&
'  Original Work;                                                              ',&
'                                                                              ',&
'  c) to distribute or communicate copies of the Original Work and Derivative  ',&
'  Works to the public, with the proviso that copies of Original Work or       ',&
'  Derivative Works that You distribute or communicate shall be licensed under ',&
'  this Open Software License;                                                 ',&
'                                                                              ',&
'  d) to perform the Original Work publicly; and                               ',&
'                                                                              ',&
'  e) to display the Original Work publicly.                                   ',&
'                                                                              ',&
'2) Grant of Patent License. Licensor grants You a worldwide, royalty-free,    ',&
'non-exclusive, sublicensable license, under patent claims owned or controlled ',&
'by the Licensor that are embodied in the Original Work as furnished by the    ',&
'Licensor, for the duration of the patents, to make, use, sell, offer for sale,',&
'have made, and import the Original Work and Derivative Works.                 ',&
'                                                                              ',&
'3) Grant of Source Code License. The term "Source Code" means the preferred   ',&
'form of the Original Work for making modifications to it and all available    ',&
'documentation describing how to modify the Original Work. Licensor agrees to  ',&
'provide a machine-readable copy of the Source Code of the Original Work along ',&
'with each copy of the Original Work that Licensor distributes. Licensor       ',&
'reserves the right to satisfy this obligation by placing a machine-readable   ',&
'copy of the Source Code in an information repository reasonably calculated to ',&
'permit inexpensive and convenient access by You for as long as Licensor       ',&
'continues to distribute the Original Work.                                    ',&
'                                                                              ',&
'4) Exclusions From License Grant. Neither the names of Licensor, nor the names',&
'of any contributors to the Original Work, nor any of their trademarks or      ',&
'service marks, may be used to endorse or promote products derived from this   ',&
'Original Work without express prior permission of the Licensor. Except as     ',&
'expressly stated herein, nothing in this License grants any license to        ',&
'Licensor''s trademarks, copyrights, patents, trade secrets or any other       ',&
'intellectual property. No patent license is granted to make, use, sell, offer ',&
'for sale, have made, or import embodiments of any patent claims other than the',&
'licensed claims defined in Section 2. No license is granted to the trademarks ',&
'of Licensor even if such marks are included in the Original Work. Nothing in  ',&
'this License shall be interpreted to prohibit Licensor from licensing under   ',&
'terms different from this License any Original Work that Licensor otherwise   ',&
'would have a right to license.                                                ',&
'                                                                              ',&
'5) External Deployment. The term "External Deployment" means the use,         ',&
'distribution, or communication of the Original Work or Derivative Works in any',&
'way such that the Original Work or Derivative Works may be used by anyone     ',&
'other than You, whether those works are distributed or communicated to those  ',&
'persons or made available as an application intended for use over a network.  ',&
'As an express condition for the grants of license hereunder, You must treat   ',&
'any External Deployment by You of the Original Work or a Derivative Work as a ',&
'distribution under section 1(c).                                              ',&
'                                                                              ',&
'6) Attribution Rights. You must retain, in the Source Code of any Derivative  ',&
'Works that You create, all copyright, patent, or trademark notices from the   ',&
'Source Code of the Original Work, as well as any notices of licensing and any ',&
'descriptive text identified therein as an "Attribution Notice." You must cause',&
'the Source Code for any Derivative Works that You create to carry a prominent ',&
'Attribution Notice reasonably calculated to inform recipients that You have   ',&
'modified the Original Work.                                                   ',&
'                                                                              ',&
'7) Warranty of Provenance and Disclaimer of Warranty. Licensor warrants that  ',&
'the copyright in and to the Original Work and the patent rights granted herein',&
'by Licensor are owned by the Licensor or are sublicensed to You under the     ',&
'terms of this License with the permission of the contributor(s) of those      ',&
'copyrights and patent rights. Except as expressly stated in the immediately   ',&
'preceding sentence, the Original Work is provided under this License on an "AS',&
'IS" BASIS and WITHOUT WARRANTY, either express or implied, including, without ',&
'limitation, the warranties of non-infringement, merchantability or fitness for',&
'a particular purpose. THE ENTIRE RISK AS TO THE QUALITY OF THE ORIGINAL WORK  ',&
'IS WITH YOU. This DISCLAIMER OF WARRANTY constitutes an essential part of this',&
'License. No license to the Original Work is granted by this License except    ',&
'under this disclaimer.                                                        ',&
'                                                                              ',&
'8) Limitation of Liability. Under no circumstances and under no legal theory, ',&
'whether in tort (including negligence), contract, or otherwise, shall the     ',&
'Licensor be liable to anyone for any indirect, special, incidental, or        ',&
'consequential damages of any character arising as a result of this License or ',&
'the use of the Original Work including, without limitation, damages for loss  ',&
'of goodwill, work stoppage, computer failure or malfunction, or any and all   ',&
'other commercial damages or losses. This limitation of liability shall not    ',&
'apply to the extent applicable law prohibits such limitation.                 ',&
'                                                                              ',&
'9) Acceptance and Termination. If, at any time, You expressly assented to this',&
'License, that assent indicates your clear and irrevocable acceptance of this  ',&
'License and all of its terms and conditions. If You distribute or communicate ',&
'copies of the Original Work or a Derivative Work, You must make a reasonable  ',&
'effort under the circumstances to obtain the express assent of recipients to  ',&
'the terms of this License. This License conditions your rights to undertake   ',&
'the activities listed in Section 1, including your right to create Derivative ',&
'Works based upon the Original Work, and doing so without honoring these terms ',&
'and conditions is prohibited by copyright law and international treaty.       ',&
'Nothing in this License is intended to affect copyright exceptions and        ',&
'limitations (including "fair use" or "fair dealing"). This License shall      ',&
'terminate immediately and You may no longer exercise any of the rights granted',&
'to You by this License upon your failure to honor the conditions in Section   ',&
'1(c).                                                                         ',&
'                                                                              ',&
'10) Termination for Patent Action. This License shall terminate automatically ',&
'and You may no longer exercise any of the rights granted to You by this       ',&
'License as of the date You commence an action, including a cross-claim or     ',&
'counterclaim, against Licensor or any licensee alleging that the Original Work',&
'infringes a patent. This termination provision shall not apply for an action  ',&
'alleging patent infringement by combinations of the Original Work with other  ',&
'software or hardware.                                                         ',&
'                                                                              ',&
'11) Jurisdiction, Venue and Governing Law. Any action or suit relating to this',&
'License may be brought only in the courts of a jurisdiction wherein the       ',&
'Licensor resides or in which Licensor conducts its primary business, and under',&
'the laws of that jurisdiction excluding its conflict-of-law provisions. The   ',&
'application of the United Nations Convention on Contracts for the             ',&
'International Sale of Goods is expressly excluded. Any use of the Original    ',&
'Work outside the scope of this License or after its termination shall be      ',&
'subject to the requirements and penalties of copyright or patent law in the   ',&
'appropriate jurisdiction. This section shall survive the termination of this  ',&
'License.                                                                      ',&
'                                                                              ',&
'12) Attorneys'' Fees. In any action to enforce the terms of this License or   ',&
'seeking damages relating thereto, the prevailing party shall be entitled to   ',&
'recover its costs and expenses, including, without limitation, reasonable     ',&
'attorneys'' fees and costs incurred in connection with such action, including ',&
'any appeal of such action. This section shall survive the termination of this ',&
'License.                                                                      ',&
'                                                                              ',&
'13) Miscellaneous. If any provision of this License is held to be             ',&
'unenforceable, such provision shall be reformed only to the extent necessary  ',&
'to make it enforceable.                                                       ',&
'                                                                              ',&
'14) Definition of "You" in This License. "You" throughout this License,       ',&
'whether in upper or lower case, means an individual or a legal entity         ',&
'exercising rights under, and complying with all of the terms of, this License.',&
'For legal entities, "You" includes any entity that controls, is controlled by,',&
'or is under common control with you. For purposes of this definition,         ',&
'"control" means (i) the power, direct or indirect, to cause the direction or  ',&
'management of such entity, whether by contract or otherwise, or (ii) ownership',&
'of fifty percent (50%) or more of the outstanding shares, or (iii) beneficial ',&
'ownership of such entity.                                                     ',&
'                                                                              ',&
'15) Right to Use. You may use the Original Work in all ways not otherwise     ',&
'restricted or conditioned by this License or by law, and Licensor promises not',&
'to interfere with or be responsible for such uses by You.                     ',&
'                                                                              ',&
'16) Modification of This License. This License is Copyright © 2005 Lawrence  ',&
'Rosen. Permission is granted to copy, distribute, or communicate this License ',&
'without modification. Nothing in this License permits You to modify this      ',&
'License as applied to the Original Work or to Derivative Works. However, You  ',&
'may modify the text of this License and copy, distribute or communicate your  ',&
'modified version (the "Modified License") and apply it to other original works',&
'of authorship subject to the following conditions: (i) You may not indicate in',&
'any way that your Modified License is the "Open Software License" or "OSL" and',&
'you may not use those names in the name of your Modified License; (ii) You    ',&
'must replace the notice specified in the first paragraph above with the notice',&
'"Licensed under <insert your license name here>" or with a notice of your own ',&
'that is not confusingly similar to the notice in this License; and (iii) You  ',&
'may not claim that your original works are open source software unless your   ',&
'Modified License has been approved by Open Source Initiative (OSI) and You    ',&
'comply with its license review and certification process.                     ',&
'                                                                              ',&
'']
if(present(topic_only))then
   if(size(textblock)>0)then
      if(topic_only) textblock=[character(len=len(textblock(1))) :: textblock(1)]
   else
      textblock=['']
   endif
endif
case('42','postgresql')
textblock=[ CHARACTER(LEN=128) :: &
'postgresql',&
'          ',&
'PostgreSQL License',&
'                  ',&
'Copyright (c) @YEAR@, @FULLNAME@',&
'                                ',&
'Permission to use, copy, modify, and distribute this software and its',&
'documentation for any purpose, without fee, and without a written agreement is',&
'hereby granted, provided that the above copyright notice and this paragraph   ',&
'and the following two paragraphs appear in all copies.                        ',&
'                                                                              ',&
'IN NO EVENT SHALL @FULLNAME@ BE LIABLE TO ANY PARTY FOR DIRECT, INDIRECT,     ',&
'SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES, INCLUDING LOST PROFITS, ARISING',&
'OUT OF THE USE OF THIS SOFTWARE AND ITS DOCUMENTATION, EVEN IF @FULLNAME@     ',&
'HAS BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.                           ',&
'                                                                              ',&
'@FULLNAME@ SPECIFICALLY DISCLAIMS ANY WARRANTIES, INCLUDING, BUT NOT          ',&
'LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A       ',&
'PARTICULAR PURPOSE. THE SOFTWARE PROVIDED HEREUNDER IS ON AN "AS IS" BASIS,   ',&
'AND @FULLNAME@ HAS NO OBLIGATIONS TO PROVIDE MAINTENANCE, SUPPORT, UPDATES,   ',&
'ENHANCEMENTS, OR MODIFICATIONS.                                               ',&
'                                                                              ',&
'']
if(present(topic_only))then
   if(size(textblock)>0)then
      if(topic_only) textblock=[character(len=len(textblock(1))) :: textblock(1)]
   else
      textblock=['']
   endif
endif
case('43','unlicense')
textblock=[ CHARACTER(LEN=128) :: &
'unlicense',&
'         ',&
'This is free and unencumbered software released into the public domain.',&
'                                                                       ',&
'Anyone is free to copy, modify, publish, use, compile, sell, or        ',&
'distribute this software, either in source code form or as a compiled  ',&
'binary, for any purpose, commercial or non-commercial, and by any      ',&
'means.                                                                 ',&
'                                                                       ',&
'In jurisdictions that recognize copyright laws, the author or authors  ',&
'of this software dedicate any and all copyright interest in the        ',&
'software to the public domain. We make this dedication for the benefit ',&
'of the public at large and to the detriment of our heirs and           ',&
'successors. We intend this dedication to be an overt act of            ',&
'relinquishment in perpetuity of all present and future rights to this  ',&
'software under copyright law.                                          ',&
'                                                                       ',&
'THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,        ',&
'EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF     ',&
'MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. ',&
'IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY CLAIM, DAMAGES OR      ',&
'OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,  ',&
'ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR  ',&
'OTHER DEALINGS IN THE SOFTWARE.                                        ',&
'                                                                       ',&
'For more information, please refer to <https://unlicense.org>          ',&
'']
if(present(topic_only))then
   if(size(textblock)>0)then
      if(topic_only) textblock=[character(len=len(textblock(1))) :: textblock(1)]
   else
      textblock=['']
   endif
endif
case('44','upl-1.0')
textblock=[ CHARACTER(LEN=128) :: &
'upl-1.0',&
'       ',&
'The Universal Permissive License (UPL), Version 1.0',&
'                                                   ',&
'Subject to the condition set forth below, permission is hereby granted to any',&
'person obtaining a copy of this software, associated documentation and/or data',&
'(collectively the "Software"), free of charge and under any and all copyright ',&
'rights in the Software, and any and all patent rights owned or freely         ',&
'licensable by each licensor hereunder covering either (i) the unmodified      ',&
'Software as contributed to or provided by such licensor, or (ii) the Larger   ',&
'Works (as defined below), to deal in both                                     ',&
'                                                                              ',&
'(a) the Software, and                                                         ',&
'(b) any piece of software and/or hardware listed in the lrgrwrks.txt file if  ',&
'one is included with the Software (each a "Larger Work" to which the Software ',&
'is contributed by such licensors),                                            ',&
'                                                                              ',&
'without restriction, including without limitation the rights to copy, create  ',&
'derivative works of, display, perform, and distribute the Software and make,  ',&
'use, sell, offer for sale, import, export, have made, and have sold the       ',&
'Software and the Larger Work(s), and to sublicense the foregoing rights on    ',&
'either these or other terms.                                                  ',&
'                                                                              ',&
'This license is subject to the following condition:                           ',&
'The above copyright notice and either this complete permission notice or at   ',&
'a minimum a reference to the UPL must be included in all copies or            ',&
'substantial portions of the Software.                                         ',&
'                                                                              ',&
'THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR    ',&
'IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,      ',&
'FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE   ',&
'AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER        ',&
'LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, ',&
'OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE ',&
'SOFTWARE.                                                                     ',&
'                                                                              ',&
'']
if(present(topic_only))then
   if(size(textblock)>0)then
      if(topic_only) textblock=[character(len=len(textblock(1))) :: textblock(1)]
   else
      textblock=['']
   endif
endif
case('45','vim')
textblock=[ CHARACTER(LEN=128) :: &
'vim',&
'   ',&
'VIM LICENSE',&
'           ',&
'I)  There are no restrictions on distributing unmodified copies of @PROJECT@',&
'    except that they must include this license text.  You can also distribute',&
'    unmodified parts of @PROJECT@, likewise unrestricted except that they must',&
'    include this license text.  You are also allowed to include executables   ',&
'    that you made from the unmodified @PROJECT@ sources, plus your own usage  ',&
'    examples and Vim scripts.                                                 ',&
'                                                                              ',&
'II) It is allowed to distribute a modified (or extended) version of @PROJECT@,',&
'    including executables and/or source code, when the following four         ',&
'    conditions are met:                                                       ',&
'    1) This license text must be included unmodified.                         ',&
'    2) The modified @PROJECT@ must be distributed in one of the following five',&
'       ways:                                                                  ',&
'       a) If you make changes to @PROJECT@ yourself, you must clearly describe',&
'          in the distribution how to contact you.  When the maintainer asks   ',&
'          you (in any way) for a copy of the modified @PROJECT@ you           ',&
'          distributed, you must make your changes, including source code,     ',&
'          available to the maintainer without fee.  The maintainer reserves   ',&
'          the right to include your changes in the official version of        ',&
'          @PROJECT@.  What the maintainer will do with your changes and under ',&
'          what license they will be distributed is negotiable.  If there has  ',&
'          been no negotiation then this license, or a later version, also     ',&
'          applies to your changes. The current maintainer is Bram Moolenaar   ',&
'          <Bram@vim.org>.  If this changes it will be announced in appropriate',&
'          places (most likely vim.sf.net, www.vim.org and/or comp.editors).   ',&
'          When it is completely impossible to contact the maintainer, the     ',&
'          obligation to send him your changes ceases.  Once the maintainer has',&
'          confirmed that he has received your changes they will not have to be',&
'          sent again.                                                         ',&
'       b) If you have received a modified @PROJECT@ that was distributed as   ',&
'          mentioned under a) you are allowed to further distribute it         ',&
'          unmodified, as mentioned at I).  If you make additional changes the ',&
'          text under a) applies to those changes.                             ',&
'       c) Provide all the changes, including source code, with every copy of  ',&
'          the modified @PROJECT@ you distribute.  This may be done in the form',&
'          of a context diff.  You can choose what license to use for new code ',&
'          you add.  The changes and their license must not restrict others    ',&
'          from making their own changes to the official version of @PROJECT@. ',&
'       d) When you have a modified @PROJECT@ which includes changes as        ',&
'          mentioned under c), you can distribute it without the source code   ',&
'          for the changes if the following three conditions are met:          ',&
'          - The license that applies to the changes permits you to distribute ',&
'            the changes to the Vim maintainer without fee or restriction, and ',&
'            permits the Vim maintainer to include the changes in the official ',&
'            version of @PROJECT@ without fee or restriction.                  ',&
'          - You keep the changes for at least three years after last          ',&
'            distributing the corresponding modified @PROJECT@.  When the      ',&
'            maintainer or someone who you distributed the modified @PROJECT@  ',&
'            to asks you (in any way) for the changes within this period, you  ',&
'            must make them available to him.                                  ',&
'          - You clearly describe in the distribution how to contact you.  This',&
'            contact information must remain valid for at least three years    ',&
'            after last distributing the corresponding modified @PROJECT@, or  ',&
'            as long as possible.                                              ',&
'       e) When the GNU General Public License (GPL) applies to the changes,   ',&
'          you can distribute the modified @PROJECT@ under the GNU GPL version ',&
'          2 or any later version.                                             ',&
'    3) A message must be added, at least in the output of the ":version"      ',&
'       command and in the intro screen, such that the user of the modified    ',&
'       @PROJECT@ is able to see that it was modified.  When distributing as   ',&
'       mentioned under 2)e) adding the message is only required for as far as ',&
'       this does not conflict with the license used for the changes.          ',&
'    4) The contact information as required under 2)a) and 2)d) must not be    ',&
'       removed or changed, except that the person himself can make            ',&
'       corrections.                                                           ',&
'                                                                              ',&
'III) If you distribute a modified version of @PROJECT@, you are encouraged to ',&
'     use the Vim license for your changes and make them available to the      ',&
'     maintainer, including the source code.  The preferred way to do this is  ',&
'     by e-mail or by uploading the files to a server and e-mailing the URL. If',&
'     the number of changes is small (e.g., a modified Makefile) e-mailing a   ',&
'     context diff will do.  The e-mail address to be used is                  ',&
'     <maintainer@vim.org>                                                     ',&
'                                                                              ',&
'IV)  It is not allowed to remove this license from the distribution of the    ',&
'     @PROJECT@ sources, parts of it or from a modified version.  You may use  ',&
'     this license for previous @PROJECT@ releases instead of the license that ',&
'     they came with, at your option.                                          ',&
'                                                                              ',&
'                                                                              ',&
'                                                                              ',&
'']
if(present(topic_only))then
   if(size(textblock)>0)then
      if(topic_only) textblock=[character(len=len(textblock(1))) :: textblock(1)]
   else
      textblock=['']
   endif
endif
case('46','wtfpl')
textblock=[ CHARACTER(LEN=128) :: &
'wtfpl',&
'     ',&
'Do What The F*ck You Want To Public License',&
'                                           ',&
'   The easiest license out there. It gives the user permissions to do',&
'   whatever they want with your code.                                ',&
'      Permissions    Conditions Limitations                          ',&
'     * Commercial use                                                ',&
'     * Distribution                                                  ',&
'     * Modification                                                  ',&
'     * Private use                                                   ',&
'                                                                     ',&
'            DO WHAT THE FUCK YOU WANT TO PUBLIC LICENSE              ',&
'                    Version 2, December 2004                         ',&
'                                                                     ',&
' Copyright (C) 2004 Sam Hocevar <sam@hocevar.net>                    ',&
'                                                                     ',&
' Everyone is permitted to copy and distribute verbatim or modified   ',&
' copies of this license document, and changing it is allowed as long ',&
' as the name is changed.                                             ',&
'                                                                     ',&
'            DO WHAT THE FUCK YOU WANT TO PUBLIC LICENSE              ',&
'   TERMS AND CONDITIONS FOR COPYING, DISTRIBUTION AND MODIFICATION   ',&
'                                                                     ',&
'  0. You just DO WHAT THE FUCK YOU WANT TO.                          ',&
'                                                                     ',&
'']
if(present(topic_only))then
   if(size(textblock)>0)then
      if(topic_only) textblock=[character(len=len(textblock(1))) :: textblock(1)]
   else
      textblock=['']
   endif
endif
case('47','zlib')
textblock=[ CHARACTER(LEN=128) :: &
'zlib',&
'    ',&
'zlib License',&
'            ',&
'(C) @YEAR@ @FULLNAME@',&
'                     ',&
'This software is provided ''as-is'', without any express or implied',&
'warranty.  In no event will the authors be held liable for any damages',&
'arising from the use of this software.                                ',&
'                                                                      ',&
'Permission is granted to anyone to use this software for any purpose, ',&
'including commercial applications, and to alter it and redistribute it',&
'freely, subject to the following restrictions:                        ',&
'                                                                      ',&
'1. The origin of this software must not be misrepresented; you must not',&
'   claim that you wrote the original software. If you use this software',&
'   in a product, an acknowledgment in the product documentation would be',&
'   appreciated but is not required.                                     ',&
'2. Altered source versions must be plainly marked as such, and must not be',&
'   misrepresented as being the original software.                         ',&
'3. This notice may not be removed or altered from any source distribution.',&
'                                                                          ',&
'   [3]Copy license text to clipboard                                      ',&
'                                                                          ',&
'']
if(present(topic_only))then
   if(size(textblock)>0)then
      if(topic_only) textblock=[character(len=len(textblock(1))) :: textblock(1)]
   else
      textblock=['']
   endif
endif
case default
textblock=[character(len=20) :: 'UNKNOWN']
end select
end function show_one
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
subroutine replaceit()
   character(len=:),allocatable :: line
   integer :: i, j, k
   do i = 1, size(textblock)
      do j=1,size(configblock)
         k=index(configblock(j),'=>')
         if(k.ne.0)then
            line = replace(textblock(i),configblock(j)(1:k-1) , trim(configblock(j)(k+2:)))
            ! ignorecase  whether to ignore ASCII case or not. Defaults to .false. .
            ! ierr        error code. iF ier = -1 bad directive, >= 0 then count of changes made.
            if(len(line).gt.len(textblock))then
               textblock=[character(len=max(len(line),len(textblock))):: textblock] ! widen file is neccessary
            endif
            textblock(i) = line
         endif
      enddo
   end do
end subroutine replaceit
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
subroutine codeit()
integer :: width
integer :: i
integer :: G_iout
integer :: ilength
character(len=:), allocatable :: buff
character(len=:), allocatable :: subname
   width = 0
   G_iout = stdout
   do i = min(istart, size(textblock)), size(textblock)
      width = max(width, len_trim(textblock(i)))
   end do
   subname=trim(textblock(1))
   if(subname.ne.'')then
      subname=replace(subname,'-','_')
      subname=transliterate(subname,' !"#$%&''()*+,-./:;<=>?@[\]^`{|}~','')
      subname='_'//subname
   endif
   write (G_iout, '(a)') 'subroutine license'//subname//'()'
   write (G_iout, '(a)') 'integer :: i'
   write (G_iout, '(a)') 'character(len=:),allocatable :: license_text(:)'
   write (G_iout, '(a,i0,a)') 'license_text'//'=[ character(len=', width, ') :: &'
   do i = min(istart, size(textblock)), size(textblock)
      buff = trim(textblock(i))
      buff = replace(buff,"'","''")         ! change single quotes in input to two adjacent single quotes
      write (G_iout, '("''",a,"'',&")') stretch(buff, width)
   end do
   write (G_iout, '(a)') "'']"
   write (G_iout, '(a)') "write(*,'(a)') (trim(license_text(i)), i=1, size(license_text))"
   write (G_iout, '(a)') "end subroutine license"//subname
end subroutine codeit
!===================================================================================================================================
!()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()()=
!===================================================================================================================================
subroutine setup()
help=[ CHARACTER(LEN=128) :: &
'NAME',&
'   fpm-license(1f) - [FPM:LICENSES] write out common licenses as text or subroutines',&
'   (LICENSE:MIT)                                                                    ',&
'                                                                                    ',&
'SYNOPSIS                                                                            ',&
'    fpm-license [spdx_tagname(s)][--all][--fortran][--verbose][--config FILENAME]   ',&
'                                                                                    ',&
'    fpm-license --file FILENAME [--config FILENAME]                                 ',&
'                                                                                    ',&
'    fpm-license --help|--version                                                    ',&
'                                                                                    ',&
'DESCRIPTION                                                                         ',&
'   fpm-license(1) can generate common license files as plain text and as a Fortran  ',&
'   subroutine, with a mechanism for replacing string macros in the license.         ',&
'                                                                                    ',&
'     + fpm projects often require or preferably contain a LICENSE.txt file.         ',&
'                                                                                    ',&
'     + Some programs want to contain a subroutine that can be called to display     ',&
'       the license.                                                                 ',&
'                                                                                    ',&
'     + Typically the output requires editing to insert specific names, dates        ',&
'       and organizations as appropriate. A configuration file can be created        ',&
'       to allow for automatic string substitutions. There are pre-defined           ',&
'       macros for the current date.                                                 ',&
'                                                                                    ',&
'OPTIONS                                                                             ',&
'   SPDX tagnames(s)    SPDX tagname of license to display. All supported            ',&
'                       tag names are displayed by default.                          ',&
'    --fortran          format license as a Fortran subroutine                       ',&
'    --all              display all supported license text                           ',&
'    --file FILENAME    ignore other options and format file as a                    ',&
'                       Fortran subroutine. FILENAME defaults to stdin.              ',&
'    --config=FILENAME  File of form LHS=>RHS that is used to replace                ',&
'                       strings in the output. Built-in strings are                  ',&
'                                                                                    ',&
'                         @YEAR@=>YYYY # where YYYY is current year                  ',&
'                         @MONTH@=>MM  # where MM is current month                   ',&
'                         @DAY@=>DD    # where DD is current day                     ',&
'                                                                                    ',&
'                       Strings are case-sensitive and all spaces are                ',&
'                       significant except trailing space on the RHS.                ',&
'                       Strings requiring editing often appear delimited             ',&
'                       by an at sign (@), such as "@FULLNAME@". Lines               ',&
'                       not containing "=>" are ignored.                             ',&
'                                                                                    ',&
'                       The value of the environment variable                        ',&
'                       FPM_LICENSE_CONFIG is used as the default.                   ',&
'    --verbose,-V  verbose mode                                                      ',&
'    --version,-v  Print version information on standard output then                 ',&
'                  exit successfully.                                                ',&
'    --help,-h     Print usage information on standard output then                   ',&
'                  exit successfully.                                                ',&
'                                                                                    ',&
'SPX NAMES                                                                           ',&
'                                                                                    ',&
'   Currently available license descriptions include                                 ',&
'                                                                                    ',&
'      ------------------------------------------      ----------------              ',&
'      License                                         License keyword               ',&
'      ------------------------------------------      ----------------              ',&
'      Academic Free License v3.0                      AFL-3.0                       ',&
'      Apache license 2.0                              Apache-2.0                    ',&
'      Artistic license 2.0                            Artistic-2.0                  ',&
'      Boost Software License 1.0                      BSL-1.0                       ',&
'      BSD 2-clause "Simplified" license               BSD-2-Clause                  ',&
'      BSD 3-clause "New" or "Revised" license         BSD-3-Clause                  ',&
'      BSD 3-clause Clear license                      BSD-3-Clause-Clear            ',&
'      BSD 4-clause "Original" or "Old" license        BSD-4-Clause                  ',&
'      BSD Zero-Clause license                         0BSD                          ',&
'      Creative Commons license family                 CC                            ',&
'      Creative Commons Zero v1.0 Universal            CC0-1.0                       ',&
'      Creative Commons Attribution 4.0                CC-BY-4.0                     ',&
'      Creative Commons Attribution ShareAlike 4.0     CC-BY-SA-4.0                  ',&
'      Do What The F*ck You Want To Public License     WTFPL                         ',&
'      Educational Community License v2.0              ECL-2.0                       ',&
'      Eclipse Public License 1.0                      EPL-1.0                       ',&
'      Eclipse Public License 2.0                      EPL-2.0                       ',&
'      European Union Public License 1.1               EUPL-1.1                      ',&
'      GNU Affero General Public License v3.0          AGPL-3.0                      ',&
'      GNU General Public License family               GPL                           ',&
'      GNU General Public License v2.0                 GPL-2.0                       ',&
'      GNU General Public License v3.0                 GPL-3.0                       ',&
'      GNU Lesser General Public License family        LGPL                          ',&
'      GNU Lesser General Public License v2.1          LGPL-2.1                      ',&
'      GNU Lesser General Public License v3.0          LGPL-3.0                      ',&
'      Internet Software Consortium                    ISC                           ',&
'      LaTeX Project Public License v1.3c              LPPL-1.3c                     ',&
'      Microsoft Public License                        MS-PL                         ',&
'      Massachusetts Institute of Technology           MIT                           ',&
'      Mozilla Public License 2.0                      MPL-2.0                       ',&
'      Open Software License 3.0                       OSL-3.0                       ',&
'      PostgreSQL License                              PostgreSQL                    ',&
'      SIL Open Font License 1.1                       OFL-1.1                       ',&
'      University of Illinois/NCSA Open Source License NCSA                          ',&
'      The Unlicense                                   Unlicense                     ',&
'      zLib License                                    Zlib                          ',&
'                                                                                    ',&
'EXAMPLES                                                                            ',&
'                                                                                    ',&
'Sample commands:                                                                    ',&
'                                                                                    ',&
'    fpm-license  # display available license names                                  ',&
'    fpm-license mit > LICENSE.txt # create a specific license file                  ',&
'    fpm-license --fortran mit # write license as Fortran code                       ',&
'                                                                                    ',&
'    fpm-license --all  # display all supported license descriptions                 ',&
'    fpm-license --help # display this help text                                     ',&
'                                                                                    ',&
'    # write a license file using specified string substitutions                     ',&
'    # from configuration file                                                       ',&
'    cat > $HOME/.local/config/FPM_LICENSE_CONFIG <<\EOF                             ',&
'    @FULLNAME@=>John S. Urban                                                       ',&
'    [name of copyright holder]=> John S. Urban                                      ',&
'    [name of copyright owner]=> John S. Urban                                       ',&
'    EOF                                                                             ',&
'    fpm-license cc0-1.0 --config $HOME/.local/config/FPM_LICENSE_CONFIG             ',&
'                                                                                    ',&
'SEE ALSO                                                                            ',&
'                                                                                    ',&
'    + Wikipedia contains descriptions of many license descriptions                  ',&
'    + https://spdx.dev/learn/areas-of-interest/licensing                            ',&
'    + https://spdx.org/licenses/                                                    ',&
'    + https://docs.github.com/en/repositories/...                                   ',&
'    managing-your-repositorys-settings-and-features/...                             ',&
'    customizing-your-repository/licensing-a-repository                              ',&
'    + https://opensource.guide/legal/...                                            ',&
'    #which-open-source-license-is-appropriate-for-my-project                        ',&
'']
version=[ CHARACTER(LEN=128) :: &
'PRODUCT:        GPF (General Purpose Fortran) utilities and examples',&
'PROGRAM:        fpm-license(1f)                                     ',&
'DESCRIPTION:    fpm(1) create license files                         ',&
'VERSION:        1.0, 2024-05-17                                     ',&
'AUTHOR:         John S. Urban                                       ',&
'LICENSE:        MIT                                                 ',&
'']
end subroutine setup
end program license
