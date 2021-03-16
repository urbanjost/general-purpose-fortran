#include <stdio.h>
#include <stdlib.h>
#include <string.h>
void jubiglet(char *chars){
char ident[] = "@(#)jubiglet(3c): Make big block letters";
/*******************************************************************************
   Write a left-justified string as large block letters. For use as:
    o A banner page for output delivery
    o An eye-readable title on a piece of microfiche.
    o A large warning message in an output stream

    Should
    o Allow a prefix and suffix string
    o Eliminate strncpy after everyone is ANSI and use static strings
    o Make option to print all non-spaces as a single specified character
    o Make option to print 'inverse' characters

   John S. Urban, 19940225
*******************************************************************************/
#define ROW 97
static int _aini = 1;
static char list[ROW];
static char alf[ROW*100];
static int ilets, ip2, irows, ipos ;
int shift, lchars ;
if( _aini ){
strncpy(list,"ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456@$%&*()-_=+\\][>.<,?/!;'\":#^789 abcdefghijklmnopqrstuvwxyz`|~{}",ROW);
shift= 0;
strncpy(&alf[shift+0*10*ROW]," AAAAAAAA BBBBBBBBB  CCCCCCCC DDDDDDDDD EEEEEEEEEE",50);
strncpy(&alf[shift+1*10*ROW],"AAAAAAAAAABBBBBBBBBBCCCCCCCCCCDDDDDDDDDDEEEEEEEEEE",50);
strncpy(&alf[shift+2*10*ROW],"AA      AABB      BBCC       CDD      DDEE        ",50);
strncpy(&alf[shift+3*10*ROW],"AA      AABB      BBCC        DD      DDEE        ",50);
strncpy(&alf[shift+4*10*ROW],"AA      AABBBBBBBBB CC        DD      DDEEEEE     ",50);
strncpy(&alf[shift+5*10*ROW],"AAAAAAAAAABBBBBBBBB CC        DD      DDEEEEE     ",50);
strncpy(&alf[shift+6*10*ROW],"AAAAAAAAAABB      BBCC        DD      DDEE        ",50);
strncpy(&alf[shift+7*10*ROW],"AA      AABB      BBCC       CDD      DDEE        ",50);
strncpy(&alf[shift+8*10*ROW],"AA      AABBBBBBBBBBCCCCCCCCCCDDDDDDDDDDEEEEEEEEEE",50);
strncpy(&alf[shift+9*10*ROW],"AA      AABBBBBBBBB  CCCCCCCC DDDDDDDDD EEEEEEEEEE",50);

shift=shift+50;
strncpy(&alf[shift+0*10*ROW],"FFFFFFFFFF GGGGGGGG HH      HHIIIIIIIIII  JJJJJJJJ",50);
strncpy(&alf[shift+1*10*ROW],"FFFFFFFFFFGGGGGGGGGGHH      HHIIIIIIIIII  JJJJJJJJ",50);
strncpy(&alf[shift+2*10*ROW],"FF        GG        HH      HH    II         JJ   ",50);
strncpy(&alf[shift+3*10*ROW],"FF        GG        HH      HH    II         JJ   ",50);
strncpy(&alf[shift+4*10*ROW],"FFFFF     GG   GGGGGHHHHHHHHHH    II         JJ   ",50);
strncpy(&alf[shift+5*10*ROW],"FFFFF     GG   GGGGGHHHHHHHHHH    II         JJ   ",50);
strncpy(&alf[shift+6*10*ROW],"FF        GG      GGHH      HH    II    JJ   JJ   ",50);
strncpy(&alf[shift+7*10*ROW],"FF        GG      GGHH      HH    II    JJ   JJ   ",50);
strncpy(&alf[shift+8*10*ROW],"FF        GGGGGGGGGGHH      HHIIIIIIIIIIJJJJJJJ   ",50);
strncpy(&alf[shift+9*10*ROW],"FF         GGGGGGGGGHH      HHIIIIIIIIII JJJJJ    ",50);

shift=shift+50;
strncpy(&alf[shift+0*10*ROW],"KK      KKLL        MM      MMNN      NN OOOOOOOO ",50);
strncpy(&alf[shift+1*10*ROW],"KK     KK LL        MMMM  MMMMNNN     NNOOOOOOOOOO",50);
strncpy(&alf[shift+2*10*ROW],"KK   KK   LL        MM MMMM MMNNNN    NNOO     OOO",50);
strncpy(&alf[shift+3*10*ROW],"KK KK     LL        MM  MM  MMNN NN   NNOO    O OO",50);
strncpy(&alf[shift+4*10*ROW],"KKKKK     LL        MM  MM  MMNN  NN  NNOO   O  OO",50);
strncpy(&alf[shift+5*10*ROW],"KK  KK    LL        MM      MMNN  NN  NNOO  O   OO",50);
strncpy(&alf[shift+6*10*ROW],"KK   KK   LL        MM      MMNN   NN NNOO O    OO",50);
strncpy(&alf[shift+7*10*ROW],"KK    KK  LL        MM      MMNN    NNNNOOO     OO",50);
strncpy(&alf[shift+8*10*ROW],"KK     KK LLLLLLLLLLMM      MMNN     NNNOOOOOOOOOO",50);
strncpy(&alf[shift+9*10*ROW],"KK      KKLLLLLLLLLLMM      MMNN      NN OOOOOOOO ",50);

shift=shift+50;
strncpy(&alf[shift+0*10*ROW],"PPPPPPPPP  QQQQQQQQ RRRRRRRRR  SSSSSSSS TTTTTTTTTT",50);
strncpy(&alf[shift+1*10*ROW],"PPPPPPPPPPQQQQQQQQQQRRRRRRRRRRSSSSSSSSSSTTTTTTTTTT",50);
strncpy(&alf[shift+2*10*ROW],"PP      PPQQ      QQRR      RRSS       S    TT    ",50);
strncpy(&alf[shift+3*10*ROW],"PP      PPQQ      QQRR      RRSS            TT    ",50);
strncpy(&alf[shift+4*10*ROW],"PPPPPPPPPPQQ      QQRRRRRRRRRRSSSSSSSSS     TT    ",50);
strncpy(&alf[shift+5*10*ROW],"PPPPPPPPP QQ      QQRRRRRRRRR  SSSSSSSSS    TT    ",50);
strncpy(&alf[shift+6*10*ROW],"PP        QQ   QQ QQRR   RR           SS    TT    ",50);
strncpy(&alf[shift+7*10*ROW],"PP        QQ    QQQQRR    RR  S       SS    TT    ",50);
strncpy(&alf[shift+8*10*ROW],"PP        QQQQQQQQQ RR     RR SSSSSSSSSS    TT    ",50);
strncpy(&alf[shift+9*10*ROW],"PP         QQQQQQ QQRR      RR SSSSSSSS     TT    ",50);

shift=shift+50;
strncpy(&alf[shift+0*10*ROW],"UU      UUVV      VVWW      WWXX      XXYY      YY",50);
strncpy(&alf[shift+1*10*ROW],"UU      UUVV      VVWW      WW XX    XX  YY    YY ",50);
strncpy(&alf[shift+2*10*ROW],"UU      UU VV    VV WW      WW  XX  XX    YY  YY  ",50);
strncpy(&alf[shift+3*10*ROW],"UU      UU VV    VV WW      WW   XXXX      YYYY   ",50);
strncpy(&alf[shift+4*10*ROW],"UU      UU VV    VV WW  WW  WW    XX        YY    ",50);
strncpy(&alf[shift+5*10*ROW],"UU      UU VV    VV WW  WW  WW   XXXX       YY    ",50);
strncpy(&alf[shift+6*10*ROW],"UU      UU  VV  VV  WW  WW  WW  XX  XX      YY    ",50);
strncpy(&alf[shift+7*10*ROW],"UU      UU  VV  VV  WW WWWW WW XX    XX     YY    ",50);
strncpy(&alf[shift+8*10*ROW],"UUUUUUUUUU   VVVV    WWW  WWW XX      XX    YY    ",50);
strncpy(&alf[shift+9*10*ROW]," UUUUUUUU     VV     WW    WW XX      XX    YY    ",50);

shift=shift+50;
strncpy(&alf[shift+0*10*ROW],"ZZZZZZZZZZ   0000       11     22222222 3333333333",50);
strncpy(&alf[shift+1*10*ROW],"ZZZZZZZZZ   000000    1111    2222222222333333333 ",50);
strncpy(&alf[shift+2*10*ROW],"      ZZ   00    00  11 11    2       22      33  ",50);
strncpy(&alf[shift+3*10*ROW],"     ZZ   00      00    11            22     33   ",50);
strncpy(&alf[shift+4*10*ROW],"    ZZ    00      00    11           222    333   ",50);
strncpy(&alf[shift+5*10*ROW],"    ZZ    00      00    11         222        333 ",50);
strncpy(&alf[shift+6*10*ROW],"   ZZ     00      00    11       222           33 ",50);
strncpy(&alf[shift+7*10*ROW],"  ZZ       00    00     11      222     3       33",50);
strncpy(&alf[shift+8*10*ROW]," ZZZZZZZZZ  000000  111111111122222222223333333333",50);
strncpy(&alf[shift+9*10*ROW],"ZZZZZZZZZZ   0000   11111111112222222222 33333333 ",50);

shift=shift+50;
strncpy(&alf[shift+0*10*ROW],"     444  5555555555 66666666               $$    ",50);
strncpy(&alf[shift+1*10*ROW],"    4444  55555555556666666666  @@@@@@   $$$$$$$$ ",50);
strncpy(&alf[shift+2*10*ROW],"   44 44  55        66       6 @@   @@@ $$$$$$$$$$",50);
strncpy(&alf[shift+3*10*ROW],"  44  44  55        66               @@ $$  $$    ",50);
strncpy(&alf[shift+4*10*ROW]," 44   44  555555555 666666666    @@@@@@ $$$$$$$$$ ",50);
strncpy(&alf[shift+5*10*ROW],"444444444455555555556666666666  @@   @@  $$$$$$$$$",50);
strncpy(&alf[shift+6*10*ROW],"4444444444        5566      66 @@    @@     $$  $$",50);
strncpy(&alf[shift+7*10*ROW],"      44  5       5566      66 @@    @@ $$$$$$$$$$",50);
strncpy(&alf[shift+8*10*ROW],"      44  55555555556666666666  @@@@@@   $$$$$$$$ ",50);
strncpy(&alf[shift+9*10*ROW],"      44   55555555  66666666               $$    ",50);

shift=shift+50;
strncpy(&alf[shift+0*10*ROW]," %%%    %%  &&&&                     (    )       ",50);
strncpy(&alf[shift+1*10*ROW],"%% %%  %%  &&  &&                  ((      ))     ",50);
strncpy(&alf[shift+2*10*ROW],"%% %% %%   &&  &&    *  *  *      ((        ))    ",50);
strncpy(&alf[shift+3*10*ROW]," %%% %%     && &&     * * *      ((          ))   ",50);
strncpy(&alf[shift+4*10*ROW],"    %%       &&&       ***       ((          ))   ",50);
strncpy(&alf[shift+5*10*ROW],"   %%       && &&   *********    ((          ))   ",50);
strncpy(&alf[shift+6*10*ROW],"  %%  %%%  &&   && &   ***       ((          ))   ",50);
strncpy(&alf[shift+7*10*ROW]," %%  %% %%&&     &&&  * * *       ((        ))    ",50);
strncpy(&alf[shift+8*10*ROW],"%%   %% %%&&     &&  *  *  *       ((      ))     ",50);
strncpy(&alf[shift+9*10*ROW],"%     %%%  &&&&&& &&                 (    )       ",50);

shift=shift+50;
strncpy(&alf[shift+0*10*ROW],"                                        \\          ",50);
strncpy(&alf[shift+1*10*ROW],"                                 ++++   \\\\        ",50);
strncpy(&alf[shift+2*10*ROW],"                                 ++++    \\\\       ",50);
strncpy(&alf[shift+3*10*ROW],"                    ==========   ++++     \\\\      ",50);
strncpy(&alf[shift+4*10*ROW],"----------          ==========++++++++++   \\\\     ",50);
strncpy(&alf[shift+5*10*ROW],"----------                    ++++++++++    \\\\    ",50);
strncpy(&alf[shift+6*10*ROW],"                    ==========   ++++        \\\\   ",50);
strncpy(&alf[shift+7*10*ROW],"                    ==========   ++++         \\\\  ",50);
strncpy(&alf[shift+8*10*ROW],"          __________             ++++          \\\\ ",50);
strncpy(&alf[shift+9*10*ROW],"          __________                            \\\\",50);

shift=shift+50;
strncpy(&alf[shift+0*10*ROW],"    ]]]]]    [[[[[                                ",50);
strncpy(&alf[shift+1*10*ROW],"       ]]    [[     >>                          <<",50);
strncpy(&alf[shift+2*10*ROW],"       ]]    [[       >>                      <<  ",50);
strncpy(&alf[shift+3*10*ROW],"       ]]    [[         >>                  <<    ",50);
strncpy(&alf[shift+4*10*ROW],"       ]]    [[           >>              <<      ",50);
strncpy(&alf[shift+5*10*ROW],"       ]]    [[             >>          <<        ",50);
strncpy(&alf[shift+6*10*ROW],"       ]]    [[           >>              <<      ",50);
strncpy(&alf[shift+7*10*ROW],"       ]]    [[         >>       ...        <<    ",50);
strncpy(&alf[shift+8*10*ROW],"       ]]    [[       >>        .....         <<  ",50);
strncpy(&alf[shift+9*10*ROW],"    ]]]]]    [[[[[  >>           ...            <<",50);

shift=shift+50;
strncpy(&alf[shift+0*10*ROW],"            ?????            /    !!              ",50);
strncpy(&alf[shift+1*10*ROW],"           ???????          //    !!        ;;;   ",50);
strncpy(&alf[shift+2*10*ROW],"          ??     ??        //     !!       ;;;;;  ",50);
strncpy(&alf[shift+3*10*ROW],"                 ??       //      !!        ;;;   ",50);
strncpy(&alf[shift+4*10*ROW],"                ??       //       !!              ",50);
strncpy(&alf[shift+5*10*ROW],"   ,,,         ??       //        !!        ;;;   ",50);
strncpy(&alf[shift+6*10*ROW],"  ,,,,,      ??        //         !!       ;;;;;  ",50);
strncpy(&alf[shift+7*10*ROW],"   ,,,,      ??       //          !!        ;;;;  ",50);
strncpy(&alf[shift+8*10*ROW],"     ,               //                        ;  ",50);
strncpy(&alf[shift+9*10*ROW],"    ,        ??      /            !!          ;   ",50);

shift=shift+50;
strncpy(&alf[shift+0*10*ROW],"    ''''  \"\"  \"\"                                  ",50);
strncpy(&alf[shift+1*10*ROW],"    ''''  \"\"  \"\"      :::       ##  ##      ^^    ",50);
strncpy(&alf[shift+2*10*ROW],"    ''''  \"\"  \"\"     :::::      ##  ##     ^^^^   ",50);
strncpy(&alf[shift+3*10*ROW],"          \"\"  \"\"      :::     ##########  ^^  ^^  ",50);
strncpy(&alf[shift+4*10*ROW],"                                ##  ##   ^^    ^^ ",50);
strncpy(&alf[shift+5*10*ROW],"                      :::       ##  ##  ^^      ^^",50);
strncpy(&alf[shift+6*10*ROW],"                     :::::    ##########          ",50);
strncpy(&alf[shift+7*10*ROW],"                      :::       ##  ##            ",50);
strncpy(&alf[shift+8*10*ROW],"                                ##  ##            ",50);
strncpy(&alf[shift+9*10*ROW],"                                                  ",50);

shift=shift+50;
strncpy(&alf[shift+0*10*ROW],"7777777777 88888888  99999999                     ",50);
strncpy(&alf[shift+1*10*ROW],"777777777788888888889999999999                    ",50);
strncpy(&alf[shift+2*10*ROW],"       77 88      8899      99                    ",50);
strncpy(&alf[shift+3*10*ROW],"      77  88      8899      99                    ",50);
strncpy(&alf[shift+4*10*ROW],"     77    88888888 9999999999                    ",50);
strncpy(&alf[shift+5*10*ROW],"    77     88888888  999999999             aaaa   ",50);
strncpy(&alf[shift+6*10*ROW],"   77     88      88        99                 a  ",50);
strncpy(&alf[shift+7*10*ROW],"  77      88      88        99            aaaaaa  ",50);
strncpy(&alf[shift+8*10*ROW]," 77       88888888889999999999           a     a  ",50);
strncpy(&alf[shift+9*10*ROW],"77         88888888  99999999             aaaaa a ",50);

shift=shift+50;
strncpy(&alf[shift+0*10*ROW]," b                          d                ff   ",50);
strncpy(&alf[shift+1*10*ROW]," b                          d               f  f  ",50);
strncpy(&alf[shift+2*10*ROW]," b                          d               f     ",50);
strncpy(&alf[shift+3*10*ROW]," b                          d             fffff   ",50);
strncpy(&alf[shift+4*10*ROW]," b                          d               f     ",50);
strncpy(&alf[shift+5*10*ROW]," bbbbbbb    cccccc    ddddddd   eeeeee      f     ",50);
strncpy(&alf[shift+6*10*ROW]," b      b  c         d      d  e      e     f     ",50);
strncpy(&alf[shift+7*10*ROW]," b      b  c         d      d  eeeeeee      f     ",50);
strncpy(&alf[shift+8*10*ROW]," b      b  c         d      d  e            f     ",50);
strncpy(&alf[shift+9*10*ROW]," bbbbbbb    cccccc    ddddddd   eeeeee      f     ",50);

shift=shift+50;
strncpy(&alf[shift+0*10*ROW],"           h                                      ",50);
strncpy(&alf[shift+1*10*ROW],"           h                             k        ",50);
strncpy(&alf[shift+2*10*ROW],"           h                      j      k        ",50);
strncpy(&alf[shift+3*10*ROW],"  ggggg    h            i                k        ",50);
strncpy(&alf[shift+4*10*ROW]," g     g   h                      j      k   k    ",50);
strncpy(&alf[shift+5*10*ROW]," g     g   hhhhhhh      i         j      k  k     ",50);
strncpy(&alf[shift+6*10*ROW],"  gggggg   h      h     i         j      kkk      ",50);
strncpy(&alf[shift+7*10*ROW],"       g   h      h     i         j      k  k     ",50);
strncpy(&alf[shift+8*10*ROW],"       g   h      h     i      j  j      k   k    ",50);
strncpy(&alf[shift+9*10*ROW],"   gggg    h      h     i       jj       k    k   ",50);

shift=shift+50;
strncpy(&alf[shift+0*10*ROW],"   ll                                             ",50);
strncpy(&alf[shift+1*10*ROW],"    l                                             ",50);
strncpy(&alf[shift+2*10*ROW],"    l                                             ",50);
strncpy(&alf[shift+3*10*ROW],"    l                                     ppppp   ",50);
strncpy(&alf[shift+4*10*ROW],"    l                                    p     p  ",50);
strncpy(&alf[shift+5*10*ROW],"    l      mmmm mmm  n nnnnn    oooooo   p     p  ",50);
strncpy(&alf[shift+6*10*ROW],"    l      m   m   m nn     n  o      o  pppppp   ",50);
strncpy(&alf[shift+7*10*ROW],"    l      m   m   m n      n  o      o  p        ",50);
strncpy(&alf[shift+8*10*ROW],"    l      m   m   m n      n  o      o  p        ",50);
strncpy(&alf[shift+9*10*ROW],"    l      m   m   m n      n   oooooo   p        ",50);

shift=shift+50;
strncpy(&alf[shift+0*10*ROW],"                                                  ",50);
strncpy(&alf[shift+1*10*ROW],"                                                  ",50);
strncpy(&alf[shift+2*10*ROW],"                                  t               ",50);
strncpy(&alf[shift+3*10*ROW],"  qqqqq                           t               ",50);
strncpy(&alf[shift+4*10*ROW]," q     q                       ttttttt            ",50);
strncpy(&alf[shift+5*10*ROW]," q     q   r rrrrrr   ssssss      t      u      u ",50);
strncpy(&alf[shift+6*10*ROW],"  qqqqqq   rr        s            t      u      u ",50);
strncpy(&alf[shift+7*10*ROW],"       q   r          ssssss      t      u      u ",50);
strncpy(&alf[shift+8*10*ROW],"       q   r                s     t      u      u ",50);
strncpy(&alf[shift+9*10*ROW],"       q   r          ssssss       ttt    uuuuuu u",50);

shift=shift+50;
strncpy(&alf[shift+0*10*ROW],"                                                  ",50);
strncpy(&alf[shift+1*10*ROW],"                                                  ",50);
strncpy(&alf[shift+2*10*ROW],"                                                  ",50);
strncpy(&alf[shift+3*10*ROW],"                               y     y            ",50);
strncpy(&alf[shift+4*10*ROW],"                                y   y             ",50);
strncpy(&alf[shift+5*10*ROW]," v       v w       w xx   xx     y y      zzzzzz  ",50);
strncpy(&alf[shift+6*10*ROW],"  v     v  w   w   w  xx xx       y          zz   ",50);
strncpy(&alf[shift+7*10*ROW],"   v   v   w   w   w    x        y          zz    ",50);
strncpy(&alf[shift+8*10*ROW],"    v v    w   w   w  xx xx     y          zz     ",50);
strncpy(&alf[shift+9*10*ROW],"     v      www www  xx   xx   y          zzzzzz  ",50);

shift=shift+50;
strncpy(&alf[shift+0*10*ROW],"````         |||                    {{{  }}}      ",50);
strncpy(&alf[shift+1*10*ROW],"````         |||      ~~~  ~~      {{      }}     ",50);
strncpy(&alf[shift+2*10*ROW]," ````        |||     ~~~~~ ~~      {{      }}     ",50);
strncpy(&alf[shift+3*10*ROW],"  ````       |||     ~~~ ~~~~     {{        }}    ",50);
strncpy(&alf[shift+4*10*ROW],"             |||     ~~  ~~     {{{          }}}  ",50);
strncpy(&alf[shift+5*10*ROW],"             |||                  {{        }}    ",50);
strncpy(&alf[shift+6*10*ROW],"             |||                  {{        }}    ",50);
strncpy(&alf[shift+7*10*ROW],"             |||                   {{      }}     ",50);
strncpy(&alf[shift+8*10*ROW],"             |||                   {{      }}     ",50);
strncpy(&alf[shift+9*10*ROW],"             |||                    {{{  }}}      ",50);

        _aini = 0;
}
   for( irows=0; irows < 10; irows++){   /* put out 10 horizontal rows */
      lchars = (int)strlen(chars);
      for( ilets=0; ilets < lchars; ilets++ ){
         /* much more efficient if find the number once outside */
         for ( ipos=0; ipos < ROW ; ipos ++ ){
            if ( chars[ilets] == list[ipos] ){
               ip2=ipos*10+10*ROW*irows;
               printf("%10.10s ",&alf[ip2]);
            }
         }
         /* ipos=64;
            fprintf(stderr,"this character not supported:%s\n",chars[ilets-1]);
         */
      }
      putchar('\n');
   }
}

/*  @(#) jubiglet_(char *s, int len) // Fortran 77 callable (sometimes) version of jubiglet */
#define BUFSIZE 256
void jubiglet_(char *s, int len){
        char            buf[BUFSIZ];
        register char   *p;

        strncpy(buf, s, len);
        buf[len] = 0;

        for (p = &buf[len - 1]; *p == ' '; p--)
                ;

        *++p = 0;

        jubiglet(buf);
}
