/* this code is licensed as public domain */
#include <stdlib.h>
#include <stdio.h>
#include "draw.h"

static TokList  *current;
/******************************************************************************/
#ident "@(#)M_DRAW:newtokens - returns the space for num tokens"
Token * draw_newtokens(int num){
   TokList  *tl;
   Token *addr;
   int   size;

   if (vdevice.tokens == (TokList *)NULL || num >= MAXTOKS - current->count) {
      if ((tl = (TokList *)malloc(sizeof(TokList))) == (TokList *)NULL)
         draw_verror("newtokens: malloc returns NULL");

      if (vdevice.tokens != (TokList *)NULL)
         current->next = tl;
      else 
         vdevice.tokens = tl;

      tl->count = 0;
      tl->next = (TokList *)NULL;
      if (num > MAXTOKS)
         size = num;
      else
         size = MAXTOKS;
      if ((tl->toks = (Token *)malloc(size * sizeof(Token))) == (Token *)NULL)
         draw_verror("newtokens: malloc returns NULL");

      current = tl;
   }

   addr = &current->toks[current->count];
   current->count += num;

   return(addr);
}
/******************************************************************************/
