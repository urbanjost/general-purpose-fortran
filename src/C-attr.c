/* this code is licensed as public domain */
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include "draw.h"

static   Astack   *asfree = (Astack *)NULL;
/******************************************************************************/
static void draw_copyattributes(Attribute *a, Attribute *b){
#ident "@(#)M_DRAW:copyattributes - copy everything except back pointer from attribute b to a"

   if (b->style != (char *)NULL) {
      if (a->style != (char *)NULL){
         draw_vfree(a->style,"from copyattributes");
      }

      a->style = (char *)draw_vallocate(strlen(b->style) + 1,"from copyattributes");
      strcpy(a->style, b->style);
   }else{
	 a->style=(char *)NULL;
   }

   a->dashp        = b->dashp;
   a->dash         = b->dash;
   a->adist        = b->adist;
   a->color        = b->color;
   a->fill         = b->fill;
   a->hatch        = b->hatch;
   a->inbackbuffer = b->inbackbuffer;
   a->textcos      = b->textcos;
   a->textsin      = b->textsin;
   a->hatchcos     = b->hatchcos;
   a->hatchsin     = b->hatchsin;
   a->hatchpitch   = b->hatchpitch;
   a->justify      = b->justify;
   a->skew         = b->skew;
   a->bold         = b->bold;
   a->fixedwidth   = b->fixedwidth;
   a->fontwidth    = b->fontwidth;
   a->fontheight   = b->fontheight;
   a->softtext     = b->softtext;
   a->exvp         = b->exvp;
   strcpy(a->font, b->font);
}
/******************************************************************************/
#ident "@(#)M_DRAW:printattribs - print attributes"
void draw_printattribs( char *s){
   printf("%s\n", s);
   printf("color      = %d\n", vdevice.attr->a.color);
   printf("fill       = %d\n", vdevice.attr->a.fill);
   printf("backface   = %d\n", vdevice.attr->a.backface);

   printf("hatch      = %d\n", vdevice.attr->a.hatch);
   printf("hatchcos   = %f\n", vdevice.attr->a.hatchcos);
   printf("hatchsin   = %f\n", vdevice.attr->a.hatchsin);
   printf("hatchpitch = %f\n", vdevice.attr->a.hatchpitch);

   printf("font         = %s\n", vdevice.attr->a.font);
   printf("fixedwidth   = %d\n", vdevice.attr->a.fixedwidth);
   printf("justify      = %d\n", vdevice.attr->a.justify);
   printf("skew         = %f\n", vdevice.attr->a.skew);
   printf("fontwidth    = %f\n", vdevice.attr->a.fontwidth); /* are these meaningless? */
   printf("fontheight   = %f\n", vdevice.attr->a.fontheight); /* are these meaningless? */
   printf("textcos      = %f\n", vdevice.attr->a.textcos);
   printf("textsin      = %f\n", vdevice.attr->a.textsin);

   printf("exvp         = %d\n", vdevice.attr->a.exvp);
   printf("softtext     = %d\n", vdevice.attr->a.softtext);
   printf("inbackbuffer = %d\n", vdevice.attr->a.inbackbuffer);
   printf("adist        = %f\n", vdevice.attr->a.adist);
   printf("dash         = %f\n", vdevice.attr->a.dash);
   printf("bold         = %d\n", vdevice.attr->a.bold);
   printf("dashp        = %s\n", vdevice.attr->a.dashp);

   if (vdevice.attr->a.style != (char *)NULL ) {
      printf("linestyle    = %s\n", vdevice.attr->a.style);
   }else{
      printf("linestyle    = (NULL)\n");
   }
}
/******************************************************************************/
void draw_getattribs( char   *s){
#ident "@(#)M_DRAW:getattribs - make something to return attributes so can query them"
/* assuming s long enough to get name */
/* assuming r long enough to store name */
      switch(s[0]) {  
      case 'b' :
   printf("backface   = %d\n", vdevice.attr->a.backface);
      break;
      case 'c' :
   printf("color      = %d\n", vdevice.attr->a.color);
      break;
      case 'e' :
   printf("exvp       = %d\n", vdevice.attr->a.exvp);
      break;
      case 'f' :
        switch(s[1]){
        case 'i' :
      printf("fixedwidth = %d\n", vdevice.attr->a.fixedwidth);
      printf("fill       = %d\n", vdevice.attr->a.fill);
   break;
        case 'o' :
      printf("font       = %s\n", vdevice.attr->a.font);
      printf("fontheight = %f\n", vdevice.attr->a.fontheight);
      printf("fontwidth  = %f\n", vdevice.attr->a.fontwidth); 
   break;
   }
      break;
      case 'h' :
   printf("hatch      = %d\n", vdevice.attr->a.hatch);
   printf("hatchcos   = %f\n", vdevice.attr->a.hatchcos);
   printf("hatchpitch = %f\n", vdevice.attr->a.hatchpitch);
   printf("hatchsin   = %f\n", vdevice.attr->a.hatchsin);
      break;
      case 'j' :
   printf("justify    = %d\n", vdevice.attr->a.justify);
      break;
      case 's' :
   printf("skew       = %f\n", vdevice.attr->a.skew);
   printf("softtext   = %d\n", vdevice.attr->a.softtext);
      break;
      case 't' :
   printf("textcos    = %f\n", vdevice.attr->a.textcos);
   printf("textsin    = %f\n", vdevice.attr->a.textsin);
      break;
      default  : printf("unknown string=%s",s);
      }
}
/******************************************************************************/
#ident "@(#)M_DRAW:printlogicals - print logical device attributes"
void draw_printlogicals( char   *s) {
   printf("%s\n", s);
   printf("initialized     = %d\n", vdevice.initialized);
   printf("writestoprocess = %d\n", vdevice.writestoprocess);
   printf("clipoff         = %d\n", vdevice.clipoff);
   printf("inobject        = %d\n", vdevice.inobject);
   printf("inpolygon       = %d\n", vdevice.inpolygon);
   printf("upset           = %d\n", vdevice.upset);
   printf("cpVvalid        = %d\n", vdevice.cpVvalid);
   printf("sync            = %d\n", vdevice.sync);
   printf("clipplanes      = %d\n", vdevice.clipplanes);
}
/******************************************************************************/
#ident "@(#)M_DRAW:printvdevice - print logical and other attributes"
void draw_printvdevice( char *s){
   printf("%s\n", s);
   draw_printlogicals("logicals:\n");
   draw_printattribs("attributes:\n");
}
/******************************************************************************/
#ident "@(#)M_DRAW:pushattributes - save the current attributes on the matrix stack"
void draw_pushattributes(void) {
   Astack   *nattr;
   Token *p;

   if (!vdevice.initialized)
      draw_verror("pushattributes:  draw not initialized");
   
   if (vdevice.inobject) {
      p = draw_newtokens(1);
      p[0].i = OBJ_PUSHATTRIBUTES;
      return;
   }
   /*
      fprintf(stderr,"*pushattributes* %s %f %f\n",vdevice.attr->a.font,vdevice.attr->a.fontwidth,vdevice.attr->a.fontheight);
      */
   if (asfree != (Astack *)NULL) {
      nattr = vdevice.attr;
      vdevice.attr = asfree;
      asfree = asfree->back;
      vdevice.attr->back = nattr;
      draw_copyattributes(&vdevice.attr->a, &nattr->a);
   } else { 
      nattr = (Astack *)draw_vallocate(sizeof(Astack),"from pushattributes");   /* create a new attribute, */
      nattr->back = vdevice.attr;                                               /* point it back to the current attribute, */
      draw_copyattributes(&nattr->a, &vdevice.attr->a);   /* copy current attribute values to the new attribute */
      vdevice.attr = nattr;                               /* and point vdevice to this new attribute */
   }

}
/******************************************************************************/
#ident "@(#)M_DRAW:popattributes - pop the top entry on the attribute stack"
void draw_popattributes(void) {
   Astack   *nattr;
   Token *p;

   if (!vdevice.initialized)
      draw_verror("popattributes: draw not initialized");

   if (vdevice.inobject) {
      p = draw_newtokens(1);
      p[0].i = OBJ_POPATTRIBUTES;
      return;
   }

   if (vdevice.attr->back == (Astack *)NULL){
      fprintf(stderr,"popattributes: attribute stack is empty");
      return;
   }else{

      nattr = vdevice.attr;
      draw_font(vdevice.attr->back->a.font);
      vdevice.attr = vdevice.attr->back;
      nattr->back = asfree;
      asfree = nattr;

      /* Must zap the contents of ones that are on the free list */

      if (nattr->a.style != (char *)NULL){
         draw_vfree(nattr->a.style,"from popattributes");
      }
      memset(&nattr->a, 0, sizeof(Attribute));
   }
  /*
   * Restore some stuff...
   */

   draw_color(vdevice.attr->a.color);

   if (vdevice.attr->a.inbackbuffer){
      draw_backbuffer();
   }

   if (vdevice.attr->a.softtext == HERSHEY ){
        draw_textsize(vdevice.attr->a.fontwidth, vdevice.attr->a.fontheight);
   }

   if (vdevice.attr->a.exvp){
      draw_expandviewport();
   }
}
/******************************************************************************/
