/* this code is licensed as public domain */
#include <stdio.h>
#include "draw.h"

static  Mstack  *msfree = (Mstack *)NULL;
/******************************************************************************/
#ident "@(#)M_DRAW:copyvector - Copy the 4 vector b to a."
void draw_copyvector(register Vector a, register Vector b){
        a[0] = b[0];
        a[1] = b[1];
        a[2] = b[2];
        a[3] = b[3];
}
/******************************************************************************/
#ident "@(#)M_DRAW:copymatrix - Copy the  4 x 4 matrix b to the 4 x 4 matrix a"
void draw_copymatrix(register Matrix a, register Matrix b){
        register int    i;
        register float  *pa, *pb;

        pa = (float *)a;
        pb = (float *)b;
        for(i = 0; i < 16; i++)
                *(pa++) = *(pb++);
}
/******************************************************************************/
#ident "@(#)M_DRAW:copytranspose - copy the transpose of the 4 x 4 matrix b to the 4 x 4 matrix a."
void draw_copytranspose(register Matrix a, register Matrix b){
        register int    i, j;

        for(i = 0; i < 4; i++)
                for(j = 0; j < 4; j++)
                        a[i][j] = b[j][i];
}
/******************************************************************************/
#ident "@(#)M_DRAW:getmatrix - Retrieve the top matrix on the stack and place it in m"
void draw_getmatrix(Matrix m){
        draw_copymatrix(m, vdevice.transmat->m);
}
/******************************************************************************/
#ident "@(#)M_DRAW:pushmatrix - Push the top matrix of the stack down, placing a copy of it on the top of the stack."
void draw_pushmatrix(void){
        Mstack  *tmpmat;
        Token   *p;

        if (vdevice.inobject) {
                p = draw_newtokens(1);

                p->i = OBJ_PUSHMATRIX;

                return;
        }

        if (msfree != (Mstack *)NULL) {
                tmpmat = vdevice.transmat;
                vdevice.transmat = msfree;
                msfree = msfree->back;
                vdevice.transmat->back = tmpmat;
                draw_copymatrix(vdevice.transmat->m, tmpmat->m);
        } else {
                tmpmat = (Mstack *)draw_vallocate(sizeof(Mstack),"from pushmatrix");
                tmpmat->back = vdevice.transmat;
                draw_copymatrix(tmpmat->m, vdevice.transmat->m);
                vdevice.transmat = tmpmat;
        }
}
/******************************************************************************/
#ident "@(#)M_DRAW:popmatrix - Pop the top matrix from the stack."
void draw_popmatrix(void){
        Token   *p;
        Mstack  *oldtop;

        if (vdevice.inobject) {
                p = draw_newtokens(1);

                p->i = OBJ_POPMATRIX;

                return;
        }

        if (vdevice.transmat->back == (Mstack *)NULL)
                draw_verror("popmatrix: matrix stack empty");
        else {
                oldtop = vdevice.transmat;
                vdevice.transmat = vdevice.transmat->back;
                oldtop->back = msfree;
                msfree = oldtop;
        }
        /* do we need to be freeing anything? */
        /*
                draw_vfree(xxx,"from popmatrix");
        */

        /* may have changed mapping from world to device coords */
        vdevice.cpVvalid = 0;
}
/******************************************************************************/
#ident "@(#)M_DRAW:loadmatrix - load matrix"
void draw_loadmatrix(Matrix mat){
   register int i;
   register float *cm, *mp;
   Token    *p;

   if (!vdevice.initialized)
      draw_verror("loadmatrix: draw not initialized");

   if (vdevice.inobject) {
      p = draw_newtokens(17);

      p[0].i = OBJ_LOADMATRIX;
      cm = (float *)mat;
      for (i = 0; i < 16; i++)
         (++p)->f = *cm++;

      return;
   }

   cm = (float *)vdevice.transmat->m;
   mp = (float *)mat;
   for (i = 0; i < 16; i++){

      *cm++ = *mp++;
    }

   /* may have changed mapping from world to device coords */
   vdevice.cpVvalid = 0;
}
/******************************************************************************/
#ident "@(#)M_DRAW:mult4x4 - multiply 4 x 4 matrices b and c assigning them into a."
/*
 * Readers are reminded that speed can be important here.
 *
 */
void draw_mult4x4(register Matrix a, register Matrix b, register Matrix c){
a[0][0]= b[0][0] *c[0][0] +b[0][1] *c[1][0] +b[0][2] *c[2][0] +b[0][3] *c[3][0];
a[0][1]= b[0][0] *c[0][1] +b[0][1] *c[1][1] +b[0][2] *c[2][1] +b[0][3] *c[3][1];
a[0][2]= b[0][0] *c[0][2] +b[0][1] *c[1][2] +b[0][2] *c[2][2] +b[0][3] *c[3][2];
a[0][3]= b[0][0] *c[0][3] +b[0][1] *c[1][3] +b[0][2] *c[2][3] +b[0][3] *c[3][3];

a[1][0]= b[1][0] *c[0][0] +b[1][1] *c[1][0] +b[1][2] *c[2][0] +b[1][3] *c[3][0];
a[1][1]= b[1][0] *c[0][1] +b[1][1] *c[1][1] +b[1][2] *c[2][1] +b[1][3] *c[3][1];
a[1][2]= b[1][0] *c[0][2] +b[1][1] *c[1][2] +b[1][2] *c[2][2] +b[1][3] *c[3][2];
a[1][3]= b[1][0] *c[0][3] +b[1][1] *c[1][3] +b[1][2] *c[2][3] +b[1][3] *c[3][3];

a[2][0]= b[2][0] *c[0][0] +b[2][1] *c[1][0] +b[2][2] *c[2][0] +b[2][3] *c[3][0];
a[2][1]= b[2][0] *c[0][1] +b[2][1] *c[1][1] +b[2][2] *c[2][1] +b[2][3] *c[3][1];
a[2][2]= b[2][0] *c[0][2] +b[2][1] *c[1][2] +b[2][2] *c[2][2] +b[2][3] *c[3][2];
a[2][3]= b[2][0] *c[0][3] +b[2][1] *c[1][3] +b[2][2] *c[2][3] +b[2][3] *c[3][3];

a[3][0]= b[3][0] *c[0][0] +b[3][1] *c[1][0] +b[3][2] *c[2][0] +b[3][3] *c[3][0];
a[3][1]= b[3][0] *c[0][1] +b[3][1] *c[1][1] +b[3][2] *c[2][1] +b[3][3] *c[3][1];
a[3][2]= b[3][0] *c[0][2] +b[3][1] *c[1][2] +b[3][2] *c[2][2] +b[3][3] *c[3][2];
a[3][3]= b[3][0] *c[0][3] +b[3][1] *c[1][3] +b[3][2] *c[2][3] +b[3][3] *c[3][3];
}

/******************************************************************************/
#ident "@(#)M_DRAW:multmatrix - Premultiply the top matrix on the stack by 'mat'"
void draw_multmatrix(Matrix mat){
        Matrix  prod;
        float   *m;
        Token   *p;
        int     i;

        if (vdevice.inobject) {
                p = draw_newtokens(17);

                p[0].i = OBJ_MULTMATRIX;
                m = (float *)mat;
                for (i = 0; i < 16; i++)
                        (++p)->f = *m++;

                return;
        }

        draw_mult4x4(prod, mat, vdevice.transmat->m);
        draw_loadmatrix(prod);
}

/******************************************************************************/
#ident "@(#)M_DRAW:identmatrix - Return a 4 x 4 identity matrix"
void draw_identmatrix(Matrix a){
        register float  *p;

        for (p = (float *)a; p != (float *)a + 16; p++)
                *p = 0;

        a[0][0] = a[1][1] = a[2][2] = a[3][3] = 1;
}

/******************************************************************************/
#ident "@(#)M_DRAW:multvector - Multiply the vector a and the matrix b to form v. Need it to be snappy again."
void draw_multvector(register Vector v, register Vector a, register Matrix b){
   v[0] = a[0] * b[0][0] + a[1] * b[1][0] + a[2] * b[2][0] + a[3] * b[3][0];
   v[1] = a[0] * b[0][1] + a[1] * b[1][1] + a[2] * b[2][1] + a[3] * b[3][1];
   v[2] = a[0] * b[0][2] + a[1] * b[1][2] + a[2] * b[2][2] + a[3] * b[3][2];
   v[3] = a[0] * b[0][3] + a[1] * b[1][3] + a[2] * b[2][3] + a[3] * b[3][3];
}

/******************************************************************************/
#ident "@(#)M_DRAW:premultvector - PreMultiply the vector a and the matrix b to form v.  Need it to be snappy again."
void draw_premultvector(Vector v, Vector a, Matrix b){
   v[0] = a[0] * b[0][0] + a[1] * b[0][1] + a[2] * b[0][2] + a[3] * b[0][3];
   v[1] = a[0] * b[1][0] + a[1] * b[1][1] + a[2] * b[1][2] + a[3] * b[1][3];
   v[2] = a[0] * b[2][0] + a[1] * b[2][1] + a[2] * b[2][2] + a[3] * b[2][3];
   v[3] = a[0] * b[3][0] + a[1] * b[3][1] + a[2] * b[3][2] + a[3] * b[3][3];
}

/******************************************************************************/
#ifdef DEBUG

#ident "@(#)M_DRAW:printmat - print s and then dump matrix m."
/*
 * Useful for debugging you get sick of typing in the print loop otherwise."
 */
void draw_printmat(char *s, Matrix m){
        int     i, j;

        printf("%s\n", s);
        for (i = 0; i < 4; i++) {
                for (j = 0; j < 4; j++)
                        printf("%f ",m[i][j]);
                printf("\n");
        }
}
/******************************************************************************/
#ident "@(#)M_DRAW:printvect - print vector"
void draw_printvect(char *s, Vector v){
        printf("%s %f %f %f %f\n", s, v[0], v[1], v[2], v[3]);
}
/******************************************************************************/
#endif
/******************************************************************************/
