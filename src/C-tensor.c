/* this code is licensed as public domain */
#include "draw.h"
/******************************************************************************/
#ident "@(#)M_DRAW:premulttensor - Premultiply the tensor b by the matrix a and place the result into the tensor c."
void draw_premulttensor(Tensor c, Matrix a,Tensor  b) {
	register	int	i, j, k;
	float		x1, x2, x3, x4;

	for (i = 0; i < 4; i++)
		for (j = 0; j < 4; j++) {
			x1 = x2 = x3 = x4 = 0.0;
			for (k = 0; k < 4; k++) {
				x1 += a[i][k] * b[0][k][j];
				x2 += a[i][k] * b[1][k][j];
				x3 += a[i][k] * b[2][k][j];
				x4 += a[i][k] * b[3][k][j];
			}
			c[0][i][j] = x1;
			c[1][i][j] = x2;
			c[2][i][j] = x3;
			c[3][i][j] = x4;
		}
}
/******************************************************************************/
#ident "@(#)M_DRAW:multtensor - Multiply the tensor b by the matrix a and place the result into the tensor c."
void draw_multtensor(Tensor c, Matrix a,Tensor  b) {
	register	int	i, j, k;
	float		x1, x2, x3, x4;

	for (i = 0; i < 4; i++)
		for (j = 0; j < 4; j++) {
			x1 = x2 = x3 = x4 = 0.0;
			for (k = 0; k < 4; k++) {
				x1 += b[0][i][k] * a[k][j];
				x2 += b[1][i][k] * a[k][j];
				x3 += b[2][i][k] * a[k][j];
				x4 += b[3][i][k] * a[k][j];
			}
			c[0][i][j] = x1;
			c[1][i][j] = x2;
			c[2][i][j] = x3;
			c[3][i][j] = x4;
		}
}
/******************************************************************************/
#ident "@(#)M_DRAW:copytensor - Copy the tensor a into b."
void draw_copytensor(Tensor b, Tensor a) {
	register	int	i, j, k;
	
	for (i = 0; i < 4; i++)
		for (j = 0; j < 4; j++)
			for (k = 0; k < 4; k++)
				b[i][j][k] = a[i][j][k];
}
/******************************************************************************/
#ident "@(#)M_DRAW:copytensortrans - Copy the tensor a into b 'transposed'."
void draw_copytensortrans(Tensor b,Tensor  a) {
	register	int	i, j, k;

	for (i = 0; i < 4; i++)
		for (j = 0; j < 4; j++)
			for (k = 0; k < 4; k++)
				b[i][j][k] = a[i][k][j];
}
/******************************************************************************/
