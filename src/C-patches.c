/* this code is licensed as public domain */
#include "draw.h"

static int	ntsegs, tsegs = 10, nusegs, usegs = 10, 
		ntcurves = 10, nucurves = 10, 
		ntiter = 1, nuiter = 1,
		makeprec_called = 0;

static Matrix	tbasis, ubasis, et, eu,
		ones =	{
				{1.0, 1.0, 1.0, 1.0},
				{1.0, 1.0, 1.0, 1.0},
				{1.0, 1.0, 1.0, 1.0},
				{1.0, 1.0, 1.0, 1.0}
			};
/******************************************************************************/
void		draw_premulttensor(Tensor c, Matrix a, Tensor b),
 		draw_multtensor(Tensor c, Matrix a, Tensor b), 
		draw_copytensor(Tensor b, Tensor a),
		draw_copytensortrans(Tensor b, Tensor a),
		draw_drpatch(Tensor R, int ntcurves, int nucurves, int ntsegs, int nusegs, int ntiter, int nuiter),
		draw_drcurve(int n, Matrix r), 
		draw_rpatch(Matrix geomx, Matrix geomy, Matrix geomz, Matrix geomw),
		draw_transformtensor(Tensor S, Matrix m);

static	void	draw_makeprec(void), 
		draw_replace(Tensor a, Matrix b, int k),
		draw_iterate(Tensor R, int n),
		draw_extract(Matrix b, Tensor a, int intk);

static	float	draw_addemup(Matrix m);
/******************************************************************************/
#ident "@(#)M_DRAW:patchbasis - Specify the two basis matrices for a patch"
void draw_patchbasis(Matrix tb, Matrix ub) {
        if(!vdevice.initialized)
                draw_verror("patchbasis: draw not initialized");

        draw_copymatrix(tbasis, tb);
	draw_copytranspose(ubasis, ub);
}
/******************************************************************************/
#ident "@(#)M_DRAW:patchprecision - Specify the lower limit on the number of line segments used to draw a curve as part of a patch."
/*
 * The actual number used varies
 * with the number of curve segments in the "other" direction.
 */
void draw_patchprecision(int tseg, int useg){
        if (!vdevice.initialized)
                draw_verror("patchprecision: draw not initialized");

        if(tseg > 0 && useg > 0) {
                tsegs = tseg;
                usegs = useg;
        } else
                draw_verror("patchprecision: number of segments <= 0");
	/*
	 * Set up the difference matrices
	 */
	draw_makeprec();
}
/******************************************************************************/
#ident "@(#)M_DRAW:patchcurves - Specify the number of curves to be drawn in each direction on a patch."
void draw_patchcurves(int nt, int nu){

        if (!vdevice.initialized)
                draw_verror("patchcurves: draw not initialized");

        if(nt > 0 && nu > 0) {
                ntcurves = nt;
                nucurves = nu;
        } else
                draw_verror("patchcurves: number of patch curves <= 0");

	/*
	 * Set up the difference matrices
	 */
	draw_makeprec();
}
/******************************************************************************/
#ident "@(#)M_DRAW:makeprec - Makes up the two precision matrices for a patch"
static	void draw_makeprec(void){
	float	n2, n3;

	/*
	 * Find ntsegs, nusegs, ntiter, nuiter....
	 * ie. the actual number of curve segments of a tcurve,
	 *     the actual number of curve segments of a ucurve,
	 *     and the number of times to iterate in each direction.
	 */

	ntsegs = tsegs;
	ntiter = ntsegs / (nucurves - 1);
	if (ntsegs > ntiter * (nucurves - 1)) 
		ntsegs = (++ntiter) * (nucurves - 1);

	nusegs = usegs;
	nuiter = nusegs / (ntcurves - 1);
	if (nusegs > nuiter * (ntcurves - 1)) 
		nusegs = (++nuiter) * (ntcurves - 1);

	/*
	 * Doing the t precision matrix.....
	 */
	draw_identmatrix(et);
	n2 = (float)(ntsegs * ntsegs);
	n3 = (float)(ntsegs * n2);

	et[0][0] = et[2][2] = et[3][3] = 0.0;
	et[1][0] = 1.0 / n3;
	et[1][1] = 1.0 / n2;
	et[2][0] = et[3][0] = 6.0 / n3;
	et[2][1] = 2.0 / n2;
	et[1][2] = 1.0 / (float)ntsegs;
	et[0][3] = 1.0;

	/*
	 * Make the Transpose of eu
	 */
	draw_identmatrix(eu);
	n2 = (float)(nusegs * nusegs);
	n3 = (float)(nusegs * n2);

	eu[0][0] = eu[2][2] = eu[3][3] = 0.0;
	eu[0][1] = 1.0 / n3;
	eu[1][1] = 1.0 / n2;
	eu[0][2] = eu[0][3] = 6.0 / n3;
	eu[1][2] = 2.0 / n2;
	eu[2][1] = 1.0 / (float)nusegs;
	eu[3][0] = 1.0;

	makeprec_called = 1;
}
/******************************************************************************/
#ident "@(#)M_DRAW:patch - Draws a bicubic patch. (ie. all the w coords a 1 and the basis matrices don't change that)"
void draw_patch(Matrix geomx, Matrix geomy, Matrix geomz){
	draw_rpatch(geomx, geomy, geomz, ones);
}
/******************************************************************************/
#ident "@(#)M_DRAW:rpatch - Draws rational bicubic patches."
/*
 *	Reference: J. H. Clark, Parametric Curves, Surfaces and volumes in 
 *	computer graphics and computer aided Geometric Design.
 *	Technical report No. 221, Nov 1981.
 *	Computer Systems Lab. Dept's of Electrical Eng. and Computer Science,
 *	Standford University, Standford, California 94305.
 */
void draw_rpatch(Matrix geomx, Matrix geomy, Matrix geomz, Matrix geomw){

	Tensor	S, R;
        Matrix	tmp, tmp2;
	float	xlast, ylast, zlast;
        int	i, j, sync;
	Token	*tok;

        if (!vdevice.initialized)
                draw_verror("patch: draw not initialized");

	/* 
	 *  Form S = et . tbasis . Gtensor . ubasisT . euT
	 */

	if (!makeprec_called)
		draw_makeprec();

	draw_mult4x4(tmp, et, tbasis);
	draw_mult4x4(tmp2, ubasis, eu);

	/*
	 * Load the geometry matrices into S.
	 */
	for (i = 0; i < 4; i++)
		for (j = 0; j < 4; j++) {
			S[0][i][j] = geomx[i][j];
			S[1][i][j] = geomy[i][j];
			S[2][i][j] = geomz[i][j];
			S[3][i][j] = geomw[i][j];
		}

	draw_premulttensor(R, tbasis, S);
	draw_multtensor(S, ubasis, R);

	/*
	 * Find the last point on the curve.
	 */
	xlast = draw_addemup(S[0]);
	ylast = draw_addemup(S[1]);
	zlast = draw_addemup(S[2]);

	/*
 	 * Multiply the precision matrices in.
	 */
	draw_premulttensor(R, et, S);
	draw_multtensor(S, eu, R);

	if (vdevice.inobject) {
		tok = draw_newtokens(74);
		tok[0].i = OBJ_RPATCH;
		tok[1].f = xlast;
		tok[2].f = ylast;
		tok[3].f = zlast;
		tok[4].i = ntcurves;
		tok[5].i = nucurves;
		tok[6].i = ntsegs;
		tok[7].i = nusegs;
		tok[8].i = ntiter;
		tok[9].i = nuiter;

		tok += 10;
		for (i = 0; i < 4; i++)
			for (j = 0; j < 4; j++) {
				(tok++)->f = S[0][i][j];
				(tok++)->f = S[1][i][j];
				(tok++)->f = S[2][i][j];
				(tok++)->f = S[3][i][j];
			}

		return;
	}

	/*
	 * Multiply by the current transformation....
	 */
	draw_transformtensor(S, vdevice.transmat->m);

	/*
	 * Draw the patch....
	 */
	if ((sync = vdevice.sync)){
		vdevice.sync = 0;
        }

	draw_drpatch(S, ntcurves, nucurves, ntsegs, nusegs, ntiter, nuiter);

	if (sync) {
		vdevice.sync = 1;
		(*vdevice.dev.Vsync)();
	}

	/*
	 * Set the current (untransformed) world spot....
	 */
	vdevice.cpW[V_X] = xlast;
	vdevice.cpW[V_Y] = ylast;
	vdevice.cpW[V_Z] = zlast;
}
/******************************************************************************/
#ident "@(#)M_DRAW:transformtensor - Transform the tensor S by the matrix m"
void draw_transformtensor(Tensor S, Matrix m){
	Matrix	tmp, tmp2;
	register	int	i;

	for (i = 0; i < 4; i++) {
		draw_extract(tmp, S, i);
		draw_mult4x4(tmp2, tmp, m);
		draw_replace(S, tmp2, i);
	}
}
/******************************************************************************/
#ident "@(#)M_DRAW:replace - Does the reverse of extract."
static	void draw_replace(Tensor a, Matrix b, int k){
	int	i, j;

	/*
	 * Not unwound because it only gets called once per patch.
	 */
	for (i = 0; i < 4; i++)
		for (j = 0; j < 4; j++)
			a[j][i][k] = b[i][j];
}
/******************************************************************************/
#ident "@(#)M_DRAW:drpatch - Actually does the work of drawing a patch."
void draw_drpatch(Tensor R, int ntcurves, int nucurves, int ntsegs, int nusegs, int ntiter, int nuiter){
	Tensor	S;
	Matrix	tmp;
	int	i;

	/*
         *  Copy R transposed into S
         */
	draw_copytensortrans(S, R);

	for (i = 0; i < ntcurves; i++) {
		draw_extract(tmp, R, 0);
		draw_drcurve(ntsegs, tmp);
		draw_iterate(R, nuiter);
	}

	/*
	 * Now using S...
	 */
	for (i = 0; i < nucurves; i++) {
		draw_extract(tmp, S, 0);
		draw_drcurve(nusegs, tmp);
		draw_iterate(S, ntiter);
	}
}
/******************************************************************************/
#ident "@(#)M_DRAW:iterate - Iterates the forward difference tensor R"
static	void draw_iterate(Tensor R, int n){
	register int	it;

/*
 * Anyone for an unwound loop or two???
 *
 *	for (it = 0; it < n; it++) {
 *		for (i = 0; i < 4; i++) 
 *			for (j = 0; j < 4; j++)
 *				for (k = 0; k < 3; k++)
 *					R[i][j][k] += R[i][j][k+1];
 *	}
 */
	for (it = 0; it < n; it++) {
		R[0][0][0] += R[0][0][1];
		R[0][0][1] += R[0][0][2];
		R[0][0][2] += R[0][0][3];

		R[0][1][0] += R[0][1][1];
		R[0][1][1] += R[0][1][2];
		R[0][1][2] += R[0][1][3];

		R[0][2][0] += R[0][2][1];
		R[0][2][1] += R[0][2][2];
		R[0][2][2] += R[0][2][3];

		R[0][3][0] += R[0][3][1];
		R[0][3][1] += R[0][3][2];
		R[0][3][2] += R[0][3][3];

		R[1][0][0] += R[1][0][1];
		R[1][0][1] += R[1][0][2];
		R[1][0][2] += R[1][0][3];

		R[1][1][0] += R[1][1][1];
		R[1][1][1] += R[1][1][2];
		R[1][1][2] += R[1][1][3];

		R[1][2][0] += R[1][2][1];
		R[1][2][1] += R[1][2][2];
		R[1][2][2] += R[1][2][3];

		R[1][3][0] += R[1][3][1];
		R[1][3][1] += R[1][3][2];
		R[1][3][2] += R[1][3][3];

		R[2][0][0] += R[2][0][1];
		R[2][0][1] += R[2][0][2];
		R[2][0][2] += R[2][0][3];

		R[2][1][0] += R[2][1][1];
		R[2][1][1] += R[2][1][2];
		R[2][1][2] += R[2][1][3];

		R[2][2][0] += R[2][2][1];
		R[2][2][1] += R[2][2][2];
		R[2][2][2] += R[2][2][3];

		R[2][3][0] += R[2][3][1];
		R[2][3][1] += R[2][3][2];
		R[2][3][2] += R[2][3][3];

		R[3][0][0] += R[3][0][1];
		R[3][0][1] += R[3][0][2];
		R[3][0][2] += R[3][0][3];

		R[3][1][0] += R[3][1][1];
		R[3][1][1] += R[3][1][2];
		R[3][1][2] += R[3][1][3];

		R[3][2][0] += R[3][2][1];
		R[3][2][1] += R[3][2][2];
		R[3][2][2] += R[3][2][3];

		R[3][3][0] += R[3][3][1];
		R[3][3][1] += R[3][3][2];
		R[3][3][2] += R[3][3][3];
	}
}
/******************************************************************************/
#ident "@(#)M_DRAW:extract - Extract the k'th column of the tensor a into the matrix b."
static	void draw_extract(Matrix b, Tensor a, int k){
	b[0][0] = a[0][0][k];
	b[0][1] = a[1][0][k];
	b[0][2] = a[2][0][k];
	b[0][3] = a[3][0][k];

	b[1][0] = a[0][1][k];
	b[1][1] = a[1][1][k];
	b[1][2] = a[2][1][k];
	b[1][3] = a[3][1][k];

	b[2][0] = a[0][2][k];
	b[2][1] = a[1][2][k];
	b[2][2] = a[2][2][k];
	b[2][3] = a[3][2][k];

	b[3][0] = a[0][3][k];
	b[3][1] = a[1][3][k];
	b[3][2] = a[2][3][k];
	b[3][3] = a[3][3][k];
}

/******************************************************************************/
#ident "@(#)M_DRAW:addemup"
static	float draw_addemup(Matrix m){
	return (m[0][0] + m[0][1] + m[0][2] + m[0][3] +
		m[1][0] + m[1][1] + m[1][2] + m[1][3] +
		m[2][0] + m[2][1] + m[2][2] + m[2][3] +
		m[3][0] + m[3][1] + m[3][2] + m[3][3]);
}
/******************************************************************************/
