/*
 * A value for pseudo-void functions to return
 */
#define UNUSED -999
/*
 * default location for font library
 */


#ifndef FONTLIB
#ifdef MSWPC       /* Stupid pox head crap */
#define FONTLIB "c:\\lib\\hershey\\"
#else
#define FONTLIB "/usr/share/hershey/"
#endif
#endif

#define SOFTHARDWARE 2
#define REALHARDWARE 0
#define HERSHEY  1

#define V_TRUE            1 
#define V_FALSE           0
/*
 * standard colour indices
 */
#define V_BLACK           0
#define V_RED             1
#define V_GREEN           2
#define V_YELLOW          3
#define V_BLUE            4
#define V_MAGENTA         5
#define V_CYAN            6
#define V_WHITE           7

#define BLACK           0
#define RED             1
#define GREEN           2
#define YELLOW          3
#define BLUE            4
#define MAGENTA         5
#define CYAN            6
#define WHITE           7

/* Hershey text justification */
#define V_XCENTERED     1
#define V_YCENTERED     2
#define V_LEFT          4       /* The default */
#define V_RIGHT         8
#define V_TOP           16
#define V_BOTTOM        32      /* The default */
/*
 * Text weights
 */
#define V_NORMAL          0       /* The default */
#define V_BOLD            1

/*
 * Line thickness
 */
#define V_THIN            0
#define V_THICK           1

/*
 * when (if ever) we need the precision
 */
#ifdef DOUBLE
#define float   double
#endif

/*
 * How to convert degrees to radians
 */
#ifndef PI
/*
#define PI      3.14159265358979323844
#define PI      3.14159265358979
*/
#define PI      3.14159265358979323846264338327950288419716939937510
#endif
#define D2R     (PI / 180.0)

/*
 * miscellaneous typedefs and type defines
 */
typedef float   Vector[4];
typedef float   Matrix[4][4];
typedef float   Tensor[4][4][4];

/*
 * when register variables get us into trouble
 */
#ifdef NOREGISTER
#define register
#endif

/*
 * max number of vertices in a polygon
 */
#define MAXVERTS        99999

/*
 * max number of characters in a font name
 */
#define FONTNAMELEN     256

/*
 * object definitions
 */
#define MAXENTS         10100             /* size of object table */
#define MAXTOKS         1000             /* num. of tokens alloc-ed at once in an object  */

/*
 * functions which can appear in objects
 */
#define OBJ_ARC             1
#define OBJ_BOXTEXT         2
#define OBJ_CALLOBJ         3
#define OBJ_CENTERTEXT      4
#define OBJ_CIRCLE          5
#define OBJ_CLEAR           6
#define OBJ_COLOR           7
#define OBJ_DRAW            8
#define OBJ_DRAWCHAR        9
#define OBJ_DRAWSTR         10
#define OBJ_FIXEDWIDTH      11
#define OBJ_FONT            12
#define OBJ_HATCHANG        13
#define OBJ_HATCHPITCH      14
#define OBJ_LOADMATRIX      15
#define OBJ_MAPCOLOR        16
#define OBJ_MOVE            17
#define OBJ_MULTMATRIX      18
#define OBJ_POLY            19
#define OBJ_POLYFILL        20
#define OBJ_POLYHATCH       21
#define OBJ_POPATTRIBUTES   22
#define OBJ_POPMATRIX       23
#define OBJ_POPVIEWPORT     24
#define OBJ_PUSHATTRIBUTES  25
#define OBJ_PUSHMATRIX      26
#define OBJ_PUSHVIEWPORT    27
#define OBJ_RCURVE          28
#define OBJ_RPATCH          29
#define OBJ_SECTOR          30
#define OBJ_TEXTANG         31
#define OBJ_TEXTSIZE        32
#define OBJ_VIEWPORT        33
#define OBJ_BACKBUFFER      34
#define OBJ_FRONTBUFFER     35
#define OBJ_SWAPBUFFER      36
#define OBJ_BACKFACING      37
#define OBJ_TRANSLATE       38
#define OBJ_ROTATE          39
#define OBJ_SCALE           40
#define OBJ_VFLUSH          41
#define OBJ_RECT            42
#define OBJ_LINEWIDTH       43
#define OBJ_XCENTERTEXT     44
#define OBJ_YCENTERTEXT     45
#define OBJ_BOTTOMJUSTIFY   46
#define OBJ_TOPJUSTIFY      47
#define OBJ_TEXTJUSTIFY     48
#define OBJ_RIGHTJUSTIFY    49
#define OBJ_LEFTJUSTIFY     50
#define OBJ_TEXTSLANT       51
#define OBJ_DASHCODE        52
#define OBJ_LINESTYLE       53

/*
 * data types for object tokens
 */
typedef union tk {
        int             i;
        float           f;
} Token;

typedef struct tls {
        int             count;
        Token           *toks;
        struct tls      *next;
} TokList;

/*
 * attributes
 */
typedef struct {
        char    *style,
                *dashp;
        char    fill,
                hatch,
                backface,
                justify,
                inbackbuffer,
                exvp,
                softtext,
                fixedwidth;
        int     color;
        int     bold;
        float   fontheight;
        float   fontwidth;
        float   skew;
        float   hatchcos,
                hatchsin,
                hatchpitch;
        float   textcos,
                textsin;
        float   dash,
                adist;
        char    font[FONTNAMELEN];
} Attribute;

/*
 * viewport
 */
typedef struct vp {
        float   left;
        float   right;
        float   bottom;
        float   top;
} Viewport;

/*
 * stacks
 */
typedef struct  ms {    /* Matrix stack entries */
        Matrix          m;
        struct  ms      *back;
} Mstack;

typedef struct  as {    /* Attribute stack entries */
        Attribute       a;
        struct  as      *back;
} Astack;

typedef struct  vs {    /* Viewport stack entries */
        Viewport        v;
        struct  vs      *back;
} Vstack;

/*
 * draw device structures
 */
typedef struct dev {
        char    *devname;                                   /* name of device */
        char    *large,                                 /* name of large font */
                *small;                                 /* name of small font */
        int     (*Vbackb)(void),                /* Set drawing in back buffer */
                (*Vchar)(char),                  /* Draw a hardware character */
                (*Vcheckkey)(void),                 /* Check if a key was hit */
                (*Vclear)(void),         /* Clear the screen to current color */
                (*Vcolor)(int),                          /* Set current color */
                (*Vdraw)(int,int),                             /* Draw a line */
                (*Vexit)(void),                              /* Exit graphics */
                (*Vfill)(int, int[],int[]),                 /* Fill a polygon */
                (*Vfont)(char *),                        /* Set hardware font */
                (*Vfrontb)(void),              /* Set drawing in front buffer */
                (*Vgetkey)(void),        /* Wait for and get the next key hit */
                (*Vinit)(void),                      /* Initialise the device */
                (*Vlocator)(int*,int*),      /* Get mouse/cross hair position */
                (*Vmapcolor)(int,int,int,int),           /* Set color indices */
                (*Vsetlw)(int),                         /* set line thickness */
                (*Vstring)(char *),                 /* Draw a hardware string */
                (*Vswapb)(void),               /* Swap front and back buffers */
                (*Vsync)(void);                     /* Syncronise the display */
} DevEntry;

typedef struct vdev {
        char            initialized,
                        writestoprocess,
                        clipoff,
                        inobject,
                        inpolygon,
                        upset,                          /* is up vector set */
                        cpVvalid,                       /* is the current device position valid */
                        sync,                           /* Do we syncronise the display */
                        clipplanes;                     /* active clipping planes */
/*      FILE            *output_file;  */               /* output file */
        void            (*pmove)(float, float, float),  /* Polygon moves */
                        (*pdraw)(float, float, float);  /* Polygon draws */
        TokList         *tokens;                        /* ptr to list of tokens for current object */
        Mstack          *transmat;                      /* top of transformation stack */
        Astack          *attr;                          /* top of attribute stack */
        Vstack          *viewport;                      /* top of viewport stack */
        float           hheight, hwidth;                /* hardware character height, width */
        float           hheightd, hwidthd;              /* desired hardware character height, width */
        Vector          cpW,                            /* current position in world coords */
                        cpWtrans,                       /* current world coords transformed */
                        upvector;                       /* world up */
        int             depth,                          /* # bit planes on screen */
                        maxVx, minVx,
                        maxVy, minVy,
                        sizeX, sizeY,                   /* size of square on screen */
                        sizeSx, sizeSy,                 /* side in x, side in y (# pixels) */
                        cpVx, cpVy;
        DevEntry        dev;
} Device;

extern Device vdevice;          /* device structure */

#define V_X     0                       /* x axis in cpW */
#define V_Y     1                       /* y axis in cpW */
#define V_Z     2                       /* z axis in cpW */
#define V_W     3                       /* w axis in cpW */

/*
 * function definitions
 */
extern void *draw_vallocate(); /* malloc with error checking */
extern void draw_vfree(); /* realloc with error checking */

/*
 * arc routines
 */
extern void     draw_arcprecision(int noseg);
extern void     draw_arc(float x,float y, float radius, float startang, float endang);
extern void     draw_circle(float x,float y,float radius);
extern void     draw_circleprecision(int noseg);
extern void     draw_sector(float x,float y,float radius,float startang,float endang);

/*
 * attr routines
 */
extern void     draw_popattributes(void);
extern void     draw_pushattributes(void);
extern void     draw_printattribs( char *s);
extern void     draw_printvdevice( char *s);

/*
 * curve routines
 */
extern void     draw_curve(float geom[4][3]);
extern void     draw_rcurve(Matrix geom);
extern void     draw_curven(int n, float geom[][3]);
extern void     draw_drcurve(int n, Matrix r);
extern void     draw_curvebasis(Matrix basis);
extern void     draw_curveprecision(int nsegments);

/*
 * draw routines
 */
extern void     draw_draw(float x,float y, float z);
extern void     draw_draw2(float x,float y);
extern void     draw_rdraw(float dx,float dy,float dz);
extern void     draw_rdraw2(float dx, float dy);
extern void     draw_sdraw2(float xs, float ys);
extern void     draw_rsdraw2(float dxs, float dys);

extern void     draw_dashcode(float d);
extern void     draw_dashline(Vector p0, Vector p1);
extern void     draw_linestyle(char *l);
extern void     draw_linewidth(int w);

/*
 * device routines
 */
extern void     draw_clear(void);
extern void     draw_color(int i);
extern int      draw_getkey(void);
extern int      draw_getdepth(void);
extern int      draw_checkkey(void);
extern int      draw_getplanes();
extern int      draw_locator(float *wx,float *wy);
extern int      draw_slocator(float *wx,float *wy);
extern void     draw_mapcolor(int i, short r, short g, short b);

extern void     draw_vinit(char *device);
extern void     draw_vexit(void);
extern void     draw_verror(char *str);
extern void     draw_voutput(char *path);
extern void     draw_vnewdev(char *device);
extern char     *draw_vgetdev(char *buf);
extern void     draw_pushdev(char *device);
extern void     draw_popdev(void);

/*
 * mapping routines
 */

extern int draw_WtoVx( float p[]);
extern int draw_WtoVy( float p[]);
extern void draw_VtoWxy( float xs, float ys, float  *xw, float *yw);
extern void draw_CalcW2Vcoeffs(void);


/*
 * general matrix and vector routines
 */
extern void draw_mult4x4(register Matrix a, register Matrix b, register Matrix c);
extern void draw_copymatrix(register Matrix a, register Matrix b);
extern void draw_identmatrix(Matrix a);
extern void draw_copyvector(register Vector a, register Vector b);
extern void draw_premultvector(Vector v, Vector a, Matrix b);
extern void draw_copytranspose(register Matrix a, register Matrix b);
extern void draw_multvector(Vector v, Vector a, Matrix b);

/*
 * matrix stack routines
 */

extern void draw_getmatrix(Matrix m);
extern void draw_popmatrix(void);
extern void draw_loadmatrix(Matrix mat);
extern void draw_pushmatrix(void);
extern void draw_multmatrix(Matrix mat);

extern void draw_printmat(char *s, Matrix m);
extern void draw_printvect(char *s, Vector v);

/*
 * move routines
 */
extern void     draw_move(float x, float y, float z);
extern void     draw_move2(float x, float y);
extern void     draw_rmove(float dx, float dy, float dz);
extern void     draw_rmove2(float dx, float dy);
extern void     draw_smove2(float xs, float ys);
extern void     draw_rsmove2(float dxs, float dys);

/*
 * object routines
 */
extern int      draw_isobj(int n);
extern int      draw_genobj(void);
extern void     draw_delobj(int n);
extern void     draw_makeobj(int n);
extern void     draw_loadobj(int n, char *file);
extern void     draw_saveobj(int n, char *file);
extern void     draw_callobj(int n);
extern void     draw_closeobj(void);
extern int      draw_getopenobj(void);

extern Token    *draw_newtokens(int num);


/*
 * patch routines.
 */
extern void draw_patch(Matrix geomx, Matrix geomy, Matrix geomz);
extern void draw_rpatch(Matrix geomx, Matrix geomy, Matrix geomz, Matrix geomw);
extern void draw_drpatch(Tensor R, int ntcurves, int nucurves, int ntsegs, int nusegs, int ntiter, int nuiter);
extern void draw_patchbasis(Matrix tb, Matrix ub) ;
extern void draw_patchcurves(int nt, int nu);
extern void draw_patchprecision(int tseg, int useg);
extern void draw_transformtensor(Tensor S, Matrix m);

/*
 * point routines
 */
extern void     draw_point(float x, float y, float z);
extern void     draw_point2(float x, float y);
extern void     draw_spoint2(float xs, float ys);

/*
 * polygon routines.
 */
extern void     draw_poly(int n, float dp[][3]);
extern void     draw_poly2(int n, float dp[][2]);
extern void     draw_hatchang(float a);
extern void     draw_makepoly(void);
extern void     draw_polyfill(int onoff);
extern void     draw_closepoly(void);
extern void     draw_polyhatch(int onoff);
extern void     draw_hatchpitch(float a);
extern void     draw_backface(int onoff);
extern void     draw_backfacedir(int cdir);

extern void     draw_polyobj(int n, Token dp[]);
extern void     draw_pmove(float x, float y, float z);
extern void     draw_pdraw(float x, float y, float z);


/*
 * rectangle routine
 */
extern void     draw_rect(float x1, float y1, float x2, float y2);
extern void     draw_srect(float x1, float y1, float x2, float y2);


/*
 * tensor routines
 */

extern void     draw_premulttensor(Tensor c, Matrix a,Tensor  b) ;
extern void     draw_multtensor(Tensor c, Matrix a,Tensor  b) ;
extern void     draw_copytensor(Tensor b, Tensor a) ;
extern void     draw_copytensortrans(Tensor b,Tensor  a) ;

/*
 * text routines
 */
extern float    draw_strlength(char *s); /* Hershey SoftHardware Hardware */
extern void     draw_boxtext(float x, float y, float l, float h, char *s);
extern void     draw_boxfit(float l, float h, int nchars);
extern int      draw_numchars(void);
extern void     draw_getcharsize(char c, float *width, float *height);
extern void     draw_drawchar(int c);
extern void     draw_textsize(float width, float height);
extern float    draw_getfontwidth(void);
extern float    draw_getfontheight(void);
extern float    draw_getfontdec(void);
extern float    draw_getfontasc(void);
extern void     draw_getfontsize(float *cw, float *ch);
extern void     draw_drawstr(char *string);
extern void     draw_centertext(int onoff);
extern void     draw_fixedwidth(int onoff);
extern void     draw_textang(float ang);
extern void     draw_font(char *name);

/*
 * transformation routines
 */
extern void     draw_scale(float x, float y, float z);
extern void     draw_translate(float x, float y, float z);
extern void     draw_rotate(float r,char axis);

/*
 * window definition routines
 */
extern void     draw_ortho(float left,float right,float bottom,float top,float hither,float yon);
extern void     draw_ortho2(float left,float  right,float  bottom,float  top);
extern void     draw_lookat(float vx,float vy,float vz,float px,float py,float pz,float twist);
extern void     draw_window(float left,float right,float bottom,float top,float hither,float yon);
extern void     draw_polarview(float dist, float azim, float inc, float twist) ;
extern void     draw_perspective(float fov,float  aspect,float  hither,float  yon) ;
extern void     draw_up(float x, float y, float z) ;

/*
 * routines for manipulating the viewport
 */
extern void     draw_getviewport(float *xlow, float *xhigh, float *ylow, float *yhigh);
extern void     draw_viewport(float xlow, float xhigh, float ylow, float yhigh);

extern void     draw_popviewport(void);
extern void     draw_pushviewport(void);

/*
 * routines for retrieving the graphics position
 */
extern void     draw_getgp(float *x,float *y,float *z);
extern void     draw_getgpt(float *x,float *y,float *z, float *w);
extern void     draw_getgp2(float *x,float *y);
extern void     draw_sgetgp2(float *xs,float *ys);

/*
 * routines for retrieving the aspect details of the device
 */
extern float    draw_getaspect(void);
extern void     draw_getfactors(float *x,float *y);
extern void     draw_getdisplaysize(float *x,float *y);
extern void     draw_expandviewport(void);
extern void     draw_unexpandviewport(void);

/*
 * routines for handling the buffering
 */
extern int      draw_backbuffer(void);
extern void     draw_frontbuffer(void);
extern int      draw_swapbuffers(void);

/*
 * routines for window sizing and positioning
 */
void draw_prefposition(int x, int y);
void draw_prefsize(int x, int y);
void draw_getprefposandsize(int *x, int *y, int *xs, int *ys);


/*
 * Misc control routines
 */
extern void     draw_clipping(int onoff);
extern void     draw_vsetflush(int yn);
extern void     draw_vflush(void);
extern void     draw_clip(Vector p0, Vector p1);
extern void     draw_quickclip(Vector p0, Vector p1);

extern void draw_yobbarays(int onoff);

extern int      draw_hershfont(char *fontname);
extern int      draw_getstring(int bcol, char *s);
extern void     draw_xcentertext(void);
extern void     draw_ycentertext(void);
extern void     draw_topjustify(void);
extern void     draw_bottomjustify(void);
extern void     draw_leftjustify(void);
extern void     draw_rightjustify(void);
extern void     draw_textjustify(char val);
extern void     draw_textslant(float val);
extern void     draw_textweight(int val);

