/* this code is licensed as public domain */
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "draw.h"

#define POSTSCRIPT
#define HPGL
#define PCL5
#define DXY
#define PCL5
#define PBM
#define PPM
#define RTF
#define MIF
#define XFIG
#define VML
#define CGMT
#define PDF
#define UNIXPLOT
#define SVG
#define CANVAS
#define USEMAP
#define FTI
#define VOG
#define NIL
#define GNU

#define TEK
#define XTEK
#define X11
/*
#define XT
#define PC
#define HPGT
 */

/*
 interactive devices
*/

int _X11_draw_devcpy(void);
int _x11_draw_devcpy(void);
int _GRW_draw_devcpy(void);
int _PC_draw_devcpy(void);
int _HPGT_draw_devcpy(void);
/*
 sometimes tek goes to a file
*/
int _TEK_draw_devcpy(void);
int _XTEK_draw_devcpy(void);
int _XT_draw_devcpy(void);

/*
PostScript
*/

int _PS_draw_devcpy(void);
int _PSP_draw_devcpy(void);
int _PPSC_draw_devcpy(void);
int _PPSG_draw_devcpy(void);
int _PSC_draw_devcpy(void);
int _PSG_draw_devcpy(void);

/*
 HP formats
*/

int _PCL_PORT_draw_devcpy(void);
int _PCL_LAND_draw_devcpy(void);
int _PCL5_PORT_draw_devcpy(void);
int _PCL5_LAND_draw_devcpy(void);

int _HPGL_LAND_draw_devcpy(void);
int _HPGL_PORT_draw_devcpy(void);
int _HPGL2_LAND_draw_devcpy(void);
int _HPGL2_PORT_draw_devcpy(void);

/*
Sorta HP
*/
int _DXY_draw_devcpy(void);

/*
# metafiles to be read by something else
*/
int _CGMT_draw_devcpy(void);
int _UNIXPLOT_draw_devcpy(void);
int _VOG_draw_devcpy(void);
int _GNU_draw_devcpy(void);
int _SVG_draw_devcpy(void);
int _XFIG_draw_devcpy(void);
int _CANVAS_draw_devcpy(void);
int _USEMAP_draw_devcpy(void);

/*
 proprietary formats
*/
int _MIF_draw_devcpy(void);
int _MIF4_draw_devcpy(void);
int _VML_draw_devcpy(void);
int _FTI_draw_devcpy(void);
int _PDF_draw_devcpy(void);
int _RTF_draw_devcpy(void);

/*
Poskanzer pixmaps
*/
int _PBM_draw_devcpy(void);
int _P1_draw_devcpy(void);
int _P4_draw_devcpy(void);
int _PPM_draw_devcpy(void);
int _P3_draw_devcpy(void);
int _P6_draw_devcpy(void);

/*
Other pixmaps
*/
int _CHAR_draw_devcpy(void);
int _BM_draw_devcpy(void);
int _XBM_draw_devcpy(void);

int _NIL_draw_devcpy(void);

struct vdev     vdevice;

FILE    *draw_fp = (FILE *)NULL;

static int      allocated = 0;
static int      badcount=0;

/* device-independent function routines */

/******************************************************************************/
/* redirect output - only for postscript, hpgl, ... and other devices
 * that write output files
 */
void draw_voutput(char *path) {

/*
Open the graphics output file for most devices. Note that
   o the special file names - and + use stdout and stderr respectively
   o filenames starting with |  open and write to a process that work in pipes
   o if the pathname is blank an attempt is made to read the variable M_DRAW_OUTPUT
     but the program must call voutput explicitly.
*/
        if (strcmp(path, "-") == 0 ) {
           draw_fp=stdout;
        }else if (strcmp(path, "+") == 0 ) {
           draw_fp=stderr;
        }else if (path[0] == '|') {
           if( (draw_fp=popen(&path[1],"w")) == (FILE *)NULL){
                fprintf(stderr,
                   "voutput: could not open process [%s]; appending to M_DRAW_OUTPUT\n",path);
                if ((draw_fp = fopen("M_DRAW_OUTPUT", "a")) == (FILE *)NULL) {
                        fprintf(stderr,"voutput: could not open M_DRAW_OUTPUT\n");
                        fprintf(stderr,"voutput: using stdout. Good Luck!\n");
                        draw_fp = stdout;
                }
                return;
           }else{
             vdevice.writestoprocess=2;
           }
        }else if ((draw_fp = fopen(path, "w")) == (FILE *)NULL) {
                fprintf(stderr,
                   "voutput: could not open [%s]; appending to M_DRAW_OUTPUT\n",path);
                if ((draw_fp = fopen("M_DRAW_OUTPUT", "a")) == (FILE *)NULL) {
                        fprintf(stderr,"voutput: could not open M_DRAW_OUTPUT\n");
                        fprintf(stderr,"voutput: using stdout. Good Luck!\n");
                        draw_fp = stdout;
                }
        }
        return;
}
/******************************************************************************/
/* return a pointer to current output file - designed for internal use only.  */
FILE * _draw_outfile(void) {

        if( draw_fp == (FILE *)NULL || fileno(draw_fp) == -1 ){
           draw_fp=stdout;
           return(stdout);
        }else{
           return(draw_fp);
        }
        /*
        return(draw_fp == (FILE *)NULL ? stdout : draw_fp);
        */
}
/******************************************************************************/
/* get the appropriate device table structure */
static void draw_getdev(char *dev) {
   char    *device = "                       ";
   char    *M_DRAW_DEVICE;
   char    sdevice[100];
   char    buf[133];
   char    dtmp[133];
   int xx, yy, xxo, yyo;
   int ifound;

   /*fprintf(stderr,"INITIAL DEVICE=%s\n",dev); */
   if (dev == (char *)NULL || *dev == (char)0) {
      /* fprintf(stderr,"NULL USER INPUT\n"); */
      if ((M_DRAW_DEVICE = getenv("M_DRAW_DEVICE")) != (char *)NULL) {
         /*fprintf(stderr,"ENVIRONMENT VARIABLE IS SET\n"); */
         if(strlen(M_DRAW_DEVICE)>0){
            /*fprintf(stderr,"ENVIRONMENT VARIABLE IS NOT BLANK AS WELL\n"); */
            strncpy(dtmp,M_DRAW_DEVICE,132);
            ifound=sscanf (dtmp, "%s %d %d %d %d", sdevice, &xx, &yy, &xxo, &yyo);
         }
      }else{
         /*fprintf(stderr,"DEV IS NULL AND ENVIRONMENT VARIABLE IS BLANK OR NOT SET\n"); */
         strncpy(dtmp,dev,132);
      }

   }else{
      /*fprintf(stderr,"DEV IS NOT NULL ALTHOUGH IT MIGHT BE BLANK\n"); */
      strncpy(dtmp,dev,132);
      /*
      if (vdevice.inobject) {
         fprintf(stderr,"warning: getdev called in object\n");
      }
      */
      /* fprintf(stderr,"DTMP SHOULD HAVE READABLE COPY \n"); */
   }
   ifound=sscanf (dtmp, "%s %d %d %d %d", sdevice, &xx, &yy, &xxo, &yyo);
   device=sdevice;
   if (ifound == 2){
      draw_prefsize(xx,xx);
   }else if (ifound == 3){
      draw_prefsize(xx,yy);
   }else if (ifound == 4){
      draw_prefsize(xx,yy);
      draw_prefposition(xxo,xxo);
   }else if (ifound >= 5){
      draw_prefsize(xx,yy);
      draw_prefposition(xxo,yyo);
   }
   /*fprintf(stderr,"DECIDED DEVICE=%s\n" ,device); */
   /* fprintf(stderr,"DEVICE=%s XSIZE=%d YSIZE=%d XOFFSET=%d YOFFSET=%d\n",device,xx,yy,xxo,yyo); */

#ifdef NIL
        if (strncmp(device, "nil", 3) == 0)
                _NIL_draw_devcpy();
        else
#endif
#ifdef SUN
        if (strncmp(device, "sun", 3) == 0)
                _SUN_draw_devcpy();
        else
#endif
#ifdef TG
        /*
         * We can have one of many device names for Tk Graphic widgets, luckily
         * all tk widgets names start with a leading dot, This should keep
         * them from clashing with other devices names.
         */
        if ((*device == '.') && (_TG_draw_devcpy(device) >= 0))
                /* NULL BODY */ ;
        else
#endif
#ifdef PC
         if (strncmp(device, "PC", 2) == 0)
                 _PC_draw_devcpy();
        else
#endif
#ifdef X11
        if (strncmp(device, "X11", 3) == 0)
                _X11_draw_devcpy();
        else
        if (strncmp(device, "x11", 3) == 0)
                _x11_draw_devcpy();
        else
#endif
#ifdef DECX11
        if (strncmp(device, "decX11", 6) == 0)
                _DECX11_draw_devcpy();
        else
#endif
#ifdef RTF
        if (strncmp(device, "rtf", 3) == 0)
                _RTF_draw_devcpy();
        else
#endif
#ifdef HPGT
        if (strncmp(device, "hpgt", 4) == 0)
                _HPGT_draw_devcpy();
        else
#endif
#ifdef SVG
        if (strncmp(device, "svg", 3) == 0)
                _SVG_draw_devcpy();
        else
#endif
#ifdef XFIG
        if (strncmp(device, "xfig", 4) == 0)
                _XFIG_draw_devcpy();
        else
#endif
#ifdef CANVAS
        if (strncmp(device, "canvas", 6) == 0)
                _CANVAS_draw_devcpy();
        else
#endif
#ifdef USEMAP
        if (strncmp(device, "usemap", 6) == 0)
                _USEMAP_draw_devcpy();
        else
#endif
#ifdef MINGW
        if (strncmp(device, "grwin", 5) == 0)
                _GRW_draw_devcpy();
        else
#endif
#ifdef NeXT
        if (strncmp(device, "NeXT", 4) == 0)
                _NeXT_draw_devcpy();
        else
#endif
#ifdef POSTSCRIPT
        if (strncmp(device, "postscript", 10) == 0) {
                _PS_draw_devcpy();
        } else if (strncmp(device, "ppostscript", 11) == 0) {
                _PSP_draw_devcpy();
        } else if (strncmp(device, "ppsm", 4) == 0) {
                _PSP_draw_devcpy();
        } else if (strncmp(device, "ppsc", 4) == 0) {
                _PPSC_draw_devcpy();
        } else if (strncmp(device, "ppsg", 4) == 0) {
                _PPSG_draw_devcpy();
        } else if (strncmp(device, "pps", 3) == 0) {
                _PSP_draw_devcpy();
        } else if (strncmp(device, "psm", 3) == 0) {
                _PS_draw_devcpy();
        } else if (strncmp(device, "psc", 3) == 0) {
                _PSC_draw_devcpy();
        } else if (strncmp(device, "psg", 3) == 0) {
                _PSG_draw_devcpy();
        } else if (strncmp(device, "ps", 2) == 0) {
                _PS_draw_devcpy();
        } else
#endif
#ifdef HPGL
        if (strncmp(device, "pclport", 7) == 0)
                _PCL_PORT_draw_devcpy();
        else if (strncmp(device, "pclland", 7) == 0)
                _PCL_LAND_draw_devcpy();
        else if (strncmp(device, "hpglland", 8) == 0)
                _HPGL_LAND_draw_devcpy();
        else if (strncmp(device, "hpglport", 8) == 0)
                _HPGL_PORT_draw_devcpy();
        else if ( strncmp(device, "hpgl", 4) == 0)
                _HPGL_LAND_draw_devcpy();
        else
#endif
#ifdef PCL5
        if (strncmp(device, "pcl5port", 8) == 0)
                _PCL5_PORT_draw_devcpy();
        else if (strncmp(device, "pcl5land", 8) == 0)
                _PCL5_LAND_draw_devcpy();
        else if (strncmp(device, "hpgl2land", 9) == 0)
                _HPGL2_LAND_draw_devcpy();
        else if (strncmp(device, "hpgl2port", 9) == 0)
                _HPGL2_PORT_draw_devcpy();
        else
#endif
#ifdef DXY
        if (strncmp(device, "dxy", 3) == 0)
                _DXY_draw_devcpy();
        else
#endif
#ifdef TEK
        if (strncmp(device, "tek", 3) == 0)
                _TEK_draw_devcpy();
        else
#endif
#ifdef XTEK
        if (strncmp(device, "xtek", 4) == 0)
                _XTEK_draw_devcpy();
        else
#endif
#ifdef XT
        if (strncmp(device, "xt", 2) == 0)
                _XT_draw_devcpy();
        else
#endif
#ifdef CGMT
        if (strncmp(device, "cgmt", 4) == 0)
                _CGMT_draw_devcpy();
        else
#endif
#ifdef MIF
        if (strncmp(device, "mif4", 4) == 0)
                _MIF4_draw_devcpy();
        else
        if (strncmp(device, "mif", 3) == 0)
                _MIF_draw_devcpy();
        else
#endif
#ifdef PDF
        if (strncmp(device, "pdf", 3) == 0)
                _PDF_draw_devcpy();
        else
#endif
#ifdef UNIXPLOT
        if (strncmp(device, "unixplot", 8) == 0)
                _UNIXPLOT_draw_devcpy();
        else
#endif
#ifdef VML
        if (strncmp(device, "vml", 3) == 0)
                _VML_draw_devcpy();
        else
#endif
#ifdef VOG
        if (strncmp(device, "vog", 3) == 0)
                _VOG_draw_devcpy();
        else
#endif
#ifdef GNU
        if (strncmp(device, "gnu", 3) == 0)
                _GNU_draw_devcpy();
        else
#endif
#ifdef FTI
        if (strncmp(device, "fti", 3) == 0)
                _FTI_draw_devcpy();
        else
#endif
#ifdef PBM
        if (strncmp(device, "pbm", 3) == 0)
                _PBM_draw_devcpy();
        else
        if (strncmp(device, "p1", 2) == 0)
                _P1_draw_devcpy();
        else
        if (strncmp(device, "p4", 2) == 0)
                _P4_draw_devcpy();
        else
        if (strncmp(device, "xbm", 3) == 0)
                _XBM_draw_devcpy();
        else
        if (strncmp(device, "bm", 2) == 0)
                _BM_draw_devcpy();
        else
#endif
#ifdef PPM
        if (strncmp(device, "char", 4) == 0)
                _CHAR_draw_devcpy();
        else
        if (strncmp(device, "ppm", 3) == 0)
                _PPM_draw_devcpy();
        else
        if (strncmp(device, "p3", 2) == 0)
                _P3_draw_devcpy();
        else
        if (strncmp(device, "p6", 2) == 0)
                _P6_draw_devcpy();
        else
#endif
#ifdef HERCULES
        if (strncmp(device, "hercules", 8) == 0)
                _hgc_draw_devcpy();
        else
#endif
#ifdef MSWIN
        if (strncmp(device, "mswin", 5) == 0)
                _mswin_draw_devcpy();
        else
#endif
#ifdef CGA
        if (strncmp(device, "cga", 3) == 0)
                _cga_draw_devcpy();
        else
#endif
#ifdef EGA
        if (strncmp(device, "ega", 3) == 0)
                _ega_draw_devcpy();
        else
#endif
#ifdef VGA
        if (strncmp(device, "vga", 3) == 0)
                _vga_draw_devcpy();
        else
#endif
#ifdef SIGMA
        if (strncmp(device, "sigma", 5) == 0)
                _sigma_draw_devcpy();
        else
#endif
        {
        if (*device == 0)
          sprintf(buf, "draw: environment variable M_DRAW_DEVICE not set properly\n");
        else
          sprintf(buf, "draw: %s is an invalid device type\n", device);

#ifdef MSWIN
        mswin_verror(buf);
#else
        fputs(buf, stderr);
        fprintf(stderr, " The devices compiled into this library are:\n");
/* -------------------------------------------------------------------------- */
        fprintf(stderr,"Primarily Interactive:\n");

#ifdef DECX11
        fprintf(stderr," decX11 \n");
#endif
#ifdef MINGW
        fprintf(stderr," grwin  (minWG GRwin PC interface)\n");
#endif
#ifdef NeXT
        fprintf(stderr," NeXT   \n");
#endif
#ifdef PC
        fprintf(stderr," PC     (PC with CygWin without X11 windows)\n");
#endif
#ifdef SUN
        fprintf(stderr," sun    (Sun workstation using SunView)\n");
#endif
#ifdef X11
        fprintf(stderr," X11    (X11 Windows white background) \n");
        fprintf(stderr," x11    (X11 Windows black background) \n");

#endif
#ifdef TEK
        fprintf(stderr," tek    (Tektronix 4010 terminal or emulator)\n");
#endif
#ifdef XTEK
        fprintf(stderr," xtek   (X11 xterm(1) Tektronix 4010 mode)\n");
#endif
#ifdef XT
        fprintf(stderr," xt     (X11 xterm(1) character mode)\n");
#endif
#ifdef HPGT
        fprintf(stderr," hpgt   (Hewlett Package Graphics Terminal)\n");
#endif
#ifdef TG
        fprintf(stderr," .*     (Tk Graphic Widgets)\n");
#endif
#ifdef HERCULES
        fprintf(stderr," hercules\n");
#endif
#ifdef CGA
        fprintf(stderr," cga\n");
#endif
#ifdef EGA
        fprintf(stderr," ega\n");
#endif
#ifdef VGA
        fprintf(stderr," vga\n");
#endif
#ifdef SIGMA
        fprintf(stderr," sigma\n");
#endif
/* -------------------------------------------------------------------------- */
#ifdef POSTSCRIPT
        fprintf(stderr," PostScript:\n");
        fprintf(stderr,"  Portrait:\n");
        fprintf(stderr, "   pps|ppostscript   (Monochrome)\n");
        fprintf(stderr,"   ppsc              (Color)\n");
        fprintf(stderr,"   ppsg              (Grayscale)\n");
        fprintf(stderr,"  Landscape:\n");
        fprintf(stderr,"   ps|psm|postscript (Monochrome)\n");
        fprintf(stderr,"   psc               (Color)\n");
        fprintf(stderr,"   psg               (Grayscale)\n");
#endif
/* -------------------------------------------------------------------------- */
#ifdef HPGL
        fprintf(stderr,"HP graphics languages (classic):\n");
        fprintf(stderr," HP:\n");
        fprintf(stderr,"   hpgl              (basic subset for importing)\n");
        fprintf(stderr,"   hpglland\n");
        fprintf(stderr,"   hpglport\n");
        fprintf(stderr,"   pclland\n");
        fprintf(stderr,"   pclport\n");
#endif
#ifdef PCL5
        fprintf(stderr,"HP graphics languages (standards):\n");
        fprintf(stderr,"   hpgl2land\n");
        fprintf(stderr,"   hpgl2port\n");
        fprintf(stderr,"   pcl5land\n");
        fprintf(stderr,"   pcl5port\n");
#endif
#ifdef DXY
        fprintf(stderr," dxy\n");
#endif
/* -------------------------------------------------------------------------- */
        fprintf(stderr,"Various Metafiles:\n");

#ifdef CANVAS
        fprintf(stderr," canvas    (HTML CANVAS)\n");
#endif
#ifdef CGMT
        fprintf(stderr," cgmt      (CGM clear text metafile)\n");
#endif
#ifdef FTI
        fprintf(stderr," fti       (SGI vector icon)\n");
#endif
#ifdef GNU
        fprintf(stderr," gnu       (GNU plotutils plot(1) metafile)\n");
#endif
#ifdef MIF
        fprintf(stderr," mif       (FrameMaker Interchange Format 3.0)\n");
        fprintf(stderr," mif4      (FrameMaker Interchange Format 4.0)\n");
#endif
#ifdef NIL
        fprintf(stderr," nil       (no output)\n");
#endif
#ifdef PDF
        fprintf(stderr," pdf       (Adobe Public Document Format)\n");
#endif
#ifdef RTF
        fprintf(stderr," rtf       (Microsoft Rich Text Format)\n");
#endif
#ifdef SVG
        fprintf(stderr," svg       (W3C Scalable Vector Graphics)\n");
#endif
#ifdef UNIXPLOT
        fprintf(stderr," unixplot  (Unix plot Format)\n");
#endif
#ifdef USEMAP
        fprintf(stderr," usemap    (HTML USEMAP polygon coordinates)\n");
#endif
#ifdef VML
        fprintf(stderr," vml       (Microsoft Vector Markup Language)\n");
#endif
#ifdef VOG
        fprintf(stderr," vog       (M_DRAW low level call (debug) driver)\n");
#endif
#ifdef XFIG
        fprintf(stderr," xfig      (X11 xfig(1) figure utility)\n");
#endif
/* -------------------------------------------------------------------------- */
#ifdef PBM
        fprintf(stderr," monochrome bitmaps:\n");
        fprintf(stderr,"  p1|pbm (Poskanzer ASCII)\n");
        fprintf(stderr,"  p4     (Poskanzer binary)\n");
        fprintf(stderr,"  xbm    X11 bitmap format\n");
        fprintf(stderr,"  bm     viewable text format for atobm(1x) and bitmap(1x)\n");
#endif
#ifdef PPM
        fprintf(stderr," color bitmaps:\n");
        fprintf(stderr,"  char    (xterm(1) characters (8 colors))\n");
        fprintf(stderr,"  p3|ppm  (Poskanzer ASCII)\n");
        fprintf(stderr,"  p6      (Poskanzer binary)\n");
#endif
/* -------------------------------------------------------------------------- */
#endif


           badcount++;
           if ( badcount > 30 ) {
              fprintf(stderr, "error: too many tries at device name [%d]'\n",badcount);
              exit(1);
           }
           fprintf(stderr, "W-A-R-N-I-N-G: device set to nil [%d].\n",badcount);
           fprintf(stderr, "               Default can be changed using environment variable $M_DRAW_DEVICE'\n");
           _NIL_draw_devcpy();

        }
}

/* -------------------------------------------------------------------------- */
/* vinit initialize M_DRAW */
void draw_vinit(char *device){
        char    *M_DRAW_OUTPUT;
        if(draw_fp == (FILE *)NULL || fileno(draw_fp) == -1 ){
                draw_fp = stdout;
                if ((M_DRAW_OUTPUT = getenv("M_DRAW_OUTPUT")) != (char *)NULL) {
                        /* fprintf(stderr,"FILENAME=%s\n",M_DRAW_OUTPUT); */
                        if(strlen(M_DRAW_OUTPUT)>0)draw_voutput(M_DRAW_OUTPUT);
                }
        }

        draw_getdev(device);

        if (vdevice.initialized)
                draw_vexit();

        if (!allocated) {
                allocated = 1;
                vdevice.transmat = (Mstack *)draw_vallocate(sizeof(Mstack),"from vinit 1");
                vdevice.transmat->back = (Mstack *)NULL;
                vdevice.attr = (Astack *)draw_vallocate(sizeof(Astack),"from vinit 2");
                vdevice.attr->back = (Astack *)NULL;
                vdevice.viewport = (Vstack *)draw_vallocate(sizeof(Vstack),"from vinit 3");
                vdevice.viewport->back = (Vstack *)NULL;
        }

        vdevice.clipoff = 0;
        vdevice.sync = 1;
        vdevice.upset = 0;
        vdevice.cpW[V_W] = 1.0;                 /* never changes */

        vdevice.attr->a.style = (char *)NULL;
        vdevice.attr->a.dashp = (char *)NULL;
        vdevice.attr->a.fill = 0;
        vdevice.attr->a.hatch = 0;
        vdevice.attr->a.backface = 0;
        vdevice.attr->a.color = 0;
        vdevice.attr->a.justify = V_LEFT|V_BOTTOM;
        vdevice.attr->a.bold = 0;
        vdevice.attr->a.inbackbuffer = 0;
        vdevice.attr->a.exvp = 0;
        vdevice.attr->a.softtext = 0;
        vdevice.attr->a.fixedwidth = 0;
        vdevice.attr->a.skew = 0;
        vdevice.attr->a.hatchcos = 1.0;
        vdevice.attr->a.hatchsin = 0.0;
        vdevice.attr->a.hatchpitch = 0.1;
        vdevice.attr->a.textcos = 1.0;
        vdevice.attr->a.textsin = 0.0;
        vdevice.attr->a.dash = 0.0;
        vdevice.attr->a.adist = 0.0;
        vdevice.attr->a.font[0] = '\0';

        if ((*vdevice.dev.Vinit)()) {
                vdevice.initialized = 1;
                vdevice.writestoprocess = 1;
                vdevice.inobject = 0;
                vdevice.inpolygon = 0;

                if(getenv("VEXPANDVP") != (char *)NULL){
                   draw_expandviewport();
                }

                draw_viewport(-1.0, 1.0, -1.0, 1.0);

                draw_identmatrix(vdevice.transmat->m);

                draw_move(0.0, 0.0, 0.0);

                if(!draw_hershfont("futura.l")){    /* set up default font */
                   draw_font("small"); /* if Hershey font not found load hardware */
                }else{
                   draw_textsize(0.05, 0.05);
                }
        } else {
                fprintf(stderr, "draw: error while setting up device\n");
                exit(1);
        }
}
/******************************************************************************/
/*
 * Hacky new device changing routines...
 */
#define DEVSTACK        8
static  Device  vdevstk[DEVSTACK];
static  int     vdevindx = 0;

void draw_pushdev(char *device) {
        /*
         * Save the old vdevice structure
         */
        draw_pushattributes();
        draw_pushviewport();

        if (vdevindx < DEVSTACK)
                vdevstk[vdevindx++] = vdevice;
        else
                draw_verror("draw: pushdev: Device stack overflow");

        vdevice.initialized = 0;

        draw_getdev(device);

        (*vdevice.dev.Vinit)();

        vdevice.initialized = 1;

        draw_popviewport();
        draw_popattributes();

}
/******************************************************************************/
/* Restore the old vdevice structure */
void draw_popdev(void) {
        draw_pushattributes();
        draw_pushviewport();

        (*vdevice.dev.Vexit)();
        if (vdevindx > 0)
                vdevice = vdevstk[--vdevindx];
        else
                draw_verror("draw: popdev: Device stack underflow");

        draw_popviewport();
        draw_popattributes();
}
/******************************************************************************/
/* reinitialize draw to use a new device but don't change any
 * global attributes like the window and viewport settings.
 */
void draw_vnewdev(char *device){
        if (!vdevice.initialized)
                draw_verror("vnewdev: draw not initialized\n");

        draw_pushviewport();

        (*vdevice.dev.Vexit)();

        vdevice.initialized = 0;

        draw_getdev(device);

        (*vdevice.dev.Vinit)();

        vdevice.initialized = 1;

        /*
         * Need to update font for this device if hardware font is what was
         * being used previously.
         */

        if (!strcmp(vdevice.attr->a.font, "small")) {
                if (!(*vdevice.dev.Vfont)(vdevice.dev.small))
                        draw_verror("font: unable to open small font");
        } else if (!strcmp(vdevice.attr->a.font, "large")) {
                if (!(*vdevice.dev.Vfont)(vdevice.dev.large))
                        draw_verror("font: unable to open large font");
        }

        draw_popviewport();
}

/******************************************************************************/
/*
 *      Returns the name of the current draw device
 *      in the buffer buf. Also returns a pointer to
 *      the start of buf.
 */
char *draw_vgetdev(char *buf) {
        /*
         * Note no exit if not initialized here - so that vexit
         * can be called before printing the name.
         */

        if (vdevice.dev.devname){
           strncpy(buf, vdevice.dev.devname,strlen(buf));
        } else{
           strncpy(buf, "(no device)",12);
        }
/*
        if (vdevice.inobject) {
           fprintf(stderr,"warning: vgetdev called in object\n");
        }
*/
        return(&buf[0]);
}
/******************************************************************************/
/* returns the next key pressed.  */
#ifndef GRX /* Has its own getkey */
int draw_getkey(void) {
        if (!vdevice.initialized)
                draw_verror("getkey: draw not initialized\n");

        return((*vdevice.dev.Vgetkey)());
}
#endif
/******************************************************************************/
/* checkkey
 *      returns true if a key has been hit, or 0 otherwise
 *      (doesn't wait around like getkey)
 */
int draw_checkkey(void) {
        if (!vdevice.initialized)
                draw_verror("checkkey: draw not initialized\n");

        return((*vdevice.dev.Vcheckkey)());
}

/******************************************************************************/
/*
 * locator
 *
 *      returns the current position of the crosshair or equivalent
 * in world coordinates, and the mouse buttons pressed (if any).
 */
int draw_locator(float *wx, float *wy){
        int     a, b, c;

        if (!vdevice.initialized){
                draw_verror("locator: draw not initialized");
        }

        c = (*vdevice.dev.Vlocator)(&a, &b);
        draw_VtoWxy((float)a, (float)b, wx, wy);

        return(c);
}

/******************************************************************************/
/*
 * slocator
 *
 *      returns the current position of the crosshair or equivalent
 * in screen coordinates, and the mouse buttons pressed (if any).
 */
int draw_slocator(float *wx, float *wy) {
        int     a, b, c;
        float   sx, sy;

        if (!vdevice.initialized)
                draw_verror("slocator: draw not initialized");

        c = (*vdevice.dev.Vlocator)(&a, &b);
        sx = vdevice.sizeX;
        sy = vdevice.sizeY;

        *wx = a / (0.5 * sx) - 1.0;
        *wy = b / (0.5 * sy) - 1.0;

        return(c);
}

/******************************************************************************/
/*
 * clear
 *
 *      clears the screen to the current colour, excepting devices
 * like a laser printer where it flushes the page.
 *
 */
void draw_clear(void) {
        Token   *tok;

        if (!vdevice.initialized)
                draw_verror("clear: draw not initialized");

        if (vdevice.inobject) {
                tok = draw_newtokens(1);
                tok->i = OBJ_CLEAR;

                return;
        }

        (*vdevice.dev.Vclear)();
}

/******************************************************************************/
/* exit the draw system */
void draw_vexit(void) {
        if (!vdevice.initialized)
                draw_verror("vexit: draw not initialized");

        (*vdevice.dev.Vexit)();

        vdevice.initialized = 0;
        draw_fp = stdout;
        badcount=0;
}

/******************************************************************************/
/* set the current colour to colour index number i.  */
void draw_color(int i){
        Token   *tok;

        if (!vdevice.initialized)
                draw_verror("color: draw not initialized");

        if (vdevice.inobject) {
                tok = draw_newtokens(2);

                tok[0].i = OBJ_COLOR;
                tok[1].i = i;
                return;
        }

        vdevice.attr->a.color = i;
        (*vdevice.dev.Vcolor)(i);
}

/******************************************************************************/
/* set the color of index i.  */
void draw_mapcolor(int i, short r, short g, short b){
        Token   *tok;

        if (!vdevice.initialized)
                draw_verror("mapcolor: draw not initialized");

        if (vdevice.inobject) {
                tok = draw_newtokens(5);

                tok[0].i = OBJ_MAPCOLOR;
                tok[1].i = i;
                tok[2].i = r;
                tok[3].i = g;
                tok[4].i = b;

                return;
        }

        (*vdevice.dev.Vmapcolor)(i, r, g, b);
}

/******************************************************************************/
/* Returns the number of bit planes on a device.  */
int draw_getdepth(void) {
        if (!vdevice.initialized)
                draw_verror("getdepth: draw not initialized\n");

        /*
        if (vdevice.inobject) {
                fprintf(stderr,"warning: getdepth called in object\n");
        }
        */

        return(vdevice.depth);
}

/******************************************************************************/
/* Controls flushing of the display - we can get considerable
 * speed-up's under X11 using this...
 */
void draw_vsetflush(int yn){
        vdevice.sync = yn;
}

/******************************************************************************/
/* Explicitly call the device flushing routine...
 * This is enabled for object so that you can force an update
 * in the middle of an object, as objects have flushing off
 * while they are drawn anyway.
 */
void draw_vflush(void){
        Token   *tok;

        if (!vdevice.initialized)
                draw_verror("vflush: draw not initialized");

        if (vdevice.inobject) {
                tok = draw_newtokens(1);
                tok->i = OBJ_VFLUSH;

                return;
        }

        (*vdevice.dev.Vsync)();
}
/******************************************************************************/
/* It is difficult to do this device independently, but trying by
 * saying 1 unit is 1/10000 of X size of display.
 */
void draw_linewidth(int w) {
        Token   *tok;
        if (!vdevice.initialized)
                draw_verror("linewidth: draw not initialized");

        if (vdevice.inobject) {
                tok = draw_newtokens(2);

                tok[0].i = OBJ_LINEWIDTH;
                tok[1].i = w;
                return;
        }
        (*vdevice.dev.Vsetlw)(w);
}
/******************************************************************************/
