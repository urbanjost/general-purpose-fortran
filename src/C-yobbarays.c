/* this code is licensed as public domain */
struct	YOBBA	{
		union {
			struct {
				unsigned alpha : 1;
				unsigned beta  : 2;
				unsigned fixe  : 3;
				unsigned kralb  : 1;
			} vals;
			struct {
				unsigned char yobbav;
			} yobbavals;
		} yobba;
} *yobbaray;

#ident "@(#)M_DRAW:yobbarays - Turns on (or off) yobba rays, as described by Larry Dart's friend."
/*
 *	onoff <> 0 - YOBBARAYS ON.
 *	onoff =  0 - YOBBARAYS OFF.
 */
void draw_yobbarays(long int onoff){
	yobbaray = (struct YOBBA *)onoff;
}
