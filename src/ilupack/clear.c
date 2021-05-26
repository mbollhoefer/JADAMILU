#include <ilupack.h>


#include <ilupackmacros.h>

void CLEAR(integer n, FLOAT *v, integer incx)

{
   integer    j=n%4;
   FLOAT *pv=v;

   if (incx<1 || n<=0)
      return;
   
   if (incx==1)
   {
      v+=n-j;
      for (; pv<v; pv+=4)
#if defined _DOUBLE_REAL_ || defined _SINGLE_REAL_
	  pv[0]=pv[1]=pv[2]=pv[3]=0;
#else
	  pv[0].r=pv[0].i=pv[1].r=pv[1].i=pv[2].r=pv[2].i=pv[3].r=pv[3].i=0;
#endif
      v+=j;
      while (pv<v) {
#if defined _DOUBLE_REAL_ || defined _SINGLE_REAL_
	    *pv++=0;
#else
	    pv->r=pv->i=0;
	    pv++;
#endif
      } // end while
   } /* end if */
   else /* incx>1 */
   {
      v+=(n-j)*incx;
      for (; pv<v; pv+=incx<<2)
#if defined _DOUBLE_REAL_ || defined _SINGLE_REAL_
	  pv[0]=pv[incx]=pv[incx<<1]=pv[incx*3]=0;
#else
	  pv[0].r=pv[0].i=pv[incx].r=pv[incx].i=pv[incx<<1].r=pv[incx<<1].i
	         =pv[incx*3].r=pv[incx*3].i=0;
#endif
      v+=j*incx;
      for (; pv<v; pv+=incx)
#if defined _DOUBLE_REAL_ || defined _SINGLE_REAL_
	  *pv=0;
#else
	  pv->r=pv->i=0;
#endif
   } /* end else */
} /* end CLEAR */
