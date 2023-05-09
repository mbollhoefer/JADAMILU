#include <string.h>
#ifdef _DOUBLE_REAL_
#define _ILUPACK_DEFINE_GLOBALS_
#endif
#include <ilupack.h>
#include <ilupackmacros.h>

#define MAX(A,B)   (((A)>=(B))?(A):(B))
#define MIN(A,B)   (((A)<=(B))?(A):(B))

void  DGLPRECSETUP(size_t    *IPdiag, 
		   FLOAT     *a,
		   integer   *n,
		   integer   *ISEARCH,
		   REALS     *SHIFT,
		   REALS     *shift0,
		   integer   *factdgl,
		   integer   *factspd,
		   REALS     *diagmin) {

  REALS *diag;
  REALS ta,del,tmp,ati,ata;
  integer i;

  diag=(REALS *)MALLOC(*n*sizeof(REALS),"dglprecsetup:diag");
  memcpy(IPdiag, &diag, sizeof(size_t));

#if defined _DOUBLE_REAL_ || defined _SINGLE_REAL_
  *diagmin=a[0];
#else
  *diagmin=a[0].r;
#endif

  ta=*diagmin;
  for (i=1; i<*n; i++) {
#if defined _DOUBLE_REAL_ || defined _SINGLE_REAL_
      *diagmin=MIN(*diagmin,a[i]);
      ta=MAX(ta,a[i]);
#else
      *diagmin=MIN(*diagmin,a[i].r);
      ta=MAX(ta,a[i].r);
#endif
  }
  ati=MAX(*diagmin,-*diagmin);
  ata=MAX(ta,-ta);
  ata=MIN(ata,ati);
  del=MAX(ta-*diagmin,ata);
  
  if (*ISEARCH<=1) {
      *shift0=*diagmin-del/(*n);  
      *factspd=-1;
      if (*ISEARCH==1) {
         if (*SHIFT<*diagmin) {
	    *shift0=*SHIFT;
	    *factspd=0;    
         }
      }
      for (i=0; i<*n; i++)
#if defined _DOUBLE_REAL_ || defined _SINGLE_REAL_
        diag[i]=1.0/(a[i]-*shift0);
#else
        diag[i]=1.0/(a[i].r-*shift0);
#endif
  }
  else {
      *factspd=0;
      del=1./del;
      for   (i=0; i<*n; i++) {
#if defined _DOUBLE_REAL_ || defined _SINGLE_REAL_
         tmp=a[i]-*shift0;
#else
         tmp=a[i].r-*shift0;
#endif
         if (tmp > 0.0)
	     tmp=MAX(tmp,del);
	 else
	     tmp=MIN(tmp,-del);
         diag[i]=1.0/tmp;
      }
  }
  *factdgl=1;
}
