#include <stdio.h>
#include <ilupack.h>
#include <ilupackmacros.h>

// #define PRINT_INFO


#ifdef _DOUBLE_REAL_
#define MYSYMILUPACKINIT     dsymamginit
#define MYSYMAMGINIT         DSYMAMGinit
#define MYSYMAMGGETPARAMS    DSYMAMGgetparams


#elif defined _SINGLE_REAL_
#define MYSYMILUPACKINIT     ssymamginit
#define MYSYMAMGINIT         SSYMAMGinit
#define MYSYMAMGGETPARAMS    SSYMAMGgetparams


#elif defined _SINGLE_COMPLEX_

#ifdef _COMPLEX_SYMMETRIC_
#define MYSYMILUPACKINIT     csymamginit
#define MYSYMAMGINIT         CSYMAMGinit
#define MYSYMAMGGETPARAMS    CSYMAMGgetparams
#else
#define MYSYMILUPACKINIT     cheramginit
#define MYSYMAMGINIT         CHERAMGinit
#define MYSYMAMGGETPARAMS    CHERAMGgetparams
#endif


#else

#ifdef _COMPLEX_SYMMETRIC_
#define MYSYMILUPACKINIT     zsymamginit
#define MYSYMAMGINIT         ZSYMAMGinit
#define MYSYMAMGGETPARAMS    ZSYMAMGgetparams
#else
#define MYSYMILUPACKINIT     zheramginit
#define MYSYMAMGINIT         ZHERAMGinit
#define MYSYMAMGGETPARAMS    ZHERAMGgetparams
#endif
#endif


#ifdef _SINGLE_REAL_ 
void ilupackdummy10005() {
}
#elif defined _SINGLE_COMPLEX_
void ilupackdummy10006() {
}

#elif defined _DOUBLE_REAL_

void sdsymamginit(integer         *n,
		  integer         *ia,
		  integer         *ja,
		  doubleprecision *a,
		  integer         *matching,
		  character       *ordering,
		  doubleprecision *droptol,
		  doubleprecision *condest,
		  doubleprecision *restol,
		  integer         *maxit,
		  doubleprecision *elbow,
		  integer         *lfil,
		  integer         *lfilS,
		  integer         *nrestart) {
  
  real *sa=(real *)a, sdroptol=*droptol, scondest=*condest, 
       srestol=*restol, selbow=*elbow;

  ssymamginit(n,ia,ja,sa, matching, ordering, &sdroptol, &scondest, 
	      &srestol, maxit, &selbow, lfil, lfilS, nrestart);
  *droptol=sdroptol; *condest=scondest;
  *restol =srestol;  *elbow  =selbow;
}
#else
void czheramginit(integer         *n,
		  integer         *ia,
		  integer         *ja,
		  doublecomplex   *a,
		  integer         *matching,
		  character       *ordering,
		  doubleprecision *droptol,
		  doubleprecision *condest,
		  doubleprecision *restol,
		  integer         *maxit,
		  doubleprecision *elbow,
		  integer         *lfil,
		  integer         *lfilS,
		  integer         *nrestart) {
  
  complex *sa=(complex *)a;
  real    sdroptol=*droptol, scondest=*condest, 
          srestol =*restol,  selbow  =*elbow;

  cheramginit(n,ia,ja,sa, matching, ordering, &sdroptol, &scondest,
	      &srestol, maxit, &selbow, lfil, lfilS, nrestart);
  *droptol=sdroptol; *condest=scondest;
  *restol =srestol;  *elbow  =selbow;
}
#endif
