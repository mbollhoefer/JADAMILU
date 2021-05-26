#include <stdio.h>
#include <string.h>
#include <ilupack.h>
#include <ilupackmacros.h>

// #define PRINT_INFO


#ifdef _DOUBLE_REAL_
#define MYSYMILUPACKSOL        dsymamgsol
#define MYSYMAMGINIT           DSYMAMGinit
#define MYSYMAMGSOL            DSYMAMGsol
#define MYSYMAMGETPARAMS       DSYMAMGgetparams
#define MYSYMAMSETPARAMS       DSYMAMGsetparams


#elif defined _SINGLE_REAL_
#define MYSYMILUPACKSOL        ssymamgsol
#define MYSYMAMGINIT           SSYMAMGinit
#define MYSYMAMGSOL            SSYMAMGsol
#define MYSYMAMGETPARAMS       SSYMAMGgetparams
#define MYSYMAMSETPARAMS       SSYMAMGsetparams


#elif defined _SINGLE_COMPLEX_

#ifdef _COMPLEX_SYMMETRIC_
#define MYSYMILUPACKSOL        csymamgsol
#define MYSYMAMGINIT           CSYMAMGinit
#define MYSYMAMGSOL            CSYMAMGsol
#define MYSYMAMGETPARAMS       CSYMAMGgetparams
#define MYSYMAMSETPARAMS       CSYMAMGsetparams
#else
#define MYSYMILUPACKSOL        cheramgsol
#define MYSYMAMGINIT           CHERAMGinit
#define MYSYMAMGSOL            CHERAMGsol
#define MYSYMAMGETPARAMS       CHERAMGgetparams
#define MYSYMAMSETPARAMS       CHERAMGsetparams
#endif


#else

#ifdef _COMPLEX_SYMMETRIC_
#define MYSYMILUPACKSOL        zsymamgsol
#define MYSYMAMGINIT           ZSYMAMGinit
#define MYSYMAMGSOL            ZSYMAMGsol
#define MYSYMAMGETPARAMS       ZSYMAMGgetparams
#define MYSYMAMSETPARAMS       ZSYMAMGsetparams
#else
#define MYSYMILUPACKSOL        zheramgsol
#define MYSYMAMGINIT           ZHERAMGinit
#define MYSYMAMGSOL            ZHERAMGsol
#define MYSYMAMGETPARAMS       ZHERAMGgetparams
#define MYSYMAMSETPARAMS       ZHERAMGsetparams
#endif
#endif

#define MYABS(A)          (((A)>=0)?(A):(-(A)))

#ifdef _SINGLE_REAL_ 
void ilupackdummy10003() {
}
#elif defined _SINGLE_COMPLEX_
void ilupackdummy10004() {
}

#elif defined _DOUBLE_REAL_
void sdsymamgsol(size_t *Fparam, 
		 size_t *FPREC,
		 integer   *nlev,
		 doubleprecision *sol,
		 doubleprecision *rhs,
		 integer   *n) {

  integer         i, nn=MYABS(*n);
  real            *ssol, *srhs;
  SILUPACKparam   *param;
  SAMGlevelmat    *PRE;

  memcpy(&param, Fparam, sizeof(size_t));
  memcpy(&PRE,   FPREC,  sizeof(size_t));

  if (param->ndbuff<5*nn) {
     param->ndbuff=5*nn;
     param->dbuff=(real *)REALLOC(param->dbuff, 
				  param->ndbuff*sizeof(real),
				  "singlesymamgsol");
  }
  ssol=param->dbuff+3*nn;
  srhs=param->dbuff+4*nn;
  
  for (i=0;i<nn; i++) {
      srhs[i]=rhs[i]*PRE->rowscal[i];
  }

  SSYMAMGsol(*PRE, *nlev, param, srhs,ssol, param->dbuff);

  for (i=0;i<PRE->n; i++) {
      sol[i]=ssol[i]*PRE->colscal[i];
  }


}
#else
void czheramgsol(size_t *Fparam, 
		 size_t *FPREC,
		 integer   *nlev,
		 doublecomplex *sol,
		 doublecomplex *rhs,
		 integer   *n) {

  integer         i,nn=MYABS(*n);
  complex         *ssol, *srhs;
  CILUPACKparam   *param;
  CAMGlevelmat    *PRE;

  memcpy(&param, Fparam, sizeof(size_t));
  memcpy(&PRE,   FPREC,  sizeof(size_t));

  if (param->ndbuff<5*nn) {
     param->ndbuff=5*nn;
     param->dbuff=(complex *)REALLOC(param->dbuff,
				     param->ndbuff*sizeof(complex),
				     "singlesymamgsol");
  }
  ssol=param->dbuff+3*nn;
  srhs=param->dbuff+4*nn; 

  for (i=0; i<nn; i++) {
      srhs[i].r=rhs[i].r*PRE->rowscal[i].r;
      srhs[i].i=rhs[i].i*PRE->rowscal[i].r;
  }

  CHERAMGsol(*PRE, *nlev, param, srhs,ssol, param->dbuff);

  for (i=0;i<PRE->n; i++) {
      sol[i].r=ssol[i].r*PRE->colscal[i].r;
      sol[i].i=ssol[i].i*PRE->colscal[i].r;
  }

}
#endif

