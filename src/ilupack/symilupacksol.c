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


void MYSYMILUPACKSOL(size_t *Fparam, 
		     size_t *FPREC,
		     integer   *nlev,
		     FLOAT     *sol,
		     FLOAT     *rhs,
		     integer   *n) {
   /*
     ILUPACK FORTRAN interface for solving a linear system iteratively

     param       parameter pointer casted to INTEGER*8
     PRE         preconditioner pointer casted to INTEGER*8

     nlev        number of AMG levels

     sol         solution vector of length n, must be initialized on input with
                 an initial guess

     rhs         right hand side  of length n

     n           size of the system

   */
  integer i,j;
  ILUPACKPARAM *param;
  AMGLEVELMAT *PRE;

  memcpy(&param, Fparam, sizeof(size_t));
  memcpy(&PRE,   FPREC,  sizeof(size_t));

  

  if (param->ndbuff<3**n) {
     param->ndbuff=3**n;
     param->dbuff=(FLOAT*)REALLOC(param->dbuff,3**n*sizeof(FLOAT),"ilupacksol:dbuff");
  }

  for (i=0;i<PRE->n; i++) {
#if defined _SINGLE_REAL_ || defined _DOUBLE_REAL_
    rhs[i]*=PRE->rowscal[i];
#else
    rhs[i].r*=PRE->rowscal[i].r;
    rhs[i].i*=PRE->rowscal[i].r;
#endif
  }

  MYSYMAMGSOL(*PRE, *nlev, param, rhs,sol, param->dbuff);

  // undo scaling
  for (i=0;i<PRE->n; i++) {
#if defined _SINGLE_REAL_ || defined _DOUBLE_REAL_
    rhs[i]/=PRE->rowscal[i];
#else
    rhs[i].r/=PRE->rowscal[i].r;
    rhs[i].i/=PRE->rowscal[i].r;
#endif
  }

  for (i=0;i<PRE->n; i++) {
#if defined _SINGLE_REAL_ || defined _DOUBLE_REAL_
    sol[i]*=PRE->colscal[i];
#else
    sol[i].r*=PRE->colscal[i].r;
    sol[i].i*=PRE->colscal[i].r;
#endif
  }



} // SYMILUPACKSOL
