#include <string.h>
#include <stdio.h>
#include <ilupack.h>
#include <ilupackmacros.h>

// #define PRINT_INFO

#ifdef _DOUBLE_REAL_
#define MYSYMILUPACKDELETE     dsymamgdelete
#define MYSYMAMGDELETE         DSYMAMGdelete
#define MYSYMAMGINIT           DSYMAMGinit
#define MYSYMAMGSOL            DSYMAMGsol
#define MYSYMAMGGETPARAMS      DSYMAMGgetparams
#define MYSYMAMGSETPARAMS      DSYMAMGsetparams


#elif defined _SINGLE_REAL_
#define MYSYMILUPACKDELETE     ssymamgdelete
#define MYSYMAMGDELETE         SSYMAMGdelete
#define MYSYMAMGINIT           SSYMAMGinit
#define MYSYMAMGSOL            SSYMAMGsol
#define MYSYMAMGGETPARAMS      SSYMAMGgetparams
#define MYSYMAMGSETPARAMS      SSYMAMGsetparams


#elif defined _SINGLE_COMPLEX_

#ifdef _COMPLEX_SYMMETRIC_
#define MYSYMILUPACKDELETE     csymamgdelete
#define MYSYMAMGDELETE         CSYMAMGdelete
#define MYSYMAMGINIT           CSYMAMGinit
#define MYSYMAMGSOL            CSYMAMGsol
#define MYSYMAMGGETPARAMS      CSYMAMGgetparams
#define MYSYMAMGSETPARAMS      CSYMAMGsetparams
#else
#define MYSYMILUPACKDELETE     cheramgdelete
#define MYSYMAMGDELETE         CHERAMGdelete
#define MYSYMAMGINIT           CHERAMGinit
#define MYSYMAMGSOL            CHERAMGsol
#define MYSYMAMGGETPARAMS      CHERAMGgetparams
#define MYSYMAMGSETPARAMS      CHERAMGsetparams
#endif


#else

#ifdef _COMPLEX_SYMMETRIC_
#define MYSYMILUPACKDELETE     zsymamgdelete
#define MYSYMAMGDELETE         ZSYMAMGdelete
#define MYSYMAMGINIT           ZSYMAMGinit
#define MYSYMAMGSOL            ZSYMAMGsol
#define MYSYMAMGGETPARAMS      ZSYMAMGgetparams
#define MYSYMAMGSETPARAMS      ZSYMAMGsetparams
#else
#define MYSYMILUPACKDELETE     zheramgdelete
#define MYSYMAMGDELETE         ZHERAMGdelete
#define MYSYMAMGINIT           ZHERAMGinit
#define MYSYMAMGSOL            ZHERAMGsol
#define MYSYMAMGGETPARAMS      ZHERAMGgetparams
#define MYSYMAMGSETPARAMS      ZHERAMGsetparams
#endif
#endif



void MYSYMILUPACKDELETE(size_t *Fparam, 
		        size_t *FPREC,
		        integer   *nlev, integer *ICNTL) {
   /*
     ILUPACK FORTRAN interface for solving a linear system iteratively

     param       parameter pointer casted to INTEGER*8
     PRE         preconditioner pointer casted to INTEGER*8

     nlev        number of AMG levels

   */

  CSRMAT       A;
  ILUPACKPARAM *param;
  AMGLEVELMAT *PRE;

  memcpy(&param, Fparam, sizeof(size_t));
  memcpy(&PRE,   FPREC,  sizeof(size_t)); 


  // if ICNTL is set then we keep the elbow space
  // in order to re-factor the matrix.
  // otherwise we discard the memory
  if (*ICNTL!=0) {
     param->ipar[6]|=RE_FACTOR;
     // printf("clear preconditioner but keep memory board\n");fflush(stdout);
  }
  else {
     param->ipar[6]&=~RE_FACTOR;
     // printf("remove whole preconditioner\n");fflush(stdout);
  }

  /* finally release memory of the preconditioner */
  A.nc=A.nr=PRE->n;
  A.ia=NULL;
  A.ja=NULL;
  A.a =NULL;
  MYSYMAMGDELETE(A,*PRE,*nlev,param);


} // SYMILUPACKDELETE
