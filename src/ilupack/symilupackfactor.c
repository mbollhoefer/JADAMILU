#include <string.h>
#include <stdio.h>
#include <ilupack.h>
#include <ilupackmacros.h>

// #define PRINT_INFO


#ifdef _DOUBLE_REAL_
#define MYSYMILUPACKFACTOR     dsymamgfactor
#define MYSYMAMGINIT           DSYMAMGinit
#define MYSYMAMGFACTOR         DSYMAMGfactor
#define MYSYMAMGGETPARAMS      DSYMAMGgetparams
#define MYSYMAMGSETPARAMS      DSYMAMGsetparams
#define MYSYMPERMMC64AMD       DSYMperm_mc64_amd


#elif defined _SINGLE_REAL_
#define MYSYMILUPACKFACTOR     ssymamgfactor
#define MYSYMAMGINIT           SSYMAMGinit
#define MYSYMAMGFACTOR         SSYMAMGfactor
#define MYSYMAMGGETPARAMS      SSYMAMGgetparams
#define MYSYMAMGSETPARAMS      SSYMAMGsetparams
#define MYSYMPERMMC64AMD       SSYMperm_mc64_amd


#elif defined _SINGLE_COMPLEX_

#ifdef _COMPLEX_SYMMETRIC_
#define MYSYMILUPACKFACTOR     csymamgfactor
#define MYSYMAMGINIT           CSYMAMGinit
#define MYSYMAMGFACTOR         CSYMAMGfactor
#define MYSYMAMGGETPARAMS      CSYMAMGgetparams
#define MYSYMAMGSETPARAMS      CSYMAMGsetparams
#define MYSYMPERMMC64AMD       CSYMperm_mc64_amd
#else
#define MYSYMILUPACKFACTOR     cheramgfactor
#define MYSYMAMGINIT           CHERAMGinit
#define MYSYMAMGFACTOR         CHERAMGfactor
#define MYSYMAMGGETPARAMS      CHERAMGgetparams
#define MYSYMAMGSETPARAMS      CHERAMGsetparams
#define MYSYMPERMMC64AMD       CHERperm_mc64_amd
#endif


#else

#ifdef _COMPLEX_SYMMETRIC_
#define MYSYMILUPACKFACTOR     zsymamgfactor
#define MYSYMAMGINIT           ZSYMAMGinit
#define MYSYMAMGFACTOR         ZSYMAMGfactor
#define MYSYMAMGGETPARAMS      ZSYMAMGgetparams
#define MYSYMAMGSETPARAMS      ZSYMAMGsetparams
#define MYSYMPERMMC64AMD       ZSYMperm_mc64_amd
#else 
#define MYSYMILUPACKFACTOR     zheramgfactor
#define MYSYMAMGINIT           ZHERAMGinit
#define MYSYMAMGFACTOR         ZHERAMGfactor
#define MYSYMAMGGETPARAMS      ZHERAMGgetparams
#define MYSYMAMGSETPARAMS      ZHERAMGsetparams
#define MYSYMPERMMC64AMD       ZHERperm_mc64_amd
#endif
#endif
#define MAX(A,B)        (((A)>(B))?(A):(B))

integer MYSYMILUPACKFACTOR(size_t *Fparam, 
		       size_t *FPREC,
		       integer   *nlev,
		       integer   *ICNTL,
		       integer   *n,
		       integer   *ia,
		       integer   *ja,
		       FLOAT     *a,
		       integer   *matching,
		       character *ordering,
		       REALS     *droptol,
		       REALS     *condest,
		       REALS     *restol,
		       integer   *maxit,
		       REALS     *elbow,
		       integer   *lfil,
		       integer   *lfilS,
		       integer   *nrestart) {
   /*
     ILUPACK FORTRAN interface for initialization

     param       parameter pointer casted to INTEGER*8
     PRE         preconditioner pointer casted to INTEGER*8

     nlev        number of AMG levels

     n           size of the system

     ia          pointer         \
     ja          column indices  | of an NxN matrix in sparse row format
     a           numerical values/

     matching    logical switch whether to turn on matching or not

     droptol     threshold for ILU

     restol      stopping tolerance for the iterative solver
                 In general this will be the backward error, for
                 the SPD case this will be the relative error in the
                 energy norm

     maxit       maximum number of iteration steps

     elbow       elbow space factor for ILU

     lfil        number of fill elements per column in L resp. rows in U

     lfilS       number of fill elements pre row in in the coarse grid 
                 system (Schur complement)

     nrestart    number of restart steps in the unsymmetric case
   */

  CSRMAT       A;
  integer      flags,i,j;
  REALS        droptols[2];
  integer      mymaxit,mynrestart,ierr,
               (*perm0)(),(*perm)(),(*permf)();
  REALS        myelbow,mycondest,myrestol;
  ILUPACKPARAM *param;
  AMGLEVELMAT *PRE;

  
  // indicate that param has not been used previously
  if (*n>0) {

     // printf("create new parameters\n");fflush(stdout);
     param=(ILUPACKPARAM *)MALLOC((size_t)1*sizeof(ILUPACKPARAM),"ilupackfactor:param");
     PRE  =(AMGLEVELMAT *) MALLOC((size_t)1*sizeof(AMGLEVELMAT), "ilupackfactor:PRE");

     memcpy(Fparam, &param, sizeof(size_t));
     memcpy(FPREC,  &PRE,   sizeof(size_t));


     A.nr=A.nc=*n;
     A.ia=ia;
     A.ja=ja;
     A.a =a;
     
     /*
       for (i=0; i<A.nr; i++) {
       for (j=A.ia[i]-1; j<A.ia[i+1]-1; j++) {
       printf("%8d",A.ja[j]);
       }
       printf("\n");
       for (j=A.ia[i]-1; j<A.ia[i+1]-1; j++) {
       printf("%8.1e",A.a[j]);
       }
       printf("\n");
       }
     */
     SYMAMGINIT(A,param);
  }
  else {
     //printf("resume using old parameters\n");fflush(stdout);
     *n=-*n;

     memcpy(&param, Fparam, sizeof(size_t));
     memcpy(&PRE,   FPREC,  sizeof(size_t));

     A.nr=A.nc=*n;
     A.ia=ia;
     A.ja=ja;
     A.a =a;
  }  

  // modify the default settings
  SYMAMGGETPARAMS(*param, &flags, &myelbow, droptols, &mycondest,
		  &myrestol, &mymaxit);
  myelbow    =*elbow;
  mycondest  =*condest;
  myrestol   =*restol;
  mymaxit    =*maxit;
  droptols[1]=*droptol;
  SYMAMGSETPARAMS(A, param, flags, myelbow, droptols, mycondest,
		  myrestol, mymaxit);

  // only use AMD for simplicity
  if (*matching) {
     /*
     perm0=SYMPERMMWMAMD;
     perm =SYMPERMMWMAMD;
     permf=SYMPERMMWMAMD;
     */
     perm0=MYSYMPERMMC64AMD;
     perm =MYSYMPERMMC64AMD;
     permf=MYSYMPERMMC64AMD;
  }
  else {
     perm0=SYMPERMAMD;
     perm =SYMPERMAMD;
     permf=SYMPERMAMD;
  }
  param->ipar[6]&=~AGGRESSIVE_DROPPING;
  param->ipar[6]&=~FINAL_PIVOTING;


  // if ICNTL is NOT set then we keep the elbow space
  // in order to re-factor the matrix.
  // otherwise we discard the memory
  if (ICNTL[1]==0) {
     param->ipar[6]|=RE_FACTOR;
  }
  else
     param->ipar[6]&=~RE_FACTOR;

  ierr=MYSYMAMGFACTOR(&A, PRE, nlev, param, perm0,perm, permf);

  // if no dynamic recomputation of the preconditioner is requested
  // then we return the true elbow space factor that was used on output
  *elbow=MAX(ILUPACK_mem[4],ILUPACK_mem[5])/(A.ia[A.nc]-1.0)+.05;
  *condest=param->fpar[2];

  return (ierr);

} // ILUPACKFACTOR
