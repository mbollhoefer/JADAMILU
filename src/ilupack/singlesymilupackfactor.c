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
#define MYABS(A)          (((A)>=0)?(A):(-(A)))



#ifdef _SINGLE_REAL_ 
void ilupackdummy10011() {
}
#elif defined _SINGLE_COMPLEX_
void ilupackdummy10012() {
}

#elif defined _DOUBLE_REAL_

integer sdsymamgfactor(size_t          *Fparam, 
		       size_t          *FPREC,
		       integer         *nlev,
		       integer         *ICNTL,
		       integer         *n,
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

  real *sa,
       sdroptol=*droptol,
       scondest=*condest,
       srestol =*restol,
       selbow  =*elbow;

  integer nz=ia[MYABS(*n)]-1, i, *sja;

  sa =(real *)MALLOC(nz*sizeof(real),"singlesymilupackfactor");
  // indices might and typically will be reordered
  sja=(integer *)MALLOC(nz*sizeof(integer),"singlesymilupackfactor");

  for (i=0; i<nz; i++) {
      sa[i] =a[i];
      sja[i]=ja[i];
  }

  i=ssymamgfactor(Fparam, FPREC, nlev, ICNTL, n,ia,sja,sa, matching,
		  ordering, &sdroptol, &scondest, &srestol, maxit,
		  &selbow, lfil, lfilS, nrestart);
  
  free(sa);
  free(sja);
  return(i);
}

#else


integer czheramgfactor(size_t          *Fparam, 
		       size_t          *FPREC,
		       integer         *nlev,
		       integer         *ICNTL,
		       integer         *n,
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

  complex *sa;
  real    sdroptol=*droptol,
          scondest=*condest,
          srestol =*restol,
          selbow  =*elbow;

  integer nz=ia[*n]-1, i, *sja;

  sa=(complex *)MALLOC(nz*sizeof(complex),"singlesymilupackfactor");
  // indices might and typically will be reordered
  sja=(integer *)MALLOC(nz*sizeof(integer),"singlesymilupackfactor");

  for (i=0; i<nz; i++) {
      sa[i].r=a[i].r;
      sa[i].i=a[i].i;
      sja[i]=ja[i];
  }


  i=cheramgfactor(Fparam, FPREC, nlev, ICNTL, n,ia,sja,sa, matching,
		ordering, &sdroptol, &scondest, &srestol, maxit,
		&selbow, lfil, lfilS, nrestart);

  free(sa);
  free(sja);
  return (i);
}

#endif
