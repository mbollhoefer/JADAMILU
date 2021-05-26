#include <string.h>
#include <stdio.h>
#include <ilupack.h>
#include <ilupackmacros.h>

// #define PRINT_INFO


#ifdef _DOUBLE_REAL_
#define MYSYMILUPACKFACTORGEP     dsymamgfactorgep
#define MYSYMAMGINIT           DSYMAMGinit
#define MYSYMAMGFACTORGEP         DSYMAMGfactor
#define MYSYMAMGGETPARAMS      DSYMAMGgetparams
#define MYSYMAMGSETPARAMS      DSYMAMGsetparams
#define MYSYMPERMMC64AMD       DSYMperm_mc64_amd


#elif defined _SINGLE_REAL_
#define MYSYMILUPACKFACTORGEP     ssymamgfactorgep
#define MYSYMAMGINIT           SSYMAMGinit
#define MYSYMAMGFACTORGEP         SSYMAMGfactor
#define MYSYMAMGGETPARAMS      SSYMAMGgetparams
#define MYSYMAMGSETPARAMS      SSYMAMGsetparams
#define MYSYMPERMMC64AMD       SSYMperm_mc64_amd


#elif defined _SINGLE_COMPLEX_

#ifdef _COMPLEX_SYMMETRIC_
#define MYSYMILUPACKFACTORGEP     csymamgfactorgep
#define MYSYMAMGINIT           CSYMAMGinit
#define MYSYMAMGFACTORGEP         CSYMAMGfactor
#define MYSYMAMGGETPARAMS      CSYMAMGgetparams
#define MYSYMAMGSETPARAMS      CSYMAMGsetparams
#define MYSYMPERMMC64AMD       CSYMperm_mc64_amd
#else
#define MYSYMILUPACKFACTORGEP     cheramgfactorgep
#define MYSYMAMGINIT           CHERAMGinit
#define MYSYMAMGFACTORGEP         CHERAMGfactor
#define MYSYMAMGGETPARAMS      CHERAMGgetparams
#define MYSYMAMGSETPARAMS      CHERAMGsetparams
#define MYSYMPERMMC64AMD       CHERperm_mc64_amd
#endif


#else

#ifdef _COMPLEX_SYMMETRIC_
#define MYSYMILUPACKFACTORGEP     zsymamgfactorgep
#define MYSYMAMGINIT           ZSYMAMGinit
#define MYSYMAMGFACTORGEP         ZSYMAMGfactor
#define MYSYMAMGGETPARAMS      ZSYMAMGgetparams
#define MYSYMAMGSETPARAMS      ZSYMAMGsetparams
#define MYSYMPERMMC64AMD       ZSYMperm_mc64_amd
#else 
#define MYSYMILUPACKFACTORGEP     zheramgfactorgep
#define MYSYMAMGINIT           ZHERAMGinit
#define MYSYMAMGFACTORGEP         ZHERAMGfactor
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

integer sdsymamgfactorgep(size_t          *Fparam, 
		       size_t          *FPREC,
		       integer         *nlev,
		       integer         *ICNTL,
		       integer         *n,
		       integer         *ia,
		       integer         *ja,
		       doubleprecision *a,
		       integer         *ib,
		       integer         *jb,
		       doubleprecision *b,
		       doubleprecision *shift,
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

  integer i,j,k,l, *sja, *sia, myn=MYABS(*n);

  // copy a-shift*b
  sia=(integer *)MALLOC((myn+1)*sizeof(integer), "singleilupackfactorgep:A.ia");
  i=ia[myn]+ib[myn];
  sja=(integer *)MALLOC(i*sizeof(integer),       "singleilupackfactorgep:A.ja");
  sa =(real *)   MALLOC(i*sizeof(real),          "singleilupackfactorgep:A.a");

  // printf("set up matrix, nnz<=%d\n",i);fflush(stdout);
  sia[0]=1;
  k=0;
  for (i=0; i<myn; i++) {
      j=ia[i]-1;
      l=ia[i+1]-ia[i];
      /* sort current row of A */
      Dqsort(a+j,ja+j,sja+k,&l);

      j=ib[i]-1;
      l=ib[i+1]-ib[i];
      /* sort current row of B */
      Dqsort(b+j,jb+j,sja+k,&l);

      /* merge A and B performing A-shift*B */
      j=ia[i]-1;
      l=ib[i]-1; 
      while (j<ia[i+1]-1 && l<ib[i+1]-1) {
	    if (ja[j]==jb[l]) {
	       sja[k]=ja[j];
	       sa[k++]=a[j++]-*shift*b[l++];
	    }
	    else if (ja[j]<jb[l]) {
	       sja[k]=ja[j];
	       sa[k++]=a[j++];
	    }
	    else {/* ja[j]>jb[l] */
	       sja[k]=jb[l];
	       sa[k++]=-*shift*b[l++];
	    }
      } /* end while */
      while (j<ia[i+1]-1) {
	    sja[k]=ja[j];
	    sa[k++]=a[j++];
      } /* end while */
      while (l<ib[i+1]-1) {
	    sja[k]=jb[l];
	    sa[k++]=-*shift*b[l++];
      } /* end while */

      sia[i+1]=k+1;
  } // end for i
  // printf("matrix copied\n");fflush(stdout);


  i=ssymamgfactor(Fparam, FPREC, nlev, ICNTL, n,sia,sja,sa, matching,
		  ordering, &sdroptol, &scondest, &srestol, maxit,
		  &selbow, lfil, lfilS, nrestart);
  
  free(sa);
  free(sja);
  free(sia);
  return(i);
}

#else


integer czheramgfactorgep(size_t          *Fparam, 
		       size_t          *FPREC,
		       integer         *nlev,
		       integer         *ICNTL,
		       integer         *n,
		       integer         *ia,
		       integer         *ja,
		       doublecomplex   *a,
		       integer         *ib,
		       integer         *jb,
		       doublecomplex   *b,
		       doubleprecision *shift,
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

  integer i,j,k,l, *sja, *sia, myn=MYABS(*n);

  // copy a-shift*b
  sia=(integer *)MALLOC((myn+1)*sizeof(integer), "singleilupackfactorgep:A.ia");
  i=ia[myn]+ib[myn];
  sja=(integer *)MALLOC(i*sizeof(integer),       "singleilupackfactorgep:A.ja");
  sa =(complex *)MALLOC(i*sizeof(complex),       "singleilupackfactorgep:A.a");

  // printf("set up matrix, nnz<=%d\n",i);fflush(stdout);
  sia[0]=1;
  k=0;
  for (i=0; i<myn; i++) {
      j=ia[i]-1;
      l=ia[i+1]-ia[i];
      /* sort current row of A */
      Zqsort(a+j,ja+j,sja+k,&l);

      j=ib[i]-1;
      l=ib[i+1]-ib[i];
      /* sort current row of B */
      Zqsort(b+j,jb+j,sja+k,&l);

      /* merge A and B performing A-shift*B */
      j=ia[i]-1;
      l=ib[i]-1; 
      while (j<ia[i+1]-1 && l<ib[i+1]-1) {
	    if (ja[j]==jb[l]) {
	       sja[k]=ja[j];
	       sa[k].r  =a[j].r  -*shift*b[l].r;
	       sa[k++].i=a[j++].i-*shift*b[l++].i;
	    }
	    else if (ja[j]<jb[l]) {
	       sja[k]=ja[j];
	       sa[k].r  =a[j].r;
	       sa[k++].i=a[j++].i;
	    }
	    else {/* ja[j]>jb[l] */
	       sja[k]=jb[l];
	       sa[k].r  =-*shift*b[l].r;
	       sa[k++].i=-*shift*b[l++].i;
	    }
      } /* end while */
      while (j<ia[i+1]-1) {
	    sja[k]=ja[j];
	    sa[k].r  =a[j].r;
	    sa[k++].i=a[j++].i;
      } /* end while */
      while (l<ib[i+1]-1) {
	    sja[k]=jb[l];
	    sa[k].r  =-*shift*b[l].r;
	    sa[k++].i=-*shift*b[l++].i;
      } /* end while */

      sia[i+1]=k+1;
  } // end for i
  // printf("matrix copied\n");fflush(stdout);


  i=cheramgfactor(Fparam, FPREC, nlev, ICNTL, n,sia,sja,sa, matching,
		ordering, &sdroptol, &scondest, &srestol, maxit,
		&selbow, lfil, lfilS, nrestart);

  free(sa);
  free(sja);
  free(sia);
  return (i);
}

#endif
