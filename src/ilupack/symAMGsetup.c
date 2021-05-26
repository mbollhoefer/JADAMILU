#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>


#include <lapack.h>
#include <ilupack.h>
#include <ilupackmacros.h>


//#define PRINT_MEM
#define MEGA      1048576.0
#define IMEGA      262144.0
#define DMEGA      131072.0

#if defined _DOUBLE_REAL_ || defined _SINGLE_REAL_
#define CONJG(A)      (A)

#ifdef _DOUBLE_REAL_
#define SYMPILUC             DSYMpiluc
#define SYMILUC              DSYMiluc
#define MYSMATVEC            DSYMmatvec
#define MYSYMAMGFACTOR       DSYMAMGfactor
#define MYSYMAMGDELETE       DSYMAMGdelete
#define MYSYMAMGEXTRACT      DSYMAMGextract
#define MYSPTRF              dsptrf

#else
#define SYMPILUC             SSYMpiluc
#define SYMILUC              SSYMiluc
#define MYSMATVEC            SSYMmatvec
#define MYSYMAMGFACTOR       SSYMAMGfactor
#define MYSYMAMGDELETE       SSYMAMGdelete
#define MYSYMAMGEXTRACT      SSYMAMGextract
#define MYSPTRF              ssptrf
#endif



#else



#ifdef _COMPLEX_SYMMETRIC_
#define CONJG(A)     (A)

#ifdef _SINGLE_COMPLEX_
#define SYMPILUC             CSYMpiluc
#define SYMILUC              CSYMiluc
#define MYSMATVEC            CSYMmatvec
#define MYSYMAMGFACTOR       CSYMAMGfactor
#define MYSYMAMGDELETE       CSYMAMGdelete
#define MYSYMAMGEXTRACT      CSYMAMGextract
#define MYSPTRF              csptrf
#else
#define SYMPILUC             ZSYMpiluc
#define SYMILUC              ZSYMiluc
#define MYSMATVEC            ZSYMmatvec
#define MYSYMAMGFACTOR       ZSYMAMGfactor
#define MYSYMAMGDELETE       ZSYMAMGdelete
#define MYSYMAMGEXTRACT      ZSYMAMGextract
#define MYSPTRF              zsptrf
#endif

#else

#ifdef _SINGLE_COMPLEX_
#define CONJG(A)     (conjg(A))
#define SYMPILUC             CHERpiluc
#define SYMILUC              CHERiluc
#define MYSMATVEC            CHERmatvec
#define MYSYMAMGFACTOR       CHERAMGfactor
#define MYSYMAMGDELETE       CHERAMGdelete
#define MYSYMAMGEXTRACT      CHERAMGextract
#define MYSPTRF              chptrf
#else
#define CONJG(A)     (dconjg(A))
#define SYMPILUC             ZHERpiluc
#define SYMILUC              ZHERiluc
#define MYSMATVEC            ZHERmatvec
#define MYSYMAMGFACTOR       ZHERAMGfactor
#define MYSYMAMGDELETE       ZHERAMGdelete
#define MYSYMAMGEXTRACT      ZHERAMGextract
#define MYSPTRF              zhptrf
#endif

#endif



#endif



#define MAX_LINE        255
#define STDERR          stderr
#define STDOUT          stdout
#define MAX(A,B)        (((A)>(B))?(A):(B))
#define MIN(A,B)        (((A)<(B))?(A):(B))

//#define PRINT_INLINE
//#define PRINT_INFO2





/*
  main driver for the multilevel ILU

  given a symmetrically structured A, a sequence of permutations and 
  incomplete LU factorizations is performed. Partially we obtain an
  approximate partial factorization
          /  B  F \   /U^T   0\ /D  0\ /U UF\
  P^TAP = |       | ~ |       | |    | |    |
          \ F^T C /   \UF^T  I/ \0  S/ \0 I /
  The D,U as well as p,p^{-1} and F are kept and the multilevel strategy
  is repeated for S
*/
integer MYSYMAMGFACTOR(CSRMAT *A, AMGLEVELMAT *PRE, integer *nlev, 
		  ILUPACKPARAM *IPparam,
		  integer (*perm0)(CSRMAT, FLOAT *, FLOAT *, integer *, integer *, integer *, 
			       ILUPACKPARAM *),
		  integer (*perm) (CSRMAT, FLOAT *, FLOAT *, integer *, integer *, integer *, 
			       ILUPACKPARAM *),
		  integer (*permf)(CSRMAT, FLOAT *, FLOAT *, integer *, integer *, integer *, 
			       ILUPACKPARAM *))
{
/*
  A        input matrix, matrix is altered by the function
           diagonal scaling is directly applied to A
           only the diagonal part of A and its upper triangular part
           are stored
  PRE      Linked list of partial preconditioners
  nlev     number of levels(blocks)
  IPparam  ILUPACK parameter list
  perm0    function for initial scaling and preprocessing, applied to
           the initial system only
  perm     regular reordering/scaling applied to all subsequent levels
  permf    final scaling/pivoting strategy, applied when perm has failed
    
           perm...(A,rowscale,colscale,p,invq,nB,IPparam)
           - rescales A, returns the results in rowscale and colscale
           - permutes the rows of A and returns the permutation vector in p
           - permutes similarly the columns of A and returns the inverse 
           - permutation vector of p in invq
           - return the size nB of the leading block B
                     /  B  F \
             P^TAP = |       |
                     \ F^T C /
             that hopefully can be safely factored

  Code written by Matthias Bollhoefer November 2003
*/


  integer i,j,k,n=A->nr, PILUCparam, nnz, nB;
  integer  *jlu, ierr=0, regular_pivoting, *ia, *ja;
  integer lfil[2], param, discardA, shiftA;
  REALS droptols[3], condest, condest0, condest1, amgcancel, ilucancel; 
  REALS seps, ptol, elbow;
  CSRMAT S;
  AMGLEVELMAT *current;
  FLOAT *alu, *a, *r,*c;
  float  systime, time_start, time_stop, secnds, 
         ILUP_sec[ILUPACK_secnds_length];
  LONG_INT imem, myimem,mymaximem;
  size_t   nibuff, ndbuff, mem, *delta;




  // init time counters
  evaluate_time(&time_start,&systime);
  // counter for the total setup time
  for (i=0; i<ILUPACK_secnds_length; i++)
      ILUP_sec[i]=0;
  ILUP_sec[7]=time_start;

  // init memory counters
  for (i=0; i<ILUPACK_mem_length; i++)
      ILUPACK_mem[i]=0;


  // for pildlc the integer buffer is required to have at least
  //    11*n for the simple Schur complement (S version)
  //    14*n for the Tismenetsky-like Schur complement (T version)
  // for mpiluc the integer buffer is required to have at least
  //    12*n for the simple Schur complement (S version)
  //    15*n for the Tismenetsky-like Schur complement (T version)

  // analogous settings for the floating point buffer
  //    2*n  (not improved version)
  //    3*n  (improved version or medium Schur complement version)


  elbow  =IPparam->elbow;
  nibuff =IPparam->nibuff;
  ndbuff =IPparam->ndbuff;
  lfil[0]=IPparam->ipar[3];
  lfil[1]=IPparam->ipar[9];
  param  =IPparam->ipar[6];

  droptols[0]=IPparam->fpar[0];
  droptols[1]=IPparam->fpar[1];
  droptols[2]=IPparam->fpar[8];
  condest    =IPparam->fpar[2];
  condest0=condest;
  ilucancel  =IPparam->fpar[3];
  amgcancel  =IPparam->fpar[4]; 

  //  if (param&RE_FACTOR)
  //   printf("entering ILU, memory should be kept\n");fflush(stdout);


  // analyze how the size of the buffer has to be chosen
  if (param&TISMENETSKY_SC) {
     if (param&MULTI_PILUC)
        nibuff=MAX(nibuff,17*(size_t)n);
     nibuff=MAX(nibuff,16*(size_t)n);
  }
  else {
     if (param&MULTI_PILUC)
        nibuff=MAX(nibuff,14*(size_t)n);
     nibuff=MAX(nibuff,13*(size_t)n); // recommended version!
  }

  if (param&DROP_INVERSE) {
     // if a more reliable estimate for the inverse is required
     if (param&IMPROVED_ESTIMATE || !(param&(SIMPLE_SC|TISMENETSKY_SC)))
        ndbuff=MAX(ndbuff,4*(size_t)n); // recommended version!
     else 
        ndbuff=MAX(ndbuff,3*(size_t)n);
  }
  else {
     // currently the norm of the inverse factors are computed
     // even if classical dropping is desired
     // The comments in (m)piluc still refer to their predecessor
     // iluc, which allows classical dropping in a strict sense

     // if a more reliable estimate for the inverse is required
     if (param&IMPROVED_ESTIMATE || !(param&(SIMPLE_SC|TISMENETSKY_SC)))
        ndbuff=MAX(ndbuff,4*(size_t)n);
     else 
        ndbuff=MAX(ndbuff,3*(size_t)n);
  }


  /* integer and floating point buffer */
#ifdef PRINT_MEM
  printf("ibuffmem=%8.2fMB, dbuffmem=%8.2fMB\n",IPparam->nibuff/IMEGA,IPparam->ndbuff/DMEGA);fflush(stdout);
#endif
  if (IPparam->nibuff==0) {
     // printf("no ibuff, allocating memory for the preconditioner\n");fflush(stdout);
     IPparam->nibuff=nibuff;
     IPparam->ibuff=(integer *)  MALLOC(IPparam->nibuff*sizeof(integer),
				    "symAMGsetup:ibuff");
#ifdef PRINT_MEM
     printf("ibuff(%8.2fMB) allocated\n",nibuff/IMEGA);fflush(stdout);
#endif
  }     
  else if (nibuff>IPparam->nibuff) {
     IPparam->nibuff=nibuff;
     IPparam->ibuff=(integer *)  REALLOC(IPparam->ibuff,
				     IPparam->nibuff*sizeof(integer),
				     "symAMGsetup:ibuff");
#ifdef PRINT_MEM
     printf("ibuff(%8.2fMB) re-allocated\n",nibuff/IMEGA);fflush(stdout);
#endif
  }
  if (IPparam->ndbuff==0) {
     IPparam->ndbuff=ndbuff;
     // printf("no dbuff, allocating memory for the preconditioner\n");fflush(stdout);
     IPparam->dbuff=(FLOAT *)MALLOC(IPparam->ndbuff*sizeof(FLOAT),
				    "symAMGsetup:dbuff");
#ifdef PRINT_MEM
     printf("dbuff(%8.2fMB) allocated\n",ndbuff/DMEGA);fflush(stdout);
#endif
  } 
  else if (ndbuff>IPparam->ndbuff) {
     IPparam->ndbuff=ndbuff;
     IPparam->dbuff=(FLOAT *)REALLOC(IPparam->dbuff,
				     IPparam->ndbuff*sizeof(FLOAT),
				     "symAMGsetup:dbuff");
#ifdef PRINT_MEM
     printf("dbuff(%8.2fMB) re-allocated\n",ndbuff/DMEGA);fflush(stdout);
#endif
  }

  // keep track on the amount of memory consumed by the buffers
  ILUPACK_mem[0]=nibuff;
  ILUPACK_mem[1]=ndbuff;



  // total amount of memory requested by the parameters
  mem=elbow*(size_t)A->ia[A->nr];
#ifdef PRINT_MEM
  printf("jlumem=%8.2fMB, alumem=%8.2fMB\n",IPparam->njlu/IMEGA,IPparam->nalu/DMEGA);fflush(stdout);
#endif
  if (IPparam->njlu==0) {
     IPparam->njlu=mem;
     // printf("no jlu, allocating memory for the preconditioner\n");fflush(stdout);
     IPparam->jlu=(integer *)  MALLOC(IPparam->njlu*sizeof(integer),
				  "symAMGsetup:jlu");
#ifdef PRINT_MEM
     printf("jlu(%8.2lfMB) allocated\n",mem/IMEGA);fflush(stdout);
#endif
  }
  else if (mem>IPparam->njlu) {
     IPparam->njlu=mem;
     IPparam->jlu=(integer *) REALLOC(IPparam->jlu,
				  IPparam->njlu*sizeof(integer),
				  "symAMGsetup:jlu");
#ifdef PRINT_MEM
     printf("jlu(%8.2fMB) re-allocated\n",mem/IMEGA);fflush(stdout);
#endif
  }
  jlu=IPparam->jlu;
  if (IPparam->nalu==0) {
     IPparam->nalu=mem;
     // printf("no alu, allocating memory for the preconditioner\n");fflush(stdout);
     IPparam->alu=(FLOAT *) MALLOC(IPparam->nalu*sizeof(FLOAT),
				   "symAMGsetup:alu");
#ifdef PRINT_MEM
     printf("alu(%8.2fMB) allocated\n",mem/DMEGA);fflush(stdout);
#endif
  }
  else if (mem>IPparam->nalu) {
     IPparam->nalu=mem;
     IPparam->alu=(FLOAT *)REALLOC(IPparam->alu,
				   IPparam->nalu*sizeof(FLOAT),
				   "symAMGsetup:alu");
#ifdef PRINT_MEM
     printf("alu(%8.2fMB) re-allocated\n",mem/DMEGA);fflush(stdout);
#endif
  }
  alu=IPparam->alu;



  if (param&FINAL_PIVOTING)
     regular_pivoting=-1;
  else 
     regular_pivoting=0;
  // default settings for PILUC within ARMS oo PILUC
  PILUCparam=COARSE_REDUCE; //DROP_INVERSE|
  if (param&IMPROVED_ESTIMATE)
     PILUCparam|=IMPROVED_ESTIMATE;
  if (param&DROP_INVERSE)
     PILUCparam|=DROP_INVERSE;
  // additional legal options
  if (param&TISMENETSKY_SC)
     PILUCparam|=TISMENETSKY_SC;
  else if (param&SIMPLE_SC)
    PILUCparam|=SIMPLE_SC;
  if (param&DIAGONAL_COMPENSATION)
     PILUCparam|=DIAGONAL_COMPENSATION;

  if (param&AGGRESSIVE_DROPPING)
     PILUCparam|=AGGRESSIVE_DROPPING;

  discardA=0;
  if (param&DISCARD_MATRIX) {
     discardA=DISCARD_MATRIX;
  }

  // store the logical gap between the allocated memory areas alu,jlu
  // and the pointers to the subsystems
  delta=(size_t *)MALLOC(A->nr*sizeof(size_t),"symAMGsetup:delta");
  for (i=0; i<A->nr; i++)
    delta[i]=0;
  /* block LU decomposition */
  S=*A; 

  n=(1+1/amgcancel)*S.nr;
  current=PRE;
  current->prev=NULL;
  *nlev=0;

  
  // while (0<S.nr & S.nr<=amgcancel*n) {
  while (0<S.nr) {
	(*nlev)++;
	
	// printf("lev %d\n",*nlev);fflush(stdout);

        n=S.nr;
	if (*nlev>1)
	{
	   current->next=(AMGLEVELMAT *)MALLOC((size_t)1*sizeof(AMGLEVELMAT),
					       "symAMGsetup:current->next");
#ifdef PRINT_INFO2
	   printf("lev %d, next level allocated\n",*nlev);fflush(stdout);
#endif
	   current->next->prev=current;
	   current=current->next;
	}
	current->n=n;
	current->next=NULL;
	current->absdiag=NULL;
	

	// start counter for preprocessing/reordering/pivoting  and  scaling
	evaluate_time(&time_start,&systime);
	/* row and column scaling if desired */
	current->colscal=(FLOAT *)MALLOC((size_t)n*sizeof(FLOAT),
					 "symAMGsetup:current->colscal");
	current->rowscal=current->colscal;
	ILUPACK_mem[9]+=n;
#ifdef PRINT_INFO2
	printf("lev %d, colscal allocated\n",*nlev);fflush(stdout);
#endif

	for (i=0; i<n; i++) {
#if defined _DOUBLE_REAL_ || defined _SINGLE_REAL_
	    current->colscal[i]=1;
#else
	    current->colscal[i].r=1;
	    current->colscal[i].i=0;
#endif
	}

	current->p   =(integer *)MALLOC((size_t)n*sizeof(integer),"symAMGsetup:current->p");
	current->invq=(integer *)MALLOC((size_t)n*sizeof(integer),"symAMGsetup:current->invq");
	ILUPACK_mem[8]+=2*n;
	for (i=0; i<n; i++)
	  current->p[i]=current->invq[i]=i+1;
	current->nB=current->n;
#ifdef PRINT_INFO2
	printf("lev %d, p,invq allocated\n",*nlev);fflush(stdout);
#endif


	// auxilliary memory that is currently left over
	IPparam->niaux=IPparam->ndaux=mem;
	IPparam->iaux=jlu;
	IPparam->daux=alu;


	/* check whether the matrix is almost dense */
	/* We define a matrix to be dense if nnz(S)>1/3 n(n+1)/2 */
	nnz=S.ia[n]-1;

#ifdef PRINT_INLINE
	printf("n=%7d, nnz=%9d, %6.1fav",n,nnz,((double)nnz)/n);
#endif

	if (*nlev>1 && ilucancel*nnz>((REALS)n)*(n+1)/2.0) {

#ifdef PRINT_INLINE
	   printf("\nswitched to full matrix processing\n");fflush(STDOUT);
#endif

	   // start counter for SPTRF
	   evaluate_time(&time_start,&systime);
	   /* uncompress S if possible */
	   if (mem<(n*(size_t)(n+1))/2-nnz) {
	      evaluate_time(&time_stop,&systime);
	      // compute total setup time
	      ILUP_sec[7]=time_stop-ILUP_sec[7];
	      // compute TOTAL time
	      ILUP_sec[8]=time_stop-ILUP_sec[8];
	      for (i=0; i<ILUPACK_secnds_length; i++)
		  ILUPACK_secnds[i]=ILUP_sec[i];
	      ierr=-2;
	      current->E.ia=current->F.ia=NULL;
	      current->E.ja=current->F.ja=NULL;
	      current->E.a =current->F.a =NULL;
	      current->LU.ja=jlu;
	      current->absdiag=NULL;

	      // undo scaling
	      r=PRE->rowscal;
	      c=PRE->colscal;
	      // adjust arrays
	      ia=A->ia;
	      ja=A->ja;
	      a =A->a;
	      ja--;
	      a--;
	      c--;
	      for (i=0; i<A->nr; i++) {
		for (j=ia[i]; j<ia[i+1]; j++) {
		    k=ja[j];
#if defined _SINGLE_REAL_ || defined _DOUBLE_REAL_
		    a[j]/=r[i]*c[k];
#else
		    a[j].r/=r[i].r*c[k].r;
		    a[j].i/=r[i].r*c[k].r;
#endif	  
		}
	      }

	      // MYSYMAMGDELETE(*A, *PRE, *nlev, IPparam);
	      return (ierr);
	   }
	   
	   /* position in alu behind the uncompressed matrix */
	   alu+=(n*(size_t)(n+1))/2-nnz;
	   delta[*nlev-1]-=nnz;
	   // additional memory needed
	   ILUPACK_mem[2]+=0;
	   ILUPACK_mem[3]+=(n*(size_t)(n+1))/2-nnz;
	   ILUPACK_mem[5]=MAX(ILUPACK_mem[5],ILUPACK_mem[3]);

	   S.ja--;
	   S.a--;
	   for (i=n-1; i>=0; i--) {
	       for (j=i; j<n; j++) 
#if defined _DOUBLE_REAL_ || defined _SINGLE_REAL_
		   IPparam->dbuff[j]=0;
#else
	           IPparam->dbuff[j].r=IPparam->dbuff[j].i=0;
#endif
	       IPparam->dbuff--;
	       for (j=S.ia[i]; j<S.ia[i+1]; j++) {
#if defined _DOUBLE_REAL_ || defined _SINGLE_REAL_ || defined _COMPLEX_SYMMETRIC_
		   IPparam->dbuff[S.ja[j]]=S.a[j];
#else /* _SINGLE_COMPLEX_ "_DOUBLE_COMPLEX_", "_COMPLEX_HERMITIAN_" */
		   IPparam->dbuff[S.ja[j]].r= S.a[j].r;
		   IPparam->dbuff[S.ja[j]].i=-S.a[j].i;
#endif
	       }
	       IPparam->dbuff++;
	       alu-=(n-i);
	       for (j=i; j<n; j++)
		   alu[j-i]=IPparam->dbuff[j];
	   }
	   S.ja++;
	   S.a++;
	   current->LUperm=NULL;
	   
#ifdef PRINT_INFO
	   printf("dense A\n");fflush(STDOUT);
	   for (i=0; i<n; i++)
	     printf("%12d",S.ia[i]);
	   printf("\n");fflush(stdout);
	   j=0;
	   for (k=0; k<n; k++) {
	     for (i=k; i<n; i++) {
	       printf("%12.4le",S.a[j++]);
	     }
	     printf("\n");fflush(stdout);
	   }
	   printf("\n");fflush(stdout);
#endif
	   MYSPTRF("L",&n,S.a,S.ia,&ierr,1); 
#ifdef PRINT_INFO
	   printf("dense U\n");fflush(STDOUT);
	   for (i=0; i<n; i++)
	     printf("%12d",S.ia[i]);
	   printf("\n");fflush(stdout);
	   j=0;
	   for (k=0; k<n; k++) {
	     for (i=k; i<n; i++) {
	       printf("%12.4le",S.a[j++]);
	     }
	     printf("\n");fflush(stdout);
	   }
	   printf("\n");fflush(stdout);
#endif
	   // store SPTRF computation time
	   evaluate_time(&time_stop,&systime);
	   ILUP_sec[4]=time_stop-time_start;

	   if (ierr) { 
	      // compute total setup time
	      ILUP_sec[7]=time_stop-ILUP_sec[7];
	      // compute TOTAL time
	      ILUP_sec[8]=time_stop-ILUP_sec[8];
	      for (i=0; i<ILUPACK_secnds_length; i++)
	          ILUPACK_secnds[i]=ILUP_sec[i];
	      current->E.ia=current->F.ia=NULL;
	      current->E.ja=current->F.ja=NULL;
	      current->E.a =current->F.a =NULL;
	      current->LU.ja=jlu;
	      current->absdiag=NULL;

	      // undo scaling
	      r=PRE->rowscal;
	      c=PRE->colscal;
	      // adjust arrays
	      ia=A->ia;
	      ja=A->ja;
	      a=A->a;
	      ja--;
	      a--;
	      c--;
	      for (i=0; i<A->nr; i++) {
		for (j=ia[i]; j<ia[i+1]; j++) {
		    k=ja[j];
#if defined _SINGLE_REAL_ || defined _DOUBLE_REAL_
		    a[j]/=r[i]*c[k];
#else
		    a[j].r/=r[i].r*c[k].r;
		    a[j].i/=r[i].r*c[k].r;
#endif	  
		}
	      }

	      // MYSYMAMGDELETE(*A, *PRE, *nlev, IPparam);
	      return (ierr);
	   }
	   current->nB=n;
	     
	   current->E.nr=current->F.nc=0;
	   current->E.nc=current->F.nr=0;
	   
	   current->LU.nr=current->nB;
	   current->LU.nc=current->LU.nr;
	   current->LU.a =alu;
	   current->LU.ja=NULL;
	   current->LU.ia=S.ia;

	   n=-1;
	   S.nr=0;
	}
	else { /* the matrix is still sparse */

	  // apply reordering techniques 
	  
	  current->absdiag=NULL;
	  // initial reordering
	  if (*nlev==1 && (param&PREPROCESS_INITIAL_SYSTEM)) {
	     IPparam->ipar[7]&=~1024;
	     IPparam->ipar[7]|=512;
	     ierr=(*perm0)(S, current->rowscal, current->colscal,current->p,
			   current->invq, &current->nB, IPparam);
	     IPparam->ipar[7]&=~(512+1024);
	  }
	  else 
	    // final pivoting
	    if (*nlev>1 && (param&FINAL_PIVOTING) && !regular_pivoting) { 
	       IPparam->ipar[7]|=512+1024;
	       ierr=(*permf)(S, current->rowscal, current->colscal, current->p,
			     current->invq, &current->nB, IPparam);
	       IPparam->ipar[7]&=~(512+1024);
	    } // end if-elseif-if
	    else
	       // regular pivoting
	       if (*nlev>1 && (param&PREPROCESS_SUBSYSTEMS)) { 
		  IPparam->ipar[7]&=~512;
		  IPparam->ipar[7]|=1024;
		  ierr=(*perm)(S, current->rowscal, current->colscal, current->p,
			       current->invq, &current->nB, IPparam);
		  IPparam->ipar[7]&=~(512+1024);
	       } // end if-elseif-elseif

	  // mymaximem=MAX(ILUPACK_mem[12],ILUPACK_mem[13]);

	  evaluate_time(&time_stop,&systime);
	  if (*nlev==1)
	     // time for initial preprocessing + scaling
	     ILUP_sec[0] =time_stop-time_start;
	  else
	     // accumulated time for reordering/pivoting and scaling 
	     // the remaining systems
	     ILUP_sec[1]+=time_stop-time_start;

	  if (ierr) {
	     // compute total setup time
	     ILUP_sec[7]=time_stop-ILUP_sec[7];
	     // compute TOTAL time
	     ILUP_sec[8]=time_stop-ILUP_sec[8];
	     for (i=0; i<ILUPACK_secnds_length; i++)
		 ILUPACK_secnds[i]=ILUP_sec[i];
	     current->E.ia=current->F.ia=NULL;
	     current->E.ja=current->F.ja=NULL;
	     current->E.a =current->F.a =NULL;
	     current->LU.ja=jlu;
	     current->absdiag=NULL;

	     // undo scaling
	     r=PRE->rowscal;
	     c=PRE->colscal;
	     // adjust arrays
	     ia=A->ia;
	     ja=A->ja;
	     a=A->a;
	     ja--;
	     a--;
	     c--;
	     for (i=0; i<A->nr; i++) {
	         for (j=ia[i]; j<ia[i+1]; j++) {
		     k=ja[j];
#if defined _SINGLE_REAL_ || defined _DOUBLE_REAL_
		     a[j]/=r[i]*c[k];
#else
		     a[j].r/=r[i].r*c[k].r;
		     a[j].i/=r[i].r*c[k].r;
#endif	  
		 }
	     }

	     // MYSYMAMGDELETE(*A, *PRE, *nlev, IPparam);
	     return (ierr);
	  }

	  if ((n-current->nB > amgcancel*n || current->nB==0) && 
	      (param&FINAL_PIVOTING) && regular_pivoting) {
	     // switch to final pivoting
#ifdef PRINT_INLINE
  	     printf("\nswitched to final pivoting\n");fflush(STDOUT);
#endif
	     regular_pivoting=0;
	     amgcancel  =IPparam->fpar[5]; 
	     current->nB=n;
	     evaluate_time(&time_start,&systime);
	     IPparam->ipar[7]|=512+1024;
	     ierr=(*permf)(S, current->rowscal, current->colscal, current->p,
			   current->invq, &current->nB, IPparam);
	     IPparam->ipar[7]&=~(512+1024);
	     // mymaximem=MAX(mymaximem,ILUPACK_mem[12]);
	     // mymaximem=MAX(mymaximem,ILUPACK_mem[13]);

	     evaluate_time(&time_stop,&systime);
	     if (*nlev==1)
	        // time for initial preprocessing + scaling
	        ILUP_sec[0] =time_stop-time_start;
	     else
	        // accumulated time for reordering/pivoting and scaling 
	        // the remaining systems
	        ILUP_sec[1]+=time_stop-time_start;

	     if (ierr) {
	        // compute total setup time
	        ILUP_sec[7]=time_stop-ILUP_sec[7];
		// compute TOTAL time
		ILUP_sec[8]=time_stop-ILUP_sec[8];
		for (i=0; i<ILUPACK_secnds_length; i++)
		    ILUPACK_secnds[i]=ILUP_sec[i];
		current->E.ia=current->F.ia=NULL;
		current->E.ja=current->F.ja=NULL;
		current->E.a =current->F.a =NULL;
		current->LU.ja=jlu;
		current->absdiag=NULL;

		// undo scaling
		r=PRE->rowscal;
		c=PRE->colscal;
		// adjust arrays
		ia=A->ia;
		ja=A->ja;
		a=A->a;
		ja--;
		a--;
		c--;
		for (i=0; i<A->nr; i++) {
		    for (j=ia[i]; j<ia[i+1]; j++) {
		        k=ja[j];
#if defined _SINGLE_REAL_ || defined _DOUBLE_REAL_
			a[j]/=r[i]*c[k];
#else
			a[j].r/=r[i].r*c[k].r;
			a[j].i/=r[i].r*c[k].r;
#endif	  
		    }
		}

		// MYSYMAMGDELETE(*A, *PRE, *nlev, IPparam);
		return (ierr);
	     }
	  } // end if


	  nB=current->nB;
	  /* perform PILUC decomposition */

	  current->LUperm=NULL;

#ifdef PRINT_INLINE
	  printf(", nB=%7d->",current->nB);fflush(STDOUT);
#endif
	  
	  if (current->nB>0) {
	     // start counter for (m)piluc
	     evaluate_time(&time_start,&systime);
	     condest1=condest;
	     condest/=2.0;
	     mymaximem=0;
	     do {
	        current->nB=nB;
		ierr=0;
	        condest*=2.0;
		// printf("level %2d, condest=%8.1le\n",*nlev,condest);fflush(stdout);
	        imem=(LONG_INT)mem;
		if (*nlev>1)
		   PILUCparam|=discardA;
		shiftA=0;
		myimem=imem;
		if (param&MULTI_PILUC){
		   SYMPILUC(&n,S.a,S.ja,S.ia,lfil,droptols,&condest,
			    // IPparam->fpar+6,
			    &current->nB,&PILUCparam,current->p,current->invq,
			    alu,jlu,&myimem,IPparam->dbuff,IPparam->ibuff,
			    &shiftA,&amgcancel,&ierr);
		}
		else {
		   SYMPILUC(&n,S.a,S.ja,S.ia,lfil,droptols,&condest,
			    &current->nB,&PILUCparam,current->p,current->invq,
			    alu,jlu,&myimem,IPparam->dbuff,IPparam->ibuff,
			    &shiftA,&amgcancel,&ierr);
		}
		mymaximem=MAX(mymaximem,myimem);
	     }
	     while (nB-current->nB>amgcancel*nB && 
		    condest<1024*condest0 && condest<16*condest1);

	     alu-=shiftA;
	     jlu-=shiftA;
	     delta[*nlev-1]-=shiftA;
	     // stop counter for (m)piluc
	     evaluate_time(&time_stop,&systime);
	     // accumulate collective time for (m)piluc
	     ILUP_sec[2]+=time_stop-time_start;
	     if (ierr) {
	        // compute total setup time
	        ILUP_sec[7]=time_stop-ILUP_sec[7];
	        // compute TOTAL time
		ILUP_sec[8]=time_stop-ILUP_sec[8];
		for (i=0; i<ILUPACK_secnds_length; i++)
		    ILUPACK_secnds[i]=ILUP_sec[i];
		current->E.ia=current->F.ia=NULL;
		current->E.ja=current->F.ja=NULL;
		current->E.a =current->F.a =NULL;
		current->LU.ja=jlu;
		current->absdiag=NULL;

		// undo scaling
		r=PRE->rowscal;
		c=PRE->colscal;
		// adjust arrays
		ia=A->ia;
		ja=A->ja;
		a=A->a;
		ja--;
		a--;
		c--;
		for (i=0; i<A->nr; i++) {
		    for (j=ia[i]; j<ia[i+1]; j++) {
		        k=ja[j];
#if defined _SINGLE_REAL_ || defined _DOUBLE_REAL_
			a[j]/=r[i]*c[k];
#else
			a[j].r/=r[i].r*c[k].r;
			a[j].i/=r[i].r*c[k].r;
#endif	  
		    }
		}

		// MYSYMAMGDELETE(*A, *PRE, *nlev, IPparam);
		return (ierr);
	     }
	  }

#ifdef PRINT_INLINE
	  printf("%7d\n",current->nB);fflush(STDOUT);
#endif


#ifdef PRINT_INFO
	  printf("1.computed U-factor\n");fflush(STDOUT);
	  for (i=0; i<current->nB; ) {
	    if (jlu[n+1+i]==0){
	      for (k=jlu[i];k<jlu[i+1]; k++) 
		printf("%8d",jlu[k-1]);
	      printf("\n");fflush(STDOUT);
	      for (k=jlu[i];k<jlu[i+1]; k++) 
		printf("%8.1le",alu[k-1]);
	      printf("\n");fflush(STDOUT);
	      i++;
	    }
	    else {
	      for (k=jlu[i];k<jlu[i+1]; k++) 
		printf("%8d",jlu[k-1]);
	      printf("\n");fflush(STDOUT);
	      for (k=jlu[i];k<jlu[i+1]; k++) 
		printf("%8.1le",alu[jlu[i]+2*(k-jlu[i])-1]);
	      printf("\n");fflush(STDOUT);
	      for (k=jlu[i];k<jlu[i+1]; k++) 
		printf("%8.1le",alu[jlu[i]+2*(k-jlu[i])]);
	      printf("\n");fflush(STDOUT);
	      i+=2;
	    }
	  }
	  printf("\n");fflush(STDOUT);
	  printf("Block diagonal factor\n");
	  for (k=0; k<current->nB;) {
	    if (jlu[n+1+k]==0){
	      printf("%8.1le",alu[k]);
	      k++;
	    }
	    else {
	      printf("%8.1le%8.1le",alu[k],alu[n+1+k]);
	      k+=2;
	    }
	  }
	  printf("\n");fflush(stdout);
	  for (k=0; k<current->nB; ) {
	    if (jlu[n+1+k]==0) {
	      printf("        ");
	      k++;
	    }
	    else {
	      printf("%8.1le%8.1le",alu[n+1+k],alu[k+1]);
	      k+=2;
	    }
	  }
	  printf("\n");fflush(stdout);
	  printf("computed Schur complement\n");fflush(STDOUT);
	  for (i=current->nB; i<n; i++) {
	    for (k=jlu[i];k<jlu[i+1]; k++) 
	      printf("%8d",jlu[k-1]);
	    printf("\n");fflush(STDOUT);
	    for (k=jlu[i];k<jlu[i+1]; k++) 
	      printf("%8.1le",alu[k-1]);
	    printf("\n");fflush(STDOUT);
	  }

	  for (i=0; i<=current->nB; i++)
	    printf("%8d",i+1);
	  printf("\n");fflush(stdout);
	  for (i=0; i<=current->nB; i++)
	    printf("%8d",jlu[i]);
	  printf("\n");fflush(stdout);
	  for (i=0; i<current->nB; i++)
	    printf("%8d",jlu[n+1+i]);
	  printf("\n");fflush(stdout);
#endif


	  
	  // COMMENT: Maybe in a later version the extraction of E and F and discarding the old
	  //          S should be done in one shot. This is reasonable, since (for *nlev>1) the old
	  //          matrix S is not needed any longer and to create some new space to store E and
	  //          F is not really necessary. But this is very technical, since the rows of S
	  //          have to be reordered in order to construct E and F :(


	  // appearently piluc failed
	  // BUT we set the final pivoting flag
	  // AND we haven't switched to final pivoting yet
	  if ((nB-current->nB > amgcancel*nB || nB==0) && 
	      (param&FINAL_PIVOTING) && regular_pivoting) {
	     // switch to final pivoting
#ifdef PRINT_INLINE
  	     printf("\nswitched to final pivoting\n");fflush(STDOUT);
#endif
	     regular_pivoting=0;
	     amgcancel  =IPparam->fpar[5]; 
	     current->nB=n;
	     evaluate_time(&time_start,&systime);
	     IPparam->ipar[7]|=512+1024;
	     ierr=(*permf)(S, current->rowscal, current->colscal, current->p,
			   current->invq, &current->nB, IPparam);
	     IPparam->ipar[7]&=~(512+1024);
	     // mymaximem=MAX(mymaximem,ILUPACK_mem[12]);
	     // mymaximem=MAX(mymaximem,ILUPACK_mem[13]);

	     evaluate_time(&time_stop,&systime);
	     if (*nlev==1)
	        // time for initial preprocessing + scaling
	        ILUP_sec[0] =time_stop-time_start;
	     else
	        // accumulated time for reordering/pivoting and scaling 
	        // the remaining systems
	        ILUP_sec[1]+=time_stop-time_start;

	     if (ierr) {
	        // compute total setup time
	        ILUP_sec[7]=time_stop-ILUP_sec[7];
		// compute TOTAL time
		ILUP_sec[8]=time_stop-ILUP_sec[8];
		for (i=0; i<ILUPACK_secnds_length; i++)
		    ILUPACK_secnds[i]=ILUP_sec[i];
		current->E.ia=current->F.ia=NULL;
		current->E.ja=current->F.ja=NULL;
		current->E.a =current->F.a =NULL;
		current->LU.ja=jlu;
		current->absdiag=NULL;

		// undo scaling
		r=PRE->rowscal;
		c=PRE->colscal;
		// adjust arrays
		ia=A->ia;
		ja=A->ja;
		a=A->a;
		ja--;
		a--;
		c--;
		for (i=0; i<A->nr; i++) {
		    for (j=ia[i]; j<ia[i+1]; j++) {
		        k=ja[j];
#if defined _SINGLE_REAL_ || defined _DOUBLE_REAL_
			a[j]/=r[i]*c[k];
#else
			a[j].r/=r[i].r*c[k].r;
			a[j].i/=r[i].r*c[k].r;
#endif	  
		    }
		}

		// MYSYMAMGDELETE(*A, *PRE, *nlev, IPparam);
		return (ierr);
	     }

	     nB=current->nB;
	     current->LUperm=NULL;

#ifdef PRINT_INLINE
	     printf(", nB=%7d->",current->nB);fflush(STDOUT);
#endif

	     if (current->nB>0) {
	        // start counter for (m)piluc
	        evaluate_time(&time_start,&systime);
		imem=(LONG_INT)mem;
		// if DISCARD_MATRIX is set, then at any level>1 the system
		// matrix is discarded as soon as possible to save memory
		if (*nlev>1)
		   PILUCparam|=discardA;
		shiftA=0;
		myimem=imem;
		if (param&MULTI_PILUC) {
		   SYMPILUC(&n,S.a,S.ja,S.ia,lfil,droptols,&condest,
			    //IPparam->fpar+6,
			    &current->nB,&PILUCparam,current->p,current->invq,
			    alu,jlu,&myimem,IPparam->dbuff,IPparam->ibuff,
			    &shiftA,&amgcancel,&ierr);
		}
		else
		   SYMPILUC(&n,S.a,S.ja,S.ia,lfil,droptols,&condest,
			    &current->nB,&PILUCparam,current->p,current->invq,
			    alu,jlu,&myimem,IPparam->dbuff,IPparam->ibuff,
			    &shiftA,&amgcancel,&ierr);
		printf("myimem=%8ld,condest=%8.1le\n",myimem,condest);
		mymaximem=MAX(mymaximem,myimem);

		alu-=shiftA;
		jlu-=shiftA;
		delta[*nlev-1]-=shiftA;
		// stop counter for (m)piluc
		evaluate_time(&time_stop,&systime);
		// accumulate collective time for (m)piluc
		ILUP_sec[2]+=time_stop-time_start;
		if (ierr) {
		   // compute total setup time
		   ILUP_sec[7]=time_stop-ILUP_sec[7];
		   // compute TOTAL time
		   ILUP_sec[8]=time_stop-ILUP_sec[8];
		   for (i=0; i<ILUPACK_secnds_length; i++)
		       ILUPACK_secnds[i]=ILUP_sec[i];
		   current->E.ia=current->F.ia=NULL;
		   current->E.ja=current->F.ja=NULL;
		   current->E.a =current->F.a =NULL;
		   current->LU.ja=jlu;
		   current->absdiag=NULL;

		   // undo scaling
		   r=PRE->rowscal;
		   c=PRE->colscal;
		   // adjust arrays
		   ia=A->ia;
		   ja=A->ja;
		   a=A->a;
		   ja--;
		   a--;
		   c--;
		   for (i=0; i<A->nr; i++) {
		       for (j=ia[i]; j<ia[i+1]; j++) {
			   k=ja[j];
#if defined _SINGLE_REAL_ || defined _DOUBLE_REAL_
			   a[j]/=r[i]*c[k];
#else
			   a[j].r/=r[i].r*c[k].r;
			   a[j].i/=r[i].r*c[k].r;
#endif	  
		       }
		   }

		   // MYSYMAMGDELETE(*A, *PRE, *nlev, IPparam);
		   return (ierr);
		}
	     }

#ifdef PRINT_INLINE
	     printf("%7d\n",current->nB);fflush(STDOUT);
#endif



#ifdef PRINT_INFO
	     printf("2.computed U-factor\n");fflush(STDOUT);
	     for (i=0; i<current->nB; ) {
	       if (jlu[n+1+i]==0){
		 for (k=jlu[i];k<jlu[i+1]; k++) 
		   printf("%8d",jlu[k-1]);
		 printf("\n");fflush(STDOUT);
		 for (k=jlu[i];k<jlu[i+1]; k++) 
		   printf("%8.1le",alu[k-1]);
		 printf("\n");fflush(STDOUT);
		 i++;
	       }
	       else {
		 for (k=jlu[i];k<jlu[i+1]; k++) 
		   printf("%8d",jlu[k-1]);
		 printf("\n");fflush(STDOUT);
		 for (k=jlu[i];k<jlu[i+1]; k++) 
		   printf("%8.1le",alu[jlu[i]+2*(k-jlu[i])-1]);
		 printf("\n");fflush(STDOUT);
		 for (k=jlu[i];k<jlu[i+1]; k++) 
		   printf("%8.1le",alu[jlu[i]+2*(k-jlu[i])]);
		 printf("\n");fflush(STDOUT);
		 i+=2;
	       }
	     }
	     printf("\n");fflush(stdout);
	     printf("Block diagonal factor\n");
	     for (k=0; k<current->nB;) {
	       if (jlu[n+1+k]==0){
		 printf("%8.1le",alu[k]);
		 k++;
	       }
	       else {
		 printf("%8.1le%8.1le",alu[k],alu[n+1+k]);
		 k+=2;
	       }
	     }
	     printf("\n");fflush(stdout);
	     for (k=0; k<current->nB; ) {
	       if (jlu[n+1+k]==0) {
		 printf("        ");
		 k++;
	       }
	       else {
		 printf("%8.1le%8.1le",alu[n+1+k],alu[k+1]);
		 k+=2;
	       }
	     }
	     printf("\n");fflush(stdout);
	     printf("computed Schur complement\n");fflush(STDOUT);
	     for (i=current->nB; i<n; i++) {
	       for (k=jlu[i];k<jlu[i+1]; k++) 
		 printf("%8d",jlu[k-1]);
	       printf("\n");fflush(STDOUT);
	       for (k=jlu[i];k<jlu[i+1]; k++) 
		 printf("%8.1le",alu[k-1]);
	       printf("\n");fflush(STDOUT);
	     }
	     for (i=0; i<=current->nB; i++)
	       printf("%8d",i+1);
	     for (i=0; i<=current->nB; i++)
	       printf("%8d",jlu[i]);
	     printf("\n");fflush(stdout);
	     printf("\n");fflush(stdout);
	     for (i=0; i<current->nB; i++)
	       printf("%8d",jlu[n+1+i]);
	     printf("\n");fflush(stdout);
#endif
	    
	  } // end if

	  
	  // maximum (peek) memory observed during sympiluc
	  ILUPACK_mem[4]=MAX(ILUPACK_mem[4],ILUPACK_mem[2]+mymaximem);
	  ILUPACK_mem[5]=MAX(ILUPACK_mem[5],ILUPACK_mem[3]+mymaximem);

	  /* if piluc reduction was successful */
	  if (nB-current->nB<=amgcancel*nB && nB>0) {
	    if (n-current->nB) {
	       MYSYMAMGEXTRACT(&current->F,S, current->p,current->invq,
			       current->nB);
	       ILUPACK_mem[6]+=current->F.nr+1
		              +current->F.ia[current->F.nr]-1;
	       ILUPACK_mem[7]+=current->F.ia[current->F.nr]-1;
	       current->E=current->F;
#ifdef PRINT_INFO2
	       printf("lev %d, F allocated\n",*nlev);fflush(stdout);
#endif
	    }
	    else {
	       current->E.ia=current->F.ia=NULL;
	       current->E.ja=current->F.ja=NULL;
	       current->E.a =current->F.a =NULL;
	       current->E.nr=current->F.nc=0;
	       current->E.nc=current->F.nr=0;
	    }

	    /* discard old S by simply shifting the new S and the L and U factors,
	       because they immediately follow S in the memory */

	    if (*nlev>1) {
	       /* number of nonzeros in the old S */
	       k=S.ia[n]-1;
	       /* number of nonzeros in U and the new S */
	       nnz=jlu[n]-1;
	       alu-=k;
	       jlu-=k;
	       delta[*nlev-1]-=k;
	       for (i=0; i<nnz; i++) {
		   alu[i]=alu[i+k];
		   jlu[i]=jlu[i+k];
	       } /* end for */
	       mem+=k;
	       ILUPACK_mem[2]-=k;
	       ILUPACK_mem[3]-=k;
	    }
	   
	    current->LU.nr=current->nB;
	    current->LU.nc=current->LU.nr;
	    current->LU.a =alu;
	    current->LU.ja=jlu;
	    current->LU.ia=NULL;
	    
	    nnz=jlu[current->nB]-1;
	    S.nr=n-current->nB; 
	    S.nc=S.nr;
	    S.a=alu+nnz;
	    S.ja=jlu+nnz;
	    S.ia=jlu+current->nB;

	    for (i=0; i<=S.nr; i++) {
#ifdef PRINT_INFO
	        printf("!%4d,%4d,%4d\n",i+1,S.ia[i],S.ia[i]-nnz);
#endif
	        S.ia[i]-=nnz;
	    }
	     
	    nnz+=jlu[n]-1;
	    alu+=nnz;
	    jlu+=nnz;
	    delta[*nlev]=delta[*nlev-1]+nnz;
	    mem-=(size_t)nnz;
	    ILUPACK_mem[2]+=(size_t)nnz;
	    ILUPACK_mem[3]+=(size_t)nnz;
	    // maximum (peek) memory observed during sympiluc
	    ILUPACK_mem[4]=MAX(ILUPACK_mem[4],ILUPACK_mem[2]);
	    ILUPACK_mem[5]=MAX(ILUPACK_mem[5],ILUPACK_mem[3]);


#ifdef PRINT_INFO
	    printf("3.computed U-factor\n");fflush(STDOUT);
	    for (i=0; i<current->nB; ) {
	      if (current->LU.ja[n+1+i]==0){
		for (k=current->LU.ja[i];k<current->LU.ja[i+1]; k++) 
		  printf("%8d",current->LU.ja[k-1]);
		printf("\n");fflush(STDOUT);
		for (k=current->LU.ja[i];k<current->LU.ja[i+1]; k++) 
		  printf("%8.1le",current->LU.a[k-1]);
		printf("\n");fflush(STDOUT);
		i++;
	      }
	      else {
		for (k=current->LU.ja[i];k<current->LU.ja[i+1]; k++) 
		  printf("%8d",current->LU.ja[k-1]);
		printf("\n");fflush(STDOUT);
		for (k=current->LU.ja[i];k<current->LU.ja[i+1]; k++) 
		  printf("%8.1le",current->LU.a[current->LU.ja[i]+2*(k-current->LU.ja[i])-1]);
		printf("\n");fflush(STDOUT);
		for (k=current->LU.ja[i];k<current->LU.ja[i+1]; k++) 
		  printf("%8.1le",current->LU.a[current->LU.ja[i]+2*(k-current->LU.ja[i])]);
		printf("\n");fflush(STDOUT);
		i+=2;
	      }
	    }
	    printf("\n");fflush(stdout);
	    printf("Block diagonal factor\n");
	    for (k=0; k<current->nB;) {
	      if (current->LU.ja[n+1+k]==0){
		printf("%8.1le",current->LU.a[k]);
		k++;
	      }
	      else {
		printf("%8.1le%8.1le",current->LU.a[k],current->LU.a[n+1+k]);
		k+=2;
	      }
	    }
	    printf("\n");fflush(stdout);
	    for (k=0; k<current->nB; ) {
	      if (current->LU.ja[n+1+k]==0) {
		printf("        ");
		k++;
	      }
	      else {
		printf("%8.1le%8.1le",current->LU.a[n+1+k],current->LU.a[k+1]);
		k+=2;
	      }
	    }
	    printf("\n");fflush(stdout);
	    printf("computed Schur complement\n");fflush(STDOUT);
	    for (i=0; i<S.nr; i++) {
	      for (k=S.ia[i];k<S.ia[i+1]; k++) 
		printf("%8d",S.ja[k-1]);
	      printf("\n");fflush(STDOUT);
	      for (k=S.ia[i];k<S.ia[i+1]; k++) 
		printf("%8.1le",S.a[k-1]);
	      printf("\n");fflush(STDOUT);
	    }
	    for (i=0; i<=current->nB; i++)
	      printf("%8d",i+1);
	    printf("\n");fflush(stdout);
	    for (i=0; i<=current->nB; i++)
	      printf("%8d",current->LU.ja[i]);
	    printf("\n");fflush(stdout);
	    for (i=0; i<current->nB; i++)
	      printf("%8d",current->LU.ja[n+1+i]);
	    printf("\n");fflush(stdout);
#endif


	  } /* end if (nB-current->nB<=amgcancel*nB) */
	  else { /* sympiluc failed to produce a sensible reduction */

#ifdef PRINT_INLINE
	     printf("level %d,  piluc failed (nB=%d), switch to SYMILUC\n",*nlev,current->nB);fflush(STDOUT);
#endif

	     /* switch to ILDLC */

	     // start counter for ILDLC
	     evaluate_time(&time_start,&systime);
	     current->LUperm=NULL;
	     for (i=0; i<n; i++)
	         current->p[i]=current->invq[i]=i+1;
	     
	     imem=(LONG_INT)mem;
	     myimem=imem;
	     SYMILUC(&n,S.a,S.ja,S.ia,&n,IPparam->fpar+7,&PILUCparam,
		     current->p,current->invq,alu,jlu,&myimem,
		     IPparam->dbuff,IPparam->ibuff,&ierr);
	     ILUPACK_mem[4]=MAX(ILUPACK_mem[4],ILUPACK_mem[2]+myimem);
	     ILUPACK_mem[5]=MAX(ILUPACK_mem[5],ILUPACK_mem[3]+myimem);

	     /*
	     for (i=0; i<n; i++) 
	       printf("%8.1le\n",alu[i]);
	     printf("\n");fflush(stdout);
	     for (i=0; i<n; i++) {
	       printf("%8d:\n",i+1);fflush(stdout);
	       for (k=jlu[i]; k<jlu[i+1];k++)
		 printf("%8d",jlu[k-1]);
	       printf("\n");fflush(stdout);
	       for (k=jlu[i]; k<jlu[i+1];k++)
		 printf("%8.1le",alu[k-1]);
	       printf("\n");fflush(stdout);
	     }
	     */

	     // computation time for ILDLC
	     evaluate_time(&time_stop,&systime);
	     ILUP_sec[3]=time_stop-time_start;
	     if (ierr) {
	        // compute total setup time
	        ILUP_sec[7]=time_stop-ILUP_sec[7];
		// compute TOTAL time
		ILUP_sec[8]=time_stop-ILUP_sec[8];
	        for (i=0; i<ILUPACK_secnds_length; i++)
		    ILUPACK_secnds[i]=ILUP_sec[i];
		current->E.ia=current->F.ia=NULL;
		current->E.ja=current->F.ja=NULL;
		current->E.a =current->F.a =NULL;
		current->LU.ja=jlu;
		current->absdiag=NULL;

		// undo scaling
		r=PRE->rowscal;
		c=PRE->colscal;
		// adjust arrays
		ia=A->ia;
		ja=A->ja;
		a=A->a;
		ja--;
		a--;
		c--;
		for (i=0; i<A->nr; i++) {
		    for (j=ia[i]; j<ia[i+1]; j++) {
		        k=ja[j];
#if defined _SINGLE_REAL_ || defined _DOUBLE_REAL_
			a[j]/=r[i]*c[k];
#else
			a[j].r/=r[i].r*c[k].r;
			a[j].i/=r[i].r*c[k].r;
#endif	  
		    }
		}

		// MYSYMAMGDELETE(*A, *PRE, *nlev, IPparam);
		return (ierr);
	     }
	     	     
	     current->nB=n;
	     
	     current->E.nr=current->F.nc=0;
	     current->E.nc=current->F.nr=0;
	     
	     /* discard old S by simply shifting the new U factor, because
		it immediately follows the old S in the memory */
	     if (*nlev>1) {
	        /* number of nonzeros in the old S */
	        k=S.ia[n]-1;
		/* number of nonzeros in U */
		nnz=jlu[n]-1;
		alu-=k;
		jlu-=k;
		delta[*nlev-1]-=k;
		for (i=0; i<nnz; i++) {
		    alu[i]=alu[i+k];
		    jlu[i]=jlu[i+k];
		} /* end for */
		mem+=k;
		ILUPACK_mem[2]-=k;
		ILUPACK_mem[3]-=k;
	     }
	   
	     current->LU.nr=current->nB;
	     current->LU.nc=current->LU.nr;
	     current->LU.a =alu;
	     current->LU.ja=jlu;
	     current->LU.ia=NULL;
	     
	     nnz=(size_t)jlu[n]-1;
	     ILUPACK_mem[2]+=(size_t)nnz;
	     ILUPACK_mem[3]+=(size_t)nnz;
	     ILUPACK_mem[4]=MAX(ILUPACK_mem[4],ILUPACK_mem[2]);
	     ILUPACK_mem[5]=MAX(ILUPACK_mem[5],ILUPACK_mem[3]);

	     n=-1;
	     S.nr=0;
	  } /* end if-else */
	} /* end if-else (*nlev>1 && 3*(S.ia[n]-1)>S.nr*S.nc) */
  } /* end while */
  
  // reduce memory to the size that was really needed
  if (!(param&RE_FACTOR)) {
    // printf("matrix will not be re-factored again\n");fflush(stdout);
     IPparam->njlu=ILUPACK_mem[2];
     IPparam->jlu=(integer *)  REALLOC(IPparam->jlu,
				   IPparam->njlu*sizeof(integer),  
				   "symAMGsetup:jlu");
     IPparam->nalu=ILUPACK_mem[3];
     IPparam->alu=(FLOAT *)REALLOC(IPparam->alu,
				   IPparam->nalu*sizeof(FLOAT),
				   "symAMGsetup:alu");

     /*
     current=PRE;
     j=0;
     for (i=0; i<*nlev; i++) {
       if (i==0)
	 printf("%8d,%8ld\n", delta[i],(long)0);
       else {
	 j+=(integer)(((unsigned long)(current->LU.a)-(unsigned long)(current->prev->LU.a))/sizeof(FLOAT));
	 printf("%8d,%8ld\n", delta[i],j);
       }
       current=current->next;
     }
     if ((unsigned long)IPparam->alu!=(unsigned long)PRE->LU.a)
       printf("pointers will be remapped\n");
     */

     // remap pointers
     current=PRE;
     for (i=0; i<*nlev; i++) {

       // sparse case
       if (current->LU.ja!=NULL)
	  current->LU.ja=IPparam->jlu+delta[i];
       else // dense case
	  current->LU.ia=current->prev->LU.ja+current->prev->LU.nr;

       current->LU.a =IPparam->alu+delta[i];
       
       current=current->next;
     }

     /*
     current=PRE;
     for (i=1; i<*nlev; i++) {
       current=current->next;
     }
     for (i=0; i<current->LU.nr; i++) 
       printf("%8.1le\n",current->LU.a[i]);
     printf("\n");fflush(stdout);
     for (i=0; i<current->LU.nr; i++) {
       printf("%8d:\n",i+1);fflush(stdout);
       for (k=current->LU.ja[i]; k<current->LU.ja[i+1];k++)
	 printf("%8d",current->LU.ja[k-1]);
       printf("\n");fflush(stdout);
       for (k=current->LU.ja[i]; k<current->LU.ja[i+1];k++)
	 printf("%8.1le",current->LU.a[k-1]);
       printf("\n");fflush(stdout);
     }
     */

#ifdef PRINT_MEM
     printf("jlu(%8.2lfMB), alu(%8.2lfMB) finally re-allocated\n",ILUPACK_mem[2]/IMEGA,ILUPACK_mem[3]/DMEGA);fflush(stdout);
#endif
  }
  else {
    // printf("matrix may be re-factored\n");fflush(stdout);
  }

  current=PRE;
  n=A->nr;
  for (j=1; j<=*nlev; j++) {
      // level i
    
      // case of a sparse inverse-based block ILU
      if (j<*nlev || current->LU.ja!=NULL) {
	 // shift block diagonal parts properly
	 if (j==*nlev-1 && current->next->LU.ja==NULL) {
	    /* exceptional case, if the next level is the final level
	       and the final level uses a dense factorization 
	       In this case move the space in ja at nB+1,...,n to 
               n+1+nB+1,...n+1+n
	    */
	    for (i=0; i<n-current->nB; i++) {
		current->LU.ja[n+1+current->nB+i]=current->LU.ja[current->nB+i];
	    }
	    // re-adjust pointer
	    current->next->LU.ia=current->LU.ja+n+1+current->nB;
	 }
	 for (i=0; i<current->nB; i++) {
	     current->LU.ja[current->nB+1+i]=current->LU.ja[n+1+i];
	     current->LU.a[current->nB+1+i] =current->LU.a[n+1+i];
	 } // end for i
	 // now we do not need to refer to the dense Schur complement anymore
	 current->LU.ja[current->nB]=current->LU.ja[current->nB-1];

#ifdef PRINT_INFO
	    printf("4.computed U-factor\n");fflush(STDOUT);
	    for (i=0; i<current->nB; ) {
	      if (current->LU.ja[current->nB+1+i]==0){
		for (k=current->LU.ja[i];k<current->LU.ja[i+1]; k++) 
		  printf("%8d",current->LU.ja[k-1]);
		printf("\n");fflush(STDOUT);
		for (k=current->LU.ja[i];k<current->LU.ja[i+1]; k++) 
		  printf("%8.1le",current->LU.a[k-1]);
		printf("\n");fflush(STDOUT);
		i++;
	      }
	      else {
		for (k=current->LU.ja[i];k<current->LU.ja[i+1]; k++) 
		  printf("%8d",current->LU.ja[k-1]);
		printf("\n");fflush(STDOUT);
		for (k=current->LU.ja[i];k<current->LU.ja[i+1]; k++) 
		  printf("%8.1le",current->LU.a[current->LU.ja[i]+2*(k-current->LU.ja[i])-1]);
		printf("\n");fflush(STDOUT);
		for (k=current->LU.ja[i];k<current->LU.ja[i+1]; k++) 
		  printf("%8.1le",current->LU.a[current->LU.ja[i]+2*(k-current->LU.ja[i])]);
		printf("\n");fflush(STDOUT);
		i+=2;
	      }
	    }
	    printf("\n");fflush(stdout);
	    printf("Block diagonal factor\n");
	    for (k=0; k<current->nB;) {
	      if (current->LU.ja[current->nB+1+k]==0){
		printf("%8.1le",current->LU.a[k]);
		k++;
	      }
	      else {
		printf("%8.1le%8.1le",current->LU.a[k],current->LU.a[current->nB+1+k]);
		k+=2;
	      }
	    }
	    printf("\n");fflush(stdout);
	    for (k=0; k<current->nB; ) {
	      if (current->LU.ja[current->nB+1+k]==0) {
		printf("        ");
		k++;
	      }
	      else {
		printf("%8.1le%8.1le",current->LU.a[current->nB+1+k],current->LU.a[k+1]);
		k+=2;
	      }
	    }
	    printf("\n");fflush(stdout);
	    for (i=0; i<=current->nB; i++)
	      printf("%8d",i+1);
	    printf("\n");fflush(stdout);
	    for (i=0; i<=current->nB; i++)
	      printf("%8d",current->LU.ja[i]);
	    printf("\n");fflush(stdout);
	    for (i=0; i<current->nB; i++)
	      printf("%8d",current->LU.ja[current->nB+1+i]);
	    printf("\n");fflush(stdout);
#endif


      }
      else {
	  // full matrix processing in the final step
#ifdef PRINT_INFO
	    printf("dense U-factor\n");fflush(STDOUT);
	    for (i=0; i<current->nB; i++)
	      printf("%8d",current->LU.ia[i]);
	    printf("\n");fflush(stdout);
	    j=0;
	    for (k=0; k<current->nB; k++) {
	        for (i=k; i<current->nB; i++) {
		    printf("%8.1le",current->LU.a[j++]);
		}
		printf("\n");fflush(stdout);
	    }
	    printf("\n");fflush(stdout);
#endif

      }
      n-=current->nB;
      current=current->next;
  } // end for j

  evaluate_time(&time_stop,&systime);
  // compute total setup time
  ILUP_sec[7]=time_stop-ILUP_sec[7];
  for (i=0; i<ILUPACK_secnds_length; i++)
      ILUPACK_secnds[i]=ILUP_sec[i];

  // export final condest
  IPparam->fpar[2]=condest;

  ILUPACK_mem[0]=IPparam->nibuff;
  ILUPACK_mem[1]=IPparam->ndbuff;

  /*
  if (ierr==0) {
     printf("maximum elbow required during factorization %8.1f\n",
	    MAX(ILUPACK_mem[4],ILUPACK_mem[5])/(A->ia[A->nc]-1.0)+.05);
     printf("maximum buffer size  %8.1f\n",
	    MAX(ILUPACK_mem[0]*sizeof(int)*1.0/sizeof(FLOAT),ILUPACK_mem[1])/(A->ia[A->nc]-1.0)+.05);
     printf("maximum size F       %8.1f\n",
	    MAX(ILUPACK_mem[6]*sizeof(int)*1.0/sizeof(FLOAT),ILUPACK_mem[7])/(A->ia[A->nc]-1.0)+.05);
     printf("maximum size scaling %8.1f\n",
	    MAX(ILUPACK_mem[8]*sizeof(int)*1.0/sizeof(FLOAT),ILUPACK_mem[9])/(A->ia[A->nc]-1.0)+.05);
     printf("maximum size drivers %8.1f\n",
	    MAX(ILUPACK_mem[10]*sizeof(int)*1.0/sizeof(FLOAT),ILUPACK_mem[11])/(A->ia[A->nc]-1.0)+.05);
     fflush(stdout);
  }
  */

  free(delta);
  return (ierr);
} /* end symAMGsetup */


