#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

#include <blas.h>
#include <pardiso.h>
#include <amdord.h>
#include <ilupack.h>

#include <ilupackmacros.h>

#define MAX(A,B)        (((A)>(B))?(A):(B))
#define MIN(A,B)        (((A)<(B))?(A):(B))
#define STDERR          stderr
#define STDOUT          stdout


// #define PRINT_INFO


#if defined _DOUBLE_REAL_ || defined _SINGLE_REAL_
#define CONJG(A)      (A)

#ifdef _SKEW_MATRIX_

#ifdef _DOUBLE_REAL_
#define MYSYMMWM            DSSMsmwm
#define MYSYMPERMMWMAMD  DSSMperm_mwm_amd

#else
#define MYSYMMWM            SSSMsmwm
#define MYSYMPERMMWMAMD  SSSMperm_mwm_amd
#endif

#define SKEW(A)      (-(A))

#else

#ifdef _DOUBLE_REAL_
#define MYSYMMWM            DSYMsmwm
#define MYSYMPERMMWMAMD     DSYMperm_mwm_amd
#else
#define MYSYMMWM            SSYMsmwm
#define MYSYMPERMMWMAMD     SSYMperm_mwm_amd
#endif

#define SKEW(A)      (A)
#endif
// end _SKEW_MATRIX_


#else

#ifdef _COMPLEX_SYMMETRIC_
#define CONJG(A)     (A)

#ifdef _SKEW_MATRIX_
#define SKEW(A)      (-(A))

#ifdef _SINGLE_COMPLEX_
#define MYSYMMWM            CSSMsmwm
#define MYSYMPERMMWMAMD  CSSMperm_mwm_amd
#else
#define MYSYMMWM            ZSSMsmwm
#define MYSYMPERMMWMAMD  ZSSMperm_mwm_amd
#endif

#else
#define SKEW(A)      (A)

#ifdef _SINGLE_COMPLEX_
#define MYSYMMWM            CSYMsmwm
#define MYSYMPERMMWMAMD  CSYMperm_mwm_amd
#else
#define MYSYMMWM            ZSYMsmwm
#define MYSYMPERMMWMAMD  ZSYMperm_mwm_amd
#endif

#endif
// end _SKEW_MATRIX_


#else
#define CONJG(A)     (-(A))

#ifdef _SKEW_MATRIX_
#define SKEW(A)      (-(A))

#ifdef _SINGLE_COMPLEX_
#define MYSYMMWM            CSHRsmwm
#define MYSYMPERMMWMAMD  CSHRperm_mwm_amd
#else
#define MYSYMMWM            ZSHRsmwm
#define MYSYMPERMMWMAMD  ZSHRperm_mwm_amd
#endif

#else
#define SKEW(A)      (A)

#ifdef _SINGLE_COMPLEX_
#define MYSYMMWM            CHERsmwm
#define MYSYMPERMMWMAMD  CHERperm_mwm_amd
#else
#define MYSYMMWM            ZHERsmwm
#define MYSYMPERMMWMAMD  ZHERperm_mwm_amd
#endif

#endif
// end _SKEW_MATRIX_

#endif
// end _COMPLEX_SYMMETRIC_

#endif
// end _DOUBLE_REAL_


/*
 scaling and permutation driver, followed by a symmetric reordering.

 here 1) maximum weight matching (PARDISO)
      3) associated symmetric block partitioning and block reordering by
         I.S. Duff and S. Pralet
      2) AMD (approximate minimum degree) from UMFPACK V4.3


 Given an n x n matrix A in compressed sparse row format this routine computes
 row and column scalings as well as row and column permutations such that

 A -> Dr * A * Dc,
 
 where Dr and Dc are diagonal matrices stored in the vectors proscal and 
 pcolscale. The scaling is explicitly applied to A.

 The permutation p,invq refer to permutation matrices P and Q^{-1} such  that

  P^T*(Dr * A * Dc)*Q 

 is hopefully better suited for the ILU.

 The routine returns a leading block size nB<=n
                        /B F\ 
  P^T*(Dr * A * Dc)*Q = |   | 
                        \E C/
 In this case the permutation recommends that the ILU is only applied to the
 leading block B of size nB x nB
 */
integer MYSYMPERMMWMAMD(CSRMAT A, FLOAT *prowscale, FLOAT *pcolscale, 
		     integer *p, integer *invq, integer *nB, ILUPACKPARAM *param)
{
/*
    A          n x n matrix in compressed sparse row format
               A is altered by the routine since the scalings are directly
               applied to A. Hopefully the scaling are only powers of 2,
               which is the case for all scaling routines from ILUPACK.

    prowscale,   vectors of length n that store the row and column scalings Dr
    pcolscale    and Dc
               A -> Dr*A*Dc

    p,invq     permutation vectors p and q^{-1} of length n which refer to
               row / column permutation matrices P and Q^{-1}.
               P^T*(Dr * A * Dc)*Q hopefully better suited for ILU

    nB         leading blocksize nB
                                     /B F\ 
               P^T*(Dr * A * Dc)*Q = |   |,  B matrix of size nB x nB
                                     \E C/
               nB is the recommended block size for the application of an ILU

    param      ILUPACK parameters


    interface driver written by Matthias Bollhoefer, November 2004, June 2005
 */ 

    integer  i,ii,j,k,l,m,imx,n=A.nr,liw,*iw,ldw,info[10],icntl[10],*cperm,job,
         iflag, num,nz,ierr=0,options[10], numflag, scale, pow,nrm, *ia, *ja;
    REALS mxr, lg2=1.0/log((double)2.0), fpow;
    double *scal, a, *b=NULL, Control[AMD_CONTROL], Info[AMD_INFO];
    FLOAT  *w,*dw,mx;
    size_t mem, ILUPACK_mem1=0, ILUPACK_mem2=0;
    CSRMAT B, C;

    ILUPACK_mem[12]=0;
    ILUPACK_mem[13]=0;
    ierr=0;
    /* --------------------   END set up parameters   -------------------- */

    // PARDISO maximum weight matching interface
    param->ndbuff=MAX(param->ndbuff,(2*(size_t)A.nc*MAX(sizeof(FLOAT),sizeof(double))/sizeof(double)));
    param->dbuff=(FLOAT *) REALLOC(param->dbuff,param->ndbuff*sizeof(FLOAT),
				  "sympermmwm_amd:dbuff");

#include "scaleprefix.c"

    // use unsymmetric version of A for maximum weight matching
    // build B=|A|+|A^T|
    param->nibuff=MAX(param->nibuff,3*(size_t)A.nc+2);
    param->ibuff=(integer *) REALLOC(param->ibuff,param->nibuff*sizeof(integer),
				 "sympermmwm_amd:ibuff");
    // param->ibuff (1,...,2n+1) will be used indicate the pruned parts of A, but 
    // also be used for B.ia!
    // param->iaux will be used for B.ja, if possible
    // dbuff (n) will be used to compute the maximum absolute entry per row.
    SETUPGRAPH_EPSILON(A,&B,(REALS)MC64THRESHOLD,
		       (REALS *)param->dbuff, param->ibuff, 
		       param->iaux, param->niaux);

    // check if MALLOC was needed to set up B.ja
    if (B.ia[B.nc]<0) {
       iflag=0;
       B.ia[B.nc]=-(B.ia[B.nc]);
       ILUPACK_mem1=B.ia[B.nc]-1;
    }
    else { // nz entries from iaux are already in use 
       iflag=B.ia[B.nc]-1; 
       ILUPACK_mem[12]=B.ia[B.nc]-1;
    }
    // convert ia from FORTRAN-style to C-style
    for (i=0; i<=A.nr; i++)
        A.ia[i]--;
    // number of nonzeros
    nz=A.ia[n];

    // convert ja from FORTRAN-style to C-style
    for (i=0; i<nz; i++)
        A.ja[i]--;


    // memory for the unsymmetric version of A
    if ((param->ndaux*sizeof(double))/sizeof(FLOAT)>=B.ia[n]-1) {
       b=(double *)param->daux;
       ILUPACK_mem[13]=B.ia[n]-1;
    }
    else {
       b=(double *)MALLOC((size_t)(B.ia[n]-1)*sizeof(double),"sympermmwm_amd:b");
       ILUPACK_mem[11]=MAX(ILUPACK_mem[11],B.ia[n]-1);
    }

    /*--------------------  actual copying  */
    iw=B.ia;
    for (i=0; i<A.nc; i++) {
        // copy indices
        for (j=A.ia[i]; j<param->ibuff[i]-1; j++) {
	    // current column index of row i in FORTRAN style
	    k=A.ja[j];
	    a=FABS(A.a[j]);
	    b[iw[i]-1]=a;
	    // advance pointer
	    iw[i]++;
	    // avoid duplicate entries
	    if (k!=i) {
	       b[iw[k]-1]=a;
	       // advance pointer
	       iw[k]++;
	    } // end if
	} // end for j
    } // end for i
    // shift iw back
    for (i=A.nc; i>0; i--) 
        iw[i]=iw[i-1];
    iw[0]=1;



    // maximum weight matching
    scal=(double *)param->dbuff;
    // convert B.ia,B.ja from FORTRAN-style to C-style
    for (i=0; i<=B.nr; i++)
        B.ia[i]--;
    for (i=0; i<B.ia[B.nr]; i++)
        B.ja[i]--;
    ierr = mps_pardiso( A.nr, B.ia,B.ja,b, p, scal+n,scal, 0);
    // values of the unsymmetric version are not longer needed
    if ((param->ndaux*sizeof(double))/sizeof(FLOAT)<B.ia[n]-1) 
       free(b);
    if (!iflag)
       free(B.ja);
    if (ierr)
       return (ierr);
    
    
    // build left and right diagonal scaling derived from the matching
    for (i=0; i<n; i++) {
        // adjust permutation vector
        p[i]++;

	mxr=sqrt(exp(scal[i]+scal[n+i]));
	// compute nearest power of 2
	fpow=log(mxr)*lg2;
	if (fpow<0.0) {
	   pow=fpow-0.5;
	   fpow=1;
	   for (l=1; l<=-pow; l++)
	       fpow*=2.0;
	   mxr=1.0/fpow;
	}
	else {
	   pow=fpow+0.5;
	   fpow=1;
	   for (l=1; l<=pow; l++)
	       fpow*=2;
	   mxr=fpow;
	}
	scal[i]=mxr;
    } // end for i

    // transfer scaling to the official interface vectors
    for (i=0; i<A.nr; i++){
#if defined _DOUBLE_REAL_ || defined  _SINGLE_REAL_
	pcolscale[i]*=scal[i];
#else
	pcolscale[i].r*=scal[i];
	pcolscale[i].i=0;
#endif
    } // end for i


    // rescale matrix explicitly
    for (i=0; i<n; i++){
        for (j=A.ia[i];j<A.ia[i+1];j++){
#if defined _DOUBLE_REAL_ || defined  _SINGLE_REAL_
	    A.a[j]=scal[i]*A.a[j]*scal[A.ja[j]];
#else
	    A.a[j].r=scal[i]*A.a[j].r*scal[A.ja[j]];
	    A.a[j].i=scal[i]*A.a[j].i*scal[A.ja[j]];
#endif
	} // end for j
    } // end for i

    // convert ia,ja back from C-style to FORTRAN-style 
    for (i=0; i<=A.nr; i++)
        A.ia[i]++;
    for (i=0; i<nz; i++)
        A.ja[i]++;


#ifdef PRINT_INFO
    for (i=0; i<n; i++)
        for (j=A.ia[i];j<A.ia[i+1];j++)
	    printf("%8d%8d%16.8le\n",i+1, A.ja[j-1],A.a[j-1]);
    printf("\n");
    fflush(stdout);
    for (i=0; i<A.nc; i++) 
        printf("%8d",p[i]);
    printf("\n");
    fflush(stdout);
#endif



    // compute symmetric weighted matching, i.e. break cycles into
    // 1x1 and 2x2 cycles
    // SYMMETRIC maximum weight matching interface
    l=MYSYMMWM(A, p, param->ibuff, param->dbuff);
    


#ifdef PRINT_INFO
    for (i=0; i<A.nr; i++)
      printf("%8d",p[i]);
    printf("\n");
    fflush(stdout);
#endif


    // reorder the pattern of A by rows in order to apply a symbolic reordering
    // allocate memory with respect to the specific permutation routine
    // param->nibuff=MAX(param->nibuff,2*(size_t)A.nc+1);
    // param->ibuff=(integer *) REALLOC(param->ibuff,param->nibuff*sizeof(integer),
    //				 "sympermmwm_amd:ibuff");
    
    // reorder the rows and columns of A, at least the pattern of it
    // inverse permutation
    cperm=param->ibuff+A.nc+1;
    for (i=0; i<A.nc; i++)
        cperm[p[i]-1]=i+1;
    C.nc=C.nr=A.nr;
    C.a=NULL;
    C.ia=param->ibuff;
    if (param->niaux>=nz) {
       C.ja=param->iaux;
       ILUPACK_mem[12]=MAX(ILUPACK_mem[12],nz);
    }
    else {
       C.ja=(integer *)MALLOC((size_t)nz*sizeof(integer), "sympermmwm_amd:C.ja");
       ILUPACK_mem2=nz;
    }
    ILUPACK_mem[10]=MAX(ILUPACK_mem[10],ILUPACK_mem1);
    ILUPACK_mem[10]=MAX(ILUPACK_mem[10],ILUPACK_mem2);

    C.ia[0]=1;
    for (i=0; i<A.nc; i++) {
        ii=p[i]-1;
	k=C.ia[i]-1;
	for (j=A.ia[ii]; j<A.ia[ii+1]; j++) {
	    C.ja[k++]=cperm[A.ja[j-1]-1];
	}
	C.ia[i+1]=C.ia[i]+ (A.ia[ii+1]-A.ia[ii]);
    }


    // compress matrix
    // the leading l rows correspond to 1x1 pivots
    for (i=0; i<l; i++)
        cperm[i]=i+1;
    j=l;
    for (; i<A.nc; i+=2)
        cperm[i]=cperm[i+1]=++j;


    // shift column indices
    for (i=0; i<nz; i++)
        C.ja[i]=cperm[C.ja[i]-1];

    // buffer for flags
    for (i=0; i<A.nc; i++)
        cperm[i]=0;
    // skip duplicate entries in the leading l rows
    k=0;
    for (i=0; i<l; i++) {
        // old start of row i
        j=C.ia[i]-1;
	// new start of row i
	C.ia[i]=k+1;
        for (; j<C.ia[i+1]-1; j++) {
	    ii=C.ja[j]-1;
	    // entry is not duplicate yet
	    if (!cperm[ii]) {
	       // check mark
	       cperm[ii]=-1;
	       // shift entry
	       C.ja[k++]=ii+1;
	    }
	}
	// clear check mark array
        for (j=C.ia[i]-1; j<k; j++) {
	    ii=C.ja[j]-1;
	    cperm[ii]=0;
	}
    }
    // merge duplicate entries and rows in the remaining  n-l rows
    m=l;
    for (; i<A.nc; i+=2,m++) {
        // old start of row i
        j=C.ia[i]-1;
	// new start of row i
	C.ia[m]=k+1;
        for (; j<C.ia[i+2]-1; j++) {
	    ii=C.ja[j]-1;
	    // entry is not duplicate yet
	    if (!cperm[ii]) {
	       // check mark
	       cperm[ii]=-1;
	       // shift entry
	       C.ja[k++]=ii+1;
	    }
	}
	// clear check mark array
        for (j=C.ia[m]-1; j<k; j++) {
	    ii=C.ja[j]-1;
	    cperm[ii]=0;
	}
    }
    C.ia[m]=k+1;
    C.nc=C.nr=m;



#ifdef PRINT_INFO
    for (i=0; i<C.nc; i++)
        for (j=C.ia[i];j<C.ia[i+1];j++)
	    printf("%8d%8d%16.8le\n",i+1, C.ja[j-1],-1.0);
    printf("\n");
    fflush(stdout);
#endif



    // setup matrix for AMD
    for (i=0; i<C.nc; i++) {
        j=C.ia[i]-1;
	k=C.ia[i+1]-1-j;
	QQSORTI(C.ja+j,param->ibuff+A.nc+1,&k);
    }


    // convert from FORTRAN style to C-style
    for (i=0; i<=C.nc; i++)
        C.ia[i]--;
    for (i=0; i<nz; i++)
        C.ja[i]--;



    // reorder the system using approximate minimum degree
#ifdef _LONG_INTEGER_
    amd_l_defaults(Control);
    cperm=invq;
    ierr=amd_l_order(C.nc,C.ia,C.ja,cperm,Control,Info);
#else
    amd_defaults(Control);
    cperm=invq;
    ierr=amd_order(C.nc,C.ia,C.ja,cperm,Control,Info);
#endif

    //printf("ierr=%d\n",ierr);
    if (param->niaux<nz) {
       free(C.ja);
       //printf("release C.ja\n");fflush(stdout);
    }

    for (i=0; i<C.nc; i++) {
        cperm[i]++;
   } // end for




#ifdef PRINT_INFO
    for (i=0; i<C.nc; i++)
        printf("%8d",cperm[i]);
    printf("\n\n");
    fflush(stdout);
#endif



    // prolongate permutation to its original size
    j=0;
    for (i=0;i<C.nc; i++)
        // 1x1 block
        if (cperm[i]<=l)
	   param->ibuff[j++]=cperm[i];
        else { // 2x2 block
	   param->ibuff[j]=2*cperm[i]-l-1;
	   param->ibuff[j+1]=param->ibuff[j]+1;
	   j+=2;
	}



#ifdef PRINT_INFO
    printf("block permutation\n");fflush(stdout);
    for (i=0; i<A.nc; i++)
        printf("%8d",param->ibuff[i]);
    printf("\n\n");
    fflush(stdout);
#endif


    
    // build product permutation for the rows (minimum weighted matching followed 
    //                                         symmetric reordering)
    for (i=0; i<n; i++) {
        cperm[i]=p[param->ibuff[i]-1];
    }
    // rewrite permutation
    for (i=0; i<n; i++) {
	p[i]=cperm[i];
    }
    // inverse permutation
    for (i=0; i<n; i++) {
	invq[p[i]-1]=i+1;
    }



#ifdef PRINT_INFO
    printf("final permutation\n");fflush(stdout);
    for (i=0; i<A.nc; i++)
        printf("%8d",p[i]);
    printf("\n\n");
    fflush(stdout);
#endif

    *nB=A.nc; 

    return (ierr);
} /* end sympermmwm_amd */
