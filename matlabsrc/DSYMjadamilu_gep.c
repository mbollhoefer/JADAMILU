/* ========================================================================== */
/* === JADAMILU mexFunction ================================================= */
/* ========================================================================== */

/*
    Usage:

    Return eigenvalues and eigenvectors by JADAMILU
    
    Example:

    % STANDARD JADAMILU call
    [V,D,options] = DSYMjadamilu_gep(A,M,k,sigma,options);



    Authors:

	Matthias Bollhoefer, TU Braunschweig

    Date:

	June 13, 2015. JADAMILU 2.0

    Notice:

	Copyright (c) 2015 by TU Braunschweig/Universite Libre de Bruxelles. 
        All Rights Reserved.

	THIS MATERIAL IS PROVIDED AS IS, WITH ABSOLUTELY NO WARRANTY
	EXPRESSED OR IMPLIED.  ANY USE IS AT YOUR OWN RISK.

    Availability:

	This file is located at

	http://homepages.ulb.ac.be/~jadamilu/
*/

/* ========================================================================== */
/* === Include files and prototypes ========================================= */
/* ========================================================================== */

#include "mex.h"
#include "matrix.h"
#include <string.h>
#include <stdlib.h>
#include <jadamilu.h>

#define MAX_FIELDS 100
#define MAX(A,B) (((A)>=(B))?(A):(B))
#define MIN(A,B) (((A)>=(B))?(B):(A))

/* #define PRINT_INFO */

/* ========================================================================== */
/* === mexFunction ========================================================== */
/* ========================================================================== */

void mexFunction
(
    /* === Parameters ======================================================= */

    int nlhs,			/* number of left-hand sides */
    mxArray *plhs [],		/* left-hand side matrices */
    int nrhs,			/* number of right--hand sides */
    const mxArray *prhs []	/* right-hand side matrices */
)


{

    const char **fnames;       /* pointers to field names */
    const mwSize  *dims;
    mxClassID  *classIDflags;
    mwSize     buflen, mrows, ncols, ndim, mydims[2], *A_ia,*A_ja, *M_ia, *M_ja;
    mxArray    *tmp, *fout, *A_input, *options_input, *options_output,
               *k_input, *sigma_input, *V_output, *D_output, *V0, *A_output,
               *M_input;
    char       *pdata, *input_buf, *output_buf;
    int        ifield, status, nfields, i,j,k,l,nnz, nnzM;
    integer    *ia, *ja, *im, *jm, maxeig, ninit, iprint, isearch, iter, madspace, disp,
               info, lx, neig, icntl[5],n;
    size_t     sizebuf;
    double     dbuf, *A_valuesR, *a, *m, *eigs, *res, mem, sigma, shift, sign,
               tol, droptol, *X, *pr, gap, condest, *M_valuesR;


    if (nrhs!=5)
       mexErrMsgTxt("Five input arguments required.");
    else if (nlhs!=3)
       mexErrMsgTxt("Three output arguments are required.");



    /* The first input must be a sparse square matrix.*/
    A_input=(mxArray *)prhs[0];
    if (!mxIsSparse(A_input))
       mexErrMsgTxt("First input must be a sparse matrix.");
    mrows=mxGetM(A_input);
    ncols=mxGetN(A_input);
    if (mrows!=ncols) {
       mexErrMsgTxt("First input must be a square matrix.");
    }
    A_ja     =(mwIndex *)mxGetIr(A_input);
    A_ia     =(mwIndex *)mxGetJc(A_input);
    A_valuesR=(double *) mxGetPr(A_input);

    n=ncols;
    /* copy the upper triangular part and use FORTRAN notation */
    l=0;
    for (i=0; i<n; i++) {
        for (j=A_ia[i]; j<A_ia[i+1]; j++) {
	    k=A_ja[j];
	    if (k>=i)
	       l++;
	} /* end for j */     
    } /* end for i */
    nnz=l;
    ia=(integer *)mxCalloc((size_t)(n+1),(size_t)sizeof(integer));
    ja=(integer *)mxCalloc((size_t)nnz,  (size_t)sizeof(integer));
    a =(double *) mxCalloc((size_t)nnz,  (size_t)sizeof(double));
    l=0;
    ia[0]=1;
    for (i=0; i<n; i++) {
        for (j=A_ia[i]; j<A_ia[i+1]; j++) {
	    k=A_ja[j];
	    if (k>=i) {
	       ja[l]=k+1;
	       a[l]=A_valuesR[j];
	       l++;
	    } /* end if */
	} /* end for j */
	ia[i+1]=l+1;
    } /* end for i */



    /* The second input must be a sparse square matrix.*/
    M_input=(mxArray *)prhs[1];
    if (!mxIsSparse(M_input))
       mexErrMsgTxt("Second input must be a sparse matrix.");
    mrows=mxGetM(M_input);
    ncols=mxGetN(M_input);
    if (mrows!=ncols || mrows!=n) {
       mexErrMsgTxt("Second input must be a square matrix.");
    }
    M_ja     =(mwIndex *)mxGetIr(M_input);
    M_ia     =(mwIndex *)mxGetJc(M_input);
    M_valuesR=(double *) mxGetPr(M_input);

    /* copy the upper triangular part and use FORTRAN notation */
    l=0;
    for (i=0; i<n; i++) {
        for (j=M_ia[i]; j<M_ia[i+1]; j++) {
	    k=M_ja[j];
	    if (k>=i)
	       l++;
	} /* end for j */     
    } /* end for i */
    nnzM=l;
    im=(integer *)mxCalloc((size_t)(n+1),(size_t)sizeof(integer));
    jm=(integer *)mxCalloc((size_t)nnzM, (size_t)sizeof(integer));
    m =(double *) mxCalloc((size_t)nnzM, (size_t)sizeof(double));
    l=0;
    im[0]=1;
    for (i=0; i<n; i++) {
        for (j=M_ia[i]; j<M_ia[i+1]; j++) {
	    k=M_ja[j];
	    if (k>=i) {
	       jm[l]=k+1;
	       m[l]=M_valuesR[j];
	       l++;
	    } /* end if */
	} /* end for j */
	im[i+1]=l+1;
    } /* end for i */



    /* Get third input argument */
    k_input=(mxArray *)prhs[2];
    if (!mxIsNumeric(k_input))
       mexErrMsgTxt("Third input must be a number");
    mrows=mxGetM(k_input);
    ncols=mxGetN(k_input);
    if (mrows*ncols!=1) {
       mexErrMsgTxt("Third input must be a scalar.");
    }
    maxeig=*mxGetPr(k_input);
    if (maxeig<0 || maxeig>n)
       mexErrMsgTxt("Third input between 1 and n");
#ifdef PRINT_INFO
    mexPrintf("k=%d\n",maxeig);
#endif
    eigs=(double *)mxCalloc((size_t)maxeig,sizeof(double));
    res =(double *)mxCalloc((size_t)maxeig,sizeof(double));
    for (i=0; i<maxeig; i++)
        eigs[i]=res[i]=0.0;


    /* Get fourth input argument */
    sigma_input=(mxArray *)prhs[3];
    if (mxIsNumeric(sigma_input)) {
       isearch=2;
       sigma=*mxGetPr(sigma_input);
       sign=1.0;
#ifdef PRINT_INFO
       mexPrintf("Compute eigenvalues near sigma=%8.1le\n",sigma);
#endif
    }
    else if (mxIsChar(sigma_input)) {
       /* formally search for the k smallest eigenvalues */
       isearch=0;
       /* sigma is not referenced */
       sigma=0.0;

       buflen=(mxGetM(sigma_input)*mxGetN(sigma_input))+1;
       /* Allocate memory for input and output strings. */
       input_buf=(char *)mxCalloc((size_t)buflen, (size_t)sizeof(char));
       mxGetString(sigma_input, input_buf, buflen);

       if (!strcmp("l",input_buf)) {
	  sign=-1.0;
	  /* replace A by -A */
	  for (l=0; l<nnz; l++)
	      a[l]=-a[l];
#ifdef PRINT_INFO
	 mexPrintf("Compute %d largest eigenvalues\n",maxeig);
#endif
       }
       else {
	  sign=1.0;
#ifdef PRINT_INFO
	  mexPrintf("Compute %d smallest eigenvalues\n",maxeig);
#endif
       }
       mxFree(input_buf);
    }
    else
       mexErrMsgTxt("Fourth input must either be a number or a character");

#ifdef PRINT_INFO
    mexPrintf("isearch=%d, sigma=%8.1e, sign=%8.1le\n",isearch,sigma,sign);
#endif

    /* Get fifth input argument */
    options_input=(mxArray *)prhs[4];
    if (!mxIsStruct(options_input)) 
       mexErrMsgTxt("Fifth input must be a structure");
    nfields = mxGetNumberOfFields(options_input);

    /* Allocate memory  for storing classIDflags */
    classIDflags=(mxClassID *)mxCalloc((size_t)nfields, (size_t)sizeof(mxClassID));
    
    /* allocate memory for storing pointers */
    fnames=mxCalloc((size_t)nfields, (size_t)sizeof(char *));
    /* Get field name pointers */
    for (ifield=0; ifield<nfields; ifield++) {
        fnames[ifield] = mxGetFieldNameByNumber(options_input,ifield);
#ifdef PRINT_INFO
	mexPrintf("options.%s identified\n",fnames[ifield]); 
	tmp=mxGetFieldByNumber(options_input,(mwIndex)0,ifield);
	ndim=mxGetNumberOfDimensions(tmp);
	dims=mxGetDimensions(tmp);
	mexPrintf("matrix size %d\n",dims[0]*dims[1]);
	sizebuf=mxGetElementSize(tmp);
	mexPrintf("element size %d\n",sizebuf);
#endif

    }

    /* overwrite shift value if desired by the user */
    if (mxGetField(options_input,0,"shift")!=NULL)
      shift=*mxGetPr(mxGetField(options_input,0,"shift"));
    else
       shift=sigma;
#ifdef PRINT_INFO
    mexPrintf("shift=%8.1le\n",shift);
#endif
    /* use isearch=1 if an initial sigma0 is provided and sigma='l' or sigma='s'  */
    if (mxGetField(options_input,0,"sigma0")!=NULL) {
       if (isearch==0) {
	  isearch=1;
	  sigma=*mxGetPr(mxGetField(options_input,0,"sigma0"));
       }
    }
#ifdef PRINT_INFO
    mexPrintf("isearch=%d,sigma=%8.1le\n",isearch,sigma);
#endif


    /* max search space */
    madspace=*mxGetPr(mxGetField(options_input,0,"madspace"));
#ifdef PRINT_INFO
    mexPrintf("madspace=%d\n",madspace);
#endif

    iter=*mxGetPr(mxGetField(options_input,0,"maxit"));
#ifdef PRINT_INFO
    mexPrintf("maxit=%d\n",iter);
#endif

    tol=*mxGetPr(mxGetField(options_input,0,"restol"));
#ifdef PRINT_INFO
    mexPrintf("restol=%8.1le\n",tol);
#endif

    disp=*mxGetPr(mxGetField(options_input,0,"disp"));
#ifdef PRINT_INFO
    mexPrintf("disp=%d\n",disp);
#endif
    if (disp>0) 
       iprint=6;
    else if (disp<0)
       iprint=-6;
    else
       iprint=0;

    mem=*mxGetPr(mxGetField(options_input,0,"mem"));
#ifdef PRINT_INFO
    mexPrintf("mem=%8.1le\n",mem);
#endif

    droptol=*mxGetPr(mxGetField(options_input,0,"droptol"));
#ifdef PRINT_INFO
    mexPrintf("droptol=%8.1le\n",droptol);
#endif

    V0=mxGetField(options_input,0,"V0");
    mrows=mxGetM(V0);
    ncols=mxGetN(V0);
    /* empty matrix */
    if (ncols==0)
       mrows=0;
    /* number of prescribed eigenvectors */
    ninit=MIN(maxeig,ncols);
    if (ncols>maxeig || (mrows!=n&&ncols>0))
       mexErrMsgTxt("V0 must be an n times l matrix with l<=k.");
    pr=mxGetPr(V0);
    lx=n*(4*madspace+2*maxeig+4)+6*madspace+4*madspace*madspace+MAX(madspace*madspace,maxeig);
    X=(double *)mxCalloc((size_t)lx,sizeof(double));
    /* init with initial eigenvector guesses */
    j=0;
    for (; j<n*ninit; j++)
        X[j]=pr[j];
    for (; j<lx; j++)
        X[j]=0.0;
#ifdef PRINT_INFO
    mexPrintf("V0\n        ");
    for (j=0; j<ninit; j++)
        mexPrintf("%8d",j+1);
    mexPrintf("\n");
    for (i=0; i<mrows; i++) {
        mexPrintf("%8d",i);
        for (j=0; j<ninit; j++)
            mexPrintf("%8.1le",pr[j*n+i]);
	mexPrintf("\n");
    }
#endif

    condest=*mxGetPr(mxGetField(options_input,0,"condest"));
#ifdef PRINT_INFO
    mexPrintf("condest=%8.1le\n",condest);
#endif
    /* additional parameters set to default */
    icntl[0]=0;
    icntl[1]=0;
    icntl[2]=0;
    icntl[3]=condest;
    icntl[4]=0;

    /* turn off adaptive preconditioning if requested by the user */
    if (mxGetField(options_input,0,"noadaptprec")!=NULL) {
       i=*mxGetPr(mxGetField(options_input,0,"noadaptprec"));
       if (i)
	  icntl[2]=1;
    }

#ifdef PRINT_INFO
    mexPrintf("icntl[1]=%4d\n",icntl[0]);
    mexPrintf("icntl[2]=%4d\n",icntl[1]);
    mexPrintf("icntl[3]=%4d\n",icntl[2]);
    mexPrintf("icntl[4]=%4d\n",icntl[3]);
    mexPrintf("icntl[5]=%4d\n",icntl[4]);
#endif





    /* number of eigenvalues computed by JADAMILU on exit */
    neig=maxeig;
    /* estimated gap w.r.t to the next eigenvalue */
    gap=0.0;
#ifdef PRINT_INFO
    mexPrintf("call PJD\n"); fflush(stdout);
#endif
    dpjd_gep(&n, a,ja,ia, m,jm,im, eigs, res, X, &lx, &neig, 
	     &sigma, &isearch, &ninit, &madspace, &iter, &tol,
	     &shift, &droptol, &mem, icntl,
	     &iprint, &info, &gap);
#ifdef PRINT_INFO
    mexPrintf("PJD completed\n"); fflush(stdout);
    mexPrintf("call PJD cleanup\n"); fflush(stdout);
#endif
    dpjdcleanup();
#ifdef PRINT_INFO
    mexPrintf("PJD clean up completed\n"); fflush(stdout);
#endif


    /* release temporarily used matrix */
    mxFree(ia);
    mxFree(ja);
    mxFree(a);
    mxFree(im);
    mxFree(jm);
    mxFree(m);




    /* three output arguments */
    nlhs=3;
    /* Create an n x neig double matrix for output */
    mydims[0]=n;
    mydims[1]=neig;
    plhs[0]=mxCreateNumericArray(2, mydims, mxDOUBLE_CLASS, mxREAL);
    V_output=plhs[0];
    pr=mxGetPr(V_output);
    /* return eigenvectors */
    for (j=0; j<n*neig; j++)
        pr[j]=X[j];
    mxFree(X);
#ifdef PRINT_INFO
    mexPrintf("eigenvectors passed\n"); fflush(stdout);
#endif

    /* Create a neig x neig sparse matrix for output */
    plhs[1]=mxCreateSparse((mwSize)neig,(mwSize)neig, (mwSize)neig, mxREAL);
    D_output=plhs[1];
    A_ja     =(mwIndex *)mxGetIr(D_output);
    A_ia     =(mwIndex *)mxGetJc(D_output);
    A_valuesR=(double *) mxGetPr(D_output);
    for (i=0; i<neig; i++) {
        A_ia[i]=i;
        A_ja[i]=i;
        A_valuesR[i]=sign*eigs[i];
    }
    A_ia[neig]=neig;
    mxFree(eigs);
#ifdef PRINT_INFO
    mexPrintf("eigenvalues passed\n"); fflush(stdout);
#endif


    plhs[2] = mxCreateStructMatrix((mwSize)1, (mwSize)1, nfields, fnames);
    options_output=plhs[2];

    /* copy data */
    for (ifield=0; ifield<nfields; ifield++) {
	tmp=mxGetFieldByNumber(options_input,(mwIndex)0,ifield);
	classIDflags[ifield]=mxGetClassID(tmp); 

	ndim=mxGetNumberOfDimensions(tmp);
	dims=mxGetDimensions(tmp);

	/* Create string/numeric array */
	if (classIDflags[ifield]==mxCHAR_CLASS) {
 	   /* Get the length of the input string. */
	   buflen=(mxGetM(tmp)*mxGetN(tmp))+1;
	   input_buf=(char *) mxCalloc((size_t)buflen, (size_t)sizeof(char));
	   mxGetString(tmp, input_buf, buflen);

	   output_buf=(char *)mxCalloc((size_t)buflen, (size_t)sizeof(char));
	   strcpy(output_buf,input_buf);
	   fout=mxCreateString(output_buf);
	   mxFree(input_buf);
	}
	else { /* numerical values */
	   if (!strcmp("niter",fnames[ifield])) {
	      mydims[0]=mydims[1]=1;
	      fout=mxCreateNumericArray(2, mydims, classIDflags[ifield], mxREAL);
	      pr=mxGetPr(fout);
	      *pr=iter;
	   }
	   else if (!strcmp("res",fnames[ifield])) {
	      mydims[0]=neig;
	      mydims[1]=1;
	      fout=mxCreateNumericArray(2, mydims, classIDflags[ifield], mxREAL);
	      pr=mxGetPr(fout);
	      for (i=0; i<neig; i++)
		  pr[i]=res[i];
	   }
	   else if (!strcmp("info",fnames[ifield])) {
	      mydims[0]=mydims[1]=1;
	      fout=mxCreateNumericArray(2, mydims, classIDflags[ifield], mxREAL);
	      pr=mxGetPr(fout);
	      *pr=info;
	   }
	   else if (!strcmp("gap",fnames[ifield])) {
	      mydims[0]=mydims[1]=1;
	      fout=mxCreateNumericArray(2, mydims, classIDflags[ifield], mxREAL);
	      pr=mxGetPr(fout);
	      *pr=gap;
	   }
	   else { /* pass input to output */
	      fout=mxCreateNumericArray((mwSize)ndim, dims, 
					classIDflags[ifield], mxREAL);
	      pdata=mxGetData(fout);
	      sizebuf=mxGetElementSize(tmp);
	      memcpy(pdata, mxGetData(tmp), sizebuf*dims[0]*dims[1]);
	   }
	}

	/* Set each field in output structure */
	mxSetFieldByNumber(options_output, (mwIndex)0, ifield, fout);
#ifdef PRINT_INFO
	mexPrintf("options.%s passed\n",fnames[ifield]); fflush(stdout);
#endif
    }

    mxFree(fnames);
    mxFree(res);

    return;
}

