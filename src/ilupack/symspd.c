#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <math.h>

#include <blas.h>
#include <lapack.h>
#include <ilupack.h>
#include <ilupackmacros.h>

#define MAX_LINE        255
#define STDERR          stderr
#define STDOUT          stdout

//#define PRINT_INFO

#define MAX(A,B)        (((A)>(B))?(A):(B))
#define MIN(A,B)        (((A)<(B))?(A):(B))
#define FRABS(A)         (((A)>=0)?(A):(-(A)))











/*
   Given a multilevel ILU decomposition performed by SYMAMGsetup, 
   SYMSPDconvert performs a conversion from a real symmetric or
   complex Hermitian but indefinite preconditioner to a positive
   definite preconditioner
*/ 
integer SYMSPDCONVERT(AMGLEVELMAT *PRE, integer nlev)

{

/*
   PRE       linked list of multilevel ILU preconditioners
   nlev      number of levels (blocks)
   rhs       given right hand side
   sol       solution obtained by SYMAMGsol
   buff      work array of length 2n, where n is the size of the original 
             matrix
  
   Code written by Matthias Bollhoefer, September 2005
*/

   AMGLEVELMAT *next, *prev;

   integer i, j, k, l=1,n, nB, lev,buffldl,buffldl2,ierr,factspd;
   
   FLOAT aji,sigma, G[4], *p, buff;
   REALS val, aii, ajj, c, s, nrm, tau, t, lambdai, lambdaj,nrmdiag;


   n=PRE->n;
   next=PRE;
   prev=NULL;
   lev=1;
   
   nrmdiag=0;
   while (lev<nlev) {
	 nB=next->nB;
 
#ifdef PRINT_INFO
	 printf("level %3d\n",lev);fflush(stdout);
#endif

	 /*
	   next->LU.a     stored as upper triangular matrix
	   nB             size
	   next->LU.ja    associated indices
	 */
	 i=0;
	 while (i<nB) {

               // 1x1 pivot
               if (next->LU.ja[nB+1+i]==0) {
		  nrmdiag=MAX(nrmdiag,FABS(next->LU.a[i]));
		  i++;
	       }
	       else { // 2x2 pivot 
#if defined _DOUBLE_REAL_ || defined _SINGLE_REAL_
		  aii=next->LU.a[i];
		  aji=next->LU.a[nB+1+i];
		  ajj=next->LU.a[i+1];

		  // compute Jacobi rotation that diagonalize
		  //    [aii aji']
		  // A= [        ]
		  //    [aji ajj ]
		  
		  // numerical almost diagonal matrix
		  if (FABS(aji)<=1e-8*(FRABS(ajj-aii))) {
		     c=1.0;
		     s=0.0;
		  }
		  else {
		     tau=(ajj-aii)/(2*aji);
		     if (tau>=0.0)
		        t= 1.0/( tau+sqrt(1.0+tau*tau));
		     else
		        t=-1.0/(-tau+sqrt(1.0+tau*tau));
		     c=1.0/sqrt(1.0+t*t); 
		     s=t*c;
		  }
		  // compute eigenvalues and take their modulus
		  // [lambdai     0   ]                       [ c   s]
		  // [                ] = J'* A * J, where J= [      ]
		  // [   0     lambdaj]		              [-s   c]
		  lambdai=c*c*aii+s*s*ajj-2*c*s*aji;
		  lambdaj=s*s*aii+c*c*ajj+2*c*s*aji;
#else
		  aii=next->LU.a[i].r;
		  aji=next->LU.a[nB+1+i];
		  ajj=next->LU.a[i+1].r;

		  // compute Jacobi rotation that diagonalize
		  //    [aii aji']
		  // A= [        ]
		  //    [aji ajj ]

		  // numerical almost diagonal matrix
		  nrm=FABS(aji);
		  if (nrm<=1e-8*(FRABS(ajj-aii))) {
		     c=1.0;
		     s=0.0;
		     sigma.r=1.0;
		     sigma.i=0.0;
		  }
		  else {
		     // sign of aji. Note that sigma*aji=nrm=|aji|
		     sigma.r= aji.r/nrm;
		     sigma.i=-aji.i/nrm;
		     tau=(ajj-aii)/(2*nrm);
		     if (tau>=0.0)
		        t= 1.0/( tau+sqrt(1.0+tau*tau));
		     else
		        t=-1.0/(-tau+sqrt(1.0+tau*tau));
		     c=1.0/sqrt(1.0+t*t); 
		     s=t*c;
		  }
		  // compute eigenvalues and take their modulus
		  // [lambdai     0   ]                       [   c     sigma*s]
		  // [                ] = J'* A * J, where J= [                ]
		  // [   0     lambdaj]		              [-sigma'*s   c   ]
		  lambdai=c*c*aii+s*s*ajj-2*c*s*nrm;
		  lambdaj=s*s*aii+c*c*ajj+2*c*s*nrm;

#endif
		  nrmdiag=MAX(nrmdiag,FRABS(lambdai));
		  nrmdiag=MAX(nrmdiag,FRABS(lambdaj));
		  i+=2;
	       }
	 } // end while

	 prev=next;
	 next=next->next;
	 lev++;
   } // end while




   // last level
   nB=next->nB;


   /* -----   last block   ----- */

   /* did we finally switch to full matrix processing? */
   if (lev>1 && next->LU.ja==NULL) {
      /*
	next->LU.a     stored as lower triangular matrix, packed format
        nB             size
        next->LU.ia    permutation array
      */
      i=0;
      j=0;
      while (i<nB) {
            // 1x1 pivot
            if (next->LU.ia[i]>0) {
	       // take the absolute value
	       nrmdiag=MAX(nrmdiag,FABS(next->LU.a[j]));
	       j+=nB-i;
	       i++;
	    }
	    else { // 2x2 pivot 
#if defined _DOUBLE_REAL_ || defined _SINGLE_REAL_
	       aii=next->LU.a[j];
	       aji=next->LU.a[j+1];
	       ajj=next->LU.a[j+nB-i];
	       
	       // compute Jacobi rotation that diagonalize
	       //    [aii aji']
	       // A= [        ]
	       //    [aji ajj ]
	       
	       // numerical almost diagonal matrix
	       if (FABS(aji)<=1e-8*(FRABS(ajj-aii))) {
 	          c=1.0;
		  s=0.0;
	       }
	       else {
	          tau=(ajj-aii)/(2*aji);
		  if (tau>=0.0)
		     t= 1.0/( tau+sqrt(1.0+tau*tau));
		  else
		     t=-1.0/(-tau+sqrt(1.0+tau*tau));
		  c=1.0/sqrt(1.0+t*t); 
		  s=t*c;
	       }
	       // compute eigenvalues and take their modulus
	       // [lambdai     0   ]                       [ c   s]
	       // [                ] = J'* A * J, where J= [      ]
	       // [   0     lambdaj]			  [-s   c]
	       lambdai=c*c*aii+s*s*ajj-2*c*s*aji;
	       lambdaj=s*s*aii+c*c*ajj+2*c*s*aji;
#else
	       aii=next->LU.a[j].r;
	       aji=next->LU.a[j+1];
	       ajj=next->LU.a[j+nB-i].r;

	       // compute Jacobi rotation that diagonalize
	       //    [aii aji']
	       // A= [        ]
	       //    [aji ajj ]
	       
	       // numerical almost diagonal matrix
	       nrm=FABS(aji);
	       if (nrm<=1e-8*(FRABS(ajj-aii))) {
 	          c=1.0;
		  s=0.0;
		  sigma.r=1.0;
		  sigma.i=0.0;
	       }
	       else {
		  // sign of aji. Note that sigma*aji=nrm=|aji|
		  sigma.r= aji.r/nrm;
		  sigma.i=-aji.i/nrm;
		  tau=(ajj-aii)/(2*nrm);
		  if (tau>=0.0)
		     t= 1.0/( tau+sqrt(1.0+tau*tau));
		  else
		     t=-1.0/(-tau+sqrt(1.0+tau*tau));
		  c=1.0/sqrt(1.0+t*t); 
		  s=t*c;
	       }
	       // compute eigenvalues and take their modulus
	       // [lambdai     0   ]                       [   c     sigma*s]
	       // [                ] = J'* A * J, where J= [                ]
	       // [   0     lambdaj]			  [-sigma'*s   c   ]
	       lambdai=c*c*aii+s*s*ajj-2*c*s*nrm;
	       lambdaj=s*s*aii+c*c*ajj+2*c*s*nrm;
#endif
	       nrmdiag=MAX(nrmdiag,FRABS(lambdai));
	       nrmdiag=MAX(nrmdiag,FRABS(lambdaj));
	       j+=2*(nB-i)-1;
	       i+=2;
	    }
      } // end while
   }
   else { // sparse case
      /*
        next->LU.a     stored as upper triangular matrix
	nB             size
	next->LU.ja    associated indices
      */
      i=0;
      while (i<nB) {
            // 1x1 pivot
            if (next->LU.ja[nB+1+i]==0) {
	       // take the absolute value
#if defined _DOUBLE_REAL_ || defined _SINGLE_REAL_
	       nrmdiag=MAX(nrmdiag,FABS(next->LU.a[i]));
#else
	       nrmdiag=MAX(nrmdiag,FRABS(next->LU.a[i].r));
#endif
	       i++;
	    }
	    else { // 2x2 pivot 
#if defined _DOUBLE_REAL_ || defined _SINGLE_REAL_
	       aii=next->LU.a[i];
	       aji=next->LU.a[nB+1+i];
	       ajj=next->LU.a[i+1];
	       
	       // compute Jacobi rotation that diagonalize
	       //    [aii aji']
	       // A= [        ]
	       //    [aji ajj ]
	       
	       // numerical almost diagonal matrix
	       if (FABS(aji)<=1e-8*(FRABS(ajj-aii))) {
		  c=1.0;
		  s=0.0;
	       }
	       else {
		  tau=(ajj-aii)/(2*aji);
		  if (tau>=0.0)
		     t= 1.0/( tau+sqrt(1.0+tau*tau));
		  else
		     t=-1.0/(-tau+sqrt(1.0+tau*tau));
		  c=1.0/sqrt(1.0+t*t); 
		  s=t*c;
	       }
	       // compute eigenvalues and take their modulus
	       // [lambdai     0   ]                       [ c   s]
	       // [                ] = J'* A * J, where J= [      ]
	       // [   0     lambdaj]		           [-s   c]
	       lambdai=c*c*aii+s*s*ajj-2*c*s*aji;
	       lambdaj=s*s*aii+c*c*ajj+2*c*s*aji;


#else
	       aii=next->LU.a[i].r;
	       aji=next->LU.a[nB+1+i];
	       ajj=next->LU.a[i+1].r;
	       
	       // compute Jacobi rotation that diagonalize
	       //    [aii aji']
	       // A= [        ]
	       //    [aji ajj ]
	       
	       // numerical almost diagonal matrix
	       nrm=FABS(aji);
	       if (nrm<=1e-8*(FRABS(ajj-aii))) {
		  c=1.0;
		  s=0.0;
		  sigma.r=1.0;
		  sigma.i=0.0;
	       }
	       else {
		  // sign of aji. Note that sigma*aji=nrm=|aji|
		  sigma.r= aji.r/nrm;
		  sigma.i=-aji.i/nrm;
		  tau=(ajj-aii)/(2*nrm);
		  if (tau>=0.0)
		     t= 1.0/( tau+sqrt(1.0+tau*tau));
		  else
		     t=-1.0/(-tau+sqrt(1.0+tau*tau));
		  c=1.0/sqrt(1.0+t*t); 
		  s=t*c;
	       }
	       // compute eigenvalues and take their modulus
	       // [lambdai     0   ]                       [   c     sigma*s]
	       // [                ] = J'* A * J, where J= [                ]
	       // [   0     lambdaj]			  [-sigma'*s   c   ]
	       lambdai=c*c*aii+s*s*ajj-2*c*s*nrm;
	       lambdaj=s*s*aii+c*c*ajj+2*c*s*nrm;


#endif
	       nrmdiag=MAX(nrmdiag,FRABS(lambdai));
	       nrmdiag=MAX(nrmdiag,FRABS(lambdaj));
	       i+=2;
	    }
      } // end while
   } // end if-else (next->LU.ja==NULL)






   next=PRE;
   prev=NULL;
   lev=1;
   factspd=0;
   // maximum diagonal entry in absolute value
   // weighted with the square root of the machine precision
   nrmdiag=sqrt(dgeteps())*nrmdiag;

   while (lev<nlev) {
	 nB=next->nB;

#ifdef PRINT_INFO
	 printf("level %3d\n",lev);fflush(stdout);
#endif
	 
	 /*
	   next->LU.a     stored as upper triangular matrix
	   nB             size
	   next->LU.ja    associated indices
	 */
	 i=0;
	 next->absdiag=(FLOAT *)MALLOC(2*(nB+1)*sizeof(FLOAT),"symspd:absdiag");
	 while (i<nB) {

               // 1x1 pivot
               if (next->LU.ja[nB+1+i]==0) {
#if defined _DOUBLE_REAL_ || defined _SINGLE_REAL_
		  // take the absolute value
		  val=next->LU.a[i];
#ifdef PRINT_INFO
		  printf("old: %12.4le\n", next->LU.a[i]);
#endif
		  /*
		    if (val<0.0 && i<nB-1) {
		       sigma=-1.0;
		       j=next->LU.ja[i];
		       k=next->LU.ja[i+1]-j;
		       l=1;
		       SCAL(&k,&sigma,next->LU.a+j-1,&l);
		    }
		  */
		  next->absdiag[i]=FRABS(val);
#ifdef PRINT_INFO
		  printf("new: %12.4le\n\n", next->absdiag[i]);
		  /*
		    j=next->LU.ja[i];
		    k=next->LU.ja[i+1]-j;
		    for (l=0; l<k; l++) 
		    printf("%12.4le",next->LU.a[j-1+l]);
		    printf("\n\n");
		  */
#endif
#else
		  val=next->LU.a[i].r;
#ifdef PRINT_INFO
		  printf("old: %12.4le+%12.4lei\n", 
			 next->LU.a[i].r, next->LU.a[i].i);
#endif
		  /* 
		     if (val<0.0) {
		        sigma.r=-1.0;
		        sigma.i= 0.0;
		        j=next->LU.ja[i];
		        k=next->LU.ja[i+1]-j;
		        l=1;
		        SCAL(&k,&sigma,next->LU.a+j-1,&l);
		     }
		  */
		  next->absdiag[i].r=FRABS(val);
		  next->absdiag[i].i=0.0;
#ifdef PRINT_INFO
		  printf("new: %12.4le+%12.4lei\n\n", 
			 next->absdiag[i].r, next->absdiag[i].i);
#endif
#endif
		  if (val<-nrmdiag)
		     factspd++;
		  i++;
	       }
	       else { // 2x2 pivot 
#if defined _DOUBLE_REAL_ || defined _SINGLE_REAL_
#ifdef PRINT_INFO
		  printf("old: %12.4le%12.4le\n", next->LU.a[i],     next->LU.a[nB+1+i]);
		  printf("     %12.4le%12.4le\n", next->LU.a[nB+1+i],next->LU.a[i+1]);
#endif
		  aii=next->LU.a[i];
		  aji=next->LU.a[nB+1+i];
		  ajj=next->LU.a[i+1];

		  // compute Jacobi rotation that diagonalize
		  //    [aii aji']
		  // A= [        ]
		  //    [aji ajj ]
		  
		  // numerical almost diagonal matrix
		  if (FABS(aji)<=1e-8*(FRABS(ajj-aii))) {
		     c=1.0;
		     s=0.0;
		  }
		  else {
		     tau=(ajj-aii)/(2*aji);
		     if (tau>=0.0)
		        t= 1.0/( tau+sqrt(1.0+tau*tau));
		     else
		        t=-1.0/(-tau+sqrt(1.0+tau*tau));
		     c=1.0/sqrt(1.0+t*t); 
		     s=t*c;
		  }
		  // compute eigenvalues and take their modulus
		  // [lambdai     0   ]                       [ c   s]
		  // [                ] = J'* A * J, where J= [      ]
		  // [   0     lambdaj]		              [-s   c]
		  lambdai=c*c*aii+s*s*ajj-2*c*s*aji;
		  lambdaj=s*s*aii+c*c*ajj+2*c*s*aji;
		  if (lambdai<-nrmdiag)
		     factspd++;
		  if (lambdaj<-nrmdiag)
		     factspd++;

                  /* 
		     G[0]=G[1]=G[2]=G[3]=0.0;
		     if (lambdai<0.0) {
		        if (lambdaj<0.0) {
		           //     [ c   s] [-1   0] [ c  -s]  
		           // G = [      ]	[      ] [      ]
		           //     [-s   c]	[ 0  -1] [ s   c]
		           G[0]=G[3]=-1.0;
		        }
		        else {
		           //     [ c   s] [-1   0] [ c  -s]  
		           // G = [      ]	[      ] [      ]
		           //     [-s   c]	[ 0   1] [ s   c]
		           G[0]=-c*c+s*s;  G[2]= 2*c*s;
			   G[1]= 2*c*s;    G[3]=-s*s+c*c;
		        }
		     }
		     else
		        if (lambdaj<0.0) {
		           //     [ c   s] [ 1   0] [ c  -s]  
		           // G = [      ]	[      ] [      ]
		           //     [-s   c]	[ 0  -1] [ s   c]
		           G[0]= c*c-s*s;  G[2]=-2*c*s;
			   G[1]=-2*c*s;    G[3]= s*s-c*c;
		        }
		        else
		           G[0]=G[3]=1.0;
		  
		     j=next->LU.ja[i];
		     k=next->LU.ja[i+1]-j;
		     p=next->LU.a+j-1;
		     for (l=0; l<k; l++) {
		         buff=G[0]*p[0]+G[2]*p[1];
		         p[1]=G[1]*p[0]+G[3]*p[1];
		         p[0]=buff;
		         p+=2;
  		     }
		  */	  

		  // recompute |A|
		  //          [|lambdai|     0    ]     
		  // |A|= J * [                   ] * J'
		  //          [    0     |lambdaj|]	   
		  lambdai=FRABS(lambdai);
		  lambdaj=FRABS(lambdaj);
		  next->absdiag[i]     = c*c*lambdai+s*s*lambdaj;
		  next->absdiag[nB+1+i]=-s*c*lambdai+c*s*lambdaj;
		  next->absdiag[i+1]   = s*s*lambdai+c*c*lambdaj;
#ifdef PRINT_INFO
		  printf("new: %12.4le%12.4le\n",  next->absdiag[i],     next->absdiag[nB+1+i]);
		  printf("     %12.4le%12.4le\n\n",next->absdiag[nB+1+i],next->absdiag[i+1]);
		  /*
		    j=next->LU.ja[i];
		    k=next->LU.ja[i+1]-j;
		    for (l=0; l<k; l++) 
		    printf("%12.4le",next->LU.a[j-1+2*l]);
		    printf("\n");
		    for (l=0; l<k; l++) 
		    printf("%12.4le",next->LU.a[j-1+2*l+1]);
		    printf("\n\n");
		  */
#endif
#else
#ifdef PRINT_INFO
		  printf("old: %12.4le+%12.4lei%12.4le+%12.4lei\n", 
			 next->LU.a[i].r,     next->LU.a[i].i,
			 next->LU.a[nB+1+i].r,next->LU.a[nB+1+i].i);
		  printf("     %12.4le+%12.4lei%12.4le+%12.4lei\n", 
			 next->LU.a[nB+1+i].r,next->LU.a[nB+1+i].i,
			 next->LU.a[i+1].r,   next->LU.a[i+1].i);
#endif
		  aii=next->LU.a[i].r;
		  aji=next->LU.a[nB+1+i];
		  ajj=next->LU.a[i+1].r;

		  // compute Jacobi rotation that diagonalize
		  //    [aii aji']
		  // A= [        ]
		  //    [aji ajj ]

		  // numerical almost diagonal matrix
		  nrm=FABS(aji);
		  if (nrm<=1e-8*(FRABS(ajj-aii))) {
		     c=1.0;
		     s=0.0;
		     sigma.r=1.0;
		     sigma.i=0.0;
		  }
		  else {
		     // sign of aji. Note that sigma*aji=nrm=|aji|
		     sigma.r= aji.r/nrm;
		     sigma.i=-aji.i/nrm;
		     tau=(ajj-aii)/(2*nrm);
		     if (tau>=0.0)
		        t= 1.0/( tau+sqrt(1.0+tau*tau));
		     else
		        t=-1.0/(-tau+sqrt(1.0+tau*tau));
		     c=1.0/sqrt(1.0+t*t); 
		     s=t*c;
		  }
		  // compute eigenvalues and take their modulus
		  // [lambdai     0   ]                       [   c     sigma*s]
		  // [                ] = J'* A * J, where J= [                ]
		  // [   0     lambdaj]		              [-sigma'*s   c   ]
		  lambdai=c*c*aii+s*s*ajj-2*c*s*nrm;
		  lambdaj=s*s*aii+c*c*ajj+2*c*s*nrm;
		  if (lambdai<-nrmdiag)
		     factspd++;
		  if (lambdaj<-nrmdiag)
		     factspd++;

		  // recompute |A|
		  //          [|lambdai|     0    ]     
		  // |A|= J * [                   ] * J'
		  //          [    0     |lambdaj|]
		  lambdai=FRABS(lambdai);
		  lambdaj=FRABS(lambdaj);
		  next->absdiag[i].r     = c*c*lambdai+s*s*lambdaj;
		  next->absdiag[i].i     = 0;
		  next->absdiag[nB+1+i].r= sigma.r*(-s*c*lambdai+c*s*lambdaj);
		  next->absdiag[nB+1+i].i=-sigma.i*(-s*c*lambdai+c*s*lambdaj);
		  next->absdiag[i+1].r   = s*s*lambdai+c*c*lambdaj;
		  next->absdiag[i+1].i   = 0;
#ifdef PRINT_INFO
		  printf("new: %12.4le+%12.4lei%12.4le+%12.4lei\n", 
			 next->absdiag[i].r,     next->absdiag[i].i,
			 next->absdiag[nB+1+i].r,next->absdiag[nB+1+i].i);
		  printf("     %12.4le+%12.4lei%12.4le+%12.4lei\n\n", 
			 next->absdiag[nB+1+i].r,next->absdiag[nB+1+i].i,
			 next->absdiag[i+1].r,   next->absdiag[i+1].i);
#endif
#endif
		  i+=2;
	       }
	 } // end while
	 // flag first index negative to indicate the SPD conversion
	 next->LU.ja[0]=-next->LU.ja[0];



	 prev=next;
	 next=next->next;
	 lev++;
   } // end while




   // last level
   nB=next->nB;

#ifdef PRINT_INFO
   printf("level %3d\n",lev);fflush(stdout);
#endif




   /* -----   last block   ----- */

   /* did we finally switch to full matrix processing? */
   if (lev>1 && next->LU.ja==NULL) {
      /*
	next->LU.a     stored as lower triangular matrix, packed format
        nB             size
        next->LU.ia    permutation array
      */
      i=0;
      j=0;
      while (i<nB) {
            // 1x1 pivot
            if (next->LU.ia[i]>0) {
	       // take the absolute value
#if defined _DOUBLE_REAL_ || defined _SINGLE_REAL_
	       val=next->LU.a[j];
#ifdef PRINT_INFO
	       printf("old: %12.4le\n", next->LU.a[j]);
#endif
	       next->LU.a[j]=FABS(val);
#ifdef PRINT_INFO
	       printf("new: %12.4le\n\n", next->LU.a[j]);
#endif
#else
	       val=next->LU.a[j].r;
#ifdef PRINT_INFO
	       printf("old: %12.4le+%12.4lei\n", 
		      next->LU.a[j].r, next->LU.a[j].i);
#endif
	       next->LU.a[j].r=FRABS(val);
	       next->LU.a[j].i=0.0;
#ifdef PRINT_INFO
	       printf("new: %12.4le+%12.4lei\n\n", 
		      next->LU.a[j].r, next->LU.a[j].i);
#endif
#endif
	       if (val<-nrmdiag)
		  factspd++;
	       j+=nB-i;
	       i++;
	    }
	    else { // 2x2 pivot 
#if defined _DOUBLE_REAL_ || defined _SINGLE_REAL_
#ifdef PRINT_INFO
	       printf("old: %12.4le%12.4le\n", next->LU.a[j],  next->LU.a[j+1]);
	       printf("     %12.4le%12.4le\n", next->LU.a[j+1],next->LU.a[j+nB-i]);
#endif
	       aii=next->LU.a[j];
	       aji=next->LU.a[j+1];
	       ajj=next->LU.a[j+nB-i];
	       
	       // compute Jacobi rotation that diagonalize
	       //    [aii aji']
	       // A= [        ]
	       //    [aji ajj ]
	       
	       // numerical almost diagonal matrix
	       if (FABS(aji)<=1e-8*(FRABS(ajj-aii))) {
 	          c=1.0;
		  s=0.0;
	       }
	       else {
	          tau=(ajj-aii)/(2*aji);
		  if (tau>=0.0)
		     t= 1.0/( tau+sqrt(1.0+tau*tau));
		  else
		     t=-1.0/(-tau+sqrt(1.0+tau*tau));
		  c=1.0/sqrt(1.0+t*t); 
		  s=t*c;
	       }
	       // compute eigenvalues and take their modulus
	       // [lambdai     0   ]                       [ c   s]
	       // [                ] = J'* A * J, where J= [      ]
	       // [   0     lambdaj]			  [-s   c]
	       lambdai=c*c*aii+s*s*ajj-2*c*s*aji;
	       lambdaj=s*s*aii+c*c*ajj+2*c*s*aji;
	       if (lambdai<-nrmdiag)
		  factspd++;
	       if (lambdaj<-nrmdiag)
		  factspd++;

	       // recompute |A|
	       //          [|lambdai|     0    ]     
	       // |A|= J * [                   ] * J'
	       //          [    0     |lambdaj|]	   
	       lambdai=FRABS(lambdai);
	       lambdaj=FRABS(lambdaj);
	       next->LU.a[j]     = c*c*lambdai+s*s*lambdaj;
	       next->LU.a[j+1]   =-s*c*lambdai+c*s*lambdaj;
	       next->LU.a[j+nB-i]= s*s*lambdai+c*c*lambdaj;
#ifdef PRINT_INFO
	       printf("new: %12.4le%12.4le\n",  next->LU.a[j],  next->LU.a[j+1]);
	       printf("     %12.4le%12.4le\n\n",next->LU.a[j+1],next->LU.a[j+nB-i]);
#endif
#else
#ifdef PRINT_INFO
	       printf("old: %12.4le+%12.4lei%12.4le+%12.4lei\n", 
		      next->LU.a[j].r,  next->LU.a[j].i,
		      next->LU.a[j+1].r,next->LU.a[j+1].i);
	       printf("     %12.4le+%12.4lei%12.4le+%12.4lei\n", 
		      next->LU.a[j+1].r,   next->LU.a[j+1].i,
		      next->LU.a[j+nB-i].r,next->LU.a[j+nB-i].i);
#endif
	       aii=next->LU.a[j].r;
	       aji=next->LU.a[j+1];
	       ajj=next->LU.a[j+nB-i].r;

	       // compute Jacobi rotation that diagonalize
	       //    [aii aji']
	       // A= [        ]
	       //    [aji ajj ]
	       
	       // numerical almost diagonal matrix
	       nrm=FABS(aji);
	       if (nrm<=1e-8*(FRABS(ajj-aii))) {
 	          c=1.0;
		  s=0.0;
		  sigma.r=1.0;
		  sigma.i=0.0;
	       }
	       else {
		  // sign of aji. Note that sigma*aji=nrm=|aji|
		  sigma.r= aji.r/nrm;
		  sigma.i=-aji.i/nrm;
		  tau=(ajj-aii)/(2*nrm);
		  if (tau>=0.0)
		     t= 1.0/( tau+sqrt(1.0+tau*tau));
		  else
		     t=-1.0/(-tau+sqrt(1.0+tau*tau));
		  c=1.0/sqrt(1.0+t*t); 
		  s=t*c;
	       }
	       // compute eigenvalues and take their modulus
	       // [lambdai     0   ]                       [   c     sigma*s]
	       // [                ] = J'* A * J, where J= [                ]
	       // [   0     lambdaj]			  [-sigma'*s   c   ]
	       lambdai=c*c*aii+s*s*ajj-2*c*s*nrm;
	       lambdaj=s*s*aii+c*c*ajj+2*c*s*nrm;
	       if (lambdai<-nrmdiag)
		  factspd++;
	       if (lambdaj<-nrmdiag)
		  factspd++;

	       // recompute |A|
	       //          [|lambdai|     0    ]     
	       // |A|= J * [                   ] * J'
	       //          [    0     |lambdaj|]	   
	       lambdai=FRABS(lambdai);
	       lambdaj=FRABS(lambdaj);
	       next->LU.a[j].r     = c*c*lambdai+s*s*lambdaj;
	       next->LU.a[j].i     = 0;
	       next->LU.a[j+1].r   = sigma.r*(-s*c*lambdai+c*s*lambdaj);
	       next->LU.a[j+1].i   =-sigma.i*(-s*c*lambdai+c*s*lambdaj);
	       next->LU.a[j+nB-i].r= s*s*lambdai+c*c*lambdaj;
	       next->LU.a[j+nB-i].i= 0;
#ifdef PRINT_INFO
	       printf("new: %12.4le+%12.4lei%12.4le+%12.4lei\n", 
		      next->LU.a[j].r,  next->LU.a[j].i,
		      next->LU.a[j+1].r,next->LU.a[j+1].i);
	       printf("     %12.4le+%12.4lei%12.4le+%12.4lei\n\n", 
		      next->LU.a[j+1].r,   next->LU.a[j+1].i,
		      next->LU.a[j+nB-i].r,next->LU.a[j+nB-i].i);
#endif
#endif
	       j+=2*(nB-i)-1;
	       i+=2;
	    }
      } // end while
   }
   else { // sparse case
      /*
        next->LU.a     stored as upper triangular matrix
	nB             size
	next->LU.ja    associated indices
      */
      i=0;
      next->absdiag=(FLOAT *)MALLOC(2*(nB+1)*sizeof(FLOAT),"symspd:absdiag");
      while (i<nB) {
            // 1x1 pivot
            if (next->LU.ja[nB+1+i]==0) {
	       // take the absolute value
#if defined _DOUBLE_REAL_ || defined _SINGLE_REAL_
	       val=next->LU.a[i];
#ifdef PRINT_INFO
	       printf("old: %12.4le\n", next->LU.a[i]);
#endif
	       /*
		 if (val<0.0) {
		 sigma=-1.0;
		 j=next->LU.ja[i];
		 k=next->LU.ja[i+1]-j;
		 l=1;
		 SCAL(&k,&sigma,next->LU.a+j-1,&l);
		 }
	       */
	       next->absdiag[i]=FRABS(val);
#ifdef PRINT_INFO
	       printf("new: %12.4le\n\n", next->absdiag[i]);
	       /*
		 j=next->LU.ja[i];
		 k=next->LU.ja[i+1]-j;
		 for (l=0; l<k; l++) 
		 printf("%12d",next->LU.ja[j-1+l]);
		 printf("\n");
		 for (l=0; l<k; l++) 
		 printf("%12.4le",next->LU.a[j-1+l]);
		 printf("\n\n");
	       */
#endif
#else
	       val=next->LU.a[i].r;
#ifdef PRINT_INFO
	       printf("old: %12.4le+%12.4lei\n", 
		      next->LU.a[i].r, next->LU.a[i].i);
#endif
	       /*
		 if (val<0.0) {
		 sigma.r=-1.0;
		 sigma.i= 0.0;
		 j=next->LU.ja[i];
		 k=next->LU.ja[i+1]-j;
		 l=1;
		 SCAL(&k,&sigma,next->LU.a+j-1,&l);
		 }
	       */
	       next->absdiag[i].r=FRABS(val);
	       next->absdiag[i].i=0.0;
#ifdef PRINT_INFO
	       printf("new: %12.4le+%12.4lei\n\n", 
		      next->absdiag[i].r, next->absdiag[i].i);
#endif
#endif
	       if (val<-nrmdiag)
		  factspd++;
	       i++;
	    }
	    else { // 2x2 pivot 
#if defined _DOUBLE_REAL_ || defined _SINGLE_REAL_
#ifdef PRINT_INFO
	       printf("old: %12.4le%12.4le\n", next->LU.a[i],     next->LU.a[nB+1+i]);
	       printf("     %12.4le%12.4le\n", next->LU.a[nB+1+i],next->LU.a[i+1]);
#endif
	       aii=next->LU.a[i];
	       aji=next->LU.a[nB+1+i];
	       ajj=next->LU.a[i+1];
	       
	       // compute Jacobi rotation that diagonalize
	       //    [aii aji']
	       // A= [        ]
	       //    [aji ajj ]
	       
	       // numerical almost diagonal matrix
	       if (FABS(aji)<=1e-8*(FRABS(ajj-aii))) {
		  c=1.0;
		  s=0.0;
	       }
	       else {
		  tau=(ajj-aii)/(2*aji);
		  if (tau>=0.0)
		     t= 1.0/( tau+sqrt(1.0+tau*tau));
		  else
		     t=-1.0/(-tau+sqrt(1.0+tau*tau));
		  c=1.0/sqrt(1.0+t*t); 
		  s=t*c;
	       }
	       // compute eigenvalues and take their modulus
	       // [lambdai     0   ]                       [ c   s]
	       // [                ] = J'* A * J, where J= [      ]
	       // [   0     lambdaj]		           [-s   c]
	       lambdai=c*c*aii+s*s*ajj-2*c*s*aji;
	       lambdaj=s*s*aii+c*c*ajj+2*c*s*aji;
	       if (lambdai<-nrmdiag)
		  factspd++;
	       if (lambdaj<-nrmdiag)
		  factspd++;

	       /*
	       G[0]=G[1]=G[2]=G[3]=0.0;
	       if (lambdai<0.0) {
		  if (lambdaj<0.0) {
		     //     [ c   s] [-1   0] [ c  -s]  
		     // G = [      ]	[      ] [      ]
		     //     [-s   c]	[ 0  -1] [ s   c]
		     G[0]=G[3]=-1.0;
		  }
		  else {
		     //     [ c   s] [-1   0] [ c  -s]  
		     // G = [      ]	[      ] [      ]
		     //     [-s   c]	[ 0   1] [ s   c]
		     G[0]=-c*c+s*s;  G[2]= 2*c*s;
		     G[1]= 2*c*s;    G[3]=-s*s+c*c;
		  }
	       }
	       else
		  if (lambdaj<0.0) {
		     //     [ c   s] [ 1   0] [ c  -s]  
		     // G = [      ]	[      ] [      ]
		     //     [-s   c]	[ 0  -1] [ s   c]
		     G[0]= c*c-s*s;  G[2]=-2*c*s;
		     G[1]=-2*c*s;    G[3]= s*s-c*c;
		  }
		  else
		     G[0]=G[3]=1.0;
	       
	       j=next->LU.ja[i];
	       k=next->LU.ja[i+1]-j;
	       p=next->LU.a+j-1;
	       for (l=0; l<k; l++) {
		   buff=G[0]*p[0]+G[2]*p[1];
		   p[1]=G[1]*p[0]+G[3]*p[1];
		   p[0]=buff;
		   p+=2;
	       }	  
	       */

	       // recompute |A|
	       //          [|lambdai|     0    ]     
	       // |A|= J * [                   ] * J'
	       //          [    0     |lambdaj|]	   
	       lambdai=FRABS(lambdai);
	       lambdaj=FRABS(lambdaj);
	       next->absdiag[i]     = c*c*lambdai+s*s*lambdaj;
	       next->absdiag[nB+1+i]=-s*c*lambdai+c*s*lambdaj;
	       next->absdiag[i+1]   = s*s*lambdai+c*c*lambdaj;
#ifdef PRINT_INFO
	       printf("new: %12.4le%12.4le\n",  next->absdiag[i],     next->absdiag[nB+1+i]);
	       printf("     %12.4le%12.4le\n\n",next->absdiag[nB+1+i],next->absdiag[i+1]);
	       /*
	       j=next->LU.ja[i];
	       k=next->LU.ja[i+1]-j;
	       for (l=0; l<k; l++) 
		 printf("%12d",next->LU.ja[j-1+l]);
	       printf("\n");
	       for (l=0; l<k; l++) 
		 printf("%12.4le",next->LU.a[j-1+2*l]);
	       printf("\n");
	       for (l=0; l<k; l++) 
		 printf("%12.4le",next->LU.a[j-1+2*l+1]);
	       printf("\n\n");
	       */
#endif
#else
#ifdef PRINT_INFO
	       printf("old: %12.4le+%12.4lei%12.4le+%12.4lei\n", 
		      next->LU.a[i].r,     next->LU.a[i].i,
		      next->LU.a[nB+1+i].r,next->LU.a[nB+1+i].i);
	       printf("     %12.4le+%12.4lei%12.4le+%12.4lei\n", 
		      next->LU.a[nB+1+i].r,next->LU.a[nB+1+i].i,
		      next->LU.a[i+1].r,   next->LU.a[i+1].i);
#endif
	       aii=next->LU.a[i].r;
	       aji=next->LU.a[nB+1+i];
	       ajj=next->LU.a[i+1].r;
	       
	       // compute Jacobi rotation that diagonalize
	       //    [aii aji']
	       // A= [        ]
	       //    [aji ajj ]
	       
	       // numerical almost diagonal matrix
	       nrm=FABS(aji);
	       if (nrm<=1e-8*(FRABS(ajj-aii))) {
		  c=1.0;
		  s=0.0;
		  sigma.r=1.0;
		  sigma.i=0.0;
	       }
	       else {
		  // sign of aji. Note that sigma*aji=nrm=|aji|
		  sigma.r= aji.r/nrm;
		  sigma.i=-aji.i/nrm;
		  tau=(ajj-aii)/(2*nrm);
		  if (tau>=0.0)
		     t= 1.0/( tau+sqrt(1.0+tau*tau));
		  else
		     t=-1.0/(-tau+sqrt(1.0+tau*tau));
		  c=1.0/sqrt(1.0+t*t); 
		  s=t*c;
	       }
	       // compute eigenvalues and take their modulus
	       // [lambdai     0   ]                       [   c     sigma*s]
	       // [                ] = J'* A * J, where J= [                ]
	       // [   0     lambdaj]			  [-sigma'*s   c   ]
	       lambdai=c*c*aii+s*s*ajj-2*c*s*nrm;
	       lambdaj=s*s*aii+c*c*ajj+2*c*s*nrm;
	       if (lambdai<-nrmdiag)
		  factspd++;
	       if (lambdaj<-nrmdiag)
		  factspd++;

	       // recompute |A|
	       //          [|lambdai|     0    ]     
	       // |A|= J * [                   ] * J'
	       //          [    0     |lambdaj|]	   
	       lambdai=FRABS(lambdai);
	       lambdaj=FRABS(lambdaj);
	       next->absdiag[i].r     = c*c*lambdai+s*s*lambdaj;
	       next->absdiag[i].i     = 0;
	       next->absdiag[nB+1+i].r= sigma.r*(-s*c*lambdai+c*s*lambdaj);
	       next->absdiag[nB+1+i].i=-sigma.i*(-s*c*lambdai+c*s*lambdaj);
	       next->absdiag[i+1].r   = s*s*lambdai+c*c*lambdaj;
	       next->absdiag[i+1].i   = 0;
#ifdef PRINT_INFO
	       printf("new: %12.4le+%12.4lei%12.4le+%12.4lei\n", 
		      next->absdiag[i].r,     next->absdiag[i].i,
		      next->absdiag[nB+1+i].r,next->absdiag[nB+1+i].i);
	       printf("     %12.4le+%12.4lei%12.4le+%12.4lei\n\n", 
		      next->absdiag[nB+1+i].r,next->absdiag[nB+1+i].i,
		      next->absdiag[i+1].r,   next->absdiag[i+1].i);
#endif
#endif
	       i+=2;
	    }
      } // end while
      // flag first index negative to indicate the SPD conversion
      next->LU.ja[0]=-next->LU.ja[0];
   } // end if-else (next->LU.ja==NULL)

   return (factspd);
} /* end symspd */


