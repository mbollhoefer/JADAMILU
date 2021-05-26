#include <stdlib.h>
#include <stdio.h>

#include <blas.h>
#include <lapack.h>
#include <ilupack.h>
#include <ilupackmacros.h>

#define MAX_LINE        255
#define STDERR          stderr
#define STDOUT          stdout
//#define PRINT_INFO

#define IABS(A)         (((A)>=0)?(A):-(A))
#define MAX(A,B)        (((A)>(B))?(A):(B))
#define MIN(A,B)        (((A)<(B))?(A):(B))


#if defined _DOUBLE_REAL_ || defined _SINGLE_REAL_
#define CONJG(A)      (A)
#define MYCSRMATVEC          CSRMATVEC 
#define MYCSRMATTVEC         CSRMATTVEC 

#ifdef _DOUBLE_REAL_
#define MYSYMAMGSOL          DSYMAMGsol
#define MYSYMPILUCSOL        DSYMpilucsol
#define MYSYMPILUCLSOL       DSYMpiluclsol
#define MYSYMPILUCUSOL       DSYMpilucusol
#define MYPRIVATESPTRS       dprivatesptrs
#define MYSPTRS              dsptrs
#else			   
#define MYSYMAMGSOL          SSYMAMGsol
#define MYSYMPILUCSOL        SSYMpilucsol
#define MYSYMPILUCLSOL       SSYMpiluclsol
#define MYSYMPILUCUSOL       SSYMpilucusol
#define MYPRIVATESPTRS       sprivatesptrs
#define MYSPTRS              ssptrs
#endif



#else



#ifdef _COMPLEX_SYMMETRIC_
#define CONJG(A)     (A)
#define MYCSRMATVEC          CSRMATVEC 
#define MYCSRMATTVEC         CSRMATTVEC 

#ifdef _SINGLE_COMPLEX_
#define MYSYMAMGSOL          CSYMAMGsol
#define MYSYMPILUCSOL        CSYMpilucsol
#define MYSYMPILUCLSOL       CSYMpiluclsol
#define MYSYMPILUCUSOL       CSYMpilucusol
#define MYSPTRS              csptrs
#else
#define MYSYMAMGSOL          ZSYMAMGsol
#define MYSYMPILUCSOL        ZSYMpilucsol
#define MYSYMPILUCLSOL       ZSYMpiluclsol
#define MYSYMPILUCUSOL       ZSYMpilucusol
#define MYSPTRS              zsptrs
#endif

#else
#define MYCSRMATVEC          CSRMATVEC 
#define MYCSRMATTVEC         CSRMATHVEC 

#ifdef _SINGLE_COMPLEX_
#define CONJG(A)     (conjg(A))
#define MYSYMAMGSOL          CHERAMGsol
#define MYSYMPILUCSOL        CHERpilucsol
#define MYSYMPILUCLSOL       CHERpiluclsol
#define MYSYMPILUCUSOL       CHERpilucusol
#define MYPRIVATESPTRS       cprivatehptrs
#define MYSPTRS              chptrs
#else
#define CONJG(A)     (dconjg(A))
#define MYSYMAMGSOL          ZHERAMGsol
#define MYSYMPILUCSOL        ZHERpilucsol
#define MYSYMPILUCLSOL       ZHERpiluclsol
#define MYSYMPILUCUSOL       ZHERpilucusol
#define MYPRIVATESPTRS       zprivatehptrs
#define MYSPTRS              zhptrs
#endif

#endif



#endif



#define MAX_LINE        255
#define STDERR          stderr
#define STDOUT          stdout
#define MAX(A,B)        (((A)>(B))?(A):(B))
#define MIN(A,B)        (((A)<(B))?(A):(B))






/*
   Given a multilevel ILU decomposition performed by SYMAMGsetup, 
   SYMAMGsol performs a simple forward backward substitution
*/ 
void MYSYMAMGSOL(AMGLEVELMAT PRE, integer nlev, 
		 ILUPACKPARAM *IPparam,
		 FLOAT *rhs, FLOAT *sol, FLOAT *buff)

{

/*
   PRE       linked list of multilevel ILU preconditioners
   nlev      number of levels (blocks)
   rhs       given right hand side
   sol       solution obtained by SYMAMGsol
   buff      work array of length 2n, where n is the size of the original 
             matrix
  
   Code written by Matthias Bollhoefer, February 2005
*/

   AMGLEVELMAT *next, *prev;

   integer i, j, k,n=PRE.n, nB, *p, *invq, sumn=0, lev,buffldl,buffldl2,ierr, flag,
           globalflag=0;
   

   FLOAT *scal, *a;
   REALS val, vali;
#ifdef PRINT_INFO
   static integer check=1;

   if (check) {
     check=0;
#ifdef _SINGLE_REAL_ 
     printf("SSYM solve\n");
#else
#ifdef _DOUBLE_REAL_
     printf("DSYM solve\n");
#else
#ifdef _SINGLE_COMPLEX_
#ifdef _COMPLEX_SYMMETRIC_
     printf("CSYM solve\n");
#else
     printf("CHER solve\n");
#endif
#else
#ifdef _COMPLEX_SYMMETRIC_
     printf("ZSYM solve\n");
#else
     printf("ZHER solve\n");
#endif
#endif
#endif
#endif
   }
#endif

   buff+=n;
   for (i=0; i<n; i++)
       buff[i]=rhs[i];
   buff-=n;
   /* block forward substitution */
   next=&PRE;
   prev=NULL;
   lev=1;
   
   while (lev<nlev) {
	 nB=next->nB;
	 p=next->p;

#ifdef PRINT_INFO
	 printf("%3d\n",lev);fflush(stdout);
#endif
	 if (lev>1) {
	    scal=next->colscal-sumn;
	    buff+=n;
	    for (i=sumn; i<n; i++) {
#if defined _DOUBLE_REAL_ || defined _SINGLE_REAL_
	        buff[i]*=scal[i];
#else
		val=buff[i].r;
	        buff[i].r= val*scal[i].r+buff[i].i*scal[i].i;
	        buff[i].i=-val*scal[i].i+buff[i].i*scal[i].r;
#endif
	    }
	    buff-=n;
	 } /* end if */

	 /* reorder the whole system */
	 p-=sumn;
	 for (i=sumn; i<n; i++)
	     buff[i]=buff[n+sumn+p[i]-1];
	 p+=sumn;
	 
	 /* first block */
	 if (next->LU.ja[0]<0) {
	    flag=-1;
	    globalflag=-1;
	 }
	 else
	   flag=0;
	 next->LU.ja[0]=IABS(next->LU.ja[0]);
	 if (flag) {
	    // printf("1.\n");fflush(stdout);
	    // solve system with [(L+D)D^{-1}]
	    MYSYMPILUCLSOL(&nB, buff+sumn,buff+n+sumn, next->LU.a,next->LU.ja);
	    // printf("2.\n");fflush(stdout);
	 }
	 else
	    MYSYMPILUCSOL(&nB, buff+sumn,buff+n+sumn, next->LU.a,next->LU.ja);

	 if (flag) {
	    sol +=sumn;
	    buff+=n+sumn;
	    // multiply by D^{-1}
	    i=0;
	    a=next->LU.a;
	    // printf("3.\n");fflush(stdout);
	    while (i<nB) {
	          if (next->LU.ja[nB+1+i]==0) {
#if defined _DOUBLE_REAL_ || defined _SINGLE_REAL_
		     sol[i]=buff[i]*a[i];
#else
		     sol[i].r=buff[i].r*a[i].r-buff[i].i*a[i].i;
		     sol[i].i=buff[i].r*a[i].i+buff[i].i*a[i].r;
#endif
		     i++;
		  }
		  else {
#if defined _DOUBLE_REAL_ || defined _SINGLE_REAL_
		     sol[i]  =a[i]            *buff[i]+a[nB+1+i]*buff[i+1];
		     sol[i+1]=a[nB+1+i]       *buff[i]+a[i+1]   *buff[i+1];
#else
		     sol[i].r  =a[i].r     *buff[i].r  -a[i].i     *buff[i].i
		               +a[nB+1+i].r*buff[i+1].r-a[nB+1+i].i*buff[i+1].i;
		     sol[i].i  =a[i].r     *buff[i].i  +a[i].i     *buff[i].r
		               +a[nB+1+i].r*buff[i+1].i+a[nB+1+i].i*buff[i+1].r;
#ifdef _COMPLEX_SYMMETRIC_		     
		     sol[i+1].r=a[nB+1+i].r*buff[i].r  -a[nB+1+i].i*buff[i].i
		               +a[i+1].r   *buff[i+1].r-a[i+1].i   *buff[i+1].i;
		     sol[i+1].i=a[nB+1+i].r*buff[i].i  +a[nB+1+i].i*buff[i].r
		               +a[i+1].r   *buff[i+1].i+a[i+1].i   *buff[i+1].r;
#else
		     sol[i+1].r=a[nB+1+i].r*buff[i].r  +a[nB+1+i].i*buff[i].i
		               +a[i+1].r   *buff[i+1].r-a[i+1].i   *buff[i+1].i;
		     sol[i+1].i=a[nB+1+i].r*buff[i].i  -a[nB+1+i].i*buff[i].r
		               +a[i+1].r   *buff[i+1].i+a[i+1].i   *buff[i+1].r;
#endif
#endif
		     i+=2;
		  }
	    } // end while
	    // multiply by |D|^{-1}
	    i=0;
	    a=next->absdiag;
	    // printf("4.\n");fflush(stdout);
	    while (i<nB) {
	          if (next->LU.ja[nB+1+i]==0) {
#if defined _DOUBLE_REAL_ || defined _SINGLE_REAL_
		     buff[i]*=a[i];
#else
		     val      =buff[i].r*a[i].r-buff[i].i*a[i].i;
		     buff[i].i=buff[i].r*a[i].i+buff[i].i*a[i].r;
		     buff[i].r=val;
#endif
		     i++;
		  }
		  else {
#if defined _DOUBLE_REAL_ || defined _SINGLE_REAL_
		     val      =a[i]            *buff[i]+a[nB+1+i]*buff[i+1];
		     buff[i+1]=a[nB+1+i]       *buff[i]+a[i+1]   *buff[i+1];
		     buff[i]  =val;
#else
		     val        =a[i].r     *buff[i].r  -a[i].i     *buff[i].i
		                +a[nB+1+i].r*buff[i+1].r-a[nB+1+i].i*buff[i+1].i;
		     buff[i].i  =a[i].r     *buff[i].i  +a[i].i     *buff[i].r
		                +a[nB+1+i].r*buff[i+1].i+a[nB+1+i].i*buff[i+1].r;
#ifdef _COMPLEX_SYMMETRIC_		     
		     vali       =a[nB+1+i].r*buff[i].r  -a[nB+1+i].i*buff[i].i
		                +a[i+1].r   *buff[i+1].r-a[i+1].i   *buff[i+1].i;
		     buff[i+1].i=a[nB+1+i].r*buff[i].i  +a[nB+1+i].i*buff[i].r
		                +a[i+1].r   *buff[i+1].i+a[i+1].i   *buff[i+1].r;
#else
		     vali       =a[nB+1+i].r*buff[i].r  +a[nB+1+i].i*buff[i].i
		                +a[i+1].r   *buff[i+1].r-a[i+1].i   *buff[i+1].i;
		     buff[i+1].i=a[nB+1+i].r*buff[i].i  -a[nB+1+i].i*buff[i].r
		                +a[i+1].r   *buff[i+1].i+a[i+1].i   *buff[i+1].r;
#endif
		     buff[i+1].r=vali;
		     buff[i].r  =val;
#endif
		     i+=2;
		  }
	    } // end while
	    // printf("5.\n");fflush(stdout);
	    // solve system with [D^{-1}(D+U)]
	    MYSYMPILUCUSOL(&nB, sol,sol, next->LU.a,next->LU.ja);
	    // printf("6.\n");fflush(stdout);
	    MYCSRMATTVEC(next->F,sol,sol+nB);
	    // printf("7.\n");fflush(stdout);
	    next->LU.ja[0]=-IABS(next->LU.ja[0]);
	    sol -=sumn;
	    buff-=n+sumn;
	 }
	 else
	    /* second block, multiply by F^H */
	    MYCSRMATTVEC(next->F,buff+n+sumn,sol+sumn+nB);

	 sumn+=nB;

	 /* coarse grid projection */
	 for (i=sumn; i<n; i++) {
#if defined _DOUBLE_REAL_ || defined _SINGLE_REAL_
	     buff[n+i]=buff[i]-sol[i];
#else
	     buff[n+i].r=buff[i].r-sol[i].r;
	     buff[n+i].i=buff[i].i-sol[i].i;
#endif
	 } // end for i
	 prev=next;
	 next=next->next;
	 lev++;
   } // end while




   // last level
   nB=next->nB;
   p=next->p;
   invq=next->invq;

#ifdef PRINT_INFO
   printf("%3d\n",lev);fflush(stdout);
#endif




   /* -----   last block   ----- */

   /* did we finally switch to full matrix processing? */
   if (lev>1 && next->LU.ja==NULL) {
#ifdef PRINT_INFO
      printf("dense U-factor\n");fflush(STDOUT);
      for (i=0; i<nB; i++)
	  printf("%12d",next->LU.ia[i]);
      printf("\n");fflush(stdout);
      j=0;
      for (k=0; k<nB; k++) {
	  for (i=k; i<nB; i++) {
	      printf("%12.4le",next->LU.a[j++]);
	  }
	  printf("\n");fflush(stdout);
      }
      printf("\n");fflush(stdout);
#endif

      i=1;
      COPY(&nB, buff+n+sumn,&i, sol+sumn,&i);
#ifdef PRINT_INFO
      printf("rhs\n");fflush(STDOUT);
      for (i=0; i<nB; i++)
	  printf("%12d",sumn+i+1);
      printf("\n");fflush(stdout);
      for (i=0; i<nB; i++)
	  printf("%12.4le",sol[sumn+i]);
      printf("\n");fflush(stdout);
      i=1;
#endif
      if (globalflag)
	 MYPRIVATESPTRS("L", &nB, &i, next->LU.a,next->LU.ia,
			sol+sumn, &nB, &ierr,1);     
      else
	 MYSPTRS("L", &nB, &i, next->LU.a,next->LU.ia,
		 sol+sumn, &nB, &ierr,1);     
#ifdef PRINT_INFO
      printf("sol, ierr=%d\n",ierr);fflush(STDOUT);
      for (i=0; i<nB; i++)
	  printf("%12.4le",sol[sumn+i]);
      printf("\n");fflush(stdout);
      i=1;
#endif
   }
   else {
      if (lev>1) {
	 scal=next->colscal-sumn;
	 buff+=n;
	 for (i=sumn; i<n; i++) {
#if defined _DOUBLE_REAL_ || defined _SINGLE_REAL_
	   buff[i]*=scal[i];
#else
	   val=buff[i].r;
	   buff[i].r= val*scal[i].r+buff[i].i*scal[i].i;
	   buff[i].i=-val*scal[i].i+buff[i].i*scal[i].r;
#endif
	 } // end for i
	 buff-=n;
      } /* end if */

      /* reorder the whole system */
      p-=sumn;
      for (i=sumn; i<n; i++)
	  buff[i]=buff[n+sumn+p[i]-1];
      p+=sumn;
     

      if (next->LU.ja[0]<0) {
	 flag=-1;
	 globalflag=-1;
      }
      else
	 flag=0;
      next->LU.ja[0]=IABS(next->LU.ja[0]);
      if (flag) {
	 buff+=sumn;
	 // printf("8.\n");fflush(stdout);
	 MYSYMPILUCLSOL(&nB, buff,buff+n, next->LU.a,next->LU.ja); 
	 i=0;
	 a=next->absdiag;
	 buff+=n;
	 // printf("9.\n");fflush(stdout);
	 while (i<nB) {
	       // printf("%3d\n",i);fflush(stdout);
	       if (next->LU.ja[nB+1+i]==0) {
#if defined _DOUBLE_REAL_ || defined _SINGLE_REAL_
		  // printf("%12.4le\n",buff[i]);fflush(stdout);
		  // printf("%12.4le\n",a[i]);fflush(stdout);
		  buff[i]*=a[i];
#else
		  val      =buff[i].r*a[i].r-buff[i].i*a[i].i;
		  buff[i].i=buff[i].r*a[i].i+buff[i].i*a[i].r;
		  buff[i].r=val;
#endif
		  i++;
	       }
	       else {
#if defined _DOUBLE_REAL_ || defined _SINGLE_REAL_
		  val      =a[i]            *buff[i]+a[nB+1+i]*buff[i+1];
		  buff[i+1]=a[nB+1+i]       *buff[i]+a[i+1]   *buff[i+1];
		  buff[i]  =val;
#else
		  val        =a[i].r     *buff[i].r  -a[i].i     *buff[i].i
		             +a[nB+1+i].r*buff[i+1].r-a[nB+1+i].i*buff[i+1].i;
		  buff[i].i  =a[i].r     *buff[i].i  +a[i].i     *buff[i].r
		             +a[nB+1+i].r*buff[i+1].i+a[nB+1+i].i*buff[i+1].r;
#ifdef _COMPLEX_SYMMETRIC_		     
		  vali       =a[nB+1+i].r*buff[i].r  -a[nB+1+i].i*buff[i].i
		             +a[i+1].r   *buff[i+1].r-a[i+1].i   *buff[i+1].i;
		  buff[i+1].i=a[nB+1+i].r*buff[i].i  +a[nB+1+i].i*buff[i].r
		             +a[i+1].r   *buff[i+1].i+a[i+1].i   *buff[i+1].r;
#else
		  vali       =a[nB+1+i].r*buff[i].r  +a[nB+1+i].i*buff[i].i
		             +a[i+1].r   *buff[i+1].r-a[i+1].i   *buff[i+1].i;
		  buff[i+1].i=a[nB+1+i].r*buff[i].i  -a[nB+1+i].i*buff[i].r
		             +a[i+1].r   *buff[i+1].i+a[i+1].i   *buff[i+1].r;
#endif
		  buff[i+1].r=vali;
		  buff[i].r  =val;
#endif
		  i+=2;
	       }
	 } // end while
	 // printf("10.\n");fflush(stdout);
	 MYSYMPILUCUSOL(&nB, buff,buff, next->LU.a,next->LU.ja); 
	 // printf("11.\n");fflush(stdout);
	 buff-=n+sumn;
      }
      else
	 MYSYMPILUCSOL(&nB, buff+sumn,buff+n+sumn, next->LU.a,next->LU.ja); 
      if (flag)
	 next->LU.ja[0]=-IABS(next->LU.ja[0]);


      /* reorder the whole system back */
      buff+=n+sumn-1;
      invq-=sumn;
      for (i=sumn; i<n; i++) 
	sol[i]=buff[invq[i]];
      invq+=sumn;
      buff-=n+sumn-1;
	
      if (lev>1) {
	 scal=next->colscal-sumn;
	 for (i=sumn; i<n; i++) {
#if defined _DOUBLE_REAL_ || defined _SINGLE_REAL_
	     sol[i]*=scal[i];
#else
	     val=sol[i].r;
	     sol[i].r=val*scal[i].r-sol[i].i*scal[i].i;
	     sol[i].i=val*scal[i].i+sol[i].i*scal[i].r;
#endif
	 } // end for i
      } /* end if */
      
   } // end if-else (next->LU.ja==NULL)

      

   /* block backward substitution */
   lev=nlev-1;
   while (lev>0) {
	 nB=prev->nB;
	 invq=prev->invq;

#ifdef PRINT_INFO
	 printf("%3d\n",lev);fflush(stdout);
#endif
	 
	 /* copy r.h.s */
	 for (i=sumn; i<n; i++)
	     buff[i]=sol[i];

	 /* second block */
	 MYCSRMATVEC(prev->F,sol+sumn,buff+sumn-nB);

	 /* first block */
	 sumn-=nB;


	 if (prev->LU.ja[0]<0) {
	    flag=-1;
	    globalflag=-1;
	 }
	 else
	    flag=0;
	 prev->LU.ja[0]=IABS(prev->LU.ja[0]);
	 if (flag) {
	    sol +=sumn;
	    buff+=sumn;
	    // printf("12.\n");fflush(stdout);
	    MYSYMPILUCLSOL(&nB, buff,buff, prev->LU.a,prev->LU.ja);
	    // printf("13.\n");fflush(stdout);
	    i=0;
	    a=prev->LU.a;
	    while (i<nB) {
	          if (prev->LU.ja[nB+1+i]==0) {
#if defined _DOUBLE_REAL_ || defined _SINGLE_REAL_
		     sol[i]=buff[i]*a[i];
#else
		     sol[i].r=buff[i].r*a[i].r-buff[i].i*a[i].i;
		     sol[i].i=buff[i].r*a[i].i+buff[i].i*a[i].r;
#endif
		     i++;
		  }
		  else {
#if defined _DOUBLE_REAL_ || defined _SINGLE_REAL_
		     sol[i]  =a[i]            *buff[i]+a[nB+1+i]*buff[i+1];
		     sol[i+1]=a[nB+1+i]       *buff[i]+a[i+1]   *buff[i+1];
#else
		     sol[i].r  =a[i].r     *buff[i].r  -a[i].i     *buff[i].i
		               +a[nB+1+i].r*buff[i+1].r-a[nB+1+i].i*buff[i+1].i;
		     sol[i].i  =a[i].r     *buff[i].i  +a[i].i     *buff[i].r
		               +a[nB+1+i].r*buff[i+1].i+a[nB+1+i].i*buff[i+1].r;
#ifdef _COMPLEX_SYMMETRIC_		     
		     sol[i+1].r=a[nB+1+i].r*buff[i].r  -a[nB+1+i].i*buff[i].i
		               +a[i+1].r   *buff[i+1].r-a[i+1].i   *buff[i+1].i;
		     sol[i+1].i=a[nB+1+i].r*buff[i].i  +a[nB+1+i].i*buff[i].r
		               +a[i+1].r   *buff[i+1].i+a[i+1].i   *buff[i+1].r;
#else
		     sol[i+1].r=a[nB+1+i].r*buff[i].r  +a[nB+1+i].i*buff[i].i
		               +a[i+1].r   *buff[i+1].r-a[i+1].i   *buff[i+1].i;
		     sol[i+1].i=a[nB+1+i].r*buff[i].i  -a[nB+1+i].i*buff[i].r
		               +a[i+1].r   *buff[i+1].i+a[i+1].i   *buff[i+1].r;
#endif
#endif
		     i+=2;
		  }
	    } // end while
	    // printf("14.\n");fflush(stdout);
	    sol -=sumn;
	    buff-=sumn;
	 }
	 else
	    MYSYMPILUCSOL(&nB, buff+sumn,sol+sumn, prev->LU.a,prev->LU.ja);

	 /* update solution */ 
	 buff+=sumn;
	 sol+=sumn;
	 for (i=0; i<nB; i++) {
#if defined _DOUBLE_REAL_ || defined _SINGLE_REAL_
	     buff[i]=buff[n+i]-sol[i];
#else
	     buff[i].r=buff[n+i].r-sol[i].r;
	     buff[i].i=buff[n+i].i-sol[i].i;
#endif
	 } // end for i
	 if (flag) {
	    // printf("15.\n");fflush(stdout);
	    MYSYMPILUCUSOL(&nB, buff,buff, prev->LU.a,prev->LU.ja);
	    // printf("16.\n");fflush(stdout);
	    prev->LU.ja[0]=-IABS(prev->LU.ja[0]);
	 }
	 sol-=sumn;
	 buff-=sumn;

	 /* reorder the whole system */
	 buff+=sumn-1;
	 invq-=sumn;
	 for (i=sumn; i<n; i++)
	     sol[i]=buff[invq[i]];
	 invq+=sumn;
	 buff-=sumn-1;

	 if (lev>1) {
	    scal=prev->colscal-sumn;
	    for (i=sumn; i<n; i++) {
#if defined _DOUBLE_REAL_ || defined _SINGLE_REAL_
	        sol[i]*=scal[i];
#else
		val=sol[i].r;
		sol[i].r=val*scal[i].r-sol[i].i*scal[i].i;
		sol[i].i=val*scal[i].i+sol[i].i*scal[i].r;
#endif
	    } // end for i
	 } /* end if */

	 
	 next=prev;
	 prev=prev->prev;
	 lev--;
   } // end while


} /* end SYMAMGsol */


