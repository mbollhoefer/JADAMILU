#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

#include <blas.h>
#include <ilupack.h>

#include <ilupackmacros.h>

#define MAX_LINE        255
#define STDERR          stdout
#define STDOUT          stdout
#define PRINT_INFO
#define MAX(A,B)        (((A)>(B))?(A):(B))
#define MIN(A,B)        (((A)<(B))?(A):(B))
#define RESTOL_FUNC(A)  sqrt(A)











void SYMAMGINIT(CSRMAT A, ILUPACKPARAM *param)
{
  /*
    A           symmetric(Hermitian) sparse matrix in compressed row storage

    param       data structure containing several parameters
                param.ipar[40] miscellaneous integer parameters
                param.fpar[40] miscellaneous real  parameters
                               
    ipar        integer parameters ipar[0,...,33]
                ipar[0]     elbow space factor. Predict the fill-in required
                            by the multilevel ILU by
                            ipar[0]*nnz(A)
                            default: 5
                ipar[1]     
                            
                            
                            
                            
		            
                ipar[2]     
                            
                            
                            
                            
		            
                ipar[3]     lfil parameter. Limit the number of nonzeros per
                            column in L / row in U by ipar[3]
                            default: n+1
                ipar[4]     flag that indicates different types of transposed
                            systems
                            Bit 0:  A^T      instead of A is stored  
                            Bit 1:  CONJG(A) instead of A is stored

                            Bit 2:  solve A^Tx=b      instead of Ax=b  
                            Bit 3:  solve CONJG(A)x=b instead of Ax=b  

                            REMARK: Note that P is derived from A. So if A^T is
			    stored instead of A, it follows that P^T is stored
			    instead of P. Similar relations hold for CONJG(A)

                ipar[5]     reserved for choice of iterative solver 
		            currently unused
			    default: 1

		ipar[6]     flags for the configuration of the multilevel ILU
                            available flags
			    inverse based dropping:                   DROP_INVERSE
			    don't shift away zero pivots(iluc):       NO_SHIFT
			    Tismentsky update:                        TISMENETSKY_SC
			    repeated ILU(iluc):                       REPEAT_FACT
			    improved estimate ||L^{-1}||,||U^{-1}||:  IMPROVED_ESTIMATE
			    diagonal compensation:                    DIAGONAL_COMPENSATION
			    reduce the partial LU to non-coarse
			    part(piluc,mpiluc):                       COARSE_REDUCE
			    use different pivoting strategy, if 
			    the regular reordering fails:             FINAL_PIVOTING
			    initial system should be reordered
			    using an initial strategy:                PREPROCESS_INITIAL_SYSTEM
			    subsequent systems should be reordered
			    using the regular strategy:               PREPROCESS_SUBSYSTEMS
			    use mpiluc as template instead of piluc:  MULTI_PILUC
			    
			    default:  DROP_INVERSE
                                     |PREPROCESS_INITIAL_SYSTEM
				     |PREPROCESS_SUBSYSTEMS
				     |IMPROVED_ESTIMATE
				     |FINAL_PIVOTING
				     |SIMPLE_SC
				     |AGGRESSIVE_DROPPING
				     |DISCARD_MATRIX

		ipar[7]     decide which kind of scaling should be combined
                            with the three permutations
			    Bit 0-2 initial preprocessing
                            Bit 0
                                     (no)  left scaling        0/  1
                            Bit 1
                                     (no)  right scaling       0/  2
                            Bit 2    row/column scaling first  0/  4
			    default: 3

			    Bit 3-5 regular reordering
                            Bit 3
                                     (no)  left scaling        0/  8
                            Bit 4
                                     (no)  right scaling       0/ 16
                            Bit 5    row/column scaling first  0/ 32
			    default: 24

			    Bit 6-8 final pivoting
                            Bit 6
                                     (no)  left scaling        0/ 64
                            Bit 7
                                     (no)  right scaling       0/128
                            Bit 8    row/column scaling first  0/256
			    default: 192

                            Bit 9-10 indicate which ordering is currently
                                     in use
                             512     initial preprocessing
                            1024     regular reordering
                            1536     final pivoting
			    default: 0 (no preference)
			    sum default: 3+24+192


		ipar[8]     decide which kind of norm should be used
                            with the three permutations
			    Bit  0- 4 initial preprocessing
                            0         maximum norm
                            1         1-norm
                            2         2-norm 
                            3         square root of the diagonal entries
                            4         symmetric scaling that keeps the entries
			              below one in absolute value
			    default: 4

			    Bit  5- 9 regular reordering
                            0         maximum norm
                            32*1      1-norm
                            32*2      2-norm 
                            32*3      square root of the diagonal entries
                            32*4      symmetric scaling that keeps the entries
			              below one in absolute value
			    default: 32*4

			    Bit 10-14 final pivoting
                            0         maximum norm
                            1024*1    1-norm
                            1024*2    2-norm 
                            1024*3    square root of the diagonal entries
                            1024*4    symmetric scaling that keeps the entries
			              below one in absolute value
			    default: 1024*4
			    sum default: 4+32*4+1024*4

                ipar[9]     lfil parameter for the approximate Schur 
		            complement. Limit the number of nonzeros per
                            row in S by ipar[9]
                            default: n+1

		ipar[10]    
		ipar[11]    
		ipar[12]    

		ipar[13,...,19] currently unused

                ipar[20,...33] parameters used by SPARSKIT solvers
	        ipar[20]    needed to init the iterative solver
		            default: 0
		ipar[21]    preconditioning side (left 1, right 2, both 3)
		            default: 2
		ipar[22]    stopping criteria 
		                     1 (relative residual)
		                     2 (relative ||b||)
			    default: 3 (relative error in the energy norm)
		ipar[23]    workspace size requested by SPARSKIT solver
		            dependent on ipar[5]. Currently only GMRES is
                            supported.
			    default: (n+3)*(ipar[4]+2)+(ipar[4]+1)*ipar[4]/2;

  	        ipar[24]    restart length for GMRES,FOM,... 
		            default: 30
		ipar[25]    maximum number of iteration steps 
		            default: MIN(1.1*n+10,500)
		ipar[26,...33] further parameters used by SPARSKIT solvers
		            default: 0


    fpar        floating point parameters 
                fpar[0]     drop tolerance for triangular factors
                            default:  1e-2
                fpar[1]     drop tolerance for the approximate Schur complement
                            default:  1e-2
                fpar[2]     norm bound for the inverse triangular factors
                            default:  1e+2
                fpar[3]     define when a matrix is dense enough to switch
                            to full matrix processing 
                            fpar[3]*nnz(S) > n^2 
                            default:  3.0
                fpar[4]     Define the minimum block size which allows the
                            the multilevel ILU to continue with the default 
                            reordering strategy (including scaling and 
                            pivoting) before switching to a different strategy
                            if a matrix A of size "n" should be factored at the
                            current level, then piluc yiels up to a permutation
                                 / B  F \
                            A -> |      | such that B is factored
                                 \ E  C /
                            We continue the multilevel factorization if
                            n-nB<=n*fpar[4]
			    default: 0.75
                fpar[5]     Provided that a final static pivoting strategy
                            is requested, we may encounter the situation when
                            the regular reodering strategy becomes ineffective,
                            i.e.
                                 / B  F \
                            A -> |      | such that B is factored
                                 \ E  C /
                            with B of tiny size (n-nB > n*fpar[4]). In this
                            situation we may switch to a different final 
                            pivoting strategy. Like the regular reordering
                            strategy, we may also encounter a situation when
                            the final reordering strategy fails due to
                            a small block B
                            We continue the multilevel factorization with the
                            final pivoting strategy as long as
                            n-nB<=n*fpar[5]
			    default: 0.75
                fpar[6]     ONLY used if MULTI_PILUC is requested. Multiple,
                            repeated use of piluc without switching to a new 
                            level is continued as long as
                            "number of entries skipped" < 
                            "number of entries skipped previously" * fpar[6]
			    default: 0.75

		fpar[7]     drop tolerance for SYMILUC, used relative to 
                            fpar[1].	    
			    default: fpar[1]

                fpar[8]     drop tolerance for the remaining Schur complement
                            default: 1e-4 (squared drop tolerance) 
			    
                fpar[9,...,19] currently unused

                fpar[20,...,30] floating parameters required for SPARSKIT 
		            solvers
		fpar[20]    relative error tolerance
		            default: sqrt(MACHEPS)
		fpar[21]    absolute error tolerance
		            default: 0
			    fpar[22]=
		ipar[23,...30] further parameters used by SPARSKIT solvers
		            default: 0

     written by Matthias Bollhoefer, October 2003
   */

   float  systime, time_start, time_stop;
   integer i;


   evaluate_time(&time_start,&systime);
   for (i=0; i<ILUPACK_secnds_length; i++) 
       ILUPACK_secnds[i]=0;
   ILUPACK_secnds[8]=time_start;

   // set integer parameters for ILU
   param->ipar[0]=10;
   param->elbow=10.0;
   param->ipar[3]=A.nr+1;
   param->ipar[4]=0; 

   param->ipar[5]=4; // symmmetric (Hermitian) QMR
   
   param->ipar[6]=DROP_INVERSE
          |PREPROCESS_INITIAL_SYSTEM
          |PREPROCESS_SUBSYSTEMS
          |IMPROVED_ESTIMATE
          |FINAL_PIVOTING
          |SIMPLE_SC
          |AGGRESSIVE_DROPPING
          |DISCARD_MATRIX;

   // scalings
   param->ipar[7]=3+24+192;
   param->ipar[8]=(1+32+1024)*4;
   param->ipar[9]=A.nr+1;


   // set integer parameters for SPARSKIT solvers
   // init solver
   param->ipar[20]=0;
   // so far use right preconditioning
   param->ipar[21]=2;
   // stopping criteria, backward error
   param->ipar[22]=3;
   // number of restart length for GMRES,FOM,... UNUSED
   param->ipar[24]=30;
   // maximum number of iteration steps 
   param->ipar[25]=1.1*A.nr+10;
   param->ipar[25]=MIN(param->ipar[25],500);
   // init with zeros
   param->ipar[26]=
     param->ipar[27]=
     param->ipar[28]=
     param->ipar[29]=
     param->ipar[30]=
     param->ipar[31]=
     param->ipar[32]=
     param->ipar[33]=0;
   // allocate memory for iterative solver depending on our choice
   i=param->ipar[24];
   switch (param->ipar[5]) {
   case  1:   // pcg 
     i=5*A.nr;
     break;
   case  2:   // sbcg
     i=5*A.nr;
     break;
   case  3:   // bcg 
     i=7*A.nr;
     break;
   case  4:   // sqmr 
     i=6*A.nr;
     break;
   case  5:   // bcgstab 
     i=8*A.nr;
     break;
   case  6:   // tfqmr 
     i=11*A.nr;
     break;
   case  7:   // fom 
     i=(A.nr+3)*(i+2)+(i+1)*i/2;
     break;
   case  8:   // gmres 
     i=(A.nr+3)*(i+2)+(i+1)*i/2;
     break;
   case  9:   // fgmres 
     i=2*A.nr*(i+1)+((i+1)*i)/2+3*i+2;
     break;
   case 10:   // dqgmres 
     i=A.nr+(i+1)*(2*A.nr+4);
     break;
   } // end switch 
   param->ipar[23]=i;



   // set floating parameters for ILU
   param->fpar[0]=1.0/10.0; //1.0;
   param->fpar[1]=1e-2;
   param->fpar[2]=1e+2;
   param->fpar[3]=3.0;
   param->fpar[4]=0.75;
   param->fpar[5]=0.75;
   param->fpar[6]=0.75;
   param->fpar[7]=param->fpar[1]/param->fpar[2];
   param->fpar[8]=1e-4;


   // set floating parameters for SPARSKIT solvers
   param->fpar[20]=sqrt(dgeteps());
#if defined _SINGLE_REAL_ || defined _SINGLE_COMPLEX_   
   /* for single precision sqrt(eps) might by too small to
      be reached. Usually the accuracy for single precision 
      has only half as much digits as double precision, which
      is the square root of eps. To raise the threshold to a 
      reasonable size we take the geometric mean between
      eps^{1/2} and eps^{1/4}.
   */
   param->fpar[20]=sqrt(param->fpar[20]*sqrt(param->fpar[20]));
#endif

   // absolute error tolerance 
   param->fpar[21]=0;
   // init with zeros 
   param->fpar[22]=
     param->fpar[23]=
     param->fpar[24]=
     param->fpar[25]=
     param->fpar[26]=
     param->fpar[27]=
     param->fpar[28]=
     param->fpar[29]=
     param->fpar[30]=0;
       

   param->nibuff=0;
   param->ndbuff=0;
   param->ibuff=NULL;
   param->dbuff=NULL;

   param->nju=0;
   param->njlu=0;
   param->nalu=0;
   param->ju =NULL;
   param->jlu=NULL;
   param->alu=NULL;

   param->niaux=0;
   param->ndaux=0;
   param->iaux=NULL;
   param->daux=NULL;
} // end symAMGinit

