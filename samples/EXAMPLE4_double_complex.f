      PROGRAM EXAMPLE4_double_complex
*     simple program to illustrate basic usage of PJD
      IMPLICIT NONE
*     needed parameters: 
*              N: size of the problem
*         MAXEIG: max. number of wanteg eig (NEIG<=MAXEIG)
*          MAXSP: max. value of MADSPACE
      INTEGER    N, MAXEIG, MAXSP
      PARAMETER (N=1000,MAXEIG=5,MAXSP=20)
*     optimal workspace (here MAXSP*MAXSP>MAXEIG)
      INTEGER   LX
      PARAMETER (LX=N*(3*MAXSP+MAXEIG+1)+4*MAXSP*MAXSP)
      DOUBLE PRECISION  EIGS(MAXEIG), RES(MAXEIG)
      DOUBLE COMPLEX    X(LX)
*     arguments to pass to the routines
      INTEGER   NEIG, MADSPACE, ISEARCH, NINIT, ICNTL(5)
      INTEGER   ITER, IPRINT, INFO, IJOB, NDX1, NDX2, IERR
      DOUBLE PRECISION SIGMA, TOL, DELTA, SHIFT, GAP, MEM, DROPTOL
*
*     some local variables
      INTEGER I,I1,I2,J,I3
*     let us define a very simple matrix:
*
*              0     i      0      ...
*             -i     0      i       0     ...
*         A =  0    -i      0       i      0     ...
*              0     0     -i       0      i      0     ...
*             ...   ...   
*
*     we use the storage scheme needed by PJD
*     (only the upper triangular part is referenced)
      DOUBLE COMPLEX A(2*N)
      INTEGER IA(N+1), JA(2*N)
      I1=1
      DO I=1,N
         IA(I)=I1
         JA(I1)=I
         A(I1)=0.0
         I1=I1+1
         IF (I .LT. N) THEN
            JA(I1)=I+1
            A(I1)=(0.0d0,1.0d0)
            I1=I1+1
         END IF
      END DO
      IA(N+1)=I1
*
*  EXAMPLE 2
*
      PRINT *, 
     $'================================================================'
      PRINT *, 
     $'Computing the smallest eigenvalues with ILU preconditioning'
      PRINT *, 
     $'---------------------------------------------------------------'
*
c     set input variables
c     the matrix is already in the required format
c   
c     standard report on standard output
      IPRINT=6
c
c     ISEARCH=1: compute the smallest eigenvalues and we pass an 
c                appropriate SHIFT & SIGMA 
      ISEARCH=1
c     Suppose that we know the a good estimate for the smallest
c     eigenvalue, maybe from a previous run
      SIGMA=-1.9
c     use a shift close to SIGMA but maybe not an exact eigenvalue
      SHIFT=-1
c     
c     elbow space factor for the fill computed during the ILU
      MEM=20.0
c     tolerence for discarded fill
      DROPTOL=1.0d-3
c
c     number of wanted eigenvalues
      NEIG=maxeig
c     no initial approximate eigenvectors
      NINIT=0
c     desired size of the search space
      MADSPACE=maxsp
c     maximum number of iteration steps
      ITER=1000
c     tolerance for the eigenvector residual
      TOL=1.0d-10
c     additional parameters set to default
      ICNTL(1)=0
      ICNTL(2)=0
      ICNTL(3)=0
      ICNTL(4)=0
      ICNTL(5)=0
c     
c     We have the exact matrix: we may call PJD, single precision version
c
       CALL ZPJD( N,a,ja,ia, EIGS, RES, X, LX, NEIG, 
     $            SIGMA, ISEARCH, NINIT, MADSPACE, ITER, TOL,
     $            SHIFT, DROPTOL, MEM, ICNTL,
     $            IPRINT, INFO, GAP)
c
c
c     release the memory allocated for preconditioning
c     (not needed if the computation really terminates here)
      CALL ZPJDCLEANUP
c
      END
