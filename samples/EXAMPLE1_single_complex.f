      PROGRAM EXAMPLE1_single_complex
*     simple program to illustrate basic usage of PJD
      IMPLICIT NONE
*     needed parameters: 
*              N: size of the problem
*         MAXEIG: max. number of wanteg eig (NEIG<=MAXEIG)
*          MAXSP: max. value of MADSPACE
      INTEGER     N, MAXEIG, MAXSP
      PARAMETER (N=1000,MAXEIG=5,MAXSP=20)
*     optimal workspace (here MAXSP*MAXSP>MAXEIG)
      INTEGER     LX
      PARAMETER (LX=N*(3*MAXSP+MAXEIG+1)+4*MAXSP*MAXSP)
      REAL        EIGS(MAXEIG), RES(MAXEIG)
      COMPLEX     X(LX)
*     arguments to pass to the routines
      INTEGER     NEIG, MADSPACE, ISEARCH, NINIT, ICNTL(5)
      INTEGER     ITER, IPRINT, INFO
      REAL        SIGMA, TOL, DELTA, SHIFT, GAP, MEM, DROPTOL
*
*     some local variables
      INTEGER I,K,I2,J,I3
*     let us define a very simple matrix:
*
*              1     5i     0      ...
*             -5i    2      5i      0     ...
*         A =  0    -5i     3       5i     0     ...
*              0     0     -5i       4      5i     0     ...
*             ...   ...   
*
*     we use the storage scheme needed by PJD
*     (only the upper triangular part is referenced)
      COMPLEX A(2*N)
      INTEGER IA(N+1), JA(2*N)
      K=1
      DO I=1,N
         IA(I)=K
         JA(K)=I
         A(K)=REAL(I)
         K=K+1
         IF (I .LT. N) THEN
            JA(K)=I+1
            A(K)=(0.0E0,5.0E0)
            K=K+1
         END IF
      END DO
      IA(N+1)=K
*
*  EXAMPLE 1
*
      PRINT *, 
     $'================================================================'
      PRINT *, 
     $'Computing the eigenvalues closest to origin ILU preconditioning'
      PRINT *, 
     $'---------------------------------------------------------------'
*
c     set input variables
c     the matrix is already in the required format
c   
c     standard report on standard output
      IPRINT=-6
c
c     we want the eigenvalues near target sigma=0         
      ISEARCH=2
      SIGMA=0.0e0
c     (then, SHIFT need not to be set)
c     
c     elbow space factor for the fill computed during the ILU
      MEM=20.0
c     tolerence for discarded fill
      DROPTOL=1.e-3
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
      TOL=1.0e-7
c     additional parameters set to default
      ICNTL(1)=0
      ICNTL(2)=0
      ICNTL(3)=0
      ICNTL(4)=0
      ICNTL(5)=0
c     
c     We have the exact matrix: we may call CPJD, single complex version
c
       CALL CPJD( N,a,ja,ia, EIGS, RES, X, LX, NEIG, 
     $           SIGMA, ISEARCH, NINIT, MADSPACE, ITER, TOL,
     $           SHIFT, DROPTOL, MEM, ICNTL,
     $           IPRINT, INFO, GAP)
c
c
c     release the memory allocated for preconditioning
c     (not needed if the computation really terminates here)
      CALL CPJDCLEANUP
c
      END
