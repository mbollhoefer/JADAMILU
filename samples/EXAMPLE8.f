      PROGRAM EXAMPLE8
*     simple program to illustrate basic usage of JDREVCOM
      IMPLICIT NONE
*     needed parameters: 
*              N: size of the problem
*         MAXEIG: max. number of wanteg eig (NEIG<=MAXEIG)
*          MAXSP: max. value of MADSPACE
      INTEGER    N, MAXEIG, MAXSP
      PARAMETER (N=1000,MAXEIG=5,MAXSP=20)
*     optimal workspace (here MAXSP*MAXSP>MAXEIG)
      INTEGER LX
      PARAMETER (LX=N*(3*MAXSP+MAXEIG+1)+4*MAXSP*MAXSP)
      DOUBLE PRECISION   EIGS(MAXEIG), RES(MAXEIG), X(LX)
*     arguments to pass to the routines
      INTEGER            NEIG, MADSPACE, ISEARCH, NINIT, ICNTL(5)
      INTEGER            ITER, IPRINT, INFO
      DOUBLE PRECISION   SIGMA, TOL, GAP
      INTEGER            IJOB, NDX1, NDX2

*     some local variables
      INTEGER I


*
*
      PRINT *, 
     $'================================================================'
      PRINT *, 
     $'Computing the smallest eigenvalues with custom preconditioning'
      PRINT *, 
     $'--------------------------------------------------------------'
*
c     set input variables
c     the matrix is already in the required format
c   
c     standard report on standard output
      IPRINT=6
c     we want the smallest eigenvalues
      ISEARCH=0
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
c     start reverse communication
      IJOB=0
 10   CALL DJDREVCOM( N,EIGS, RES, X, LX, NEIG, 
     $                SIGMA, ISEARCH, NINIT, MADSPACE, ITER, TOL,
     $                IJOB, NDX1, NDX2, IPRINT, INFO, GAP)
c
c     your private matrix-vector multiplication
      IF (IJOB.EQ.1) THEN
c        X(NDX1) input,  X(NDX2) output
         X(NDX2)=4*X(NDX1)-X(NDX1+1)-X(NDX1+N-1)
         DO I=1,N-2
            X(NDX2+I)=-X(NDX1+I-1)+4*X(NDX1+I)-X(NDX1+I+1)
	 END DO
         X(NDX2+N-1)=-X(NDX1)-X(NDX1+N-2)+4*X(NDX1+N-1)
         GOTO 10
c     your private preconditioner
      ELSE IF (IJOB.EQ.2) THEN
c        X(NDX2) input,  X(NDX1) output
         X(NDX1)  =(16*X(NDX2)+ 4*X(NDX2+1)+  X(NDX2+2)+X(NDX2+N-2)
     +              +4*X(NDX2+N-1))/64.0
         X(NDX1+1)=( 4*X(NDX2)+16*X(NDX2+1)+4*X(NDX2+2)+X(NDX2+3)
     +              +X(NDX2+N-1))/64.0
         DO I=2,N-3
            X(NDX1+I)=(X(NDX2+I-2)+4*X(NDX2+I-1)+16*X(NDX2+I)
     +              +4*X(NDX2+I+1)+X(NDX2+I+2))/64.0
	 END DO
         X(NDX1+N-2)=(X(NDX2)+X(NDX2+N-4)+4*X(NDX2+N-3)
     +            +16*X(NDX2+N-2)+4*X(NDX2+N-1))/64.0
         X(NDX1+N-1)=(4*X(NDX2)+X(NDX2+1)+X(NDX2+N-3)
     +               +4*X(NDX2+N-2)+16*X(NDX2+N-1))/64.0
         GOTO 10
      END IF
c
c     work done - release internal memory and discard preconditioner
      CALL DPJDCLEANUP
c
      END
