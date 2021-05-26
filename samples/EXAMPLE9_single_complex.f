      PROGRAM EXAMPLE9_single_complex
*     simple program to illustrate basic usage of PJDREVCOM with
*     diagonal preconditioning
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
      REAL      EIGS(MAXEIG), RES(MAXEIG)
      COMPLEX   X(LX), D(N)
*     arguments to pass to the routines
      INTEGER            NEIG, MADSPACE, ISEARCH, NINIT, ICNTL(5)
      INTEGER            ITER, IPRINT, INFO
      REAL               SIGMA, TOL, GAP, MEM, DROPTOL, SHIFT
      INTEGER            IJOB, NDX1, NDX2

*     some local variables
      INTEGER I,J,K
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
            A(K)=(0.0,5.0)
            K=K+1
         END IF
      END DO
      IA(N+1)=K
*
*
      PRINT *, 
     $'================================================================'
      PRINT *, 
     $'Computing the smallest eigenvalues with diagonal preconditioning'
      PRINT *, 
     $'---------------------------------------------------------------'
*
c     use a copy of the diagonal entries to be passed to PJDREVCOM
      DO I=1,N
         D(I)=0.0
         DO J=IA(I),IA(I+1)-1
            K=JA(J)
            IF (K.EQ.I) D(I)=A(J)
         END DO
      END DO
c     set input variables
c     the matrix is already in the required format
c   
c     standard report on standard output
      IPRINT=6
c
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
      TOL=1.0e-5
c     additional parameters set to default
      ICNTL(1)=0
      ICNTL(2)=0
      ICNTL(3)=0
      ICNTL(4)=0
      ICNTL(5)=0

      IJOB=0
 10   CALL CPJDREVCOM( N,D,-1,-1,EIGS, RES, X, LX, NEIG, 
     $                 SIGMA, ISEARCH, NINIT, MADSPACE, ITER, TOL,
     $                 SHIFT, DROPTOL, MEM, ICNTL,
     $                 IJOB, NDX1, NDX2, IPRINT, INFO, GAP)
c
c     your private matrix-vector multiplication
      IF (IJOB.EQ.1) THEN
c        X(NDX1) input,  X(NDX2) output
         call matvec(N,IA,JA,A,X(NDX1),X(NDX2))
         GOTO 10
      END IF
c
c     release internal memory and discard preconditioner
      CALL CPJDCLEANUP
c
      END


cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     Matrix-Vector Multiplication
c     (This routine should work for any matrix a, ja, ia compliant to 
c     the format required by PJD/PJDREVCOM) 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine matvec(n,ia,ja,a,x,y) 
      integer n, ia(n+1), ja(*)
      complex a(*),x(n),y(n)

      integer i,k,j

      do i=1,n
         y(i)=(0.0,0.0)
      end do

      do i=1,n
         do k=ia(i),ia(i+1)-1
           j=ja(k)
           if (i.eq.j) then
             y(i)=y(i)+a(k)*x(i)
           else
             y(i)=y(i)+a(k)*x(j)
             y(j)=y(j)+conjg(a(k))*x(i)
           end if
         end do
      end do
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
