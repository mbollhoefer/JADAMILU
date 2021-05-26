      PROGRAM EXAMPLE5
*     simple program to illustrate basic usage of PJDREVCOM
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
      INTEGER            ITER, IPRINT, INFO, IJOB, NDX1, NDX2
      DOUBLE PRECISION   SIGMA, TOL, VAL, SHIFT, GAP, MEM, DROPTOL
*
*     some local variables
      INTEGER I,K
*     let us define a very simple matrix:
*
*              0     5      0      ...
*              5     0      5       0     ...
*         A =  0     5      0       5      0     ...
*              0     0      5       0      5      0     ...
*             ...   ...   
*
*     we use the storage scheme needed by PJDREVCOM
*     (only the upper triangular part is referenced)
      DOUBLE PRECISION A(2*N)
      INTEGER IA(N+1), JA(2*N)
      K=1
      DO I=1,N
         IA(I)=K
         JA(K)=I
         A(K)=0.0D0
         K=K+1
         IF (I .LT. N) THEN
            JA(K)=I+1
            A(K)=5.0D0
            K=K+1
         END IF
      END DO
      IA(N+1)=K
*
      PRINT *, 
     $'================================================================'
      PRINT *, 
     $'Computing the smallest eigenvalues with ILU preconditioning'
      PRINT *, 
     $'-----------------------------------------------------------'
*
c     set input variables
c     the matrix is already in the required format
c   
c     standard report on standard output
      IPRINT=6
c
c     ISEARCH=0: compute the smallest eigenvalues and no a priori
c                information on the eigenvalues is available
      ISEARCH=0
c     (ISEARCH.eq.0: SIGMA, SHIFT need not to be set)
c     
c     elbow space factor for the fill computed during the ILU
      MEM=20.0
c     tolerence for discarded fill
      DROPTOL=1.d-3
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
c     Here we start the computation
      IJOB=0
c     
c     We do not have the exact matrix, which A plus a rank one correction;
c     we call PJDREVCOM (double precision version)
 10   CALL DPJDREVCOM( N,A,JA,IA, EIGS, RES, X, LX, NEIG, 
     $                 SIGMA, ISEARCH, NINIT, MADSPACE, ITER, TOL,
     $                 SHIFT, DROPTOL, MEM, ICNTL,
     $                 IJOB, NDX1, NDX2, IPRINT, INFO, GAP)
c
c
      if (IJOB.NE.1) GOTO 20
*     IJOB=1: MATVEC including rank-1 correction
c     X(NDX1) input, X(NDX2) output
c     MATVEC without rank-1
      call matvec(N,IA,JA,A,X(NDX1),X(NDX2))
c     rank-1 update
c       inner product needed by the rank-1 correction
        VAL=0.0
        do I=0,N-1
           VAL=VAL+X(NDX1+I)
        end do
c       scaling factor
        VAL=VAL*0.1
c       perform update
        do I=0,N-1
           X(NDX2+I)=X(NDX2+I)+VAL
        end do
      goto 10
c
 20   continue
c
c     release internal memory and discard preconditioner
      CALL DPJDCLEANUP
c
      END

cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     Matrix-Vector Multiplication
c     (This routine should work for any matrix a, ja, ia compliant to 
c     the format required by PJD/PJDREVCOM) 
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      subroutine matvec(n,ia,ja,a,x,y) 
      integer n, ia(n+1), ja(*)
      doubleprecision a(*),x(n),y(n)

      integer i,k,j

      do i=1,n
         y(i)=0.0
      end do

      do i=1,n
         do k=ia(i),ia(i+1)-1
           j=ja(k)
           if (i.eq.j) then
             y(i)=y(i)+a(k)*x(i)
           else
             y(i)=y(i)+a(k)*x(j)
             y(j)=y(j)+a(k)*x(i)
           end if
         end do
      end do
      end
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
