#include <ilupack_fortran.h>
#include <ilupackmacros.h>

c     real case
#if defined _DOUBLE_REAL_ || defined _SINGLE_REAL_
#define CONJUG(A)      (A)

c     real and skew symmetric
#ifdef _SKEW_MATRIX_

#ifdef _DOUBLE_REAL_
#define MYSYMPILUCLSOL       DSSMpiluclsol
#else
#define MYSYMPILUCLSOL       SSSMpiluclsol
#endif

#define SKEW(A)      (-(A))
c     end real and skew symmetric

c     real and symmetric
#else

#ifdef _DOUBLE_REAL_
#define MYSYMPILUCLSOL       DSYMpiluclsol
#else
#define MYSYMPILUCLSOL       SSYMpiluclsol
#endif

#define SKEW(A)      (A)
#endif
c     end real and symmetric
c     end real case



c     complex case
#else

c     complex and symmetric or skew-symmetric
#ifdef _COMPLEX_SYMMETRIC_
#define CONJUG(A)     (A)

c     complex and skew-symmetric
#ifdef _SKEW_MATRIX_
#define SKEW(A)      (-(A))

#ifdef _SINGLE_COMPLEX_
#define MYSYMPILUCLSOL       CSSMpiluclsol
#else
#define MYSYMPILUCLSOL       ZSSMpiluclsol
#endif
c     end complex and skew-symmetric

c     complex and symmetric
#else
#define SKEW(A)      (A)

#ifdef _SINGLE_COMPLEX_
#define MYSYMPILUCLSOL       CSYMpiluclsol
#else
#define MYSYMPILUCLSOL       ZSYMpiluclsol
#endif

#endif
c     end complex and symmetric
c     end complex and symmetric or skew-symmetric


c    complex and Hermitian or skew-Hermitian
#else
#define CONJUG(A)     CONJ(A)

c    complex and skew-Hermitian
#ifdef _SKEW_MATRIX_
#define SKEW(A)      (-(A))

#ifdef _SINGLE_COMPLEX_
#define MYSYMPILUCLSOL       CSHRpiluclsol
#else
#define MYSYMPILUCLSOL       ZSHRpiluclsol
#endif
c    end complex and skew-Hermitian

c    complex and Hermitian
#else
#define SKEW(A)      (A)

#ifdef _SINGLE_COMPLEX_
#define MYSYMPILUCLSOL       CHERpiluclsol
#else
#define MYSYMPILUCLSOL       ZHERpiluclsol
#endif

#endif
c    complex and Hermitian

#endif
c    complex and Hermitian or skew-Hermitian

#endif
c    end complex case


        subroutine MYSYMPILUCLSOL(n, y, x, aldl, jldl)
        integer n, jldl(*)
        FLOAT x(n), y(n), aldl(*)
c-----------------------------------------------------------------------
c
c This routine solves the system (U^TD^{-1}) x = y, 
c given an LD^{-1}L^T=U^TD^{-1}U decomposition of a matrix stored in 
c (aldl, jldl) modified sparse row format 
c
c ***********     The routines fits with SYMPILUC     ******************
c
c-----------------------------------------------------------------------
c Code written by Matthias Bollhoefer, January 23, 2005                *
c----------------------------------------------------------------------*
c on entry:
c n   = dimension of system 
c y   = the right-hand-side vector
c aldl, jldl
c     = the LD^{-1}L^T=U^TD^{-1}U matrix as provided from the SYMPILUC
c       routine. 
c
c on return
c x   = solution of U^TD^{-1} x = y.     
c-----------------------------------------------------------------------
c 
c Note: routine is in place: call sympiluclsol (n, x, x, aldl, jldl) 
c       will solve the system with rhs x and overwrite the result on x. 
c
c-----------------------------------------------------------------------
c local variables
c
        integer i,j,k,ii
        FLOAT buff,buff2
c
c forward solve with U^TD^{-1}. Note that L=U^T is stored by columns!
c

        do i=1,n
           x(i)=y(i)
        end do


        i=1
c       while i<=n
 40     if (i.gt.n) goto 50
           if (jldl(n+1+i).eq.0) then
              buff = x(i)*aldl(i)
              do k=jldl(i),jldl(i+1)-1
                 j=jldl(k)
                 x(j) = x(j) - SKEW(CONJUG(aldl(k)))*buff
              end do
              i=i+1
           else
              buff =aldl(i)                  *x(i)+aldl(n+1+i)*x(i+1)
              buff2=CONJUG(SKEW(aldl(n+1+i)))*x(i)+aldl(i+1)  *x(i+1)
              ii=jldl(i)
              do k=jldl(i),jldl(i+1)-1
                 j=jldl(k)
                 x(j)=x(j)-SKEW(CONJUG(aldl(ii)))  *buff
     +                    -SKEW(CONJUG(aldl(ii+1)))*buff2
                 ii=ii+2
              end do
              i=i+2
           end if
        goto 40
c       end while
c
 50     return
c----------------end of sympiluclsol ------------------------------------
c-----------------------------------------------------------------------
        end
