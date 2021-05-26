#include <ilupack_fortran.h>
#include <ilupackmacros.h>

c----------------------------------------------------------------------- 
      subroutine SYMSCALE(n,a,ja,ia,diag,dbuff,ierr)  
      implicit none
      integer n,ja(*),ia(n+1),ierr, I_AMAX
      FLOAT a(*)
      FLOAT diag(n), dbuff(n)
      external I_AMAX
c-----------------------------------------------------------------------
c     scales an real/complex symmetric or Hermitian matrix such that its 
c     entries are less than or equal to one in absolute value and the
c     hopefully the maximum entry in each row/column is close to one in
c     absolute value
c-----------------------------------------------------------------------
c on entry:
c ---------
c n    = size of the matrix A
c
c a,
c ja,
c ia   = Matrix A in compressed sparse row format.
c 
c on return:
c----------
c
c diag = diagonal matrix stored as a vector containing the matrix
c        by which the rows/columns have been scaled, i.e., on return 
c        we have A = Diag*A*Diag.
c
c	    
c ierr  = error message. ierr=0     : Normal return 
c                        ierr=i > 0 : Row number i is a zero row.
c Notes:
c-------
c 1)        The column dimension of A is not needed. 
c
c     code adapted from DIAMUA, AMUDIA of Yousef Saad's SPARSKIT.
c     adapted by Matthias Bollhoefer, December 2004 
c-----------------------------------------------------------------
      integer i,j,k,pow

      REALS log2, fpow
      REALS SQRTFNAME, ABSFNAME, LOGFNAME
      REALS zero, one, two
#if defined _SINGLE_REAL_ || defined _SINGLE_COMPLEX_
      parameter(zero=0.0e0, one=1.0e0, two=2.0e0)
#else
      parameter(zero=0.0D0, one=1.0D0, two=2.0D0)
#endif

      

      log2=one/LOG(two)

c     init scaling vector
      do j=1, n
         diag(j)=FNULL
         dbuff(j)=FNULL
      end do

c     compute maximum of any row/column
      do j=1, n
         i=ia(j)
         k=ia(j+1)-i
         i=i-1+I_AMAX(k,a(i),1)
         diag(j)=max(ABS(a(i)),CTOD(diag(j)))
         do i=ia(j),ia(j+1)-1
            k=ja(i)
            diag(k)=max(ABS(a(i)),CTOD(diag(k)))
         end do
      end do

      ierr = 0
c     invert diagonal entries
      do j=1, n
         if (diag(j).eq.FNULL) then
            ierr = j 
            return
         else
c           invert diagonal entry
            diag(j)=one/CTOD(diag(j))
         end if
      end do

c     compute maximum of any row/column if only row scaling would have
c     been applied
      do j=1, n
         do i=ia(j),ia(j+1)-1
            k=ja(i)
            dbuff(k)=max(CTOD(diag(j))*ABS(a(i)),CTOD(dbuff(k)))
            dbuff(j)=max(CTOD(diag(k))*ABS(a(i)),CTOD(dbuff(j)))
         end do
      end do

c     invert diagonal entries
      do j=1, n
         if (dbuff(j) .eq. FNULL) then
            ierr = j 
            return
         else
c           invert diagonal entry and take geometric mean of column
c           and row scaling factors
            diag(j)=SQRT(CTOD(diag(j)/dbuff(j)))
         end if
      end do

c     take nearest power of 2
      do j=1, n
         fpow=LOG(ABS(diag(j)))*log2
         if (fpow.lt.zero) then
            pow=fpow-0.5
            fpow=one
            do i=1,-pow
               fpow=fpow*two
            end do
            diag(j) = FONE/fpow
         else
            pow=fpow+0.5
            fpow=one
            do i=1,pow
               fpow=fpow*two
            end do
            diag(j)=fpow
         end if
      end do
      call DIAMUA(n,a,ja,ia,diag)
      call AMUDIA(n,a,ja,ia,diag)

      return
c--------end-of-symscale------------------------------------------------ 
c-----------------------------------------------------------------------
      end
c-----------------------------------------------------------------------
