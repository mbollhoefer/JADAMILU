#include <ilupack_fortran.h>
#include <ilupackmacros.h>

c#define PRINT_INFO3


c     real case
#if defined _DOUBLE_REAL_ || defined _SINGLE_REAL_
#define CONJUG(A)      (A)

c     real and skew symmetric
#ifdef _SKEW_MATRIX_

#ifdef _DOUBLE_REAL_
#define MYSYMPILUC          DSSMpiluc
#define MYSYMPILUCUPDATE    DSSMpilucupdate
#define MYILUCLIST          DSSMiluclist2
#define MYSSYMILUCUPDATE    DSSMssymilucupdate
#define AGGRESSIVEDROPPING  DSSMaggressivedropping
#define FAGGRESSIVEDROPPING DSSMfaggressivedropping
#else
#define MYSYMPILUC          SSSMpiluc
#define MYSYMPILUCUPDATE    SSSMpilucupdate
#define MYILUCLIST          SSSMiluclist2
#define MYSSYMILUCUPDATE    SSSMssymilucupdate
#define AGGRESSIVEDROPPING  SSSMaggressivedropping
#define FAGGRESSIVEDROPPING SSSMfaggressivedropping
#endif

#define SKEW(A)      (-(A))
c     end real and skew symmetric

c     real and symmetric
#else

#ifdef _DOUBLE_REAL_
#define MYSYMPILUC          DSYMpiluc
#define MYSYMPILUCUPDATE    DSYMpilucupdate
#define MYILUCLIST          DSYMiluclist2
#define MYSSYMILUCUPDATE    DSYMssymilucupdate
#define AGGRESSIVEDROPPING  DSYMaggressivedropping
#define FAGGRESSIVEDROPPING DSYMfaggressivedropping
#else
#define MYSYMPILUC          SSYMpiluc
#define MYSYMPILUCUPDATE    SSYMpilucupdate
#define MYILUCLIST          SSYMiluclist2
#define MYSSYMILUCUPDATE    SSYMssymilucupdate
#define AGGRESSIVEDROPPING  SSYMaggressivedropping
#define FAGGRESSIVEDROPPING SSYMfaggressivedropping
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
#define MYSYMPILUC          CSSMpiluc
#define MYSYMPILUCUPDATE    CSSMpilucupdate
#define MYILUCLIST          CSSMiluclist2
#define MYSSYMILUCUPDATE    CSSMssymilucupdate
#define AGGRESSIVEDROPPING  CSSMaggressivedropping
#define FAGGRESSIVEDROPPING CSSMfaggressivedropping
#else
#define MYSYMPILUC          ZSSMpiluc
#define MYSYMPILUCUPDATE    ZSSMpilucupdate
#define MYILUCLIST          ZSSMiluclist2
#define MYSSYMILUCUPDATE    ZSSMssymilucupdate
#define AGGRESSIVEDROPPING  ZSSMaggressivedropping
#define FAGGRESSIVEDROPPING ZSSMfaggressivedropping
#endif
c     end complex and skew-symmetric

c     complex and symmetric
#else
#define SKEW(A)      (A)

#ifdef _SINGLE_COMPLEX_
#define MYSYMPILUC          CSYMpiluc
#define MYSYMPILUCUPDATE    CSYMpilucupdate
#define MYILUCLIST          CSYMiluclist2
#define MYSSYMILUCUPDATE    CSYMssymilucupdate
#define AGGRESSIVEDROPPING  CSYMaggressivedropping
#define FAGGRESSIVEDROPPING CSYMfaggressivedropping
#else
#define MYSYMPILUC          ZSYMpiluc
#define MYSYMPILUCUPDATE    ZSYMpilucupdate
#define MYILUCLIST          ZSYMiluclist2
#define MYSSYMILUCUPDATE    ZSYMssymilucupdate
#define AGGRESSIVEDROPPING  ZSYMaggressivedropping
#define FAGGRESSIVEDROPPING ZSYMfaggressivedropping
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
#define MYSYMPILUC          CSHRpiluc
#define MYSYMPILUCUPDATE    CSHRpilucupdate
#define MYILUCLIST          CSHRiluclist2
#define MYSSYMILUCUPDATE    CSHRssymilucupdate
#define AGGRESSIVEDROPPING  CSHRaggressivedropping
#define FAGGRESSIVEDROPPING CSHRfaggressivedropping
#else
#define MYSYMPILUC          ZSHRpiluc
#define MYSYMPILUCUPDATE    ZSHRpilucupdate
#define MYILUCLIST          ZSHRiluclist2
#define MYSSYMILUCUPDATE    ZSHRssymilucupdate
#define AGGRESSIVEDROPPING  ZSHRaggressivedropping
#define FAGGRESSIVEDROPPING ZSHRfaggressivedropping
#endif
c    end complex and skew-Hermitian

c    complex and Hermitian
#else
#define SKEW(A)      (A)

#ifdef _SINGLE_COMPLEX_
#define MYSYMPILUC          CHERpiluc
#define MYSYMPILUCUPDATE    CHERpilucupdate
#define MYILUCLIST          CHERiluclist2
#define MYSSYMILUCUPDATE    CHERssymilucupdate
#define AGGRESSIVEDROPPING  CHERaggressivedropping
#define FAGGRESSIVEDROPPING CHERfaggressivedropping
#else
#define MYSYMPILUC          ZHERpiluc
#define MYSYMPILUCUPDATE    ZHERpilucupdate
#define MYILUCLIST          ZHERiluclist2
#define MYSSYMILUCUPDATE    ZHERssymilucupdate
#define AGGRESSIVEDROPPING  ZHERaggressivedropping
#define FAGGRESSIVEDROPPING ZHERfaggressivedropping
#endif

#endif
c    complex and Hermitian

#endif
c    complex and Hermitian or skew-Hermitian

#endif
c    end complex case




      subroutine MYSYMPILUC(n,a,ja,ia,lfilv,droptols,condest,nLU,
     +     param,p,invq,aldl,jldl,iwk,w,jw,shiftA,amgcancel,ierr)
c-----------------------------------------------------------------------
      implicit none 
      integer n,iwk,nLU,p(n),invq(n),shiftA,
     +        ja(*),ia(n+1),jldl(iwk),jw(*),lfilv(2),ierr,param
      REALS droptols(3),condest,amgcancel
      FLOAT a(*),aldl(iwk),w(*)
c----------------------------------------------------------------------*
c       *** SYMPILUC, SYMMETRIC PARTIAL ILUC preconditioner ***        *
c                                                                      *
c symmetric version of PILUC                                           *
c     SYMPILUC factors a (skew-) symmetric (Hermitian) matrix (may be  *
c     symmetrically permuted) partially                                *
c         .          / L  0\ /D 0\ /U F_U\                             *
c     i.e. P^T A P ~ |     | |   | |     |                             *
c                    \E_L I/ \0 S/ \0  I /                             *
c     Certainly only the upper triangular part is stored.              *
c     The ILU uses diagonal pivoting to keep the inverse triangular    *
c     factors bounded                                                  *
c                                                                      *
c SYPMPILUC goes far beyond the simple single-level ILDLC by           *
c     introducing additional features                                  *
c     - symmetric permutations (remember that only the upper triangular*
c                               part is stored!!!)                     *
c     - a PARTIAL ILDLC decomposition instead of a full decomposition, *
c       i.e. the decomposition is terminated on an earlier stage       *
c     - diagonal pivoting, i.e. at step k of the algorithm, column/row *
c       k are skipped and pushed to the end, if the norm of U^{-1}     *
c       exceeds a prescribed bound                                     *
c                                                                      *
c Further options.                                                     *
c     - After the partial decomposition the approximate Schur          *
c       complement can be provided                                     *
c     - diagonal compensation is offered                               *
c     - symmetry is preserved                                          *
c     - as approximate Schur complement either                         *
c        o  the standard (simple) approximate Schur complement can be  *
c           used or,                                                   *
c        o  a medium approximate Schur-complement is offered that is   *
c           essentially derived from applying the approximate inverse  *
c           to the original system or,                                 *
c        o  the even more precise (and more expensive) Tismenetsky-like*
c           approximate Schur complement can be computed (in all steps)*
c                                                                      *
c----------------------------------------------------------------------*
c Code written by Matthias Bollhoefer, February, 2005                  *
c----------------------------------------------------------------------*
c PARAMETERS                                                           
c-----------                                                           
c
c on entry:
c========== 
c n         integer. The row dimension of the matrix A. The matrix 
c
c a,ja,ia   matrix stored in Compressed Sparse Row format.              
c           since the matrix is symmetric we assume that only the upper
c           triangular part is stored
c           ia: integer. array of size n+1. ia(j) points to the starting
c               position of row j, ia(n+1)=number of nonzeros +1, first
c               position behind row n
c           ja,a: integer/float array of size ia(n+1)-1. Column indices
c                 and associated numerical values
c           IMPORTANT: we assume that the column indices are stored in
c           increasing order. The algorithm will alter a and ja in order
c           change the order of the column indices and the numerical
c           values
c
c lfilv     array of integer. The fill-in parameters. Each column of L and each
c           row of U will have a maximum of lfilv(1) elements (excluding the
c           diagonal element). 
c           Each row of the approximate Schur complement will have at most
c           lfilv(2) entries.
c           lfilv(1),lfilv(2) must be .ge. 0.
c
c droptols  real array of size 2. 
c           droptols[1] sets the threshold for dropping small terms in the
c           factorization. See below for details on dropping strategy.
c           droptols[2] is used to drop entries from the Schur complement
c
c  
c iwk       integer. The lengths of arrays aldl and jldl. If the arrays
c           are not big enough to store the ILU factorizations, SYMPILUC
c           will stop with an error message. 
c
c param     integer. Options of SYMPILUC (to be read bitwise)
c           bit 0
c            0   simple dual threshold dropping strategy   
c            1   inverse based dropping strategy
c           bit 2
c            0   simple Schur complement, very sparse but less accurate
c                version
c            4   Tismenetsky-like Schur complement, more dense but the 
c                error is squared
c           bit 4
c            0   simple inverse-based dropping strategy is used
c           16   inverse based dropping strategy is improved by a more 
c                precise estimate of the inverse
c           bit 5
c            0   no diagonal compensation
c           32   diagonal compensation is used, i.e., we ensure that the
c                ILU satisfies LU=A+E, where E*1=0 (1 denotes the vector
c                with all entries equal to 1)
c           bit 6
c            0   skipped entries are kept in the partial ILU
c           64   skipped entries are removed from the partial ILU
c           bit 7
c           bit 8
c           bit 9
c            0   remaining Schur-complement is computed by applying the
c                approximate inverse factors to the original system
c          512   simple remaining Schur-complement is computed
c
c On return:
c===========
c
c jldl,aldl integer/float array. 
c           recommended minimum size: 2n+1 + number of nonzeros of a.
c           Matrix stored in Modified Sparse Row (MSR) format containing
c           the U factor and if desired, the approximate Schur complement. 
c           The tridiagonal part is located in aldl(1:2n-1)
c
c ierr      integer. Error message with the following meaning.
c           ierr  = 0    --> successful return.
c           ierr .gt. 0  --> zero pivot encountered at step number ierr.
c           ierr  = -1   --> Error. input matrix may be wrong.
c                            (The elimination process has generated a
c                            row U whose length is .gt.  n.)
c           ierr  = -3   --> The matrix U overflows the array aldl.
c           ierr  = -4   --> Illegal value for lfil.
c
c work arrays:
c=============
c jw        integer work array of length 12*n (if bit 2 is NOT set).
c           Then length is 14*n, if bit 2 is set.
c 
c           note that some variables share the same array. There is
c           no memory conflict because these variables access different
c           parts of the array at step k
c
c           jw(1:n)      index indicator array, first row
c           jw(n+1:2n)   list of nonzero indices, first row
c
c           jw(njw1(2)+1 index indicator array, second row
c             :njw1(2)+n)      
c           jw(njw2(2)+1 list of nonzero indices, second row
c             :njw2(2)+n)      
c        
c           jw(2n+1:3n)  head of the linked list for the columns of A.
c                        on entry to step k only jw(2n+k:3n) is
c                        referenced, after that only jw(2n+k+1:3n) is
c                        needed
c
c           jw(3n+1:4n)  linked list for the leading columns of A
c                        (A is stored by rows, only the upper triangular part)
c
c           jw(4n+1:5n)  pointer to the first entry of any row at step k
c
c           jw(5n+1:6n)  number of skipped entries in each row of U.
c
c           jw(6n+1:7n)  linked list to the nonzero elements in column k
c                        of U (although U is stored by rows). jw(3n+k) 
c                        contains the reference of the first nonzero
c                        entry, jw(6n+jw(6n+k)) the second one and so on
c                        until a zero is reached which indicates the end
c
c           jw(7n+1:8n)  additional auxiliary pointer to the
c                        free space behind the regular part of U.
c                        on entry to step k, only jw(nb+1:nb+k-1) is
c                        referenced, later on when sparsifying row k
c                        of U, jw(nb+k) is set
c
c           jw(8n+1:9n)  pointer to first element of U(j,k:n) for j<k,
c                        epsilon size part
c                        on entry to step k, only jw(8n+1:8n+k-1) is 
c                        referenced, at the end in `iluclist', jw(8n+k)
c                        is also set
c
c           jw(9n+1:10n) pointer to first element of U(j,k:n) for 1<=j<k
c                        on entry to step k, only jw(na+1:na+k-1) is 
c                        referenced, at the end (in `iluclist'),
c                        jw(na+k) is also set. Only non-skipped components
c                        are referenced. 
c
c           jw(10n+1:11n)linked list to the nonzero elements in column k
c                        of U, part with epsilon size values (although U
c                        is stored by rows). jw(10n+k) contains the
c                        reference of the first nonzero entry,
c                        jw(10n+jw(10n+k)) the second one and so on
c                        until a zero is reached which indicates the end
c
c
c           jw(nc+1:nc+n) inverse mapping for all nodes that have been 
c                         skipped. nc is chosen dependent on the
c                         switches given by param
c
c           jw(el+1:el+n) number of skipped entries in each row of U,
c                         extended part (necessary if Tismenetsky update
c                         is required).
c                         el is chosen dependent on the switches given 
c                         by param. 
c
c           jw(ne+1:ne+n)counter for the number of essential nonzeros in
c                        each column of U^{-1}, used to estimate the
c                        infinity norm based on the 1-norm estimate that
c                        is computed on entry to step k, only
c                        jw(ne+k:ne+n) is referenced, later on after
c                        sparsifying jw(ne+k) is no longer needed. ne is
c                        chosen dependent on the switches given by param. 
c
c           memory layout
c           if bit 2 of param is NOT set:     if bit 2 of param IS set:
c                  nc= 8n                            nc=11n             
c                  ne=10n                            ne=13n
c                                                    el=12n             
c
c
c w         real work array of length 3*n, 4*n if bit 4 is set
c
c           w(1:n)       used as buffer for the numerical values
c
c           if bit 0 is set to 1(or not set):
c            w(n+1:2n)    condition estimator for U
c            w(2n+1:3n)   used as buffer for the numerical values
c                         of the second column
c
c           if bit 0 is set to 1(or not set), AND bit 4 is set to 16:
c           also if the default Schur complement is used, then this buffer
c           is needed
c            w(2n+1:3n)   improved condition estimator for U
c            w(3n+1:4n)   used as buffer for the numerical values
c                         of the second column
c
c----------------------------------------------------------------------*
c     locals
      integer i,j,ii,jj,k,l,m,r,n2,n3,n4,n5,n6,n7,n8,n9,n10,na,nc,nl,
     +        droptype,sctype,len,improved,ne,nnzr,nnzc,simplesc,el,
     +        milu,nskipped,nb,nd,tailor,loadrow,nw,kk,mode,finalrow,
     +        rlen(2),njw1(2),njw2(2),lfil,lfils,jbuff,
     +        aggressive,discardA,maxiwk
      REALS  ax,ay,axp,axm,mup,mum,mup2,mum2,buffer,absaldlk,
     +        droptolL,droptolU,sdroptol,droptol2,nup,num,droptol,
     +        Lmax,Umax,DLmax,DUmax,sigma,maxcondest,nup2,num2,axp2,
     +        axm2,buffer2,condest1,condest2,maxcondestold
      logical skip
      intrinsic max,min0
      FLOAT x,x2,y,xp,xm,xp2,xm2,signum,signum2,aldlk,ialdlk,
     +      xp3,xp4,xm3,xm4,U11,U12,U21,U22,wbuff
      REALS ABSFNAME, SQRTFNAME, ASUM
      external iluclist,piluclist
      
c i,j,k,l,m        counters
c
c n2,...,n1       abbreviations for 2*n,...,10*n
c
c droptype         0 standard dropping,       1 inverse based dropping
c sctype           0 simple Schur complement, 1 Tismenetsky-like
c improved         0 simple inverse estimates 1 improved estimates
c milu             0 no diagonal comp.        1 diagonal comp. used
c tailor           0 coarse grid entries kept 1 coarse grid entries removed
c
c len              number of nonzeros in column/row k of L/U
c
c Lmax,Umax        maximum element of |L(k:n,k)*U(k,k)| and |U(k,k:n)|
c 
c x,xp,xm,mup,mum  used for inverse-based dropping
c
c aldlk             |D(k,k)| 
c loadrow          integer if row k and row k+1 or only row k+1has to be 
c                  loaded in the next elimination step.
c                  Depending on whether a 1x1 or a 2x2 update has been 
c                  performed in the previous step we need to load row
c                  k or rows k and k+1 

      lfil =lfilv(1)
      lfils=lfilv(2)

      if (lfil.lt.0) goto 994

c     initial matrix unshifted
      shiftA=0

c     drop tolerances
      droptol=droptols(1)
      droptol2=droptols(2)/condest
c     keep track on the largest norm of ||L^{-1}||,||U^{-1}||
      maxcondest=RONE

c     abbreviations
      n2 =n  +n
      n3 =n2 +n
      n4 =n3 +n
      n5 =n4 +n
      n6 =n5 +n
      n7 =n6 +n
      n8 =n7 +n
      n9 =n8 +n
      n10=n9 +n


c     bit 0
c     which kind of dropping do we use
c     0 standard, 1 inverse based
      i=param
      droptype=mod(i,2)
c
c     bit 1
      i=i/2
c
c     bit 2
c     which kind of Schur complement do we use
c     0 simple, 1 Tismenetsky-like
      i=i/2
      sctype=mod(i,2)

c     bit 3
      i=i/2
c     bit 4
c     is an improved estimate desired?
c     0 no, 1 yes
      i=i/2
      improved=mod(i,2)

c     bit 5
c     is a diagonal compensation desired
c     0 no, 1 yes
      i=i/2
      milu=mod(i,2)

c     bit  6
c     should the ILU be tailored to non-skipped part
c     0 no, 1 yes
      i=i/2
      tailor=mod(i,2)

c     bit  7: FINAL_PIVOTING unused
      i=i/2

c     bit  8: ENSURE_SPD unused
      i=i/2

c     bit  9
c     should the remaining approximate Schur complement be computed
c     in a simple manner
c     0 no, 1 yes
      i=i/2
      simplesc=mod(i,2)

c     bit 10: PREPROCESS_INITIAL_SYSTEM unused
      i=i/2
c     bit 11: PREPROCESS_SUBSYSTEMS unused
      i=i/2
c     bit 12: MULTI_PILUC unused
      i=i/2
c     bit 13: RE_FACTOR unused
      i=i/2

c     bit 14: AGGRESSIVE_DROPPING
      i=i/2
      aggressive=mod(i,2)
c      aggressive=0

c     bit 15: DISCARD_MATRIX
      i=i/2
      discardA=mod(i,2)


c     memory layout for the vectors of indices that handle skipped parts
c     of the incomplete LU factorization
c     if bit 2 of param IS set:
c     storage positions for two subsequent columns
      njw1(1)=0
      njw2(1)=n
      if (sctype.gt.0) then
         nb=n7
         na=n9                 
         nc=11*n                 
         nl=n5                 
         el=12*n                 
         ne=13*n
         njw1(2)=14*n
         njw2(2)=15*n
c     if bit 2 of param is NOT set:
      else
         nb=n7
         na=n9                 
         nc=n8
         nl=n5  
         ne=n10
         njw1(2)=11*n
         njw2(2)=12*n
      end if

c     buffer for the numerical values of the second column
      if (improved.gt.0) then 
         nw=n3
      else
         nw=n2
      end if
        

c     ensure that the entries of A in any row are sorted in 
c     increasing order
      do i=1,n
         j=ia(i)
         k=ia(i+1)-j
c        check if row i is empty. No problem, only negative k is illegal!
         if (k.gt.0) then
            call QQSORT(a(j),ja(j),jw(n+1),k,invq)

c           column indices are out of range
            if (invq(ja(j)).lt.1 .or. invq(ja(j+k-1)).gt.n) 
     +         goto 991
         else if (k.lt.0) then
            goto 991
         end if 


c        also clear some space
c
c        clear space for the nonzero indicator arrays
         jw(i)        =0
         jw(njw1(2)+i)=0
c
c        clear Ulist 
         jw(n6+i)=0
c
c        clear counters for nonzero entries in any/column of A(p,q)
         jw(n8+i)=0
c
c        clear workspace for the numerical values
         w(i)   =FNULL
         w(nw+i)=FNULL

c        clear array that indicates whether a row of U appears in pairs
         jldl(n+1+i)=0

c        clear arrays for skipped parts of the incomplete LU factorization
         jw(na+i)=0
         jw(nc+i)=0
         jw(nl+i)=0
      end do

c     clear estimates for the norm of the inverse
      do i=1,n
c        clear arrays to hold the estimates for the inverses
         w(n+i)=FNULL
c        init counter arrays
         jw(ne+i)=0
      end do
      if (improved.gt.0) then
         do i=1,n
            w(n2+i)=FNULL
         end do
      end if
c
c     clear workspace for Tismenetsky-like update
      if (sctype.gt.0) then
         do i=1,n
c           clear Ulist (epsilon size part)
            jw(n10+i)=0

c           clear arrays for skipped parts of the incomplete LU factorization
            jw(el+i)=0
         end do
      end if

c     set up arrays for A^T (A is stored by rows)
c     This is even necessary in the case of a symmetric matrix, since
c     only half of the matrix is stored and in addition the matrix is 
c     reordered by a symmetric perumtation which destroys the original
c     triangular structure
c
c     jw(2n+1:3n)  head of the linked list for the columns of A
c     jw(3n+1:4n)  linked list for the leading columns of A up to step k
c     jw(4n+1:5n)  pointer to the first entry at step k
      do k=1,n
c        clear head of the linked list (empty) 
         jw(n2+k)=0
      end do
      do i=1,n
c        pointer to the first nonzero element in row p(i)
         j=ia(p(i))
c        we have to make sure that we are still inside row p(i)
         if (j.lt.ia(p(i)+1)) then
c           first column index k of  A(p(i),q(k))
            k=invq(ja(j))
c           pointer to the current first nonzero entry in row i
            jw(n4+i)=j
c           add new entry to the head of the list
            jw(n3+i)=jw(n2+k)
            jw(n2+k)=i
         end if
      end do


c     extract diagonal part of A(p,q) and store it in aldl(1:n)
      maxiwk=0
      if (iwk.lt.n2+1) goto 992
      maxiwk=max(maxiwk,n2+1)
      do i=1,n
         aldl(i)=FNULL
         aldl(n+1+i)=FNULL
         k=ia(p(i))
         l=ia(p(i)+1)
c        while k<l and invq(ja(k))<i
 90         if (k.ge.l .or. invq(ja(k)).ge.i) goto 100
            k=k+1
         goto 90
c        end while
 100     if (k.lt.l .and. invq(ja(k)).eq.i) then
            aldl(i)=a(k)
         end if
      end do


c     initial values for jldl, 
c      o  jldl(1:n+1) are used as pointers to point to the U part, 
c      o  jldl(n+2:2n+1) are left with respect to the numerical
c         values (tridiagonal part) so 
c      o  the indices start at 2n+2
      jldl(1)=n2+2
      if (iwk.lt.n2+2) goto 992
      maxiwk=max(maxiwk,n2+2)

#ifdef PRINT_INFO
      write (6,'(A)') 'permutation vector'
      write (6,'(20I4)') (p(i), i=1,n)
      write (6,'(A)') 'input diagonal part'
      do k=1,n
         do i=ia(k),ia(k+1)-1
            if (ja(i).eq.k) write 
     +           (6,'(A,I3,A,I3,A,1P,E20.12,A,1P,E20.12,A)')
     +           'A(',k,',',k,')=',
     +           dreal(a(i)),'+i*',dimag(a(i)),';'
         end do
      end do
      write (6,'(A)') 'input matrix'
      do k=1,n
         do i=ia(k),ia(k+1)-1
            write (6,'(A,I3,A,I3,A,1P,E20.12,A,1P,E20.12,A)')
     +           'A(',k,',',ja(i),')=',
     +           dreal(a(i)),'+i*',dimag(a(i)),';'
         end do
      end do
#endif


      
      nskipped=0
c     main loop
      k=1
c     initially we need to load 2 subsequent rows
      loadrow=1
      finalrow=2
      if (nLU.le.1) finalrow=1
      rlen(1)=0
      rlen(2)=0
         
c     while k<=nLU
 110     if (k.gt.nLU) goto 111
       
#if defined PRINT_INFO3 || defined PRINT_INFO2
         write (6, '(A,I6)') 'step ',k
#endif

c        initially, do not skip
         skip=.false.


c        -----   dealing with the upper triangular part   -----
c
c        compute rows k,k+1 whenever necessary
         do kk=loadrow,finalrow
#if defined PRINT_INFO || defined PRINT_INFO2
            write (6, '(A,I4)') 'kk: ',kk
#endif
            mode=kk-1
c           extract skipped entries of column k (in order to complete
c           the data that is not stored in row k)
            rlen(kk)=0

c           extract strict lower triangular and skipped part of A, column k
c           if A(p(k+1:n),q(k)) is nonempty, get the entry point to the
c           linked list of column k
            i=jw(n2+k)
c           while i>0         
 120           if (i.le.0) goto 130

c              pointer to A(p(i),q(k+mode))
               j=jw(n4+i)+mode
c              store next row from the linked list
               m=jw(n3+i)
c              is the entry still within row p(i)
               if (j.lt.ia(p(i)+1)) then
c                 column index l of  A(p(i),q(l))
                  l=invq(ja(j))

c                 decide whether this entry has to be stored or not
c                 if (l=k+mode and i>=k and i<>k+mode) or entry skipped
                  if (l.eq.k+mode .and. ((i.ge.k .and. i.ne.k+mode)
     +                 .or. jw(nc+i).gt.0)) then 
                     rlen(kk)=rlen(kk)+1
c                    flag component i as nonzero
                     jw(njw1(kk)+i)=rlen(kk)
c                    add i to the list of nonzeros
                     jw(njw2(kk)+rlen(kk))=i
c                    extract numerical value
                     w(nw*mode+i)=CONJUG(SKEW(a(j)))
                  end if
               end if

c              recover next row entry
               i=m
            goto 120
c           end while

#ifdef PRINT_INFO2
 130        write (6, '(A,I4)') 'row ',k+mode
            write (6, '(10I8)')      (jw(njw2(kk)+l),   l=1,rlen(kk))
            write (6, '(1P,10E8.1)') 
     +            (dreal(w(nw*mode+jw(njw2(kk)+l))),l=1,rlen(kk))
            write (6, '(1P,10E8.1)') 
     +            (dimag(w(nw*mode+jw(njw2(kk)+l))),l=1,rlen(kk))
#endif


c           if column k+1 is loaded then the linked list is incomplete.
c           first one has to scan again the linked list of column k (previous
c           loop) and after that the linked list of column k+1
#ifdef PRINT_INFO2
            if (mode.eq.1) then
#else
 130        if (mode.eq.1) then
#endif
c              extract strict lower triangular and skipped part of A, column k+1
c              if A(p(k:n),q(k+1)) is nonempty, get the entry point to the
c              linked list of column k+1
               i=jw(n2+k+1)
c              while i>0         
 121           if (i.le.0) goto 131

c                 pointer to A(p(i),q(k+1))
                  j=jw(n4+i)
c                 store next row from the linked list
                  m=jw(n3+i)
c                 is the entry still within row p(i)
                  if (j.lt.ia(p(i)+1)) then
c                    column index l of  A(p(i),q(l))
                     l=invq(ja(j))

c                    decide whether this entry has to be stored or not
c                    if (l=k+1 and i>=k and i<>k+1) or entry skipped
                     if (l.eq.k+1 .and. ((i.ge.k .and. i.ne.k+1) 
     +                    .or. jw(nc+i).gt.0)) then 
                        rlen(2)=rlen(2)+1
c                       flag component i as nonzero
                        jw(njw1(2)+i)=rlen(2)
c                       add i to the list of nonzeros
                        jw(njw2(2)+rlen(2))=i
c                       extract numerical value
                        w(nw+i)=CONJUG(SKEW(a(j)))
                     end if
                  end if

c                 recover next row entry
                  i=m
               goto 121
c              end while

#ifdef PRINT_INFO2
 131           write (6, '(A,I4)') 'row ',k+mode
               write (6, '(10I8)')      (jw(njw2(kk)+l),   l=1,rlen(kk))
               write (6, '(1P,10E8.1)') 
     +               (dreal(w(nw*mode+jw(njw2(kk)+l))),l=1,rlen(kk))
               write (6, '(1P,10E8.1)') 
     +               (dimag(w(nw*mode+jw(njw2(kk)+l))),l=1,rlen(kk))
#endif


#ifdef PRINT_INFO2
            end if
#else
 131           j=j
            end if
#endif
c           extract strict upper triangular and skipped part of A, row k+mode
            do l=ia(p(k+mode)),ia(p(k+mode)+1)-1
c              column index i of A(p(k+mode),q(i))
               i=invq(ja(l))
c              extract upper triangular part of A(p,q), row k+mode
c              in addition, take care for the skipped part
               if (jw(nc+i).gt.0 .or. i.ge.k) then
                  rlen(kk)=rlen(kk)+1
c                 flag component i as nonzero
                  jw(njw1(kk)+i)=rlen(kk)
c                 add i to the list of nonzeros
                  jw(njw2(kk)+rlen(kk))=i
c                 extract numerical value
                  w(nw*mode+i)=a(l)
               end if
            end do

#ifdef PRINT_INFO2
            write (6, '(A,I4)') 'row ',k+mode
            write (6, '(10I8)')      (jw(njw2(kk)+l),   l=1,rlen(kk))
            write (6, '(1P,10E8.1)') 
     +            (dreal(w(nw*mode+jw(njw2(kk)+l))),l=1,rlen(kk))
            write (6, '(1P,10E8.1)') 
     +            (dimag(w(nw*mode+jw(njw2(kk)+l))),l=1,rlen(kk))
#endif

c           manually add zero diagonal entry if necessary
            if (jw(njw1(kk)+k+mode).eq.0) then
               rlen(kk)=rlen(kk)+1
c              flag component i as nonzero
               jw(njw1(kk)+k+mode)=rlen(kk)
c              add i to the list of nonzeros
               jw(njw2(kk)+rlen(kk))=k+mode
c              extract numerical value
               w(nw*mode+k+mode)=FNULL
            end if



c           update row k of the Schur complement by subtracting any
c           previous row i=1,...,k-1 depending on the nonzero entries
c           of U(1:k-1,k)
c
            if (sctype.gt.0) then
c              additional Tismenetsky updates using regular previous 
c              rows of U but epsilon size elements of U
c           
c              n              size of the problem
c              k              current step of the update
c              aldl           numerical values
c              jldl           associated indices
c              jldl(2)        pointers to the first space behind any
c                             row of U (behind the epsilon size entries)
c              jw(nb+1)       pointers to the first space behind any
c                             row of U (behind the regular entries)
c              jw(n10+1)      Ulist  for the epsilon size part of U
c              jw(n8+1)       Ufirst for the epsilon size part of U
c              jw(na+1)       Ufirst for the regular part of U
c              jw(njw1(kk)+1) indices of nonzero entries and 
c              jw(njw2(kk)+1) associated list 
c              rlen(kk)       length of this list
c              w(nw*mode+1)   numerical values
c              jldl           pointers to the start of the U-part
c              jw(nl+1)       number of skipped entries, located at the
c                             start of every row, regular part
               call MYSYMPILUCUPDATE(n,k,aldl,jldl,jldl(2),
     +                             jw(nb+1),jw(n10+1),jw(n8+1),jw(na+1),
     +                             jw(njw1(kk)+1),jw(njw2(kk)+1),
     +                             rlen(kk),w(nw*mode+1),jldl,jw(nl+1),
     +                             mode)


c              additional Tismenetsky updates using epsilon size previous
c              rows of U but regular elements of U
c           
c              n              size of the problem
c              k              current step of the update
c              aldl           numerical values
c              jldl           associated indices
c              jw(nb+1)       pointers to the first space behind any
c                             row of U (behind the regular entries)
c              jldl(2)        pointers to the first space behind any
c                             row of U (behind the epsilon size entries)
c              jw(n6+1)       Ulist  for the regular part of U
c              jw(na+1)       Ufirst for the regular part of U
c              jw(n8+1)       Ufirst for the epsilon size part of U
c              jw(njw1(kk)+1) indices of nonzero entries and 
c              jw(njw2(kk)+1) associated list 
c              rlen(kk)       length of this list
c              w(nw*mode+1)   numerical values
c              jw(nb+1)       pointers to the start of the epsilon size
c                             part of U
c              jw(el+1)       number of skipped (epsilon size) entries,
c                             located at the start of every row
               call MYSYMPILUCUPDATE(n,k,aldl,jldl,jw(nb+1),
     +                             jldl(2),jw(n6+1),jw(na+1),jw(n8+1),  
     +                             jw(njw1(kk)+1),jw(njw2(kk)+1),
     +                             rlen(kk),w(nw*mode+1),jw(nb+1),
     +                             jw(el+1),mode)
            end if
         
c           regular update 
c           n              size of the problem
c           k              current step of the update
c           aldl           numerical values
c           jldl           associated indices
c           jw(nb+1)       pointers to the first space behind any
c                          row of U (behind the regular entries)
c           jw(nb+1)       pointers to the first space behind any
c                          row of U (behind the regular entries)
c           jw(n6+1)       Ulist  for the regular part of U
c           jw(na+1)       Ufirst for the regular part of U
c           jw(na+1)       Ufirst for the regular part of U
c           jw(njw1(kk)+1) indices of nonzero entries and 
c           jw(njw2(kk)+1) associated list 
c           rlen(kk)       length of this list
c           w(nw*mode+1)   numerical values
c           jldl           pointers to the start of the U-part
c           jw(nl+1)       number of skipped entries, located at the
c                          start of every column
            call MYSYMPILUCUPDATE(n,k,aldl,jldl,jw(nb+1),
     +                          jw(nb+1),jw(n6+1),jw(na+1),jw(na+1),
     +                          jw(njw1(kk)+1),jw(njw2(kk)+1),
     +                          rlen(kk),w(nw*mode+1),jldl,
     +                          jw(nl+1),mode)

#ifdef PRINT_INFO2
            write (6, '(A,I4)') 'row ',k+mode
            write (6, '(10I8)')      (jw(njw2(kk)+l),   l=1,rlen(kk))
            write (6, '(1P,10E8.1)') 
     +            (dreal(w(nw*mode+jw(njw2(kk)+l))),l=1,rlen(kk))
            write (6, '(1P,10E8.1)') 
     +            (dimag(w(nw*mode+jw(njw2(kk)+l))),l=1,rlen(kk))
#endif

c           remove diagonal entry
c           position of the diagonal entry in the list
            l=jw(njw1(kk)+k+mode)
            if (l.ne.0) then
c              shuffle last entry in the list to the position of the 
c              diagonal entry
               i=jw(njw2(kk)+rlen(kk))
               jw(njw2(kk)+l)=i
               jw(njw1(kk)+i)=l
               rlen(kk)=rlen(kk)-1
c              clear diagonal entry
               jw(njw1(kk)+k+mode)=0
               w(nw*mode+k+mode)=FNULL
            end if

#ifdef PRINT_INFO2
            write (6, '(A,I4)') 'row ',k+mode
            write (6, '(10I8)')      (jw(njw2(kk)+l),   l=1,rlen(kk))
            write (6, '(1P,10E8.1)') 
     +            (dreal(w(nw*mode+jw(njw2(kk)+l))),l=1,rlen(kk))
            write (6, '(1P,10E8.1)') 
     +            (dimag(w(nw*mode+jw(njw2(kk)+l))),l=1,rlen(kk))
#endif

         end do
c        end computation of rows (k,)k+1

#ifdef PRINT_INFO2
         write (6, '(A,I4)') 'row ',k
         write (6, '(10I8)')      (jw(njw2(1)+l),   l=1,rlen(1))
         write (6, '(1P,10E8.1)') (dreal(w(jw(njw2(1)+l))),l=1,rlen(1))
         write (6, '(1P,10E8.1)') (dimag(w(jw(njw2(1)+l))),l=1,rlen(1))
         if (k.lt.nLU) then
            write (6, '(A,I4)') 'row ',k+1
            write (6, '(10I8)')      (jw(njw2(2)+l),      l=1,rlen(2))
            write (6, '(1P,10E8.1)')
     +           (dreal(w(nw+jw(njw2(2)+l))),l=1,rlen(2))
            write (6, '(1P,10E8.1)')
     +           (dimag(w(nw+jw(njw2(2)+l))),l=1,rlen(2))
         end if
#endif
#ifdef PRINT_INFO
         write (6, '(A,I4)') 'row ',k
         write (6, '(10I8)')      (jw(njw2(1)+l),   l=1,rlen(1))
         write (6, '(1P,10E8.1)') (w(jw(njw2(1)+l)),l=1,rlen(1))
         if (k.lt.nLU) then
            write (6, '(A,I4)') 'row ',k+1
            write (6, '(10I8)')      (jw(njw2(2)+l),      l=1,rlen(2))
            write (6, '(1P,10E8.1)')
     +           (w(nw+jw(njw2(2)+l)),l=1,rlen(2))
         end if
#endif

c        decide whether to use a 1x1 or a 2x2 pivot
         Umax=ABS(aldl(k))

#ifdef PRINT_INFO2
         write (6, '(A,1P,E8.1,A,1P,E8.1)') 'diagonal entry ',
     +        dreal(aldl(k)),'+i*',dimag(aldl(k))
         if (k.lt.nLU) then
            write (6, '(A,1P,E8.1,A,1P,E8.1)')
     +    'diagonal entry ',dreal(aldl(k+1)),'+i*',dimag(aldl(k+1))
         end if
#endif
#ifdef PRINT_INFO
         write (6, '(A,1P,E8.1)') 'diagonal entry ',
     +        aldl(k)
         if (k.lt.nLU) then
            write (6, '(A,1P,E8.1)')
     +    'diagonal entry ',aldl(k+1)
         end if
#endif

c        check whether A(k,k+1)<>0
         l=jw(njw1(2)+k)
c        absolute value of the determinant of the 2x2 block
c        not the tentative final 1x1 step
         if (finalrow.ne.1) then
c           A(k,k+1)
            xp=w(nw+k)
c           A(k+1,k)
            xm=CONJUG(SKEW(xp))
            ialdlk=aldl(k)*aldl(k+1)-xm*xp
            DUmax=ABS(ialdlk)
         else
            DUmax=RZERO
            xp=FNULL
            l=0
         end if
c        zero diagonal entry or singular block
         if (DUmax.eq.RZERO .and. Umax.eq.RZERO) then
c           skip row/column k
            skip=.true.
            mode=0

#if defined PRINT_INFO3 || defined PRINT_INFO2
            write (6, '(A)') '1. skipped '
#endif

         else
c           check diagonal dominance

c           1x1 case
            if (Umax.ne.RZERO) then
               nup=RZERO
               mup=RZERO
               axp=condest*Umax*0.25d0
               do j=1,rlen(1)
                  l=jw(njw2(1)+j)
                  ax=ABS(w(l))
c                 maximum entry in absolute value
                  mup=max(mup,ax)
c                 accumulate significant nonzero entries
                  if (ax.gt.axp) nup=nup+ax
               end do
               mup=mup/Umax
            end if

c           2x2 case
c           2x2 block is not diagonal
cc new
            if (l.ne.0 .and. DUmax.ne.RZERO  .and. ABS(xp).gt.
     +          1e-1*min(ABS(aldl(k)),ABS(aldl(k+1)))) then
cc old            if (l.ne.0 .and. DUmax.ne.RZERO) then
               num=RZERO
               mum=RZERO
               axp=condest*DUmax*0.25
               do j=1,rlen(1)
                  i=jw(njw2(1)+j)
                  if (i.ne.k+1) then
c                    maximum entry in absolute value
c                    ||[A(k,k)   A(k,k+1)  ]^{-1}*U(k:k+1,i)||
c                    ||[A(k+1,k) A(k+1,k+1)]                ||
                     if (jw(njw1(2)+i).ne.0) then
                        ax=ABS( aldl(k+1)*w(i)-xp       *w(nw+i))
     +                    +ABS(-xm       *w(i)+aldl(k)  *w(nw+i))
                     else
                        ax=ABS(aldl(k+1) *w(i))
     &                    +ABS(-xm       *w(i))
                     end if
                     mum=max(mum,ax)
c                    accumulate significant nonzero entries
                     if (ax.gt.axp) num=num+ax
                  end if
               end do
               do j=1,rlen(2)
                  i=jw(njw2(2)+j)
                  if (i.ne.k) then
c                    maximum entry in absolute value
c                    ||[A(k,k)   A(k,k+1)  ]^{-1}*U(k:k+1,i)||
c                    ||[A(k+1,k) A(k+1,k+1)]                ||
                     if (jw(njw1(1)+i).ne.0) then
                        ax=ABS( aldl(k+1)*w(i)-xp       *w(nw+i))
     +                    +ABS(-xm       *w(i)+aldl(k)  *w(nw+i))
                     else
                        ax=ABS(-xp       *w(nw+i))
     +                    +ABS( aldl(k)  *w(nw+i))
                     end if
                     mum=max(mum,ax)
c                    accumulate significant nonzero entries
                     if (ax.gt.axp) num=num+ax
                  end if
               end do
               mum=mum/DUmax
            else
               mum=2.0*(mup+1.0)
            end if

c           when A(k,k)=0 we formally have to set mup to 
c           something significantly larger than  mum
            if (Umax.eq.RZERO) mup=10.0*(mum+1.0)
         end if


c        use a 2x2 update only if it is more diagonal dominant
c        than the 1x1 update would be
         if (.not.skip) then

#if defined PRINT_INFO || defined PRINT_INFO2
            if (k.lt.nLU) then
               write (6, '(A,1P,E8.1,1P,E8.1)') 'diag. dom.',mup,mum
            else
               write (6, '(A,1P,E8.1)') 'diag. dom.',mup
            end if
#endif

c           compare diagonal dominance of the L-factor
c           prefer a 1x1 update
            ax=max(ABS(aldl(k)),ABS(aldl(k+1)))
            ax=max(ax,ABS(xp))
            if (mup.le.mum) then
               mode=0
               Umax=mup
               mup=nup

#if defined PRINT_INFO3 || defined PRINT_INFO2
               write (6, '(A)') '1x1 update'
#endif

c           (unfortunately) a 2x2 update is recommended
            else
               mode=1
               Umax=mum
               mup=num

#if defined PRINT_INFO3 || defined PRINT_INFO2
               write (6, '(A)') '2x2 update'
#endif

            end if 
         end if

         aldlk=aldl(k)

c        check for zero pivot or |U(k,k+1:n)| with several large components
         if (mup.gt.RFOUR*axp) then
c           skip row/column k
            skip=.true.
            mode=0

#if defined PRINT_INFO3 || defined PRINT_INFO2
            write (6, '(A)') '2. skipped '
#endif

         end if

         if (skip) then
            do j=1,rlen(1)
c              extract column index l from the list of nonzeros
               l=jw(njw2(1)+j)
               w(l)=FNULL
c              clear flag
               jw(njw1(1)+l)=0
            end do
         end if

c        invert diagonal entry/determinant
         if (mode.eq.0 .and. .not.skip) then
            ialdlk=FONE/aldlk
         else if (.not.skip) then
c           ialdlk is the determinant
            ialdlk=FONE/ialdlk
c           [A(k,k)   A(k,k+1)  ]^{-1}
c           [A(k+1,k) A(k+1,k+1)]
            U11= aldl(k+1)         *ialdlk
            U12=-CONJUG(w(nw+k))
c           store -A(k,k+1)
            aldl(n+1+k)=U12
            U21= CONJUG(SKEW(U12))*ialdlk
            U12= U12              *ialdlk
            U22= aldl(k)          *ialdlk
         end if

c        for several estimates we still need |D(k,k)|
         absaldlk=ABS(aldlk)

c        position where the next row of U starts
c        1x1 update
         if (mode.eq.0 .and. .not.skip) then
            jldl(k+1)=jldl(k)+rlen(1)
            if (jldl(k+1)-1.gt.iwk .and.
     +          aggressive.gt.0) then 
c              apply aggressive dropping to U
c              n              size of the problem
c              k-1            current finished step of the update
c              aldl           numerical values
c              jldl           associated indices
c              jw(nb+1)       pointers to the first space behind any
c                             row of U (behind the regular entries)
c              jw(na+1)       Ufirst for the regular part of U
c              jw(n8+1)       Ufirst for the epsilon size part of U
c              jw(nl+1)       number of skipped entries, located at the
c                             start of every column
c              jw(el+1)       number of skipped (epsilon size) entries,
c                             located at the start of every row
c              condest        inverse bound
c              droptols(1)    
c              sctype         indicate whether Tismenetsky update is used
               call AGGRESSIVEDROPPING(n,k-1,aldl,jldl,jw(nb+1),
     +                                 jw(na+1),jw(n8+1),jw(nl+1),
     +                                 jw(el+1),condest,droptols(1),
     +                                 sctype)
               jldl(k+1)=jldl(k)+rlen(1)
               if (jldl(k+1)-1.gt.iwk) goto 992
            else if (jldl(k+1)-1.gt.iwk) then
               goto 992
            end if
            maxiwk=max(maxiwk,jldl(k+1)-1)
c           copy buffer to jldl/aldl
            j=1
            do i=jldl(k),jldl(k+1)-1
c              extract column index l from the list of nonzeros
               l=jw(njw2(1)+j)
               jldl(i)=l
c              extract numerical value
               aldl(i)=w(l)
               w(l)=FNULL
c              clear flag
               jw(njw1(1)+l)=0
               j=j+1
            end do

c        2x2 update
         else if (.not.skip) then
            if (jldl(k)+2*rlen(1)+2*rlen(2)-1.gt.iwk .and.
     +          aggressive.gt.0) then 
c              apply aggressive dropping to U
c              n              size of the problem
c              k-1            current finished step of the update
c              aldl           numerical values
c              jldl           associated indices
c              jw(nb+1)       pointers to the first space behind any
c                             row of U (behind the regular entries)
c              jw(na+1)       Ufirst for the regular part of U
c              jw(n8+1)       Ufirst for the epsilon size part of U
c              jw(nl+1)       number of skipped entries, located at the
c                             start of every column
c              jw(el+1)       number of skipped (epsilon size) entries,
c                             located at the start of every row
c              condest        inverse bound
c              droptols(1)    
c              sctype         indicate whether Tismenetsky update is used
               call AGGRESSIVEDROPPING(n,k-1,aldl,jldl,jw(nb+1),
     +                                 jw(na+1),jw(n8+1),jw(nl+1),
     +                                 jw(el+1),condest,droptols(1),
     +                                 sctype)
               if (jldl(k)+2*rlen(1)+2*rlen(2)-1.gt.iwk) goto 992
            else if (jldl(k)+2*rlen(1)+2*rlen(2)-1.gt.iwk) then 
               goto 992
            end if
            maxiwk=max(maxiwk,jldl(k)+2*rlen(1)+2*rlen(2)-1)

c           extract column/row k
c           start of column/row k
            i=jldl(k)
            ii=i
            do j=1,rlen(1)
c              extract column index l from the list of nonzeros
               l=jw(njw2(1)+j)
c              make sure that L(k+1,k) is not stored 
               if (l.ne.k+1) then
                  jldl(i)=l
c                 extract numerical values L(l,k:k+1)
c                 L(l,k)
                  aldl(ii)  =w(l)
c                 L(l,k+1) might be zero, don't care!
                  aldl(ii+1)=w(nw+l)
                  i=i+1
                  ii=ii+2
               end if
            end do

c           extract additional entries in column/row k+1
            do j=1,rlen(2)
c              extract column index l from the list of nonzeros
               l=jw(njw2(2)+j)
c              make sure that L(k,k+1) is not stored 
c              also ensure that the entries have not already been
c              stored in the first pass
               if (l.ne.k .and. jw(njw1(1)+l).eq.0) then
                  jldl(i)=l
c                 extract numerical values L(l,k:k+1)
c                 L(l,k) might be zero, don't care!
                  aldl(ii)  =w(l)
c                 L(l,k+1)
                  aldl(ii+1)=w(nw+l)
                  i=i+1
                  ii=ii+2
               end if
c              do NOT clear w(nw), jw(njw1(2)), jw(njw2(2))
c              at this point! If it turns out that the 2x2 update
c              is skipped due to stability reasons, then we have
c              to shift this column to prepare the next update step
            end do
            do j=1,rlen(1)
               l=jw(njw2(1)+j)
               w(l)=FNULL
c              clear flag
               jw(njw1(1)+l)=0
            end do
c           new positions of indices/numerical values
            jldl(k+1)=i
            jldl(k+2)=ii
         end if
         
         
c        estimate norm of the columns of U^{-1}
         if (.not. skip) then
            signum =FONE
            signum2=FONE
#if !defined _DOUBLE_REAL_ && !defined _SINGLE_REAL_
c           1x1 update 
            if (mode.eq.0) then
               mup=RZERO
               j=jldl(k)
               do i=jldl(k),jldl(k+1)-1
c                 column index l
                  l=jldl(i)
                  xp=aldl(i)*ialdlk
                  xm=w(n+l)-w(n+k)*xp
                  axp=ABS(xm)+ABS(xp)
                  if (axp.gt.mup) then 
                     mup=axp
                     j=i
                  end if
               end do
c              choose the sign such that component l is maximized
               if (jldl(k).lt.jldl(k+1)) then
                  l=jldl(j)
                  xp=aldl(j)*ialdlk
                  xm=w(n+l)-w(n+k)*xp
                  if (xp.ne.FNULL .and. 
     +                xm.ne.FNULL) then
                     signum=(xm/ABS(xm))*(CONJ(xp)/ABS(xp))
                  end if
               end if
            else
c              2x2 update 


c              signs, column k:k+1
               mup=RZERO
               mum=RZERO
               j=jldl(k)
               jj=j
               ii=j
               do i=jldl(k),jldl(k+1)-1
c                 column index l
                  l=jldl(i)
c                 first row of 
c                 [A(k,k)   A(k,k+1)  ]^{-1}*U(k:k+1,l)
c                 [A(k+1,k) A(k+1,k+1)]
                  xp=U11*aldl(ii)+U12*aldl(ii+1)
                  xm=w(n+l)-w(n+k)*xp
                  axp=ABS(xm)+ABS(xp)
                  if (axp.gt.mup) then 
                     mup=axp
                     j=i
                  end if
c                 second row of 
c                 [A(k,k)   A(k,k+1)  ]^{-1}*U(k:k+1,l)
c                 [A(k+1,k) A(k+1,k+1)]
                  xp=U21*aldl(ii)+U22*aldl(ii+1)
                  xm=w(n+l)-w(n+k+1)*xp
                  axp=ABS(xm)+ABS(xp)
                  if (axp.gt.mum) then 
                     mum=axp
                     jj=i
                  end if
                  ii=ii+2
               end do
               if (jldl(k).lt.jldl(k+1)) then
                  l=jldl(j)
                  ii=jldl(k)+2*(j-jldl(k))
c                 first row of 
c                 [A(k,k)   A(k,k+1)  ]^{-1}*U(k:k+1,l)
c                 [A(k+1,k) A(k+1,k+1)]
                  xp=U11*aldl(ii)+U12*aldl(ii+1)
                  xm=w(n+l)-w(n+k)*xp
                  if (xp.ne.FNULL .and.
     +                xm.ne.FNULL) then
                     signum=(xm/ABS(xm))*(CONJ(xp)/ABS(xp))
                  end if
                  l=jldl(jj)
                  ii=jldl(k)+2*(jj-jldl(k))
c                 second row of 
c                 [A(k,k)   A(k,k+1)  ]^{-1}*U(k:k+1,l)
c                 [A(k+1,k) A(k+1,k+1)]
                  xp=U21*aldl(ii)+U22*aldl(ii+1)
                  xm=w(n+l)-w(n+k+1)*xp
                  if (xp.ne.FNULL .and.
     +                xm.ne.FNULL) then
                     signum2=(xm/ABS(xm))*(CONJ(xp)/ABS(xp))
                  end if
               end if
            end if
#else
#endif

c           1x1 update
            if (mode.eq.0) then
c              try +/-SIGNUM as k-th component of the right hand side
               xp=( SIGNUM-w(n+k))*ialdlk
               xm=(-SIGNUM-w(n+k))*ialdlk
c              for both cases estimate the 1-norm of U^{-1} by evaluating
c              U(1:k,k+1:n)^T*U(1:k,1:k)^{-T}*rhs(1:k)
               mup=RZERO 
               mum=RZERO
               nup=RZERO
               num=RZERO
               do i=jldl(k),jldl(k+1)-1
c                 row index l
                  l=jldl(i)
                  ax=ABS(w(n+l)+xp*aldl(i))
                  nup=max(nup,ax)
                  mup=mup+ax
                  ax=ABS(w(n+l)+xm*aldl(i))
                  num=max(num,ax)
                  mum=mum+ax
               end do
               if (mup.ge.mum) then
                  x=xp
                  axp=ABS( SIGNUM-w(n+k))
               else
                  x=xm
                  axp=ABS(-SIGNUM-w(n+k))
                  nup=num
               end if
c              use a buffer to store x
c              if the improved estimate turns out to skip entry k although
c              the first estimate has not indicated this, then we have to
c              'undo' the update
               buffer=-x
c              does the norm estimate exceed the prescribed bound? 

#if defined PRINT_INFO3 || defined PRINT_INFO2
               if (max(nup,axp).gt.condest) 
     +            write (6, '(A)') '3. skipped '
#endif

               if (max(nup,axp).gt.condest) skip=.true.

            else
c              2x2 update
c              update using columns k,k+1
c              try +/-SIGNUM, and +/-SIGNUM2 as k-th and (k+1)-th component
c              of the right hand side

c              [ SIGNUM-w(n+k),   SIGNUM2-w(n+k+1)] * [A(k,k)   A(k,k+1)  ]^{-1}
c                                                     [A(k+1,k) A(k+1,k+1)]     
               xp =( SIGNUM-w(n+k))*U11+( SIGNUM2-w(n+k+1))*U21
               xp2=( SIGNUM-w(n+k))*U12+( SIGNUM2-w(n+k+1))*U22

c              [ SIGNUM -w(n+k), -SIGNUM2-w(n+k+1)] * [A(k,k)   A(k,k+1)  ]^{-1}
c                                                     [A(k+1,k) A(k+1,k+1)]     
               xp3=( SIGNUM-w(n+k))*U11+(-SIGNUM2-w(n+k+1))*U21
               xp4=( SIGNUM-w(n+k))*U12+(-SIGNUM2-w(n+k+1))*U22

c              [-SIGNUM -w(n+k), -SIGNUM2-w(n+k+1)] * [A(k,k)   A(k,k+1)  ]^{-1}
c                                                     [A(k+1,k) A(k+1,k+1)]     
               xm =(-SIGNUM-w(n+k))*U11+(-SIGNUM2-w(n+k+1))*U21
               xm2=(-SIGNUM-w(n+k))*U12+(-SIGNUM2-w(n+k+1))*U22

c              [-SIGNUM -w(n+k),  SIGNUM2-w(n+k+1)] * [A(k,k)   A(k,k+1)  ]^{-1}
c                                                     [A(k+1,k) A(k+1,k+1)]     
               xm3=(-SIGNUM-w(n+k))*U11+( SIGNUM2-w(n+k+1))*U21
               xm4=(-SIGNUM-w(n+k))*U12+( SIGNUM2-w(n+k+1))*U22

c              for both cases estimate the 1-norm of U^{-1} by evaluating
c              U(1:k+1,k+2:n)^T*U(1:k+1,1:k+1)^{-T}*rhs(1:k+1)
               mup=RZERO 
               mum=RZERO
               nup=RZERO
               num=RZERO
               mup2=RZERO 
               mum2=RZERO
               nup2=RZERO
               num2=RZERO
               ii=jldl(k)
               do i=jldl(k),jldl(k+1)-1
c                 row index l
                  l=jldl(i)
c                 +1/+1
                  ax=ABS(w(n+l)+aldl(ii)*xp +aldl(ii+1)*xp2)
                  nup=max(nup,ax)
                  mup=mup+ax
c                 +1/-1
                  ax=ABS(w(n+l)+aldl(ii)*xp3+aldl(ii+1)*xp4)
                  nup2=max(nup2,ax)
                  mup2=mup2+ax
c                 -1/-1
                  ax=ABS(w(n+l)+aldl(ii)*xm +aldl(ii+1)*xm2)
                  num=max(num,ax)
                  mum=mum+ax
c                 -1/+1
                  ax=ABS(w(n+l)+aldl(ii)*xm3+aldl(ii+1)*xm4)
                  num2=max(num2,ax)
                  mum2=mum2+ax
                  ii=ii+2
               end do
               if (mup.ge.mum .and. mup.ge.mup2 .and. mup.ge.mum2) then
                  x =xp
                  x2=xp2
                  axp=max(ABS( SIGNUM -w(n+k)),
     +                    ABS( SIGNUM2-w(n+k+1)))  
               elseif (mup2.ge.mum.and.mup2.ge.mup.and.mup2.ge.mum2)then
                  x =xp3
                  x2=xp4
                  axp=max(ABS( SIGNUM -w(n+k)),
     +                    ABS(-SIGNUM2-w(n+k+1)))  
                  nup=nup2
               elseif (mum2.ge.mum.and.mum2.ge.mup.and.mum2.ge.mup2)then
                  x =xm3
                  x2=xm4
                  axp=max(ABS(-SIGNUM -w(n+k)),
     +                    ABS( SIGNUM2-w(n+k+1)))  
                  nup=num2
               else
                  x =xm
                  x2=xm2
                  axp=max(ABS(-SIGNUM -w(n+k)),
     +                    ABS(-SIGNUM2-w(n+k+1)))  
                  nup=num
               end if
c              use a buffer to store x,x2
c              if the improved estimate turns out to skip entry k although
c              the first estimate has not indicated this, then we have to
c              'undo' the update
               buffer =-x
               buffer2=-x2
c              does the norm estimate exceed the prescribed bound? 

#if defined PRINT_INFO3 || defined PRINT_INFO2
               if (max(nup,axp).gt.condest) 
     +            write (6, '(A)') '4. skipped '
#endif

               if (max(nup,axp).gt.condest) skip=.true.
            end if
         end if

c        1x1 update
         if (mode.eq.0) then
c           number of peaks in row k
            m=jw(ne+k)
c           will the diagonal entry become a peak of column k
c           this is the case if the average of peaks is at most 1
            if (ABS(w(n+k)).le.IREAL(m)) m=m+1
c           store new number of peaks
            jw(ne+k)=m
            if (.not. skip) then 
               do i=jldl(k),jldl(k+1)-1
                  l=jldl(i)
                  y=x*aldl(i)
c                 will we have a serious contribution to the rank-1
c                 update of U^{-1} in column l?
c                 the new average of peaks |y|/m caused by the rank-1
c                 update is at least as much as the current average of
c                 peaks |w(n+l)|/jw(ne+l) in row l
                  if (ABS(y)*IREAL(jw(ne+l)).ge.
     +                ABS(w(n+l))*IREAL(m))
     +                 jw(ne+l)=m
                  w(n+l)=w(n+l)+y
               end do
               axp=ABS( SIGNUM-w(n+k))
               axm=ABS(-SIGNUM-w(n+k))
               w(n+k)=axm
               if (axp.gt.axm) w(n+k)=axp
            else
               w(n+k)=FONE
            end if
            condest1=CTOD(w(n+k))

         else
c           2x2 update

c           number of peaks in row k,k+1
            m=jw(ne+k)
            j=jw(ne+k+1)
            jbuff=j
c           will the diagonal entry become a peak of column k,k+1
c           this is the case if the average of peaks is at most 1
            if (ABS(w(n+k))  .le.IREAL(m)) m=m+1
            wbuff=w(n+k+1)
            if (ABS(w(n+k+1)).le.IREAL(j) .and. .not.skip) j=j+1
c           store new number of peaks
            jw(ne+k)  =m
            if (.not.skip) jw(ne+k+1)=j
            if (.not.skip) then 
               ii=jldl(k)
               do i=jldl(k),jldl(k+1)-1
                  l=jldl(i)
                  xp=aldl(ii)*x
                  xm=aldl(ii+1)*x2
c                 will we have a serious contribution to the rank-2
c                 update of U^{-1} in column l?
c                 the new average of peaks |xp|/m caused by the rank-2
c                 update is at least as much as the current average of
c                 peaks |w(n+l)|/jw(ne+l) in row l
                  if (ABS(xp)*IREAL(jw(ne+l)).ge.
     +                ABS(w(n+l))*IREAL(m))
     +               jw(ne+l)=m
c                 the new average of peaks |xm|/j caused by the rank-2
c                 update is at least as much as the current average of
c                 peaks |w(n+l)|/jw(ne+l) in row l
                  if (ABS(xm)*IREAL(jw(ne+l)).ge.
     +                ABS(w(n+l))*IREAL(j))
     +               jw(ne+l)=j
                  w(n+l)=w(n+l)+xp+xm
                  ii=ii+2
               end do
               axp=ABS( SIGNUM -w(n+k))
               axm=ABS(-SIGNUM -w(n+k))
               w(n+k)=axm
               if (axp.gt.axm) w(n+k)=axp
               axp=ABS( SIGNUM2-w(n+k+1))
               axm=ABS(-SIGNUM2-w(n+k+1))
               w(n+k+1)=axm
               if (axp.gt.axm) w(n+k+1)=axp
            else
c              skip at least column/row k
               w(n+k)  =FONE
            end if
            condest1=CTOD(w(n+k))
            condest2=CTOD(w(n+k+1))
         end if

c        if the inverse-based estimate forces to skip the update,
c        then we still have to formally set the mode flag
         if (skip) mode=0
c
c        improved estimate using a different right hand side
         if (improved.gt.0 .and. .not.skip) then
            signum =FONE
            signum2=FONE
#if !defined _DOUBLE_REAL_ && !defined _SINGLE_REAL_
c           1x1 update 
            if (mode.eq.0) then
               mup=RZERO
               j=jldl(k)
               do i=jldl(k),jldl(k+1)-1
c                 column index l
                  l=jldl(i)
                  xp=aldl(i)*ialdlk
                  xm=w(n2+l)-w(n2+k)*xp
                  axp=ABS(xm)+ABS(xp)
                  if (axp.gt.mup) then 
                     mup=axp
                     j=i
                  end if
               end do
c              choose the sign such that component l is maximized
               if (jldl(k).lt.jldl(k+1)) then
                  l=jldl(j)
                  xp=aldl(j)*ialdlk
                  xm=w(n2+l)-w(n2+k)*xp
                  if (xp.ne.FNULL .and. 
     +                xm.ne.FNULL) then
                     signum=(xm/ABS(xm))*(CONJ(xp)/ABS(xp))
                  end if
               end if
            else
c              2x2 update 


c              signs, column k:k+1
               mup=RZERO
               mum=RZERO
               j=jldl(k)
               jj=j
               ii=j
               do i=jldl(k),jldl(k+1)-1
c                 column index l
                  l=jldl(i)
c                 first row of 
c                 [A(k,k)   A(k,k+1)  ]^{-1}*U(k:k+1,l)
c                 [A(k+1,k) A(k+1,k+1)]
                  xp=U11*aldl(ii)+U12*aldl(ii+1)
                  xm=w(n2+l)-w(n2+k)*xp
                  axp=ABS(xm)+ABS(xp)
                  if (axp.gt.mup) then 
                     mup=axp
                     j=i
                  end if
c                 second row of 
c                 [A(k,k)   A(k,k+1)  ]^{-1}*U(k:k+1,l)
c                 [A(k+1,k) A(k+1,k+1)]
                  xp=U21*aldl(ii)+U22*aldl(ii+1)
                  xm=w(n2+l)-w(n2+k+1)*xp
                  axp=ABS(xm)+ABS(xp)
                  if (axp.gt.mum) then 
                     mum=axp
                     jj=i
                  end if
                  ii=ii+2
               end do
               if (jldl(k).lt.jldl(k+1)) then
                  l=jldl(j)
                  ii=jldl(k)+2*(j-jldl(k))
c                 first row of 
c                 [A(k,k)   A(k,k+1)  ]^{-1}*U(k:k+1,l)
c                 [A(k+1,k) A(k+1,k+1)]
                  xp=U11*aldl(ii)+U12*aldl(ii+1)
                  xm=w(n2+l)-w(n2+k)*xp
                  if (xp.ne.FNULL .and.
     +                xm.ne.FNULL) then
                     signum=(xm/ABS(xm))*(CONJ(xp)/ABS(xp))
                  end if
                  l=jldl(jj)
                  ii=jldl(k)+2*(jj-jldl(k))
c                 second row of 
c                 [A(k,k)   A(k,k+1)  ]^{-1}*U(k:k+1,l)
c                 [A(k+1,k) A(k+1,k+1)]
                  xp=U21*aldl(ii)+U22*aldl(ii+1)
                  xm=w(n2+l)-w(n2+k+1)*xp
                  if (xp.ne.FNULL .and.
     +                xm.ne.FNULL) then
                     signum2=(xm/ABS(xm))*(CONJ(xp)/ABS(xp))
                  end if
               end if
            end if
#else
#endif

c           1x1 update
            if (mode.eq.0) then
c              try +/-1 as k-th component of the right hand side
               xp=( SIGNUM-w(n2+k))*ialdlk
               xm=(-SIGNUM-w(n2+k))*ialdlk
c              for both cases count the number of serious changes by
c              evaluating U(1:k,k+1:n)^T*U(1:k,1:k)^{-T}*rhs(1:k)
               j=0
               m=0
               nup=RZERO
               num=RZERO
               do i=jldl(k),jldl(k+1)-1
c                 column index l
                  l=jldl(i)
                  ax=ABS(w(n2+l))
                  ay=ABS(w(n2+l)+xp*aldl(i))
                  nup=max(nup,ay)
c                 entry seriously increases                  
                  if (ay.gt.RTWO*ax .and. ay.gt.0.5)  j=j+1
c                 entry seriously decreases              
                  if (RTWO*ay.lt.ax .and. ax.gt.0.5)  j=j-1
                  ay=ABS(w(n2+l)+xm*aldl(i))
                  num=max(num,ay)
c                 entry seriously increases              
                  if (ay.gt.RTWO*ax .and. ay.gt.0.5)  m=m+1
c                 entry seriously decreases              
                  if (RTWO*ay.lt.ax .and. ax.gt.0.5)  m=m-1
               end do
               if (j.ge.m) then
                  x=xp
                  axp=ABS( SIGNUM-w(n2+k))
               else
                  x=xm
                  axp=ABS(-SIGNUM-w(n2+k))
                  nup=num
               end if
               
c              does the norm estimate exceed the prescribed bound? 

#if defined PRINT_INFO3 || defined PRINT_INFO2
               if (max(nup,axp).gt.condest) 
     +            write (6, '(A)') '5. skipped '
#endif

               if (max(nup,axp).gt.condest) then 
                  skip=.true.
               end if

               if (.not. skip) then 
                  do i=jldl(k),jldl(k+1)-1
c                    column index l
                     l=jldl(i)
                     w(n2+l)=w(n2+l)+x*aldl(i)
                  end do
                  axp=ABS( SIGNUM-w(n2+k))
                  axm=ABS(-SIGNUM-w(n2+k))
                  if (axm.gt.CTOD(w(n+k))) w(n+k)=axm
                  if (axp.gt.CTOD(w(n+k))) w(n+k)=axp
               else
                  w(n+k)=FONE
c                 'undo' update 
                  do i=jldl(k),jldl(k+1)-1
                     l=jldl(i)
                     y=buffer*aldl(i)
                     w(n+l)=w(n+l)+y
                  end do
               end if
               condest1=CTOD(w(n+k))

c           2x2 update
            else

c              update using columns k,k+1
c              try +/-SIGNUM, and +/-SIGNUM2 as k-th and (k+1)-th component
c              of the right hand side

c              [ SIGNUM -w(n2+k),  SIGNUM2-w(n2+k+1)] * [A(k,k)   A(k,k+1)  ]^{-1}
c                                                       [A(k+1,k) A(k+1,k+1)]     
               xp =( SIGNUM -w(n2+k))*U11+( SIGNUM2-w(n2+k+1))*U21
               xp2=( SIGNUM -w(n2+k))*U12+( SIGNUM2-w(n2+k+1))*U22

c              [ SIGNUM -w(n2+k), -SIGNUM2-w(n2+k+1)] * [A(k,k)   A(k,k+1)  ]^{-1}
c                                                       [A(k+1,k) A(k+1,k+1)]     
               xp3=( SIGNUM -w(n2+k))*U11+(-SIGNUM2-w(n2+k+1))*U21
               xp4=( SIGNUM -w(n2+k))*U12+(-SIGNUM2-w(n2+k+1))*U22

c              [-SIGNUM -w(n2+k), -SIGNUM2-w(n2+k+1)] * [A(k,k)   A(k,k+1)  ]^{-1}
c                                                       [A(k+1,k) A(k+1,k+1)]     
               xm =(-SIGNUM -w(n2+k))*U11+(-SIGNUM2-w(n2+k+1))*U21
               xm2=(-SIGNUM -w(n2+k))*U12+(-SIGNUM2-w(n2+k+1))*U22

c              [-SIGNUM -w(n2+k),  SIGNUM2-w(n2+k+1)] * [A(k,k)   A(k,k+1)  ]^{-1}
c                                                       [A(k+1,k) A(k+1,k+1)]     
               xm3=(-SIGNUM -w(n2+k))*U11+( SIGNUM2-w(n2+k+1))*U21
               xm4=(-SIGNUM -w(n2+k))*U12+( SIGNUM2-w(n2+k+1))*U22

c              for both cases count the number of serious changes by
c              evaluating U(1:k,k+1:n)^T*U(1:k,1:k)^{-T}*rhs(1:k)
               j=0
               m=0
               jj=0
               r=0
               nup=RZERO
               num=RZERO
               nup2=RZERO
               num2=RZERO
               ii=jldl(k)
               do i=jldl(k),jldl(k+1)-1
c                 column index l
                  l=jldl(i)
                  ax=ABS(w(n2+l))

c                 +1/+1 
                  ay=ABS(w(n2+l)+aldl(ii)*xp +aldl(ii+1)*xp2)
                  nup=max(nup,ay)
c                 entry seriously increases                  
                  if (ay.gt.RTWO*ax .and. ay.gt.0.5)  j=j+1
c                 entry seriously decreases              
                  if (RTWO*ay.lt.ax .and. ax.gt.0.5)  j=j-1

c                 +1/-1 
                  ay=ABS(w(n2+l)+aldl(ii)*xp3+aldl(ii+1)*xp4)
                  nup2=max(nup2,ay)
c                 entry seriously increases                  
                  if (ay.gt.RTWO*ax .and. ay.gt.0.5)  jj=jj+1
c                 entry seriously decreases              
                  if (RTWO*ay.lt.ax .and. ax.gt.0.5)  jj=jj-1

c                 -1/-1 
                  ay=ABS(w(n2+l)+aldl(ii)*xm +aldl(ii+1)*xm2)
                  num=max(num,ay)
c                 entry seriously increases              
                  if (ay.gt.RTWO*ax .and. ay.gt.0.5)  m=m+1
c                 entry seriously decreases              
                  if (RTWO*ay.lt.ax .and. ax.gt.0.5)  m=m-1

c                 -1/+1 
                  ay=ABS(w(n2+l)+aldl(ii)*xm3+aldl(ii+1)*xm4)
                  num2=max(num2,ay)
c                 entry seriously increases              
                  if (ay.gt.RTWO*ax .and. ay.gt.0.5)  r=r+1
c                 entry seriously decreases              
                  if (RTWO*ay.lt.ax .and. ax.gt.0.5)  r=r-1

                  ii=ii+2
               end do
               if (j.ge.m .and. j.ge.jj .and. j.ge.r) then
                  x =xp
                  x2=xp2
                  axp=max(ABS( SIGNUM -w(n2+k)),
     +                    ABS( SIGNUM2-w(n2+k+1)))  
               elseif (jj.ge.m .and. jj.ge.j .and. jj.ge.r) then
                  x =xp3
                  x2=xp4
                  axp=max(ABS( SIGNUM -w(n2+k)),
     +                    ABS(-SIGNUM2-w(n2+k+1)))  
                  nup=nup2
               elseif (r.ge.m .and. r.ge.j .and. r.ge.jj) then
                  x =xm3
                  x2=xm4
                  axp=max(ABS(-SIGNUM -w(n2+k)),
     +                    ABS( SIGNUM2-w(n2+k+1)))  
                  nup=num2
               else
                  x =xm
                  x2=xm2
                  axp=max(ABS(-SIGNUM -w(n2+k)),
     +                    ABS(-SIGNUM2-w(n2+k+1)))  
                  nup=num
               end if
               
c              does the norm estimate exceed the prescribed bound? 

#if defined PRINT_INFO3 || defined PRINT_INFO2
               if (max(nup,axp).gt.condest) 
     +            write (6, '(A)') '6. skipped '
#endif

               if (max(nup,axp).gt.condest) then 
                  skip=.true.
               end if

               if (.not. skip) then 
c                 up to this point w(nw), jw(njw1(2)),jw(njw2(2)) still
c                 carry the information of column k+1. Now they are no
c                 longer needed, since we will perform a 2x2 update
                  do l=1,rlen(2)
                     i=jw(njw2(2)+l)
                     w(nw+i)=FNULL
                     jw(njw1(2)+i)=0
                  end do

                  ii=jldl(k)
                  do i=jldl(k),jldl(k+1)-1
c                    column index l
                     l=jldl(i)
                     w(n2+l)=w(n2+l)+aldl(ii)*x+aldl(ii+1)*x2
                     ii=ii+2
                  end do

                  axp=ABS( SIGNUM-w(n2+k))
                  axm=ABS(-SIGNUM-w(n2+k))
                  if (axm.gt.CTOD(w(n+k))) w(n+k)=axm
                  if (axp.gt.CTOD(w(n+k))) w(n+k)=axp

                  axp=ABS( SIGNUM2-w(n2+k+1))
                  axm=ABS(-SIGNUM2-w(n2+k+1))
                  if (axm.gt.CTOD(w(n+k+1))) w(n+k+1)=axm
                  if (axp.gt.CTOD(w(n+k+1))) w(n+k+1)=axp
               else
                  w(n+k)=FONE
c                 'undo' update k,k+1
                  ii=jldl(k)
                  do i=jldl(k),jldl(k+1)-1
                     l=jldl(i)
                     y=aldl(ii)*buffer+aldl(ii+1)*buffer2
                     w(n+l)=w(n+l)+y
                     ii=ii+2
                  end do
                  w(n+k+1)=wbuff
                  jw(ne+k+1)=jbuff
               end if
               condest1=CTOD(w(n+k))
               condest2=CTOD(w(n+k+1))
            end if
         end if
c        end improved estimate

c        if the inverse-based estimate forces to skip the update,
c        then we still have to formally set the mode flag
         if (skip) mode=0

c
c        estimate the infinity norm from the estimate of the 1-norm
c        by dividing the 1-norm by the estimated number of essential
c        nonzeros in row k
c        on the average we roughly assume that half of the off-diagonal
c        entries may cancel as a result of using only ONE test vector
         if (jw(ne+k).gt.2) 
     +      w(n+k)=2*w(n+k)/IREAL(jw(ne+k))
         if (CTOD(w(n+k)).lt.RONE) w(n+k)=FONE
         condest1=max(condest1,CTOD(w(n+k)))
         if (mode.eq.1 .and. .not.skip) then
            if (jw(ne+k+1).gt.2)
     +         w(n+k+1)=2*w(n+k+1)/IREAL(jw(ne+k+1))
            if (CTOD(w(n+k+1)).lt.RONE) w(n+k+1)=FONE
            condest2=max(condest2,CTOD(w(n+k)))
         end if
c        end estimate norm of the inverse 

c        advance list and pointers for columns of A
         i=jw(n2+k)
c        while i>0         
 261     if (i.le.0) goto 262

c           pointer to A(p(i),q(k))
            j=jw(n4+i)
c           column index l of  A(p(i),q(l))
            l=invq(ja(j))
c           store next row from the linked list
            m=jw(n3+i)

c           update column information
            jw(n4+i)=j+1
c           pointer to the next nonzero element in row i
            j=jw(n4+i)
c           we have to make sure that we are still inside row i
            if (j.lt.ia(p(i)+1)) then
c              column index l of A(p(i),q(l))
               l=invq(ja(j))
c              add new entry to the head of the list
               jw(n3+i)=jw(n2+l)
               jw(n2+l)=i
            end if

c           recover next row entry
            i=m
         goto 261
c        end while

c        if step k has to be skipped due to a suitable criterion
 262     if (skip) then

c           skip node k
            nskipped=nskipped+1
            jw(nc+k)=nskipped


c           permute row/column k behind the (possibly) already existing
c           skipped parts

c           -----   dealing with the upper triangular part   -----
c           additional Tismenetsky entries
            if (sctype.gt.0) then
c              linked list for the epsilon size part of U
               i=jw(n10+k)
c              while i>0
 266           if (i.eq.0) goto 267
c                 does there exist an entry in U(i,k:n)?
c                 first nonzero entry in the epsilon size part of U
                  l=jw(n8+i)
                  if (l.lt.jldl(i+1)) then 
c                    if this is the case, is the first entry equal to k,
c                    otherwise we don't need changes since U(i,k)=0
c                    U(i,k)?
                     if (jldl(l).eq.k) then
c                       last skipped entry
                        j=jw(nb+i)+jw(el+i)
c                       swap indices
                        jldl(l)=jldl(j)
                        jldl(j)=k
c                       swap numerical values
c                       single row i
                        if (jldl(n+1+i).eq.0) then
                           x=aldl(l)
                           aldl(l)=aldl(j)
                           aldl(j)=x

c                       double row i,i+1
                        else
c                          numerical values appear in pairs
                           l=jldl(i)+2*(l-jldl(i))
                           j=jldl(i)+2*(j-jldl(i))
                           x=aldl(l)
                           aldl(l)=aldl(j)
                           aldl(j)=x
                           l=l+1
                           j=j+1
                           x=aldl(l)
                           aldl(l)=aldl(j)
                           aldl(j)=x
                        end if
c                       increment number of skipped entries
                        jw(el+i)=jw(el+i)+1
                     end if 
                  end if
                  i=jw(n10+i)
               goto 266
c              end while
            end if
c           linked list for the regular part of U
 267        i=jw(n6+k)
c           while i>0
 268        if (i.eq.0) goto 273
c              does there exist an entry in U(i,k:n)?
c              first nonzero entry in the regular part of U
               l=jw(na+i)
               if (l.lt.jw(nb+i)) then 
c                 if this is the case, is the first entry equal to k,
c                 otherwise we don't need changes since U(i,k)=0
c                 U(i,k)?
                  if (jldl(l).eq.k) then
c                    position after the last skipped entry
                     j=jldl(i)+jw(nl+i)
c                    swap indices
                     jldl(l)=jldl(j)
                     jldl(j)=k
c                    swap numerical values
                     if (jldl(n+1+i).eq.0) then
                        x=aldl(l)
                        aldl(l)=aldl(j)
                        aldl(j)=x

c                    double row i,i+1
                     else
c                       numerical values appear in pairs
                        l=jldl(i)+2*(l-jldl(i))
                        j=jldl(i)+2*(j-jldl(i))
                        x=aldl(l)
                        aldl(l)=aldl(j)
                        aldl(j)=x
                        l=l+1
                        j=j+1
                        x=aldl(l)
                        aldl(l)=aldl(j)
                        aldl(j)=x
                     end if
c                    increment number of skipped entries
                     jw(nl+i)=jw(nl+i)+1
                  end if 
               end if
               i=jw(n6+i)
            goto 268
c           end while
         end if
        


c        -----   sparsify U   -----
c
c        drop tolerances
c
c        classical dropping
c        drop entries that are less than a tolerance droptol multiplied
c        by the absolute value of the diagonal entry
 273     if (mode.eq.0) then
            if (skip) goto 280
            droptolU=droptol*absaldlk


c           do we use inverse-based dropping?
            if (droptype.gt.0) then
               droptolU=droptolU/CTOD(w(n+k))
            end if 
            maxcondest=max(maxcondest,CTOD(w(n+k)))
            droptol2=droptols(2)/maxcondest

c           upper triangular part
c           separate nonzero entries of row k of U between those which
c           are greater than droptolU and those which are equal to or less
c           than droptolU (in absolute values)
            i=jldl(k)
            j=jldl(k+1)
c           1x1
c           Umax  = max_(m>k)   |U(k,k)\U(k,m)|
c           while i<j
 270           if (i.ge.j) goto 280
c              column index
               l=jldl(i)
c              move small size entries to the end
c
c              Suppose that the entries of the Schur complement
c              are of magnitude O(1) (by some initial scaling)
c              1. check whether the contribution to row l by
c                 the rank-1 update is small:
c                                |U(k,m)|      droptols(2)
c                 |U(k,l)| * max --------  <=  -----------
c                             m  |U(k,k)|      maxcondest(k)
c                 If the entries of the Schur complement are
c                 of magnitude O(1), then the contribution to
c                 columns j, such that U(k,j)<>0 will also be 
c                 small.
c     
c              2. keep error in |tildeU^{-1}*U-I| small
c                 |U(k,l)|                       droptols(1)
c                 --------                 <=  ---------------
c                 |U(k,k)|                     ||U^{-1}(:,k)||
               if (ABS(aldl(i)*Umax) .le.droptol2
c     +             .and. ABS(aldl(i)).le.droptolU
     +            ) then
                  j=j-1

#if defined PRINT_INFO || defined PRINT_INFO2
                  write (6,'(A,I4)')'drop ',l
#endif
c                 swap entries
                  x=aldl(i)
                  aldl(i)=aldl(j)
                  aldl(j)=x
               
                  jldl(i)=jldl(j)
                  jldl(j)=l
               else
                  i=i+1
               end if
               goto 270
c           end while
c
c           now starting from position j we will have all the dirt


 280        if (skip) then
c              clear U information and treat row k as if it were
c              not present
               jldl(k+1) =jldl(k)
               j=jldl(k)
            end if

c           truncate row k such that we will have at most lfil entries
c           first truncate the regular part if necessary
            i=jldl(k)
c           maximum length
            len=min0(j-i,lfil)
c           select the `len' largest entries in modulus
            call QSPLIT(aldl(i),jldl(i),j-i,len)

c           apply diagonal compensation if desired
            if (milu.gt.0) then
               do l=i+len,jldl(k+1)-1
                  m=jldl(l)
                  aldl(k)=aldl(k)+aldl(l)
                  aldl(m)=aldl(m)+CONJUG(SKEW(aldl(l)))
               end do
            end if


c           invert diagonal entry
            ialdlk=FONE/aldl(k)
            if (.not.skip) aldl(k)=ialdlk



c           Tismenetsky case
            if (sctype.gt.0 .and. .not.skip) then
c              refined diagonal compensation
               if (milu.gt.0) then
                  xp=FNULL
                  xm=FNULL
                  do l=i,i+len-1
                     xp=xp+aldl(l)
                  end do
                  do l=i+len,jldl(k+1)-1
                     xm=xm+aldl(l)
                  end do
                  xp=xp*ialdlk
                  xm=xm*ialdlk
                  do l=i,i+len-1
                     aldl(jldl(l))=aldl(jldl(l))+aldl(l)*xm
                  end do
                  do l=i+len,jldl(k+1)-1
                     aldl(jldl(l))=aldl(jldl(l))+aldl(l)*xp
                  end do
               end if

c              if there is some space leftover truncate the epsilon part
               if (j-i.lt.lfil) then
c                 maximum length
                  len=min0(jldl(k+1)-j,lfil-j+i)
                  call QSPLIT(aldl(j),jldl(j),jldl(k+1)-j,len)
                  len=len+j-i
               else
                  j=i+len
               end if
            else
c              simple case
               j=i+len
            end if


c           keep a pointer to the first space behind the regular part of U
            jw(nb+k)=j
c           adapt new starting position of next column of L
            jldl(k+1)=jldl(k)+len
c           sort regular part and epsilon size part separately
            call QSORT(aldl(i),jldl(i),jw(n+1),j-i)
            call QSORT(aldl(j),jldl(j),jw(n+1),len-j+i)

#ifdef PRINT_INFO2
            if (.not.skip) then
               if (mode.eq.0) then
                  write (6, '(A,I4)') 'row ',k
                  write (6, '(1P,E8.1)') aldl(k)
                  write (6, '(10I8)')      
     +                 (jldl(l),   l=jldl(k),jldl(k+1)-1)
                  write (6, '(1P,10E8.1)')
     +                 (dreal(aldl(l)), l=jldl(k),jldl(k+1)-1)
                  write (6, '(1P,10E8.1)')
     +                 (dimag(aldl(l)), l=jldl(k),jldl(k+1)-1)
               else
                  write (6, '(A,I4,I4)') '1.rows ',k,k+1
                  write (6, '(A,I4,I4,I4)') 'jldl ',
     +                  jldl(k),jldl(k+1),jldl(k+2)
                  write (6, '(1P,E8.1,A,1P,E8.1,1P,E8.1,A,1P,E8.1)') 
     +                 dreal(aldl(k)),'+i*',dimag(aldl(k)),
     +                 dreal(aldl(n+1+k)),'+i*',dimag(aldl(n+1+k))
                  write (6, '(1P,E8.1,A,1P,E8.1,1P,E8.1,A,1P,E8.1)') 
     +                 dreal(CONJUG(SKEW(aldl(n+1+k)))),'+i*',
     +                 dimag(CONJUG(SKEW(aldl(n+1+k)))),
     +                 dreal(aldl(k+1)),'+i*',dimag(aldl(k+1))
                  write (6, '(10I8)')      
     +                 (jldl(l),   l=jldl(k),jldl(k+1)-1)
                  write (6, '(1P,10E8.1)')
     +       (dreal(aldl(jldl(k)+2*(l-jldl(k)))),l=jldl(k),jldl(k+1)-1)
                  write (6, '(1P,10E8.1)')
     +       (dimag(aldl(jldl(k)+2*(l-jldl(k)))),l=jldl(k),jldl(k+1)-1)
                  write (6, '(1P,10E8.1)')
     +     (dreal(aldl(jldl(k)+2*(l-jldl(k))+1)),l=jldl(k),jldl(k+1)-1)
                  write (6, '(1P,10E8.1)')
     +     (dimag(aldl(jldl(k)+2*(l-jldl(k))+1)),l=jldl(k),jldl(k+1)-1)
               end if
            end if
#endif



c           upper triangular part
c           removed
c


c           -----   update diagonal entries   -----

#ifdef PRINT_INFO2
            write (6, '(A)') 'updated diagonal entries'
#endif

c
c           regular update
            i=jldl(k)
c           while i<jw(nb+k)
 400           if (i.ge.jw(nb+k)) goto 398
               l=jldl(i)
               aldl(l)=aldl(l)-aldl(i)*ialdlk*CONJUG(SKEW(aldl(i)))

#ifdef PRINT_INFO2
               write (6, '(I4,1P,E8.1,A,1P,E8.1)') l,
     +              dreal(aldl(l)),'+i*',dimag(aldl(l))
#endif

               i=i+1
               goto 400
c           end while

c
c           -----   shuffle embedded skipped entries to the front   -----
c
c           U-part
 398        i=0
            j=jldl(k)-1
c           scan row k
            do l=jldl(k),jw(nb+k)-1
c              index m of U(k,m)
               m=jldl(l)
c              is this node NOT skipped?
c              use iw and w as buffer
               if (jw(nc+m).eq.0) then
                  i=i+1
                  jw(i)=m
                  w(i)=aldl(l)
               else 
c                 shift skipped entries to the front
                  j=j+1
                  jldl(j)=m
                  aldl(j)=aldl(l)
               end if
            end do
c           number of skipped nodes in row k of U
            jw(nl+k)=j-jldl(k)+1
c           put NON-skipped grid entries back to the end
            i=0
            j=j+1
c           while j<jw(nb+k)
 402           if (j.ge.jw(nb+k)) goto 403
               i=i+1
               m=jw(i)
               jldl(j)=m
               aldl(j)=w(i)
               jw(i)=0
               w(i)=FNULL
               j=j+1
               goto 402
c           end while
 403        if (sctype.gt.0) then
c              U-part, Tismenetsky updates
               i=0
               j=jw(nb+k)-1
c              scan row k, epsilon size part
               do l=jw(nb+k),jldl(k+1)-1
c                 index m of U(k,m)
                  m=jldl(l)
c                 is this node NOT skipped?
c                 use iw and w as buffer
                  if (jw(nc+m).eq.0) then
                     i=i+1
                     jw(i)=m
                     w(i)=aldl(l)
                  else 
c                    shift skipped entries to the front
                     j=j+1
                     jldl(j)=m
                     aldl(j)=aldl(l)
                  end if
               end do
c              number of skipped nodes in row k of U, epsilon size part
               jw(el+k)=j-jw(nb+k)+1
c              put NON-skipped entries back to the end
               i=0
               j=j+1
c              while j<jldl(k+1)
 405              if (j.ge.jldl(k+1)) goto 410
                  i=i+1
                  m=jw(i)
                  jldl(j)=m
                  aldl(j)=w(i)
                  jw(i)=0
                  w(i)=FNULL
                  j=j+1
               goto 405
c              end while
            end if


c        2x2 case
         else
            if (skip) goto 281
            droptolU=droptol

c           do we use inverse-based dropping?
            if (droptype.gt.0) then
               droptolU=droptolU/max(CTOD(w(n+k)),
     +                               CTOD(w(n+k+1)))
            end if 
            maxcondest=max(maxcondest,CTOD(w(n+k)))
            maxcondest=max(maxcondest,CTOD(w(n+k+1)))
            droptol2=droptols(2)/maxcondest

c           upper triangular part
c           separate nonzero entries of row k of U between those which
c           are greater than droptolU and those which are equal to or less
c           than droptolU (in absolute values)
            i=jldl(k)
            j=jldl(k+1)
            ii=i
            jj=jldl(k+2)
c           2x2
c           Umax  = max_(m>k+1) ||U(k:k+1,k:k+1)^{-1}*U(k:k+1,m)||
c           while i<j
 271        if (i.ge.j) goto 281
c              column index
               l=jldl(i)
c              move small size entries to the end
c
c              Suppose that the entries of the Schur complement
c              are of magnitude O(1) (by some initial scaling)
c              1. check whether the contribution to row l by
c                 the rank-2 update is small:
c                                             droptols(2)
c                 ||U(k:k+1,l)|| * Umax  <=  -------------
c                                            maxcondest(k)
c                 If the entries of the Schur complement are
c                 of magnitude O(1), then the contribution to
c                 columns j, such that U(k:k+1,j)<>0 will also be 
c                 small.
               axp=ABS(aldl(ii))*Umax
               axm=ABS(aldl(ii+1))*Umax
c
c
c              2. keep error in |tildeU^{-1}*U-I| small
c                                                             droptols(1)
c                 ||U(k:k+1,k:k+1)^{-1}*U(k:k+1,l)||  <=  -------------------
c                                                         ||U^{-1}(:,k:k+1)||
               ax=ABS(U11*aldl(ii)+U12*aldl(ii+1))
               ay=ABS(U21*aldl(ii)+U22*aldl(ii+1))
               if (max(axp,axm).le.droptol2
c     +             .and. max(ax,ay).le.droptolU
     +              ) then
#if defined PRINT_INFO || defined PRINT_INFO2
                  write (6,'(A,I4)')'drop ',l
#endif
                  j=j-1
                  jj=jj-2

c                 swap entries
                  x=aldl(ii)
                  aldl(ii)=aldl(jj)
                  aldl(jj)=x
                  x=aldl(ii+1)
                  aldl(ii+1)=aldl(jj+1)
                  aldl(jj+1)=x
               
                  jldl(i)=jldl(j)
                  jldl(j)=l
               else
c                 sparsify single entries if possible
                  if (max(ax,ay).le.droptolU) then
                     if (axp.le.droptol2) aldl(ii)  =FNULL
                     if (axm.le.droptol2) aldl(ii+1)=FNULL
                  end if
                  i=i+1
                  ii=ii+2
               end if
            goto 271
c           end while

c           NOW we can clear the U part
c 281        do i=jldl(k),jldl(k+1)-1
c               l=jldl(i)
c            end do

 281        if (skip) then
c              clear U information and treat row k as if it were
c              not present
               jldl(k+1) =jldl(k)
               jldl(k+2) =jldl(k)
               j=jldl(k)
            end if

c           truncate row k,k+1 such that we will have at most lfil entries
c           first truncate the regular part if necessary
            i=jldl(k)
c           maximum length
            len=min0(j-i,lfil)
c           select the `len' largest entries in modulus
            call QSPLIT2(aldl(i),jldl(i),j-i,len)

c           apply diagonal compensation if desired
            if (milu.gt.0) then
               ii=jldl(k)+2*(i+len-jldl(k))
               do l=i+len,jldl(k+1)-1
                  m=jldl(l)
                  aldl(k)  =aldl(k)  +aldl(ii)
                  aldl(k+1)=aldl(k+1)+aldl(ii+1)
                  aldl(m)=aldl(m)+CONJUG(SKEW(aldl(ii)))
     +                           +CONJUG(SKEW(aldl(ii+1)))
                  ii=ii+2
               end do
            end if


c           invert diagonal block
c           restore -A(k,k+1)
            U12=aldl(n+1+k)
            U21=CONJUG(SKEW(U12))
            U11=aldl(k+1)
            U22=aldl(k)
            ialdlk=FONE/(U11*U22-U21*U12)
            if (.not.skip) then
               U12=U12*ialdlk
               U21=U21*ialdlk
               U11=U11*ialdlk
               U22=U22*ialdlk
               aldl(k)    =U11
               aldl(k+1)  =U22
               aldl(n+1+k)=U12
c              indicate block of size 2
               jldl(n+1+k)=2
            end if


c           Tismenetsky case
            if (sctype.gt.0 .and. .not.skip) then
c              refined diagonal compensation
               if (milu.gt.0) then
                  xp =FNULL
                  xm =FNULL
                  xp2=FNULL
                  xm2=FNULL
                  ii=jldl(k)+2*(i-jldl(k))
                  do l=i,i+len-1
                     xp =xp +aldl(ii)
                     xp2=xp2+aldl(ii+1)
                     ii=ii+2
                  end do
                  do l=i+len,jldl(k+1)-1
                     xm =xm +aldl(ii)
                     xm2=xm2+aldl(ii+1)
                     ii=ii+2
                  end do
c                 [xp ]=U(k:k+1,k:k+1)^{-1}*[xp ]
c                 [xp2]                     [xp2]
                  x=xp
                  xp =U11*x+U12*xp2
                  xp2=U21*x+U22*xp2
c                 [xm ]=U(k:k+1,k:k+1)^{-1}*[xm ]
c                 [xm2]                     [xm2]
                  x=xm
                  xm =U11*x+U12*xm2
                  xm2=U21*x+U22*xm2
                  ii=jldl(k)+2*(i-jldl(k))
                  do l=i,i+len-1
                     m=jldl(l)
                     aldl(m)=aldl(m)+aldl(ii)*xm+aldl(ii+1)*xm2
                     ii=ii+2
                  end do
                  do l=i+len,jldl(k+1)-1
                     m=jldl(l)
                     aldl(m)=aldl(m)+aldl(ii)*xp+aldl(ii+1)*xp2
                     ii=ii+2
                  end do
               end if

c              if there is some space leftover truncate the epsilon part
               if (j-i.lt.lfil) then
c                 maximum length
                  len=min0(jldl(k+1)-j,lfil-j+i)
                  call QSPLIT2(aldl(j),jldl(j),jldl(k+1)-j,len)
                  len=len+j-i
               else
                  j=i+len
               end if
            else
c              simple case
               j=i+len
            end if



c           keep a pointer to the first space behind the regular part of U
            jw(nb+k)=j
c           adapt new starting position of next column of L
            jldl(k+1)=jldl(k)  +len
            jldl(k+2)=jldl(k+1)+len
c           sort regular part and epsilon size part separately
            call QSORT2(aldl(i),jldl(i),jw(n+1),j-i)
            call QSORT2(aldl(j),jldl(j),jw(n+1),len-j+i)

#ifdef PRINT_INFO2
            if (.not.skip) then
               if (mode.eq.0) then
                  write (6, '(A,I4)') 'row ',k
                  write (6, '(1P,E8.1)') aldl(k)
                  write (6, '(10I8)')      
     +                 (jldl(l),   l=jldl(k),jldl(k+1)-1)
                  write (6, '(1P,10E8.1)')
     +                (dreal(aldl(l)),   l=jldl(k),jldl(k+1)-1)
                  write (6, '(1P,10E8.1)')
     +                (dimag(aldl(l)),   l=jldl(k),jldl(k+1)-1)
               else
                  write (6, '(A,I4,I4)') '2.rows ',k,k+1
                write (6, '(1P,E8.1,A,1P,E8.1,1P,E8.1,A,1P,E8.1)') 
     +                 dreal(aldl(k)),'+i*',dimag(aldl(k)),
     +                 dreal(aldl(n+1+k)),'+i*',dimag(aldl(n+k+1))
                write (6, '(1P,E8.1,A,1P,E8.1,1P,E8.1,A,1P,E8.1)') 
     +                 dreal(CONJUG(SKEW(aldl(n+1+k)))),'+i*',
     +                 dimag(CONJUG(SKEW(aldl(n+1+k)))),
     +                 dreal(aldl(k+1)),'+i*',dimag(aldl(k+1))
                  write (6, '(10I8)')      
     +                 (jldl(l),   l=jldl(k),jldl(k+1)-1)
                  write (6, '(1P,10E8.1)')
     +      (dreal(aldl(jldl(k)+2*(l-jldl(k)))),l=jldl(k),jldl(k+1)-1)
                  write (6, '(1P,10E8.1)')
     +      (dimag(aldl(jldl(k)+2*(l-jldl(k)))),l=jldl(k),jldl(k+1)-1)
                  write (6, '(1P,10E8.1)')
     +      (dreal(aldl(jldl(k)+2*(l-jldl(k))+1)),l=jldl(k),jldl(k+1)-1)
                  write (6, '(1P,10E8.1)')
     +      (dimag(aldl(jldl(k)+2*(l-jldl(k))+1)),l=jldl(k),jldl(k+1)-1)
               end if
            end if
#endif

c           upper triangular part
c           removed
c

c           -----   update diagonal entries   -----
c

#ifdef PRINT_INFO2
            write (6, '(A)') 'updated diagonal entries'
#endif
            
c           regular update
            i=jldl(k)
c           while i<jw(nb+k)
 408        if (i.ge.jw(nb+k)) goto 409
               l=jldl(i)
               ii=jldl(k)+2*(i-jldl(k))
c              decrement aldl(l) by U(k:k+1,l)^T*[A(k,k)   A(k,k+1)  ]^{-1}*U(k:k+1,l)
c                                                [A(k+1,k) A(k+1,k+1)]
               xp=aldl(ii)
               xm=aldl(ii+1)
               aldl(l)=aldl(l)
     +                -CONJUG(SKEW(xp))*U11              *xp  
     +                -CONJUG(SKEW(xp))*U12              *xm
     +                -CONJUG(SKEW(xm))*CONJUG(SKEW(U12))*xp
     +                -CONJUG(SKEW(xm))*U22              *xm

#ifdef PRINT_INFO2
               write (6, '(I4,1P,E8.1)') l,aldl(l) 
#endif

               i=i+1
            goto 408
c           end while
c
c           -----   shuffle embedded skipped entries to the front   -----
c
c           U-part
 409        i=0
            j=jldl(k)-1
            jj=j-1
            ii=jldl(k)
c           scan row k,k+1
            do l=jldl(k),jw(nb+k)-1
c              index m of U(k:k+1,m)
               m=jldl(l)
c              is this node NOT skipped?
c              use iw and w as buffer
               if (jw(nc+m).eq.0) then
                  i=i+1
                  jw(i)=m
                  w(i)   =aldl(ii)
                  w(nw+i)=aldl(ii+1)
               else 
c                 shift skipped entries to the front
                  j=j+1
                  jj=jj+2
                  jldl(j)=m
                  aldl(jj)  =aldl(ii)
                  aldl(jj+1)=aldl(ii+1)
               end if
               ii=ii+2
            end do
c           number of skipped nodes in row k of U
            jw(nl+k)=j-jldl(k)+1
c           put NON-skipped grid entries back to the end
            i=0
            j=j+1
            jj=jj+2
c           while j<jw(nb+k)
 404        if (j.ge.jw(nb+k)) goto 401
               i=i+1
               m=jw(i)
               jldl(j)=m
               aldl(jj)  =w(i)
               aldl(jj+1)=w(nw+i)
               jw(i)=0
               w(i)   =FNULL
               w(nw+i)=FNULL
               j=j+1
               jj=jj+2
            goto 404
c           end while
 401        if (sctype.gt.0) then
c              U-part, Tismenetsky updates
               i=0
               j=jw(nb+k)-1
               jj=jldl(k)+2*(j-jldl(k))
c              scan row k, epsilon size part
               do l=jw(nb+k),jldl(k+1)-1
c                 index m of U(k,m)
                  m=jldl(l)
c                 is this node NOT skipped?
c                 use iw and w as buffer
                  if (jw(nc+m).eq.0) then
                     i=i+1
                     jw(i)=m
                     w(i)   =aldl(ii)
                     w(nw+i)=aldl(ii+1)
                  else 
c                    shift skipped entries to the front
                     j=j+1
                     jj=jj+2
                     jldl(j)=m
                     aldl(jj)  =aldl(ii)
                     aldl(jj+1)=aldl(ii+1)
                  end if
                  ii=ii+2
               end do
c              number of skipped nodes in row k of U, epsilon size part
               jw(el+k)=j-jw(nb+k)+1
c              put NON-skipped entries back to the end
               i=0
               j=j+1
               jj=jj+2
c              while j<jldl(k+1)
 406           if (j.ge.jldl(k+1)) goto 410
                  i=i+1
                  m=jw(i)
                  jldl(j)=m
                  aldl(jj)  =w(i)
                  aldl(jj+1)=w(nw+i)
                  jw(i)=0
                  w(i)   =FNULL
                  w(nw+i)=FNULL
                  j=j+1
                  jj=jj+2
               goto 406
c              end while
            end if
         end if      
c
c
#ifdef PRINT_INFO2
 410     if (.not.skip) then
            if (mode.eq.0) then
               write (6, '(A,I4)') 'row ',k
               write (6, '(1P,E8.1)') aldl(k)
               write (6, '(10I8)')      
     +              (jldl(l),   l=jldl(k),jldl(k+1)-1)
               write (6, '(1P,10E8.1)')
     +            (dreal(aldl(l)),   l=jldl(k),jldl(k+1)-1)
               write (6, '(1P,10E8.1)')
     +           (dimag(aldl(l)),   l=jldl(k),jldl(k+1)-1)
            else
               write (6, '(A,I4,I4)') '3.rows ',k,k+1
               write (6, '(1P,E8.1,A,1P,E8.1,1P,E8.1,A,1P,E8.1)') 
     +              dreal(aldl(k)),'+i*',dimag(aldl(k)),
     +              dreal(aldl(n+1+k)),'+i*',dimag(aldl(n+1+k))
               write (6, '(1P,E8.1,A,1P,E8.1,1P,E8.1,A,1P,E8.1)') 
     +              dreal(CONJUG(SKEW(aldl(n+1+k)))),'+i*',
     +              dimag(CONJUG(SKEW(aldl(n+1+k)))),
     +              dreal(aldl(k+1)),'+i*',dimag(aldl(k+1))
               write (6, '(10I8)')      
     +              (jldl(l),   l=jldl(k),jldl(k+1)-1)
               write (6, '(1P,10E8.1)')
     +        (dreal(aldl(jldl(k)+2*(l-jldl(k)))),l=jldl(k),jldl(k+1)-1)
               write (6, '(1P,10E8.1)')
     +        (dimag(aldl(jldl(k)+2*(l-jldl(k)))),l=jldl(k),jldl(k+1)-1)
               write (6, '(1P,10E8.1)')
     +      (dreal(aldl(jldl(k)+2*(l-jldl(k))+1)),l=jldl(k),jldl(k+1)-1)
               write (6, '(1P,10E8.1)')
     +      (dimag(aldl(jldl(k)+2*(l-jldl(k))+1)),l=jldl(k),jldl(k+1)-1)
            end if
         end if
#endif


c        -----   Update Ufirst, Ulist   -----
c
c        Update Ufirst, Ulist 
c
c        n         size of the problem
c        k         current step of the update procedure
c        jldl(k)   start of row k of U, regular part shifted by the
c       +jw(nl+k)  number of embedded skipped entries   
c        jldl      index array
c        jw(nb+1)  pointers to the first space behind any row of U
c                  (points behind the space of the regular part)
c        jw(n6+1)  linked list for the nonzeros of U in column k
c                  (regular part)
c        jw(na+1)  first nonzero entry in U(i,k:n)
c                  (regular part)
#ifdef PRINT_INFO2
         call iluclist(n,k,jldl(k)+jw(nl+k),jldl,jw(nb+1),jw(n6+1),
     +                 jw(na+1))
#else
 410     call iluclist(n,k,jldl(k)+jw(nl+k),jldl,jw(nb+1),jw(n6+1),
     +                 jw(na+1))
#endif

c        Tismenetsky update
         if (sctype.gt.0) then

c           save number of nonzeros in column k (Tismenetsky case)
c           n         size of the problem
c           k         current step of the update procedure
c           jw(nb+k)  start of row k of U, epsilon size part shifted by 
c          +jw(el+k)  the number of embedded skipped entries  
c           jldl      index array
c           jldl(2)   pointers to the first space behind any row of U
c                     (points behind the space of the epsilon part)
c           jw(n10+1) linked list for the nonzeros of U in column k
c                     (epsilon size part)
c           jw(n8+1)  first nonzero entry in U(i,k:n)
c                     (epsilon size part)
            call iluclist(n,k,jw(nb+k)+jw(el+k),jldl,jldl(2),jw(n10+1),
     +                    jw(n8+1))
         end if


#ifdef PRINT_INFO2
         do j=k,n,-1
            write (6,'(A,I4)')'linked list, column ',j
            i=jw(n6+j)
 791        if (i.eq.0) goto 792
               write (6,'(I4)')i
               i=jw(n6+i)
            goto 791
 792     end do


         
         if (.not.skip) then
            if (mode.eq.0) then
               write (6, '(A,I4)') 'row ',k
               write (6, '(1P,E8.1)') aldl(k)
               write (6, '(10I8)')      
     +               (jldl(l),   l=jldl(k),jldl(k+1)-1)
               write (6, '(1P,10E8.1)')
     +         (dreal(aldl(l)),   l=jldl(k),jldl(k+1)-1)
               write (6, '(1P,10E8.1)')
     +         (dimag(aldl(l)),   l=jldl(k),jldl(k+1)-1)
            else
               write (6, '(A,I4,I4)') '4.rows ',k,k+1
             write (6, '(1P,E8.1,A,1P,E8.1,1P,E8.1,A,1P,E8.1)') 
     +              dreal(aldl(k)),'+i*',
     +              dimag(aldl(k)),
     +              dreal(aldl(n+1+k)),'+i*',
     +              dimag(aldl(n+1+k))
             write (6, '(1P,E8.1,A,1P,E8.1,1P,E8.1,A,1P,E8.1)') 
     +              dreal(CONJUG(SKEW(aldl(n+1+k)))),'+i*',
     +              dimag(CONJUG(SKEW(aldl(n+1+k)))),
     +              dreal(aldl(k+1)),'+i*',
     +              dimag(aldl(k+1))
               write (6, '(10I8)')      
     +              (jldl(l),   l=jldl(k),jldl(k+1)-1)
               write (6, '(1P,10E8.1)')
     +     (dreal(aldl(jldl(k)+2*(l-jldl(k)))),l=jldl(k),jldl(k+1)-1)
               write (6, '(1P,10E8.1)')
     +     (dimag(aldl(jldl(k)+2*(l-jldl(k)))),l=jldl(k),jldl(k+1)-1)
               write (6, '(1P,10E8.1)')
     +   (dreal(aldl(jldl(k)+2*(l-jldl(k))+1)),l=jldl(k),jldl(k+1)-1)
               write (6, '(1P,10E8.1)')
     +   (dimag(aldl(jldl(k)+2*(l-jldl(k))+1)),l=jldl(k),jldl(k+1)-1)
            end if
         end if
#endif

c        check whether we have performed a 1x1 or 2x2 update
c        either increment k by 1 or by 2
c        First case. We have performed a 1x1 update or we
c        skipped row/column k
         if (mode.eq.0) then

#if defined PRINT_INFO || defined PRINT_INFO2
            do l=1,n
               if (jw(njw1(1)+l).ne.0)
     +              write (6,'(A,I4,I4)') '!, ',k,l
            end do
            do l=1,n
               if (w(l).ne.FNULL)
     +              write (6,'(A,I4)') '!!, ',l
            end do
#endif

            k=k+1
            loadrow=2
c           since we have performed "only" a 1x1 update the second
c           column has been left untouched. Now shift column k+1
            do l=1,rlen(2)
c              index i of l-th nonzero in the list
               i=jw(njw2(2)+l)
               jw(njw2(1)+l)=i
c              flag component i as nonzero
               jw(njw1(1)+i)=l
               jw(njw1(2)+i)=0
c              copy numerical value
               w(i)=w(nw+i)
               w(nw+i)=FNULL
            end do



            rlen(1)=rlen(2)
            rlen(2)=0
c           check whether we have to update this column from the previous
c           step
            if (.not.skip) then
c              rewrite norm of U^{-1}
               if (CTOD(w(n+k-1)).gt.condest1)
     +            write (6,'(A,1P,E8.1,1P,E8.1)') 
     +            'condest1 is too small',CTOD(w(n+k-1)),condest1
               w(n+k-1)=condest1
c              remove entry in position k-1
               i=jw(njw1(1)+k-1)
               if (i.ne.0) then
c                 swap last entry
                  l=jw(njw2(1)+rlen(1))
                  jw(njw1(1)+l)=i
                  jw(njw2(1)+i)=l
c                 clear entry
                  w(k-1)=FNULL
                  jw(njw1(1)+k-1)=0
                  rlen(1)=rlen(1)-1
               end if
               i=0
               do l=jldl(k-1)+jw(nl+k-1),jw(nb+k-1)-1
c                 is U(k-1,k)<>0?
                  if (jldl(l).eq.k) i=l
               end do
c              update column k
               if (i.gt.0) then
                  xp=CONJUG(SKEW(aldl(i)))*aldl(k-1)
                  do l=jldl(k-1),jldl(k)-1
                     j=jldl(l)
c                    make sure that the diagonal entry does not re-enter again
                     if (j.ne.k) then
                        w(j)=w(j)-xp*aldl(l)
                        if (jw(njw1(1)+j).eq.0) then
                           rlen(1)=rlen(1)+1
                           jw(njw2(1)+rlen(1))=j
                           jw(njw1(1)+j)=rlen(1)
                        end if
                     end if
                  end do
               end if

c              epsilon size part
               if (sctype.gt.0 .and. i.eq.0) then
c                 continue to update epsilon size part
                  do l=jw(nb+k-1)+jw(el+k-1),jldl(k)-1
c                    is U(k,l)<>0?
                     if (jldl(l).eq.k) i=l
                  end do
                  if (i.gt.0) then
                     xp=CONJUG(SKEW(aldl(i)))*aldl(k-1)
                     do l=jldl(k-1),jw(nb+k-1)-1
                        j=jldl(l)
c                       make sure that the diagonal entry does not re-enter again
                        if (j.ne.k) then
                           w(j)=w(j)-xp*aldl(l)
                           if (jw(njw1(1)+j).eq.0) then
                              rlen(1)=rlen(1)+1
                              jw(njw1(1)+rlen(1))=j
                              jw(njw1(1)+j)=rlen(1)
                           end if
                        end if
                     end do
                  end if
               end if
            end if
c           make sure that in the final step no column is loaded anymore
            if (k.ge.nLU) finalrow=1

#if defined PRINT_INFO3 || defined PRINT_INFO2
            do l=1,n
               if (jw(njw1(2)+l).ne.0)
     +              write (6,'(A,I4,I4)') '!!!, ',k-1,l
            end do
            do l=1,n
               if (w(nw+l).ne.FNULL)
     +              write (6,'(A,I4)') '!!!!, ',l
            end do
            if (.not. skip) write (6,'(I6,A)')k-1,' successful'
#endif

         else
c        Second case. We have performed a 2x2 update
c           rewrite norm of U^{-1}
            if (CTOD(w(n+k)).gt.condest1)
     +         write (6,'(A,1P,E8.1,1P,E8.1)') 
     +         'condest1 is too small',CTOD(w(n+k)),condest1
            if (CTOD(w(n+k+1)).gt.condest2)
     +         write (6,'(A,1P,E8.1,1P,E8.1)') 
     +         'condest2 is too small',CTOD(w(n+k+1)),condest2
            w(n+k)  =condest1
            w(n+k+1)=condest2


#if defined PRINT_INFO3 || defined PRINT_INFO2
            do l=1,n
               if (jw(njw1(1)+l).ne.0)
     +              write (6,'(A,I4,I4)') '!!!!!, ',k,l
            end do
            do l=1,n
               if (w(l).ne.FNULL)
     +              write (6,'(A,I4)') '!!!!!!, ',l
            end do
            do l=1,n
               if (jw(njw1(2)+l).ne.0)
     +              write (6,'(A,I4,I4)') '!!!!!!!, ',k,l
            end do
            do l=1,n
               if (w(nw+l).ne.FNULL)
     +              write (6,'(A,I4)') '!!!!!!!!, ',l
            end do
            if (.not. skip) write (6,'(I6,I6,A)')k,k+1,' successful'
#endif


            rlen(1)=0
            rlen(2)=0

c           advance list and pointers for columns of A again
            i=jw(n2+k+1)
c           while i>0         
 263        if (i.le.0) goto 264

c              pointer to A(p(i),q(k+1))
               j=jw(n4+i)
c              column index l of  A(p(i),q(l))
               l=invq(ja(j))
c              store next row from the linked list
               m=jw(n3+i)

c              update column information
               jw(n4+i)=j+1
c              pointer to the next nonzero element in row i
               j=jw(n4+i)
c              we have to make sure that we are still inside row i
               if (j.lt.ia(p(i)+1)) then
c                 column index l of A(p(i),q(l))
                  l=invq(ja(j))
c                 add new entry to the head of the list
                  jw(n3+i)=jw(n2+l)
                  jw(n2+l)=i
               end if

c              recover next row entry
               i=m
            goto 263
c           end while




c           -----   Update Ufirst, Ulist again -----
c

c           Update Ufirst, Ulist 
c
c           n         size of the problem
c           k+1       current step of the update procedure
c           jldl(k)   start of row k+1 of U, regular part shifted by the
c          +jw(nl+k)  number of embedded skipped entries   
c           jldl      index array
c           jw(nb+1)  pointers to the first space behind any row of U
c                     (points behind the space of the regular part)
c           jw(n6+1)  linked list for the nonzeros of U in column k+1
c                     (regular part)
c           jw(na+1)  first nonzero entry in U(i,k+1:n)
c                     (regular part)
 264     call MYILUCLIST(n,k+1,jldl(k)+jw(nl+k),jldl,jw(nb+1),
     +                    jw(n6+1),jw(na+1))



c           Tismenetsky update
            if (sctype.gt.0) then

c              save number of nonzeros in column k+1 (Tismenetsky case)
c              n         size of the problem
c              k+1       current step of the update procedure
c              jw(nb+k)  start of row k+1 of U, epsilon size part shifted by 
c             +jw(el+k)  the number of embedded skipped entries  
c              jldl      index array
c              jldl(2)   pointers to the first space behind any row of U
c                        (points behind the space of the epsilon part)
c              jw(n10+1) linked list for the nonzeros of U in column k+1
c                        (epsilon size part)
c              jw(n8+1)  first nonzero entry in U(i,k+1:n)
c                        (epsilon size part)
               call MYILUCLIST(n,k+1,jw(nb+k)+jw(el+k),jldl,jldl(2),
     +                         jw(n10+1),jw(n8+1))
            end if
            k=k+2

            loadrow=1
            if (k.ge.nLU) finalrow=1
         end if 

#ifdef PRINT_INFO2
         do j=k,n,-1
            write (6,'(A,I4)')'linked list, column ',j
            i=jw(n6+j)
 701        if (i.eq.0) goto 702
               write (6,'(I4)')i
               i=jw(n6+i)
            goto 701
 702     end do

         j=1
 793     if (j.ge.k) goto 794
            write (6,'(I4,A,I4,A,I4,A,I4)')
     +        j,'pointers ',jldl(j),', ',jw(na+j),', ',jw(nb+j)
            if (jldl(n+1+j).eq.0) then
               j=j+1
            else
               j=j+2
            end if
            goto 793
 794     j=1
#endif

      goto 110
c     end while


c     skip last ILU step if there are not at least two rows/columns left
 111  if (k.eq.nLU) nLU=nLU-1

c     if the last step was a 1x1 update then it might be that the buffers
c     have not been cleared yet
      do l=1,n
         jw(l)=0
         w(l)=FNULL
      end do


c     reduction was not successful enough
      if (1.0*nskipped.gt.amgcancel*nLU) then
         nLU=nLU-nskipped
         ierr=0
         iwk=maxiwk
         return
      end if


      if (discardA.gt.0 .and. (simplesc.gt.0 .or. sctype.gt.0)) then 
         do i=1,n
c            write (6,'(10I8)') (invq(ja(j)), j=ia(p(i)),ia(p(i)+1)-1)
c            write (6,'(1P,10E8.1)') (a(j), j=ia(p(i)),ia(p(i)+1)-1)          
         end do
         
c         write (6,'(A)') 'skip'
         do i=1,n
c            if (jw(nc+i).ne.0) write (6,'(I8)') i
         end do
c         write (6,'(A,I8)') 'nLU=',nLU
      end if

c     Check whether the old system matrix A is located in front of
c     LU and if so, whether it can be discarded
      if (discardA.gt.0 .and. (simplesc.gt.0 .or. sctype.gt.0)) then 
c         write (6,'(A)') 'discard matrix'
c        note that since A (i.e. ja,a) is assumed to be in the same chunk of 
c        memory as aldl,jldl we have to physically shift these factors as well

c        scan matrix A and skip (1,1) part
c        these are the rows 1,...,nLU of A(p,p) provided they were not skipped
c        during the factorization. In addition the same principle applies
c        to the column indices
c        jw(n+1:2n) inverse permutation with respect to p
         do i=1,n
            jw(n+p(i))=i
         end do
         l=1
         do i=1,n
c           old shift
            m=shiftA
c           i=p(r)
            r=jw(n+i)
c           this row may be sparsified
            if (r.le.nLU .and. jw(nc+r).eq.0) then
               do j=ia(i),ia(i+1)-1
c                 column index k of A(p(r),q(k))
                  k=invq(ja(j))
c                 these entries can be skipped
                  if (k.le.nLU .and. jw(nc+k).eq.0) then
c                    increase number of skipped entries
                     shiftA=shiftA+1
                  else
c                    shift remaining entries
                     ja(l-shiftA)=ja(l)
                     a (l-shiftA)= a(l)
                  end if
                  l=l+1
               end do
            else
c              shift all entries in this row
               do j=ia(i),ia(i+1)-1
                  ja(l-shiftA)=ja(l)
                  a (l-shiftA)= a(l)
                  l=l+1
               end do
            end if
c           advance beginning of current row by previous shift
            ia(i)=ia(i)-m
         end do
c        advance beginning of next row by final shift
         ia(n+1)=ia(n+1)-shiftA

c        now physically shift U-factor
         l=jldl(nLU+1)-1
         do j=1,l
            jldl(j-shiftA)=jldl(j)
            aldl(j-shiftA)=aldl(j)
         end do
      end if



      if (discardA.gt.0 .and. (simplesc.gt.0 .or. sctype.gt.0)) then 
         do i=1,n
c            write (6,'(10I8)') (invq(ja(j)), j=ia(p(i)),ia(p(i)+1)-1)
c            write (6,'(1P,10E8.1)') (a(j), j=ia(p(i)),ia(p(i)+1)-1)          
         end do
      end if


      maxcondestold=maxcondest
      if (aggressive.gt.0) then 
         write (6,'(A,1P,E8.1)')'maxcondest=',maxcondest

c        compute norm of the (1,2) block of U^{-1}
c        clear unused norm of the inverse for the skipped entries
c         do k=1,nLU
c            if (jw(nc+k).gt.0) w(n+k)=FNULL
c         end do
c         k=1
cc        while k<=nLU
c 601     if (k.gt.nLU) goto 602
c            if (jldl(n+1+k-shiftA).eq.0) then
c               do j=jldl(k-shiftA),jldl(k-shiftA)+jw(nl+k)-1
cc                 column index i of U(k,i), this is a skipped entry
c                  i=jldl(j-shiftA)
cc                 add ||U^{-1}(:,k)|| * |D(k,k)^{-1}*U(k,i)|
c                  w(n+i)=max(CTOD(w(n+i)),
c     +                 ABS(aldl(j-shiftA)*aldl(k-shiftA))*CTOD(w(n+k)))
c               end do
c               k=k+1
c            else
c               U11=aldl(k-shiftA)
c               U12=aldl(n+1+k-shiftA)
c               U21=CONJUG(SKEW(U12))
c               U22=aldl(k+1-shiftA)
c               jj=jldl(k-shiftA)
c               do j=jldl(k-shiftA),jldl(k-shiftA)+jw(nl+k)-1
cc                 column index i of U(k,i), this is a skipped entry
c                  i=jldl(j-shiftA)
cc                 add ||U^{-1}(:,k:k+1)|| * |D(k:k+1,kk:k+1)^{-1}*U(k:k+1,i)|
c                  w(n+i)=max(CTOD(w(n+i)),
c     +               ABS(U11*aldl(jj-shiftA)+U12*aldl(jj+1-shiftA))*CTOD(w(n+k)))
c                  w(n+i)=max(CTOD(w(n+i)),
c     +               ABS(U21*aldl(jj-shiftA)+U22*aldl(jj+1-shiftA))*CTOD(w(n+k+1)))
c                  jj=jj+2
c               end do
c               k=k+2
c            end if
c         goto 601
c 602     continue
c         do k=1,nLU
c            if (jw(nc+k).gt.0) write (6,'(1P,E8.1)')w(n+k)
c         end do


c        backward solve with U in order to find out if the
c        inverse norm estimate by COLUMNS fits with the inverse
c        norm estimate by ROWS
c        ((L+D)D^{-1})^{-T}b=((D^{-1}L^T+I))^{-1}b
         i=nLU
c        while i>=1
 190     if (i.lt.1) goto 191
            if (jw(nc+i).eq.0) then
c              scalar product row i and w(n+1:n+nLU)
               xp=FNULL
               if (i.gt.1 .and. jldl(n+1+i-1-shiftA).gt.0) then
c                 2x2 case
                  xp2=FNULL
                  axp=0.0
                  axm=0.0
                  i=i-1
                  ii=jldl(i-shiftA)
     +              +2*(jldl(i-shiftA)+jw(nl+i)-jldl(i-shiftA))
c                 scalar product rows i,i+1 and w(n+1:n+nLU)
                  do k=jldl(i-shiftA)+jw(nl+i),jw(nb+i)-1
                     xp =xp +aldl(ii-shiftA)  *w(n+jldl(k-shiftA))
                     xp2=xp2+aldl(ii+1-shiftA)*w(n+jldl(k-shiftA))
                     axp=axp+ABS
     &            (aldl(i-shiftA)*aldl(ii-shiftA)
     &            +aldl(n+1+i-shiftA)*aldl(ii+1-shiftA))
                     axm=axm+ABS
     &            (CONJUG(aldl(n+1+i-shiftA))
     &            *aldl(ii-shiftA)
     &            +aldl(i+1-shiftA)*aldl(ii+1-shiftA))
                     ii=ii+2
                  end do
                  maxcondest=max(maxcondest,axp)
                  maxcondest=max(maxcondest,axm)
                  xm =xp
                  xm2=xp2
                  xp =aldl(i-shiftA)                  *xm
     +               +aldl(n+1+i-shiftA)*xm2
                  xp2=CONJUG(SKEW(aldl(n+1+i-shiftA)))*xm
     +               +aldl(i+1-shiftA)  *xm2
                  signum =FONE
                  signum2=FONE
#if !defined _DOUBLE_REAL_ && !defined _SINGLE_REAL_
                  if (xp.ne.FNULL)  signum =xp/ABS(xp)
                  if (xp2.ne.FNULL) signum2=xp2/ABS(xp2)
#else 
#endif
                  xp = SIGNUM -xp
                  xm =-SIGNUM -xp
                  xp2= SIGNUM2-xp2
                  xm2=-SIGNUM2-xp2
                  axp=ABS(xp)
                  axm=ABS(xm)
                  axp2=ABS(xp2)
                  axm2=ABS(xm2)
                  if (axp.ge.axm) then
                     w(n+i)=xp
                  else
                     w(n+i)=xm
                  end if
                  maxcondest=max(maxcondest,ABS(w(n+i)))
                  if (axp2.ge.axm2) then
                     w(n+i+1)=xp2
                  else
                     w(n+i+1)=xm2
                  end if
                  maxcondest=max(maxcondest,ABS(w(n+i+1)))
               else
                  do k=jldl(i-shiftA)+jw(nl+i),jw(nb+i)-1
c                     write(6,'(i4,a,i4)') i,',',jldl(k-shiftA)
                     xp=xp+aldl(k-shiftA)*w(n+jldl(k-shiftA))
                  end do
                  xp=aldl(i-shiftA)*xp
                  signum=FONE
#if !defined _DOUBLE_REAL_ && !defined _SINGLE_REAL_
                  if (xp.ne.FNULL) signum=xp/ABS(xp)
#else
#endif
                  xp= SIGNUM-xp
                  xm=-SIGNUM-xp
                  axp=ABS(xp)
                  axm=ABS(xm)
                  if (axp.ge.axm) then
                     w(n+i)=xp
                  else
                     w(n+i)=xm
                  end if
c                  write(6,'(1P,E8.1)') ABS(w(n+i))
                  maxcondest=max(maxcondest,ABS(w(n+i)))
                  k=1
                  j=jw(nb+i)-jldl(i-shiftA)-jw(nl+i)
                  axp=ASUM(j,aldl(jldl(i-shiftA)+jw(nl+i)-shiftA),k)
                  maxcondest=max(maxcondest,ABS(aldl(i-shiftA))*axp)
               end if
            end if
            i=i-1
         goto 190
c        end while
 191     continue

         write (6,'(A,1P,E8.1)')'maxcondest=',maxcondest

c        apply aggressive dropping to U
c        n              size of the problem
c        k-1            current finished step of the update
c        aldl           numerical values
c        jldl           associated indices
c        jw(nb+1)       pointers to the first space behind any
c                       row of U (behind the regular entries)
c        jw(na+1)       Ufirst for the regular part of U
c        jw(n8+1)       Ufirst for the epsilon size part of U
c        jw(nl+1)       number of skipped entries, located at the
c                       start of every column
c        jw(el+1)       number of skipped (epsilon size) entries,
c                       located at the start of every row
c        maxcondest     inverse estimate
c        droptols(1)    
c        sctype         indicate whether Tismenetsky update is used
         call AGGRESSIVEDROPPING(n,nLU,
     +                           aldl(1-shiftA),jldl(1-shiftA),
     +                           jw(nb+1),
     +                           jw(na+1),jw(n8+1),jw(nl+1),
     +                           jw(el+1),maxcondest,droptols(1),
     +                           sctype)



c        now reorder the remaining entries in the (1,1) part in increasing
c        order. This is needed to compute a much more reliable norm 
c        estimate for each row of U^{-1}. Note that the data structures of
c        U do not properly fit for backward substitution if the right and side
c        is created dynamically and the solution has to be computed via 
c        rank-1 updates. Since U is stored by rows but at the same time we need
c        to access U by columns we again exploit the linked lists
         i=1
c        while i<=nLU
 603     if (i.gt.nLU) goto 604
            if (jldl(n+1+i-shiftA).eq.0) then
               j=jldl(i-shiftA)+jw(nl+i)
               k=jw(nb+i)-j
               call QQSORTS(aldl(j-shiftA),jldl(j-shiftA),jw(n+1),k)
               i=i+1
            else
c               write (6,'(10I8)')(jldl(j-shiftA),j=jldl(i-shiftA)+jw(nl+i),jw(nb+i)-1)
c               write (6,'(1P,10E8.1)')(aldl(jldl(i-shiftA)+2*(j-jldl(i-shiftA))-shiftA),
c     +                j=jldl(i-shiftA)+jw(nl+i),jw(nb+i)-1)
c               write (6,'(1P,10E8.1)')(aldl(jldl(i-shiftA)+1+2*(j-jldl(i-shiftA))-shiftA),
c     +                j=jldl(i-shiftA)+jw(nl+i),jw(nb+i)-1)
               j=jldl(i-shiftA)+jw(nl+i)
               jj=jldl(i-shiftA)+2*(j-jldl(i-shiftA))
               k=jw(nb+i)-j
               call QQSORTS2(aldl(jj-shiftA),jldl(j-shiftA),jw(n+1),k)
c               write (6,'(10I8)')(jldl(j-shiftA),j=jldl(i-shiftA)+jw(nl+i),jw(nb+i)-1)
c               write (6,'(1P,10E8.1)')(aldl(jldl(i-shiftA)+2*(j-jldl(i-shiftA))-shiftA),
c     +                j=jldl(i-shiftA)+jw(nl+i),jw(nb+i)-1)
c               write (6,'(1P,10E8.1)')(aldl(jldl(i-shiftA)+1+2*(j-jldl(i-shiftA))-shiftA),
c     +                j=jldl(i-shiftA)+jw(nl+i),jw(nb+i)-1)
               i=i+2
            end if
         goto 603
 604     continue


c        set up linked list to access the columns of U correctly,
c        starting with row n and going backwards
c
c        jw(n+1:2n)  head of the linked list for the columns of U
c         
c        jw(6n+1:7n) linked list for the terminating columns of U at 
c                    step k starting from the last column
c        
c        jw(1:n)     pointer to the last entry at step k

c        SKETCH:
c        0) clear arrays
c        1) set up linked lists
c        2) start with the last column of U and go backwards
c           In each step k we first
c           2.1) extract column U(:,k), compute the regular estimate
c           2.2) extract column U(:,k), compute the improved regular estimate
c           2.3) advance the pointers for the linked list
c        3) clear arrays again if needed



c         i=1
cc        while i<=nLU
c 693     if (i.gt.nLU) goto 694
c            if (jldl(n+1+i-shiftA).eq.0) then
c               write (6,'(I4,A)')i,':'
c               write (6,'(10I8)')(jldl(j-shiftA),j=jldl(i-shiftA)+jw(nl+i),jw(nb+i)-1)
c               write (6,'(1P,10E8.1)')(aldl(j-shiftA),
c     +                j=jldl(i-shiftA)+jw(nl+i),jw(nb+i)-1)
c               i=i+1
c            else
c               write (6,'(I4,A,I4,A)')i,':',i+1,':'
c               write (6,'(10I8)')(jldl(j-shiftA),j=jldl(i-shiftA)+jw(nl+i),jw(nb+i)-1)
c               write (6,'(1P,10E8.1)')(aldl(jldl(i-shiftA)+2*(j-jldl(i-shiftA))-shiftA),
c     +                j=jldl(i-shiftA)+jw(nl+i),jw(nb+i)-1)
c               write (6,'(1P,10E8.1)')(aldl(jldl(i-shiftA)+1+2*(j-jldl(i-shiftA))-shiftA),
c     +                j=jldl(i-shiftA)+jw(nl+i),jw(nb+i)-1)
c               i=i+2
c            end if
c         goto 693
c 694     continue





         do k=1,nLU
c           clear head of the linked list (empty) 
            jw(n+k)=0
c           clear inverse information
            w(n+k)=FNULL
c            if (jw(nc+k).eq.0) w(n+k)=FNULL
         end do
         if (improved.gt.0) then
            do k=1,nLU
c              clear improved inverse information
               w(n2+k)=FNULL
            end do
         end if

         i=nLU
c        while i>=1
 617     if (i.lt.1) goto 618
c           2x2 case
            if (i.gt.1 .and. jldl(n+1+i-1-shiftA).ne.0) then
               i=i-1
            end if
c           pointer to the last nonzero element in row i
            j=jw(nb+i)-1
c           column index k of U(i,k)
            k=jldl(j-shiftA)
c           we have to make sure that we are still inside row i
            if (j.ge.jldl(i-shiftA)+jw(nl+i)) then
c              pointer to the current last nonzero entry in row i
               jw(i)=j
c              add new entry to the head of the list
               jw(n6+i)=jw(n+k)
               jw(n+k)=i
            end if
            i=i-1
         goto 617
c        end while
 618     continue


c        first compute the norms of the (1,2) block
         axp=min(condest,maxcondest)
         k=1
         l=1
c        while k<=nLU
 601     if (k.gt.nLU) goto 602
            j=jldl(k-shiftA)
            i=jldl(k-shiftA)+jw(nl+k)-j
c           1x1 case
            if (jldl(n+1+k-shiftA).eq.0) then
               if (i.gt.0) then
                  w(n+k)=ASUM(i,aldl(j-shiftA),l)
                  w(n+k)=w(n+k)*ABS(aldl(k-shiftA))*axp
                  if (improved.gt.0) w(n2+k)=w(n+k)
               end if
               k=k+1
            else
c              2x2 case
               if (i.gt.0) then
                  U11=aldl(k-shiftA)
                  U12=aldl(n+1+k-shiftA)
                  U21=CONJUG(SKEW(U12))
                  U22=aldl(k+1-shiftA)
                  jj=jldl(k-shiftA)
                  do j=jldl(k-shiftA),jldl(k-shiftA)+jw(nl+k)-1
c                    add ||U^{-1}(:,k:k+1)|| * |D(k:k+1,kk:k+1)^{-1}*U(k:k+1,i)|
                     w(n+k)=w(n+k)+
     +                ABS(U11*aldl(jj-shiftA)+U12*aldl(jj+1-shiftA))
                     w(n+k+1)=w(n+k+1)+
     +                ABS(U21*aldl(jj-shiftA)+U22*aldl(jj+1-shiftA))
                     jj=jj+2
                  end do
                  w(n+k)  =w(n+k)*axp
                  w(n+k+1)=w(n+k+1)*axp
                  if (improved.gt.0) then 
                     w(n2+k)  =w(n+k)
                     w(n2+k+1)=w(n+k+1)
                  end if
               end if
               k=k+2
            end if
         goto 601
 602     continue

c        main loop for backward solve with U
c        This is done by successive rank-1 updates
         do k=nLU,1,-1

c           access column k of U
c           if U(1:k-1,k) is nonempty, get the entry point to the
c           linked list of row k
            signum=FONE
#if !defined _DOUBLE_REAL_ && !defined _SINGLE_REAL_
            j=jw(n+k)
            mup=RZERO
            m=j
c           while j>0         
 630        if (j.le.0) goto 640
c              pointer to U(j,k) (resp. U(j:j+1,k))
               i=jw(j)
               ii=jldl(j-shiftA)+2*(i-jldl(j-shiftA))
c              we have to make sure that we are still inside row j
               if (i.ge.jldl(j-shiftA)+jw(nl+j)) then 
                  if (jldl(n+1+j-shiftA).eq.0) then
c                    1x1 case
                     xp=aldl(j-shiftA)*aldl(i-shiftA)
                     xm=w(n+j)-xp*w(n+j)
                     axp=ABS(xm)+ABS(xp)
                     if (axp.gt.mup) then 
                        mup=axp
                        m=j
                     end if
                  else 
c                    2x2 case
                     xp =aldl(j-shiftA)    *aldl(ii-shiftA)
     +                  +aldl(n+1+j-shiftA)*aldl(ii+1-shiftA)
                     xp2=CONJUG(SKEW(aldl(n+1+j-shiftA)))
     +                  *aldl(ii-shiftA)
     +                  +aldl(j+1-shiftA)                
     +                  *aldl(ii+1-shiftA)
                     xm =w(n+j)  -xp *w(n+j)
                     xm2=w(n+j+1)-xp2*w(n+j+1)
                     axp =ABS(xm) +ABS(xp)
                     axp2=ABS(xm2)+ABS(xp2)
                     if (axp.gt.mup) then 
                        mup=axp
                        m=j
                     end if
                     if (axp2.gt.mup) then 
                        mup=axp2
                        m=-j
                     end if
                  end if
               end if
c              advance to next row
               j=jw(n6+j)
            goto 630
c           end while
c           choose the sign such that component j is maximized
 640        if (m.gt.0) then
               j=m
            else
               j=-m
            end if
            i=jw(j)
            if (j.gt.0) then
               if (jldl(n+1+j-shiftA).eq.0) then
c                 1x1 case
                  xp=aldl(j-shiftA)*aldl(i-shiftA)
                  xm=w(n+j)-xp*w(n+j)
                  if (xp.ne.FNULL .and. 
     +                xm.ne.FNULL) then
                     signum=(xm/ABS(xm))*(CONJ(xp)/ABS(xp))
                  end if
               else 
c                 2x2 case
                  if (m.gt.0) then
                     xp=aldl(j-shiftA)    *aldl(ii-shiftA)
     +                 +aldl(n+1+j-shiftA)*aldl(ii+1-shiftA)
                     xm=w(n+j)-  xp*w(n+j)
                  else
                     xp=CONJUG(SKEW(aldl(n+1+j-shiftA)))
     +                 *aldl(ii-shiftA)
     +                 +aldl(j+1-shiftA)                
     +                 *aldl(ii+1-shiftA)
                     xm=w(n+j+1)-xp*w(n+j+1)
                  end if
                  if (xp.ne.FNULL .and. 
     +                xm.ne.FNULL) then
                     signum=(xm/ABS(xm))*(CONJ(xp)/ABS(xp))
                  end if
               end if
            end if
#else
#endif
c           try +/-1 as k-th component of the right hand side
            xp=( SIGNUM-w(n+k))
            xm=(-SIGNUM-w(n+k))
c           for both cases estimate the 1-norm of U^{-1} by evaluating
c           U(1:k-1,k:n)*U(k:n,k:n)^{-1}*rhs(k:n)
            mup=RZERO 
            mum=RZERO
            axp=0.0
            j=jw(n+k)
c           while j>0         
 632        if (j.le.0) goto 642
c               write (6,'(I4,I4)') j,k
c              pointer to U(j,k)
               i=jw(j)
               ii=jldl(j-shiftA)+2*(i-jldl(j-shiftA))
c              we have to make sure that we are still inside row j
               if (i.ge.jldl(j-shiftA)+jw(nl+j)) then 
                  if (jldl(n+1+j-shiftA).eq.0) then
c                    1x1 case
c                    D(j,j)^{-1}*U(j,k)
                     x=aldl(j-shiftA)*aldl(i-shiftA)
                     mup=mup+ABS(w(n+j)+x*xp)
                     mum=mum+ABS(w(n+j)+x*xm)
                     axp=axp+ABS(x)
                  else
c                    2x2 case
c                    D(j:j+1,j:j+1)^{-1}*U(j:j+1,k)
                     x =aldl(j-shiftA)    *aldl(ii-shiftA)
     +                 +aldl(n+1+j-shiftA)*aldl(ii+1-shiftA)
                     x2=CONJUG(SKEW(aldl(n+1+j-shiftA)))
     +                 *aldl(ii-shiftA)
     +                 +aldl(j+1-shiftA)                
     +                 *aldl(ii+1-shiftA)
                     mup=mup+ABS(w(n+j)  +x *xp)
     +                      +ABS(w(n+j+1)+x2*xp)
                     mum=mum+ABS(w(n+j)  +x *xm)
     +                      +ABS(w(n+j+1)+x2*xm)
                     axp=axp+ABS(x)+ABS(x2)
                  end if
               end if
               j=jw(n6+j)
            goto 632
c           end while
 642        if (mup.ge.mum) then
               x=xp
            else
               x=xm
            end if
c           local largest norm of U^{-1}(k,:)
            if (jw(nc+k).eq.0) then 
               w(n+k)=max(ABS(xp),ABS(xm))
               w(n+k)=max(CTOD(w(n+k)),axp)
               maxcondest=max(maxcondest,CTOD(w(n+k)))
            end if
            j=jw(n+k)
c           while j>0         
 635        if (j.le.0) goto 645
c              pointer to U(j,k)
               i=jw(j)
               ii=jldl(j-shiftA)+2*(i-jldl(j-shiftA))
c              we have to make sure that we are still inside row j
               if (i.ge.jldl(j-shiftA)+jw(nl+j)) then 
                  if (jldl(n+1+j-shiftA).eq.0) then
c                    1x1 case
                     w(n+j)=w(n+j)+aldl(j-shiftA)*aldl(i-shiftA)*x
                  else
c                    2x2 case
                     w(n+j)  =w(n+j)
     +                       +(aldl(j-shiftA)    *aldl(ii-shiftA)
     +                       + aldl(n+1+j-shiftA)*aldl(ii+1-shiftA))*x
                     w(n+j+1)=w(n+j+1)
     +                       +(CONJUG(SKEW(aldl(n+1+j-shiftA)))
     +                       *aldl(ii-shiftA)
     +                       + aldl(j+1-shiftA)                
     +                       *aldl(ii+1-shiftA))*x
                  end if
               end if
               j=jw(n6+j)
            goto 635
c           end while

c           improved estimate using a different right hand side
 645        if (improved.gt.0) then
               signum=FONE
#if !defined _DOUBLE_REAL_ && !defined _SINGLE_REAL_
               j=jw(n+k)
               mup=RZERO
               m=j
c              while j>0         
 631           if (j.le.0) goto 641
c                 pointer to U(j,k) (resp. U(j:j+1,k))
                  i=jw(j)
                  ii=jldl(j-shiftA)+2*(i-jldl(j-shiftA))
c                 we have to make sure that we are still inside row j
                  if (i.ge.jldl(j-shiftA)+jw(nl+j)) then 
                     if (jldl(n+1+j-shiftA).eq.0) then
c                       1x1 case
                        xp=aldl(j-shiftA)*aldl(i-shiftA)
                        xm=w(n2+j)-xp*w(n2+j)
                        axp=ABS(xm)+ABS(xp)
                        if (axp.gt.mup) then 
                           mup=axp
                           m=j
                        end if
                     else 
c                       2x2 case
                        xp =aldl(j-shiftA)    *aldl(ii-shiftA)
     +                     +aldl(n+1+j-shiftA)*aldl(ii+1-shiftA)
                        xp2=CONJUG(SKEW(aldl(n+1+j-shiftA)))
     +                     *aldl(ii-shiftA)
     +                     +aldl(j+1-shiftA)                
     +                     *aldl(ii+1-shiftA)
                        xm =w(n2+j)-  xp *w(n2+j)
                        xm2=w(n2+j+1)-xp2*w(n2+j+1)
                        axp =ABS(xm) +ABS(xp)
                        axp2=ABS(xm2)+ABS(xp2)
                        if (axp.gt.mup) then 
                           mup=axp
                           m=j
                        end if
                        if (axp2.gt.mup) then 
                           mup=axp2
                           m=-j
                        end if
                     end if
                  end if
c                 advance to next row
                  j=jw(n6+j)
               goto 631
c              end while
c              choose the sign such that component j is maximized
 641           if (m.gt.0) then
                  j=m
               else
                  j=-m
               end if
               i=jw(j)
               if (j.gt.0) then
                  if (jldl(n+1+j-shiftA).eq.0) then
c                    1x1 case
                     xp=aldl(j-shiftA)*aldl(i-shiftA)
                     xm=w(n2+j)-xp*w(n2+j)
                     if (xp.ne.FNULL .and. 
     +                   xm.ne.FNULL) then
                        signum=(xm/ABS(xm))*(CONJ(xp)/ABS(xp))
                     end if
                  else 
c                    2x2 case
                     if (m.gt.0) then
                        xp=aldl(j-shiftA)    *aldl(ii-shiftA)
     +                    +aldl(n+1+j-shiftA)*aldl(ii+1-shiftA)
                        xm=w(n2+j)-  xp*w(n2+j)
                     else
                        xp=CONJUG(SKEW(aldl(n+1+j-shiftA)))
     +                    *aldl(ii-shiftA)
     +                    +aldl(j+1-shiftA)                
     +                    *aldl(ii+1-shiftA)
                        xm=w(n2+j+1)-xp*w(n2+j+1)
                     end if
                     if (xp.ne.FNULL .and. 
     +                   xm.ne.FNULL) then
                        signum=(xm/ABS(xm))*(CONJ(xp)/ABS(xp))
                     end if
                  end if
               end if
#else
#endif
c              try +/-1 as k-th component of the right hand side
               xp=( SIGNUM-w(n2+k))
               xm=(-SIGNUM-w(n2+k))
c              for both cases count the number of serious changes by
c              evaluating U(1:k-1,k:n)*U(k:n,k:n)^{-1}*rhs(k:n)
               jj=0
               m=0
               j=jw(n+k)
c              while j>0         
 633           if (j.le.0) goto 643
c                 pointer to U(j,k)
                  i=jw(j)
                  ii=jldl(j-shiftA)+2*(i-jldl(j-shiftA))
c                 we have to make sure that we are still inside row j
                  if (i.ge.jldl(j-shiftA)+jw(nl+j)) then 
                     if (jldl(n+1+j-shiftA).eq.0) then
c                       1x1 case
c                       D(j,j)^{-1}*U(j,k)
                        x=aldl(j-shiftA)*aldl(i-shiftA)
                        ax=ABS(w(n2+j))
                        ay=ABS(w(n2+j)+x*xp)
c                       entry seriously increases                  
                        if (ay.gt.RTWO*ax .and. ay.gt.0.5)  jj=jj+1
c                       entry seriously decreases              
                        if (RTWO*ay.lt.ax .and. ax.gt.0.5)  jj=jj-1
                        ay=ABS(w(n2+j)+x*xm)
c                       entry seriously increases              
                        if (ay.gt.RTWO*ax .and. ay.gt.0.5)  m=m+1
c                       entry seriously decreases              
                        if (RTWO*ay.lt.ax .and. ax.gt.0.5)  m=m-1
                     else
c                       2x2 case
c                       D(j:j+1,j:j+1)^{-1}*U(j:j+1,k)
                        x =aldl(j-shiftA)    *aldl(ii-shiftA)
     +                    +aldl(n+1+j-shiftA)*aldl(ii+1-shiftA)
                        x2=CONJUG(SKEW(aldl(n+1+j-shiftA)))
     +                    *aldl(ii-shiftA)
     +                    +aldl(j+1-shiftA)                
     +                    *aldl(ii+1-shiftA)

                        ax=ABS(w(n2+j))
                        ay=ABS(w(n2+j)+x*xp)
c                       entry seriously increases                  
                        if (ay.gt.RTWO*ax .and. ay.gt.0.5)  jj=jj+1
c                       entry seriously decreases              
                        if (RTWO*ay.lt.ax .and. ax.gt.0.5)  jj=jj-1
                        ay=ABS(w(n2+j)+x*xm)
c                       entry seriously increases              
                        if (ay.gt.RTWO*ax .and. ay.gt.0.5)  m=m+1
c                       entry seriously decreases              
                        if (RTWO*ay.lt.ax .and. ax.gt.0.5)  m=m-1

                        ax=ABS(w(n2+j+1))
                        ay=ABS(w(n2+j+1)+x2*xp)
c                       entry seriously increases                  
                        if (ay.gt.RTWO*ax .and. ay.gt.0.5)  jj=jj+1
c                       entry seriously decreases              
                        if (RTWO*ay.lt.ax .and. ax.gt.0.5)  jj=jj-1
                        ay=ABS(w(n2+j+1)+x2*xm)
c                       entry seriously increases              
                        if (ay.gt.RTWO*ax .and. ay.gt.0.5)  m=m+1
c                       entry seriously decreases              
                        if (RTWO*ay.lt.ax .and. ax.gt.0.5)  m=m-1
                     end if
                  end if
                  j=jw(n6+j)
               goto 633
c              end while
 643           if (jj.ge.m) then
                  x=xp
               else
                  x=xm
               end if
               axp=max(ABS(xp),ABS(xm))
c              local largest norm of U^{-1}(k,:)
               if (jw(nc+k).eq.0) then 
                  w(n+k)=max(CTOD(w(n+k)),axp)
                  maxcondest=max(maxcondest,CTOD(w(n+k)))
               end if
               j=jw(n+k)
c              while j>0         
 634           if (j.le.0) goto 644
c                 pointer to U(j,k)
                  i=jw(j)
                  ii=jldl(j-shiftA)+2*(i-jldl(j-shiftA))
c                 we have to make sure that we are still inside row j
                  if (i.ge.jldl(j-shiftA)+jw(nl+j)) then 
                     if (jldl(n+1+j-shiftA).eq.0) then
c                       1x1 case
                        w(n2+j)=w(n2+j)+aldl(j-shiftA)*aldl(i-shiftA)*x
                     else
c                       2x2 case
                        w(n2+j)  =w(n2+j)
     +                       +(aldl(j-shiftA)    *aldl(ii-shiftA)
     +                       + aldl(n+1+j-shiftA)*aldl(ii+1-shiftA))*x
                        w(n2+j+1)=w(n2+j+1)
     +                       +(CONJUG(SKEW(aldl(n+1+j-shiftA)))
     +                       *aldl(ii-shiftA)
     +                       + aldl(j+1-shiftA)                
     +                       *aldl(ii+1-shiftA))*x
                     end if
                  end if
                  j=jw(n6+j)
               goto 634
c              end while

c              advance linked list to the previous column
 644           j=jw(n+k)
c              while j>0         
 636           if (j.le.0) goto 646
c                 store previous column from the linked list
                  m=jw(n6+j)
c                 downdate pointer to the current last entry in row j
                  jw(j)=jw(j)-1
c                 pointer to the previous nonzero element U(j,l) in row j
                  i=jw(j)
c                 row index l of U(j,l)
                  l=jldl(i-shiftA)
c                 we have to make sure that we are still inside row j
                  if (i.ge.jldl(j-shiftA)+jw(nl+j)) then 
c                    add new entry to the head of the list
                     jw(n6+j)=jw(n+l)
                     jw(n+l)=j
                  end if

c                 recover previous column entry
                  j=m
               goto 636
c              end while
 646           j=j
            end if
         end do
c        end while, main loop
         do k=1,n
            jw(k)=0
            w(k)=FNULL
         end do

c         do k=1,nLU
c            if (jw(nc+k).gt.0) write (6,'(1P,E8.1)')w(n+k)
c         end do


c         k=1
cc        while k<=nLU
c 609     if (k.gt.nLU) goto 611
c            if (jldl(n+1+k-shiftA).eq.0) then
c               do j=jldl(k-shiftA),jldl(k-shiftA)+jw(nl+k)-1
cc                 column index i of U(k,i), this is a skipped entry
c                  i=jldl(j-shiftA)
cc                 add ||U^{-1}(:,k)|| * |D(k,k)^{-1}*U(k,i)|
c                  w(i)=max(CTOD(w(i)),ABS(aldl(j-shiftA)*aldl(k-shiftA)))
c               end do
c               k=k+1
c            else
c               U11=aldl(k-shiftA)
c               U12=aldl(n+1+k-shiftA)
c               U21=CONJUG(SKEW(U12))
c               U22=aldl(k+1-shiftA)
c               jj=jldl(k-shiftA)
c               do j=jldl(k-shiftA),jldl(k-shiftA)+jw(nl+k)-1
cc                 column index i of U(k,i), this is a skipped entry
c                  i=jldl(j-shiftA)
cc                 add ||U^{-1}(:,k)|| * |D(k,k)^{-1}*U(k,i)|
c                  w(i)=max(CTOD(w(i)),
c     +                 ABS(U11*aldl(jj-shiftA)+U12*aldl(jj+1-shiftA)))
c                  w(i)=max(CTOD(w(i)),
c     +                 ABS(U21*aldl(jj-shiftA)+U22*aldl(jj+1-shiftA)))
c                  jj=jj+2
c               end do
c               k=k+2
c            end if
c         goto 609
c 611     continue
c
c         k=1
cc        while k<=nLU
c 613     if (k.gt.nLU) goto 614
c            axp=0.0
c            axm=0.0
c            do j=jldl(k-shiftA),jldl(k-shiftA)+jw(nl+k)-1
c               i=jldl(j-shiftA)
c               axp=axp+CTOD(w(n+i))
c               axm=axm+CTOD(w(i))
c            end do
c            if (jldl(n+1+k-shiftA).eq.0) then
c               axp=min(axp,axm*CTOD(w(n+k)))
cc               if (jw(nc+k).eq.0)
cc     +            write (6,'(1P,E8.1,1P,E8.1)')condest*axp,CTOD(w(n+k))
cc              speculate that the maximum norm of the coarse part 
cc              is of the same order as the a priori bound condest or the 
cc              computed real bound maxcondest, if this is better
c               axm=min(condest,maxcondest)
c               w(n+k)=max(CTOD(w(n+k)),axm*axp)
c               w(n+k)=min(CTOD(w(n+k)),maxcondest)
c               k=k+1
c            else
c               axp2=min(axp,axm*CTOD(w(n+k)))
cc               if (jw(nc+k).eq.0)
cc     +            write (6,'(1P,E8.1,1P,E8.1)')condest*axp,CTOD(w(n+k))
cc              speculate that the maximum norm of the coarse part 
cc              is of the same order as the a priori bound condest or the 
cc              computed real bound maxcondest, if this is better
c               axm2=min(condest,maxcondest)
c               w(n+k)=max(CTOD(w(n+k)),axm2*axp2)
c               w(n+k)=min(CTOD(w(n+k)),maxcondest)
c               axp2=min(axp,axm*CTOD(w(n+k+1)))
c               axm2=min(condest,maxcondest)
c               w(n+k+1)=max(CTOD(w(n+k+1)),axm2*axp2)
c               w(n+k+1)=min(CTOD(w(n+k+1)),maxcondest)
c               k=k+2
c            end if
c         goto 613
c 614     continue
c
c         do k=1,n
c            w(k)=FNULL
c         end do

         write (6,'(A,1P,E8.1)')'maxcondest=',maxcondest

         call FAGGRESSIVEDROPPING(n,nLU,
     +                            aldl(1-shiftA),jldl(1-shiftA),
     +                            jw(nb+1),
     +                            jw(na+1),jw(n8+1),jw(nl+1),
     +                            jw(el+1),w(n+1),droptols(1),
     +                            sctype)
      end if



c     skip selected columns
      l=1
      m=1
      do k=1,nLU
c        this entry is a non-skipped entry
         if (jw(nc+k).eq.0) then
c           U part, pointers
            jldl(l-shiftA)=jldl(k-shiftA)
c           U part, block size
            jldl(n+1+l-shiftA)=jldl(n+1+k-shiftA)

c           (block) diagonal part, numerical values 
            aldl(l-shiftA)    =aldl(k-shiftA)
            aldl(n+1+l-shiftA)=aldl(n+1+k-shiftA)

c           pointers to the actual first element in U (regular parts)
            jw(na+l)=jw(na+k)

c           pointers to the actual first element in U (epsilon size parts)
            if (sctype.gt.0) then
               jw(n8+l)=jw(n8+k)
            end if

c           pointers to the space behind U (regular parts)
            jw(nb+l)=jw(nb+k)

c           store how many components a non-skipped entry has to advance
c           this gives the new label after reordering
            jw(n+k)=l
            l=l+1
         else
c           diagonal part, skipped numerical values 
            w(m)   =aldl(k-shiftA)

c           new position of k after (sym.) permutation
            jw(n+k)=nLU-nskipped+m
            m=m+1
         end if
      end do
c     shift final reference
      jldl(l-shiftA)=jldl(nLU+1-shiftA)
      do m=1,nskipped
c        diagonal part, skipped numerical values 
         aldl(nLU-nskipped+m-shiftA)=w(m)
         w(m)=FNULL
      end do
      do k=nLU+1,n
c        remaining entries stay where they are
         jw(n+k)=k
c        add the last n-nLU entries to the skipped part
         nskipped=nskipped+1
         jw(nc+k)=nskipped
      end do


      nLU=n-nskipped
      if (nLU.eq.0) then 
         return
      end if
    




c     remove the embedded small and skipped entries
#if defined PRINT_INFO || defined PRINT_INFO2
      do k=1,nLU+1
         write (6,'(A,I4,A,I4,I4)') 
     +        'row ',k,', previous starts: ',jldl(k-shiftA),jw(nb+k)
      end do
#endif

  
c     list to the nonzero elements in column k of U, regular part (although
c     U is stored by rows). 
c     jw(n6+k)    contains the reference of the first nonzero entry,
c                 jw(n6+jw(n6+k)) the second one and so on 
c                 ... until a zero is reached which indicates the end
      do k=1,n
         jw(n6+k)=0
      end do
c     additional Tismenetsky updates
c     list to the nonzero elements in row k of L, epsilon size part
c     (although L is stored by columns). 
c     jw(n10+k)    contains the reference of the first nonzero entry,
c                 jw(n10+jw(n10+k)) the second one and so on 
c                 ... until a zero is reached which indicates the end
      if (sctype.gt.0) then
         do k=1,n
            jw(n10+k)=0
         end do
      end if


c     shuffle skipped entries to the end
      k=1
c     while k<=nLU
 830  if (k.gt.nLU) goto 850
c        U-part
         i=0
         j=jldl(k-shiftA)-1
         r=jw(na+k)-1
         if (jldl(n+1+k-shiftA).eq.0) then
            do l=jldl(k-shiftA),r
c              index m of U(k,m)
               m=jldl(l-shiftA)
c              has this entry been skipped?
c              use jw and w as buff
               if (jw(nc+m).gt.0) then
                  i=i+1
c                 in addition to the shift we have to update the index
c                 with respect to the new permutation
                  jw(i)=jw(n+m)
                  w(i) =aldl(l-shiftA)
               else 
c                 shift entries
                  j=j+1
c                 in addition to the shift we have to update the index
c                 with respect to the new permutation
                  jldl(j-shiftA)=jw(n+m)
                  aldl(j-shiftA)=aldl(l-shiftA)
               end if
            end do

c           first skipped entry in row k of U, regular part
            j=j+1
            jw(na+k)=j
c           is there at least one skipped entry?
            if (j.le.r) then
c              position of the leading skipped entry in row k
               m=jw(1)
c              concatenate linked list for column m
c              add link to row k in front of the list
               l=jw(n6+m)
               jw(n6+m)=k
               jw(n6+k)=l
            elseif (r.lt.jw(nb+k)-1) then 
c              position of the leading skipped entry in row k
               m=jldl(j-shiftA)
c              concatenate linked list for column m
c              add link to row k in front of the list
               l=jw(n6+m)
               jw(n6+m)=k
               jw(n6+k)=l
            end if 
c           put skipped entries back to the end
            i=0
c           while j<=r
 860        if (j.gt.r) goto 870
               i=i+1
               jldl(j-shiftA)=jw(i)
               aldl(j-shiftA)=w(i)
               jw(i)=0
               w(i)=FNULL
               j=j+1
            goto 860
c           end while

c           additional Tismenetsky updates
 870        if (sctype.gt.0) then
c              U-part
               i=0
               j=jw(nb+k)-1
               r=jw(n8+k)-1
c               do  l=jw(nb+k),jldl(k+1-shiftA)-1
               do  l=jw(nb+k),r
c                 index m of U(k,m)
                  m=jldl(l-shiftA)
c                 has this entry been skipped?
c                 use jw and w as buff
                  if (jw(nc+m).gt.0) then
                     i=i+1
c                    in addition to the shift we have to update the index with
c                    respect to the new permutation
                     jw(i)=jw(n+m)
                     w(i) =aldl(l-shiftA)
                  else 
c                    shift entries
                     j=j+1
c                    in addition to the shift we have to update the index with
c                    respect to the new permutation
                     jldl(j-shiftA)=jw(n+m)
                     aldl(j-shiftA)=aldl(l-shiftA)
                  end if
               end do

c              first skipped entry in row k of U, epsilon size part
               j=j+1
               jw(n8+k)=j
c              is there at least one node skipped ?
               if (j.le.r) then
c                 position of the leading skipped node in column k
                  m=jw(1)
c                 concatenate linked list for row jw(nc+m)
c                 add link to column k in front of the list
                  l=jw(n10+m)
                  jw(n10+m)=k
                  jw(n10+k)=l
               elseif (r.lt.jldl(k+1-shiftA)-1) then
c                 position of the leading skipped node in column k
                  m=jldl(j-shiftA)
c                 concatenate linked list for row jw(nc+m)
c                 add link to column k in front of the list
                  l=jw(n10+m)
                  jw(n10+m)=k
                  jw(n10+k)=l
               end if
c              put skipped entries back to the end
               i=0
c              while j<=r
 880           if (j.gt.r) goto 840
                  i=i+1
                  jldl(j-shiftA)=jw(i)
                  aldl(j-shiftA) =w(i)
                  jw(i)=0
                  w(i)=FNULL
                  j=j+1
               goto 880
c              end while
            end if
            k=k+1
         else
            jj=j-1
            ii=jldl(k-shiftA)
            do l=jldl(k-shiftA),r
c              index m of U(k,m)
               m=jldl(l-shiftA)
c              has this entry been skipped?
c              use jw and w as buff
               if (jw(nc+m).gt.0) then
                  i=i+1
c                 in addition to the shift we have to update the index
c                 with respect to the new permutation
                  jw(i)=jw(n+m)
                  w(i)   =aldl(ii-shiftA)
                  w(nw+i)=aldl(ii+1-shiftA)
               else 
c                 shift entries
                  j=j+1
                  jj=jj+2
c                 in addition to the shift we have to update the index
c                 with respect to the new permutation
                  jldl(j-shiftA)=jw(n+m)
                  aldl(jj-shiftA)  =aldl(ii-shiftA)
                  aldl(jj+1-shiftA)=aldl(ii+1-shiftA)
               end if
               ii=ii+2
            end do

c           first skipped entry in row k of U, regular part
            j=j+1
            jj=jj+2
            jw(na+k)=j
c           is there at least one skipped entry?
            if (j.le.r) then
c              position of the leading skipped entry in row k
               m=jw(1)
c              concatenate linked list for column m
c              add link to row k in front of the list
               l=jw(n6+m)
               jw(n6+m)=k
               jw(n6+k)=l
            elseif (r.lt.jw(nb+k)-1) then 
c              position of the leading skipped entry in row k
               m=jldl(j-shiftA)
c              concatenate linked list for column m
c              add link to row k in front of the list
               l=jw(n6+m)
               jw(n6+m)=k
               jw(n6+k)=l
            end if 
c           put skipped entries back to the end
            i=0
c           while j<=r
 861        if (j.gt.r) goto 871
               i=i+1
               jldl(j-shiftA)=jw(i)
               aldl(jj-shiftA)  =w(i)
               aldl(jj+1-shiftA)=w(nw+i)
               jw(i)=0
               w(i)   =FNULL
               w(nw+i)=FNULL
               j=j+1
               jj=jj+2
            goto 861
c           end while

c           additional Tismenetsky updates
 871        if (sctype.gt.0) then
c              U-part
               i=0
               j=jw(nb+k)-1
               r=jw(n8+k)-1
               ii=jldl(k-shiftA)+2*(jw(nb+k)-jldl(k-shiftA))
               jj=jldl(k-shiftA)+2*(j-jldl(k-shiftA))
c               do l=jw(nb+k),jldl(k+1-shiftA)-1
               do l=jw(nb+k),r
c                 index m of U(k,m)
                  m=jldl(l-shiftA)
c                 has this entry been skipped?
c                 use jw and w as buff
                  if (jw(nc+m).gt.0) then
                     i=i+1
c                    in addition to the shift we have to update the index with
c                    respect to the new permutation
                     jw(i)=jw(n+m)
                     w(i)   =aldl(ii-shiftA)
                     w(nw+i)=aldl(ii+1-shiftA)
                  else 
c                    shift entries
                     j=j+1
                     jj=jj+2
c                    in addition to the shift we have to update the index with
c                    respect to the new permutation
                     jldl(j-shiftA)=jw(n+m)
                     aldl(jj-shiftA)  =aldl(ii-shiftA)
                     aldl(jj+1-shiftA)=aldl(ii+1-shiftA)
                  end if
                  ii=ii+2
               end do

c              first skipped entry in row k of U, epsilon size part
               j=j+1
               jj=jj+2
               jw(n8+k)=j
c              is there at least one node skipped ?
               if (j.le.r) then
c                 position of the leading skipped node in column k
                  m=jw(1)
c                 concatenate linked list for row jw(nc+m)
c                 add link to column k in front of the list
                  l=jw(n10+m)
                  jw(n10+m)=k
                  jw(n10+k)=l
               elseif (r.lt.jldl(k+1-shiftA)-1) then
c                 position of the leading skipped node in column k
                  m=jldl(j-shiftA)
c                 concatenate linked list for row jw(nc+m)
c                 add link to column k in front of the list
                  l=jw(n10+m)
                  jw(n10+m)=k
                  jw(n10+k)=l
               end if
c              put skipped entries back to the end
               i=0
c              while j<=r
 881           if (j.gt.r) goto 840
                  i=i+1
                  jldl(j-shiftA)=jw(i)
                  aldl(jj-shiftA)  =w(i)
                  aldl(jj+1-shiftA)=w(nw+i)
                  jw(i)=0
                  w(i)   =FNULL
                  w(nw+i)=FNULL
                  j=j+1
                  jj=jj+2
               goto 881
c              end while
            end if
            k=k+2
         end if
 840     continue
      goto 830
c     end while







      

c     reorder p -> [p(F),p(C)]
 850  i=0
      j=0
      do k=1,n
         if (jw(nc+k).gt.0) then
            i=i+1
            jw(i)=p(k)
         else
            j=j+1
            p(j)=p(k)
         end if
      end do
c     reinsert entries at the end
      i=0
c     while j<n
 710  if (j.ge.n) goto 720
         j=j+1
         i=i+1
         p(j)=jw(i)
         jw(i)=0
      goto 710
c     end while



c     compute permutation q itself
c     use jw(n+1:2n) as buffer
 720  do k=1,n
         jw(n+invq(k))=k
      end do
c     reorder q -> [q(F),q(C)]
      i=0
      j=0
      do k=1,n
         if (jw(nc+k).gt.0) then
            i=i+1
            jw(i)=jw(n+k)
         else
            j=j+1
            jw(n+j)=jw(n+k)
         end if
      end do
c     reinsert entries at the end
      i=0
c     while j<n
 715  if (j.ge.n) goto 725
         j=j+1
         i=i+1
         jw(n+j)=jw(i)
         jw(i)=0
      goto 715
c     end while

c     correct inverse permutation
 725  do k=1,n
         invq(jw(n+k))=k
      end do


c     shuffle skipped entries to the end
      do k=1,n
c        A-part
         i=0
         j=ia(p(k))-1
         r=ia(p(k)+1)-1
         do l=ia(p(k)),r
c           index m of A(p(k),q(m))
            m=invq(ja(l))
c           has this node been skipped?
c           use jw and w as buff
            if (m.gt.nLU) then
               i=i+1
               jw(i)=ja(l)
               w(i) =a(l)
            else 
c              shift entries
               j=j+1
               ja(j)=ja(l)
               a(j) =a(l)
            end if
         end do
c        first skipped entry in row k of A
         j=j+1
c        store Afirst
         jw(n4+k)=j
c        put skipped entries back to the end
         i=0
c        while j<=r
 896     if (j.gt.r) goto 897
            i=i+1
            ja(j)=jw(i)
            a(j) =w(i)
            jw(i)=0
            w(i)=FNULL
            j=j+1
         goto 896
c        end while
 897     continue
      end do






c     Re-set up arrays for A^T (A is stored by rows)
c     This is even necessary in the case of a symmetric matrix, since
c     only half of the matrix is stored and in addition the matrix is 
c     reordered by a symmetric permutation which destroys the original
c     triangular structure
c
c     jw(2n+1:3n)  head of the linked list for the columns of A
c     jw(3n+1:4n)  linked list for the leading columns of A up to step k
c     jw(4n+1:5n)  pointer to the first entry at step k
      do k=nLU+1,n
c        clear head of the linked list (empty) 
         jw(n2+k)=0
      end do
      do i=nLU+1,n
c        pointer to the first nonzero element in row p(i)
c        it suffices to start at the revised Afirst
         j=jw(n4+i)
c        first column index k of  A(p(i),q(k))
         if (j.lt.ia(p(i)+1)) k=invq(ja(j))
c        we have to make sure that we are still inside row p(i)
c        and that only indices from the remaining block of size (n-nLU)
c        are considered
c        while j<ia(p(i)+1) and k<=nLU
 905     if (j.ge.ia(p(i)+1) .or. k.gt.nLU) goto 910
            j=j+1
c           associated column index
            k=invq(ja(j))
         goto 905
c        end while
 910     if (j.lt.ia(p(i)+1) .and. k.gt.nLU) then
c           pointer to the current first nonzero entry in row i
            jw(n4+i)=j
c           add new entry to the head of the list
            jw(n3+i)=jw(n2+k)
            jw(n2+k)=i
         end if
      end do




c     compute more accurate approximate Schur complement
      if (simplesc.eq.0.and.sctype.eq.0) then
c        compute W_FC= U_{FF}^{-1}U_{FC}
c        starting from the last row and going back

c        some preparations, norms
c        w(n+k),    k<=nLU: ||A(p(k),p(nLU+1:n))||_oo
c        w(n2+k),   k<=nLU:  max_i>nLU|a_p(i),p(k)|/|S(i,i)|

         do k=1,nLU
c           used to store ||A(p(k),p(nLU+1:n))||_oo, k<=nLU
            w(n+k)=FNULL
         end do
         do i=nLU+1,n
c           init remaining part of w(n2) to hold the entries for
c           diagonal compensation
            w(n2+i)=FNULL
c           stop before Afirst
            m=jw(n4+i)
            do j=ia(p(i)),m-1
               k=invq(ja(j))
               ax=ABS(a(j))
c              w(n+k) =max_i>nLU ||A(p(i),p(k))||_oo
               w(n+k) =max(CTOD(w(n+k)), ax)
            end do
         end do


c        rescale U_FC
         k=1
c        while k<=nLU
 431     if (k.gt.nLU) goto 435
c           start at Ufirst
c           1x1 case
            if (jldl(n+1+k-shiftA).eq.0) then
               do l=jw(na+k),jw(nb+k)-1
                  aldl(l-shiftA)=aldl(k-shiftA)*aldl(l-shiftA)
               end do
               k=k+1

c           2x2 case
            else
               ii=jldl(k-shiftA)+2*(jw(na+k)-jldl(k-shiftA))
               do l=jw(na+k),jw(nb+k)-1
                  x =aldl(ii-shiftA)
                  x2=aldl(ii+1-shiftA)
                  U12=aldl(n+1+k-shiftA)
                  aldl(ii-shiftA)  =aldl(k-shiftA)          *x
     +                             +U12      *x2
                  aldl(ii+1-shiftA)=CONJUG(SKEW(U12))*x
     +                             +aldl(k+1-shiftA)*x2
                  ii=ii+2
               end do
               k=k+2
            end if
         goto 431
c        end while


c        init U-pointer for the additional columns/rows
 435     jw(nc+nLU)=jldl(nLU+1-shiftA)
         k=nLU-1
c        check if the last update is a 2x2 update
         if (k.gt.1) then
            if (jldl(n+k-shiftA).gt.0) then
               k=k-1
               mode=1
            else
               mode=0
            end if
         else 
            mode=0
         end if
c        while k>=1
 440     if (k.lt.1) goto 450
c           U_FF^{-1}U_FC  can be written as a sequence of rank-1 and rank-2
c           updates starting with the last (but first/second) row and going 
c           down to the first.

c           For the entries of U(k,k+1:nLU) it suffices to stop before Ufirst
            len=0

c           1x1 case
c           In any step k perform linear combination using the associated 
c           rows of U_FC, i.e. compute  U_FF^{-1}(k,:)U_FC, this can be 
c           rewritten as (e_k^T-U_FF(k,k+1:nLU)U_FF^{-1}(k+1:nLU,:) ) U_FC 
c           if U_FF^{-1}(k+1:nLU,:)U_FC ==U_FC(k+1:nLU,:) + W_CF(k+1:nLU,:),
c           then U_FF^{-1}(k,:) U_FC  can be rewritten as
c             U_FC(k,:) - U_FF(k,k+1:nLU) U_FF^{-1}(k+1:nLU,:) U_FC  
c            =U_FC(k,:) - U_FF(k,k+1:nLU) [U_FC(k+1:nLU,:) + W_FC(k+1:nLU,:)]
c           =:U_FC(k,:) + W_FC(k,:)
c           => First perform 
c                -sum_{m:k<m<=nLU} (D(k,k)\U(k,m)) * U(m,nLU+1:n)
c              Then compute
c                -sum_{m:k<m<nLU}  (D(k,k)\U(k,m)) * W_FC(m,LU+1:n)
c                (last column is always zero)
            if (mode.eq.0) then
               do l=jldl(k-shiftA),jw(na+k)-1
c                 associated row index m>k
                  m=jldl(l-shiftA)
c                 D(k,k)\U(k,m)
                  x=aldl(k-shiftA)*aldl(l-shiftA)

c                 First step: 
c                   -sum_{m:k<m<=nLU} (D(k,k)\U(k,m)) * U(m,LU+1:n)
c                 perform linear combinations with column m of U_FC
c                 here it suffices to start at Ufirst
                  do j=jw(na+m),jw(nb+m)-1
c                    associated row index i
                     i=jldl(j-shiftA)
                     w(i)=w(i)-x*aldl(j-shiftA)
c                    is this a fill-in?
                     if (jw(i).eq.0) then
                        len=len+1
c                       flag component i as nonzero
                        jw(i)=len
c                       add i to the list of nonzeros
                        jw(n+len)=i
                     end if
                  end do

c                 Second step:
c                   -sum_{m:k<m<nLU}  (D(k,k)\U(k,m)) * W_FC(m,LU+1:n)
c                   (last column is always zero)
                  if (m.lt.nLU) then
c                    perform linear combinations with column m of W_FC

c                    is row m of W_FC the first row of a block row?
                     if (jldl(n+1+m-shiftA).gt.0) then
                        jj=jw(nc+m+2)
                        do j=jw(nc+m+2),jw(nc+m+1)-1
c                          associated row index
                           i=jldl(j-shiftA)
                           w(i)=w(i)-x*aldl(jj-shiftA)
c                          is this a fill-in?
                           if (jw(i).eq.0) then
                              len=len+1
c                             flag component i as nonzero
                              jw(i)=len
c                             add i to the list of nonzeros
                              jw(n+len)=i
                           end if
                           jj=jj+2
                        end do

c                    is row m of W_FC the second row of a block row?
                     else if (m.gt.1. .and. jldl(n+m-shiftA).gt.0) then
                        jj=jw(nc+m+2)+1
                        do j=jw(nc+m+2),jw(nc+m+1)-1
c                          associated row index
                           i=jldl(j-shiftA)
                           w(i)=w(i)-x*aldl(jj-shiftA)
c                          is this a fill-in?
                           if (jw(i).eq.0) then
                              len=len+1
c                             flag component i as nonzero
                              jw(i)=len
c                             add i to the list of nonzeros
                              jw(n+len)=i
                           end if
                           jj=jj+2
                        end do

                     else
                        do j=jw(nc+m+1),jw(nc+m)-1
c                          associated row index
                           i=jldl(j-shiftA)
                           w(i)=w(i)-x*aldl(j-shiftA)
c                          is this a fill-in?
                           if (jw(i).eq.0) then
                              len=len+1
c                             flag component i as nonzero
                              jw(i)=len
c                             add i to the list of nonzeros
                              jw(n+len)=i
                           end if
                        end do
                     end if

                  end if
               end do

c           2x2 case
c           In any step k,k+1 perform linear combination using the associated 
c           rows of U_FC, i.e. compute  U_FF^{-1}(k:k+1,:)U_FC, this can be 
c           rewritten as 
c           (e_(k:k+1)^T-U_FF(k:k+1,k+2:nLU)U_FF^{-1}(k+2:nLU,:) ) U_FC 
c           if U_FF^{-1}(k+2:nLU,:)U_FC ==U_FC(k+2:nLU,:) + W_CF(k+2:nLU,:),
c           then U_FF^{-1}(k:k+1,:) U_FC  can be rewritten as
c             U_FC(k:k+1,:) - U_FF(k:k+1,k+2:nLU) U_FF^{-1}(k+2:nLU,:) U_FC  
c            =U_FC(k:k+1,:) - U_FF(k:k+1,k+2:nLU) [U_FC(k+2:nLU,:) + W_FC(k+2:nLU,:)]
c           =:U_FC(k:k+1,:) + W_FC(k:k+1,:)
c           => First perform 
c                -sum_{m:k+1<m<=nLU} (D(k:k+1,k:k+1)\U(k:k+1,m)) * U(m,nLU+1:n)
c              Then compute
c                -sum_{m:k+1<m<nLU-1}  (D(k:k+1,k:k+1)\U(k:k+1,m)) * W_FC(m,nLU+1:n)
c                (last block column is always zero)
            else
               ii=jldl(k-shiftA)
               do l=jldl(k-shiftA),jw(na+k)-1
c                 associated row index m>k+1
                  m=jldl(l-shiftA)
c                 U(k:k+1,k:k+1)^{-1}U(k:k+1,m)
                  xp =aldl(ii-shiftA)
                  xp2=aldl(ii+1-shiftA)
                  U12=aldl(n+1+k-shiftA)
                  x =aldl(k-shiftA)          *xp+U12      *xp2
                  x2=CONJUG(SKEW(U12))*xp+aldl(k+1-shiftA)*xp2


c                 First step: 
c                   -sum_{m:k+1<m<=nLU} (D(k:k+1,k:k+1)\U(k:k+1,m)) * U(m,LU+1:n)
c                 perform linear combinations with column m of U_FC
c                 here it suffices to start at Ufirst
                  jj=jldl(m-shiftA)+2*(jw(na+m)-jldl(m-shiftA))
                  do j=jw(na+m),jw(nb+m)-1
c                    associated row index
                     i=jldl(j-shiftA)
                     w(i)   =w(i)   -x *aldl(jj-shiftA)  
                     w(nw+i)=w(nw+i)-x2*aldl(jj+1-shiftA)
c                    is this a fill-in?
                     if (jw(i).eq.0) then
                        len=len+1
c                       flag component i as nonzero
                        jw(i)=len
c                       add i to the list of nonzeros
                        jw(n+len)=i
                     end if
                     jj=jj+2
                  end do

c                 Second step:
c                   -sum_{m:k+1<m<nLU-1}  (D(k:k+1,k:k+1)\U(k:k+1,m)) * W_FC(m,LU+1:n)
c                   (last block column is always zero)
                  if (m.lt.nLU-1) then
c                    perform linear combinations with column m of W_FC

c                    is row m of W_FC the first row of a block row?
                     if (jldl(n+1+m-shiftA).gt.0) then
                        jj=jw(nc+m+2)
                        do j=jw(nc+m+2),jw(nc+m+1)-1
c                          associated row index
                           i=jldl(j-shiftA)
                           w(i)   =w(i)   -x *aldl(jj-shiftA)  
                           w(nw+i)=w(nw+i)-x2*aldl(jj-shiftA)
c                          is this a fill-in?
                           if (jw(i).eq.0) then
                              len=len+1
c                             flag component i as nonzero
                              jw(i)=len
c                             add i to the list of nonzeros
                              jw(n+len)=i
                           end if
                           jj=jj+2
                        end do

c                    is row m of W_FC the second row of a block row?
                     elseif (m.gt.1 .and. jldl(n+m-shiftA).gt.0) then
                        jj=jw(nc+m+1)+1
                        do j=jw(nc+m+1),jw(nc+m)-1
c                          associated row index
                           i=jldl(j-shiftA)
                           w(i)   =w(i)   -x *aldl(jj-shiftA)  
                           w(nw+i)=w(nw+i)-x2*aldl(jj-shiftA)
c                          is this a fill-in?
                           if (jw(i).eq.0) then
                              len=len+1
c                             flag component i as nonzero
                              jw(i)=len
c                             add i to the list of nonzeros
                              jw(n+len)=i
                           end if
                           jj=jj+2
                        end do

c                    regular row of W_FC
                     else
                        do j=jw(nc+m+1),jw(nc+m)-1
c                          associated row index
                           i=jldl(j-shiftA)
                           w(i)   =w(i)   -x *aldl(j-shiftA)  
                           w(nw+i)=w(nw+i)-x2*aldl(j-shiftA)
c                          is this a fill-in?
                           if (jw(i).eq.0) then
                              len=len+1
c                             flag component i as nonzero
                              jw(i)=len
c                             add i to the list of nonzeros
                              jw(n+len)=i
                           end if
                        end do
                     end if
                  end if
                  ii=ii+2
               end do
            end if


         
c           sparsify new row(s)
c           keep in mind that we essentially update 
c           a_p(i),p(j) by a_p(i),p(j)-a(p(i),p(k))W_FC(k,j)
c           or
c           a_p(i),p(j) by a_p(i),p(j)-a(p(i),p(k:k+1))W_FC(k:k+1,j)
c           compute maximum norm of W_FC(k,:), W_FC(k:k+1,:)
            Umax=RZERO
            if (mode.eq.0) then
               do l=1,len
                  i=jw(n+l)
                  Umax=max(Umax,ABS(w(i)))
               end do
            else
               do l=1,len
                  i=jw(n+l)
                  Umax=max(Umax,ABS(w(i)))
                  Umax=max(Umax,ABS(w(nw+i)))
               end do
            end if
c           compute estimates for the maximum row norm
c           complete computation of DUmax via row k
c           use already computed value from column k
            DUmax=max(RONE/Umax,CTOD(w(n+k)))
c           complete computation of Lmax=||A(p(k),p(nLU+1:n))||, k<=nLU
            Lmax=CTOD(w(n+k))
c           complete computation of
c           DUmax=max{1/Umax, max_j>nLU|a_p(k),p(j)|}
c           it suffices to start at Afirst
            do l=jw(n4+k),ia(p(k)+1)-1
c              associated column index j>nLU
               j=invq(ja(l))
c              |A(p(k),q(j))|
               ax=ABS(a(l))
               Lmax=max(Lmax,ax)
               DUmax=max(DUmax,ax)
            end do

c           now finally sparsify W_FC(k,:), W_FC(k+1,:)
            l=1
            m=len
c           while l<=m
 900        if (l.gt.m) goto 912
c              associated column index i>nLU
               i=jw(n+l)
c              absolute value of the entry that should be dropped
               ax=ABS(w(i))
               if (mode.eq.1) ax=max(ax,ABS(w(nw+i)))

c              three criteria for dropping
c              1. inverse factor U^{-1}, here Workarray is already part
c                 of the inverse, so droptol is correct
c              2. measure the impact of the update 
c                 a_{p(j),p(i)}-a(p(j),p(k...))*W_FC(k...,i) for any row j>nLU
c              3. measure the impact of the update 
c                 a_{p(j),p(i)}-a(p(j),p(k...))*W_FC(k...,i) for column i>nLU
               if (ax.le.droptol            .and.
     +             ax*DUmax.le.droptol2     .and.
     +             ax*Lmax .le.droptol2)    then
c                 drop entry
                  w(i)=FNULL
                  if (mode.eq.1) w(nw+i)=0
                  jw(i)=0
c                 shuffle final entry in the list to the current position
                  jw(n+l)=jw(n+m)
c                 cut length
                  m=m-1
               else
                  l=l+1
               end if
            goto 900
c           end while
c           new length after dropping
 912        len=m

c           store new row of W_FC behind the previous one of W_FC
            if (mode.eq.0) then
               if (jw(nc+k+1)+len.gt.iwk+shiftA) goto 992
               maxiwk=max(maxiwk,jw(nc+k+1)+len-shiftA)

               m=jw(nc+k+1)-1
               do l=1,len
                  i=jw(n+l)
                  jldl(m+l-shiftA)=i
                  aldl(m+l-shiftA)=w(i)
                  jw(i)=0
                  w(i)=FNULL
               end do
               jw(nc+k)=jw(nc+k+1)+len
            else
               if (jw(nc+k+2)+2*len.gt.iwk+shiftA) goto 992
               maxiwk=max(maxiwk,jw(nc+k+2)+2*len-shiftA)
               m=jw(nc+k+2)
               ii=0
               do l=0,len-1
                  i=jw(n+l+1)
                  jldl(m+l-shiftA)=i
                  aldl(m+ii-shiftA)  =w(i)
                  aldl(m+ii+1-shiftA)=w(nw+i)
                  jw(i)=0
                  w(i)   =FNULL
                  w(nw+i)=FNULL
                  ii=ii+2
               end do
               jw(nc+k+1)=jw(nc+k+2)+len
               jw(nc+k)  =jw(nc+k+1)+len
            end if


c           decide whether to perform a 1x1 or a 2x2 update next
            k=k-1
            if (k.gt.1) then
               if (jldl(n+k-shiftA).gt.0) then
                  k=k-1
                  mode=1
               else
                  mode=0
               end if
            else 
               mode=0
            end if
         goto 440
c        end while


c        sort the W_FC buffer in increasing order
c        this is necessary to use the linked list properly
 450     k=1
c        while k<nLU
 460     if (k.ge.nLU) goto 470
            if (jldl(n+1+k-shiftA).gt.0) then
               j=jw(nc+k+2)
               l=jw(nc+k+1)-j
               call QQSORTS2(aldl(j-shiftA),jldl(j-shiftA),jw(n+1),l)
               k=k+2
            else
               j=jw(nc+k+1)
               l=jw(nc+k)-j
               call QQSORTS(aldl(j-shiftA),jldl(j-shiftA),jw(n+1),l)
               k=k+1
            end if
         goto 460
c        end while

c        now set up linked list for the additional computed parts
c        jw(nl+k) W_FC,first
c        jw(ne+k) W_FC,list
 470     do k=1,n
            jw(ne+k)=0
         end do
         k=1
c        while k<nLU
 480     if (k.ge.nLU) goto 490
c           W_FC-part, row k(,k+1)
            if (jldl(n+1+k-shiftA).gt.0) then
               mode=1
            else
               mode=0
            end if
            j=jw(nc+k+1+mode)
c           first skipped entry in row k of W_FC
            jw(nl+k)=j
c           is there at least one skipped entry?
            if (j.lt.jw(nc+k+mode)) then
c              position of the leading skipped entry in row k
               m=jldl(j-shiftA)
c              concatenate linked list for column m
c              add link to row k in front of the list
               l=jw(ne+m)
               jw(ne+m)=k
               jw(ne+k)=l
            end if 
            k=k+1+mode
         goto 480
c        end while

c        jw(n2) is not needed as head for the linked list of A
c        we use it to store a copy of jw(na), since jw(na) is advanced
 490     do k=1,nLU
            jw(n2+k)=jw(na+k)
         end do


c        finally let the approximate Schur complement start behind
c        the nested parts
         jldl(nLU+1-shiftA)=jw(nc+1)

      end if
c     end more accurate Schur complement









c     generate approximate Schur complement
c     for the alternative computation of the approximate Schur complement
c     use the squared drop tolerance
      droptolU=droptols(3)/maxcondestold
      do k=nLU+1,n

c        preparation for the alternative computation of the approximate
c        Schur complement
c        Compute row k of (U_FC+W_FC)'*A_FF
         if (simplesc.eq.0 .and. sctype.eq.0) then
            len=0
c           where does column k of W_FC start in the linked list
            i=jw(ne+k)
c           while i>0
 921        if (i.eq.0) goto 929
c              does there exist an entry in W_FC(i,k)?
c              position of the leading entry in row i

c              1x1 case
               if (jldl(n+1+i-shiftA).eq.0) then
                  jj=jw(nl+i)
c                 nonempty column?
                  if (jj.lt.jw(nc+i)) then
c                    if this is the case, is the first entry equal to k,
c                    otherwise we don't need to do an update because W_FC(i,k)=0
                     if (jldl(jj-shiftA).eq.k) then
c                       W_FC(i,k)'
                        x=CONJUG(SKEW(aldl(jj-shiftA)))

c                       nonzeros in row i of A_FF
                        jj=ia(p(i))
c                       innermost loop, update, it suffices to stop before Afirst
                        do l=jj,jw(n4+i)-1
c                          column index j of A(p(i),p(j))
                           j=invq(ja(l))
c                          update numerical value
c                          use only half of the diagonal value
                           if (j.eq.i) then
                              w(j)=w(j)+a(l)*x/RTWO
                           else
                              w(j)=w(j)+a(l)*x
                           end if
                           if (jw(j).eq.0) then
                              len=len+1
c                          flag component i as nonzero
                              jw(j)=len
c                          add j to the list of nonzeros
                              jw(n+len)=j
                           end if
                        end do
                     end if 
                  end if
                  
c              2x2 case    
               else
                  jj=jw(nl+i)
c                 nonempty column?
                  if (jj.lt.jw(nc+i)) then
c                    if this is the case, is the first entry equal to k,
c                    otherwise we don't need to do an update because W_FC(i:i+1,k)=0
                     if (jldl(jj-shiftA).eq.k) then
c                       W_FC(i:i+1,k)'
                        ii=jw(nc+i+2)+2*(jj-jw(nc+i+2))
                        x =CONJUG(SKEW(aldl(ii-shiftA)))
                        x2=CONJUG(SKEW(aldl(ii+1-shiftA)))

c                       nonzeros in row i of A_FF
                        jj=ia(p(i))
c                       innermost loop, update, it suffices to stop before Afirst
                        do l=jj,jw(n4+i)-1
c                          column index j of A(p(i),p(j))
                           j=invq(ja(l))
c                          update numerical value
c                          use only half of the diagonal value
                           if (j.eq.i) then
                              w(j)=w(j)+a(l)*x/RTWO
                           else
                              w(j)=w(j)+a(l)*x
                           end if
                           if (jw(j).eq.0) then
                              len=len+1
c                             flag component i as nonzero
                              jw(j)=len
c                             add j to the list of nonzeros
                              jw(n+len)=j
                           end if
                        end do

c                       nonzeros in row i+1 of A_FF
                        jj=ia(p(i+1))
c                       innermost loop, update, it suffices to stop before Afirst
                        do l=jj,jw(n4+i+1)-1
c                          column index j of A(p(i+1),p(j))
                           j=invq(ja(l))
c                          update numerical value
c                          use only half of the diagonal value
                           if (j.eq.i+1) then
                              w(j)=w(j)+a(l)*x2/RTWO
                           else
                              w(j)=w(j)+a(l)*x2
                           end if
                           if (jw(j).eq.0) then
                              len=len+1
c                             flag component i+1 as nonzero
                              jw(j)=len
c                             add j to the list of nonzeros
                              jw(n+len)=j
                           end if
                        end do
                     end if 
                  end if
               end if
c              next column i that has a nonzero in row k
               i=jw(ne+i)
            goto 921
c           end while

c           where does column k of U_FC start in the linked list
 929        i=jw(n6+k)
c           while i>0
 931        if (i.eq.0) goto 939
c              1x1 case
               if (jldl(n+1+i-shiftA).eq.0) then
c                 does there exist an entry  U(i,k)?
                  jj=jw(na+i)
c                 empty column?
                  if (jj.lt.jw(nb+i)) then
c                    if this is the case, is the first entry equal to k,
c                    otherwise we don't need to do an update because U(i,k)=0
                     if (jldl(jj-shiftA).eq.k) then
c                       U(i,k)'
                        x=CONJUG(SKEW(aldl(jj-shiftA)))
c                       nonzeros in column i of A

                        jj=ia(p(i))
c                       innermost loop, update, it suffices to stop before Afirst 
                        do l=jj,jw(n4+i)-1
c                          index j of A(j,i)
                           j=invq(ja(l))
c                          update numerical value
c                          use only half of the diagonal value
                           if (j.eq.i) then
                              w(j)=w(j)+a(l)*x/RTWO
                           else
                              w(j)=w(j)+a(l)*x
                           end if
                           if (jw(j).eq.0) then
                              len=len+1
c                             flag component i as nonzero
                              jw(j)=len
c                             add j to the list of nonzeros
                              jw(n+len)=j
                           end if
                        end do
                     end if 
                  end if

c              2x2 case
               else
c                 does there exist an entry  U(i:i+1,k)?
                  jj=jw(na+i)
c                 empty column?
                  if (jj.lt.jw(nb+i)) then
c                    if this is the case, is the first entry equal to k,
c                    otherwise we don't need to do an update because U(i:i+1,k)=0
                     if (jldl(jj-shiftA).eq.k) then
c                       U(i:i+1,k)'
                        ii=jldl(i-shiftA)+2*(jj-jldl(i-shiftA))
                        x =CONJUG(SKEW(aldl(ii-shiftA)))
                        x2=CONJUG(SKEW(aldl(ii+1-shiftA)))

c                       nonzeros in column i of A
                        jj=ia(p(i))
c                       innermost loop, update, it suffices to stop before Afirst 
                        do l=jj,jw(n4+i)-1
c                          index j of A(j,i)
                           j=invq(ja(l))
c                          update numerical value
c                          use only half of the diagonal value
                           if (j.eq.i) then
                              w(j)=w(j)+a(l)*x/RTWO
                           else
                              w(j)=w(j)+a(l)*x
                           end if
                           if (jw(j).eq.0) then
                              len=len+1
c                             flag component i as nonzero
                              jw(j)=len
c                             add j to the list of nonzeros
                              jw(n+len)=j
                           end if
                        end do

c                       nonzeros in column i+1 of A
                        jj=ia(p(i+1))
c                       innermost loop, update, it suffices to stop before Afirst 
                        do l=jj,jw(n4+i+1)-1
c                          index j of A(j,i+1)
                           j=invq(ja(l))
c                          update numerical value
c                          use only half of the diagonal value
                           if (j.eq.i) then
                              w(j)=w(j)+a(l)*x2/RTWO
                           else
                              w(j)=w(j)+a(l)*x2
                           end if
                           if (jw(j).eq.0) then
                              len=len+1
c                             flag component i+1 as nonzero
                              jw(j)=len
c                             add j to the list of nonzeros
                              jw(n+len)=j
                           end if
                        end do
                     end if 
                  end if
               end if
c              next column i that has a nonzero in row k
               i=jw(n6+i)
            goto 931
c           end while

c           transfer entries to w(n2+i), jw(n2-nLU+i) and leave them there
c           we use at most w(n2+1),...w(n2+nLU) and jw(n2-nLU+1),...,jw(n2)
 939        m=len
            do i=len,1,-1
               j=jw(n+i)
               jw(n2-nLU+i)=j
               w(n2+i)=w(j)
               w(j)=FNULL
               jw(j)=0
            end do

         end if
c        end preparation for the more accurate Schur complement


c        for the more accurate Schur complement use A=U+U^T, where
c        U is half of the diagonal part and that part of A that is 
c        physically stored. Then we only consider U and later on we merge
c        it with the U^T contributions


c        extract skipped entries of row k from A(p,q)
         if (simplesc.eq.0 .and. sctype.eq.0) then
            jj=nLU+1
         else
            jj=k
         end if
         
         len=0
c        it suffices to start at Afirst
         j=jw(n4+k)
         do l=j,ia(p(k)+1)-1
c           column index i of A(p(k),p(i))
            i=invq(ja(l))
c           ensure that only the upper triangular part is referenced
            if (i.ge.jj) then
               len=len+1
c              flag component i as nonzero
               jw(i)=len
c              add i to the list of nonzeros
               jw(n+len)=i
c              extract numerical value
               w(i)=a(l)
c              for the alternative Schur complement we really only consider
c              half of the matrix and thus, half of the diagonal entry
               if (simplesc.eq.0 .and. sctype.eq.0 .and. i.eq.k)
     +            w(i)=a(l)/RTWO
            end if
         end do

#ifdef PRINT_INFO
         write (6,'(A,I4,I4)')
     +        'Schur complement including A(k,:), row ',k,k-nLU
         write (6,'(10I8)')      (jw(n+l),    l=1,len)
         write (6,'(1P,10E8.1)') (w(jw(n+l)), l=1,len)
#endif
#ifdef PRINT_INFO2
         write (6,'(A,I4,I4)')
     +        'Schur complement including A(k,:), row ',k,k-nLU
         write (6,'(10I8)')      (jw(n+l),    l=1,len)
         write (6,'(1P,10E8.1)') (dreal(w(jw(n+l))), l=1,len)
         write (6,'(1P,10E8.1)') (dimag(w(jw(n+l))), l=1,len)
#endif

         if (simplesc.ne.0 .or. sctype.ne.0) then
c     
c           extract lower triangular skipped part of A(p,p), column k
c           first row index
            i=jw(n2+k)
c           while i>0         
 920        if (i.le.0) goto 930

c              pointer to A(p(i),q(k))
               j=jw(n4+i)
c              true column index l of  A(p(i),q(l))
               l=invq(ja(j))
c              store next row from the linked list
               m=jw(n3+i)

c              decide whether this entry has to be stored or not
c              if l=k and k<i
c              remark: since l=k is trivially true this condition reduces to
c              if l<i
               if (l.lt.i) then 
                  len=len+1
c                 flag component i as nonzero
                  jw(i)=len
c                 add i to the list of nonzeros
                  jw(n+len)=i
c                 extract numerical value
                  w(i)=CONJUG(SKEW(a(j)))
               end if

c              update column information
               if (j.lt.ia(n)) j=j+1
c              associated column index
               if (j.lt.ia(p(i)+1)) l=invq(ja(j))
c              we have to make sure that we are still inside row i
 915           if (j.ge.ia(p(i)+1) .or. l.gt.nLU) goto 916
                  j=j+1
c                 associated column index
                  l=invq(ja(j))
               goto 915
c              end while
c              pointer to the next nonzero element in row i
 916           jw(n4+i)=j
               if (j.lt.ia(p(i)+1) .and. l.gt.nLU) then
c                 add new entry to the head of the list
                  jw(n3+i)=jw(n2+l)
                  jw(n2+l)=i
               end if

c              recover next row entry
               i=m
            goto 920
c           end while
         end if

#ifdef PRINT_INFO
 930     write (6,'(A,I4,I4)')
     +        'Schur complement including A(:,k)^T, row ',k,k-nLU
         write (6,'(10I8)')      (jw(n+l),   l=1,len)
         write (6,'(1P,10E8.1)') (w(jw(n+l)),l=1,len)
#endif
#ifdef PRINT_INFO2
 930     write (6,'(A,I4,I4)')
     +        'Schur complement including A(:,k)^T, row ',k,k-nLU
         write (6,'(10I8)')      (jw(n+l),   l=1,len)
         write (6,'(1P,10E8.1)') (dreal(w(jw(n+l))),l=1,len)
         write (6,'(1P,10E8.1)') (dimag(w(jw(n+l))),l=1,len)
#endif


#if defined PRINT_INFO || defined PRINT_INFO2
         if (simplesc.eq.0 .and. sctype.eq.0) then
#else
 930     if (simplesc.eq.0 .and. sctype.eq.0) then
#endif
c           alternative computation of the approximate Schur complement
c           update row k of the Schur complement

c           1. step (-U_FC(:,k)-W_FC(:,k))^T*A_FC 
c           1.1 -W_FC(:,k)'*A_FC
c           linked list, column k of W_FC
            i=jw(ne+k)
c           while i>0
 901        if (i.eq.0) goto 909
c              1x1 case
               if (jldl(n+1+i-shiftA).eq.0) then
c                 does there exist an entry in W(k:n,i)?
                  jj=jw(nl+i)
c                 empty column?
                  if (jj.lt.jw(nc+i)) then
c                    if this is the case, is the first entry equal to k,
c                    otherwise we don't need to do an update because W_FC(i,k)=0
                     if (jldl(jj-shiftA).eq.k) then
c                       W_FC(i,,k)'
                        x=CONJUG(SKEW(aldl(jj-shiftA)))
c                       nonzeros in row i of A, start at position at least k

                        jj=jw(n4+i)
c                       innermost loop, update 
c                       it suffices to start at Afirst
                        do l=jj,ia(p(i)+1)-1
c                          index j of A(p(i),p(j))
                           j=invq(ja(l))
c                          update numerical value
                           w(j)=w(j)-a(l)*x
                           if (jw(j).eq.0) then
                              len=len+1
c                             flag component i as nonzero
                              jw(j)=len
c                             add j to the list of nonzeros
                              jw(n+len)=j
                           end if
                        end do
                     end if 
                  end if

c              2x2 case
               else
c                 does there exist an entry in W(k:n,i)?
                  jj=jw(nl+i)
c                 empty column?
                  if (jj.lt.jw(nc+i)) then
c                    if this is the case, is the first entry equal to k,
c                    otherwise we don't need to do an update because W_FC(i,k)=0
                     if (jldl(jj-shiftA).eq.k) then
c                       W_FC(i:i+1,k)'
                        ii=jw(nc+i+2)+2*(jj-jw(nc+i+2))
                        x =CONJUG(SKEW(aldl(ii-shiftA)))
                        x2=CONJUG(SKEW(aldl(ii+1-shiftA)))

c                       nonzeros in row i of A, start at position at least k
                        jj=jw(n4+i)
c                       innermost loop, update 
c                       it suffices to start at Afirst
                        do l=jj,ia(p(i)+1)-1
c                          index j of A(p(i),p(j))
                           j=invq(ja(l))
c                          update numerical value
                           w(j)=w(j)-a(l)*x
                           if (jw(j).eq.0) then
                              len=len+1
c                             flag component i as nonzero
                              jw(j)=len
c                             add j to the list of nonzeros
                              jw(n+len)=j
                           end if
                        end do

c                       nonzeros in row i+1 of A, start at position at least k
                        jj=jw(n4+i+1)
c                       innermost loop, update 
c                       it suffices to start at Afirst
                        do l=jj,ia(p(i+1)+1)-1
c                          index j of A(p(i+1),p(j))
                           j=invq(ja(l))
c                          update numerical value
                           w(j)=w(j)-a(l)*x2
                           if (jw(j).eq.0) then
                              len=len+1
c                             flag component i+1 as nonzero
                              jw(j)=len
c                             add j to the list of nonzeros
                              jw(n+len)=j
                           end if
                        end do
                     end if 
                  end if
               end if
               i=jw(ne+i)
            goto 901
c           end while

c           1.2 -U_FC(:,k)^T*A_FC
c           linked list for the nonzeros in column k of U
 909        i=jw(n6+k)
c           while i>0
 911        if (i.eq.0) goto 919
c              1x1 case
               if (jldl(n+1+i-shiftA).eq.0) then
c                 does there exist an entry in U(i,k:n)?
                  jj=jw(na+i)
c                 empty column?
                  if (jj.lt.jw(nb+i)) then
c                    if this is the case, is the first entry equal to k,
c                    otherwise we don't need to do an update because U(i,k)=0
                     if (jldl(jj-shiftA).eq.k) then
c                       U(i,k)'
                        x=CONJUG(SKEW(aldl(jj-shiftA)))

c                       nonzeros in row i of A, start at position at least k
                        jj=jw(n4+i)
c                       innermost loop, update, it suffices to start with Afirst
                        do l=jj,ia(p(i)+1)-1
c                          index j of A(p(i),p(j))
                           j=invq(ja(l))
c                          update numerical value
                           w(j)=w(j)-a(l)*x
                           if (jw(j).eq.0) then
                              len=len+1
c                             flag component i as nonzero
                              jw(j)=len
c                             add j to the list of nonzeros
                              jw(n+len)=j
                           end if
                        end do
                     end if 
                  end if 

c              2x2 case
               else
c                 does there exist an entry in U(i:i+1,k:n)?
                  jj=jw(na+i)
c                 empty column?
                  if (jj.lt.jw(nb+i)) then
c                    if this is the case, is the first entry equal to k,
c                    otherwise we don't need to do an update because U(i:i+1,k)=0
                     if (jldl(jj-shiftA).eq.k) then
c                       U(i:i+1,k)'
                        ii=jldl(i-shiftA)+2*(jj-jldl(i-shiftA))
                        x =CONJUG(SKEW(aldl(ii-shiftA)))
                        x2=CONJUG(SKEW(aldl(ii+1-shiftA)))

c                       nonzeros in row i of A, start at position at least k
                        jj=jw(n4+i)
c                       innermost loop, update, it suffices to start with Afirst
                        do l=jj,ia(p(i)+1)-1
c                          index j of A(p(i),p(j))
                           j=invq(ja(l))
c                          update numerical value
                           w(j)=w(j)-x*a(l)
                           if (jw(j).eq.0) then
                              len=len+1
c                             flag component i as nonzero
                              jw(j)=len
c                             add j to the list of nonzeros
                              jw(n+len)=j
                           end if
                        end do

c                       nonzeros in row i+1 of A, start at position at least k
                        jj=jw(n4+i+1)
c                       innermost loop, update, it suffices to start with Afirst
                        do l=jj,ia(p(i+1)+1)-1
c                          index j of A(p(i+1),p(j))
                           j=invq(ja(l))
c                          update numerical value
                           w(j)=w(j)-x2*a(l)
                           if (jw(j).eq.0) then
                              len=len+1
c                             flag component i+1 as nonzero
                              jw(j)=len
c                             add j to the list of nonzeros
                              jw(n+len)=j
                           end if
                        end do
c                       end do l
                     end if 
c                    end if jldl(jj-shiftA)=k
                  end if 
c                 end if jj<jw(nb+i)
               end if
c              end if-else
               i=jw(n6+i)
            goto 911
c           end while



c           2. step: A_CF(k,:)*(-W_FC-U_FC)
c           it suffices to stop before Afirst
 919        do i=ia(p(k)),jw(n4+k)-1
c              column index jj of A(p(k),p(jj))
               jj=invq(ja(i))
               x=a(i)

c              2.1 -A_CF(k,:)*U_FC
c              innermost loop, update with U_FC
c              it suffices to start at Ufirst
c              Note that (see label 490) jw(n2+...) stores
c              a copy of jw(na+...) since jw(na+...) is still
c              needed and jw(n2+...) is altered

c              is row jj of U_FC the first row of a block row?
               if (jldl(n+1+jj-shiftA).gt.0) then
                  ii=jldl(jj-shiftA)+2*(jw(n2+jj)-jldl(jj-shiftA))
                  do l=jw(n2+jj),jw(nb+jj)-1
c                    index j of U(jj,j)
                     j=jldl(l-shiftA)
c                    update numerical value
                     w(j)=w(j)-aldl(ii-shiftA)*x
                     if (jw(j).eq.0) then
                        len=len+1
c                       flag component i as nonzero
                        jw(j)=len
c                       add j to the list of nonzeros
                        jw(n+len)=j
                     end if
                     ii=ii+2
                  end do

c                 2.2 -A_CF(k,:)*W_FC
                  if (jj.lt.nLU-1) then
c                    innermost loop, update with W_FC
                     ii=jw(nc+jj+2)
                     do l=jw(nc+jj+2),jw(nc+jj+1)-1
c                       index j of W_FC(jj,j)
                        j=jldl(l-shiftA)
c                       update numerical value
                        w(j)=w(j)-aldl(ii-shiftA)*x
                        if (jw(j).eq.0) then
                           len=len+1
c                          flag component i as nonzero
                           jw(j)=len
c                          add j to the list of nonzeros
                           jw(n+len)=j
                        end if
                        ii=ii+2
                     end do
                  end if

c              is row jj of U_FC the second row of a block row?
               elseif (jj.gt.1 .and. jldl(n+jj-shiftA).gt.0) then
                  ii=jldl(jj-1-shiftA)
     +              +2*(jw(n2+jj-1)-jldl(jj-1-shiftA))+1
                  do l=jw(n2+jj-1),jw(nb+jj-1)-1
c                    index j of U(jj,j)
                     j=jldl(l-shiftA)
c                    update numerical value
                     w(j)=w(j)-aldl(ii-shiftA)*x
                     if (jw(j).eq.0) then
                        len=len+1
c                       flag component i as nonzero
                        jw(j)=len
c                       add j to the list of nonzeros
                        jw(n+len)=j
                     end if
                     ii=ii+2
                  end do

c                 2.2 -A_CF(k,:)*W_FC
                  if (jj.lt.nLU-1) then
c                    innermost loop, update with W_FC
                     ii=jw(nc+jj+1)+1
                     do l=jw(nc+jj+1),jw(nc+jj)-1
c                       index j of W_FC(jj,j)
                        j=jldl(l-shiftA)
c                       update numerical value
                        w(j)=w(j)-aldl(ii-shiftA)*x
                        if (jw(j).eq.0) then
                           len=len+1
c                          flag component i as nonzero
                           jw(j)=len
c                          add j to the list of nonzeros
                           jw(n+len)=j
                        end if
                        ii=ii+2
                     end do
                  end if

c              a regular row
               else
                  do l=jw(n2+jj),jw(nb+jj)-1
c                    index j of U(jj,j)
                     j=jldl(l-shiftA)
c                    update numerical value
                     w(j)=w(j)-aldl(l-shiftA)*x
                     if (jw(j).eq.0) then
                        len=len+1
c                       flag component i as nonzero
                        jw(j)=len
c                       add j to the list of nonzeros
                        jw(n+len)=j
                     end if
                  end do

c                 2.2 -A_CF(k,:)*W_FC
                  if (jj.lt.nLU) then
c                    innermost loop, update with W_FC
                     do l=jw(nc+jj+1),jw(nc+jj)-1
c                       index j of W_FC(jj,j)
                        j=jldl(l-shiftA)
c                       update numerical value
                        w(j)=w(j)-aldl(l-shiftA)*x
                        if (jw(j).eq.0) then
                           len=len+1
c                          flag component i as nonzero
                           jw(j)=len
c                          add j to the list of nonzeros
                           jw(n+len)=j
                        end if
                     end do
                  end if
               end if
            end do

c           3.step
c           continue computation of (U_FC(:,k)+W_FC(:,k))^T*A_FF*(W_FC+U_FC)
c           recall that (U_FC(:,k)+W_FC(:,k))^T*A_FF is already computed and
c           stored w(n2+i), jw(n2-nLU+i), i=1,...,m
            do i=1,m
               jj=jw(n2-nLU+i)
               x=w(n2+i)

c              first row of a block row
               if (jldl(n+1+jj-shiftA).gt.0) then
c                 3.1 [(U_FC(:,k)+W_FC(:,k))^T*A_FF] * U_FC
c                 innermost loop, update with U_FC
                  ii=jldl(jj-shiftA)+2*(jw(n2+jj)-jldl(jj-shiftA))
                  do l=jw(n2+jj),jw(nb+jj)-1
c                    index j of U(i,j)
                     j=jldl(l-shiftA)
c                    update numerical value
                     w(j)=w(j)+aldl(ii-shiftA)*x
                     if (jw(j).eq.0) then
                        len=len+1
c                       flag component j as nonzero
                        jw(j)=len
c                       add j to the list of nonzeros
                        jw(n+len)=j
                     end if
                     ii=ii+2
                  end do

c                 3.2 [(U_FC(:,k)+W_FC(:,k))^T*A_FF] * W_FC
c                 innermost loop, update with W_FC
                  if (jj.lt.nLU-1) then
                     ii=jw(nc+jj+2)
                     do l=jw(nc+jj+2),jw(nc+jj+1)-1
c                       index j of W_FC(i,j)
                        j=jldl(l-shiftA)
c                       update numerical value
                        w(j)=w(j)+aldl(ii-shiftA)*x
                        if (jw(j).eq.0) then
                           len=len+1
c                          flag component j as nonzero
                           jw(j)=len
c                          add j to the list of nonzeros
                           jw(n+len)=j
                        end if
                        ii=ii+2
                     end do
                  end if

c              second row of a block row
               elseif (jj.gt.1 .and. jldl(n+jj-shiftA).gt.0) then
c                 3.1 [(U_FC(:,k)+W_FC(:,k))^T*A_FF] * U_FC
c                 innermost loop, update with U_FC
                  ii=jldl(jj-1-shiftA)
     +              +2*(jw(n2+jj-1)-jldl(jj-1-shiftA))+1
                  do l=jw(n2+jj-1),jw(nb+jj-1)-1
c                    index j of U(i,j)
                     j=jldl(l-shiftA)
c                    update numerical value
                     w(j)=w(j)+aldl(ii-shiftA)*x
                     if (jw(j).eq.0) then
                        len=len+1
c                       flag component j as nonzero
                        jw(j)=len
c                       add j to the list of nonzeros
                        jw(n+len)=j
                     end if
                     ii=ii+2
                  end do

c                 3.2 [(U_FC(:,k)+W_FC(:,k))^T*A_FF] * W_FC
c                 innermost loop, update with W_FC
                  if (jj.lt.nLU-1) then
                     ii=jw(nc+jj+1)+1
                     do l=jw(nc+jj+1),jw(nc+jj)-1
c                       index j of W_FC(i,j)
                        j=jldl(l-shiftA)
c                       update numerical value
                        w(j)=w(j)+aldl(ii-shiftA)*x
                        if (jw(j).eq.0) then
                           len=len+1
c                          flag component j as nonzero
                           jw(j)=len
c                          add j to the list of nonzeros
                           jw(n+len)=j
                        end if
                        ii=ii+2
                     end do
                  end if

c              regular row
               else
c                 3.1 [(U_FC(:,k)+W_FC(:,k))^T*A_FF] * U_FC
c                 innermost loop, update with U_FC
                  do l=jw(n2+jj),jw(nb+jj)-1
c                    index j of U(i,j)
                     j=jldl(l-shiftA)
c                    update numerical value
                     w(j)=w(j)+aldl(l-shiftA)*x
                     if (jw(j).eq.0) then
                        len=len+1
c                       flag component i as nonzero
                        jw(j)=len
c                       add j to the list of nonzeros
                        jw(n+len)=j
                     end if
                  end do

c                 3.2 [(U_FC(:,k)+W_FC(:,k))^T*A_FF] * W_FC
c                 innermost loop, update with W_FC
                  if (jj.lt.nLU) then
                     do l=jw(nc+jj+1),jw(nc+jj)-1
c                       index j of W_FC(i,j)
                        j=jldl(l-shiftA)
c                      update numerical value
                        w(j)=w(j)+aldl(l-shiftA)*x
                        if (jw(j).eq.0) then
                           len=len+1
c                          flag component i as nonzero
                           jw(j)=len
c                          add j to the list of nonzeros
                           jw(n+len)=j
                        end if
                     end do
                  end if
               end if
            end do
c           end update of the more accurate Schur complement

         else
c           update row k of the Schur complement by subtracting any
c           previous row i=1,...,k-1 depending on the nonzero entries
c           of U(1:k-1,k)
c        
            if (sctype.gt.0) then
c              additional Tismenetsky updates using regular previous rows
c              of U but epsilon size elements of L
c           
c              n         size of the problem
c              k         current step of the update
c              aldl      numerical values
c              jldl      associated indices
c              jldl(2-shiftA)   pointers to the first space behind any row of U
c                        (behind the epsilon size entries)
c              jw(nb+1)  pointers to the first space behind any row of U
c                        (behind the regular entries)
c              jw(n10+1) Ulist  for the epsilon size part of U
c              jw(n8+1)  Ufirst for the epsilon size part of U
c              jw(na+1)  Ufirst for the regular part of U
c              jw        indices of nonzero entries and associated list
c              len       length of this list
c              w         numerical values
               call MYSSYMILUCUPDATE(n,k,
     +                               aldl(1-shiftA),jldl(1-shiftA),
     +                               jldl(2-shiftA),jw(nb+1),
     +                               jw(n10+1),jw(n8+1),jw(na+1),
     +                               jw,len,w)

c              additional Tismenetsky updates using epsilon size previous
c              rows of U but regular elements of L
c           
c              n         size of the problem
c              k         current step of the update
c              aldl      numerical values
c              jldl      associated indices
c              jw(nb+1)  pointers to the first space behind any row of U
c                        (behind the regular entries)
c              jldl(2-shiftA)   pointers to the first space behind any row of U
c                        (behind the epsilon size entries)
c              jw(n6+1)  Ulist  for the regular part of U
c              jw(na+1)  Ufirst for the regular part of U
c              jw(n8+1 ) Ufirst for the epsilon size part of U
c              jw        indices of nonzero entries and associated list
c              len       length of this list
c              w         numerical values
               call MYSSYMILUCUPDATE(n,k,
     +                               aldl(1-shiftA),jldl(1-shiftA),
     +                               jw(nb+1),jldl(2-shiftA),
     +                               jw(n6+1),jw(na+1),jw(n8+1),
     +                               jw,len,w)
            end if
         
c           regular update 
c           n         size of the problem
c           k         current step of the update
c           aldl      numerical values
c           jldl      associated indices
c           jw(nb+1)  pointers to the first space behind any row of U
c                     (behind the regular entries)
c           jw(nb+1)  pointers to the first space behind any row of U
c                     (behind the regular size entries)
c           jw(n6+1)  Ulist  for the regular part of U
c           jw(na+1)  Ufirst for the regular part of U
c           jw(na+1)  Ufirst for the regular part of U
c           jw        indices of nonzero entries and associated list
c           len       length of this list
c           w         numerical values
            call MYSSYMILUCUPDATE(n,k,
     +                            aldl(1-shiftA),jldl(1-shiftA),
     +                            jw(nb+1),jw(nb+1),
     +                            jw(n6+1),jw(na+1),jw(na+1),
     +                            jw,len,w)
         end if
c        end computation of row k of the Schur complement

#ifdef PRINT_INFO
         write (6,'(A,I4,I4)')
     +        'Schur complement including U, row ',k,k-nLU
         write (6,'(10I8)')      (jw(n+l),   l=1,len)
         write (6,'(1P,10E8.1)') (w(jw(n+l)),l=1,len)
#endif
#ifdef PRINT_INFO2
         write (6,'(A,I4,I4)')
     +        'Schur complement including U, row ',k,k-nLU
         write (6,'(10I8)')      (jw(n+l),   l=1,len)
         write (6,'(1P,10E8.1)') (dreal(w(jw(n+l))),l=1,len)
         write (6,'(1P,10E8.1)') (dimag(w(jw(n+l))),l=1,len)
#endif

c        is the diagonal entry structurally zero?
         if (jw(k).eq.0) then
            len=len+1
c           flag component i as nonzero
            jw(k)=len
c           add k to the list of nonzeros
            jw(n+len)=k
c           reset numerical value
            w(k)=FNULL
         end if
c        transfer diagonal entry back including diagonal compensation 
         if (simplesc.ne.0 .or. sctype.ne.0) w(k)=aldl(k-shiftA)

#ifdef PRINT_INFO3
         write (6,'(A,I6)') 'Schur complement, row ',k
         write (6,'(10I8)')      (jw(n+l),   l=1,len)
         write (6,'(1P,10E8.1)') (w(jw(n+l)),l=1,len)
#endif
#ifdef PRINT_INFO2
         write (6,'(A,I4)') 'Schur complement, row ',k
         write (6,'(10I8)')      (jw(n+l),   l=1,len)
         write (6,'(1P,10E8.1)') (dreal(w(jw(n+l))),l=1,len)
         write (6,'(1P,10E8.1)') (dimag(w(jw(n+l))),l=1,len)
#endif

c        --- update approximate Schur complement ---
c        position where the next row of S starts
         jldl(k+1-shiftA)=jldl(k-shiftA)+len
         if (jldl(k+1-shiftA)-1.gt.iwk+shiftA) goto 992
 989     j=1
         maxiwk=max(maxiwk,jldl(k+1-shiftA)-1-shiftA)
         do i=jldl(k-shiftA),jldl(k+1-shiftA)-1
c           extract column index l from the list of nonzeros
            l=jw(n+j)
c           shift column index by nLU to ensure that the indices lie
c           between 1 and n-nLU
            jldl(i-shiftA)=l-nLU
c           extract numerical value
            aldl(i-shiftA)=w(l)
c           auxiliary space w(l)
            w(l)=FNULL
c           clear flag
            jw(l)=0
            j=j+1
         end do

c        Sparsify the approximate Schur complement
c         if (simplesc.ne.0 .or. sctype.ne.0) then
            j=jldl(k-shiftA)-1
            l=jldl(k-shiftA)
c           length of the original row A(p(k),:) is  ia(p(k)+1)-ia(p(k))
            do i=jldl(k-shiftA),jldl(k+1-shiftA)-1
               m=jldl(i-shiftA)
               x=aldl(i-shiftA)
               if (m.eq.k-nLU .or.
     +             ABS(x).gt.droptolU)
     +            then
                  j=j+1
c                 shift and reinsert values
                  jldl(j-shiftA)=m
                  aldl(j-shiftA)=x
c                 store the location of the diagonal entry
                  if (m.eq.k-nLU) l=j
               else
c                 apply diagonal compensation if desired
                  if (milu.gt.0) then
                     if (simplesc.eq.0 .and. sctype.eq.0) then
c                       store the accumulated diagonal compensation
                        w(n2+k)=w(n2+k)+x
                        w(n2+m+nLU)=w(n2+m+nLU)+CONJUG(SKEW(x))
                     end if
c                    apply diagonal compensation to the diagonal entries
                     aldl(k-shiftA)=aldl(k-shiftA)+x
                     aldl(m+nLU-shiftA)=aldl(m+nLU-shiftA)
     +                                 +CONJUG(SKEW(x))
                  end if
               end if
            end do
c           zero row encountered, set diagonal entry to 1
            if (simplesc.ne.0 .or. sctype.ne.0) then
c              transfer separately stored diagonal entry to compressed row
c              data structure
               aldl(l-shiftA)=aldl(k-shiftA)
            end if

c           new start of row k+1 
            jldl(k+1-shiftA)=j+1

c         end if
c        END Sparsify the approximate Schur complement



c        Update Ufirst, Ulist 
c
c        n         size of the problem
c        k         current step of the update procedure
c        jldl      index array
c        jw(nb+1)  pointers to the first space behind any row of U
c                  (points behind the space of the regular part)
c        jw(n6+1)  linked list for the nonzeros of U in column k
c                  (regular part)
c        jw(na+1)  first nonzero entry in U(i,k:n)
c                  (regular part)
         call piluclist(n,k,jldl(1-shiftA),jw(nb+1),jw(n6+1),jw(na+1))

c        Tismenetsky update
         if (sctype.gt.0) then

c           n         size of the problem
c           k         current step of the update procedure
c           jldl      index array
c           jldl(2-shiftA)   pointers to the first space behind any row of U
c                     (points behind the space of the epsilon part)
c           jw(n10+1) linked list for the nonzeros of U in column k
c                     (epsilon size part)
c           jw(n8+1)  first nonzero entry in U(i,k:n)
c                     (epsilon size part)
            call piluclist(n,k,jldl(1-shiftA),jldl(2-shiftA), 
     +                     jw(n10+1),jw(n8+1))
         end if

c        alternative approximate Schur complement, update Wfirst, Wlist
         if (simplesc.eq.0 .and. sctype.eq.0) then
c           n         size of the problem
c           k         current step of the update procedure
c           jldl       index array
c           jw(nc+1)  pointers to the first space behind any row of W_FC
c                     (points to the start of W_FC, array shifted by one)
c           jw(ne+1)  linked list for the nonzeros of W in row k
c           jw(nl+1)  first nonzero entry in W(k:n,i)
            call piluclist(n,k,jldl(1-shiftA),jw(nc+1),
     +                     jw(ne+1),jw(nl+1))
         end if

c        skip garbage collection
         goto 740

c        out of memory, try to get more free space using garbage collection
c        truncate those parts of U that are not needed anymore
 990     len=0
         write (6,'(A)') 'garbage collection'
         goto 992
         l=1
c        while l<=nLU
 510     if (l.gt.nLU) goto 520
c           U-part
c           shift the whole row

c           1x1 case
            if (jldl(n+1+l-shiftA).eq.0) then
               do j=jldl(l-shiftA),jw(nb+l)-1
c                 shift values
                  jldl(j-len-shiftA)=jldl(j-shiftA)
                  aldl(j-len-shiftA)=aldl(j-shiftA)
               end do
c              adjust the associated pointer(s)
c              new start of row l
               jldl(l-shiftA)=jldl(l-shiftA)-len
c              new Ufirst
               jw(na+l)=jw(na+l)-len
c              new copy of the original Ufirst
               if (simplesc.eq.0 .and. sctype.eq.0)
     +            jw(n2+l)=jw(n2+l)-len


c              Tismenetsky updates
               if (sctype.gt.0) then
c                 shift the remaining part, starting at Ufirst, epsilon
                  jj=jw(n8+l)-jw(nb+l)
c                 adjust the associated pointer(s)
                  jw(nb+l)=jw(nb+l)-len
                  len=len+jj
                  do j=jw(n8+l),jldl(l+1-shiftA)-1
c                    shift values
                     jldl(j-len-shiftA)=jldl(j-shiftA)
                     aldl(j-len-shiftA)=aldl(j-shiftA)
                  end do
c                 adjust the associated pointer(s)
c                 new Ufirst, epsilon
                  jw(n8+l)=jw(n8+l)-len
               else
c                 adjust the associated pointer(s)
c                 new pointer to the space behind row l
                  jw(nb+l)=jw(nb+l)-len
               end if 
               l=l+1

c           2x2 case
            else
               ii=jldl(l-shiftA)
               do j=jldl(l-shiftA),jw(nb+l)-1
c                 shift values
                  jldl(j-len-shiftA)=jldl(j-shiftA)
                  aldl(ii-len-shiftA)  =aldl(ii-shiftA)
                  aldl(ii+1-len-shiftA)=aldl(ii+1-shiftA)
                  ii=ii+2
               end do
c              adjust the associated pointer(s)
c              new start of row l,l+1
               jldl(l-shiftA)  =jldl(l-shiftA)-len
               jldl(l+1-shiftA)=jldl(l+1-shiftA)-len
c              new Ufirst
               jw(na+l)=jw(na+l)-len
c              new copy of the original Ufirst
               if (simplesc.eq.0 .and. sctype.eq.0)
     +            jw(n2+l)=jw(n2+l)-len


c              Tismenetsky updates
               if (sctype.gt.0) then
c                 shift the remaining part, starting at Ufirst, epsilon
                  jj=jw(n8+l)-jw(nb+l)
c                 adjust the associated pointer(s)
                  jw(nb+l)=jw(nb+l)-len
                  len=len+2*jj
                  ii=jldl(l-shiftA)+2*(jw(n8+l)-jldl(l-shiftA))
                  do j=jw(n8+l),jldl(l+1-shiftA)-1
c                    shift values
                     jldl(j-len-shiftA)=jldl(j-shiftA)
                     aldl(ii-len-shiftA)  =aldl(ii-shiftA)
                     aldl(ii+1-len-shiftA)=aldl(ii+1-shiftA)
                     ii=ii+2
                  end do
c                 adjust the associated pointer(s)
c                 new Ufirst, epsilon
                  jw(n8+l)=jw(n8+l)-len
               else
c                 adjust the associated pointer(s)
c                 new pointer to the space behind row l
                  jw(nb+l)=jw(nb+l)-len
               end if 

               l=l+2
            end if
         goto 510
c        end shift U


c        shift workspace W_FC
 520     if (simplesc.eq.0 .and. sctype.eq.0) then
            l=nLU-1
c           check if the last update is a 2x2 update
            if (l.gt.1) then
               if (jldl(n+l-shiftA).gt.0) then
                  l=l-1
                  mode=1
               else
                  mode=0
               end if
            else 
               mode=0
            end if
c           while l>=1
 530        if (l.lt.1) goto 540
               if (mode.eq.0) then
c                 W_FC-part
c                 shift all entries
                  do j=jw(nc+l+1),jw(nc+l)-1
c                    shift values
                     jldl(j-len-shiftA)=jldl(j-shiftA)
                     aldl(j-len-shiftA)=aldl(j-shiftA)
                  end do
c                 adjust the associated pointer(s)
c                 new Wfirst
                  jw(nl+l)=jw(nl+l)-len
c                 new pointer to the beginning of each row
                  jw(nc+l+1)=jw(nc+l+1)-len
               else
c                 W_FC-part
c                 shift all entries
                  ii=jw(nc+l+2)
                  do j=jw(nc+l+2),jw(nc+l+1)-1
c                    shift values
                     jldl(j-len-shiftA)=jldl(j-shiftA)
                     aldl(ii-len-shiftA)  =aldl(ii-shiftA)
                     aldl(ii+1-len-shiftA)=aldl(ii+1-shiftA)
                     ii=ii+2
                  end do
c                 adjust the associated pointer(s)
c                 new Wfirst
                  jw(nl+l)=jw(nl+l)-len
c                 new pointer to the beginning of each row
                  jw(nc+l+2)=jw(nc+l+2)-len
                  jw(nc+l+1)=jw(nc+l+1)-len
               end if

c              decide whether to consider a 1x1 or a 2x2 update next
               l=l-1
               if (l.gt.1) then
                  if (jldl(n+l-shiftA).gt.0) then
                     l=l-1
                     mode=1
                  else
                     mode=0
                  end if
               else 
                  mode=0
               end if
            goto 530
c           shift final reference
 540        if (mode.eq.0) then
               jw(nc+1)=jw(nc+1)-len
            else
               jw(nc+1)=jw(nc+1)-len
               jw(nc+2)=jw(nc+2)-len
            end if
         end if
c        end shift W_FC

c        shift approximate Schur-complement
         do l=nLU+1,k-1
            do m=jldl(l-shiftA),jldl(l+1-shiftA)
               aldl(m-len-shiftA)=aldl(m-shiftA)
               jldl(m-len-shiftA)=jldl(m-shiftA)
            end do
c           adjust pointer
            jldl(l-shiftA)=jldl(l-shiftA)-len
         end do
c        adjust final pointer
         jldl(k-shiftA)=jldl(k-shiftA)-len
c        adjust new pointer
         jldl(k+1-shiftA)=jldl(k+1-shiftA)-len

c        did we successfully reduce the memory?
         if (jldl(k+1-shiftA)-1.gt.iwk+shiftA) then
c           no! out of memory
            goto 992
         else
c           yes! resume 
            maxiwk=max(maxiwk,jldl(k+1-shiftA)-1-shiftA)
            goto 989
         end if
c        garbage collection completed
 740     continue
      end do



c     advance pointers to the space behind the regular part by the
c     number of skipped entries that are located in front of them
      if (tailor.gt.0) then
         k=1
c        while k<=nLU
 550     if (k.gt.nLU) goto 560
c           U-part
c           position of the last regular entry in column k
            j=jw(nb+k)-1
c           associated column index
            l=jldl(j-shiftA)
c           while l>nLU & j>=jldl(k-shiftA)
 606        if (l.le.nLU .or. j.lt.jldl(k-shiftA)) goto 607
               j=j-1
               l=jldl(j-shiftA)
            goto 606
c           end while
 607        jw(nb+k)=j+1
c           1x1 or 2x2 case?
            if (jldl(n+1+k-shiftA).eq.0) then
               k=k+1
            else
               k=k+2
            end if
         goto 550
c        end while
 560     j=j
      end if




#ifdef PRINT_INFO
      k=1
      write (6,'(A)') '1.current U-factor'
c     while k<=nLU
 501  if (k.gt.nLU) goto 502
         if (jldl(n+1+k-shiftA).eq.0) then
            write (6,'(10I8)') (jldl(i-shiftA),
     +           i=jldl(k-shiftA),jldl(k+1-shiftA)-1)
            write (6,'(1P,10E8.1)') (aldl(i-shiftA),
     +           i=jldl(k-shiftA),jldl(k+1-shiftA)-1)
            k=k+1
         else
            write (6,'(10I8)') (jldl(i-shiftA), 
     +           i=jldl(k-shiftA),jldl(k+1-shiftA)-1)
            write (6,'(1P,10E8.1)') 
     +            (aldl(jldl(k-shiftA)+2*(i-jldl(k-shiftA))-shiftA),
     +           i=jldl(k-shiftA),jldl(k+1-shiftA)-1)
            write (6,'(1P,10E8.1)') 
     +          (aldl(jldl(k-shiftA)+2*(i-jldl(k-shiftA))+1-shiftA),
     +           i=jldl(k-shiftA),jldl(k+1-shiftA)-1)
            k=k+2
         end if
      goto 501
c     end while
 502  k=1
      write (6,'(A)') 'current block diagonal part'
c     while k<=nLU
 503  if (k.gt.nLU) goto 504
         if (jldl(n+1+k-shiftA).eq.0) then
            write (6,'(1P,E8.1)') aldl(k-shiftA)
            k=k+1
         else
            write (6,'(1P,E8.1,1P,E8.1)') 
     +            aldl(k-shiftA),aldl(n+1+k-shiftA)
            write (6,'(1P,E8.1,1P,E8.1)') 
     +            CONJUG(SKEW(aldl(n+1+k-shiftA))),
     +            aldl(k+1-shiftA)
            k=k+2
         end if
      goto 503
c     end while
 504  k=1
#endif
#ifdef PRINT_INFO2
      k=1
      write (6,'(A)') '1.current U-factor'
c     while k<=nLU
 501  if (k.gt.nLU) goto 502
         if (jldl(n+1+k-shiftA).eq.0) then
            write (6,'(10I8)') (jldl(i-shiftA),
     +           i=jldl(k-shiftA),jldl(k+1-shiftA)-1)
         write (6,'(1P,10E8.1)') (dreal(aldl(i-shiftA)),
     +           i=jldl(k-shiftA),jldl(k+1-shiftA)-1)
         write (6,'(1P,10E8.1)') (dimag(aldl(i-shiftA)),
     +        i=jldl(k-shiftA),jldl(k+1-shiftA)-1)
            k=k+1
         else
            write (6,'(10I8)') (jldl(i-shiftA), 
     +           i=jldl(k-shiftA),jldl(k+1-shiftA)-1)
            write (6,'(1P,10E8.1)') 
     +       (dreal(aldl(jldl(k-shiftA)+2*(i-jldl(k-shiftA))-shiftA)),
     +           i=jldl(k-shiftA),jldl(k+1-shiftA)-1)
            write (6,'(1P,10E8.1)') 
     +       (dimag(aldl(jldl(k-shiftA)+2*(i-jldl(k-shiftA))-shiftA)), 
     +           i=jldl(k-shiftA),jldl(k+1-shiftA)-1)
            write (6,'(1P,10E8.1)') 
     +     (dreal(aldl(jldl(k-shiftA)+2*(i-jldl(k-shiftA))+1-shiftA)),
     +           i=jldl(k-shiftA),jldl(k+1-shiftA)-1)
            write (6,'(1P,10E8.1)') 
     +     (dimag(aldl(jldl(k-shiftA)+2*(i-jldl(k-shiftA))+1-shiftA)),
     +           i=jldl(k-shiftA),jldl(k+1-shiftA)-1)
            k=k+2
         end if
      goto 501
c     end while
 502  k=1
      write (6,'(A)') 'current block diagonal part'
c     while k<=nLU
 503  if (k.gt.nLU) goto 504
         if (jldl(n+1+k-shiftA).eq.0) then
            write (6,'(1P,E8.1)') aldl(k-shiftA)
            k=k+1
         else
            write (6,'(1P,E8.1,A,1P,E8.1)') 
     +            dreal(aldl(k-shiftA)),'+i*',
     +            dimag(aldl(k-shiftA))
            write (6,'(1P,E8.1,A,1P,E8.1)') 
     +            dreal(aldl(n+k+1-shiftA)),'+i*',
     +            dimag(aldl(n+1+k-shiftA))
            write (6,'(1P,E8.1,A,1P,E8.1)') 
     +            dreal(CONJUG(SKEW(aldl(n+1+k-shiftA)))),'+i*',
     +            dimag(CONJUG(SKEW(aldl(n+1+k-shiftA))))
            write (6,'(1P,E8.1,A,1P,E8.1)') 
     +            dreal(aldl(k+1-shiftA)),'+i*',
     +            dimag(aldl(k+1-shiftA))
            k=k+2
         end if
      goto 503
c     end while
 504  k=1
#endif







c     remove the embedded small and skipped entries
#if defined PRINT_INFO || defined PRINT_INFO2
      do k=1,nLU+1
         write (6,'(A,I6,A,I6,I6)') 
     +        'row ',k,', previous starts: ',jldl(k-shiftA),jw(nb+k)
      end do
#endif
      len=0
      k=1
c     while "k<nLU" ...
 600  if (k.ge.nLU .or. (k.ge.nLU-1 .and. jldl(n+1+k-shiftA).gt.0))
     +     goto 610

c        1x1 case
         if (jldl(n+1+k-shiftA).eq.0) then
c           gap behind row k of U
            len=len+jldl(k+1-shiftA)-jw(nb+k)
c           shift U part
c           the following row is a regular row
            if (jldl(n+1+k+1-shiftA).eq.0) then
               do j=jldl(k+1-shiftA),jw(nb+k+1)-1
                  aldl(j-len-shiftA)=aldl(j-shiftA)
                  jldl(j-len-shiftA)=jldl(j-shiftA)
               end do
               jldl(k+1-shiftA)=jldl(k+1-shiftA)-len

c           the following row is a block row
            else
               ii=jldl(k+1-shiftA)
               do j=jldl(k+1-shiftA),jw(nb+k+1)-1
                  aldl(ii-len-shiftA)  =aldl(ii-shiftA)
                  aldl(ii+1-len-shiftA)=aldl(ii+1-shiftA)
                  jldl(j-len-shiftA)=jldl(j-shiftA)
                  ii=ii+2
               end do
               jldl(k+1-shiftA)=jldl(k+1-shiftA)-len
            end if
            k=k+1

c        2x2 case
         else
c           gap behind row k,k+1 of U
            l=jldl(k+1-shiftA)-jw(nb+k)
            jldl(k+1-shiftA)=jldl(k+1-shiftA)-len
            len=len+2*l
c           shift U part
c           the following row is a regular row
            if (jldl(n+1+k+2-shiftA).eq.0) then
               do j=jldl(k+2-shiftA),jw(nb+k+2)-1
                  aldl(j-len-shiftA)=aldl(j-shiftA)
                  jldl(j-len-shiftA)=jldl(j-shiftA)
               end do
               jldl(k+2-shiftA)=jldl(k+2-shiftA)-len
            else
               ii=jldl(k+2-shiftA)
               do j=jldl(k+2-shiftA),jw(nb+k+2)-1
                  aldl(ii-len-shiftA)  =aldl(ii-shiftA)
                  aldl(ii+1-len-shiftA)=aldl(ii+1-shiftA)
                  jldl(j-len-shiftA)=jldl(j-shiftA)
                  ii=ii+2
               end do
c              shift the next block physically by len
               jldl(k+2-shiftA)=jldl(k+2-shiftA)-len
            end if
c           re-adjust logical start of row k+1
            jldl(k+1-shiftA)=jldl(k+1-shiftA)-l
            k=k+2
         end if
      goto 600
c     final column/row
 610  if (k.eq.nLU) then
c        gap behind row k of U
c        this also includes W_FC
         len=len+jldl(k+1-shiftA)-jw(nb+k)
      else
c        re-adjust logical start of row k+1
         jldl(k+1-shiftA)=jldl(k-shiftA)
c        gap behind row k of U
c        this also includes W_FC
         len=len+jldl(k+2-shiftA)-jw(nb+k)
      end if

c     if the first row is a block row, then adjust the
c     internal reference jldl(2-shiftA)
      if (jldl(n+2-shiftA).ne.0) then
         jldl(2-shiftA)=jw(nb+1)
      end if



      
#if defined PRINT_INFO || defined PRINT_INFO2
      do k=1,nLU+1
         write (6,'(A,I6,A,I6)') 
     +        'row ',k,', start: ',jldl(k-shiftA)
      end do
#endif








c     Finally merge S and S^T and restrict it to its upper triangular part
      if (simplesc.eq.0 .and. sctype.eq.0) then
c        sort entries in increasing order to avoid duplicate entries
c        natural ordering

         do k=nLU+1,n

c           find lower triangular part if it exists
c           initial start of the upper triangular part
            m=jldl(k-shiftA)
            do j=jldl(k-shiftA),jldl(k+1-shiftA)-1
c              index in the lower triangular part?
               if (jldl(j-shiftA)+nLU.le.k) then
c                 swap entries
                  l=jldl(m-shiftA)
                  jldl(m-shiftA)=jldl(j-shiftA)
                  jldl(j-shiftA)=l
                  x=aldl(m-shiftA)
                  aldl(m-shiftA)=aldl(j-shiftA)
                  aldl(j-shiftA)=x
c                 increase counter
                  m=m+1
               end if
            end do
            j=jldl(k-shiftA)
c           if the lower triangular part is present
            if (m.gt.j) then
c              separate lower triangular part including diagonal entry
c              from the strict upper triangular part

c              number of entries in the lower triangular part
               l=m-j
c              sort indices of the lower triangular part (including
c              the  diagonal entry) in increasing order
               call QQSORTS(aldl(j-shiftA),jldl(j-shiftA),jw(n+1),l)
               m=j+l-1

c              where does the upper triangular part begin?
               if (jldl(m-shiftA)+nLU.eq.k) then
c                 diagonal entry present
                  jw(nc+k)=m
c                 duplicate diagonal entry and incorporate diagonal 
c                 compensation
                  aldl(m-shiftA)=aldl(m-shiftA)
     +                      +CONJUG(SKEW(aldl(m-shiftA))+w(n2+k))
                  w(n2+k)=FNULL
               else
c                 no diagonal entry present
                  jw(nc+k)=m+1
               end if

            else
c              only strict upper triangular part present
               jw(nc+k)=j
            end if
c           clear head of the linked list
            jw(n2+k)=0
         end do

c        use jw(n2) for lower triangular part of S, head of the linked lists
c            jw(n3) for lower triangular part of S, linked list
c            jw(n4) for lower triangular part of S, first entry
c            jw(nc) start of S, upper triangular part

c        for any column check how many additional entries are encountered
         do k=nLU+1,n
            j=jldl(k-shiftA)
c           pointer to the current first nonzero entry in row k
            jw(n4+k)=j
c           associated column index l
            l=jldl(j-shiftA)

c           set up linked list
            if (l+nLU.lt.k .and. j.lt.jldl(k+1-shiftA)) then
c              add new entry to the head of the list
               jw(n3+k)=jw(n2+l+nLU)
               jw(n2+l+nLU)=k
            end if

         end do

c        initial r
         r=jldl(nLU+1-shiftA)
         do k=nLU+1,n
c           start with row k, beginning with the diagonal entry
c           copy row k to the buffer, starting with the diagonal part
            jj=0
            do j=jw(nc+k),jldl(k+1-shiftA)-1
               jj=jj+1
c              associated column index i
               i=jldl(j-shiftA)
               jw(n+jj)=i
               w(i)=aldl(j-shiftA)
               jw(i)=jj
            end do

c           extract lower triangular part of S, column k
c           initial row index
            i=jw(n2+k)
 760        if (i.le.0) goto 770

c              pointer to S(i,k)
               j=jw(n4+i)
c              associated column index l of S(i,l+nLU), l should be k
               l=jldl(j-shiftA)
c              get next row from the linked list
               m=jw(n3+i)

               w(i-nLU)=w(i-nLU)+CONJUG(SKEW(aldl(j-shiftA)))
c              is this a fill-in?
               if (jw(i-nLU).eq.0) then
                  jj=jj+1
c                 associated column index
                  jw(n+jj)=i-nLU
                  jw(i-nLU)=jj
               end if

c              update column information
               if (j.lt.jw(nc+i)) j=j+1
c              pointer to the next nonzero element in row i
               jw(n4+i)=j
               if (j.lt.jw(nc+i)) then
c                 associated column index
                  l=jldl(j-shiftA)
c                 add new entry to the head of the list
                  jw(n3+i)=jw(n2+l+nLU)
                  jw(n2+l+nLU)=i
               end if

c              recover next row entry
               i=m
            goto 760
c           end while

c           Sparsify the approximate Schur complement
c           make sure that the diagonal entry is present
 770        if (jw(k-nLU).eq.0) then
               jj=jj+1
               jw(n+jj)=k-nLU
               jw(k-nLU)=jj
               w(k-nLU)=FNULL
            end if
            i=1
c           while i<=jj
 741        if (i.gt.jj) goto 742
               m=jw(n+i)
               x=w(m)
               if (m.ne.k-nLU .and.
     +             ABS(x).le.droptolU*
     +             min(ABS(aldl(k-shiftA)),ABS(aldl(m+nLU-shiftA))))
     +            then
c                 apply diagonal compensation if desired
                  if (milu.gt.0) then
c                    store the accumulated diagonal compensation
                     w(n2+k)=w(n2+k)+x
                     w(n2+m+nLU)=w(n2+m+nLU)+CONJUG(SKEW(x))
c                    apply diagonal compensation to the diagonal entries
                     aldl(k-shiftA)=aldl(k-shiftA)+x
                     aldl(m+nLU-shiftA)=aldl(m+nLU-shiftA)
     +                                 +CONJUG(SKEW(x))
                  end if
c                 shuffle last entry to the front
                  jw(n+i)=jw(n+jj)
                  jw(m)=0
                  w(m)=FNULL
                  jj=jj-1
               else 
                  i=i+1
               end if
            goto 741
c           end while

c           include diagonal compensation
 742        w(k)=w(k)+w(n2+k)

c           new start of row k
            jldl(k-shiftA)=jldl(k-shiftA)-len
c           check whether there is still enough space 
            if (jldl(k-shiftA)+jj.gt.jldl(k+1-shiftA)) then
c              if not, shift the remaining rows by n
c              out of memory?
               if (jldl(n+1-shiftA)-1+n.gt.iwk+shiftA) goto 992
               maxiwk=max(maxiwk,jldl(n+1-shiftA)-1+n-shiftA)
               do l=n,k+1,-1
                  do m=jldl(l+1-shiftA)-1,jldl(l-shiftA),-1
                     aldl(m+n-shiftA)=aldl(m-shiftA)
                     jldl(m+n-shiftA)=jldl(m-shiftA)
                  end do
c                 shift pointers
                  jldl(l+1-shiftA)=jldl(l+1-shiftA)+n
                  jw(n4+l)=jw(n4+l)+n
                  jw(nc+l)=jw(nc+l)+n
               end do
c              shift final reference
               jldl(k+1-shiftA)=jldl(k+1-shiftA)+n
            end if

c           now we have enough space
c           transfer buffer back
            r=jldl(k-shiftA)
            do i=1,jj
               j=jw(n+i)
               aldl(r-shiftA)=w(j)
               jldl(r-shiftA)=j
               w(j)=FNULL
               jw(j)=0
               r=r+1
            end do

c           new gap
            len=jldl(k+1-shiftA)-r

         end do
c        adapt final reference
         jldl(n+1-shiftA)=r
      else
c        shift the approximate Schur complement
         do j=jldl(nLU+1-shiftA),jldl(n+1-shiftA)-1
            aldl(j-len-shiftA)=aldl(j-shiftA)
            jldl(j-len-shiftA)=jldl(j-shiftA)
         end do
         do k=nLU+1,n+1
#if defined PRINT_INFO || defined PRINT_INFO2
            write (6,'(A,I6,A,I6,A,I6)') 
     +  'row ',k,', old start: ',jldl(k-shiftA),
     +           ', new start: ',jldl(k-shiftA)-len
#endif
            jldl(k-shiftA)=jldl(k-shiftA)-len
         end do
      end if




#ifdef PRINT_INFO2
      k=1
      write (6,'(A)') '2.current U-factor'
c     while k<=nLU
 505  if (k.gt.nLU) goto 506
         if (jldl(n+1+k-shiftA).eq.0) then
            write (6,'(10I8)') (jldl(i-shiftA),
     +           i=jldl(k-shiftA),jldl(k+1-shiftA)-1)
         write (6,'(1P,10E8.1)') (dreal(aldl(i-shiftA)),
     +           i=jldl(k-shiftA),jldl(k+1-shiftA)-1)
         write (6,'(1P,10E8.1)') (dimag(aldl(i-shiftA)),
     +        i=jldl(k-shiftA),jldl(k+1-shiftA)-1)
            k=k+1
         else
            write (6,'(10I8)') (jldl(i-shiftA),
     +           i=jldl(k-shiftA),jldl(k+1-shiftA)-1)
            write (6,'(1P,10E8.1)') 
     +       (dreal(aldl(jldl(k-shiftA)+2*(i-jldl(k-shiftA))-shiftA)),
     +           i=jldl(k-shiftA),jldl(k+1-shiftA)-1)
            write (6,'(1P,10E8.1)') 
     +       (dimag(aldl(jldl(k-shiftA)+2*(i-jldl(k-shiftA))-shiftA)),
     +           i=jldl(k-shiftA),jldl(k+1-shiftA)-1)
            write (6,'(1P,10E8.1)') 
     +     (dreal(aldl(jldl(k-shiftA)+2*(i-jldl(k-shiftA))+1-shiftA)),
     +           i=jldl(k-shiftA),jldl(k+1-shiftA)-1)
            write (6,'(1P,10E8.1)') 
     +     (dimag(aldl(jldl(k-shiftA)+2*(i-jldl(k-shiftA))+1-shiftA)),
     +           i=jldl(k-shiftA),jldl(k+1-shiftA)-1)
            k=k+2
         end if
      goto 505
c     end while
 506  k=1
      write (6,'(A)') 'current block diagonal part'
c     while k<=nLU
 507  if (k.gt.nLU) goto 508
         if (jldl(n+1+k-shiftA).eq.0) then
            write (6,'(1P,E8.1,A,1P,E8.1)')
     +           dreal(aldl(k-shiftA)),'+i*',dimag(aldl(k-shiftA))
            k=k+1
         else
            write (6,'(1P,E8.1,A,1P,E8.1,1P,E8.1,A,1P,E8.1)') 
     +            dreal(aldl(k-shiftA)),'+i*',dimag(aldl(k-shiftA)),
     +            dreal(aldl(n+1+k-shiftA)),'+i*',
     +            dimag(aldl(n+1+k-shiftA))
            write (6,'(1P,E8.1,A,1P,E8.1,1P,E8.1,A,1P,E8.1)') 
     +            dreal(CONJUG(SKEW(aldl(n+1+k-shiftA)))),'+i*',
     +            dimag(CONJUG(SKEW(aldl(n+1+k-shiftA)))),
     +            dreal(aldl(k+1-shiftA)),'+i*',
     +            dimag(aldl(k+1-shiftA))
            k=k+2
         end if
      goto 507
c     end while
 508  write (6,'(A)') 'approximate Schur complement'
      do k=nLU+1,n
         write (6,'(10I8)') (jldl(i-shiftA),
     +        i=jldl(k-shiftA),jldl(k+1-shiftA)-1)
         write (6,'(1P,10E8.1)') (dreal(aldl(i-shiftA)),
     +        i=jldl(k-shiftA),jldl(k+1-shiftA)-1)
         write (6,'(1P,10E8.1)') (dimag(aldl(i-shiftA)),
     +        i=jldl(k-shiftA),jldl(k+1-shiftA)-1)
      end do
#endif





      ierr = 0
      iwk=maxiwk
      return
c     END regular routine



c     -----   ERROR handling   -----
c

c
c     incomprehensible error. Matrix must be wrong.
c     
 991  ierr = -1
      return
c     
c     insufficient storage in U
c     
 992  ierr = -3
      return
c     
c     illegal lfil entered.
c     
 994  ierr = -4
      return
c
c     zero row encountered
c     
 995  ierr = -5
      return
c----------------end-of-sympiluc----------------------------------------
c-----------------------------------------------------------------------
      end


      
            
      subroutine MYSYMPILUCUPDATE(n,k,aldl,jldl,lnext,unext,Llist,
     +     Lfirst,Ufirst,jw1,jw2,len,w,cstart,clen,mode)
c-----------------------------------------------------------------------
      implicit none 
      integer n,k,len,cstart(n),clen(n),mode
      integer jldl(*),lnext(n),unext(n),Llist(*),Lfirst(*),
     +        Ufirst(k-1),jw1(n),jw2(n)
      FLOAT aldl(*),w(n)
c----------------------------------------------------------------------*
c     update row k+mode of the Schur complement
c
c     n         size of the problem
c     k         current step of the update procedure
c     aldl      numerical values
c     jldl      associated indices
c     lnext     pointers to the first space behind any column of L
c     unext     pointers to the first space behind any row of U
c     Llist     linked list for the nonzeros of L in row k
c     Lfirst    first nonzero entry in L(k:n,i)
c     Ufirst    first nonzero entry in U(i,k:n)
c     jw1       indices of nonzero entries and
c     jw2       associated list
c     len       length of this list
c     w         numerical values
c     cstart    pointers to the start of the L-part
c     clen      number of skipped entries, located at the start of
c               every column of L
c     mode      if mode=0, row k is updated, otherwise (mode=1) 
c               row k+1 is updated using the linked lists of 
c               row k and row k+1

      integer i,j,jj,l,ii
      FLOAT x,x2

      i=Llist(k)
c     while i>0
 10      if (i.eq.0) goto 100
c        does there exist an entry in L(k+mode:n,i)?
c        Or to be more precise, is the L(k+mode,i)<>0?
         jj=Lfirst(i)+mode
         if (jj.lt.lnext(i)) then
c           if this is the case, is the first entry equal to k,
c           otherwise we don't need to do an update because L(k+mode,i)=0
            if (jldl(jj).eq.k+mode) then
c              1x1 update
               if (jldl(n+1+i).eq.0) then
c                 L(k+mode,i)/D(i,i)
                  x=CONJUG(SKEW(aldl(jj)))*aldl(i)

c              2x2 update
               else
                  ii=jldl(i)+2*(jj-jldl(i))
c                 L(k+mode,i:i+1)*A(i:i+1,i:i+1)^{-1}
                  x =CONJUG(SKEW(aldl(ii)))  *aldl(i)
     +              +CONJUG(SKEW(aldl(ii+1)))*CONJUG(SKEW(aldl(n+1+i)))
                  x2=CONJUG(SKEW(aldl(ii)))  *aldl(n+1+i)
     +              +CONJUG(SKEW(aldl(ii+1)))*aldl(i+1)
               end if
c              nonzeros in row i of U, start at position at least k
               jj=Ufirst(i)
c              if the first index is k and we want to update row k, then 
c              we can skip it, since we don't have to recompute the 
c              diagonal entry
               if (jj.lt.unext(i) .and. mode.eq.0) then
                  j=jldl(jj)
                  if (j.eq.k) jj=jj+1
               end if

c              innermost loop, update 
               if (jldl(n+1+i).eq.0) then
c                 1x1 update
                  do l=jj,unext(i)-1
                     j=jldl(l)
                     w(j)=w(j)-aldl(l)*x
                     if (jw1(j).eq.0 .and. 
     +                   w(j).ne.FNULL) then
                        len=len+1
                        jw1(j)=len
                        jw2(len)=j
                     end if
                  end do
               else
c                 2x2 update
                  ii=jldl(i)+2*(jj-jldl(i))
                  do l=jj,unext(i)-1
                     j=jldl(l)
                     w(j)=w(j)-aldl(ii)*x-aldl(ii+1)*x2
                     if (jw1(j).eq.0 .and.
     +                   w(j).ne.FNULL) then
                        len=len+1
                        jw1(j)=len
                        jw2(len)=j
                     end if
                     ii=ii+2
                  end do
               end if

c              do there exist coarse grid entries in U(i,i+1:n)?
c              innermost loop, update 
               if (jldl(n+1+i).eq.0) then
c                 1x1 update
                  do l=cstart(i),cstart(i)+clen(i)-1
                     j=jldl(l)
                     w(j)=w(j)-aldl(l)*x
                     if (jw1(j).eq.0 .and.
     +                   w(j).ne.FNULL) then
                        len=len+1
                        jw1(j)=len
                        jw2(len)=j
                     end if
                  end do
               else
c                 2x2 update
                  ii=jldl(i)+2*(cstart(i)-jldl(i))
                  do l=cstart(i),cstart(i)+clen(i)-1
                     j=jldl(l)
                     w(j)=w(j)-aldl(ii)*x-aldl(ii+1)*x2
                     if (jw1(j).eq.0 .and.
     +                   w(j).ne.FNULL) then
                        len=len+1
                        jw1(j)=len
                        jw2(len)=j
                     end if
                     ii=ii+2
                  end do
               end if
            end if 
         end if
         i=Llist(i)
      goto 10
c     end while

c     also use the existing linked list for column k+1
 100  if (mode.eq.1) then
         i=Llist(k+1)
c        while i>0
 20         if (i.eq.0) goto 999
c           does there exist an entry in L(k+1:n,i)?
c           Or to be more precise, is the L(k+1,i)<>0?
            jj=Lfirst(i)
            if (jj.lt.lnext(i)) then
c              if this is the case, is the first entry equal to k+1,
c              otherwise we don't need to do an update because L(k+1,i)=0
               if (jldl(jj).eq.k+1) then
c                 1x1 update
                  if (jldl(n+1+i).eq.0) then
c                   L(k+1,i)/D(i,i)
                    x=CONJUG(SKEW(aldl(jj)))*aldl(i)

c                 2x2 update
                  else
                    ii=jldl(i)+2*(jj-jldl(i))
c                   L(k+mode,i:i+1)*A(i:i+1,i:i+1)^{-1}
                    x =CONJUG(SKEW(aldl(ii)))  *aldl(i)
     +               +CONJUG(SKEW(aldl(ii+1)))*CONJUG(SKEW(aldl(n+1+i)))
                    x2=CONJUG(SKEW(aldl(ii)))  *aldl(n+1+i)
     +               +CONJUG(SKEW(aldl(ii+1)))*aldl(i+1)
                  end if
c                 nonzeros in row i of U, start at position at least k+1
                  jj=Ufirst(i)

c                 if the first index is k+1, then we can skip it, since we
c                 don't have to recompute the diagonal entry
                  if (jj.lt.unext(i)) then
                     j=jldl(jj)
                     if (j.eq.k+1) jj=jj+1
                  end if

c                 innermost loop, update 
                  if (jldl(n+1+i).eq.0) then
c                    1x1 update
                     do l=jj,unext(i)-1
                        j=jldl(l)
                        w(j)=w(j)-aldl(l)*x
                        if (jw1(j).eq.0 .and. 
     +                      w(j).ne.FNULL) then
                           len=len+1
                           jw1(j)=len
                           jw2(len)=j
                        end if
                     end do
                  else
c                    2x2 update
                     ii=jldl(i)+2*(jj-jldl(i))
                     do l=jj,unext(i)-1
                        j=jldl(l)
                        w(j)=w(j)-aldl(ii)*x-aldl(ii+1)*x2
                        if (jw1(j).eq.0 .and. 
     +                      w(j).ne.FNULL) then
                           len=len+1
                           jw1(j)=len
                           jw2(len)=j
                        end if
                        ii=ii+2
                     end do
                  end if

c                 do there exist coarse grid entries in U(i,i+1:n)?
c                 innermost loop, update 
                  if (jldl(n+1+i).eq.0) then
c                    1x1 update
                     do l=cstart(i),cstart(i)+clen(i)-1
                        j=jldl(l)
                        w(j)=w(j)-aldl(l)*x
                        if (jw1(j).eq.0 .and.
     +                      w(j).ne.FNULL) then
                           len=len+1
                           jw1(j)=len
                           jw2(len)=j
                        end if
                     end do
                  else
c                    2x2 update
                     ii=jldl(i)+2*(cstart(i)-jldl(i))
                     do l=cstart(i),cstart(i)+clen(i)-1
                        j=jldl(l)
                        w(j)=w(j)-aldl(ii)*x-aldl(ii+1)*x2
                        if (jw1(j).eq.0 .and. 
     +                      w(j).ne.FNULL) then
                           len=len+1
                           jw1(j)=len
                           jw2(len)=j
                        end if
                        ii=ii+2
                     end do
                  end if
                  
               end if 
            end if
            i=Llist(i)
         goto 20
c        end while
      end if

 999  return
c----------------end-of-sympilucupdate----------------------------------
c-----------------------------------------------------------------------
      end



      subroutine MYSSYMILUCUPDATE(n,k,aldl,jldl,lnext,unext,Llist,
     +                            Lfirst,Ufirst,jw,len,w)
c-----------------------------------------------------------------------
      implicit none 
      integer n,k,len
      integer jldl(*),lnext(n),unext(n),Llist(k),Lfirst(k-1),
     +        Ufirst(k-1),jw(2*n)
      FLOAT aldl(*),w(n)
c----------------------------------------------------------------------*
c     update row k of the Schur complement (resp. column k)
c
c     n         size of the problem
c     k         current step of the update procedure
c     aldl      numerical values
c     jldl      associated indices
c     lnext     pointers to the first space behind any column of L
c     unext     pointers to the first space behind any row of U
c     Llist     linked list for the nonzeros of L in row k
c     Lfirst    first nonzero entry in L(k:n,i)
c     Ufirst    first nonzero entry in U(i,k:n)
c     jw        indices of nonzero entries and associated list
c     len       length of this list
c     w         numerical values

      integer i,j,jj,l,ii
      FLOAT x,x2

      i=Llist(k)
c     while i>0
 10      if (i.eq.0) goto 999
c        does there exist an entry in L(k:n,i)?
         jj=Lfirst(i)
         if (jj.lt.lnext(i)) then
c           if this is the case, is the first entry equal to k,
c           otherwise we don't need to do an update because L(k,i)=0
            if (jldl(jj).eq.k) then
c              1x1 case
               if (jldl(n+1+i).eq.0) then
c                 L(k,i)/D(i,i)
                  x=CONJUG(SKEW(aldl(jj)))*aldl(i)

c                 nonzeros in row i of U, start at position at least k
                  jj=Ufirst(i)
c                 innermost loop, update 
                  do l=jj,unext(i)-1
c                    index j of U(i,j)
                     j=jldl(l)
c                    update numerical value
                     w(j)=w(j)-x*aldl(l)
                     if (jw(j).eq.0) then
                        len=len+1
c                       flag component i as nonzero
                        jw(j)=len
c                       add j to the list of nonzeros
                        jw(n+len)=j
                     end if
                  end do

c              2x2 case
               else
c                 L(k,i:i+1)/D(i:i+1,i:i+1)
                  ii=jldl(i)+2*(jj-jldl(i))
                  x =CONJUG(SKEW(aldl(ii)))  *aldl(i)
     +              +CONJUG(SKEW(aldl(ii+1)))*CONJUG(SKEW(aldl(n+1+i)))
                  x2=CONJUG(SKEW(aldl(ii)))  *aldl(n+1+i)
     +              +CONJUG(SKEW(aldl(ii+1)))*aldl(i+1)

c                 nonzeros in row i:i+1 of U, start at position at least k
                  jj=Ufirst(i)
                  ii=jldl(i)+2*(jj-jldl(i))
c                 innermost loop, update 
                  do l=jj,unext(i)-1
c                    index j of U(i,j)
                     j=jldl(l)
c                    update numerical value
                     w(j)=w(j)-x*aldl(ii)-x2*aldl(ii+1)
                     if (jw(j).eq.0) then
                        len=len+1
c                       flag component i as nonzero
                        jw(j)=len
c                       add j to the list of nonzeros
                        jw(n+len)=j
                     end if
                     ii=ii+2
                  end do
               end if
            end if 
         end if
         i=Llist(i)

      goto 10
c     end while
 999  return
c----------------end-of-MYSSYMILUCUPDATE------------------------------
c---------------------------------------------------------------------
      end



      subroutine MYILUCLIST(n,k,startk,jlu,jnext,Ulist,Ufirst)
c-----------------------------------------------------------------------
      implicit none 
      integer n,k,startk
      integer jlu(*),jnext(n),Ulist(n),Ufirst(k)
c-----------------------------------------------------------------------
c     update linked lists of U (resp. L)
c
c     n         size of the problem
c     k         current step of the update procedure
c     startk    start of row k of U
c     jlu       index array
c     jnext     pointers to the first space behind any row of U
c     Ulist     linked list for the nonzeros of U in column k
c     Ufirst    first nonzero entry in U(i,k:n)

      integer i,j,l
      
c     position of the first nonzero entry U(i,k) in column k. Note
c     that U is stored by rows!
      i=Ulist(k)
c     while i>0
 10      if (i.le.0) goto 20

c        does there exist an entry in U(i,k:n)?
         Ufirst(i)=Ufirst(i)+1
         l=Ufirst(i)
c        are there nonzeros leftover?
         if (l.lt.jnext(i)) then
            j=jlu(l)
c           save the old successor of i
            l=Ulist(i)
c           add i to the linked list of column j
            Ulist(i)=Ulist(j)
            Ulist(j)=i
c           restore the old successor
            i=l
         else
            i=Ulist(i)
         end if
      goto 10
c     end while

 20   return
c----------------end-of-MYILUCLIST------------------------------------
c---------------------------------------------------------------------
      end




      subroutine AGGRESSIVEDROPPING(n,k,aldl,jldl,unext,Ufirst,
     +                              Ufirste,clen,clene,condest,
     +                              droptol,flag)
c-----------------------------------------------------------------------
      implicit none 
      integer n,k,jldl(*),unext(n),Ufirst(n),Ufirste(n),clen(n), 
     +        clene(n),flag 
      FLOAT   aldl(*)
      REALS   droptol,condest
c----------------------------------------------------------------------*
c
c     n          size of the problem
c     k          current finished step of the ILU
c     aldl       numerical values
c     jldl       associated indices
c     unext      pointers to the first space behind any row of U 
c                (behind the regular entries)

c     Ufirst     first nonzero entry in U(i,k:n), regular part
c     Ufirste    first nonzero entry in U(i,k:n), epsilon size part
c     clen       number of skipped entries, located at the start of
c                every row of U
c     clene      number of skipped entries, located at the start of
c                every row of U, epsilon size part
c     condest    estimate or bound for ||U^{-1}||
c     droptol    drop tolerance
c     flag       if nonzero, then Tismenetsky Schur complement is used

      integer i,j,shift,m,jj,p,count,q
      REALS   droptolU,d,ax,ay
      FLOAT   U11,U22,U12,U21

      count=0
#ifdef PRINT_INFO
      write (6,'(A)') 'apply aggressive dropping'
      write (6,'(A)') 'old U'
      i=1
 10   if (i.gt.k) goto 20
         if (jldl(n+1+i).eq.0) then
            write (6,'(A,I4)') 'row ',i
            write (6,'(10I8)') (jldl(j), j=jldl(i),jldl(i)+clen(i)-1)
            write (6,'(1P,10E8.1)')
     +            (aldl(j), j=jldl(i),jldl(i)+clen(i)-1)
            write (6,'(10I8)') (jldl(j), j=jldl(i)+clen(i),unext(i)-1)
            write (6,'(1P,10E8.1)') 
     +            (aldl(j), j=jldl(i)+clen(i),unext(i)-1)
            if (flag.eq.0) goto 15
            write (6,'(10I8)') (jldl(j), j=unext(i),unext(i)+clene(i)-1)
            write (6,'(1P,10E8.1)')
     +            (aldl(j), j=unext(i),unext(i)+clene(i)-1)
           write (6,'(10I8)') (jldl(j), j=unext(i)+clene(i),jldl(i+1)-1)
            write (6,'(1P,10E8.1)') 
     +            (aldl(j), j=unext(i)+clene(i),jldl(i+1)-1)
 15         i=i+1
         else
            write (6,'(A,I4,A,I4)') 'row ',i,':',i+1
            write (6,'(10I8)') (jldl(j), j=jldl(i),jldl(i)+clen(i)-1)
            write (6,'(1P,10E8.1)') (aldl(jldl(i)+2*(j-jldl(i))),
     +           j=jldl(i),jldl(i)+clen(i)-1)
            write (6,'(1P,10E8.1)') (aldl(jldl(i)+2*(j-jldl(i))+1),
     +           j=jldl(i),jldl(i)+clen(i)-1)
            write (6,'(10I8)') (jldl(j), j=jldl(i)+clen(i),unext(i)-1)
            write (6,'(1P,10E8.1)') (aldl(jldl(i)+2*(j-jldl(i))), 
     +           j=jldl(i)+clen(i),unext(i)-1)
            write (6,'(1P,10E8.1)') (aldl(jldl(i)+2*(j-jldl(i))+1), 
     +           j=jldl(i)+clen(i),unext(i)-1)
            if (flag.eq.0) goto 16
            write (6,'(10I8)') (jldl(j), j=unext(i),unext(i)+clene(i)-1)
            write (6,'(1P,10E8.1)')
     +   (aldl(unext(i)+2*(j-unext(i))), j=unext(i),unext(i)+clene(i)-1)
            write (6,'(1P,10E8.1)')
     + (aldl(unext(i)+2*(j-unext(i))+1), j=unext(i),unext(i)+clene(i)-1)
           write (6,'(10I8)') (jldl(j), j=unext(i)+clene(i),jldl(i+1)-1)
            write (6,'(1P,10E8.1)') 
     +  (aldl(unext(i)+2*(j-unext(i))), j=unext(i)+clene(i),jldl(i+1)-1)
            write (6,'(1P,10E8.1)') 
     +(aldl(unext(i)+2*(j-unext(i))+1), j=unext(i)+clene(i),jldl(i+1)-1)
 16         i=i+2
         end if      
      goto 10


 20   write (6,'(A)') 'old Ufirst'
      i=1
 30   if (i.gt.k) goto 40
         if (jldl(n+1+i).eq.0) then
            write (6,'(I2AI4)') i,',    ',Ufirst(i)
            i=i+1
         else
            write (6,'(I2AI2AI4)') i,':',i+1,', ',Ufirst(i)
            i=i+2
         end if
         goto 30
 40   continue

      write (6,'(A)') 'old jldl'
      i=1
 60   if (i.gt.k) goto 70
         if (jldl(n+1+i).eq.0) then
            write (6,'(I2AI4)') i,',    ',jldl(i)
            i=i+1
         else
            write (6,'(I2AI2AI4)') i,':',i+1,', ',jldl(i)
            i=i+2
         end if
         goto 60
 70   continue
      write (6,'(I2AI4)') i,',    ',jldl(i)

      if (flag.eq.0) goto 99
      write (6,'(A)') 'old Ufirste'
      i=1
 35   if (i.gt.k) goto 45
         if (jldl(n+1+i).eq.0) then
            write (6,'(I2AI4)') i,',    ',Ufirste(i)
            i=i+1
         else
            write (6,'(I2AI2AI4)') i,':',i+1,', ',Ufirste(i)
            i=i+2
         end if
         goto 35
 45   continue
#endif


 99   shift=0
      i=1
c     while i<=k
 100  if (i.gt.k) goto 200
         p=shift
         if (jldl(n+1+i).eq.0) then
c           1x1 update 

c           shift skipped part of U(i,i+1:n)
            do j=jldl(i),jldl(i)+clen(i)-1
               jldl(j-shift)=jldl(j)
               aldl(j-shift)=aldl(j)
            end do

            droptolU=droptol/condest
            q=unext(i)-jldl(i)-clen(i)
            if (q.gt.0) droptolU=droptolU/q
#ifdef PRINT_INFO
            write (6,'(A,I2,A,1P,E8.1)')'step ',i,', droptolU=',droptolU
#endif
c           aggressive dropping applied to U(i,1:k)
            m=0
            do j=jldl(i)+clen(i),unext(i)-1
               if (ABS(aldl(j)).le.droptolU .and. jldl(j).le.k) then
c                  write (6,'(A,I4,A,I4,A)')'discard (',i,',',jldl(j),')'
                  count=count+1
                  shift=shift+1
                  m=m+1
               else
                  jldl(j-shift)=jldl(j)
                  aldl(j-shift)=aldl(j)
               end if
            end do           
            Ufirst(i)=Ufirst(i)-shift
            jldl(i)=jldl(i)-p

c           epsilon size part
            p=shift
            if (flag.gt.0) then
c              shift skipped part of U(i,i+1:n)
               do j=unext(i),unext(i)+clene(i)-1
                  jldl(j-shift)=jldl(j)
                  aldl(j-shift)=aldl(j)
               end do

c              aggressive dropping applied to U(i,1:k)
               m=0
               droptolU=droptol/condest
               q=jldl(i+1)-unext(i)-clene(i)
               if (q.gt.0) droptolU=droptolU/q
               do j=unext(i)+clene(i),jldl(i+1)-1
                  if (ABS(aldl(j)).le.droptolU .and. jldl(j).le.k)then
c                     write (6,'(A,I4,A,I4,A)')
c     +                     'discard (',i,',',jldl(j),')'
                     count=count+1
                     shift=shift+1
                     m=m+1
                  else
                     jldl(j-shift)=jldl(j)
                     aldl(j-shift)=aldl(j)
                  end if
               end do          
               Ufirste(i)=Ufirste(i)-shift
            end if
            unext(i)=unext(i)-p

            i=i+1
         else
c           2x2 update 

c           shift skipped part of U(i,i+1:n)
            jj=jldl(i)
            do j=jldl(i),jldl(i)+clen(i)-1
               jldl(j   -shift)=jldl(j)
               aldl(jj  -shift)=aldl(jj)
               aldl(jj+1-shift)=aldl(jj+1)
               jj=jj+2
            end do

            U11=aldl(i)
            U22=aldl(i+1)
            U12=aldl(n+1+i)
            U21=SKEW(CONJUG(U12))
            droptolU=droptol/condest
            q=unext(i)-jldl(i)-clen(i)
            if (q.gt.0) droptolU=droptolU/q
#ifdef PRINT_INFO
            write (6,'(A,I2,A,1P,E8.1)')'step ',i,', droptolU=',droptolU
#endif
c           aggressive dropping applied to U(i,1:k)
            m=0
            do j=jldl(i)+clen(i),unext(i)-1
               ax=ABS(U11*aldl(jj)+U12*aldl(jj+1))
               ay=ABS(U21*aldl(jj)+U22*aldl(jj+1))
               if (max(ax,ay).le.droptolU .and. jldl(j).le.k) then
c                  write (6,'(A,I4,A,I4,A,I4,A)')
c     +                  'discard (',i,':',i+1,',',jldl(j),')'
                  count=count+1
                  m=m+1
               else
                  jldl(j   -shift-  m)=jldl(j)
                  aldl(jj  -shift-2*m)=aldl(jj)
                  aldl(jj+1-shift-2*m)=aldl(jj+1)
               end if
               jj=jj+2
            end do           
            Ufirst(i)=Ufirst(i)-shift-m
            jldl(i)=jldl(i)-p

c           epsilon size part
            p=shift+m
            if (flag.gt.0) then
c              shift skipped part of U(i,i+1:n)
               do j=unext(i),unext(i)+clene(i)-1
                  jldl(j   -shift-  m)=jldl(j)
                  aldl(jj  -shift-2*m)=aldl(jj)
                  aldl(jj+1-shift-2*m)=aldl(jj+1)
                  jj=jj+2
               end do

c              aggressive dropping applied to U(i,1:k)
               droptolU=droptol/condest
               q=jldl(i+1)-unext(i)-clene(i)
               if (q.gt.0) droptolU=droptolU/q
               do j=unext(i)+clene(i),jldl(i+1)-1
                  ax=ABS(U11*aldl(jj)+U12*aldl(jj+1))
                  ay=ABS(U21*aldl(jj)+U22*aldl(jj+1))
                  if (max(ax,ay).le.droptolU .and. jldl(j).le.k) then
c                     write (6,'(A,I4,A,I4,A,I4,A)')
c     +                    'discard (',i,':',i+1,',',jldl(j),')'
                     count=count+1
                     m=m+1
                  else
                     jldl(j   -shift-  m)=jldl(j)
                     aldl(jj  -shift-2*m)=aldl(jj)
                     aldl(jj+1-shift-2*m)=aldl(jj+1)
                  end if
                  jj=jj+2
               end do
               Ufirste(i)=Ufirste(i)-shift-m
            end if
            unext(i)=unext(i)-p
            jldl(i+1)=jldl(i+1)-shift-m
            shift=shift+2*m

            i=i+2
         end if
      goto 100
 200  continue
      jldl(i)=jldl(i)-shift
c     end while


#ifdef PRINT_INFO
      write (6,'(A)') 'aggressive dropping applied'
      write (6,'(A)') 'new U'
      i=1
 110  if (i.gt.k) goto 120
         if (jldl(n+1+i).eq.0) then
            write (6,'(A,I4)') 'row ',i
            write (6,'(10I8)') (jldl(j), j=jldl(i),jldl(i)+clen(i)-1)
            write (6,'(1P,10E8.1)')
     +            (aldl(j), j=jldl(i),jldl(i)+clen(i)-1)
            write (6,'(10I8)') (jldl(j), j=jldl(i)+clen(i),unext(i)-1)
            write (6,'(1P,10E8.1)') 
     +            (aldl(j), j=jldl(i)+clen(i),unext(i)-1)
            if (flag.eq.0) goto 115
            write (6,'(10I8)') (jldl(j), j=unext(i),unext(i)+clene(i)-1)
            write (6,'(1P,10E8.1)')
     +            (aldl(j), j=unext(i),unext(i)+clene(i)-1)
           write (6,'(10I8)') (jldl(j), j=unext(i)+clene(i),jldl(i+1)-1)
            write (6,'(1P,10E8.1)') 
     +            (aldl(j), j=unext(i)+clene(i),jldl(i+1)-1)
 115        i=i+1
         else
            write (6,'(A,I4,A,I4)') 'row ',i,':',i+1
            write (6,'(10I8)') (jldl(j), j=jldl(i),jldl(i)+clen(i)-1)
            write (6,'(1P,10E8.1)') (aldl(jldl(i)+2*(j-jldl(i))),
     +           j=jldl(i),jldl(i)+clen(i)-1)
            write (6,'(1P,10E8.1)') (aldl(jldl(i)+2*(j-jldl(i))+1),
     +           j=jldl(i),jldl(i)+clen(i)-1)
            write (6,'(10I8)') (jldl(j), j=jldl(i)+clen(i),unext(i)-1)
            write (6,'(1P,10E8.1)') (aldl(jldl(i)+2*(j-jldl(i))), 
     +           j=jldl(i)+clen(i),unext(i)-1)
            write (6,'(1P,10E8.1)') (aldl(jldl(i)+2*(j-jldl(i))+1), 
     +           j=jldl(i)+clen(i),unext(i)-1)
            if (flag.eq.0) goto 116
            write (6,'(10I8)') (jldl(j), j=unext(i),unext(i)+clene(i)-1)
            write (6,'(1P,10E8.1)')
     +   (aldl(unext(i)+2*(j-unext(i))), j=unext(i),unext(i)+clene(i)-1)
            write (6,'(1P,10E8.1)')
     + (aldl(unext(i)+2*(j-unext(i))+1), j=unext(i),unext(i)+clene(i)-1)
           write (6,'(10I8)') (jldl(j), j=unext(i)+clene(i),jldl(i+1)-1)
            write (6,'(1P,10E8.1)') 
     +  (aldl(unext(i)+2*(j-unext(i))), j=unext(i)+clene(i),jldl(i+1)-1)
            write (6,'(1P,10E8.1)') 
     +(aldl(unext(i)+2*(j-unext(i))+1), j=unext(i)+clene(i),jldl(i+1)-1)
 116        i=i+2
         end if      
         goto 110
 120  continue


      write (6,'(A)') 'new Ufirst'
      i=1
 130  if (i.gt.k) goto 140
         if (jldl(n+1+i).eq.0) then
            write (6,'(I2AI4)') i,',    ',Ufirst(i)
            i=i+1
         else
            write (6,'(I2AI2AI4)') i,':',i+1,', ',Ufirst(i)
            i=i+2
         end if
         goto 130
 140  continue

      write (6,'(A)') 'new jldl'
      i=1
 160  if (i.gt.k) goto 170
         if (jldl(n+1+i).eq.0) then
            write (6,'(I2AI4)') i,',    ',jldl(i)
            i=i+1
         else
            write (6,'(I2AI2AI4)') i,':',i+1,', ',jldl(i)
            i=i+2
         end if
         goto 160
 170  continue
      write (6,'(I2AI4)') i,',    ',jldl(i)

      if (flag.eq.0) goto 900
      write (6,'(A)') 'new Ufirste'
      i=1
 135  if (i.gt.k) goto 145
         if (jldl(n+1+i).eq.0) then
            write (6,'(I2AI4)') i,',    ',Ufirste(i)
            i=i+1
         else
            write (6,'(I2AI2AI4)') i,':',i+1,', ',Ufirste(i)
            i=i+2
         end if
         goto 135
 145  continue
#endif
      
 900  write (6,'(I10,A)') count,' entries discarded'
 901  return
c----------------end-of-AGGRESSIVEDROPPING----------------------------
c---------------------------------------------------------------------
      end









      subroutine FAGGRESSIVEDROPPING(n,k,aldl,jldl,unext,Ufirst,
     +                              Ufirste,clen,clene,condest,
     +                              droptol,flag)
c-----------------------------------------------------------------------
      implicit none 
      integer n,k,jldl(*),unext(n),Ufirst(n),Ufirste(n),clen(n), 
     +        clene(n),flag 
      FLOAT   aldl(*),condest(n)
      REALS   droptol
c----------------------------------------------------------------------*
c
c     n          size of the problem
c     k          current finished step of the ILU
c     aldl       numerical values
c     jldl       associated indices
c     unext      pointers to the first space behind any row of U 
c                (behind the regular entries)

c     Ufirst     first nonzero entry in U(i,k:n), regular part
c     Ufirste    first nonzero entry in U(i,k:n), epsilon size part
c     clen       number of skipped entries, located at the start of
c                every row of U
c     clene      number of skipped entries, located at the start of
c                every row of U, epsilon size part
c     condest    estimates or bound for ||U^{-1}(:,i)||,i=1,...,k
c     droptol    drop tolerance
c     flag       if nonzero, then Tismenetsky Schur complement is used

      integer i,j,shift,m,jj,p,count,q
      REALS   droptolU,d,ax,ay
      FLOAT   U11,U22,U12,U21

      count=0
#ifdef PRINT_INFO
      write (6,'(A)') 'apply aggressive dropping'
      write (6,'(A)') 'old U'
      i=1
 10   if (i.gt.k) goto 20
         if (jldl(n+1+i).eq.0) then
            write (6,'(A,I4)') 'row ',i
            write (6,'(10I8)') (jldl(j), j=jldl(i),jldl(i)+clen(i)-1)
            write (6,'(1P,10E8.1)')
     +            (aldl(j), j=jldl(i),jldl(i)+clen(i)-1)
            write (6,'(10I8)') (jldl(j), j=jldl(i)+clen(i),unext(i)-1)
            write (6,'(1P,10E8.1)') 
     +            (aldl(j), j=jldl(i)+clen(i),unext(i)-1)
            if (flag.eq.0) goto 15
            write (6,'(10I8)') (jldl(j), j=unext(i),unext(i)+clene(i)-1)
            write (6,'(1P,10E8.1)')
     +            (aldl(j), j=unext(i),unext(i)+clene(i)-1)
           write (6,'(10I8)') (jldl(j), j=unext(i)+clene(i),jldl(i+1)-1)
            write (6,'(1P,10E8.1)') 
     +            (aldl(j), j=unext(i)+clene(i),jldl(i+1)-1)
 15         i=i+1
         else
            write (6,'(A,I4,A,I4)') 'row ',i,':',i+1
            write (6,'(10I8)') (jldl(j), j=jldl(i),jldl(i)+clen(i)-1)
            write (6,'(1P,10E8.1)') (aldl(jldl(i)+2*(j-jldl(i))),
     +           j=jldl(i),jldl(i)+clen(i)-1)
            write (6,'(1P,10E8.1)') (aldl(jldl(i)+2*(j-jldl(i))+1),
     +           j=jldl(i),jldl(i)+clen(i)-1)
            write (6,'(10I8)') (jldl(j), j=jldl(i)+clen(i),unext(i)-1)
            write (6,'(1P,10E8.1)') (aldl(jldl(i)+2*(j-jldl(i))), 
     +           j=jldl(i)+clen(i),unext(i)-1)
            write (6,'(1P,10E8.1)') (aldl(jldl(i)+2*(j-jldl(i))+1), 
     +           j=jldl(i)+clen(i),unext(i)-1)
            if (flag.eq.0) goto 16
            write (6,'(10I8)') (jldl(j), j=unext(i),unext(i)+clene(i)-1)
            write (6,'(1P,10E8.1)')
     +   (aldl(unext(i)+2*(j-unext(i))), j=unext(i),unext(i)+clene(i)-1)
            write (6,'(1P,10E8.1)')
     + (aldl(unext(i)+2*(j-unext(i))+1), j=unext(i),unext(i)+clene(i)-1)
           write (6,'(10I8)') (jldl(j), j=unext(i)+clene(i),jldl(i+1)-1)
            write (6,'(1P,10E8.1)') 
     +  (aldl(unext(i)+2*(j-unext(i))), j=unext(i)+clene(i),jldl(i+1)-1)
            write (6,'(1P,10E8.1)') 
     +(aldl(unext(i)+2*(j-unext(i))+1), j=unext(i)+clene(i),jldl(i+1)-1)
 16         i=i+2
         end if      
      goto 10


 20   write (6,'(A)') 'old Ufirst'
      i=1
 30   if (i.gt.k) goto 40
         if (jldl(n+1+i).eq.0) then
            write (6,'(I2AI4)') i,',    ',Ufirst(i)
            i=i+1
         else
            write (6,'(I2AI2AI4)') i,':',i+1,', ',Ufirst(i)
            i=i+2
         end if
         goto 30
 40   continue

      write (6,'(A)') 'old jldl'
      i=1
 60   if (i.gt.k) goto 70
         if (jldl(n+1+i).eq.0) then
            write (6,'(I2AI4)') i,',    ',jldl(i)
            i=i+1
         else
            write (6,'(I2AI2AI4)') i,':',i+1,', ',jldl(i)
            i=i+2
         end if
         goto 60
 70   continue
      write (6,'(I2AI4)') i,',    ',jldl(i)

      if (flag.eq.0) goto 99
      write (6,'(A)') 'old Ufirste'
      i=1
 35   if (i.gt.k) goto 45
         if (jldl(n+1+i).eq.0) then
            write (6,'(I2AI4)') i,',    ',Ufirste(i)
            i=i+1
         else
            write (6,'(I2AI2AI4)') i,':',i+1,', ',Ufirste(i)
            i=i+2
         end if
         goto 35
 45   continue
#endif


 99   shift=0
      i=1
c     while i<=k
 100  if (i.gt.k) goto 200
         p=shift
         if (jldl(n+1+i).eq.0) then
c           1x1 update 

c           shift skipped part of U(i,i+1:n)
            do j=jldl(i),jldl(i)+clen(i)-1
               jldl(j-shift)=jldl(j)
               aldl(j-shift)=aldl(j)
            end do

            droptolU=droptol
            q=unext(i)-jldl(i)-clen(i)
            if (q.gt.0) droptolU=droptolU/q
#ifdef PRINT_INFO
            write (6,'(A,I2,A,1P,E8.1)')'step ',i,', droptolU=',droptolU
#endif
c           aggressive dropping applied to U(i,1:k)
            m=0
            do j=jldl(i)+clen(i),unext(i)-1
c              column index q of U(i,q)
               q=jldl(j)
               if (ABS(aldl(j))*CTOD(condest(q)).le.droptolU
     +             .and. jldl(j).le.k) then
c                  write (6,'(A,I4,A,I4,A)')'discard (',i,',',jldl(j),')'
                  count=count+1
                  shift=shift+1
                  m=m+1
               else
                  jldl(j-shift)=q
                  aldl(j-shift)=aldl(j)
               end if
            end do           
            Ufirst(i)=Ufirst(i)-shift
            jldl(i)=jldl(i)-p

c           epsilon size part
            p=shift
            if (flag.gt.0) then
c              shift skipped part of U(i,i+1:n)
               do j=unext(i),unext(i)+clene(i)-1
                  jldl(j-shift)=jldl(j)
                  aldl(j-shift)=aldl(j)
               end do

c              aggressive dropping applied to U(i,1:k)
               m=0
               droptolU=droptol
               q=jldl(i+1)-unext(i)-clene(i)
               if (q.gt.0) droptolU=droptolU/q
               do j=unext(i)+clene(i),jldl(i+1)-1
c                 column index q of U(i,q)
                  q=jldl(j)
                  if (ABS(aldl(j))*CTOD(condest(q)).le.droptolU
     +                .and. jldl(j).le.k)then
c                     write (6,'(A,I4,A,I4,A)')
c     +                     'discard (',i,',',jldl(j),')'
                     count=count+1
                     shift=shift+1
                     m=m+1
                  else
                     jldl(j-shift)=q
                     aldl(j-shift)=aldl(j)
                  end if
               end do          
               Ufirste(i)=Ufirste(i)-shift
            end if
            unext(i)=unext(i)-p

            i=i+1
         else
c           2x2 update 

c           shift skipped part of U(i,i+1:n)
            jj=jldl(i)
            do j=jldl(i),jldl(i)+clen(i)-1
               jldl(j   -shift)=jldl(j)
               aldl(jj  -shift)=aldl(jj)
               aldl(jj+1-shift)=aldl(jj+1)
               jj=jj+2
            end do

            U11=aldl(i)
            U22=aldl(i+1)
            U12=aldl(n+1+i)
            U21=SKEW(CONJUG(U12))
            droptolU=droptol
            q=unext(i)-jldl(i)-clen(i)
            if (q.gt.0) droptolU=droptolU/q
#ifdef PRINT_INFO
            write (6,'(A,I2,A,1P,E8.1)')'step ',i,', droptolU=',droptolU
#endif
c           aggressive dropping applied to U(i,1:k)
            m=0
            do j=jldl(i)+clen(i),unext(i)-1
c              column index q of U(i,q)
               q=jldl(j)
               ax=ABS(U11*aldl(jj)+U12*aldl(jj+1))
               ay=ABS(U21*aldl(jj)+U22*aldl(jj+1))
               if (max(ax,ay)*CTOD(condest(q)).le.droptolU
     +             .and. jldl(j).le.k) then
c                  write (6,'(A,I4,A,I4,A,I4,A)')
c     +                  'discard (',i,':',i+1,',',jldl(j),')'
                  count=count+1
                  m=m+1
               else
                  jldl(j   -shift-  m)=q
                  aldl(jj  -shift-2*m)=aldl(jj)
                  aldl(jj+1-shift-2*m)=aldl(jj+1)
               end if
               jj=jj+2
            end do           
            Ufirst(i)=Ufirst(i)-shift-m
            jldl(i)=jldl(i)-p

c           epsilon size part
            p=shift+m
            if (flag.gt.0) then
c              shift skipped part of U(i,i+1:n)
               do j=unext(i),unext(i)+clene(i)-1
                  jldl(j   -shift-  m)=jldl(j)
                  aldl(jj  -shift-2*m)=aldl(jj)
                  aldl(jj+1-shift-2*m)=aldl(jj+1)
                  jj=jj+2
               end do

c              aggressive dropping applied to U(i,1:k)
               droptolU=droptol
               q=jldl(i+1)-unext(i)-clene(i)
               if (q.gt.0) droptolU=droptolU/q
               do j=unext(i)+clene(i),jldl(i+1)-1
c                 column index q of U(i,q)
                  q=jldl(j)
                  ax=ABS(U11*aldl(jj)+U12*aldl(jj+1))
                  ay=ABS(U21*aldl(jj)+U22*aldl(jj+1))
                  if (max(ax,ay)*CTOD(condest(q)).le.droptolU
     +                .and. jldl(j).le.k) then
c                     write (6,'(A,I4,A,I4,A,I4,A)')
c     +                    'discard (',i,':',i+1,',',jldl(j),')'
                     count=count+1
                     m=m+1
                  else
                     jldl(j   -shift-  m)=q
                     aldl(jj  -shift-2*m)=aldl(jj)
                     aldl(jj+1-shift-2*m)=aldl(jj+1)
                  end if
                  jj=jj+2
               end do
               Ufirste(i)=Ufirste(i)-shift-m
            end if
            unext(i)=unext(i)-p
            jldl(i+1)=jldl(i+1)-shift-m
            shift=shift+2*m

            i=i+2
         end if
      goto 100
 200  continue
      jldl(i)=jldl(i)-shift
c     end while


#ifdef PRINT_INFO
      write (6,'(A)') 'aggressive dropping applied'
      write (6,'(A)') 'new U'
      i=1
 110  if (i.gt.k) goto 120
         if (jldl(n+1+i).eq.0) then
            write (6,'(A,I4)') 'row ',i
            write (6,'(10I8)') (jldl(j), j=jldl(i),jldl(i)+clen(i)-1)
            write (6,'(1P,10E8.1)')
     +            (aldl(j), j=jldl(i),jldl(i)+clen(i)-1)
            write (6,'(10I8)') (jldl(j), j=jldl(i)+clen(i),unext(i)-1)
            write (6,'(1P,10E8.1)') 
     +            (aldl(j), j=jldl(i)+clen(i),unext(i)-1)
            if (flag.eq.0) goto 115
            write (6,'(10I8)') (jldl(j), j=unext(i),unext(i)+clene(i)-1)
            write (6,'(1P,10E8.1)')
     +            (aldl(j), j=unext(i),unext(i)+clene(i)-1)
           write (6,'(10I8)') (jldl(j), j=unext(i)+clene(i),jldl(i+1)-1)
            write (6,'(1P,10E8.1)') 
     +            (aldl(j), j=unext(i)+clene(i),jldl(i+1)-1)
 115        i=i+1
         else
            write (6,'(A,I4,A,I4)') 'row ',i,':',i+1
            write (6,'(10I8)') (jldl(j), j=jldl(i),jldl(i)+clen(i)-1)
            write (6,'(1P,10E8.1)') (aldl(jldl(i)+2*(j-jldl(i))),
     +           j=jldl(i),jldl(i)+clen(i)-1)
            write (6,'(1P,10E8.1)') (aldl(jldl(i)+2*(j-jldl(i))+1),
     +           j=jldl(i),jldl(i)+clen(i)-1)
            write (6,'(10I8)') (jldl(j), j=jldl(i)+clen(i),unext(i)-1)
            write (6,'(1P,10E8.1)') (aldl(jldl(i)+2*(j-jldl(i))), 
     +           j=jldl(i)+clen(i),unext(i)-1)
            write (6,'(1P,10E8.1)') (aldl(jldl(i)+2*(j-jldl(i))+1), 
     +           j=jldl(i)+clen(i),unext(i)-1)
            if (flag.eq.0) goto 116
            write (6,'(10I8)') (jldl(j), j=unext(i),unext(i)+clene(i)-1)
            write (6,'(1P,10E8.1)')
     +   (aldl(unext(i)+2*(j-unext(i))), j=unext(i),unext(i)+clene(i)-1)
            write (6,'(1P,10E8.1)')
     + (aldl(unext(i)+2*(j-unext(i))+1), j=unext(i),unext(i)+clene(i)-1)
           write (6,'(10I8)') (jldl(j), j=unext(i)+clene(i),jldl(i+1)-1)
            write (6,'(1P,10E8.1)') 
     +  (aldl(unext(i)+2*(j-unext(i))), j=unext(i)+clene(i),jldl(i+1)-1)
            write (6,'(1P,10E8.1)') 
     +(aldl(unext(i)+2*(j-unext(i))+1), j=unext(i)+clene(i),jldl(i+1)-1)
 116        i=i+2
         end if      
         goto 110
 120  continue


      write (6,'(A)') 'new Ufirst'
      i=1
 130  if (i.gt.k) goto 140
         if (jldl(n+1+i).eq.0) then
            write (6,'(I2AI4)') i,',    ',Ufirst(i)
            i=i+1
         else
            write (6,'(I2AI2AI4)') i,':',i+1,', ',Ufirst(i)
            i=i+2
         end if
         goto 130
 140  continue

      write (6,'(A)') 'new jldl'
      i=1
 160  if (i.gt.k) goto 170
         if (jldl(n+1+i).eq.0) then
            write (6,'(I2AI4)') i,',    ',jldl(i)
            i=i+1
         else
            write (6,'(I2AI2AI4)') i,':',i+1,', ',jldl(i)
            i=i+2
         end if
         goto 160
 170  continue
      write (6,'(I2AI4)') i,',    ',jldl(i)

      if (flag.eq.0) goto 900
      write (6,'(A)') 'new Ufirste'
      i=1
 135  if (i.gt.k) goto 145
         if (jldl(n+1+i).eq.0) then
            write (6,'(I2AI4)') i,',    ',Ufirste(i)
            i=i+1
         else
            write (6,'(I2AI2AI4)') i,':',i+1,', ',Ufirste(i)
            i=i+2
         end if
         goto 135
 145  continue
#endif
      
 900  write (6,'(A,I10,A)') 'additional ',count,' entries discarded'
 901  return
c----------------end-of-FAGGRESSIVEDROPPING---------------------------
c---------------------------------------------------------------------
      end
