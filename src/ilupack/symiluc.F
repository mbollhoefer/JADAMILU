#include <ilupack_fortran.h>
#include <ilupackmacros.h>

c#define PRINT_INFO3


c     real case
#if defined _DOUBLE_REAL_ || defined _SINGLE_REAL_
#define CONJUG(A)      (A)

c     real and skew symmetric
#ifdef _SKEW_MATRIX_

#ifdef _DOUBLE_REAL_
#define MYSYMILUC           DSSMiluc
#define MYSYMPILUCUPDATE    DSSMpilucupdate
#define MYILUCLIST          DSSMiluclist2
#define MYSSYMILUCUPDATE    DSSMssymilucupdate
#define AGGRESSIVEDROPPING  DSSMaggressivedropping
#define FAGGRESSIVEDROPPING DSSMfaggressivedropping
#else
#define MYSYMILUC           SSSMiluc
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
#define MYSYMILUC           DSYMiluc
#define MYSYMPILUCUPDATE    DSYMpilucupdate
#define MYILUCLIST          DSYMiluclist2
#define MYSSYMILUCUPDATE    DSYMssymilucupdate
#define AGGRESSIVEDROPPING  DSYMaggressivedropping
#define FAGGRESSIVEDROPPING DSYMfaggressivedropping
#else
#define MYSYMILUC           SSYMiluc
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
#define MYSYMILUC           CSSMiluc
#define MYSYMPILUCUPDATE    CSSMpilucupdate
#define MYILUCLIST          CSSMiluclist2
#define MYSSYMILUCUPDATE    CSSMssymilucupdate
#define AGGRESSIVEDROPPING  CSSMaggressivedropping
#define FAGGRESSIVEDROPPING CSSMfaggressivedropping
#else
#define MYSYMILUC           ZSSMiluc
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
#define MYSYMILUC           CSYMiluc
#define MYSYMPILUCUPDATE    CSYMpilucupdate
#define MYILUCLIST          CSYMiluclist2
#define MYSSYMILUCUPDATE    CSYMssymilucupdate
#define AGGRESSIVEDROPPING  CSYMaggressivedropping
#define FAGGRESSIVEDROPPING CSYMfaggressivedropping
#else
#define MYSYMILUC           ZSYMiluc
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
#define MYSYMILUC           CSHRiluc
#define MYSYMPILUCUPDATE    CSHRpilucupdate
#define MYILUCLIST          CSHRiluclist2
#define MYSSYMILUCUPDATE    CSHRssymilucupdate
#define AGGRESSIVEDROPPING  CSHRaggressivedropping
#define FAGGRESSIVEDROPPING CSHRfaggressivedropping
#else
#define MYSYMILUC           ZSHRiluc
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
#define MYSYMILUC           CHERiluc
#define MYSYMPILUCUPDATE    CHERpilucupdate
#define MYILUCLIST          CHERiluclist2
#define MYSSYMILUCUPDATE    CHERssymilucupdate
#define AGGRESSIVEDROPPING  CHERaggressivedropping
#define FAGGRESSIVEDROPPING CHERfaggressivedropping
#else
#define MYSYMILUC           ZHERiluc
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




      subroutine MYSYMILUC(n,a,ja,ia,lfil,droptol,
     +     param,p,invq,aldl,jldl,iwk,w,jw,ierr)
c-----------------------------------------------------------------------
      implicit none 
      integer n,iwk,p(n),invq(n),
     +        ja(*),ia(n+1),jldl(iwk),jw(*),lfil,ierr,param
      REALS droptol
      FLOAT a(*),aldl(iwk),w(*)
c----------------------------------------------------------------------*
c       *** SYMILUC, SYMMETRIC ILUC preconditioner ***        *
c                                                                      *
c symmetric indefinite version of ILUC                                 *
c     SYMILUC factors a (skew-) symmetric (Hermitian) matrix (may be  *
c     symmetrically permuted) partially                                *
c         .          / L  0\ /D 0\ /U F_U\                             *
c     i.e. P^T A P ~ |     | |   | |     |                             *
c                    \E_L I/ \0 S/ \0  I /                             *
c     Certainly only the upper triangular part is stored.              *
c     The ILU uses shifts to avoid small pivots                        *
c                                                                      *
c SYMILUC goes far beyond the simple single-level ILDLC by             *
c     introducing additional features                                  *
c     - symmetric permutations (remember that only the upper triangular*
c                               part is stored!!!)                     *
c                                                                      *
c Further options.                                                     *
c     - diagonal compensation is offered                               *
c     - symmetry is preserved                                          *
c----------------------------------------------------------------------*
c Code written by Matthias Bollhoefer, April, 2005                     *
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
c lfil      integer. The fill-in parameters. Each row of U will have
c           a maximum of lfil elements (excluding the diagonal 
c           element). 
c           lfil must be .ge. 0.
c
c droptol   droptol sets the threshold for dropping small terms in the
c           factorization. See below for details on dropping strategy.
c
c  
c iwk       integer. The lengths of arrays aldl and jldl. If the arrays
c           are not big enough to store the ILU factorizations, SYMILUC
c           will stop with an error message. 
c
c param     integer. Options of SYMILUC (to be read bitwise)
c           bit 0
c            0   simple dual threshold dropping strategy   
c            1   inverse based dropping strategy
c           bit 4
c            0   simple inverse-based dropping strategy is used
c           16   inverse based dropping strategy is improved by a more 
c                precise estimate of the inverse
c           bit 5
c            0   no diagonal compensation
c           32   diagonal compensation is used, i.e., we ensure that the
c                ILU satisfies LDL'=A+E, where E*1=0 (1 denotes the vector
c                with all entries equal to 1)
c           bit 6
c            0   skipped entries are kept in the partial ILU
c           64   skipped entries are removed from the partial ILU
c           bit 7
c           bit 8
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
     +        milu,nb,nd,tailor,loadrow,nw,kk,mode,finalrow,
     +        rlen(2),njw1(2),njw2(2),jbuff,
     +        aggressive,zeropivots,maxiwk
      REALS  ax,ay,axp,axm,mup,mum,mup2,mum2,buffer,absaldlk,
     +        droptolU,droptol2,nup,num,maxcondest,
     +        Lmax,Umax,DLmax,DUmax,sigma,nup2,num2,axp2,
     +        axm2,buffer2,condest1,condest2,condest
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


      if (lfil.lt.0) goto 994

c     count how often we encountered a zero pivot
      zeropivots=0

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

c     bit 15: DISCARD_MATRIX unused
      i=i/2


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
      if (iwk.lt.n2+1) goto 992
      maxiwk=n2+1
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
      maxiwk=0
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


      
c     main loop
      k=1
c     initially we need to load 2 subsequent rows
      loadrow=1
      finalrow=2
      if (n.le.1) finalrow=1
      rlen(1)=0
      rlen(2)=0
         
c     while k<=n
 110     if (k.gt.n) goto 111
       
#if defined PRINT_INFO3 || defined PRINT_INFO2
         write (6, '(A,I6)') 'step ',k
#endif



c        -----   dealing with the upper triangular part   -----
c
c        compute rows k,k+1 whenever necessary
         do kk=loadrow,finalrow
#if defined PRINT_INFO || defined PRINT_INFO2
            write (6, '(A,I4)') 'kk: ',kk
#endif
            mode=kk-1
            rlen(kk)=0

c           extract strict lower triangular part of A, column k
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
c                 if (l=k+mode and i>=k and i<>k+mode) 
                  if (l.eq.k+mode .and. (i.ge.k .and. 
     +                i.ne.k+mode)) then 
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
c              extract strict lower triangular part of A, column k+1
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
c                    if (l=k+1 and i>=k and i<>k+1) 
                     if (l.eq.k+1 .and. (i.ge.k .and. i.ne.k+1))
     +                  then 
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
c           extract strict upper triangular part of A, row k+mode
            do l=ia(p(k+mode)),ia(p(k+mode)+1)-1
c              column index i of A(p(k+mode),q(i))
               i=invq(ja(l))
c              extract upper triangular part of A(p,q), row k+mode
               if (i.ge.k) then
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
         if (k.lt.n) then
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
         if (k.lt.n) then
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
         if (k.lt.n) then
            write (6, '(A,1P,E8.1,A,1P,E8.1)')
     +    'diagonal entry ',dreal(aldl(k+1)),'+i*',dimag(aldl(k+1))
         end if
#endif
#ifdef PRINT_INFO
         write (6, '(A,1P,E8.1)') 'diagonal entry ',
     +        aldl(k)
         if (k.lt.n) then
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
            l=0
         end if
c        zero diagonal entry or singular block
c        artificially shift diagonal entry
         if (DUmax.eq.RZERO .and. Umax.eq.RZERO) then
            mup=RZERO
            do j=1,rlen(1)
               l=jw(njw2(1)+j)
               ax=ABS(w(l))
c              maximum entry in absolute value
               mup=max(mup,ax)
            end do
            if (mup.eq.0.0) mup=1.0
            aldl(k)=(1.0e-4+droptol)*mup
            Umax=ABS(aldl(k))
         end if
c        check diagonal dominance

c        1x1 case
         if (Umax.ne.RZERO) then
            nup=RZERO
            mup=RZERO
            axp=condest*Umax*0.25d0
            do j=1,rlen(1)
               l=jw(njw2(1)+j)
               ax=ABS(w(l))
c              maximum entry in absolute value
               mup=max(mup,ax)
c              accumulate significant nonzero entries
               if (ax.gt.axp) nup=nup+ax
            end do
            mup=mup/Umax
         end if

c        2x2 case
c        2x2 block is not diagonal
         if (l.ne.0 .and. DUmax.ne.RZERO) then
            num=RZERO
            mum=RZERO
            axp=condest*DUmax*0.25
            do j=1,rlen(1)
               i=jw(njw2(1)+j)
               if (i.ne.k+1) then
c                 maximum entry in absolute value
c                 ||[A(k,k)   A(k,k+1)  ]^{-1}*U(k:k+1,i)||
c                 ||[A(k+1,k) A(k+1,k+1)]                ||
                  if (jw(njw1(2)+i).ne.0) then
                     ax=ABS( aldl(k+1)*w(i)-xp       *w(nw+i))
     +                 +ABS(-xm       *w(i)+aldl(k)  *w(nw+i))
                  else
                     ax=ABS(aldl(k+1) *w(i))
     &                 +ABS(-xm       *w(i))
                  end if
                  mum=max(mum,ax)
c                 accumulate significant nonzero entries
                  if (ax.gt.axp) num=num+ax
               end if
            end do
            do j=1,rlen(2)
               i=jw(njw2(2)+j)
               if (i.ne.k) then
c                 maximum entry in absolute value
c                 ||[A(k,k)   A(k,k+1)  ]^{-1}*U(k:k+1,i)||
c                 ||[A(k+1,k) A(k+1,k+1)]                ||
                  if (jw(njw1(1)+i).ne.0) then
                     ax=ABS( aldl(k+1)*w(i)-xp       *w(nw+i))
     +                 +ABS(-xm       *w(i)+aldl(k)  *w(nw+i))
                  else
                     ax=ABS(-xp       *w(nw+i))
     +                 +ABS( aldl(k)  *w(nw+i))
                  end if
                  mum=max(mum,ax)
c                 accumulate significant nonzero entries
                  if (ax.gt.axp) num=num+ax
               end if
            end do
            mum=mum/DUmax
         else
            mum=2.0*(mup+1.0)
         end if

c        when A(k,k)=0 we formally have to set mup to 
c        something significantly larger than  mum
         if (Umax.eq.RZERO) mup=10.0*(mum+1.0)



c        use a 2x2 update only if it is more diagonal dominant
c        than the 1x1 update would be

#if defined PRINT_INFO || defined PRINT_INFO2
         if (k.lt.n) then
            write (6, '(A,1P,E8.1,1P,E8.1)') 'diag. dom.',mup,mum
         else
            write (6, '(A,1P,E8.1)') 'diag. dom.',mup
         end if
#endif

c        compare diagonal dominance of the L-factor
c        prefer a 1x1 update
         ax=max(ABS(aldl(k)),ABS(aldl(k+1)))
         ax=max(ax,ABS(xp))
         if (mup.le.mum) then
            mode=0
            Umax=mup
            mup=nup

#if defined PRINT_INFO3 || defined PRINT_INFO2
            write (6, '(A)') '1x1 update'
#endif

c        (unfortunately) a 2x2 update is recommended
         else
            mode=1
            Umax=mum
            mup=num

#if defined PRINT_INFO3 || defined PRINT_INFO2
            write (6, '(A)') '2x2 update'
#endif
            
         end if 

         aldlk=aldl(k)

c        invert diagonal entry/determinant
         if (mode.eq.0) then
            ialdlk=FONE/aldlk
         else 
c           ialdlk is the determinant
            ialdlk=FONE/ialdlk
c           [A(k,k)   A(k,k+1)  ]^{-1}
c           [A(k+1,k) A(k+1,k+1)]
            U11= aldl(k+1)         *ialdlk
            U12=-w(nw+k)           
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
         if (mode.eq.0) then
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
c              droptol
c              sctype         indicate whether Tismenetsky update is used
               call AGGRESSIVEDROPPING(n,k-1,aldl,jldl,jw(nb+1),
     +                                 jw(na+1),jw(n8+1),jw(nl+1),
     +                                 jw(el+1),condest,droptol,
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
         else 
            if (jldl(k)+2*rlen(1)+2*rlen(2)-1.gt.iwk .and.
     +           aggressive.gt.0) then 
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
c              droptol
c              sctype         indicate whether Tismenetsky update is used
               call AGGRESSIVEDROPPING(n,k-1,aldl,jldl,jw(nb+1),
     +                                 jw(na+1),jw(n8+1),jw(nl+1),
     +                                 jw(el+1),condest,droptol,
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
         signum =FONE
         signum2=FONE
#if !defined _DOUBLE_REAL_ && !defined _SINGLE_REAL_
c        1x1 update 
         if (mode.eq.0) then
            mup=RZERO
            j=jldl(k)
            do i=jldl(k),jldl(k+1)-1
c              column index l
               l=jldl(i)
               xp=aldl(i)*ialdlk
               xm=w(n+l)-w(n+k)*xp
               axp=ABS(xm)+ABS(xp)
               if (axp.gt.mup) then 
                  mup=axp
                  j=i
               end if
            end do
c           choose the sign such that component l is maximized
            if (jldl(k).lt.jldl(k+1)) then
               l=jldl(j)
               xp=aldl(j)*ialdlk
               xm=w(n+l)-w(n+k)*xp
               if (xp.ne.FNULL .and. 
     +              xm.ne.FNULL) then
                  signum=(xm/ABS(xm))*(CONJ(xp)/ABS(xp))
               end if
            end if
         else
c           2x2 update 


c           signs, column k:k+1
            mup=RZERO
            mum=RZERO
            j=jldl(k)
            jj=j
            ii=j
            do i=jldl(k),jldl(k+1)-1
c              column index l
               l=jldl(i)
c              first row of 
c              [A(k,k)   A(k,k+1)  ]^{-1}*U(k:k+1,l)
c              [A(k+1,k) A(k+1,k+1)]
               xp=U11*aldl(ii)+U12*aldl(ii+1)
               xm=w(n+l)-w(n+k)*xp
               axp=ABS(xm)+ABS(xp)
               if (axp.gt.mup) then 
                  mup=axp
                  j=i
               end if
c              second row of 
c              [A(k,k)   A(k,k+1)  ]^{-1}*U(k:k+1,l)
c              [A(k+1,k) A(k+1,k+1)]
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
c              first row of 
c              [A(k,k)   A(k,k+1)  ]^{-1}*U(k:k+1,l)
c              [A(k+1,k) A(k+1,k+1)]
               xp=U11*aldl(ii)+U12*aldl(ii+1)
               xm=w(n+l)-w(n+k)*xp
               if (xp.ne.FNULL .and.
     +              xm.ne.FNULL) then
                  signum=(xm/ABS(xm))*(CONJ(xp)/ABS(xp))
               end if
               l=jldl(jj)
               ii=jldl(k)+2*(jj-jldl(k))
c              second row of 
c              [A(k,k)   A(k,k+1)  ]^{-1}*U(k:k+1,l)
c              [A(k+1,k) A(k+1,k+1)]
               xp=U21*aldl(ii)+U22*aldl(ii+1)
               xm=w(n+l)-w(n+k+1)*xp
               if (xp.ne.FNULL .and.
     +              xm.ne.FNULL) then
                  signum2=(xm/ABS(xm))*(CONJ(xp)/ABS(xp))
               end if
            end if
         end if
#else
#endif

c        1x1 update
         if (mode.eq.0) then
c           try +/-SIGNUM as k-th component of the right hand side
            xp=( SIGNUM-w(n+k))*ialdlk
            xm=(-SIGNUM-w(n+k))*ialdlk
c           for both cases estimate the 1-norm of U^{-1} by evaluating
c           U(1:k,k+1:n)^T*U(1:k,1:k)^{-T}*rhs(1:k)
            mup=RZERO 
            mum=RZERO
            nup=RZERO
            num=RZERO
            do i=jldl(k),jldl(k+1)-1
c              row index l
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
c           use a buffer to store x
c           if the improved estimate turns out to skip entry k although
c           the first estimate has not indicated this, then we have to
c           'undo' the update
            buffer=-x
c           does the norm estimate exceed the prescribed bound? 

         else
c           2x2 update
c           update using columns k,k+1
c           try +/-SIGNUM, and +/-SIGNUM2 as k-th and (k+1)-th component
c           of the right hand side

c           [ SIGNUM-w(n+k),   SIGNUM2-w(n+k+1)] * [A(k,k)   A(k,k+1)  ]^{-1}
c                                                  [A(k+1,k) A(k+1,k+1)]     
            xp =( SIGNUM-w(n+k))*U11+( SIGNUM2-w(n+k+1))*U21
            xp2=( SIGNUM-w(n+k))*U12+( SIGNUM2-w(n+k+1))*U22

c           [ SIGNUM -w(n+k), -SIGNUM2-w(n+k+1)] * [A(k,k)   A(k,k+1)  ]^{-1}
c                                                     [A(k+1,k) A(k+1,k+1)]     
            xp3=( SIGNUM-w(n+k))*U11+(-SIGNUM2-w(n+k+1))*U21
            xp4=( SIGNUM-w(n+k))*U12+(-SIGNUM2-w(n+k+1))*U22

c           [-SIGNUM -w(n+k), -SIGNUM2-w(n+k+1)] * [A(k,k)   A(k,k+1)  ]^{-1}
c                                                     [A(k+1,k) A(k+1,k+1)]     
            xm =(-SIGNUM-w(n+k))*U11+(-SIGNUM2-w(n+k+1))*U21
            xm2=(-SIGNUM-w(n+k))*U12+(-SIGNUM2-w(n+k+1))*U22

c           [-SIGNUM -w(n+k),  SIGNUM2-w(n+k+1)] * [A(k,k)   A(k,k+1)  ]^{-1}
c                                                     [A(k+1,k) A(k+1,k+1)]     
            xm3=(-SIGNUM-w(n+k))*U11+( SIGNUM2-w(n+k+1))*U21
            xm4=(-SIGNUM-w(n+k))*U12+( SIGNUM2-w(n+k+1))*U22

c           for both cases estimate the 1-norm of U^{-1} by evaluating
c           U(1:k+1,k+2:n)^T*U(1:k+1,1:k+1)^{-T}*rhs(1:k+1)
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
c              row index l
               l=jldl(i)
c              +1/+1
               ax=ABS(w(n+l)+aldl(ii)*xp +aldl(ii+1)*xp2)
               nup=max(nup,ax)
               mup=mup+ax
c              +1/-1
               ax=ABS(w(n+l)+aldl(ii)*xp3+aldl(ii+1)*xp4)
               nup2=max(nup2,ax)
               mup2=mup2+ax
c              -1/-1
               ax=ABS(w(n+l)+aldl(ii)*xm +aldl(ii+1)*xm2)
               num=max(num,ax)
               mum=mum+ax
c              -1/+1
               ax=ABS(w(n+l)+aldl(ii)*xm3+aldl(ii+1)*xm4)
               num2=max(num2,ax)
               mum2=mum2+ax
               ii=ii+2
            end do
            if (mup.ge.mum .and. mup.ge.mup2 .and. mup.ge.mum2) then
               x =xp
               x2=xp2
               axp=max(ABS( SIGNUM -w(n+k)),
     +                 ABS( SIGNUM2-w(n+k+1)))  
            elseif (mup2.ge.mum.and.mup2.ge.mup.and.mup2.ge.mum2)then
               x =xp3
               x2=xp4
               axp=max(ABS( SIGNUM -w(n+k)),
     +                 ABS(-SIGNUM2-w(n+k+1)))  
               nup=nup2
            elseif (mum2.ge.mum.and.mum2.ge.mup.and.mum2.ge.mup2)then
               x =xm3
               x2=xm4
               axp=max(ABS(-SIGNUM -w(n+k)),
     +                 ABS( SIGNUM2-w(n+k+1)))  
               nup=num2
            else
               x =xm
               x2=xm2
               axp=max(ABS(-SIGNUM -w(n+k)),
     +                 ABS(-SIGNUM2-w(n+k+1)))  
               nup=num
            end if
c           use a buffer to store x,x2
c           if the improved estimate turns out to skip entry k although
c           the first estimate has not indicated this, then we have to
c           'undo' the update
            buffer =-x
            buffer2=-x2
c           does the norm estimate exceed the prescribed bound? 

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
            do i=jldl(k),jldl(k+1)-1
               l=jldl(i)
               y=x*aldl(i)
c              will we have a serious contribution to the rank-1
c              update of U^{-1} in column l?
c              the new average of peaks |y|/m caused by the rank-1
c              update is at least as much as the current average of
c              peaks |w(n+l)|/jw(ne+l) in row l
               if (ABS(y)*IREAL(jw(ne+l)).ge.
     +              ABS(w(n+l))*IREAL(m))
     +              jw(ne+l)=m
               w(n+l)=w(n+l)+y
            end do
            axp=ABS( SIGNUM-w(n+k))
            axm=ABS(-SIGNUM-w(n+k))
            w(n+k)=axm
            if (axp.gt.axm) w(n+k)=axp
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
            if (ABS(w(n+k+1)).le.IREAL(j)) j=j+1
c           store new number of peaks
            jw(ne+k)  =m
            jw(ne+k+1)=j
            ii=jldl(k)
            do i=jldl(k),jldl(k+1)-1
               l=jldl(i)
               xp=aldl(ii)*x
               xm=aldl(ii+1)*x2
c              will we have a serious contribution to the rank-2
c              update of U^{-1} in column l?
c              the new average of peaks |xp|/m caused by the rank-2
c              update is at least as much as the current average of
c              peaks |w(n+l)|/jw(ne+l) in row l
               if (ABS(xp)*IREAL(jw(ne+l)).ge.
     +              ABS(w(n+l))*IREAL(m))
     +              jw(ne+l)=m
c              the new average of peaks |xm|/j caused by the rank-2
c              update is at least as much as the current average of
c              peaks |w(n+l)|/jw(ne+l) in row l
               if (ABS(xm)*IREAL(jw(ne+l)).ge.
     +              ABS(w(n+l))*IREAL(j))
     +              jw(ne+l)=j
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
            condest1=CTOD(w(n+k))
            condest2=CTOD(w(n+k+1))
         end if

c
c        improved estimate using a different right hand side
         if (improved.gt.0) then
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


               do i=jldl(k),jldl(k+1)-1
c                 column index l
                  l=jldl(i)
                  w(n2+l)=w(n2+l)+x*aldl(i)
               end do
               axp=ABS( SIGNUM-w(n2+k))
               axm=ABS(-SIGNUM-w(n2+k))
               if (axm.gt.CTOD(w(n+k))) w(n+k)=axm
               if (axp.gt.CTOD(w(n+k))) w(n+k)=axp
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

c              up to this point w(nw), jw(njw1(2)),jw(njw2(2)) still
c              carry the information of column k+1. Now they are no
c              longer needed, since we will perform a 2x2 update
               do l=1,rlen(2)
                  i=jw(njw2(2)+l)
                  w(nw+i)=FNULL
                  jw(njw1(2)+i)=0
               end do
               
               ii=jldl(k)
               do i=jldl(k),jldl(k+1)-1
c                 column index l
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
               condest1=CTOD(w(n+k))
               condest2=CTOD(w(n+k+1))
            end if
         end if
c        end improved estimate


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
         if (mode.eq.1) then
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

 262     continue


c        -----   sparsify U   -----
c
c        drop tolerances
c
c        classical dropping
c        drop entries that are less than a tolerance droptol multiplied
c        by the absolute value of the diagonal entry
 273     if (mode.eq.0) then
            droptolU=droptol*absaldlk


c           do we use inverse-based dropping?
            if (droptype.gt.0) then
               droptolU=droptolU/CTOD(w(n+k))
            end if 
            maxcondest=max(maxcondest,CTOD(w(n+k)))
            droptol2=droptol/CTOD(w(n+k))

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
c                                |U(k,m)|      droptol2
c                 |U(k,l)| * max --------  <=  -----------
c                             m  |U(k,k)|      condest(k)
c                 If the entries of the Schur complement are
c                 of magnitude O(1), then the contribution to
c                 columns j, such that U(k,j)<>0 will also be 
c                 small.
c     
c              2. keep error in |tildeU^{-1}*U-I| small
c                 |U(k,l)|                       droptol
c                 --------                 <=  ---------------
c                 |U(k,k)|                     ||U^{-1}(:,k)||
               if (ABS(aldl(i)*Umax) .le.droptol2
     +             .and. ABS(aldl(i)).le.droptolU
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


 280        continue

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
            aldl(k)=ialdlk



c           Tismenetsky case
            if (sctype.gt.0) then
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
c              use iw and w as buffer
               i=i+1
               jw(i)=m
               w(i)=aldl(l)
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
c                 use iw and w as buffer
                  i=i+1
                  jw(i)=m
                  w(i)=aldl(l)
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
            droptolU=droptol

c           do we use inverse-based dropping?
            if (droptype.gt.0) then
               droptolU=droptolU/max(CTOD(w(n+k)),
     +                               CTOD(w(n+k+1)))
            end if 
            maxcondest=max(maxcondest,CTOD(w(n+k)))
            maxcondest=max(maxcondest,CTOD(w(n+k+1)))
            droptol2=droptol/max(CTOD(w(n+k)),CTOD(w(n+k+1)))

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
c                                             droptol
c                 ||U(k:k+1,l)|| * Umax  <=  -------------
c                                            condest(k)
c                 If the entries of the Schur complement are
c                 of magnitude O(1), then the contribution to
c                 columns j, such that U(k:k+1,j)<>0 will also be 
c                 small.
               axp=ABS(aldl(ii))*Umax
               axm=ABS(aldl(ii+1))*Umax
c
c
c              2. keep error in |tildeU^{-1}*U-I| small
c                                                             droptol
c                 ||U(k:k+1,k:k+1)^{-1}*U(k:k+1,l)||  <=  -------------------
c                                                         ||U^{-1}(:,k:k+1)||
               ax=ABS(U11*aldl(ii)+U12*aldl(ii+1))
               ay=ABS(U21*aldl(ii)+U22*aldl(ii+1))
               if (max(axp,axm).le.droptol2
     +             .and. max(ax,ay).le.droptolU) then
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

 281        continue

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

            U12=U12*ialdlk
            U21=U21*ialdlk
            U11=U11*ialdlk
            U22=U22*ialdlk
            aldl(k)    =U11
            aldl(k+1)  =U22
            aldl(n+1+k)=U12
c           indicate block of size 2
            jldl(n+1+k)=2


c           Tismenetsky case
            if (sctype.gt.0) then
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
c              use iw and w as buffer
               i=i+1
               jw(i)=m
               w(i)   =aldl(ii)
               w(nw+i)=aldl(ii+1)
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
c                 use iw and w as buffer
                  i=i+1
                  jw(i)=m
                  w(i)   =aldl(ii)
                  w(nw+i)=aldl(ii+1)
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
 410     continue
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
c           rewrite norm of U^{-1}
            if (CTOD(w(n+k-1)).gt.condest1)
     +           write (6,'(A,1P,E8.1,1P,E8.1)') 
     +           'condest1 is too small',CTOD(w(n+k-1)),condest1
            w(n+k-1)=condest1
c           remove entry in position k-1
            i=jw(njw1(1)+k-1)
            if (i.ne.0) then
c              swap last entry
               l=jw(njw2(1)+rlen(1))
               jw(njw1(1)+l)=i
               jw(njw2(1)+i)=l
c              clear entry
               w(k-1)=FNULL
               jw(njw1(1)+k-1)=0
               rlen(1)=rlen(1)-1
            end if
            i=0
            do l=jldl(k-1)+jw(nl+k-1),jw(nb+k-1)-1
c              is U(k-1,k)<>0?
               if (jldl(l).eq.k) i=l
            end do
c           update column k
            if (i.gt.0) then
               xp=CONJUG(SKEW(aldl(i)))*aldl(k-1)
               do l=jldl(k-1),jldl(k)-1
                  j=jldl(l)
c                 make sure that the diagonal entry does not re-enter again
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

c           epsilon size part
            if (sctype.gt.0 .and. i.eq.0) then
c              continue to update epsilon size part
               do l=jw(nb+k-1)+jw(el+k-1),jldl(k)-1
c                 is U(k,l)<>0?
                  if (jldl(l).eq.k) i=l
               end do
               if (i.gt.0) then
                  xp=CONJUG(SKEW(aldl(i)))*aldl(k-1)
                  do l=jldl(k-1),jw(nb+k-1)-1
                     j=jldl(l)
c                    make sure that the diagonal entry does not re-enter again
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
c           make sure that in the final step no column is loaded anymore
            if (k.ge.n) finalrow=1

#if defined PRINT_INFO3 || defined PRINT_INFO2
            do l=1,n
               if (jw(njw1(2)+l).ne.0)
     +              write (6,'(A,I4,I4)') '!!!, ',k-1,l
            end do
            do l=1,n
               if (w(nw+l).ne.FNULL)
     +              write (6,'(A,I4)') '!!!!, ',l
            end do
            write (6,'(I6,A)')k-1,' successful'
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
            write (6,'(I6,I6,A)')k,k+1,' successful'
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
            if (k.ge.n) finalrow=1
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
 111  continue

c     if the last step was a 1x1 update then it might be that the buffers
c     have not been cleared yet
      do l=1,n
         jw(l)=0
         w(l)=FNULL
      end do




  

c     remove the embedded small and skipped entries
#if defined PRINT_INFO || defined PRINT_INFO2
      do k=1,n+1
         write (6,'(A,I6,A,I6,I6)') 
     +        'row ',k,', previous starts: ',jldl(k),jw(nb+k)
      end do
#endif
      len=0
      k=1
c     while "k<n" ...
 600  if (k.ge.n .or. (k.ge.n-1 .and. jldl(n+1+k).gt.0))
     +     goto 610

c        1x1 case
         if (jldl(n+1+k).eq.0) then
c           gap behind row k of U
            len=len+jldl(k+1)-jw(nb+k)
c           shift U part
c           the following row is a regular row
            if (jldl(n+1+k+1).eq.0) then
               do j=jldl(k+1),jw(nb+k+1)-1
                  aldl(j-len)=aldl(j)
                  jldl(j-len)=jldl(j)
               end do
               jldl(k+1)=jldl(k+1)-len

c           the following row is a block row
            else
               ii=jldl(k+1)
               do j=jldl(k+1),jw(nb+k+1)-1
                  aldl(ii-len)  =aldl(ii)
                  aldl(ii+1-len)=aldl(ii+1)
                  jldl(j-len)=jldl(j)
                  ii=ii+2
               end do
               jldl(k+1)=jldl(k+1)-len
            end if
            k=k+1

c        2x2 case
         else
c           gap behind row k,k+1 of U
            l=jldl(k+1)-jw(nb+k)
            jldl(k+1)=jldl(k+1)-len
            len=len+2*l
c           shift U part
c           the following row is a regular row
            if (jldl(n+1+k+2).eq.0) then
               do j=jldl(k+2),jw(nb+k+2)-1
                  aldl(j-len)=aldl(j)
                  jldl(j-len)=jldl(j)
               end do
               jldl(k+2)=jldl(k+2)-len
            else
               ii=jldl(k+2)
               do j=jldl(k+2),jw(nb+k+2)-1
                  aldl(ii-len)  =aldl(ii)
                  aldl(ii+1-len)=aldl(ii+1)
                  jldl(j-len)=jldl(j)
                  ii=ii+2
               end do
c              shift the next block physically by len
               jldl(k+2)=jldl(k+2)-len
            end if
c           re-adjust logical start of row k+1
            jldl(k+1)=jldl(k+1)-l
            k=k+2
         end if
      goto 600
c     final column/row
 610  if (k.eq.n) then
c        gap behind row k of U
c        this also includes W_FC
         len=len+jldl(k+1)-jw(nb+k)
      else
c        re-adjust logical start of row k+1
         jldl(k+1)=jldl(k)
c        gap behind row k of U
c        this also includes W_FC
         len=len+jldl(k+2)-jw(nb+k)
      end if

c     if the first row is a block row, then adjust the
c     internal reference jldl(2)
      if (jldl(n+2).ne.0) then
         jldl(2)=jw(nb+1)
      end if



      
#if defined PRINT_INFO || defined PRINT_INFO2
      do k=1,n+1
         write (6,'(A,I6,A,I6)') 
     +        'row ',k,', start: ',jldl(k)
      end do
#endif





      ierr = 0
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
c----------------end-of-symiluc-----------------------------------------
c-----------------------------------------------------------------------
      end


