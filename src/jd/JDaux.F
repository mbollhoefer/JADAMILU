#include <ilupack_fortran.h>
#include <ilupackmacros.h>
*======================================================================*
*
       SUBROUTINE PJDRVCOM( N,a,ja,ia, b,jb,ib, EIGS, RES, X, LX, NEIG,
     $                      SIGMA, ISEARCH, NINIT, MADSPACE, ITER, TOL,
     $                      SHIFT, DROPTOL, MEM, ICNTL, IJOB,
     $                      NDX1, NDX2, NDX3, IPRINT, INFO, GAP, GEP)
      implicit none  
*
*     .. Scalar Arguments ..
      integer          N, LX, NEIG, ISEARCH, NINIT, MADSPACE, ITER
      integer          ICNTL(5), IJOB, NDX1, NDX2, NDX3, IPRINT, INFO
      REALS            SIGMA, TOL, SHIFT, DROPTOL, MEM, GAP
      logical          GEP
*     ..
*     .. Array Arguments ..
      integer          ja(*), ia(*), jb(*), ib(*)
      FLOAT            a(*), b(*), X(*)
      REALS            EIGS(*), RES(*)
*     ..
*
*  Arguments
*  =========
*   see comments for PJD & PJDREVCOM
*
*     .. Scalars that are defined in PJDINIT ..


      DOUBLE PRECISION  timefact,shift0,droptol0,diagmin,shiftmax,
     +                  memrequested,memused,slightlyless,toldiv,
     +                  condest0
      INTEGER*8         IPparam,IPPREC,IPdiag
      integer           IPnlev,factdgl,factspd,recompilu,IUNIT,PR,
     +                  prvdr,ggsh
      logical           reenterfirsttime,flagsingle
      common/PJDINITPJD/timefact,shift0,droptol0,diagmin,shiftmax,
     +                  memrequested,memused,slightlyless,toldiv,
     +                  IPparam,IPPREC,IPdiag,IPnlev,factdgl,factspd,
     +                  prvdr,IUNIT,PR,condest0,reenterfirsttime,
     +                  flagsingle
      REALS            LAMCH
      EXTERNAL         LAMCH
      REALS            EPSMACH
cmb   data statement uniformly moved to external definition
c      PARAMETER (EPSMACH=MY_OWN_EPS)
      DOUBLE PRECISION timeilu,rinit,rcurr,rcum,shift1
      logical flagcg
      common/JDRVCOMJD/timeilu,rinit,rcurr,rcum,shift1,flagcg
      integer ilumem  
      common/ILUPACKMEM/ilumem
cmb   conflict of data statements between different arithmetics
c      data ilumem /-1/
      external      DGLPRECSOL
      REALS         SYMILUPACKSAVEDIAG,val,fillfact,DT0,NRM2,
     +              SYMILUPACKSAVEDIAGGEP
      integer       SYMILUPACKNNZ
      EXTERNAL      evaluatetime,
     +              SYMILUPACKSAVEDIAG,SYMILUPACKRESTOREDIAG,
     +              SYMILUPACKNNZ,NRM2,SYMILUPACKSAVEDIAGGEP
#if !defined _SINGLE_REAL_ && !defined _SINGLE_COMPLEX_
      REALS         SINGLESYMILUPACKSAVEDIAG,SINGLESYMILUPACKSAVEDIAGGEP
      integer       SINGLESYMILUPACKNNZ
      EXTERNAL      SINGLESYMILUPACKSAVEDIAG,
     +              SINGLESYMILUPACKSAVEDIAGGEP,
     +              SINGLESYMILUPACKRESTOREDIAG,
     +              SINGLESYMILUPACKNNZ, SINGLESYMILUPACKINIT
#endif

*
*     .. Local Scalars ..
      integer NW, NEIGP, ierr, factspd0,i,j,k,l
      INTEGER*8 param
      REALS tent,delta,rprev,rrel,time0,time1,tmsol,timeilu0
      real tic, toc, tic0
      logical reinit
*
*     saving all.
      SAVE
*     ..
*     .. Executable Statements ..
c
c     ilumem: common block variable to indicate whether 
c             1. no memory has been allocated for the ILU so far 
c                (ilumem=-1)
c             2. memory is still available from previous computations 
c                and should be kept
c                (ilumem>0)
c             3. memory is still available but can be removed
c                (ilumem=0)
*
*     Perform some initialisation
*
*     IJOB=0 refers to the first time that PJDRVCOM is called from 
*     PJD/PJDREVCOM. Note however, that if ICNTL(2)=-1, or -2, then
*     earlier eigenvalue computation including a preconditioner have
*     been made, maybe with different options, different matrix,...

cmb   data statement uniformly moved to external definition
cmb   bug gfortran + dlamch.f slamch.f also affects LAPACK's own routines,
cmb   e.g., dsyev.f.  So bypassing this problem using constants does not
cmb   really cure the problem. Compiling dlamch.f and slamch.f without -O helps
      EPSMACH=LAMCH('E')
      call COMMONILUPACKMEMDEF

      IF (IJOB.eq.0) THEN
         if (icntl(5).ne.0) then 
            flagsingle=.true.
         else
            flagsingle=.false.
         end if
         call evaluatetime(tic0)
         IUNIT=IPRINT
         PR=1
         IF (IUNIT.eq.0) THEN
            PR=0
            IUNIT=6
         ELSE IF (IUNIT.lt.0) THEN
            PR=2
            IUNIT=-IUNIT
         END IF
         IF (IUNIT.eq.5) IUNIT=6
         if (PR.ge.1) then
            write (IUNIT,'(/,A,/)') 
     +       '**ENTERING PJDREVCOM************************************'
            if (ISEARCH.le.1) then
               write (IUNIT,'(A,/,A)') 
     +          'Computing the smallest eigenvalue(s)',
     +          '------------------------------------'
            else
               write (IUNIT,'(A,1P,E12.5,/,A)') 
     +          'Computing the eigenvalue(s) closest to ',SIGMA,
     +          '---------------------------------------------------'
            end if
         end if
c        if we do not resume from earlier computations and do not have
c        some old preconditioner still available
         if ((ICNTL(2).ne.-1.and.ICNTL(2).ne.-2).or.factdgl.eq.0) then
c           no previous variation of DROPTOL
            prvdr=0
c           means that no previous call to PJDINIT for the same problem
            timefact=-1.0
c           if there is still some old memory available, 'ilumem' will 
c           ensure that the memory is not given away and also that 
c           PJDCLEANUP1 is not really invoked if there no preconditioner
c           available yet
            CALL PJDCLEANUP1
            CALL PJDINIT(N,a,ja,ia,b,jb,ib,SIGMA,ISEARCH,
     $                   SHIFT,DROPTOL,MEM,ICNTL,IERR,GEP)
            if (icntl(5).ne.0) then 
               flagsingle=.true.
            else
               flagsingle=.false.
            end if
            reenterfirsttime=.false.
         else if (ICNTL(2).eq.-1.or.ICNTL(2).eq.-2) then
c           no previous variation of DROPTOL (or forget about it)
            prvdr=0
c           we are entering the routine with a possibly new matrix
c           old preconditioner for the first time
            reenterfirsttime=.true.
            rinit=0.0
            rprev=0.0
            rrel =0.0
            rcurr=0.0
            rcum =0.0
c
c           reset and re-init some of the global parameters
c           in particular the matrix could have changed
c
            IF (ja(1).lt.0) THEN
c              indicate that diagonal preconditioning is desired
               IF (ISEARCH.ge.2) shift0=SIGMA
            ELSE
c              Compute Gershgorin discs and save the diagonal entries
               do i=1,n
                  do j=ia(i),ia(i+1)-1
                     k=ja(j)
                     if (k.eq.i) val =a(j)
                  end do
                  if (i.eq.1) then
                     diagmin=val
                  else
                     diagmin=MIN(dble(val),diagmin)
                  end if
               end do

               if (.not.GEP) then
#if !defined _SINGLE_REAL_ && !defined _SINGLE_COMPLEX_
                  if (flagsingle) then
c                  write(6,'(A)') 'apply mixed savediag driver'
                     shift0=SINGLESYMILUPACKSAVEDIAG(param,n,ia,ja,a)
                  else
c                  write(6,'(A)') 'apply double savediag driver'
                     shift0=SYMILUPACKSAVEDIAG(param,n,ia,ja,a)
                  end if
#else
c               write(6,'(A)') 'apply single savediag driver'
                  shift0=SYMILUPACKSAVEDIAG(param,n,ia,ja,a)
#endif
               else
c                 HERE INSERT Gershgorin estimate without saving dgl.
                  shift0=SYMILUPACKSAVEDIAGGEP(param,n,ia,ja,a,ib,jb,b)
               end if

c              in all other cases, the shift is provided by the user
               if (ISEARCH.ge.2) shift0=sigma
               ggsh=0
               if (ISEARCH.eq.1.and.SHIFT.lt.diagmin) then
                  shift0=SHIFT
               else
                  ggsh=1
               end if

c              permute input matrix properly
               do i=1,n
                  do j=ia(i),ia(i+1)-1
                     k=ja(j)
                     if (k.eq.i) then
c                       swap leading entry with the diagonal entry
                        l=ia(i)
                        ja(j)=ja(l)
                        ja(l)=k
                        val =a(j)
                        a(j)=a(l)
                        a(l)=val
                     end if
                  end do
               end do
c              restore old unshifted value

               if (.not.GEP) then
#if !defined _SINGLE_REAL_ && !defined _SINGLE_COMPLEX_
                  if (flagsingle) then
c                  write(6,'(A)') 'apply mixed restorediag driver'
                     call SINGLESYMILUPACKRESTOREDIAG(param,n,ia,ja,a)
                  else
c                  write(6,'(A)') 'apply double restorediag driver'
                     call SYMILUPACKRESTOREDIAG(param,n,ia,ja,a)
                  end if
#else
c               write(6,'(A)') 'apply single restorediag driver'
                  call SYMILUPACKRESTOREDIAG(param,n,ia,ja,a)
#endif
               end if
c              re-set time ILU 
               timeilu=0.0
            END IF
         end if
c
         if (ierr.ne.0) then 
c           multilevel ILU did not succeed
            info=-54321
            IJOB=0
            return
         end if
         rprev=1.0
         NW=NEIG
         call evaluatetime(tic)
         timeilu0=max(tic-tic0,0.0)
      END IF
c
 1    CONTINUE

c      write (6,'(A)')'call JDRVCOM'
      CALL JDRVCOM( N, EIGS, RES, X, LX, NEIG, SIGMA,
     $              ISEARCH, NINIT, MADSPACE, ITER, TOL, IJOB,
     $              NDX1, NDX2, NDX3, IPRINT, INFO, GAP, GEP)
c      write (6,'(A)')'JDRVCOM completed'     

      IF (IJOB.eq.0)  GOTO 100
      IF (IJOB.eq.2)  GOTO 10
      If (ICNTL(1).NE.1) RETURN 
*     IJOB=1, 3, 4 : Return for MATVEC
      IF (IJOB.eq.1 .OR. IJOB.eq.3) 
     $    call MATVECA(N,ia,ja,a,X(NDX1),X(NDX2))
      IF ((IJOB.eq.1 .OR. IJOB.eq.4) .AND. GEP) 
     $    call MATVECA(N,ib,jb,b,X(NDX1),X(NDX3))
      GOTO 1
*     IJOB=2: Preconditioning solve needed
 10   CONTINUE
*      Compute a new preconditioner, if possible and useful
         reinit=.false.
         factspd0=0
         call evaluatetime(toc)
*        first consider the diagonal case 
         if (factdgl.gt.0 .and. .not.flagcg .and. ISEARCH.le.1 .and.
     +       (ICNTL(2).eq.0.or.ICNTL(2).eq.-2) .and. ICNTL(1).ne.2) then
            if (NEIG.ge.1) then
               tent=EIGS(NW)
               factspd0=-1
            else
               tent=shift1-rcurr
               factspd0=-1
               if ( 10*rcurr.ge.diagmin-tent .and.
     +             shift1.gt.shift0 ) factspd0=factspd
            end if   
            if ( diagmin.gt.tent .and. factspd0.lt.0 .and.
     +          10*abs(shift0-tent).ge.(diagmin-shift0) ) then
               SHIFT=tent
               reinit=.true.
            end if
         end if
*        now the ILU case
         if (factdgl.lt.0 .and. .not.flagcg .and.
     +       (ICNTL(2).eq.0.or.ICNTL(2).eq.-2).and.ICNTL(1).ne.2 .and. 
     +       rcum-rprev.le.6*log(rinit/tol) ) then
c           check if refactorizing is potentially cost effective
            tmsol=toc-tic
            if (tmsol.lt.1e-1) tmsol=0.0
            if (rcum.gt.rprev) then
              time1=tmsol*log(rinit/tol)/(rcum-rprev)
              time0=tmsol*log(rcurr/tol)/(rcum-rprev)
            else
              time1=4*timefact
              time0=6*timefact
            end if
            if (2*timefact.lt.time1 .and. 
     +          4*timefact.lt.(time0+(NW-NEIG-1)*time1) ) then
c     time1 is the estimated time for computing 1 eigenvalue (from 
c     scratch)
c     time0 is the estimated time for finishing the computation of the 
c     current eigenvalue (in both cases, we assume that log(residual) is 
c     linear in time). The idea behind the second condition is that a 
c     new (more accurate) factorization may be twice as costly as the 
c     available one, but may also lead to a reduction of the time for 
c     computing eigenvalues by a factor of 2.
c     If this heuristic is exact, performing a new factorization is cost
c     effective iff the second condition hold.
c
c              shift possibly too small when factspd<0
c              do not change DROPTOL because increasing SHIFT implies more fill
c
               if (factspd.lt.0) then
                  if (NEIG.ge.1) then 
                     if (NEIG.gt.1) then
                        delta=abs(EIGS(NW)-EIGS(NW-NEIG+1))
     +                       /dble(NEIG-1)
                     else
                        delta=max(GAP,RZERO)
                     end if
                     tent=min(2*shiftmax-dble(EIGS(NW)),
     +                        dble(EIGS(NW))-dble(delta))       
                  else 
                     tent=min(2*shiftmax-shift1,
     +                        shift1-rcurr-max(GAP,RZERO))
                     if (2*rcurr.ge.diagmin-tent) factspd0=factspd
                  end if
                  if ( (factspd0.eq.0 .or. 4*timefact.lt.time0)   
     +                 .and. diagmin.gt.tent .and. 
     +                 2*(tent-shift0).ge.(diagmin-shift0) ) then
                     SHIFT=tent
                     rprev=rcum
                     reinit=.true.
                  end if
               end if
               if (factspd0.ge.0 .and. (.not.reinit) .and.
     +             memused.lt.slightlyless*memrequested
     +             .and. prvdr.le.0 .and. DROPTOL.ge.SQRT(EPSMACH))
     +                                                  then
c    To avoid recomputing again and again a new
c    factorization when we already have a close-to-exact one
c    within fairly small CPU time (e.g. :tridiagonal matrices).
c    This refers to direct solver in single precision
c   
c              if there is still 20-40% memory left in the elbow space then
c              we try to re-factor the matrix with a smaller droptol
c                 if the elbow space is large enough, then even reduce
c                 the drop tolerance further
                  if (ICNTL(2).ne.-2) then
                     reenterfirsttime=.false.
                  end if
                  DT0=DROPTOL
                  if (.not.reenterfirsttime) then
                     if (memused.lt.(3*slightlyless-2)*memrequested)then
                        DROPTOL=DROPTOL/(4*toldiv)
                     elseif (memused.lt.(2*slightlyless-1)*memrequested)
     +                       then
                        DROPTOL=DROPTOL/(2*toldiv)
                     else
                        DROPTOL=DROPTOL/toldiv
                     end if
                     DROPTOL=max(DROPTOL,1.01*SQRT(EPSMACH))
                  end if
                  rprev=rcum
                  reinit=.true.
                  if (DROPTOL.eq.1.01*SQRT(EPSMACH) .and.
     +                DT0.eq.DROPTOL .and.
     +                .not.reenterfirsttime) then 
                     reinit=.false.
                  end if
                  reenterfirsttime=.false.
                  SHIFT=shift0
c                 last shift slightly too large when  facspd>0
                  if (factspd.gt.0) then 
                     if (NEIG.ge.1) then
                        if(NEIG.gt.1) then
                           delta=(EIGS(NW)-EIGS(NW-NEIG+1))/dble(NEIG-1)
                        else
                           delta=max(GAP,RZERO)
                        end if
                        SHIFT=min(2*shift0-dble(EIGS(NW)),
     +                            dble(EIGS(NW)-delta),shiftmax)
                     else if (shift0.gt.shift1) then   
                        SHIFT=min(shift1,shiftmax)
                     end if
                  end if
               else if (factspd.gt.0) then
c              finally consider cases where SHIFT was fairly too large
                  if (NEIG.ge.1) then
                     if (shift0.gt.EIGS(NW)) then
                        if(NEIG.gt.1) then
                           delta=(EIGS(NW)-EIGS(NW-NEIG+1))/dble(NEIG-1)
                        else
                           delta=max(GAP,RZERO)
                        end if
                        SHIFT=min(dble(EIGS(NW)-delta),shiftmax)
                        rprev=rcum
                        reinit=.true.
                     end if
                  else if (shift0.gt.shift1 .and.
     +                    2*(shift0-shift1).ge.(diagmin-shift0) ) then
                     SHIFT=min(shift1,shiftmax)
                     rprev=rcum
                     reinit=.true.
                  end if   
               end if
            end if
         end if
         if (reinit) then
            CALL PJDINIT(N,a,ja,ia,b,jb,ib,SIGMA,max(ISEARCH,1),
     $                   SHIFT,DROPTOL,MEM,ICNTL,IERR,GEP)
            if (icntl(5).ne.0) then 
               flagsingle=.true.
            else
               flagsingle=.false.
            end if
            if (ierr.ne.0) then
c              multilevel ILU did not succeed
cmb     advice by Yvan
               factdgl=1
               info=-54321
               IJOB=0
               return
            end if
            call evaluatetime(tic)
            timeilu=timeilu+max(tic-toc,0.0)
            if (factspd0.lt.0.and.factspd.le.0) factspd=factspd0
         end if
         if (factdgl.gt.0) then
c            write (IUNIT,'(A)') 'solve dgl. precond. system'
            call DGLPRECSOL(IPdiag,N,X(NDX1),X(NDX2))
         else
c            write (IUNIT,'(A)') 'solve ILUPACK precond. system'

#if !defined _SINGLE_REAL_ && !defined _SINGLE_COMPLEX_
            if (flagsingle) then
c               write(6,'(A)') 'apply mixed sol driver'
               call SINGLESYMILUPACKSOL(IPparam,IPPREC,IPnlev, 
     +                                  X(NDX1),X(NDX2),n)
            else
c               write(6,'(A)') 'apply double sol driver'
               call SYMILUPACKSOL(IPparam,IPPREC,IPnlev, 
     +                            X(NDX1),X(NDX2),n)
            end if
#else
c            write(6,'(A)') 'apply single sol driver'
            call SYMILUPACKSOL(IPparam,IPPREC,IPnlev,X(NDX1),X(NDX2),n)
#endif

         end if
         NEIGP=NEIG
        goto 1
*
*     comes here to terminate
 100  CONTINUE
      call evaluatetime(toc)
      IF (PR.GE.1) WRITE (IUNIT,'(A,F10.2,A,/,A,F10.2,A,/)') 
     $  '       CPU time for preconditioning:',timeilu+timeilu0,'[sec]',
     $  '                     Total CPU time:',toc-tic0,'[sec]'
      SHIFT=shift0
      IF (factdgl.ge.0) THEN
         SHIFT=EIGS(1) 
      ELSE IF (factspd.ne.0.or.ISEARCH.le.0) THEN
c
c      heuristic rule for new shift
c
       if(NEIG.gt.1) then
        delta=(EIGS(NEIG)-EIGS(1))/dble(NEIG-1)
       else
        delta=max(GAP,RZERO)
       end if
       if (factspd.gt.0) then
         SHIFT=min(dble(SHIFT),shiftmax)
         SHIFT=min(2*SHIFT-EIGS(1),EIGS(1)-delta)
       else
         SHIFT=min(2*shiftmax-dble(EIGS(1)),dble(EIGS(1)-delta))
         if (shift0.lt.EIGS(1)) SHIFT=max(dble(SHIFT),shift0)
       end if
c
c      heuristic rule for new droptol
c       
c      
      ELSE
          tmsol=max(toc-tic,1.0e-2)
         if (rcum.gt.rprev) then
           time1=tmsol*log(rinit/tol)/(rcum-rprev)
         else
           time1=4*timefact
         end if
         if (2*timefact.lt.time1) then
c        DROPTOL likely too large
               if (memused.lt.slightlyless*memrequested .and.
     +        (prvdr.le.0 .or. (ISEARCH.le.1.and.SHIFT.lt.shift0))
     +        .and. (ISEARCH.ge.2 .or. SHIFT.le.shift0) ) then
c                 if the elbow space is large enough, then even reduce
c                 the drop tolerance further
                  if (memused.lt.(3*slightlyless-2)*memrequested) then
                     DROPTOL=DROPTOL/(4*toldiv)
                  elseif (memused.lt.(2*slightlyless-1)*memrequested)
     +                    then
                     DROPTOL=DROPTOL/(2*toldiv)
                  else
                     DROPTOL=DROPTOL/toldiv
                  end if
                  DROPTOL=max(DROPTOL,1.01*SQRT(EPSMACH))
               end if
            else if (timefact.gt.2*time1) then
c           DROPTOL likely too small
               tent=timefact/time1
               DROPTOL=min(tent*DROPTOL,sqrt(DROPTOL))
               DROPTOL=max(DROPTOL,1.01*SQRT(EPSMACH))
            end if
         END IF
         if (PR.ge.1.and.ISEARCH.le.1) write(IUNIT,900) SHIFT
         if (PR.ge.1.and.factdgl.le.0) write (IUNIT,901) DROPTOL
         if (PR.ge.1) write (IUNIT,'(/,A,/)') 
     +        '**LEAVING PJDREVCOM*************************************'
*
      RETURN
*
 900  format(' (Suggested SHIFT   if restart needed:',1pe9.2,')')
 901  format(' (Suggested DROPTOL if restart needed:',1pe9.2,')')
      END
*
*=============================================================================*
*=============================================================================*
*
      SUBROUTINE PJDINIT(N,a,ja,ia,b,jb,ib,SIGMA,ISEARCH,  
     $                  SHIFT,DROPTOL,MEM,ICNTL,IERR,GEP)
       implicit none  
*     ..  Arguments ..
      integer           N, ja(*),ia(*), jb(*),ib(*), ISEARCH,
     +                  ICNTL(5), IERR
      FLOAT  a(*), b(*)
      REALS  SIGMA, SHIFT, DROPTOL, MEM
      logical GEP
    
*  Arguments
*  =========
*
* see comments for PJD & PJDREVCOM
* 
* IERR     (output) integer
*              IERR=0 if normal termination. 
*              IERR.NE.0 if an error occurred - see printed output for details.
*
*  ===========================================================
*
*     .. Global scalars for use in PJDREVCOM ..
      DOUBLE PRECISION  timefact,shift0,droptol0,diagmin,shiftmax,
     +                  memrequested,memused,slightlyless,toldiv,
     +                  condest0
      INTEGER*8         IPparam,IPPREC,IPdiag
      integer           IPnlev,factdgl,factspd,IUNIT,PR,
     +                  prvdr
      logical           reenterfirsttime,flagsingle
      common/PJDINITPJD/timefact,shift0,droptol0,diagmin,shiftmax,
     +                  memrequested,memused,slightlyless,toldiv,
     +                  IPparam,IPPREC,IPdiag,IPnlev,factdgl,factspd,
     +                  prvdr,IUNIT,PR,condest0,reenterfirsttime,
     +                  flagsingle
      integer ilumem  
      common/ILUPACKMEM/ilumem
cmb   conflict of data statements between different arithmetics
c      data factdgl /0/
c      data slightlyless/0.8d0/
c      data toldiv/2.0d0/
*     .. Global scalars for use in PJDREVCOM ..
*
* IPparam  (input/output) INTEGER*8 
* IPPREC   (input/output) INTEGER*8
*          IPparam and IPPREC internal variables that carry the memory address
*          of C-pointers needed by ILUPACK
*
* IPnlev   (input/output) integer 
*          number of levels of the multilevel ILU
*
*
*     .. local variables for use in PJDINIT ..
      integer i,j,k,l, matching,maxit,lfil,lfilS,nrestart
      integer newsh,ggsh,recompilu
      INTEGER*8        param
      character ordering*20
      REALS    mydroptol,restol,myelbow,val,condest,dbuff,
     +         SYMILUPACKSAVEDIAG,fillfact,mymem,nposeig,
     +         RDIAGMIN,sshift0,SYMILUPACKSAVEDIAGGEP
      REALS            LAMCH
      EXTERNAL         LAMCH
      REALS            EPSMACH
cmb   data statement uniformly moved to external definition
c      PARAMETER (EPSMACH=MY_OWN_EPS)
      REAL     tic,toc,totaltime
      external evaluatetime
*     ILUPACK parameters
*     .. external functions ..
      integer  SYMILUPACKFACTOR, SYMILUPACKFACTORGEP, 
     +         SYMILUPACKNNZ,SYMSPDILUPACKCONVERT
      external SYMILUPACKINIT, SYMILUPACKSOL, SYMILUPACKDELETE,
     +         SYMILUPACKFACTOR,SYMILUPACKFACTORGEP,  
     +         SYMSPDILUPACKCONVERT,
     +         SYMILUPACKINFO,SYMILUPACKNNZ, MATVECA, 
     +         DGLPRECSETUP,SYMILUPACKSAVEDIAGGEP,
     +         SYMILUPACKSAVEDIAG,SYMILUPACKRESTOREDIAG
#if !defined _SINGLE_REAL_ && !defined _SINGLE_COMPLEX_
      REALS         SINGLESYMILUPACKSAVEDIAG,SINGLESYMILUPACKSAVEDIAGGEP
      integer       SINGLESYMILUPACKFACTOR, SINGLESYMILUPACKFACTORGEP, 
     +              SINGLESYMILUPACKNNZ,SINGLESYMSPDILUPACKCONVERT
      EXTERNAL      SINGLESYMILUPACKINIT, SINGLESYMILUPACKSOL, 
     +              SINGLESYMILUPACKDELETE,
     +              SINGLESYMILUPACKFACTOR, SINGLESYMILUPACKFACTORGEP, 
     +              SINGLESYMSPDILUPACKCONVERT,
     +              SINGLESYMILUPACKINFO,SINGLESYMILUPACKNNZ,
     +              SINGLESYMILUPACKSAVEDIAG,
     +              SINGLESYMILUPACKRESTOREDIAG,
     +              SINGLESYMILUPACKRESTOREDIAGGEP
#endif

*     
*     .. local variables for use in PJDINIT ..
*
* i,j,k,l    integer. various indices
*
* matching   integer. flag that is set in order to indicate the use of
*            symmetric matchings
*
* maxit      integer. dummy, not used
*
* lfil,lfilS integer. number of nonzeros per row, set to n+1
*
* nrestart   integer. dummy, not used
* 
* ordering   CHARACTER*20. string used to passed the desired ordering
*            (here default: 'amd')
*
* mydroptol  REALS. default drop tolerance provided by ILUPACK
*            only used if the user passes a negative drop tolerance
*
* restol     REALS. dummy, not used
*
* myelbow    REALS. elbow space in terms of nnz(A), only used if
*            the user provides a negative MEM
*
* val        REALS. auxilliary variable used for the shift 
*
* condest    REALS. norm of the inverse triangular factors, 
*            adapted during its computation and passed to ICNTL(1)
*
* tic,toc    REAL. used to measure the computing time
*
* TIMEARRAY  ARRAY (REAL). auxilliary buffer
* 
* totaltime  REAL. used for measuring the total computing time
*
*
*
*     saving all.
      SAVE
*     ..
*     .. Executable Statements ..
c     in this subroutine we want to compute a new preconditioner. If we
c     still have an old preconditioner available, then we first have to
c     clean it up. factdgl=0 means that currently no preconditioner is
c     available. However, some meory might be available from previous
c     computations. In that case inside PJDCLEANUP1, 'ilumem' will make
c     sure that the memory is kept and immediately available to cover
c     the next preconditioner

cmb   data statement uniformly moved to external definition
      EPSMACH=LAMCH('E')
      call COMMONPJDINITPJDDEF


      if (factdgl.ne.0) call  PJDCLEANUP1
c
      IF (ja(1).lt.0) THEN
c        indicate that diagonal preconditioning is desired
         IF (ISEARCH.ge.2) shift0=SIGMA
         RDIAGMIN=diagmin
         sshift0=shift0
         call DGLPRECSETUP(IPdiag,a,n,ISEARCH,SHIFT,sshift0,
     +                     factdgl,factspd,RDIAGMIN)
         shift0=sshift0
         if (PR.ge.1) write (IUNIT,'(/,A,/,A,1PE12.4,/,A)') 
     +     '- Setting up a new diagonal preconditioner -------------',
     +     'used shift:',shift0,
     +     '--------------------------------------------------------'  
        return
      ELSE
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c        non-diagonal case: use ILUPACK
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c        SETUP PRECONDITIONER
c        cccccccccccccccccccc
c        initialize parameters for ILUPACK to their default values

#if !defined _SINGLE_REAL_ && !defined _SINGLE_COMPLEX_
         if (flagsingle) then
c            write(6,'(A)') 'apply mixed init driver'
            call SINGLESYMILUPACKINIT(n,ia,ja,a,matching,ordering,
     +                                mydroptol,condest,restol,maxit,
     +                                myelbow,lfil,lfilS,nrestart)
         else
c            write(6,'(A)') 'apply double init driver'
            call SYMILUPACKINIT(n,ia,ja,a,matching,ordering,mydroptol,
     +                          condest,restol,maxit,myelbow,
     +                          lfil,lfilS,nrestart)
         end if
#else
c         write(6,'(A)') 'apply single init driver'
         call SYMILUPACKINIT(n,ia,ja,a,matching,ordering,mydroptol,
     +                       condest,restol,maxit,myelbow,
     +                       lfil,lfilS,nrestart)
#endif

c         write (IUNIT,'(A)') 'AMG parameter initialized'
c        now the user may vary the parameters to gain optimal performance

c        maximum weight matching
c        default value is different from zero, matching turned on
         matching=1

c        multilevel orderings
c        'amd' (default) Approximate Minimum Degree
c        nothing else used in JD

c        threshold for ILU, default: 1e-2
         if (droptol.lt.0) droptol=mydroptol
      
c        norm bound for the inverse factors L^{-1}, U^{-1}, default: 5.0
c        condest is automatically adapted when the preconditioner is computed
         if (timefact.gt.0.0) then
            condest=condest0
c           condest retrieved from previous call to PJDINIT
         else if (ICNTL(4).le.0) then
            condest=5.0
         else
            condest=ICNTL(4)
         end if

c        relative error for the backward error (SPD case: relative energy
c        norm) used during the iterative solver, default: sqrt(eps)
c        restol=1e-12. NOT USED in JD

c        maximum number of iterative steps, default: 500
c        maxit=1000. NOT USED in JD

c        elbow space factor for the fill computed during the ILU, default: 10
         if (MEM.le.0.0) MEM=myelbow
c        maximum number of nonzeros per column in L/ row in U, default: n+1
c        lfil=n+1

c        maximum number of nonzeros per row in the approximate Schur complement,
c        default: n+1
c        lfilS=n+1

c        restart length for GMRES, default: 30
c        nrestart=20. NOT USED in JD

c        compute multilevel ILU
c        cccccccccccccccccccccc
c        Note that the initial input matrix A will be rescaled by rows and
c        by columns (powers of 2.0) and that the order in the array might have
c        been altered

         newsh=0
 3       call evaluatetime(tic)
         if (PR.ge.1) write (IUNIT,'(/,A)') 
     +     '- Setting up a new ILU preconditioner ------------------'
c        Compute Gershgorin discs and save the diagonal entries
         do i=1,n
            do j=ia(i),ia(i+1)-1
               k=ja(j)
               if (k.eq.i) val =a(j)
            end do
            if (i.eq.1) then
             diagmin=val
            else
             diagmin=min(dble(val),diagmin)
            end if
         end do

         if (.not. GEP) then
#if !defined _SINGLE_REAL_ && !defined _SINGLE_COMPLEX_
            if (flagsingle) then
c            write(6,'(A)') 'apply mixed savediag driver'
               shift0=SINGLESYMILUPACKSAVEDIAG(param,n,ia,ja,a)
            else
c            write(6,'(A)') 'apply double savediag driver'
               shift0=SYMILUPACKSAVEDIAG(param,n,ia,ja,a)
            end if
#else
c         write(6,'(A)') 'apply single savediag driver'
            shift0=SYMILUPACKSAVEDIAG(param,n,ia,ja,a)
#endif
         else
c         HERE INSERT Gershgorin estimate without saving dgl.
            shift0=SYMILUPACKSAVEDIAGGEP(param,n,ia,ja,a,ib,jb,b)
         end if

c        in all other cases, the shift is provided by the user
         if (ISEARCH.ge.2) shift0=sigma
         ggsh=0
         if (ISEARCH.eq.1.and.SHIFT.lt.diagmin) then
            shift0=SHIFT
         else
            ggsh=1
         end if
         if (ISEARCH.le.0.and.newsh.eq.1)  shift0=SHIFT
         if (newsh.ne.1.and.timefact.le.0.0) shiftmax=diagmin 
c         write (6, '(A,1P,E12.4)') 'use shift ',shift0
c        shift input matrix
         if (.not.GEP) then
            do i=1,n
c           write (IUNIT,'(10I8)') (ja(j),j=ia(i),ia(i+1)-1)
c           write (IUNIT,'(1P,10E8.1)') (a(j),j=ia(i),ia(i+1)-1)
               do j=ia(i),ia(i+1)-1
                  k=ja(j)
                  if (k.eq.i) then
                     a(j)=a(j)-shift0
                  end if
               end do
            end do
         end if
         mymem=mem
c        store the amount of memory requested by the user
         memrequested=mem
c        keep track on the request of the user to do adaptive preconditioning
         if (ICNTL(2).eq.0.or.ICNTL(2).eq.-2) then
           recompilu=1
         else
           recompilu=-1
         end if
         if (ICNTL(3).le.0) then
           nposeig=0.01
         else
cmb        turn off having negative eigenvalues in the factorization
           nposeig=0
         end if
c        note that the decision whether the memory is kept or not is 
c        performed based on the value of ICNTL(2) (if zero or -2, then
c        the memory is kept, otherwise the remaining memory is given
c        away on exit)
c        If haven't computed an ILU yet or if do not use adaptive 
c        preconditioning then we enter the ILU and start from scratch
         sshift0=shift0
         if (ilumem.le.0) then

#if !defined _SINGLE_REAL_ && !defined _SINGLE_COMPLEX_
            if (flagsingle) then
c               write(6,'(A)') 'apply mixed factor driver'
               if (GEP) then
                  ierr=SINGLESYMILUPACKFACTORGEP(IPparam,IPPREC,IPnlev,
     +                                           ICNTL,n,ia,ja,a,ib,jb,
     +                                           b,shift0,matching,
     +                                           ordering,droptol, 
     +                                           condest, restol,maxit,
     +                                           mymem, lfil, lfilS,
     +                                           nrestart)
               else
                  ierr=SINGLESYMILUPACKFACTOR(IPparam,IPPREC,IPnlev,
     +                                        ICNTL,n,ia,ja,a,matching,
     +                                        ordering,droptol,condest,
     +                                        restol, maxit, mymem,
     +                                        lfil, lfilS, nrestart)
               endif
            else
c               write(6,'(A)') 'apply double factor driver'
               if (GEP) then
                  ierr=SYMILUPACKFACTORGEP(IPparam,IPPREC,IPnlev,ICNTL,
     +                                     n,ia,ja,a,ib,jb,b,sshift0,
     +                                     matching,ordering,droptol,
     +                                     condest,restol,maxit,mymem,
     +                                     lfil,lfilS, nrestart)
               else
                  ierr=SYMILUPACKFACTOR(IPparam,IPPREC,IPnlev,ICNTL,
     +                                  n,ia,ja,a,matching,ordering,
     +                                  droptol,condest,restol,maxit,
     +                                  mymem, lfil, lfilS, nrestart)
               endif
            end if
#else
c            write(6,'(A)') 'apply single factor driver'
            if (GEP) then
               ierr=SYMILUPACKFACTORGEP(IPparam,IPPREC,IPnlev,ICNTL,n,
     +                                  ia,ja,a,ib,jb,b,sshift0,
     +                                  matching,ordering, droptol,
     +                                  condest, restol, maxit, mymem,
     +                                  lfil, lfilS, nrestart)
            else
               ierr=SYMILUPACKFACTOR(IPparam,IPPREC,IPnlev,ICNTL,
     +                               n,ia,ja,a,matching,ordering,
     +                               droptol, condest, restol, maxit,
     +                               mymem, lfil, lfilS, nrestart)
            endif
#endif
            

         else
c           ilumem=1
c           we have at least once computed the ILU. We are resuming
c           from previous computations. Indicate that by a negative n
c           it will be changed back inside the function
            i=-n

#if !defined _SINGLE_REAL_ && !defined _SINGLE_COMPLEX_
            if (flagsingle) then
c               write(6,'(A)') 'apply mixed factor driver'
               if (GEP) then
                  ierr=SINGLESYMILUPACKFACTORGEP(IPparam,IPPREC,IPnlev,
     +                                           ICNTL,i,ia,ja,a,ib,jb,
     +                                           b,shift0,matching,
     +                                           ordering,droptol,
     +                                           condest,restol,maxit,
     +                                           mymem,lfil,lfilS,
     +                                           nrestart)
               else
                  ierr=SINGLESYMILUPACKFACTOR(IPparam,IPPREC,IPnlev,
     +                                        ICNTL,i,ia,ja,a,matching,
     +                                        ordering,droptol,condest,
     +                                        restol,maxit,mymem,lfil,
     +                                        lfilS,nrestart)
               endif
            else
c               write(6,'(A)') 'apply double factor driver'
               if (GEP) then
                  ierr=SYMILUPACKFACTORGEP(IPparam,IPPREC,IPnlev,ICNTL,
     +                                     i,ia,ja,a,ib,jb,b,sshift0,
     +                                     matching,ordering, droptol,
     +                                     condest,restol,maxit,mymem,
     +                                     lfil,lfilS, nrestart)
               else
                  ierr=SYMILUPACKFACTOR(IPparam,IPPREC,IPnlev,ICNTL,
     +                                  i,ia,ja,a,matching,ordering,
     +                                  droptol, condest, restol,
     +                                  maxit, mymem, lfil, lfilS,
     +                                  nrestart)
               endif
            end if
#else
c            write(6,'(A)') 'apply single factor driver'
            if (GEP) then
               ierr=SYMILUPACKFACTORGEP(IPparam,IPPREC,IPnlev,ICNTL,
     +                                  i,ia,ja,a,ib,jb,b,sshift0,
     +                                  matching,ordering,droptol,
     +                                  condest,restol,maxit, mymem,
     +                                  lfil,lfilS, nrestart)
            else
               ierr=SYMILUPACKFACTOR(IPparam,IPPREC,IPnlev,ICNTL,
     +                               i,ia,ja,a,matching,ordering,
     +                               droptol, condest, restol,
     +                               maxit, mymem, lfil, lfilS,
     +                               nrestart)
            endif
#endif

         end if

c        set flags in order to indicate what happened
         if (ICNTL(2).eq.0.or.ICNTL(2).eq.-2) then
c           if adaptive preconditioning is desired, then we hold the 
c           remaining memory up even if it is currently not needed by 
c           the preconditioner to be computed by ILUPACK
            ilumem=1
         else
c           if no adaptive preconditioning is requested, then we can give
c           away the memory that is not needed by ILUPACK
            ilumem=0
         end if

c        if an error occured during the factorization, then the initial 
c        matrix will be unscaled anyway
         if (ierr.eq.0.and..not.GEP) then
#if !defined _SINGLE_REAL_ && !defined _SINGLE_COMPLEX_
            if (flagsingle) then
c              no unscaling necessary since the preconditioner
c              was applied to a copy of A anyway
            else
c               write(6,'(A)') 'apply double unscale driver'
               call ILUPACKUNDOSCALING(IPparam,IPPREC,IPnlev,n,ia,ja,a)
            end if
#else
c            write(6,'(A)') 'apply single unscale driver'
            call ILUPACKUNDOSCALING(IPparam,IPPREC,IPnlev,n,ia,ja,a)
#endif

c            write (IUNIT,'(A)') 'matrix rescaled'
         end if
c        permute input matrix back
         do i=1,n
            do j=ia(i),ia(i+1)-1
               k=ja(j)
               if (k.eq.i) then
c                 swap leading entry with the diagonal entry
                  l=ia(i)
                  ja(j)=ja(l)
                  ja(l)=k
                  dbuff =a(j)
                  a(j)=a(l)
                  a(l)=dbuff
               end if
            end do
         end do
c         write (IUNIT,'(A)') 'diagonal entries swapped'
c        restore old unshifted value

         if (.not.GEP) then
#if !defined _SINGLE_REAL_ && !defined _SINGLE_COMPLEX_
            if (flagsingle) then
c            write(6,'(A)') 'apply mixed restorediag driver'
               call SINGLESYMILUPACKRESTOREDIAG(param,n,ia,ja,a)
            else
c            write(6,'(A)') 'apply double restorediag driver'
               call SYMILUPACKRESTOREDIAG(param,n,ia,ja,a)
            end if
#else
c         write(6,'(A)') 'apply single restorediag driver'
            call SYMILUPACKRESTOREDIAG(param,n,ia,ja,a)
#endif
         end if
c         write (IUNIT,'(A)') 'diagonal part restored'
c         do i=1,n
c            write (IUNIT,'(10I8)') (ja(j),j=ia(i),ia(i+1)-1)
c            write (IUNIT,'(1P,10E8.1)') (a(j),j=ia(i),ia(i+1)-1)
c         end do


c        keep track on the true amount of memory that was consumed
         memused=mymem
c        if no adaptive preconditioning is desired, then return the
c        effective elbow space factor on exit

c         write (IUNIT,'(A)') 'multilevel ILU computed'

         if (ierr.eq.-1) then
c
c           wrong data
c
            write (IUNIT,'(A)') 'Error. input matrix may be wrong.'
         elseif (ierr.eq.-3 .or. ierr.eq.-2) then
c
c           out of memory
c
c           increase the drop tolerance significantly and hope
c           that afterwards it fits into the elbow space
            if (recompilu.eq.-1 .or. prvdr.eq.-1) then
c prvdr to avoid cycling (increase droptol, next decrease it)
               write (IUNIT,'(A)') 'Not enough memory: factorization sho
     +uld be restarted with larger DROPTOL'
            else
               call evaluatetime(toc)
               totaltime=toc-tic
               prvdr=1
               DROPTOL=DROPTOL*2*toldiv
               if (PR.ge.1) write (IUNIT,'(A,F10.2,A)') 
     +             '   ILU computed:',totaltime,'[sec]'
               write (IUNIT,'(A,1P,E8.1)') 'Not enough memory: factoriza
     +tion will be restarted with larger DROPTOL ',DROPTOL 
c
               factdgl=-1
               call PJDCLEANUP1 
               GOTO 3
            end if
         elseif (ierr.eq.-5) then
c
c           zero row encountered
c
            if (recompilu.eq.-1 .or. prvdr.eq.1) then
c prvdr to avoid cycling (increase droptol, next decrease it)
              write (IUNIT,'(A)')
     +              'REDUCE DROPTOL! (zero row encountered)'
            else
c              if there is still 20-40% memory left in the elbow space then
c              we try to re-factor the matrix with a smaller droptol
               if (memused.lt.slightlyless*memrequested) then
c                 if the elbow space is large enough, then even reduce
c                 the drop tolerance further
                  if (memused.lt.(3*slightlyless-2)*memrequested) then
                     DROPTOL=DROPTOL/(4*toldiv)
                  elseif (memused.lt.(2*slightlyless-1)*memrequested)
     +                    then
                     DROPTOL=DROPTOL/(2*toldiv)
                  else
                     DROPTOL=DROPTOL/toldiv
                  end if
                  DROPTOL=max(DROPTOL,1.01*SQRT(EPSMACH))
                  call evaluatetime(toc)
                  totaltime=toc-tic
                  prvdr=-1
                  if (PR.ge.1) write (IUNIT,'(A,F10.2,A)') 
     +                 '   ILU computed:',totaltime,'[sec]'
                  write (IUNIT,'(A,1P,E8.1)') 'zero row encountered: fac
     +torization will be restarted with smaller DROPTOL ',DROPTOL
c
                  factdgl=-1
                  call PJDCLEANUP1 
                  GOTO 3
               else 
                  write (IUNIT,'(A)')
     +         'reduce DROPTOL and increase MEM! (zero row encountered)'
               end if  
            end if
         elseif (ierr.eq.-6) then  
c
c           zero column encountered
c
            if (recompilu.eq.-1 .or. prvdr.eq.1) then
               write (IUNIT,'(A)')
     +         'reduce DROPTOL! (zero column encountered)'
            else
c              if there is still 20-40% memory left in the elbow space 
c              then we try to re-factor the matrix with a smaller droptol
               if (memused.lt.slightlyless*memrequested) then
c                 if the elbow space is large enough, then even reduce
c                 the drop tolerance further
                  if (memused.lt.(3*slightlyless-2)*memrequested) then
                     DROPTOL=DROPTOL/(4*toldiv)
                  elseif (memused.lt.(2*slightlyless-1)*memrequested)
     +                    then
                     DROPTOL=DROPTOL/(2*toldiv)
                  else
                     DROPTOL=DROPTOL/toldiv
                  end if
                  DROPTOL=max(DROPTOL,1.01*SQRT(EPSMACH))
                  call evaluatetime(toc)
                  totaltime=toc-tic
                  prvdr=-1
                  if (PR.ge.1) write (IUNIT,'(A,F10.2,A)') 
     +                 '   ILU computed:',totaltime,'[sec]'
                  write (IUNIT,'(A,1P,E8.1)') 'zero column encountered: 
     +factorization will be restarted with smaller DROPTOL ',DROPTOL
c
                  factdgl=-1
                  call PJDCLEANUP1
                  GOTO 3
               else 
                  write (IUNIT,'(A)') 'reduce DROPTOL and increase MEM! (
     +zero column encountered)'
               end if  
            end if
         elseif (ierr.gt.0) then  
c
c           zero pivot encountered
c
            if (recompilu.eq.-1 .or. prvdr.eq.1) then
               write (IUNIT,'(A,I3,A)')
     +      'reduce DROPTOL! (zero pivot encountered at step number',
     +      ierr,')'
            else
c              if there is still 20-40% memory left in the elbow space then
c              we try to re-factor the matrix with a smaller droptol
               if (memused.lt.slightlyless*memrequested) then
c                 if the elbow space is large enough, then even reduce
c                 the drop tolerance further
                  if (memused.lt.(3*slightlyless-2)*memrequested) then
                     DROPTOL=DROPTOL/(4*toldiv)
                  elseif (memused.lt.(2*slightlyless-1)*memrequested)
     +                    then
                     DROPTOL=DROPTOL/(2*toldiv)
                  else
                     DROPTOL=DROPTOL/toldiv
                  end if
                  DROPTOL=max(DROPTOL,1.01*SQRT(EPSMACH))
                  call evaluatetime(toc)
                  totaltime=toc-tic
                  prvdr=-1
                  if (PR.ge.1) write (IUNIT,'(A,F10.2,A)') 
     +                 '   ILU computed:',totaltime,'[sec]'
                  write (IUNIT,'(A,1P,E8.1)') 'zero pivot encountered: f
     +actorization will be restarted with smaller DROPTOL ',DROPTOL
c
                  factdgl=-1
                  call PJDCLEANUP1 
                  GOTO 3
               else 
                  write (IUNIT,'(A)') 'reduce DROPTOL and increase MEM! 
     +(zero pivot encountered)'
               end if  
            end if
         elseif (ierr.lt.0) then
c
c           just for safety
c
            write (IUNIT,'(A,I3,A)')
     +      'error code ', ierr,' detected. Please contact the authors.'
         endif
         if (ierr.ne.0) then 
c           give away the ILU preconditioner and the memory
            factdgl=-1
            CALL PJDCLEANUP_EV
            return
         end if
c

#if !defined _SINGLE_REAL_ && !defined _SINGLE_COMPLEX_
         if (flagsingle) then
c            write(6,'(A)') 'apply mixed nnz driver'
            i=SINGLESYMILUPACKNNZ(IPparam,IPPREC,IPnlev)
         else
c            write(6,'(A)') 'apply double nnz driver'
            i=SYMILUPACKNNZ(IPparam,IPPREC,IPnlev)
         end if
#else
c         write(6,'(A)') 'apply single nnz driver'
         i=SYMILUPACKNNZ(IPparam,IPPREC,IPnlev)
#endif

         fillfact=dble(i)/dble(ia(n+1)-1)
c           Just for fun: display multilevel information on the screen
         if (PR.ge.2) then
c        extendend information

#if !defined _SINGLE_REAL_ && !defined _SINGLE_COMPLEX_
            if (flagsingle) then
c               write(6,'(A)') 'apply mixed info driver'
               call SINGLESYMILUPACKINFO(IPparam,IPPREC,IPnlev,
     +                                   n,ia,ja,a)
            else
c               write(6,'(A)') 'apply double info driver'
               call SYMILUPACKINFO(IPparam,IPPREC,IPnlev,n,ia,ja,a)
            end if
#else
c            write(6,'(A)') 'apply single info driver'
            call SYMILUPACKINFO(IPparam,IPPREC,IPnlev,n,ia,ja,a)
#endif

            write (IUNIT,'(A)') 'Summary:'
         end if
         if (PR.ge.1) 
     +      write (IUNIT,'(A,F10.2,A,/,A,1P,E13.5,/,A,1P,E13.5)')
     +      ' fill-in factor:',fillfact,' (=nnz(LU)/nnz(A))',
     +      '   used droptol: ',DROPTOL,
     +      '     used shift: ',shift0
         if (PR.ge.1 .and. ISEARCH .eq.1 .and. ggsh.eq.1)
     +      write (IUNIT,'(A,A)') 
     +        ' (user shift was converted because larger',
     +         ' than the smallest diag. entry)'
         call evaluatetime(toc)
         totaltime=toc-tic
         if (PR.ge.1) write (IUNIT,'(A,F10.2,A)') 
     +            '   ILU computed:',totaltime,'[sec]'
         factspd=0
         if (ISEARCH.le.1) then
c           convert the preconditioner to an SPD preconditioner
c           pass to the user the information if there were relatively
c           large negative diagonal entries

#if !defined _SINGLE_REAL_ && !defined _SINGLE_COMPLEX_
            if (flagsingle) then
c               write(6,'(A)') 'apply mixed symspd driver'
               factspd=SINGLESYMSPDILUPACKCONVERT(IPparam,IPPREC,IPnlev)
            else
c               write(6,'(A)') 'apply double symspd driver'
               factspd=SYMSPDILUPACKCONVERT(IPparam,IPPREC,IPnlev)
            end if
#else
c            write(6,'(A)') 'apply single symspd driver'
            factspd=SYMSPDILUPACKCONVERT(IPparam,IPPREC,IPnlev)
#endif

            if (factspd.gt.0) then
c
c              negative diagonal entries
c
              if (recompilu.eq.-1 .or. factspd.le.n*nposeig) then             
               if (PR.ge.1) write (IUNIT,'(A,I10,A)') 
     +            'ILU-> pos. def.:',factspd,
     +            ' negative diag. entries changed to positive'
               else
                  shiftmax=min(shift0,shiftmax) 
                  SHIFT=2*SHIFT-diagmin
                  newsh=1
                  write (IUNIT,'(A)') 'Too many neg. diagonal entries:' 
                  write (IUNIT,'(A,1P,E12.4)')    'factorization will be
     + restarted with smaller shift ', SHIFT
c         smaller shift means less fill-in for same DROPTOL
                  prvdr=min(prvdr,0)
cmb               Is this necessary or should it be avoided?
                  factdgl=-1
                  call PJDCLEANUP1
                  GOTO 3
               end if
            else
c   set factspd to negative when the used SHIFT is potentially too small
               if (ISEARCH.le.0.or.newsh.eq.1.or.ggsh.eq.1) factspd=-1
            end if
         end if
c        some parameters will be saved to the common block structure for
c        subsequent use
         timefact=max(totaltime,1.0e-2)
         factdgl=-1
c        store the drop tolerance and norm of L^{-1} that were used 
c        finally in order to allow a proper re-factorization of the 
c        system during the JD execution
         droptol0=DROPTOL
         condest0=condest
         if (PR.ge.1) write (IUNIT,'(A)') 
     +     '--------------------------------------------------------'
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c        END SETUP ILUPACK
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
      END IF
*
      RETURN
      END
*
*=============================================================================*
*
*
*=============================================================================*
*
      SUBROUTINE PJDCLEANUP1
      implicit none
      DOUBLE PRECISION  timefact,shift0,droptol0,diagmin,shiftmax,
     +                  memrequested,memused,slightlyless,toldiv,
     +                  condest0
      INTEGER*8         IPparam,IPPREC,IPdiag
      integer           IPnlev,factdgl,factspd,IUNIT,PR,
     +                  prvdr
      logical           reenterfirsttime,flagsingle
      common/PJDINITPJD/timefact,shift0,droptol0,diagmin,shiftmax,
     +                  memrequested,memused,slightlyless,toldiv,
     +                  IPparam,IPPREC,IPdiag,IPnlev,factdgl,factspd,
     +                  prvdr,IUNIT,PR,condest0,reenterfirsttime,
     +                  flagsingle
      integer ilumem
      common/ILUPACKMEM/ilumem
      EXTERNAL      SYMILUPACKDELETE
#if !defined _SINGLE_REAL_ && !defined _SINGLE_COMPLEX_
      EXTERNAL      SINGLESYMILUPACKDELETE
#endif


c     prevent PJDCLEANUP1 from giving away memory if no preconditioner
c     has been built yet, i.e. there would be no memory to release
      if (ilumem.ne.-1) then
         if (factdgl.gt.0) then
            call DGLPRECDELETE(IPdiag)
         else if (factdgl.lt.0) then

#if !defined _SINGLE_REAL_ && !defined _SINGLE_COMPLEX_
            if (flagsingle) then
c               write(6,'(A)') 'apply mixed delete driver'
               call SINGLESYMILUPACKDELETE(IPparam,IPPREC,IPnlev,ilumem)
            else
c               write(6,'(A)') 'apply double delete driver'
               call SYMILUPACKDELETE(IPparam,IPPREC,IPnlev,ilumem)
            end if
#else
c            write(6,'(A)') 'apply single delete driver'
            call SYMILUPACKDELETE(IPparam,IPPREC,IPnlev,ilumem)
#endif

         end if
c        ensure that twice calling PJDCLEANUP1 is captured
         if (ilumem.eq.0) ilumem=-1
      end if
      factdgl=0
c
      return
      end
*
*
*=============================================================================*




*=============================================================================*
*
      SUBROUTINE JDRVCOM ( N, EIGS, RES, X, LX, NEIG, SIGMA,
     $                     ISEARCH, NINIT, MADSPACE, ITER, TOL, IJOB,
     $                     NDX1, NDX2, NDX3, IPRINT, INFO, GAP, GEP)

      implicit none  
*
*     .. Scalar Arguments ..
      integer            N, LX, NEIG, MADSPACE, ISEARCH, NINIT
      integer            ITER, IJOB, NDX1, NDX2, NDX3, IPRINT, INFO
      logical            GEP
      REALS              SIGMA, TOL, GAP
*     ..
*     .. Array Arguments ..
      FLOAT   X( * )
      REALS   EIGS( *), RES(*)
*     ..
*
*  Arguments
*  =========
* See comments in subroutine JDREVCOM
*
*     .. Parameters ..
      REALS   ZERO, ONE, HALF
      PARAMETER        ( ZERO = RZERO, ONE = RONE, HALF = RHALF )
      FLOAT   ZEROF, ONEF
      PARAMETER  ( ZEROF = FNULL)
      PARAMETER  ( ONEF = FONE )
*     ..
      logical flagcg
      DOUBLE PRECISION timeilu,rinit,rcurr,rcum,shift1
      common/JDRVCOMJD/timeilu,rinit,rcurr,rcum,shift1,flagcg
cmb   conflict of data statements between different arithmetics
c      DATA timeilu/0.0d0/
*     .. Local Scalars ..
      LOGICAL       FTFLG, CONV, UKNOWN, INITFLG, neweig
      integer       MVMAX, MIDSPACE, MODSPACE, NWANT, ITARG, ISERCH
      integer       XX, XCG, XCG2, WCG, VV, WW, UU, REIG, ITERP 
      integer       MM, NN, MP, NP, YY, YY2, WL, WL2, WL3, LWL
      integer       XLAST, KXP, ITCG, IFO, ISTEP, IUNIT, ISTAG, PR
      integer       BX, BU, BT, BV, KBXP, I, IJ, J, K, II, JJ
      REALS RESID, SCLR1, SCLR2, THETA, ETA, THMT
      REALS DELTA1, DELTAO, ETA0, NREIG, THETA2, ETA1, EPSMSR
      REALS NREIGP, TRS, RNDIND, GR, GL, GG, NRC, RINITM, NBU
      REALS ALPHA , GK, ALPRHO
      FLOAT AXPYALPHA, AXPYBETA
      REALS            LAMCH
      EXTERNAL         LAMCH
      REALS            EPSM,RINF
cmb   data statement uniformly moved to external definition
c      PARAMETER (EPSM=MY_OWN_EPS,RINF=MY_OWN_RINF)
      REAL tic,toc,totaltime
*     .. External Routines ..
      REALS NRM2
      FLOAT MYDDOT
      EXTERNAL      AXPY, COPY, RSCAL, NRM2
      EXTERNAL      SYR2K, SYRK, SYR20, SYR0
      EXTERNAL      evaluatetime, GEMM, GEMV
      EXTERNAL      SYMV, MYDDOT 
*
*     ..
*
*     indicates where to resume from. Only valid when IJOB /= 0!
      integer RLBL
      DATA RLBL/0/
*
*     saving all.
      SAVE
*     ..
*     .. Executable Statements ..
*

cmb   data statement uniformly moved to external definition
      EPSM=LAMCH('E')
      RINF=LAMCH('O')
      call COMMONJDRVCOMJDDEF

*     Entry point, so test IJOB
      IF (IJOB.EQ.0) GOTO 1
*        here we do resumption handling
         IF (RLBL .eq. 1) GOTO 5
         IF (RLBL .eq. 2) GOTO 225
         IF (RLBL .eq. 3) GOTO 37
         IF (RLBL .eq. 4) GOTO 38
         IF (RLBL .eq. 5) GOTO 255
         IF (RLBL .eq. 6) GOTO 55
         IF (RLBL .eq.11) GOTO 15
         IF (RLBL .eq.12) GOTO 45
*        if neither of these, then error
         INFO = -15
         GOTO 190
*
 1    CONTINUE
      call evaluatetime(tic)
      flagcg=.true.
      neweig=.true.
      rcum=ZERO
      RINITM=ONE
*
*     Test the input parameters.
*
      IUNIT=IPRINT
      PR=1
      IF (IUNIT.eq.0) THEN
        PR=0
        IUNIT=6
      ELSE IF (IUNIT.lt.0) THEN
        PR=2
        IUNIT=-IUNIT
      END IF
      IF (IUNIT.eq.5) IUNIT=6
      INFO=0
      IF ( N.LT.2 ) THEN
         INFO = -1
      ELSE IF ( NEIG.LE.0 ) THEN
         INFO = -5
      ELSE IF ( MADSPACE.LE.2 ) THEN
         INFO = -9
      ELSE IF (
     $   N*(2*MADSPACE+NEIG+4)+3*MADSPACE**2+MAX(MADSPACE**2,NEIG).GT.LX
     $            ) THEN
         INFO = -10
      ELSE IF ( GEP .AND. N*(3*MADSPACE+2*NEIG+4)
     $          +3*MADSPACE**2+MAX(MADSPACE**2,NEIG).GT.LX
     $            ) THEN
         INFO = -10
      ELSE IF (GEP .AND. ISEARCH.GE.2 .AND. 
     $          N*(3*MADSPACE+2*NEIG)+6*MADSPACE
     $          +3*MADSPACE**2+MAX(MADSPACE**2,NEIG).GT.LX) THEN
         INFO = -10
      ELSE IF ( ITER.LE.0 ) THEN
         INFO = -11
      ELSE IF ( TOL.LE.ZERO ) THEN
         INFO = -12
      ENDIF
      IF ( INFO.NE.0 ) GOTO 190
      MODSPACE=MIN(MADSPACE,N)
      NWANT=MIN(NEIG,N)
      MVMAX=ITER
      ISERCH=MAX(ISEARCH,0)
      ISERCH=MIN(ISEARCH,2)
*
*     workspace
*
      XX=1
      VV=XX+NWANT*N
      WW=VV+MODSPACE*N
      IF (GEP) THEN
      BX=WW+MODSPACE*N
      BV=BX+NWANT*N
      MM=BV+MODSPACE*N
      ELSE
      BX=XX
      BV=VV
      MM=WW+MODSPACE*N
      END IF
      NN=MM+MODSPACE*MODSPACE
      MP=NN+MODSPACE*MODSPACE
      NP=MP+MODSPACE*MODSPACE
      WCG=NP+MAX(MODSPACE*MODSPACE,NWANT)
      XLAST=WCG+4*N-1


      XCG2=WW+(MODSPACE-1)*N      
      BT=BV+(MODSPACE-1)*N      
      REIG=WCG
      YY=MP
      YY2=MP+MODSPACE

      WL=REIG+N
      LWL=LX-WL+1
      WL2=WL+MODSPACE
      WL3=WL2+MODSPACE

      UU=XX+(NWANT-1)*N
      BU=BX+(NWANT-1)*N
      KXP=VV
      KBXP=BV
*
*     initialization
      EPSMSR=SQRT(EPSM)
      NEIG=0
      ITER=0
      ITERP=0
*
      ISTEP=0
      ETA1=RINF
      ETA0=SIGMA
      DELTA1=RINF
      IF (ISERCH.EQ.0) ETA0=RINF
      NREIG=RINF
      RESID=RINF
      RNDIND=ZERO
*
      MIDSPACE = (MODSPACE+1)/2
      IF (NINIT.GT.MIDSPACE) THEN
        INITFLG=.FALSE.
        DO II=2,NINIT
           IF (EIGS(II).NE.EIGS(1)) INITFLG=.TRUE.
        END DO
        IF (INITFLG) THEN
          CALL SORTEV(N,NINIT,X(XX),N,ZEROF,N,EIGS,RES,X(VV),X(WW),
     $                ISERCH,SIGMA)
        ELSE
          DO II=1,NINIT/2
             JJ=NINIT-II+1
             CALL COPY (N,X(1+(JJ-1)*N),1,X(VV),1)
             CALL COPY (N,X(1+(II-1)*N),1,X(1+(JJ-1)*N),1)
             CALL COPY (N,X(VV),1,X(1+(II-1)*N),1)
          END DO
        END IF
      END IF
      INITFLG=.FALSE.
*
 2    CONTINUE
      IF (NINIT.LE.NEIG) THEN
         J=1
         CALL COPY(N, ONEF, 0, X(VV), 1)
      ELSE
         J=MIN(NINIT-NEIG,MIDSPACE)
         IJ=NINIT-NEIG-J
         IF (IJ.GT.0) INITFLG=.TRUE.
         CALL COPY (N*J, X(N*IJ+1), 1, X(VV), 1)
      END IF
*
      IJ=0
 4    CONTINUE
      IJ=IJ+1
      CALL REPGS(N,X(KXP),N,X(KBXP),N,NEIG+IJ-1,X(VV+(IJ-1)*N),X(WL),
     $           EPSM,1)
      IF (.NOT.GEP) THEN
           IF(NEIG+IJ-1.GT.0) THEN
             SCLR1=CTOD(X(WL+NEIG+IJ-1))
           ELSE
             SCLR1=NRM2(N,X(VV),1)
           END IF
        CALL RSCAL(N,ONE/SCLR1,X(VV+(IJ-1)*N),1)
      END IF
*     call matvec - return for revcom.
      NDX1=VV+(IJ-1)*N
      NDX2=WW+(IJ-1)*N
      NDX3=BV+(IJ-1)*N
      IJOB=1
      RLBL=1
      RETURN
 5    CONTINUE
      ITER=ITER+1
      IF (GEP) THEN
        SCLR1=CTOD(MYDDOT(N,X(VV+(IJ-1)*N),1,X(BV+(IJ-1)*N),1))
        IF (SCLR1.LE.ZERO) GOTO 196
        SCLR1=SQRT(SCLR1)
        CALL RSCAL(N,ONE/SCLR1,X(BV+(IJ-1)*N),1)
        CALL RSCAL(N,ONE/SCLR1,X(VV+(IJ-1)*N),1)
        CALL RSCAL(N,ONE/SCLR1,X(WW+(IJ-1)*N),1)
      END IF
*
      IF (IJ.LT.J) GOTO 4
*
      AXPYALPHA=-SIGMA
      IF (ISERCH.EQ.2) THEN 
         CALL AXPY(N*J,AXPYALPHA,X(BV),1,X(WW),1)
         CALL SYRK('U','C',J,N,ONE,X(WW),N, ZERO,X(NN),MODSPACE)
      END IF
      IF (ISERCH.EQ.2 .AND. GEP) THEN
         CALL GEMM('C','N',J,J,N,ONEF,X(WW),N,X(BV),N,
     $             ZEROF,X(MM),MODSPACE)
      ELSE
         CALL SYR2K('U','C',J,N,ONEF,X(VV),N,X(WW),N,
     $              ZERO,X(MM),MODSPACE)
         CALL RSCAL(MODSPACE*MODSPACE,HALF,X(MM),1)
      END IF
      UKNOWN=.FALSE.
      IF (J.LE.1) UKNOWN=.TRUE.
*
*  THE JD MAIN LOOP
*
 10   CONTINUE
*
*  EXTRACTION
*
        CALL COPY(MODSPACE*MODSPACE, X(MM), 1, X(MP),1)
        IF (ISERCH.EQ.2) CALL COPY(MODSPACE*MODSPACE,X(NN),1,X(NP),1)
        CALL SOLPROJEP(J,X(MP),X(NP),MODSPACE,THETA,THETA2,X(REIG),
     $                 LWL+N,ISERCH,INFO,GEP)
        IF (INFO.NE.0) THEN
           IF(ISERCH.EQ.2 .AND. GEP) GOTO 194
           IF(ISERCH.EQ.2) GOTO 191
           GOTO 192
        END IF

        IF (.NOT.UKNOWN) THEN
           IF (ISERCH.EQ.2) THEN
              SCLR1=NRM2(J,X(YY),1)
              CALL RSCAL(J,ONE/SCLR1,X(YY),1)
           END IF
           CALL GEMV('N',N,J,ONEF,X(VV),N,X(YY),1,ZEROF,X(UU),1)
           CALL GEMV('N',N,J,ONEF,X(WW),N,X(YY),1,ZEROF,X(REIG),1)
           IF (GEP) CALL GEMV('N',N,J,ONEF,X(BV),N,X(YY),1,
     +                        ZEROF,X(BU),1)
        ELSE
          X(YY)=ONEF
          CALL COPY(J-1,ZEROF,0,X(YY+1),1)
          CALL COPY(N,X(VV),1,X(UU),1)
          CALL COPY(N,X(WW),1,X(REIG),1)
          IF (GEP) CALL COPY(N,X(BV),1,X(BU),1)
       END IF
       IF (GEP) NBU=NRM2(N,X(BU),1)

        IF (ISERCH.EQ.2) THEN
          IF (GEP) THEN
           CALL GEMV('N',J,J,ONEF,X(MM),MODSPACE,X(YY),1,ZEROF,X(WL),1)
           THMT=CTOD(MYDDOT(J,X(WL),1,X(YY),1))/(NBU*NBU)
          ELSE
           CALL SYMV('U',J,ONEF,X(MM),MODSPACE,X(YY),1,ZEROF,X(WL),1)
           THMT=CTOD(MYDDOT(J,X(WL),1,X(YY),1))
          END IF
          THETA=THMT+SIGMA
        ELSE
           THMT=THETA
           ETA0=MIN(ETA0,THETA)
        END IF

        AXPYALPHA=-THMT
        CALL AXPY(N,AXPYALPHA,X(BU),1,X(REIG),1)
        NREIGP=NREIG
        NREIG=NRM2(N,X(REIG),1)
        IF (GEP) NREIG=NREIG/NBU
        if (neweig) then
           rinit=NREIG
           neweig=.false.
        end if
        IF (RABS(NREIG/NREIGP-ONE).LT.0.01) THEN
                 ISTAG=ISTAG+1
        ELSE
                 ISTAG=0
        END IF
* 
*       prevent stagnation because of rounding errors
        IF ( NREIG.GE.TOL .AND. ISTAG.LE.2 )            GOTO 20
        IF ( NREIG.GE.TOL .AND. RNDIND.LE.ONE )         GOTO 20
        IF ( NREIG.GE.TOL .AND. RESID.GT.0.1*NREIG )    GOTO 20
*
*       convergence detected 
*
         CALL REPGS(N,X(KXP),N,X(KBXP),N,NEIG,X(UU),X(WL),EPSM,0)
         If (NEIG.GT.0 .AND. X(WL+NEIG).EQ.ZEROF) GOTO 195
         IF (GEP) GOTO 14
           IF(NEIG.GT.0) THEN
             SCLR1=CTOD(X(WL+NEIG))
           ELSE
             SCLR1=NRM2(N,X(UU),1)
           END IF
           GOTO 16
 14      CONTINUE
*          call matvecb - return for revcom.
           NDX1=UU
           NDX3=BU
           IJOB=4
           RLBL=11
           RETURN
 15        CONTINUE
           SCLR1=CTOD(MYDDOT(N,X(UU),1,X(BU),1))
           IF (SCLR1.LE.ZERO) GOTO 196
           SCLR1=SQRT(SCLR1)
           CALL RSCAL(N,ONE/SCLR1,X(BU),1)
 16      CONTINUE
         CALL RSCAL(N,ONE/SCLR1,X(UU),1)
*
*  END OF EXTRACTION
*
 18   CONTINUE
*
*  CONVERGENCE DETECTED: (1) EXPAND THE PARTIAL SCHUR FORM
*
      
      NEIG=NEIG+1
      KXP=UU
      KBXP=BU
      UU=UU-N
      BU=BU-N
      EIGS(NWANT-NEIG+1)=THETA
      RES(NWANT-NEIG+1)=NREIG
      RINITM=RINITM*rinit
      IF(PR.GE.2) WRITE(IUNIT,902) ISTEP, NEIG, ITER, THETA, ITER-ITERP
      ITERP=ITER
      GAP=THETA2-THETA

      IF (NEIG.GE.NWANT) GOTO 200
      DELTA1=RINF
      NREIG=RINF
      RESID=RINF
      RNDIND=ZERO
*
*                        (2) UPDATE SUBSPACE FOR NEXT SEARCH
*
      IF (ISERCH.NE.2) THEN
        ETA1=MIN(ETA0,ETA1)
        DO II=1,NEIG
           ETA0=MAX(ETA0,EIGS(II))
        END DO
      END IF
*     complete restart if J=1 or if unexploited initial approximations
      IF (J.EQ.1 .OR. INITFLG) GOTO 2
*     otherwise use the already build subspace
      K=J
      J=MIN(MIDSPACE+1,J-1)
      J=MIN(J,MODSPACE-1)
      IF (ISERCH.EQ.2) THEN
       DO IJ=2,J+1
        CALL REPGS(K,X(YY),K,X(YY),K,1,X(MP+MODSPACE*(IJ-1)),
     +             X(WL),EPSM,1)
       END DO
      END IF

       CALL SHRINK(N,J,K,X(YY2),MODSPACE,X(NP),X(WL),LWL,
     $             X(VV),X(WW),X(BV),X(MM),X(NN),INFO,ISERCH,GEP)
       IF (INFO.NE.0) GOTO 193

*         ...  return to extraction - Ritz vector is known
      UKNOWN=.TRUE.
      neweig=.true.
      GOTO 10   
*
 20   CONTINUE
      IF(PR.GE.2) WRITE(IUNIT,901) ISTEP, NEIG+1, ITER, THETA, NREIG
      TRS=SQRT(ONE/10)
      IF (ISTAG.GT.0) TRS=ONE/10
      ISTEP=ISTEP+1
      IF (ITER.GE.MVMAX-NEIG-1) GOTO 197
*
*     SHRINK THE SEARCH SPACE IF NEEDED
*
      IF (J.GE.MODSPACE) THEN
       K=J
       J=MIDSPACE
       CALL SHRINK(N,J,K,X(MP),MODSPACE,X(NP),X(WL),LWL,
     $             X(VV),X(WW),X(BV),X(MM),X(NN),INFO,ISERCH,GEP)
       IF (INFO.NE.0) GOTO 193
          SCLR1=ZERO
          DO IJ=0,N-1
             IF(ABS(X(UU+IJ)).GT.SCLR1) THEN
                  SCLR1=ABS(X(UU+IJ))
                  JJ=IJ
             END IF
          END DO
          AXPYALPHA=X(UU+JJ)/X(VV+JJ)
          X(YY)=AXPYALPHA/ABS(AXPYALPHA)
          CALL COPY(J-1,ZEROF,0,X(YY+1),1)
      END IF
*
*     EXPANSION: SOLVING THE CORR. EQ. WITH PCG (CGREVCOM)
*
      ETA=ETA0
      IF (J.GT.1) THEN
         DELTAO=DELTA1
         IF (ISERCH.EQ.2) THEN
           IF (GEP) THEN
            THETA2=ONE/THETA2
           ELSE
            SCLR1=NRM2(J,X(YY2),1)
            CALL RSCAL(J,ONE/SCLR1,X(YY2),1)
            CALL SYMV('U',J,ONEF,X(MM),MODSPACE,X(YY2),1,ZEROF,X(WL),1)
            THETA2=CTOD(MYDDOT(J,X(YY2),1,X(WL),1))
           END IF
          DELTA1=RABS(THETA2)-RABS(THETA-SIGMA)
         ELSE
            DELTA1=THETA2-THETA
         END IF
         IF (NREIG.LT.RABS(DELTA1).AND.RABS(DELTA1/DELTAO-1).LT.0.1) 
     $         ETA=THETA
      END IF
      CALL REPGS(N,X(BU),N,X(UU),N,NEIG+1,X(REIG),X(WL),EPSM,-1)
      RNDIND=ABS(X(WL+NEIG)/(NREIG*EPSMSR))
      RESID=NREIG
      XCG=VV+J*N
      FTFLG = .TRUE.
      ITCG=MVMAX-ITER-NEIG-1
*
*     First time call always init.
*
      IJOB = 1
      IFO = 0
      IF (ISERCH.NE.2) IFO = 1
      GK=RESID
      rcurr=NREIG
      if (NEIG.le.0) then
          shift1=THETA
        if (J.gt.1) then
          GAP=THETA2-THETA
        else
          GAP=ZERO
        end if
      end if
      IF(GEP) AXPYBETA=MYDDOT(N,X(BU),1,X(REIG),1)/(NBU*NBU)  
 30   CONTINUE
      CALL LSOLREVCOM( N, X(XCG), X(WCG), N, X(XCG2), X(BT), ITCG, 
     $                 IFO, NDX1, NDX2, NDX3, GK, ALPHA, IJOB, GEP)

*     On a return from CGREVCOM() we use IJOB to figure out what is reqd.
*
      GOTO (31,32,33,34) IJOB

*        IJOB=1: call matvec - return for revcom.
 31      RLBL=3 
         NDX1=NDX1+WCG-1
         NDX2=NDX2+WCG-1
         NDX3=NDX3+WCG-1
         RETURN
 37      IJOB = 2
         AXPYALPHA=-ETA
         IF (GEP) THEN
           CALL AXPY(N,AXPYALPHA,X(NDX3),1,X(NDX2),1)
         ELSE
           CALL AXPY(N,AXPYALPHA,X(NDX1),1,X(NDX2),1)
         END IF
         IF (GEP) AXPYALPHA=MYDDOT(N,X(BU),1,X(NDX2),1)/(NBU*NBU)
         CALL REPGS(N,X(BU),N,X(UU),N,1,X(NDX2),X(NP),EPSM,-1)
         ALPRHO=CTOD(X(NP))
         ITER=ITER+1
         GOTO 30

*        IJOB=2: call solve - return for revcom.
 32      RLBL=4
         NDX1=NDX1+WCG-1
         NDX2=NDX2+WCG-1
         RETURN
 38      IJOB = 2
c        after we have applied preconditioning for the first
c        time we are not allowed to change the preconditioner
c        anymore
         flagcg=.true.
         CALL REPGS(N,X(UU),N,X(BU),N,NEIG+1,X(NDX1),X(NP),EPSM,-1)
         GOTO 30

 33      CONTINUE
*        IJOB=3: do stopping test
*        if first time, set IFO=-1 of IFO=-2.
         NDX1=NDX1+WCG-1
         IF( FTFLG ) IFO = -1
         IF( FTFLG .AND. ISERCH.EQ.2) IFO=-2
         AXPYALPHA=AXPYALPHA*ALPHA
         ALPRHO=ALPRHO*ALPHA
         CALL STOPTEST( N,X(XCG),X(BT),X(BU),TOL,RESID,NBU,ALPRHO,GK,
     $                  AXPYALPHA,AXPYBETA,THETA,ETA,IFO,TRS,GEP)
         FTFLG = .FALSE.
         IJOB = 2
         GOTO 30

*        IJOB=4: revcom wants to terminate, so do it.
 34      CONTINUE  
         IF (ISERCH.NE.2.AND.ETA.LE.ETA0.AND.NEIG.GT.0.AND.IFO.EQ.2)
     $      ETA0=MIN(ETA1,ETA0)
*
*     END OF PCG
*
c 
      CONV=.FALSE.
      flagcg=.false.
      rcum=rcum+log(NREIG/RESID)
      IF (RESID.GE.TOL) GOTO 50
*
*        convergence reached during PCG
*
         CALL AXPY(N,-ONEF,X(XCG),1,X(UU),1)
         CALL REPGS(N,X(KXP),N,X(KBXP),N,NEIG,X(UU),X(NP),EPSM,0)
         If (NEIG.GT.0 .AND. X(NP+NEIG).EQ.ZEROF) GOTO 195
         IF (GEP) GOTO 44
           IF(NEIG.GT.0) THEN
             SCLR1=CTOD(X(NP+NEIG))
           ELSE
             SCLR1=NRM2(N,X(UU),1)
           END IF
           GOTO 46
 44      CONTINUE
*          call matvecb - return for revcom.
           NDX1=UU
           NDX3=BU
           IJOB=4
           RLBL=12
           RETURN
 45        CONTINUE
           SCLR1=CTOD(MYDDOT(N,X(UU),1,X(BU),1))
           IF (SCLR1.LE.ZERO) GOTO 196
           SCLR1=SQRT(SCLR1)
           CALL RSCAL(N,ONE/SCLR1,X(BU),1)
 46      CONTINUE
         CALL RSCAL(N,ONE/SCLR1,X(UU),1)
           THETA=THETA/SCLR1+ETA
           CONV=.TRUE.
           NREIG=RESID
           IF (NEIG.GE.NWANT-1) GOTO 18
*
*     EXPAND THE SUBSPACE WITH THE COMPUTED SOLUTION
*          
 50   CONTINUE
      CALL REPGS(N,X(KXP),N,X(KBXP),N,NEIG+J,X(XCG),X(REIG),EPSM,1)
      CALL AXPY(J,-ONEF,X(REIG+NEIG),1,X(YY),1)
      IF (.NOT.GEP) THEN
        SCLR1=X(REIG+NEIG+J)
        CALL RSCAL(N,ONE/SCLR1,X(XCG),1)
      END IF
*          call matvec - return for revcom.
           NDX1=XCG
           NDX2=WW+J*N
           NDX3=BV+J*N
           IJOB=1
           RLBL=6
           RETURN
 55        CONTINUE
           ITER=ITER+1
      IF (GEP) THEN
        SCLR1=CTOD(MYDDOT(N,X(XCG),1,X(BV+J*N),1))
        IF (SCLR1.LE.ZERO) GOTO 196
        SCLR1=SQRT(SCLR1)
        CALL RSCAL(N,ONE/SCLR1,X(XCG),1)
        CALL RSCAL(N,ONE/SCLR1,X(WW+J*N),1)
        CALL RSCAL(N,ONE/SCLR1,X(BV+J*N),1)
      END IF
      X(YY+J)=-SCLR1
           AXPYALPHA=-SIGMA
*      rmk: NDX3=VV+J*N=XCG if .not.GEP
       IF (ISERCH.EQ.2) CALL AXPY(N,AXPYALPHA,X(NDX3),1,X(NDX2),1)
       IF (ISERCH.EQ.2 .AND. GEP) THEN
         CALL GEMV('C',N,J+1,ONEF,X(WW),N,X(NDX3),1,
     $             ZEROF,X(MM+J*MODSPACE),1)
         CALL GEMV('C',N,J,ONEF,X(BV),N,X(NDX2),1,
     $             ZEROF,X(MM+J),MODSPACE)
#ifdef _DOUBLE_REAL_
         GOTO 57
#elif defined _SINGLE_REAL_
         GOTO 57
#else 
         DO 56 IJ=J,J+(J-1)*MODSPACE,MODSPACE
 56         X(MM+IJ)=CONJ(X(MM+IJ))
#endif
 57      CONTINUE
       ELSE
         CALL GEMV('C',N,J+1,ONEF,X(VV),N,X(NDX2),1,
     $             ZEROF,X(MM+J*MODSPACE),1)
       END IF
       IF (ISERCH.EQ.2) CALL GEMV('C',N,J+1,ONEF,X(WW),N,X(NDX2),
     $                              1,ZEROF,X(NN+J*MODSPACE),1)
       J=J+1
       UKNOWN=.FALSE.
*      goto extraction if not converged yet
       IF (.NOT.CONV) GOTO 10
*
*     convergence detected:  prepare for restart
*
        IF ( ISERCH.NE.2 .OR. (.NOT.GEP)) THEN
          SCLR1=NRM2(J,X(YY),1)
          CALL RSCAL(J,ONE/SCLR1,X(YY),1)
        END IF
        IF (ISERCH.EQ.2) THEN
           CALL SYMV('U',J,ONEF,X(NN),MODSPACE,X(YY),1,ZEROF,X(WL2),1)
           SCLR1=CTOD(MYDDOT(J,X(YY),1,X(WL2),1))
          IF (GEP) THEN
            CALL GEMV('N',J,J,ONEF,X(MM),MODSPACE,X(YY),1,
     $                ZEROF,X(WL),1)
            AXPYALPHA=MYDDOT(J,X(YY),1,X(WL),1)
            THMT=CTOD(AXPYALPHA)/(ALPRHO*NBU*NBU)
            AXPYALPHA=-AXPYALPHA/SCLR1
            CALL AXPY(J,AXPYALPHA,X(WL2),1,X(WL),1)
            CALL GEMV('C',J,J,ONEF,X(MM),MODSPACE,X(YY),1,
     $                ZEROF,X(WL3),1)
            CALL COPY(MODSPACE*MODSPACE, X(MM), 1, X(MP),1)
            CALL GERK(J,J,-ONE/SCLR1,X(WL),1,X(WL2),1,X(MP),MODSPACE)
            CALL GERK(J,J,-ONE/SCLR1,X(WL2),1,X(WL3),1,X(MP),MODSPACE)
          ELSE
           CALL SYMV('U',J,ONEF,X(MM),MODSPACE,X(YY),1,ZEROF,X(WL),1)
           THMT=CTOD(MYDDOT(J,X(YY),1,X(WL),1))
           AXPYALPHA=-THMT/(2*SCLR1)
           CALL AXPY(J,AXPYALPHA,X(WL2),1,X(WL),1)
           AXPYALPHA=-ONEF/SCLR1
           CALL COPY(MODSPACE*MODSPACE, X(MM), 1, X(MP),1)
           CALL SYR20('U',J,AXPYALPHA,X(WL),1,X(WL2),1,X(MP),MODSPACE)
          END IF
          THETA=THMT+SIGMA
          CALL COPY(MODSPACE*MODSPACE, X(NN), 1, X(NP),1)
        ELSE
           CALL SYMV('U',J,ONEF,X(MM),MODSPACE,X(YY),1,ZEROF,X(WL),1)
           THETA=CTOD(MYDDOT(J,X(YY),1,X(WL),1))
           ETA0=MIN(ETA0,THETA)
           SCLR1=NRM2(J,X(MP),MODSPACE+1)
           IF (THETA.LT.0) SCLR1=SCLR1-2*THETA
           AXPYALPHA=SCLR1
           CALL AXPY(J,AXPYALPHA,X(YY),1,X(WL),1)
           CALL COPY(J,X(YY),1,X(WL2),1)
           CALL COPY(MODSPACE*MODSPACE, X(MM), 1, X(MP),1)
           CALL SYR20('U',J,-ONEF,X(WL2),1,X(WL),1,X(MP),MODSPACE)
        END IF
        CALL SOLPROJEP(J,X(MP),X(NP),MODSPACE,SCLR1,SCLR2,X(REIG),
     $                 LWL+N,-ISERCH,INFO,GEP)
        IF (INFO.NE.0) THEN
           IF(ISERCH.EQ.2 .AND. GEP) GOTO 194
           IF(ISERCH.EQ.2) GOTO 191
           GOTO 192
        END IF
        IF (ISERCH.EQ.2) THEN
          SCLR1=NRM2(J,X(YY),1)
          CALL RSCAL(J,ONE/SCLR1,X(YY),1)
        END IF
        GOTO 18
*
*     HERE TERMINATES THE JD LOOP
*
*    ... some error occured
 190  WRITE(IUNIT,900) -INFO
      GOTO 301
 191  IF (INFO.GT.0) THEN
        WRITE(IUNIT,*) ' Error in LAPACK routine (D)SYGV/(Z)HEGV'
        WRITE(IUNIT,*) ' Either BUG or error in calling sequence or',
     $          ' MODSPACE too large'
        INFO=-INFO
      ELSE
        WRITE(IUNIT,*) ' Error in LAPACK routine (D)SYGV/(Z)HEGV ',
     $          ' - BUG or error in calling sequence'
      END IF
      GOTO 198
 192  IF (INFO.GT.0) THEN
        WRITE(IUNIT,*) ' Error in LAPACK routine (D)SYEV/(Z)HEEV'
        WRITE(IUNIT,*)' Either BUG or error in calling sequence or',
     $          ' MODSPACE too large'
        INFO=-INFO
      ELSE
        WRITE(IUNIT, *) ' Error in LAPACK routine (D)SYEV/(Z)HEEV ',
     $          ' - BUG or error in calling sequence'
      END IF
      GOTO 198
 193  CONTINUE
      IF (INFO.GT.0) THEN
        WRITE(IUNIT, *) ' Error in LAPACK routine (D)GEQRF',
     $          ' - BUG or error in calling sequence'
        INFO=-INFO
      ELSE
        WRITE(IUNIT,*) ' Error in LAPACK routine (D)ORMQR',
     $          ' - BUG or error in calling sequence'
      END IF
      GOTO 198
 194  IF (INFO.GT.0) THEN
        WRITE(IUNIT,*) ' Error in LAPACK routine (D)POSV or (D)GEEV'
        WRITE(IUNIT,*) ' Either BUG or error in calling sequence or',
     $          ' MODSPACE too large'
        INFO=-INFO
      ELSE
        WRITE(IUNIT,*) ' Error in LAPACK routine (D)POSV or (D)GEEV',
     $          ' - BUG or error in calling sequence'
      END IF
      GOTO 198
 195  CONTINUE
      WRITE(IUNIT, *) 
     $          ' Error: some orthogonalization failed unexpectedly'
      WRITE(IUNIT, *) 
     $          ' BUG or error in calling sequence'
      INFO=-54320
      GOTO 198 
 196  CONTINUE
      WRITE(IUNIT, *) 
     $          ' Error: the mass matrix B is not positive definite'
      INFO=-54322
      GOTO 198 
 197  IF (PR.GE.1) WRITE(IUNIT,'()')
      WRITE(IUNIT, *)
     $' Max number of MATVEC reached without detecting all wanted eigs;'
      IF (PR.GE.1) WRITE(IUNIT, *)
     $'     INFO set equal to the index of the last computed eig.'      
      IF (PR.GE.1) WRITE(IUNIT, *)
     $'     X(1+N*(INFO-1):N*INFO), EIGS(INFO), RES(INFO)',
     $' set accordingly'      
      INFO=NEIG+1
      RINITM=RINITM*rinit
 198  CONTINUE
      IF (NEIG.GT.0) CALL COPY(N,X(UU),1,X(VV),1)
      IF (NEIG.GT.0 .AND. GEP) CALL COPY(N,X(BU),1,X(BV),1)
*
 200  CONTINUE
*    ... normal termination
*        first put the approx eigenvectors at the top of X
*
      IF (PR.GE.1) WRITE(IUNIT,904)
      IF (NEIG.LT.NWANT) THEN
        rinit=RINITM**(1.0/dble(NEIG+1))
        IF (NEIG.GT.0) THEN
         JJ=MIN(NEIG,NWANT-NEIG)
         CALL COPY(N*JJ,X(N*(NWANT-JJ)+XX),1,X(XX),1)
         CALL COPY(JJ,EIGS(NWANT-JJ+1),1,EIGS,1)
         CALL COPY(JJ,RES(NWANT-JJ+1),1,RES,1)
         CALL COPY(N,X(VV),1,X(N*(NEIG)+XX),1)
         IF (GEP) THEN
           CALL COPY(N*JJ,X(N*(NWANT-JJ)+BX),1,X(BX),1)
           CALL COPY(N,X(BV),1,X(N*(NEIG)+BX),1)
         END IF
         EIGS(NEIG+1)=THETA
         RES(NEIG+1)=NREIG
         THETA2=THETA
        ELSE
         CALL COPY(N,X(UU),1,X(XX),1)
         IF(GEP) CALL COPY(N,X(BU),1,X(BX),1)
         EIGS(1)=THETA
         RES(1)=NREIG
         GAP=ZERO 
         GOTO 300
        END IF
      ELSE
        rinit=RINITM**(1.0/dble(NEIG))        
      END IF
*        next sort them in ascending order
      IJ=4
      IF (GEP) IJ=-4
      CALL SORTEV(N,NEIG,X(XX),N,X(BX),N,EIGS,RES,X(VV),X(WW),IJ,ZERO)
*       find where is the target
       IF(ISERCH.GE.2) THEN
         DO ITARG=1,NEIG
           IF(EIGS(ITARG).gt.SIGMA) GOTO 205
         END DO
 205     CONTINUE
      ELSE
         ITARG=0
      END IF
*        estimate distance to the remaining part of the spectrum
       IF (ISERCH.EQ.2) THEN
        GAP=RABS(THETA2-SIGMA)-
     $          MAX(RABS(EIGS(NEIG)-SIGMA),RABS(EIGS(1)-SIGMA))
       ELSE
        GAP=THETA2-EIGS(NEIG)
       END IF
*       estimate errors and print output information
      IF (ISERCH.EQ.2) THEN 
         GG=EIGS(1)+EIGS(NEIG)-2*SIGMA
      ELSE
         GG=RINF
      END IF
      IF (GG.GT.ZERO) THEN
         II=1
         JJ=NEIG-1
      ELSE
         II=2
         JJ=NEIG
      END IF
      IF (NEIG.EQ.1) THEN
        II=0
        JJ=0
      END IF
      I=1
      K=0
*
 210  CONTINUE
      IF (I.LT.II .OR. I.GE.JJ) GOTO 250
      IF (I.EQ.1) THEN
        GL=GG-RES(NEIG)
      ELSE
        GL=EIGS(I)-EIGS(I-1)-RES(I-1)
      END IF
      IF (GL.LT.10*(EIGS(I+1)-EIGS(I))) GOTO 250
      J=MIN(I+MODSPACE,JJ+1)
*
 215  J=J-1
      IF (J.EQ.I) GOTO 250
      IF (J.EQ.NEIG) THEN
        GR=-GG-RES(1)
      ELSE
        GR=EIGS(J+1)-EIGS(J)-RES(J+1)
      END IF
      IF (10*(EIGS(J)-EIGS(I)).GT.MIN(GL,GR)) GOTO 215
*
*     cluster detected - perform additional Rayleigh-Ritz
*
      K=K+1
      J=J-I+1
      CALL COPY(N*J,X(N*(I-1)+XX),1,X(VV),1)
      IJ=0
 220  CONTINUE
      IJ=IJ+1
*     call matvec - return for revcom.
      NDX1=VV+(IJ-1)*N
      NDX2=WW+(IJ-1)*N
      IJOB=3
      RLBL=2
      RETURN
 225  CONTINUE
      ITER=ITER+1
      IF (IJ.LT.J) GOTO 220
*
      CALL SYR2K('U','C',J,N,ONEF,X(VV),N,X(WW),N,ZERO,X(MM),MODSPACE)
      CALL RSCAL(MODSPACE*MODSPACE,HALF,X(MM),1)
      CALL SOLPROJEP(J,X(MM),X(NP),MODSPACE,SCLR1,SCLR2,X(REIG),
     $               LWL+N,3,IJ,GEP)
      IF (IJ.NE.0) GOTO 250
      CALL GEMM('N','N',N,J,J,ONEF,X(VV),N,X(MM),MODSPACE,
     $           ZEROF,X(N*(I-1)+XX),N)
      CALL GEMM('N','N',N,J,J,ONEF,X(WW),N,X(MM),MODSPACE,
     $           ZEROF,X(VV),N)
      IF (GEP) THEN
           CALL COPY(N*J,X(N*(I-1)+BX),1,X(BV),1)
           CALL GEMM('N','N',N,J,J,ONEF,X(BV),N,X(MM),MODSPACE,
     $               ZEROF,X(N*(I-1)+BX),N)
      END IF
      NRC=ZERO
      GL=GL-EIGS(I)
      GR=GR+EIGS(I+J-1)
      DO IJ=1,J
        THETA=CTOD(X(NP+IJ-1))
        EIGS(I+IJ-1)=THETA
        AXPYALPHA=-THETA
        CALL AXPY(N,AXPYALPHA,X(N*(I+IJ-2)+BX),1,X(VV+N*(IJ-1)),1)
        NREIG=NRM2(N,X(VV+N*(IJ-1)),1)
        IF (GEP) THEN
          NBU=NRM2(N,X(N*(I+IJ-2)+BX),1)
          NREIG=NREIG/NBU
        END IF
        RES(I+IJ-1)=NREIG
        NRC=NRC+NREIG*NREIG
      END DO
      DO IJ=I,I+J-1
        SCLR1=MIN(EIGS(IJ)+GL,-EIGS(IJ)+GR)
        IF (SCLR1.GT.ZERO) THEN
           SCLR1=MIN(NRC/SCLR1,RES(IJ))
        ELSE
           SCLR1=RES(IJ)
        END IF
        IF (IJ.EQ.ITARG) ITARG=ITARG-1
        IF (PR.GE.1) WRITE(IUNIT,905) EIGS(IJ),RES(IJ),SCLR1,IJ-ITARG,K
      END DO
      I=I+J
      IF (I.GT.NEIG) GOTO 300
      GOTO 210
*
 250  CONTINUE
*     isolated eigenvalue - compute true residual
*
*     call matvec - return for revcom.
      NDX1=N*(I-1)+1
      NDX2=VV
      IJOB=3
      RLBL=5
      RETURN
 255  CONTINUE
      ITER=ITER+1
      THETA=CTOD(MYDDOT(N,X(N*(I-1)+XX),1,X(VV),1))
      AXPYALPHA=-THETA
      CALL AXPY(N,AXPYALPHA,X(N*(I-1)+BX),1,X(VV),1)
      NREIG=NRM2(N,X(VV),1)
        IF (GEP) THEN
          NBU=NRM2(N,X(N*(I-1)+BX),1)
          NREIG=NREIG/NBU
        END IF
      EIGS(I)=THETA
      RES(I)=NREIG
      INITFLG=.FALSE.
      IF (I.EQ.1) THEN
        GL=GG-RES(NEIG)
        IF (I.NE.II) THEN
          GL=GAP
          INITFLG=.TRUE.
        END IF
      ELSE
        GL=EIGS(I)-EIGS(I-1)-RES(I-1)
      END IF
      IF (I.EQ.NEIG) THEN
        GR=-GG-RES(1)
        IF (I.NE.JJ) THEN
          GR=GAP
          INITFLG=.TRUE.
        END IF
      ELSE
        GR=EIGS(I+1)-EIGS(I)-RES(I+1)
      END IF
      SCLR1=MIN(GR,GL)
        IF (SCLR1.GT.ZERO) THEN
           SCLR1=MIN(NREIG*NREIG/SCLR1,RES(I))
        ELSE
           SCLR1=RES(I)
           INITFLG=.FALSE.
        END IF
        IF (I.EQ.ITARG) ITARG=ITARG-1
      IF (INITFLG) THEN
        IF (PR.GE.1) WRITE(IUNIT,907) EIGS(I),RES(I),SCLR1,I-ITARG
      ELSE
        IF (PR.GE.1) WRITE(IUNIT,906) EIGS(I),RES(I),SCLR1,I-ITARG
      END IF
*
      I=I+1
      IF (I.LE.NEIG) GOTO 210
 300  CONTINUE
      IF (INFO.GT.0 .AND. PR.GE.1) WRITE(IUNIT,908) EIGS(INFO),RES(INFO)
      IF (NEIG.GT.0 .AND. PR.GE.1) WRITE(IUNIT,903)
      IF (PR.GE.1) WRITE(IUNIT,909) ITER, GAP, INFO
*
*     job done - return
      call evaluatetime(toc)
      IF (PR.GE.1) WRITE (IUNIT,'(A,F10.2,A)') 
     $  'CPU time for eigenvalue computation:',toc-tic-timeilu,'[sec]'
 301  IJOB=0
      RETURN
 900  FORMAT(' Error in the input parameters:',i3,
     $'th argument has an illegal value')
 901  FORMAT('Step:',i4,'(',i3,') ;  #MV:',i7,' ;  Rayl. quot.:',1pe12.5
     $        ,' ;  Res. norm:',1pe8.1)
 902  FORMAT('Step:',i4,'(',i3,') ;  #MV:',i7,' ;  Rayl. quot.:',1pe12.5
     $        ,' * converged (',i4,' MV)',/)
 903  FORMAT(/,
     $' (Warning: error estimates are subject to several limitations,'
     $,/,'  see documentation for details;', 
     $' for (*), error estimate is indicative only)')
 904  FORMAT(/,
     $'            Eigenvalue     Res. norm    Error est.   No(Cluster)'
     $)
#ifdef _SINGLE_REAL_
 905  FORMAT(1pe22.6,1pe14.1,1pe14.1,i9,'(',i3,')')
 906  FORMAT(1pe22.6,1pe14.1,1pe14.1,i9,'(---)')
 907  FORMAT(1pe22.6,1pe14.1,1pe14.1,i9,'(---) (*)')
 908  FORMAT(1pe22.6,1pe14.1,'    * Unconverged eig. *')
#elif defined _SINGLE_COMPLEX_
 905  FORMAT(1pe22.6,1pe14.1,1pe14.1,i9,'(',i3,')')
 906  FORMAT(1pe22.6,1pe14.1,1pe14.1,i9,'(---)')
 907  FORMAT(1pe22.6,1pe14.1,1pe14.1,i9,'(---) (*)')
 908  FORMAT(1pe22.6,1pe14.1,'    * Unconverged eig. *')
#else 
 905  FORMAT(1pe22.13,1pe14.1,1pe14.1,i9,'(',i3,')')
 906  FORMAT(1pe22.13,1pe14.1,1pe14.1,i9,'(---)')
 907  FORMAT(1pe22.13,1pe14.1,1pe14.1,i9,'(---) (*)')
 908  FORMAT(1pe22.13,1pe14.1,'    * Unconverged eig. *')
#endif
 909  FORMAT (/,'  Number of MATVEC needed:',i7,
     $          ' ;  Estimated gap:',1pe9.2,' ;  INFO:',i3,/)
      END
*=============================================================================*
*
      SUBROUTINE SHRINK(N,J,K,U,LDU,ZZ,WL,LWL,V,W,BV,MM,NN,INFO,
     +                  ISERCH,GEP)
      implicit none
      integer N, J, K, LDU, LWL, INFO, ISERCH, II, JJ
      FLOAT U(*), ZZ(*), WL(*), V(*), W(*), BV(*)
      FLOAT MM(LDU,*), NN(LDU,*) 
      logical GEP
*     .. Parameters ..
      REALS   ZERO, ONE
      PARAMETER        ( ZERO = RZERO, ONE = RONE )
      EXTERNAL GEQRF, ORMQR

       CALL GEQRF(K,J,U,LDU,ZZ,WL,LWL,INFO)
       IF (INFO.LT.0) THEN
          INFO=-INFO
          RETURN
       END IF
       CALL ORMQR('R','N',N,K,J,U,LDU,ZZ,V,N,WL,LWL,INFO)
       IF (INFO.LT.0) RETURN
       CALL ORMQR('R','N',N,K,J,U,LDU,ZZ,W,N,WL,LWL,INFO)
       IF (INFO.LT.0) RETURN
       IF (GEP) CALL ORMQR('R','N',N,K,J,U,LDU,ZZ,BV,N,WL,LWL,INFO)
       IF (INFO.LT.0) RETURN

       IF (ISERCH.NE.2 .OR. (.NOT.GEP)) THEN
        DO  10 II=2,K
         DO 10 JJ=1,II-1
 10         MM(II,JJ)=CONJ(MM(JJ,II))
       END IF
       CALL ORMQR('R','N',K,K,J,U,LDU,ZZ,MM,LDU,WL,LWL,INFO)
       IF (INFO.LT.0) RETURN
       CALL ORMQR('L',TRANS,K,K,J,U,LDU,ZZ,MM,LDU,WL,LWL,INFO)
       IF (INFO.LT.0) RETURN

       IF (ISERCH.EQ.2) THEN
         DO  20 II=2,K
           DO 20 JJ=1,II-1
 20           NN(II,JJ)=CONJ(NN(JJ,II))
         CALL ORMQR('R','N',K,K,J,U,LDU,ZZ,NN,LDU,WL,LWL,INFO)
         IF (INFO.LT.0) RETURN
         CALL ORMQR('L',TRANS,K,K,J,U,LDU,ZZ,NN,LDU,WL,LWL,INFO)
         IF (INFO.LT.0) RETURN
       END IF
      RETURN
      END
*=============================================================================*
      SUBROUTINE SORTEV(N,J,V,LDV,V2,LDV2,EIGS,R,W,IW,IT,SIGMA)
      implicit none
      integer           N,J,LDV,LDV2,IW(J),IT
      FLOAT  V(LDV,J), V2(LDV2,J+1)
      REALS  EIGS(J), R(J), W(*), SIGMA
c ! W/IW also used as float workspace !
c ! This causes trouble when using single precision but 64 bit integer !

      integer I, IS, IC, IR
      REALS W1,R1

      IF (J.LE.1) RETURN

      IF (IT.EQ.2.OR.IT.EQ.-2) THEN
         DO I=1,J
           W(I)=-RABS(EIGS(I)-SIGMA)
         END DO
      ELSEIF (IT.EQ.4.OR.IT.EQ.-4) THEN
         DO I=1,J
           W(I)=EIGS(I)
         END DO
      ELSE
         DO I=1,J
           W(I)=-EIGS(I)
         END DO
      END IF
         
      CALL SORTRX(J,W,IW)

      IF (IT.EQ.-2) THEN 
        IS=IW(J)
        DO I=J-1,1,-1
         IW(I+1)=IW(I)
        END DO
        IW(1)=IS
      END IF

      IC=0
 10   CONTINUE
      IS=0
 11   CONTINUE
      IS=IS+1
      IF(IW(IS).LT.0) GOTO 11
      IF(IW(IS).EQ.IS) THEN
         IW(IS)=-IW(IS)
         IC=IC+1
         IF (IC.GE.J) RETURN
         GOTO 11
      END IF
      IR=IW(IS)

      CALL COPY(N,V(1,IS),1,W,1)
      IF (IT.EQ.-4) CALL COPY(N,V2(1,IS),1,V2(1,J+1),1)
      IF (IT.EQ.4.OR.IT.EQ.-4) R1=R(IS)
      W1=EIGS(IS)

 15   CONTINUE
      CALL COPY(N,V(1,IR),1,V(1,IS),1)
      IF (IT.EQ.-4) CALL COPY(N,V2(1,IR),1,V2(1,IS),1)
      IF (IT.EQ.4.OR.IT.EQ.-4) R(IS)=R(IR)
      EIGS(IS)=EIGS(IR)
      IW(IS)=-IR
      IC=IC+1
      IS=IR
      IR=IW(IS)
      IF (IW(IR).GT.0) GOTO 15

      CALL COPY(N,W,1,V(1,IS),1)
      IF (IT.EQ.-4) CALL COPY(N,V2(1,J+1),1,V2(1,IS),1)
      IF (IT.EQ.4.OR.IT.EQ.-4) R(IS)=R1
      EIGS(IS)=W1
      IW(IS)=-IR
      IC=IC+1
      IF (IC.GE.J) RETURN
      GOTO 10
      
      END         
*=============================================================================*
      SUBROUTINE STOPTEST( N,X,BT,BU,TOL,RK,NBU,ALPRHO,GK,PCAD,PCR,
     $                     THETA,ETA,INFO,TRS,GEP)
      implicit none
*
*     .. Scalar Arguments ..
      integer            N, INFO
      REALS   TOL, RK, NBU, GK, ALPRHO, THETA, ETA, TRS, ALP
      FLOAT   PCAD,PCR
      logical GEP
*     ..
*     .. Array Arguments ..
      FLOAT   X( * ), BT( * ), BU( * )
*     ..
*
*  Computes the stopping criterion.
*
*
*     .. Parameters ..
      REALS   ZERO, ONE
      PARAMETER        ( ZERO = RZERO, ONE = RONE )
*     ..
*
*     .. Local Scalars ..
      integer          ISTAG, ISTT
      LOGICAL          TOUPDATE, POSDEF          
      REALS G0, GS, GTRS, GKMIN, BK, BETA, TN, UN2, GKM1
      FLOAT BKGEP, BUBT
*
*     saving all.
      SAVE
*   
*     .. External Routines ..
      FLOAT MYDDOT
      REALS NRM2
      EXTERNAL         NRM2, MYDDOT
*     ..
*     .. Executable Statements ..
*
      IF (INFO.EQ.0) GOTO 10
*
      POSDEF=INFO.EQ.-1
      ISTAG=0
      ISTT=0
      G0=RK
      GKM1=RK
      GTRS = TRS*G0
      IF (GTRS.LT.TOL) GTRS=TOL
      GS = GTRS
      GKMIN = G0
      BK = THETA-ETA
      IF (GEP) BKGEP = THETA-ETA+PCR
      UN2=-ONE
*
 10   CONTINUE
      BK=BK-ALPRHO
      IF (GEP) THEN
        BKGEP = BKGEP-PCAD
        BETA=ABS(BKGEP)
        GK=GK/NBU
      ELSE
        BETA=RABS(BK)
      END IF
*
*     From input:  GK = NRM2( N, R, 1 )
*
      IF (GK.LT.GKMIN) THEN
         GKMIN = GK
         ISTAG=0
      END IF
      IF (GK.GT.GKM1) THEN
         ISTAG=ISTAG+1
         ISTT=ISTT+1
      END IF

      IF (ISTAG.GE.25.OR.ISTT.GE.100) GOTO 40
      IF (GKMIN.GE.GTRS) GOTO 20
*
      TOUPDATE=.TRUE.
      IF (GK.GE.GS)  GOTO 15
 13   CONTINUE
        IF (GEP) THEN
          TN = NRM2( N, BT, 1 )
          TN = TN/NBU
          BUBT=-MYDDOT(N,BU,1,BT,1)/(NBU*NBU)
          ALP=ABS(ONE+BUBT)
          TN=SQRT(CTOD(CONJ(TN+BUBT)*(TN-BUBT)))/ALP
        ELSE
          ALP=ONE
          TN = NRM2( N, X, 1 )
        END IF
        UN2=ONE+TN**2
        TOUPDATE=.FALSE.
        IF (GK.LT.0.1*G0) THEN
           GS=TOL
        ELSE
           GS=0.1*G0
        END IF
        IF (GS.LT.TOL) GS=TOL
 15   CONTINUE
      IF (GEP) THEN
        IF (BETA .LT. GK*TN) THEN
              RK=SQRT((GK*GK+BETA*BETA)/UN2)/ALP
        ELSE
              RK=(GK+BETA*TN)/(ALP*UN2)
        END IF
      ELSE
        RK=SQRT(GK*GK*UN2+TN*TN*BETA*BETA)/UN2
      END IF
      IF (RK.LT.TOL .AND. TOUPDATE) GOTO 13
      IF (RK.LT.TOL) GOTO 40
      IF (GK*sqrt(UN2).LT.15*TN*BETA .AND. BETA*TN.GT.0.5*TOL*UN2*ALP)
     $     GOTO 40
      IF (ISTAG.GE.3) GOTO 40
      IF (ISTAG.GE.1 .AND. BETA*TN.GT.0.5*TOL*UN2*ALP .AND .GK.LT.GTRS)
     $     GOTO 40
*
 20   CONTINUE
*
      INFO = 0
      GKM1=GK
      RETURN
*
 40   CONTINUE
      INFO=MAX(INFO,1)
       IF (GEP) ALPRHO=UN2*ALP*ALP
       THETA=BK
      RETURN
*
*     End of STOPTEST
*
      END
*=============================================================================*
      SUBROUTINE REPGS(N,V,LDV,BV,LDBV,JV,W,Y,EPSM,GAM)
      implicit none
      integer          N, LDV, LDBV, JV, GAM
      FLOAT V(LDV,*), BV(LDBV,*), W(*), Y(*)
      REALS EPSM
*
*     .. Parameters ..
      REALS   ZERO, ONE
      PARAMETER        ( ZERO = RZERO, ONE = RONE )
      FLOAT   ZEROF, ONEF
      PARAMETER  ( ZEROF = FNULL)
      PARAMETER  ( ONEF = FONE )
*     ..
*     .. Local Scalars ..
      integer          IS(4),IREP
      DATA             IS/1,7,11,17/
      SAVE             IS
      LOGICAL          NRND
      REALS NRO, NR, NRN, NRM2
*     .. External Routines ..
      EXTERNAL         AXPY, COPY, RSCAL, NRM2, 
     +                 GEMV, LARNV
*
      NRND=.TRUE.
*
*  do nothing if JV<=0 
*         
      IF (JV.LE.0) RETURN
*
*  classical Gram-Schimdt if GAM<0
*
      IF (GAM.GE.0) GOTO 2      

        CALL GEMV('C',N,JV,ONEF,BV,LDBV,W,1,ZEROF,Y,1)
        CALL GEMV('N',N,JV,-ONEF,V,LDV,Y,1,ONEF,W,1)
        RETURN
*
*  standard case
*
 2      CONTINUE

        NRN=NRM2(N,W,1)
        NR=EPSM*NRN

        IF (NRND) CALL COPY(JV,ZEROF,0,Y,1)

        IREP=-1
 5      CONTINUE
        IREP=IREP+1
        if (irep .gt. 1) print *, 'Dependence problem'
        NRO=NRN
*       classical Gram-Schmidt
        CALL GEMV('C',N,JV,ONEF,BV,LDBV,W,1,ZEROF,Y(JV+1),1)
        CALL GEMV('N',N,JV,-ONEF,V,LDV,Y(JV+1),1,ONEF,W,1)
        NRN=NRM2(N,W,1)
        IF (NRND) CALL AXPY(JV,ONEF,Y(JV+1),1,Y,1)
*       exit if norm too small
        IF (NRN.LE.NR) GOTO 10
*       iterate if criterion satisfied
        IF (NRN.LT.0.5*NRO) GOTO 5
 
*       orthogonalization done - give the final norm back to calling program
        Y(JV+1)=NRN
        RETURN

*       orthogonalization failed; expand with random vector if GAM=1
 10     CONTINUE
        IF (GAM.EQ.1) THEN
           IF(JV.LT.N) THEN
             CALL LARNV(1,IS,N,W)
             NRND=.FALSE.
             GOTO 2
           END IF
        ELSE
           CALL COPY(N,ZEROF,0,W,1)
           Y(JV+1)=ZEROF
        END IF
        RETURN

        END
*=============================================================================*
        
C From Leonard J. Moss of SLAC:

C Here's a hybrid QuickSort I wrote a number of years ago.  It's
C based on suggestions in Knuth, Volume 3, and performs much better
C than a pure QuickSort on short or partially ordered input arrays.  

      SUBROUTINE SORTRX(N,DATA,INDEX)
C===================================================================
C
C     SORTRX -- SORT, Real input, indeX output
C
C
C     Input:  N     integer
C             DATA  FLOAT
C
C     Output: INDEX integer (DIMENSION N)
C
C This routine performs an in-memory sort of the first N elements of
C array DATA, returning into array INDEX the indices of elements of
C DATA arranged in ascending order.  Thus,
C
C    DATA(INDEX(1)) will be the smallest number in array DATA;
C    DATA(INDEX(N)) will be the largest number in DATA.
C
C The original data is not physically rearranged.  The original order
C of equal input values is not necessarily preserved.
C
C===================================================================
C
C SORTRX uses a hybrid QuickSort algorithm, based on several
C suggestions in Knuth, Volume 3, Section 5.2.2.  In particular, the
C "pivot key" [my term] for dividing each subsequence is chosen to be
C the median of the first, last, and middle values of the subsequence;
C and the QuickSort is cut off when a subsequence has 9 or fewer
C elements, and a straight insertion sort of the entire array is done
C at the end.  The result is comparable to a pure insertion sort for
C very short arrays, and very fast for very large arrays (of order 12
C micro-sec/element on the 3081K for arrays of 10K elements).  It is
C also not subject to the poor performance of the pure QuickSort on
C partially ordered data.
C
C Created:  15 Jul 1986  Len Moss
C
C===================================================================

      integer   N,INDEX(N)
      REALS      DATA(N)
 
      integer   LSTK(31),RSTK(31),ISTK
      integer   L,R,I,J,P,INDEXP,INDEXT
      REALS      DATAP
 
C     QuickSort Cutoff
C
C     Quit QuickSort-ing when a subsequence contains M or fewer
C     elements and finish off at end with straight insertion sort.
C     According to Knuth, V.3, the optimum value of M is around 9.
 
      integer   M
      PARAMETER (M=9)
 
C===================================================================
C
C     Make initial guess for INDEX
 
      DO 50 I=1,N
         INDEX(I)=I
   50    CONTINUE
 
C     If array is short, skip QuickSort and go directly to
C     the straight insertion sort.
 
      IF (N.LE.M) GOTO 900
 
C===================================================================
C
C     QuickSort
C
C     The "Qn:"s correspond roughly to steps in Algorithm Q,
C     Knuth, V.3, PP.116-117, modified to select the median
C     of the first, last, and middle elements as the "pivot
C     key" (in Knuth's notation, "K").  Also modified to leave
C     data in place and produce an INDEX array.  To simplify
C     comments, let DATA[I]=DATA(INDEX(I)).
 
C Q1: Initialize
      ISTK=0
      L=1
      R=N
 
  200 CONTINUE
 
C Q2: Sort the subsequence DATA[L]..DATA[R].
C
C     At this point, DATA[l] <= DATA[m] <= DATA[r] for all l < L,
C     r > R, and L <= m <= R.  (First time through, there is no
C     DATA for l < L or r > R.)
 
      I=L
      J=R
 
C Q2.5: Select pivot key
C
C     Let the pivot, P, be the midpoint of this subsequence,
C     P=(L+R)/2; then rearrange INDEX(L), INDEX(P), and INDEX(R)
C     so the corresponding DATA values are in increasing order.
C     The pivot key, DATAP, is then DATA[P].
 
      P=(L+R)/2
      INDEXP=INDEX(P)
      DATAP=DATA(INDEXP)
 
      IF (DATA(INDEX(L)) .GT. DATAP) THEN
         INDEX(P)=INDEX(L)
         INDEX(L)=INDEXP
         INDEXP=INDEX(P)
         DATAP=DATA(INDEXP)
      ENDIF
 
      IF (DATAP .GT. DATA(INDEX(R))) THEN
         IF (DATA(INDEX(L)) .GT. DATA(INDEX(R))) THEN
            INDEX(P)=INDEX(L)
            INDEX(L)=INDEX(R)
         ELSE
            INDEX(P)=INDEX(R)
         ENDIF
         INDEX(R)=INDEXP
         INDEXP=INDEX(P)
         DATAP=DATA(INDEXP)
      ENDIF
 
C     Now we swap values between the right and left sides and/or
C     move DATAP until all smaller values are on the left and all
C     larger values are on the right.  Neither the left or right
C     side will be internally ordered yet; however, DATAP will be
C     in its final position.
 
  300 CONTINUE
 
C Q3: Search for datum on left >= DATAP
C
C     At this point, DATA[L] <= DATAP.  We can therefore start scanning
C     up from L, looking for a value >= DATAP (this scan is guaranteed
C     to terminate since we initially placed DATAP near the middle of
C     the subsequence).
 
         I=I+1
         IF (DATA(INDEX(I)).LT.DATAP) GOTO 300
 
  400 CONTINUE
 
C Q4: Search for datum on right <= DATAP
C
C     At this point, DATA[R] >= DATAP.  We can therefore start scanning
C     down from R, looking for a value <= DATAP (this scan is guaranteed
C     to terminate since we initially placed DATAP near the middle of
C     the subsequence).
 
         J=J-1
         IF (DATA(INDEX(J)).GT.DATAP) GOTO 400
 
C Q5: Have the two scans collided?
 
      IF (I.LT.J) THEN
 
C Q6: No, interchange DATA[I] <--> DATA[J] and continue
 
         INDEXT=INDEX(I)
         INDEX(I)=INDEX(J)
         INDEX(J)=INDEXT
         GOTO 300
      ELSE
 
C Q7: Yes, select next subsequence to sort
C
C     At this point, I >= J and DATA[l] <= DATA[I] == DATAP <= DATA[r],
C     for all L <= l < I and J < r <= R.  If both subsequences are
C     more than M elements long, push the longer one on the stack and
C     go back to QuickSort the shorter; if only one is more than M
C     elements long, go back and QuickSort it; otherwise, pop a
C     subsequence off the stack and QuickSort it.
 
         IF (R-J .GE. I-L .AND. I-L .GT. M) THEN
            ISTK=ISTK+1
            LSTK(ISTK)=J+1
            RSTK(ISTK)=R
            R=I-1
         ELSE IF (I-L .GT. R-J .AND. R-J .GT. M) THEN
            ISTK=ISTK+1
            LSTK(ISTK)=L
            RSTK(ISTK)=I-1
            L=J+1
         ELSE IF (R-J .GT. M) THEN
            L=J+1
         ELSE IF (I-L .GT. M) THEN
            R=I-1
         ELSE
C Q8: Pop the stack, or terminate QuickSort if empty
            IF (ISTK.LT.1) GOTO 900
            L=LSTK(ISTK)
            R=RSTK(ISTK)
            ISTK=ISTK-1
         ENDIF
         GOTO 200
      ENDIF
 
  900 CONTINUE
 
C===================================================================
C
C Q9: Straight Insertion sort
 
      DO 950 I=2,N
         IF (DATA(INDEX(I-1)) .GT. DATA(INDEX(I))) THEN
            INDEXP=INDEX(I)
            DATAP=DATA(INDEXP)
            P=I-1
  920       CONTINUE
               INDEX(P+1) = INDEX(P)
               P=P-1
               IF (P.GT.0) THEN
                  IF (DATA(INDEX(P)).GT.DATAP) GOTO 920
               ENDIF
            INDEX(P+1) = INDEXP
         ENDIF
  950    CONTINUE
 
C===================================================================
C
C     All done
 
      END
*=============================================================================*
      SUBROUTINE LSOLREVCOM( N, X, WORK, LDW, X2, BT, ITER, INFO,
     $                       NDX1, NDX2, NDX3, GK, ALPHA, IJOB,GEP)
      implicit none
*
*     .. Scalar Arguments ..
      integer            N, LDW, ITER, INFO, NDX1, NDX2, NDX3, IJOB
      REALS   GK, ALPHA
      logical  GEP
*     ..
*     .. Array Arguments ..
      FLOAT   X( * ), WORK( LDW,* ), X2(*),BT(*)
*
*     ..
*
*  Purpose
*  =======
*
*  Solves the linear system Ax = b using the
*  Conjugate Gradient or the sQMR iterative method with preconditioning.
*
*  Arguments
*  =========
*
*  N       (input) integer.
*          On entry, the dimension of the matrix.
*          Unchanged on exit.
*
*  X       FLOAT array, dimension N.
*          On exit, the iterated approximate solution.
*
*  WORK    (input/workspace) FLOAT array, dimension (LDW,4).
*          On input, the right hand side of the system to solve
*          Workspace for residual, direction vector, etc.
*
*  LDW     (input) integer
*          The leading dimension of the array WORK. LDW >= max(1,N).
*
*  ITER    (input/output) integer
*          On input, the maximum iterations to be performed.
*          On output, actual number of iterations performed.
*
*
*  INFO    (input/output) integer
*          input: 
*              when ijob=1, info=1 iff the problem to solve is positive definite,
*                           in which case CG is used; otherwise, sQMR is used.
*              when ijob=4, info=1 iff termination required
*
*
*          output:
*
*          =  0: Successful exit. 
*
*          >  0: Breakdown or maximum number of iterations reached
*
*          <  0: Illegal input parameter.
*
*                   -1: matrix dimension N < 0
*                   -2: LDW < N
*                   -3: Maximum number of iterations ITER <= 0.
*                   -5: Erroneous NDX1/NDX2 in INIT call.
*                   -6: Erroneous RLBL.
*
*  NDX1    (input/output) integer. 
*  NDX2    On entry in INIT call contain indices required by interface
*          level for stopping test.
*          All other times, used as output, to indicate indices into
*          WORK[] for the MATVEC, PSOLVE done by the interface level.
*
*  SCLR1   (input/output) REALS.
*  SCLR2   Used to pass the scalars
*
*  IJOB    (input/output) integer. 
*          Used to communicate job code between the levels.
*
*  BLAS CALLS:   AXPY, COPY, MYDDOT, NRM2
*  ============================================================
*
*     .. Parameters ..
      REALS   ZERO, ONE
      PARAMETER        ( ZERO = RZERO, ONE = RONE )
*     ..
*     .. Local Scalars ..
      integer          MAXIT, R, Z, P, Q
      LOGICAL          POSDEF
      REALS BETA, RHO, RHO1, NRM2
      REALS GK2, CK2, GGK2, TTK2, TTK2M
*
*     indicates where to resume from. Only valid when IJOB = 2!
      integer RLBL
      FLOAT AXPYALPHA, MYDDOT
*
*     saving all.
      SAVE
*     ..
*     .. External Routines ..
      EXTERNAL         AXPY, COPY, MYDDOT, NRM2
*     ..
*     .. Executable Statements ..
*
*     Entry point, so test IJOB
      IF (IJOB .eq. 1) THEN
         GOTO 1
      ELSEIF (IJOB .eq. 2) THEN
*        here we do resumption handling
         IF (RLBL .eq. 3) GOTO 3
         IF (RLBL .eq. 4) GOTO 4
         IF (RLBL .eq. 5) GOTO 5
*        if neither of these, then error
         INFO = -6
         GOTO 20
      ENDIF
*
* init.
*****************
 1    CONTINUE
*****************
*
      POSDEF = INFO.EQ.1
      INFO = 0
      MAXIT = ITER
      GGK2=GK*GK
*
*     Alias workspace columns.
*
      R = 1
      Z = 2
      P = 3
      Q = 4
*
*     Set initial approximation (implicit if POSDEF=.FALSE).
*
      AXPYALPHA=ZERO
      IF (POSDEF) CALL COPY( N, AXPYALPHA, 0, X, 1 )
      IF (GEP)    CALL COPY( N, AXPYALPHA, 0, BT, 1 )
* 
      ITER = 0
*
   10 CONTINUE
*
*        Perform Preconditioned Conjugate Gradient iteration.
*
         ITER = ITER + 1
*
*        Preconditioner Solve.
*
*********CALL PSOLVE( WORK(1,Z), WORK(1,R) )
*
         NDX1 = ((Z - 1) * LDW) + 1
         NDX2 = ((R - 1) * LDW) + 1
*        Prepare for resumption & return
         RLBL = 3
         IJOB = 2
         RETURN
*
*****************
 3       CONTINUE
*****************
*
         RHO = CTOD(MYDDOT( N, WORK(1,R), 1, WORK(1,Z), 1 ))
         IF (POSDEF.AND.RHO.LE.ZERO) THEN
          PRINT *, 
     $    ' Non fatal bug: the preconditioner is not positive definite'
          GOTO 40
         END IF
         IF (RHO.EQ.ZERO) GOTO 40         
*
*        Compute direction vector P.
*
         IF ( ITER.GT.1 ) THEN
            BETA = RHO / RHO1
            AXPYALPHA=BETA
            CALL AXPY( N, AXPYALPHA, WORK(1,P), 1, WORK(1,Z), 1 )
            CALL COPY( N, WORK(1,Z), 1, WORK(1,P), 1 )
         ELSE
            CALL COPY( N, WORK(1,Z), 1, WORK(1,P), 1 )
         ENDIF
*
*        Compute scalar ALPHA (save A*P to Q).
*
*********CALL MATVEC( ONE, WORK(1,P), ZERO, WORK(1,Q) )
*
         NDX1 = ((P - 1) * LDW) + 1
         NDX2 = ((Q - 1) * LDW) + 1
         NDX3 = ((Z - 1) * LDW) + 1
*        Prepare for resumption & return
         RLBL = 4
         IJOB = 1
         RETURN
*
*****************
 4       CONTINUE
*****************
*
         ALPHA =  CTOD(MYDDOT( N, WORK(1,P), 1, WORK(1,Q), 1 ))
         IF ((POSDEF.AND.ALPHA.LE.ZERO).OR.ALPHA.EQ.0) GOTO 40
         ALPHA =  RHO / ALPHA 
*
*        Compute residual vector R, find norm
*
         AXPYALPHA=-ALPHA
         CALL AXPY( N, AXPYALPHA,  WORK(1,Q), 1, WORK(1,R), 1 )
         GK = NRM2(N, WORK(1,R), 1)
*
*        Compute current solution vector X.
*
         IF (GEP) THEN
             AXPYALPHA=ALPHA
             CALL AXPY( N, AXPYALPHA, WORK(1,Z), 1, BT, 1 )
         END IF
         IF (POSDEF) THEN
              AXPYALPHA=ALPHA
              CALL AXPY( N, AXPYALPHA, WORK(1,P), 1, X, 1 )
         ELSE
             GK2=GK*GK
             TTK2M=TTK2
             TTK2=GK2/GGK2
             CK2=ONE/(ONE+TTK2)
             GGK2=GGK2*TTK2*CK2
             IF (ITER.GT.1) THEN
                AXPYALPHA=ALPHA/TTK2M
               CALL AXPY( N, AXPYALPHA, WORK(1,P), 1, X2, 1 )
               CALL RSCAL( N, TTK2M*CK2, X2, 1)
               AXPYALPHA=ONE
               CALL AXPY( N, AXPYALPHA, X2, 1, X, 1) 
             ELSE
               CALL COPY( N, WORK(1,P), 1, X2, 1 )
               CALL RSCAL( N, ALPHA*CK2, X2, 1)
               CALL COPY( N, X2, 1, X, 1 )
             END IF
         END IF
*
*        Do stopping test based on residual norm in GK
*        Prepare for resumption & return
         RLBL = 5
         IJOB = 3
         RETURN
*
*****************
 5       CONTINUE
*****************
         IF( INFO.EQ.1 ) GO TO 30
*
         IF ( ITER.EQ.MAXIT ) GOTO 40
*
         RHO1 = RHO
*
         GO TO 10
*
   20 CONTINUE
*
*     Report error
*
      RLBL = -1
      IJOB = 4
      RETURN
*
   30 CONTINUE
*
*     Iteration successful; return.
*
      INFO = 0
      RLBL = -1
      IJOB = 4
      RETURN
*
   40 CONTINUE
*
*     Iteration fails.
*
      INFO = 1
      IF (ALPHA.LE.ZERO) INFO=2
      RLBL = -1
      IJOB =  4
      IF (ITER.EQ.1) CALL COPY(N, WORK(1,P), 1, X, 1)
      RETURN
*
*     End of CGREVCOM
*
      END
*
*=============================================================================*
      SUBROUTINE SOLPROJEP(J,MM,NN,MODSPACE,THETA,THETA2,W,LW,IS,
     $                     INFO,GEP)
      implicit none
*
*     .. Scalar Arguments ..
      integer J,LW,MODSPACE,IS,INFO,IJ,IK
      REALS   THETA, THETA2
      logical GEP
*     ..
*     .. Array Arguments ..
      FLOAT   MM(MODSPACE,MODSPACE),NN(MODSPACE,MODSPACE),DUM
      REALS   W(*)
*
      EXTERNAL      SYGV, SYEV
      REALS   ZERO, ONE
      PARAMETER        ( ZERO = RZERO, ONE = RONE )
*     
#if defined _DOUBLE_REAL_ || defined _SINGLE_REAL_
*
* Real case
*
      IF (IS.EQ.2.OR.IS.EQ.-2) THEN
         IF (GEP) THEN
            CALL POSV('U',J,J,NN,MODSPACE,MM,MODSPACE,INFO)
            CALL COPY(MODSPACE*MODSPACE,MM,1,NN,1)
            IF (INFO.NE.0) RETURN
            CALL GEEV('N','V',J,NN,MODSPACE,W,W(J+1),DUM,J,MM,
     $                 MODSPACE,W(2*J+1),LW-2*J,INFO)
         ELSE
            CALL SYGV(1,'V','U',J,MM,MODSPACE,NN,
     $                MODSPACE,W,W(J+1),LW-J,INFO)

         END IF
         GOTO 110
      ELSE
         CALL SYEV('V','U',J,MM,MODSPACE,W,W(J+1),LW-J,INFO)
         GOTO 100
      END IF
*
#else 
*
* Complex case
*
      IF (IS.EQ.2.OR.IS.EQ.-2) THEN
         IF (GEP) THEN
            CALL POSV('U',J,J,NN,MODSPACE,MM,MODSPACE,INFO)
            CALL COPY(MODSPACE*MODSPACE,MM,1,NN,1)
            IF (INFO.NE.0) RETURN
            CALL GEEV('N','V',J,NN,MODSPACE,W,DUM,J,MM,
     $                MODSPACE,W(4*J+1),LW-2*J,W(2*J+1),INFO)
            CALL COPY(J,W,1,NN,1)
            DO IJ=1,J
               W(IJ)=CTOD(NN(IJ,1))
            END DO
         ELSE
            CALL SYGV(1,'V','U',J,MM,MODSPACE,NN,
     $                MODSPACE,W,W(4*J+1),LW-2*J,W(J+1),INFO)
         END IF
         GOTO 110
      ELSE
         CALL SYEV('V','U',J,MM,MODSPACE,W,W(4*J+1),LW-2*J,W(J+1),INFO)
         GOTO 100
      END IF
*
#endif
*
 100    CONTINUE
* |IS|.NE.2, all cases
        IF (INFO.NE.0) RETURN
        IF (IS.EQ.3) THEN
          DO IJ=1,J
            NN(IJ,1)=W(IJ)
          END DO
        ELSE
          THETA=W(1)
          THETA2=W(2)
        END IF
        RETURN
*
 110    CONTINUE
* |IS|.EQ.2, all cases
        IF (INFO.NE.0) RETURN
        CALL SORTEV(J,J,MM,MODSPACE,NN,MODSPACE,W,NN,W(3*J+1),
     $              W(J+1),IS,ZERO)
        THETA2=W(2)
        RETURN
*
      END
*
*=============================================================================*
      SUBROUTINE ESTERR(NEIG,EIGS,RES,ISERCH,ERREST,iw,GAP)
      implicit none
      integer NEIG,ISERCH,iw(NEIG),I
      REALS EIGS(NEIG),RES(NEIG),ERREST(NEIG),GAP,DEL

      IF (NEIG.LE.1) THEN
           ERREST(1)=min(RES(1)*RES(1)/GAP,RES(1))   
           RETURN
      END IF

      if (ISERCH.EQ.2) then
        call SORTRX(NEIG,EIGS,IW)
        DEL=min(GAP,EIGS(iw(2))-EIGS(iw(1)))
        ERREST(iw(1))=min(RES(iw(1))*RES(iw(1))/DEL,RES(iw(1)))
        do i=2,NEIG-1
          DEL=min(EIGS(iw(i+1))-EIGS(iw(i)),EIGS(iw(i))-EIGS(iw(i-1)))
          ERREST(iw(i))=min(RES(iw(i))*RES(iw(i))/DEL,RES(iw(i)))
        end do
        DEL=min(GAP,EIGS(iw(NEIG))-EIGS(iw(NEIG-1)))
        ERREST(iw(NEIG))=
     $         min(RES(iw(NEIG))*RES(iw(NEIG))/DEL,RES(iw(NEIG)))
      else
        ERREST(1)=min(RES(1)*RES(1)/(EIGS(2)-EIGS(1)),RES(1))
        do i=2,NEIG-1
          DEL=min(EIGS(i+1)-EIGS(i),EIGS(i)-EIGS(i-1))
          ERREST(i)=min(RES(i)*RES(i)/DEL,RES(i))
        end do
        DEL=min(GAP,EIGS(NEIG)-EIGS(NEIG-1))
        ERREST(NEIG)=min(RES(NEIG)*RES(NEIG)/DEL,RES(NEIG))
      end if
      RETURN
      END
*======================================================================*
c     cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c     matrix-vector multiplication
c     cccccccccccccccccccccccccccc
      subroutine MATVECA(n,ia,ja,a,x,y)
      integer n, ia(n+1), ja(*)
      FLOAT a(*),x(n),y(n)
      FLOAT   ZEROF
      PARAMETER  ( ZEROF = FNULL)

      integer i,k

      do i=1,n
         y(i)=ZEROF
      end do
       
      do i=1,n
c        first element is supposed to be the diagonal entry
         k=ia(i)
         y(i)=y(i)+a(k)*x(i)
         do k=ia(i)+1,ia(i+1)-1
            y(i)=y(i)+a(k)*x(ja(k))
            y(ja(k))=y(ja(k))+CONJ(a(k))*x(i)
         end do
      end do
      end
*======================================================================*
