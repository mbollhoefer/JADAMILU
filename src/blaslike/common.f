      subroutine COMMONPJDINITPJDDEF
      DOUBLE PRECISION  timefact,shift0,droptol0,diagmin,shiftmax,
     +                  memrequested,memused,slightlyless,toldiv,
     +                  condest0
      INTEGER*8         IPparam,IPPREC,IPdiag
      integer           IPnlev,factdgl,factspd,IUNIT,PR,prvdr
      logical           reenterfirsttime,flagsingle
      common/PJDINITPJD/timefact,shift0,droptol0,diagmin,shiftmax,
     +                  memrequested,memused,slightlyless,toldiv,
     +                  IPparam,IPPREC,IPdiag,IPnlev,factdgl,factspd,
     +                  prvdr,IUNIT,PR,condest0,reenterfirsttime,
     +                  flagsingle
      data factdgl /0/
      data slightlyless/0.8d0/
      data toldiv/2.0d0/
      DATA flagsingle/.false./
      end


      subroutine COMMONJDRVCOMJDDEF
      DOUBLE PRECISION timeilu,rinit,rcurr,rcum,shift1
      logical flagcg
      common/JDRVCOMJD/timeilu,rinit,rcurr,rcum,shift1,flagcg
      DATA timeilu/0.0d0/
      end


      subroutine COMMONILUPACKMEMDEF
      integer ilumem  
      common/ILUPACKMEM/ilumem
      data ilumem /-1/
      end
