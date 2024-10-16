#ifndef _NAMES_ILUPACK_H
#define _NAMES_ILUPACK_H

#include "f2c.h"


/* on several architectures names of fortran routines are passed to C in 
   different ways. To cover this different architectures use in C only lower
   letters for the fortran names. Dependent on the switch you use they are
   replaced by the correct function name
*/

/* only use capital letters */
#if defined __CAPS__ && !defined __UNDERSCORE__ && !defined __2UNDERSCORES__

#define mysdot                    MYSDOT
#define myddot                    MYDDOT
#define mycdotc                   MYCDOTC
#define myzdotc                   MYZDOTC
#define sdsymamgsavediag          SDSYMAMGSAVEDIAG	    
#define sdsymamgsavediaggep       SDSYMAMGSAVEDIAGGEP
#define sdsymamgsol		  SDSYMAMGSOL		    
#define sdsymamginit		  SDSYMAMGINIT		    
#define sdsymamgrestorediag	  SDSYMAMGRESTOREDIAG	    
#define sdamgundoscaling	  SDAMGUNDOSCALING	    
#define sdsymamgfactor		  SDSYMAMGFACTOR
#define sdsymamgfactorgep	  SDSYMAMGFACTORGEP

#define czheramgsavediag	  CZHERAMGSAVEDIAG	    
#define czheramgsavediaggep	  CZHERAMGSAVEDIAGGEP
#define czheramgsol		  CZHERAMGSOL		    
#define czheramginit		  CZHERAMGINIT		    
#define czheramgrestorediag	  CZHERAMGRESTOREDIAG	    
#define czamgundoscaling	  CZAMGUNDOSCALING	    
#define czheramgfactor            CZHERAMGFACTOR              
#define czheramgfactorgep         CZHERAMGFACTORGEP


#define iprandom            IPRANDOM
#define ipsrandom           IPSRANDOM
#define evaluatetime        EVALUATETIME
#define dprivatesptrs       DPRIVATESPTRS
#define sprivatesptrs       SPRIVATESPTRS
#define cprivatehptrs       CPRIVATEHPTRS
#define zprivatehptrs       ZPRIVATEHPTRS

#define sdglprecsetup       SDGLPRECSETUP
#define sdglprecsol         SDGLPRECSOL
#define sdglprecdelete      SDGLPRECDELETE
#define ddglprecsetup       DDGLPRECSETUP
#define ddglprecsol         DDGLPRECSOL
#define ddglprecdelete      DDGLPRECDELETE

#define cdglprecsetup       CDGLPRECSETUP
#define cdglprecsol         CDGLPRECSOL
#define cdglprecdelete      CDGLPRECDELETE
#define zdglprecsetup       ZDGLPRECSETUP
#define zdglprecsol         ZDGLPRECSOL
#define zdglprecdelete      ZDGLPRECDELETE

#define dsymamgsavediag     DSYMAMGSAVEDIAG	  
#define dsymamgsavediaggep  DSYMAMGSAVEDIAGGEP	  
#define dsymamgrestorediag  DSYMAMGRESTOREDIAG
#define ssymamgsavediag     SSYMAMGSAVEDIAG	  
#define ssymamgsavediaggep  SSYMAMGSAVEDIAGGEP
#define ssymamgrestorediag  SSYMAMGRESTOREDIAG
#define cheramgsavediag     CHERAMGSAVEDIAG	  
#define cheramgsavediaggep  CHERAMGSAVEDIAGGEP
#define cheramgrestorediag  CHERAMGRESTOREDIAG
#define zheramgsavediag     ZHERAMGSAVEDIAG	  
#define zheramgsavediaggep  ZHERAMGSAVEDIAGGEP
#define zheramgrestorediag  ZHERAMGRESTOREDIAG

#define samgundoscaling     SAMGUNDOSCALING
#define damgundoscaling     DAMGUNDOSCALING
#define camgundoscaling     CAMGUNDOSCALING
#define zamgundoscaling     ZAMGUNDOSCALING

#define sgnlamginit         SGNLAMGINIT    
#define sgnlamgfactor       SGNLAMGFACTOR         
#define sgnlamgsolver       SGNLAMGSOLVER         
#define sgnlamgsol          SGNLAMGSOL    
#define sgnlamgdelete       SGNLAMGDELETE         
#define sgnlamginfo         SGNLAMGINFO
#define sgnlamgnnz          SGNLAMGNNZ

#define ssymspdamgconvert   SSYMSPDAMGCONVERT
                                              
#define sspdamginit         SSPDAMGINIT   
#define sspdamgfactor       SSPDAMGFACTOR         
#define sspdamgsolver       SSPDAMGSOLVER         
#define sspdamgsol          SSPDAMGSOL    
#define sspdamgdelete       SSPDAMGDELETE         
#define sspdamginfo         SSPDAMGINFO
#define sspdamgnnz          SSPDAMGNNZ
                                              
#define ssymamginit         SSYMAMGINIT   
#define ssymamgfactor       SSYMAMGFACTOR         
#define ssymamgfactorgep    SSYMAMGFACTORGEP
#define ssymamgsolver       SSYMAMGSOLVER         
#define ssymamgsol          SSYMAMGSOL    
#define ssymamgdelete       SSYMAMGDELETE         
#define ssymamginfo         SSYMAMGINFO
#define ssymamgnnz          SSYMAMGNNZ
                                              
                                              
#define dgnlamginit         DGNLAMGINIT   
#define dgnlamgfactor       DGNLAMGFACTOR         
#define dgnlamgsolver       DGNLAMGSOLVER         
#define dgnlamgsol          DGNLAMGSOL    
#define dgnlamgdelete       DGNLAMGDELETE         
#define dgnlamginfo         DGNLAMGINFO
#define dgnlamgnnz          DGNLAMGNNZ

#define Dsymspdamgconvert   DSYMSPDAMGCONVERT
                                              
#define dspdamginit         DSPDAMGINIT   
#define dspdamgfactor       DSPDAMGFACTOR         
#define dspdamgsolver       DSPDAMGSOLVER         
#define dspdamgsol          DSPDAMGSOL    
#define dspdamgdelete       DSPDAMGDELETE         
#define dspdamginfo         DSPDAMGINFO
#define dspdamgnnz          DSPDAMGNNZ
                                              
#define dsymamginit         DSYMAMGINIT   
#define dsymamgfactor       DSYMAMGFACTOR         
#define dsymamgfactorgep    DSYMAMGFACTORGEP
#define dsymamgsolver       DSYMAMGSOLVER         
#define dsymamgsol          DSYMAMGSOL    
#define dsymamgdelete       DSYMAMGDELETE         
#define dsymamginfo         DSYMAMGINFO
#define dsymamgnnz          DSYMAMGNNZ
                                              
                                              
#define cgnlamginit         CGNLAMGINIT   
#define cgnlamgfactor       CGNLAMGFACTOR         
#define cgnlamgsolver       CGNLAMGSOLVER         
#define cgnlamgsol          CGNLAMGSOL    
#define cgnlamgdelete       CGNLAMGDELETE         
#define cgnlamginfo         CGNLAMGINFO
#define cgnlamgnnz          CGNLAMGNNZ

#define cherhpdamgconvert   CHERHPDAMGCONVERT
                                              
#define chpdamginit         CHPDAMGINIT   
#define chpdamgfactor       CHPDAMGFACTOR         
#define chpdamgsolver       CHPDAMGSOLVER         
#define chpdamgsol          CHPDAMGSOL    
#define chpdamgdelete       CHPDAMGDELETE         
#define chpdamginfo         CHPDAMGINFO
#define chpdamgnnz          CHPDAMGNNZ
                                              
#define cheramginit         CHERAMGINIT   
#define cheramgfactor       CHERAMGFACTOR         
#define cheramgfactorgep    CHERAMGFACTORGEP
#define cheramgsolver       CHERAMGSOLVER         
#define cheramgsol          CHERAMGSOL    
#define cheramgdelete       CHERAMGDELETE         
#define cheramginfo         CHERAMGINFO
#define cheramgnnz          CHERAMGNNZ
                                              
#define csymamginit         CSYMAMGINIT   
#define csymamgfactor       CSYMAMGFACTOR         
#define csymamgsolver       CSYMAMGSOLVER         
#define csymamgsol          CSYMAMGSOL    
#define csymamgdelete       CSYMAMGDELETE         
#define csymamginfo         CSYMAMGINFO
#define csymamgnnz          CSYMAMGNNZ
                                              
                                              
#define zgnlamginit         ZGNLAMGINIT   
#define zgnlamgfactor       ZGNLAMGFACTOR         
#define zgnlamgsolver       ZGNLAMGSOLVER         
#define zgnlamgsol          ZGNLAMGSOL    
#define zgnlamgdelete       ZGNLAMGDELETE         
#define zgnlamginfo         ZGNLAMGINFO
#define zgnlamgnnz          ZGNLAMGNNZ
                                              
#define zherhpdamgconvert   ZHERHPDAMGCONVERT

#define zhpdamginit         ZHPDAMGINIT   
#define zhpdamgfactor       ZHPDAMGFACTOR         
#define zhpdamgsolver       ZHPDAMGSOLVER         
#define zhpdamgsol          ZHPDAMGSOL    
#define zhpdamgdelete       ZHPDAMGDELETE         
#define zhpdamginfo         ZHPDAMGINFO
#define zhpdamgnnz          ZHPDAMGNNZ
                                              
#define zheramginit         ZHERAMGINIT   
#define zheramgfactor       ZHERAMGFACTOR         
#define zheramgfactorgep    ZHERAMGFACTORGEP
#define zheramgsolver       ZHERAMGSOLVER         
#define zheramgsol          ZHERAMGSOL    
#define zheramgdelete       ZHERAMGDELETE         
#define zheramginfo         ZHERAMGINFO
#define zheramgnnz          ZHERAMGNNZ
                                              
#define zsymamginit         ZSYMAMGINIT   
#define zsymamgfactor       ZSYMAMGFACTOR         
#define zsymamgsolver       ZSYMAMGSOLVER         
#define zsymamgsol          ZSYMAMGSOL    
#define zsymamgdelete       ZSYMAMGDELETE     
#define zsymamginfo         ZSYMAMGINFO
#define zsymamgnnz          ZSYMAMGNNZ


#define qqsorti             QQSORTI

#define dsymilupack         DSYMILUPACK
#define dsymilupackfac      DSYMILUPACKFAC
#define dsymilupacksol      DSYMILUPACKSOL
#define dsymilupackdel      DSYMILUPACKDEL

#define DGNLlupq            DGNLLUPQ
#define DGNLlupqsol         DGNLLUPQSOL
#define DGNLlupqtsol        DGNLLUPQTSOL
#define DGNLlupqlsol        DGNLLUPQLSOL
#define DGNLlupqtlsol       DGNLLUPQTLSOL
#define DGNLlupqusol        DGNLLUPQUSOL
#define DGNLlupqtusol       DGNLLUPQTUSOL
#define DGNLlupqdlsol       DGNLLUPQDLSOL
#define DGNLlupqtdlsol      DGNLLUPQTDLSOL
#define DGNLlupqdusol       DGNLLUPQDUSOL
#define DGNLlupqtdusol      DGNLLUPQTDUSOL
#define DSPDldlp            DSPDLDLP
#define DSPDldlpsol         DSPDLDLPSOL

#define DGNLilutp           DGNLILUTP
#define DGNLilut            DGNLILUT
#define DGNLlusol           DGNLLUSOL
#define DGNLlutsol          DGNLLUTSOL
#define DGNLlulsol          DGNLLULSOL
#define DGNLlutlsol         DGNLLUTLSOL
#define DGNLluusol          DGNLLUUSOL
#define DGNLlutusol         DGNLLUTUSOL
#define DGNLludlsol         DGNLLUDLSOL
#define DGNLlutdlsol        DGNLLUTDLSOL
#define DGNLludusol         DGNLLUDUSOL
#define DGNLlutdusol        DGNLLUTDUSOL

#define DGNLiluc            DGNLILUC
#define DGNLilucsol         DGNLILUCSOL
#define DGNLiluctsol        DGNLILUCTSOL
#define DGNLilucdlsol       DGNLILUCDLSOL
#define DGNLiluctdlsol      DGNLILUCTDLSOL
#define DGNLilucdusol       DGNLILUCDUSOL
#define DGNLiluctdusol      DGNLILUCTDUSOL
#define DGNLiluclsol        DGNLILUCLSOL
#define DGNLiluctlsol       DGNLILUCTLSOL
#define DGNLilucusol        DGNLILUCUSOL
#define DGNLiluctusol       DGNLILUCTUSOL

#define DGNLpilucdlsol      DGNLPILUCDLSOL
#define DGNLpiluctdlsol     DGNLPILUCTDLSOL
#define DGNLpilucdusol      DGNLPILUCDUSOL
#define DGNLpiluctdusol     DGNLPILUCTDUSOL
#define DGNLpiluclsol       DGNLPILUCLSOL
#define DGNLpiluctlsol      DGNLPILUCTLSOL
#define DGNLpilucusol       DGNLPILUCUSOL
#define DGNLpiluctusol      DGNLPILUCTUSOL

#define DSYMildlc           DSYMILDLC
#define DSYMildlcsol        DSYMILDLCSOL
#define DSSMildlc           DSSMILDLC
#define DSSMildlcsol        DSSMILDLCSOL
#define DGNLpiluc           DGNLPILUC
#define DGNLspiluc          DGNLSPILUC
#define DGNLmpiluc          DGNLMPILUC
#define DSPDpiluc           DSPDPILUC
#define DSPDmpiluc          DSPDMPILUC

#define DSYMiluc            DSYMILUC
#define DSYMpiluc           DSYMPILUC
#define DSYMmpiluc          DSYMMPILUC
#define DSYMpilucsol        DSYMPILUCSOL
#define DSYMpiluclsol       DSYMPILUCLSOL
#define DSYMpilucusol       DSYMPILUCUSOL


#define Dpcg                DPCG
#define Dbcg                DBCG
#define DSYMbcg             DSYMBCG
#define DSYMqmr             DSYMQMR
#define Dgmres              DGMRES
#define Dfgmres             DFGMRES
#define Ddistdot            DDISTDOT


#define Droscal             DROSCAL
#define Dcoscal             DCOSCAL
#define Drowscale           DROWSCALE
#define Dcolscale           DCOLSCALE
#define DSPDscale           DSPDSCALE
#define DSYMscale           DSYMSCALE
#define Dcsrcsc             DCSRCSC
#define Dqsort              DQSORT
#define Dqqsort             DQQSORT
#define Dqqsort2            DQQSORT2
#define Dqqsorts            DQQSORTS
#define Dqqsorts2           DQQSORTS2

#define Dreadmtc            DREADMTC
#define Dwritemtc           DWRITEMTC
#define Dreadvectors        DREADVECTORS
#define Dwritevectors       DWRITEVECTORS



#define ssymilupack         SSYMILUPACK
#define ssymilupackfac      SSYMILUPACKFAC
#define ssymilupacksol      SSYMILUPACKSOL
#define ssymilupackdel      SSYMILUPACKDEL

#define SGNLlupq            SGNLLUPQ
#define SGNLlupqsol         SGNLLUPQSOL
#define SGNLlupqtsol        SGNLLUPQTSOL
#define SGNLlupqlsol        SGNLLUPQLSOL
#define SGNLlupqtlsol       SGNLLUPQTLSOL
#define SGNLlupqusol        SGNLLUPQUSOL
#define SGNLlupqtusol       SGNLLUPQTUSOL
#define SGNLlupqdlsol       SGNLLUPQDLSOL
#define SGNLlupqtdlsol      SGNLLUPQTDLSOL
#define SGNLlupqdusol       SGNLLUPQDUSOL
#define SGNLlupqtdusol      SGNLLUPQTDUSOL
#define SSPDldlp            SSPDLDLP
#define SSPDldlpsol         SSPDLDLPSOL

#define SGNLilutp           SGNLILUTP
#define SGNLilut            SGNLILUT
#define SGNLlusol           SGNLLUSOL
#define SGNLlutsol          SGNLLUTSOL
#define SGNLlulsol          SGNLLULSOL
#define SGNLlutlsol         SGNLLUTLSOL
#define SGNLluusol          SGNLLUUSOL
#define SGNLlutusol         SGNLLUTUSOL
#define SGNLludlsol         SGNLLUDLSOL
#define SGNLlutdlsol        SGNLLUTDLSOL
#define SGNLludusol         SGNLLUDUSOL
#define SGNLlutdusol        SGNLLUTDUSOL

#define SGNLiluc            SGNLILUC
#define SGNLilucsol         SGNLILUCSOL
#define SGNLiluctsol        SGNLILUCTSOL
#define SGNLilucdlsol       SGNLILUCDLSOL
#define SGNLiluctdlsol      SGNLILUCTDLSOL
#define SGNLilucdusol       SGNLILUCDUSOL
#define SGNLiluctdusol      SGNLILUCTDUSOL
#define SGNLiluclsol        SGNLILUCLSOL
#define SGNLiluctlsol       SGNLILUCTLSOL
#define SGNLilucusol        SGNLILUCUSOL
#define SGNLiluctusol       SGNLILUCTUSOL

#define SGNLpilucdlsol      SGNLPILUCDLSOL
#define SGNLpiluctdlsol     SGNLPILUCTDLSOL
#define SGNLpilucdusol      SGNLPILUCDUSOL
#define SGNLpiluctdusol     SGNLPILUCTDUSOL
#define SGNLpiluclsol       SGNLPILUCLSOL
#define SGNLpiluctlsol      SGNLPILUCTLSOL
#define SGNLpilucusol       SGNLPILUCUSOL
#define SGNLpiluctusol      SGNLPILUCTUSOL

#define SSYMildlc           SSYMILDLC
#define SSYMildlcsol        SSYMILDLCSOL
#define SSSMildlc           SSSMILDLC
#define SSSMildlcsol        SSSMILDLCSOL
#define SGNLpiluc           SGNLPILUC
#define SGNLspiluc          SGNLSPILUC
#define SGNLmpiluc          SGNLMPILUC
#define SSPDpiluc           SSPDPILUC
#define SSPDmpiluc          SSPDMPILUC
#define SSYMpiluc           SSYMPILUC
#define SSYMiluc            SSYMILUC
#define SSYMmpiluc          SSYMMPILUC
#define SSYMpilucsol        SSYMPILUCSOL
#define SSYMpiluclsol       SSYMPILUCLSOL
#define SSYMpilucusol       SSYMPILUCUSOL


#define Spcg                SPCG
#define Sbcg                SBCG
#define SSYMbcg             SSYMBCG
#define SSYMqmr             SSYMQMR
#define Sgmres              SGMRES
#define Sfgmres             SFGMRES
#define Sdistdot            SDISTDOT


#define Sroscal             SROSCAL
#define Scoscal             SCOSCAL
#define Srowscale           SROWSCALE
#define Scolscale           SCOLSCALE
#define SSPDscale           SSPDSCALE
#define SSYMscale           SSYMSCALE
#define Scsrcsc             SCSRCSC
#define Sqsort              SQSORT
#define Sqqsort             SQQSORT
#define Sqqsort2            SQQSORT2
#define Sqqsorts            SQQSORTS
#define Sqqsorts2           SQQSORTS2

#define Sreadmtc            SREADMTC
#define Swritemtc           SWRITEMTC
#define Sreadvectors        SREADVECTORS
#define Swritevectors       SWRITEVECTORS



#define zsymilupack         ZSYMILUPACK
#define zsymilupackfac      ZSYMILUPACKFAC
#define zsymilupacksol      ZSYMILUPACKSOL
#define zsymilupackdel      ZSYMILUPACKDEL
#define zherilupack         ZHERILUPACK
#define zherilupackfac      ZHERILUPACKFAC
#define zherilupacksol      ZHERILUPACKSOL
#define zherilupackdel      ZHERILUPACKDEL

#define ZGNLlupq            ZGNLLUPQ
#define ZGNLlupqsol         ZGNLLUPQSOL
#define ZGNLlupqtsol        ZGNLLUPQTSOL
#define ZGNLlupqhsol        ZGNLLUPQHSOL
#define ZGNLlupqlsol        ZGNLLUPQLSOL
#define ZGNLlupqtlsol       ZGNLLUPQTLSOL
#define ZGNLlupqusol        ZGNLLUPQUSOL
#define ZGNLlupqtusol       ZGNLLUPQTUSOL
#define ZGNLlupqdlsol       ZGNLLUPQDLSOL
#define ZGNLlupqtdlsol      ZGNLLUPQTDLSOL
#define ZGNLlupqdusol       ZGNLLUPQDUSOL
#define ZGNLlupqtdusol      ZGNLLUPQTDUSOL
#define ZHPDldlp            ZHPDLDLP
#define ZHPDldlpsol         ZHPDLDLPSOL


#define ZGNLilutp           ZGNLILUTP
#define ZGNLilut            ZGNLILUT
#define ZGNLlusol           ZGNLLUSOL
#define ZGNLlutsol          ZGNLLUTSOL
#define ZGNLlulsol          ZGNLLULSOL
#define ZGNLlutlsol         ZGNLLUTLSOL
#define ZGNLluusol          ZGNLLUUSOL
#define ZGNLlutusol         ZGNLLUTUSOL
#define ZGNLludlsol         ZGNLLUDLSOL
#define ZGNLlutdlsol        ZGNLLUTDLSOL
#define ZGNLludusol         ZGNLLUDUSOL
#define ZGNLlutdusol        ZGNLLUTDUSOL

#define ZGNLiluc            ZGNLILUC
#define ZGNLilucsol         ZGNLILUCSOL
#define ZGNLiluctsol        ZGNLILUCTSOL
#define ZGNLilucdlsol       ZGNLILUCDLSOL
#define ZGNLiluctdlsol      ZGNLILUCTDLSOL
#define ZGNLilucdusol       ZGNLILUCDUSOL
#define ZGNLiluctdusol      ZGNLILUCTDUSOL
#define ZGNLiluclsol        ZGNLILUCLSOL
#define ZGNLiluctlsol       ZGNLILUCTLSOL
#define ZGNLilucusol        ZGNLILUCUSOL
#define ZGNLiluctusol       ZGNLILUCTUSOL

#define ZGNLpilucdlsol      ZGNLPILUCDLSOL
#define ZGNLpiluctdlsol     ZGNLPILUCTDLSOL
#define ZGNLpilucdusol      ZGNLPILUCDUSOL
#define ZGNLpiluctdusol     ZGNLPILUCTDUSOL
#define ZGNLpiluclsol       ZGNLPILUCLSOL
#define ZGNLpiluctlsol      ZGNLPILUCTLSOL
#define ZGNLpilucusol       ZGNLPILUCUSOL
#define ZGNLpiluctusol      ZGNLPILUCTUSOL

#define ZHERildlc           ZHERILDLC
#define ZHERildlcsol        ZHERILDLCSOL
#define ZSYMildlc           ZSYMILDLC
#define ZSYMildlcsol        ZSYMILDLCSOL
#define ZSHRildlc           ZSHRILDLC
#define ZSHRildlcsol        ZSHRILDLCSOL
#define ZSSMildlc           ZSSMILDLC
#define ZSSMildlcsol        ZSSMILDLCSOL
#define ZGNLpiluc           ZGNLPILUC
#define ZGNLspiluc          ZGNLSPILUC
#define ZGNLmpiluc          ZGNLMPILUC
#define ZHPDpiluc           ZHPDPILUC
#define ZHPDmpiluc          ZHPDMPILUC
#define ZSYMpiluc           ZSYMPILUC
#define ZSYMiluc            ZSYMILUC
#define ZSYMmpiluc          ZSYMMPILUC
#define ZSYMpilucsol        ZSYMPILUCSOL
#define ZHERpiluc           ZHERPILUC
#define ZHERiluc            ZHERILUC
#define ZHERmpiluc          ZHERMPILUC
#define ZHERpilucsol        ZHERPILUCSOL
#define ZSYMpiluclsol       ZSYMPILUCLSOL
#define ZSYMpilucusol       ZSYMPILUCUSOL
#define ZHERpiluclsol       ZHERPILUCLSOL
#define ZHERpilucusol       ZHERPILUCUSOL


#define Zpcg                ZPCG
#define Zbcg                ZBCG
#define ZSYMbcg             ZSYMBCG
#define ZHERbcg             ZHERBCG
#define ZSYMqmr             ZSYMQMR
#define ZHERqmr             ZHERQMR
#define Zgmres              ZGMRES
#define Zfgmres             ZFGMRES
#define Zdistdotc           ZDISTDOTC
#define Zdistdotu           ZDISTDOTU


#define Zroscal             ZROSCAL
#define Zcoscal             ZCOSCAL
#define Zrowscale           ZROWSCALE
#define Zcolscale           ZCOLSCALE
#define ZHPDscale           ZHPDSCALE
#define ZSYMscale           ZSYMSCALE
#define ZHERscale           ZHERSCALE
#define Zcsrcsc             ZCSRCSC
#define Zqsort              ZQSORT
#define Zqqsort             ZQQSORT
#define Zqqsort2            ZQQSORT2
#define Zqqsorts            ZQQSORTS
#define Zqqsorts2           ZQQSORTS2


#define Zreadmtc            ZREADMTC
#define Zwritemtc           ZWRITEMTC
#define Zreadvectors        ZREADVECTORS
#define Zwritevectors       ZWRITEVECTORS



#define csymilupack         CSYMILUPACK
#define csymilupackfac      CSYMILUPACKFAC
#define csymilupacksol      CSYMILUPACKSOL
#define csymilupackdel      CSYMILUPACKDEL
#define cherilupack         CHERILUPACK
#define cherilupackfac      CHERILUPACKFAC
#define cherilupacksol      CHERILUPACKSOL
#define cherilupackdel      CHERILUPACKDEL

#define CGNLlupq            CGNLLUPQ
#define CGNLlupqsol         CGNLLUPQSOL
#define CGNLlupqtsol        CGNLLUPQTSOL
#define CGNLlupqhsol        CGNLLUPQHSOL
#define CGNLlupqlsol        CGNLLUPQLSOL
#define CGNLlupqtlsol       CGNLLUPQTLSOL
#define CGNLlupqusol        CGNLLUPQUSOL
#define CGNLlupqtusol       CGNLLUPQTUSOL
#define CGNLlupqdlsol       CGNLLUPQDLSOL
#define CGNLlupqtdlsol      CGNLLUPQTDLSOL
#define CGNLlupqdusol       CGNLLUPQDUSOL
#define CGNLlupqtdusol      CGNLLUPQTDUSOL
#define CHPDldlp            CHPDLDLP
#define CHPDldlpsol         CHPDLDLPSOL


#define CGNLilutp           CGNLILUTP
#define CGNLilut            CGNLILUT
#define CGNLlusol           CGNLLUSOL
#define CGNLlutsol          CGNLLUTSOL
#define CGNLlulsol          CGNLLULSOL
#define CGNLlutlsol         CGNLLUTLSOL
#define CGNLluusol          CGNLLUUSOL
#define CGNLlutusol         CGNLLUTUSOL
#define CGNLludlsol         CGNLLUDLSOL
#define CGNLlutdlsol        CGNLLUTDLSOL
#define CGNLludusol         CGNLLUDUSOL
#define CGNLlutdusol        CGNLLUTDUSOL

#define CGNLiluc            CGNLILUC
#define CGNLilucsol         CGNLILUCSOL
#define CGNLiluctsol        CGNLILUCTSOL
#define CGNLilucdlsol       CGNLILUCDLSOL
#define CGNLiluctdlsol      CGNLILUCTDLSOL
#define CGNLilucdusol       CGNLILUCDUSOL
#define CGNLiluctdusol      CGNLILUCTDUSOL
#define CGNLiluclsol        CGNLILUCLSOL
#define CGNLiluctlsol       CGNLILUCTLSOL
#define CGNLilucusol        CGNLILUCUSOL
#define CGNLiluctusol       CGNLILUCTUSOL

#define CGNLpilucdlsol      CGNLPILUCDLSOL
#define CGNLpiluctdlsol     CGNLPILUCTDLSOL
#define CGNLpilucdusol      CGNLPILUCDUSOL
#define CGNLpiluctdusol     CGNLPILUCTDUSOL
#define CGNLpiluclsol       CGNLPILUCLSOL
#define CGNLpiluctlsol      CGNLPILUCTLSOL
#define CGNLpilucusol       CGNLPILUCUSOL
#define CGNLpiluctusol      CGNLPILUCTUSOL

#define CHERildlc           CHERILDLC
#define CHERildlcsol        CHERILDLCSOL
#define CSYMildlc           CSYMILDLC
#define CSYMildlcsol        CSYMILDLCSOL
#define CSHRildlc           CSHRILDLC
#define CSHRildlcsol        CSHRILDLCSOL
#define CSSMildlc           CSSMILDLC
#define CSSMildlcsol        CSSMILDLCSOL
#define CGNLpiluc           CGNLPILUC
#define CGNLspiluc          CGNLSPILUC
#define CGNLmpiluc          CGNLMPILUC
#define CHPDpiluc           CHPDPILUC
#define CHPDmpiluc          CHPDMPILUC
#define CSYMpiluc           CSYMPILUC
#define CSYMiluc            CSYMILUC
#define CSYMmpiluc          CSYMMPILUC
#define CSYMpilucsol        CSYMPILUCSOL
#define CHERpiluc           CHERPILUC
#define CHERiluc            CHERILUC
#define CHERmpiluc          CHERMPILUC
#define CHERpilucsol        CHERPILUCSOL
#define CSYMpiluclsol       CSYMPILUCLSOL
#define CSYMpilucusol       CSYMPILUCUSOL
#define CHERpiluclsol       CHERPILUCLSOL
#define CHERpilucusol       CHERPILUCUSOL


#define Cpcg                CPCG
#define Cbcg                CBCG
#define CSYMbcg             CSYMBCG
#define CHERbcg             CHERBCG
#define CSYMqmr             CSYMQMR
#define CHERqmr             CHERQMR
#define Cgmres              CGMRES
#define Cfgmres             CFGMRES
#define Cdistdotc           CDISTDOTC
#define Cdistdotu           CDISTDOTU


#define Croscal             CROSCAL
#define Ccoscal             CCOSCAL
#define Crowscale           CROWSCALE
#define Ccolscale           CCOLSCALE
#define CHPDscale           CHPDSCALE
#define CSYMscale           CSYMSCALE
#define CHERscale           CHERSCALE
#define Ccsrcsc             CCSRCSC
#define Cqsort              CQSORT
#define Cqqsort             CQQSORT
#define Cqqsort2            CQQSORT2
#define Cqqsorts            CQQSORTS
#define Cqqsorts2           CQQSORTS2


#define Creadmtc            CREADMTC
#define Cwritemtc           CWRITEMTC
#define Creadvectors        CREADVECTORS
#define Cwritevectors       CWRITEVECTORS




/* no capital letters */
#elif defined __UNDERSCORE__ && !defined __CAPS__ && !defined __2UNDERSCORES__
#define mysdot                    mysdot_
#define myddot                    myddot_
#define mycdotc                   mycdotc_
#define myzdotc                   myzdotc_
#define sdsymamgsavediag          sdsymamgsavediag_	   
#define sdsymamgsavediaggep       sdsymamgsavediaggep_
#define sdsymamgsol		  sdsymamgsol_		   
#define sdsymamginit		  sdsymamginit_		   
#define sdsymamgrestorediag	  sdsymamgrestorediag_	   
#define sdamgundoscaling	  sdamgundoscaling_	   
#define sdsymamgfactor		  sdsymamgfactor_		   
#define sdsymamgfactorgep	  sdsymamgfactorgep_

#define czheramgsavediag	  czheramgsavediag_	   
#define czheramgsavediaggep	  czheramgsavediaggep_
#define czheramgsol		  czheramgsol_		   
#define czheramginit		  czheramginit_		   
#define czheramgrestorediag	  czheramgrestorediag_	   
#define czamgundoscaling	  czamgundoscaling_	   
#define czheramgfactor            czheramgfactor_              
#define czheramgfactorgep         czheramgfactorgep_

#define iprandom            iprandom_
#define ipsrandom           ipsrandom_
#define evaluatetime        evaluatetime_
#define dprivatesptrs       dprivatesptrs_
#define sprivatesptrs       sprivatesptrs_
#define cprivatehptrs       cprivatehptrs_
#define zprivatehptrs       zprivatehptrs_

#define sdglprecsetup        sdglprecsetup_
#define sdglprecsol          sdglprecsol_
#define sdglprecdelete       sdglprecdelete_
#define ddglprecsetup        ddglprecsetup_
#define ddglprecsol          ddglprecsol_
#define ddglprecdelete       ddglprecdelete_

#define cdglprecsetup       cdglprecsetup_
#define cdglprecsol         cdglprecsol_
#define cdglprecdelete      cdglprecdelete_
#define zdglprecsetup       zdglprecsetup_
#define zdglprecsol         zdglprecsol_
#define zdglprecdelete      zdglprecdelete_

#define dsymamgsavediag     dsymamgsavediag_	  
#define dsymamgsavediaggep  dsymamgsavediaggep_
#define dsymamgrestorediag  dsymamgrestorediag_
#define ssymamgsavediag     ssymamgsavediag_	  
#define ssymamgsavediaggep  ssymamgsavediaggep_
#define ssymamgrestorediag  ssymamgrestorediag_
#define cheramgsavediag     cheramgsavediag_	  
#define cheramgsavediaggep  cheramgsavediaggep_
#define cheramgrestorediag  cheramgrestorediag_
#define zheramgsavediag     zheramgsavediag_	  
#define zheramgsavediaggep  zheramgsavediaggep_
#define zheramgrestorediag  zheramgrestorediag_

#define samgundoscaling     samgundoscaling_
#define damgundoscaling     damgundoscaling_
#define camgundoscaling     camgundoscaling_
#define zamgundoscaling     zamgundoscaling_

#define sgnlamginit         sgnlamginit_             
#define sgnlamgfactor       sgnlamgfactor_          
#define sgnlamgsolver       sgnlamgsolver_           
#define sgnlamgsol          sgnlamgsol_      
#define sgnlamgdelete       sgnlamgdelete_              
#define sgnlamginfo         sgnlamginfo_
#define sgnlamgnnz          sgnlamgnnz_

#define ssymspdamgconvert   ssymspdamgconvert_
                                            
#define sspdamginit         sspdamginit_        
#define sspdamgfactor       sspdamgfactor_              
#define sspdamgsolver       sspdamgsolver_              
#define sspdamgsol          sspdamgsol_      
#define sspdamgdelete       sspdamgdelete_              
#define sspdamginfo         sspdamginfo_
#define sspdamgnnz          sspdamgnnz_
                                            
#define ssymamginit         ssymamginit_        
#define ssymamgfactor       ssymamgfactor_              
#define ssymamgfactorgep    ssymamgfactorgep_
#define ssymamgsolver       ssymamgsolver_              
#define ssymamgsol          ssymamgsol_      
#define ssymamgdelete       ssymamgdelete_              
#define ssymamginfo         ssymamginfo_
#define ssymamgnnz          ssymamgnnz_
                                            
                                            
#define dgnlamginit         dgnlamginit_        
#define dgnlamgfactor       dgnlamgfactor_              
#define dgnlamgsolver       dgnlamgsolver_              
#define dgnlamgsol          dgnlamgsol_     
#define dgnlamgdelete       dgnlamgdelete_              
#define dgnlamginfo         dgnlamginfo_
#define dgnlamgnnz          dgnlamgnnz_
                                            
#define dsymspdamgconvert   dsymspdamgconvert_

#define dspdamginit         dspdamginit_        
#define dspdamgfactor       dspdamgfactor_              
#define dspdamgsolver       dspdamgsolver_              
#define dspdamgsol          dspdamgsol_      
#define dspdamgdelete       dspdamgdelete_              
#define dspdamginfo         dspdamginfo_
#define dspdamgnnz          dspdamgnnz_
                                            
#define dsymamginit         dsymamginit_        
#define dsymamgfactor       dsymamgfactor_              
#define dsymamgfactorgep    dsymamgfactorgep_
#define dsymamgsolver       dsymamgsolver_              
#define dsymamgsol          dsymamgsol_      
#define dsymamgdelete       dsymamgdelete_              
#define dsymamginfo         dsymamginfo_
#define dsymamgnnz          dsymamgnnz_
                                            
                                            
#define cgnlamginit         cgnlamginit_        
#define cgnlamgfactor       cgnlamgfactor_              
#define cgnlamgsolver       cgnlamgsolver_              
#define cgnlamgsol          cgnlamgsol_      
#define cgnlamgdelete       cgnlamgdelete_              
#define cgnlamginfo         cgnlamginfo_
#define cgnlamgnnz          cgnlamgnnz_
                                            
#define cherhpdamgconvert   cherhpdamgconvert_

#define chpdamginit         chpdamginit_        
#define chpdamgfactor       chpdamgfactor_              
#define chpdamgsolver       chpdamgsolver_              
#define chpdamgsol          chpdamgsol_      
#define chpdamgdelete       chpdamgdelete_              
#define chpdamginfo         chpdamginfo_
#define chpdamgnnz          chpdamgnnz_
                                            
#define cheramginit         cheramginit_        
#define cheramgfactor       cheramgfactor_              
#define cheramgfactorgep    cheramgfactorgep_
#define cheramgsolver       cheramgsolver_              
#define cheramgsol          cheramgsol_      
#define cheramgdelete       cheramgdelete_              
#define cheramginfo         cheramginfo_
#define cheramgnnz          cheramgnnz_
                                            
#define csymamginit         csymamginit_        
#define csymamgfactor       csymamgfactor_              
#define csymamgsolver       csymamgsolver_              
#define csymamgsol          csymamgsol_      
#define csymamgdelete       csymamgdelete_              
#define csymamginfo         csymamginfo_
#define csymamgnnz          csymamgnnz_
                                            
                                            
#define zgnlamginit         zgnlamginit_        
#define zgnlamgfactor       zgnlamgfactor_              
#define zgnlamgsolver       zgnlamgsolver_              
#define zgnlamgsol          zgnlamgsol_      
#define zgnlamgdelete       zgnlamgdelete_              
#define zgnlamginfo         zgnlamginfo_
#define zgnlamgnnz          zgnlamgnnz_
                                            
#define zherhpdamgconvert   zherhpdamgconvert_

#define zhpdamginit         zhpdamginit_        
#define zhpdamgfactor       zhpdamgfactor_              
#define zhpdamgsolver       zhpdamgsolver_              
#define zhpdamgsol          zhpdamgsol_      
#define zhpdamgdelete       zhpdamgdelete_              
#define zhpdamginfo         zhpdamginfo_
#define zhpdamgnnz          zhpdamgnnz_
                                            
#define zheramginit         zheramginit_        
#define zheramgfactor       zheramgfactor_              
#define zheramgfactorgep    zheramgfactorgep_
#define zheramgsolver       zheramgsolver_              
#define zheramgsol          zheramgsol_      
#define zheramgdelete       zheramgdelete_              
#define zheramginfo         zheramginfo_
#define zheramgnnz          zheramgnnz_
                                            
#define zsymamginit         zsymamginit_        
#define zsymamgfactor       zsymamgfactor_              
#define zsymamgsolver       zsymamgsolver_              
#define zsymamgsol          zsymamgsol_      
#define zsymamgdelete       zsymamgdelete_       
#define zsymamginfo         zsymamginfo_
#define zsymamgnnz          zsymamgnnz_



#define qqsorti             qqsorti_

#define dsymilupack         dsymilupack_
#define dsymilupackfac      dsymilupackfac_
#define dsymilupacksol      dsymilupacksol_
#define dsymilupackdel      dsymilupackdel_

#define DGNLlupq            dgnllupq_
#define DGNLlupqsol         dgnllupqsol_
#define DGNLlupqtsol        dgnllupqtsol_
#define DGNLlupqlsol        dgnllupqlsol_
#define DGNLlupqtlsol       dgnllupqtlsol_
#define DGNLlupqusol        dgnllupqusol_
#define DGNLlupqtusol       dgnllupqtusol_
#define DGNLlupqdlsol       dgnllupqdlsol_
#define DGNLlupqtdlsol      dgnllupqtdlsol_
#define DGNLlupqdusol       dgnllupqdusol_
#define DGNLlupqtdusol      dgnllupqtdusol_
#define DSPDldlp            dspdldlp_
#define DSPDldlpsol         dspdldlpsol_

#define DGNLilutp           dgnlilutp_
#define DGNLilut            dgnlilut_
#define DGNLlusol           dgnllusol_
#define DGNLlutsol          dgnllutsol_
#define DGNLlulsol          dgnllulsol_
#define DGNLlutlsol         dgnllutlsol_
#define DGNLluusol          dgnlluusol_
#define DGNLlutusol         dgnllutusol_
#define DGNLludlsol         dgnlludlsol_
#define DGNLlutdlsol        dgnllutdlsol_
#define DGNLludusol         dgnlludusol_
#define DGNLlutdusol        dgnllutdusol_

#define DGNLiluc            dgnliluc_
#define DGNLilucsol         dgnlilucsol_
#define DGNLiluctsol        dgnliluctsol_
#define DGNLilucdlsol       dgnlilucdlsol_
#define DGNLiluctdlsol      dgnliluctdlsol_
#define DGNLilucdusol       dgnlilucdusol_
#define DGNLiluctdusol      dgnliluctdusol_
#define DGNLiluclsol        dgnliluclsol_
#define DGNLiluctlsol       dgnliluctlsol_
#define DGNLilucusol        dgnlilucusol_
#define DGNLiluctusol       dgnliluctusol_

#define DGNLpilucdlsol      dgnlpilucdlsol_
#define DGNLpiluctdlsol     dgnlpiluctdlsol_
#define DGNLpilucdusol      dgnlpilucdusol_
#define DGNLpiluctdusol     dgnlpiluctdusol_
#define DGNLpiluclsol       dgnlpiluclsol_
#define DGNLpiluctlsol      dgnlpiluctlsol_
#define DGNLpilucusol       dgnlpilucusol_
#define DGNLpiluctusol      dgnlpiluctusol_

#define DSYMildlc           dsymildlc_
#define DSYMildlcsol        dsymildlcsol_
#define DSSMildlc           dssmildlc_
#define DSSMildlcsol        dssmildlcsol_
#define DGNLpiluc           dgnlpiluc_
#define DGNLspiluc          dgnlspiluc_
#define DGNLmpiluc          dgnlmpiluc_
#define DSPDpiluc           dspdpiluc_
#define DSPDmpiluc          dspdmpiluc_
#define DSYMpiluc           dsympiluc_
#define DSYMiluc            dsymiluc_
#define DSYMmpiluc          dsymmpiluc_
#define DSYMpilucsol        dsympilucsol_
#define DSYMpiluclsol       dsympiluclsol_
#define DSYMpilucusol       dsympilucusol_


#define Dpcg                dpcg_
#define Dbcg                dbcg_
#define DSYMbcg             dsymbcg_
#define DSYMqmr             dsymqmr_
#define Dgmres              dgmres_
#define Dfgmres             dfgmres_
#define Ddistdot            ddistdot_


#define Droscal             droscal_
#define Dcoscal             dcoscal_
#define Drowscale           drowscale_
#define Dcolscale           dcolscale_
#define DSPDscale           dspdscale_
#define DSYMscale           dsymscale_
#define Dcsrcsc             dcsrcsc_
#define Dqsort              dqsort_
#define Dqqsort             dqqsort_
#define Dqqsort2            dqqsort2_
#define Dqqsorts            dqqsorts_
#define Dqqsorts2           dqqsorts2_

#define Dreadmtc            dreadmtc_
#define Dwritemtc           dwritemtc_
#define Dreadvectors        dreadvectors_
#define Dwritevectors       dwritevectors_



#define ssymilupack         ssymilupack_
#define ssymilupackfac      ssymilupackfac_
#define ssymilupacksol      ssymilupacksol_
#define ssymilupackdel      ssymilupackdel_

#define SGNLlupq            sgnllupq_
#define SGNLlupqsol         sgnllupqsol_
#define SGNLlupqtsol        sgnllupqtsol_
#define SGNLlupqlsol        sgnllupqlsol_
#define SGNLlupqtlsol       sgnllupqtlsol_
#define SGNLlupqusol        sgnllupqusol_
#define SGNLlupqtusol       sgnllupqtusol_
#define SGNLlupqdlsol       sgnllupqdlsol_
#define SGNLlupqtdlsol      sgnllupqtdlsol_
#define SGNLlupqdusol       sgnllupqdusol_
#define SGNLlupqtdusol      sgnllupqtdusol_
#define SSPDldlp            sspdldlp_
#define SSPDldlpsol         sspdldlpsol_

#define SGNLilutp           sgnlilutp_
#define SGNLilut            sgnlilut_
#define SGNLlusol           sgnllusol_
#define SGNLlutsol          sgnllutsol_
#define SGNLlulsol          sgnllulsol_
#define SGNLlutlsol         sgnllutlsol_
#define SGNLluusol          sgnlluusol_
#define SGNLlutusol         sgnllutusol_
#define SGNLludlsol         sgnlludlsol_
#define SGNLlutdlsol        sgnllutdlsol_
#define SGNLludusol         sgnlludusol_
#define SGNLlutdusol        sgnllutdusol_

#define SGNLiluc            sgnliluc_
#define SGNLilucsol         sgnlilucsol_
#define SGNLiluctsol        sgnliluctsol_
#define SGNLilucdlsol       sgnlilucdlsol_
#define SGNLiluctdlsol      sgnliluctdlsol_
#define SGNLilucdusol       sgnlilucdusol_
#define SGNLiluctdusol      sgnliluctdusol_
#define SGNLiluclsol        sgnliluclsol_
#define SGNLiluctlsol       sgnliluctlsol_
#define SGNLilucusol        sgnlilucusol_
#define SGNLiluctusol       sgnliluctusol_

#define SGNLpilucdlsol      sgnlpilucdlsol_
#define SGNLpiluctdlsol     sgnlpiluctdlsol_
#define SGNLpilucdusol      sgnlpilucdusol_
#define SGNLpiluctdusol     sgnlpiluctdusol_
#define SGNLpiluclsol       sgnlpiluclsol_
#define SGNLpiluctlsol      sgnlpiluctlsol_
#define SGNLpilucusol       sgnlpilucusol_
#define SGNLpiluctusol      sgnlpiluctusol_

#define SSYMildlc           ssymildlc_
#define SSYMildlcsol        ssymildlcsol_
#define SSSMildlc           sssmildlc_
#define SSSMildlcsol        sssmildlcsol_
#define SGNLpiluc           sgnlpiluc_
#define SGNLspiluc          sgnlspiluc_
#define SGNLmpiluc          sgnlmpiluc_
#define SSPDpiluc           sspdpiluc_
#define SSPDmpiluc          sspdmpiluc_
#define SSYMpiluc           ssympiluc_
#define SSYMiluc            ssymiluc_
#define SSYMmpiluc          ssymmpiluc_
#define SSYMpilucsol        ssympilucsol_
#define SSYMpiluclsol       ssympiluclsol_
#define SSYMpilucusol       ssympilucusol_


#define Spcg                spcg_
#define Sbcg                sbcg_
#define SSYMbcg             ssymbcg_
#define SSYMqmr             ssymqmr_
#define Sgmres              sgmres_
#define Sfgmres             sfgmres_
#define Sdistdot            sdistdot_


#define Sroscal             sroscal_
#define Scoscal             scoscal_
#define Srowscale           srowscale_
#define Scolscale           scolscale_
#define SSPDscale           sspdscale_
#define SSYMscale           ssymscale_
#define Scsrcsc             scsrcsc_
#define Sqsort              sqsort_
#define Sqqsort             sqqsort_
#define Sqqsort2            sqqsort2_
#define Sqqsorts            sqqsorts_
#define Sqqsorts2           sqqsorts2_

#define Sreadmtc            sreadmtc_
#define Swritemtc           swritemtc_
#define Sreadvectors        sreadvectors_
#define Swritevectors       swritevectors_


#define zsymilupack         zsymilupack_
#define zsymilupackfac      zsymilupackfac_
#define zsymilupacksol      zsymilupacksol_
#define zsymilupackdel      zsymilupackdel_
#define zherilupack         zherilupack_
#define zherilupackfac      zherilupackfac_
#define zherilupacksol      zherilupacksol_
#define zherilupackdel      zherilupackdel_

#define ZGNLlupq            zgnllupq_
#define ZGNLlupqsol         zgnllupqsol_
#define ZGNLlupqtsol        zgnllupqtsol_
#define ZGNLlupqhsol        zgnllupqhsol_
#define ZGNLlupqlsol        zgnllupqlsol_
#define ZGNLlupqtlsol       zgnllupqtlsol_
#define ZGNLlupqusol        zgnllupqusol_
#define ZGNLlupqtusol       zgnllupqtusol_
#define ZGNLlupqdlsol       zgnllupqdlsol_
#define ZGNLlupqtdlsol      zgnllupqtdlsol_
#define ZGNLlupqdusol       zgnllupqdusol_
#define ZGNLlupqtdusol      zgnllupqtdusol_
#define ZHPDldlp            zhpdldlp_
#define ZHPDldlpsol         zhpdldlpsol_

#define ZGNLilutp           zgnlilutp_
#define ZGNLilut            zgnlilut_
#define ZGNLlusol           zgnllusol_
#define ZGNLlutsol          zgnllutsol_
#define ZGNLlulsol          zgnllulsol_
#define ZGNLlutlsol         zgnllutlsol_
#define ZGNLluusol          zgnlluusol_
#define ZGNLlutusol         zgnllutusol_
#define ZGNLludlsol         zgnlludlsol_
#define ZGNLlutdlsol        zgnllutdlsol_
#define ZGNLludusol         zgnlludusol_
#define ZGNLlutdusol        zgnllutdusol_

#define ZGNLiluc            zgnliluc_
#define ZGNLilucsol         zgnlilucsol_
#define ZGNLiluctsol        zgnliluctsol_
#define ZGNLilucdlsol       zgnlilucdlsol_
#define ZGNLiluctdlsol      zgnliluctdlsol_
#define ZGNLilucdusol       zgnlilucdusol_
#define ZGNLiluctdusol      zgnliluctdusol_
#define ZGNLiluclsol        zgnliluclsol_
#define ZGNLiluctlsol       zgnliluctlsol_
#define ZGNLilucusol        zgnlilucusol_
#define ZGNLiluctusol       zgnliluctusol_

#define ZGNLpilucdlsol      zgnlpilucdlsol_
#define ZGNLpiluctdlsol     zgnlpiluctdlsol_
#define ZGNLpilucdusol      zgnlpilucdusol_
#define ZGNLpiluctdusol     zgnlpiluctdusol_
#define ZGNLpiluclsol       zgnlpiluclsol_
#define ZGNLpiluctlsol      zgnlpiluctlsol_
#define ZGNLpilucusol       zgnlpilucusol_
#define ZGNLpiluctusol      zgnlpiluctusol_

#define ZHERildlc           zherildlc_
#define ZHERildlcsol        zherildlcsol_
#define ZSHRildlc           zshrildlc_
#define ZSHRildlcsol        zshrildlcsol_
#define ZSYMildlc           zsymildlc_
#define ZSYMildlcsol        zsymildlcsol_
#define ZSSMildlc           zssmildlc_
#define ZSSMildlcsol        zssmildlcsol_
#define ZGNLpiluc           zgnlpiluc_
#define ZGNLspiluc          zgnlspiluc_
#define ZGNLmpiluc          zgnlmpiluc_
#define ZHPDpiluc           zhpdpiluc_
#define ZHPDmpiluc          zhpdmpiluc_
#define ZSYMpiluc           zsympiluc_
#define ZSYMiluc            zsymiluc_
#define ZSYMmpiluc          zsymmpiluc_
#define ZSYMpilucsol        zsympilucsol_
#define ZHERpiluc           zherpiluc_
#define ZHERiluc            zheriluc_
#define ZHERmpiluc          zhermpiluc_
#define ZHERpilucsol        zherpilucsol_
#define ZSYMpiluclsol       zsympiluclsol_
#define ZSYMpilucusol       zsympilucusol_
#define ZHERpiluclsol       zherpiluclsol_
#define ZHERpilucusol       zherpilucusol_


#define Zpcg                zpcg_
#define Zbcg                zbcg_
#define ZSYMbcg             zsymbcg_
#define ZHERbcg             zherbcg_
#define ZSYMqmr             zsymqmr_
#define ZHERqmr             zherqmr_
#define Zgmres              zgmres_
#define Zfgmres             zfgmres_
#define Zdistdotc           zdistdotc_
#define Zdistdotu           zdistdotu_


#define Zroscal             zroscal_
#define Zcoscal             zcoscal_
#define Zrowscale           zrowscale_
#define Zcolscale           zcolscale_
#define ZHPDscale           zhpdscale_
#define ZSYMscale           zsymscale_
#define ZHERscale           zherscale_
#define Zcsrcsc             zcsrcsc_
#define Zqsort              zqsort_
#define Zqqsort             zqqsort_
#define Zqqsort2            zqqsort2_
#define Zqqsorts            zqqsorts_
#define Zqqsorts2           zqqsorts2_

#define Zreadmtc            zreadmtc_
#define Zwritemtc           zwritemtc_
#define Zreadvectors        zreadvectors_
#define Zwritevectors       zwritevectors_



#define csymilupack         csymilupack_
#define csymilupackfac      csymilupackfac_
#define csymilupacksol      csymilupacksol_
#define csymilupackdel      csymilupackdel_
#define cherilupack         cherilupack_
#define cherilupackfac      cherilupackfac_
#define cherilupacksol      cherilupacksol_
#define cherilupackdel      cherilupackdel_

#define CGNLlupq            cgnllupq_
#define CGNLlupqsol         cgnllupqsol_
#define CGNLlupqtsol        cgnllupqtsol_
#define CGNLlupqhsol        cgnllupqhsol_
#define CGNLlupqlsol        cgnllupqlsol_
#define CGNLlupqtlsol       cgnllupqtlsol_
#define CGNLlupqusol        cgnllupqusol_
#define CGNLlupqtusol       cgnllupqtusol_
#define CGNLlupqdlsol       cgnllupqdlsol_
#define CGNLlupqtdlsol      cgnllupqtdlsol_
#define CGNLlupqdusol       cgnllupqdusol_
#define CGNLlupqtdusol      cgnllupqtdusol_
#define CHPDldlp            chpdldlp_
#define CHPDldlpsol         chpdldlpsol_

#define CGNLilutp           cgnlilutp_
#define CGNLilut            cgnlilut_
#define CGNLlusol           cgnllusol_
#define CGNLlutsol          cgnllutsol_
#define CGNLlulsol          cgnllulsol_
#define CGNLlutlsol         cgnllutlsol_
#define CGNLluusol          cgnlluusol_
#define CGNLlutusol         cgnllutusol_
#define CGNLludlsol         cgnlludlsol_
#define CGNLlutdlsol        cgnllutdlsol_
#define CGNLludusol         cgnlludusol_
#define CGNLlutdusol        cgnllutdusol_

#define CGNLiluc            cgnliluc_
#define CGNLilucsol         cgnlilucsol_
#define CGNLiluctsol        cgnliluctsol_
#define CGNLilucdlsol       cgnlilucdlsol_
#define CGNLiluctdlsol      cgnliluctdlsol_
#define CGNLilucdusol       cgnlilucdusol_
#define CGNLiluctdusol      cgnliluctdusol_
#define CGNLiluclsol        cgnliluclsol_
#define CGNLiluctlsol       cgnliluctlsol_
#define CGNLilucusol        cgnlilucusol_
#define CGNLiluctusol       cgnliluctusol_

#define CGNLpilucdlsol      cgnlpilucdlsol_
#define CGNLpiluctdlsol     cgnlpiluctdlsol_
#define CGNLpilucdusol      cgnlpilucdusol_
#define CGNLpiluctdusol     cgnlpiluctdusol_
#define CGNLpiluclsol       cgnlpiluclsol_
#define CGNLpiluctlsol      cgnlpiluctlsol_
#define CGNLpilucusol       cgnlpilucusol_
#define CGNLpiluctusol      cgnlpiluctusol_

#define CHERildlc           cherildlc_
#define CHERildlcsol        cherildlcsol_
#define CSHRildlc           cshrildlc_
#define CSHRildlcsol        cshrildlcsol_
#define CSYMildlc           csymildlc_
#define CSYMildlcsol        csymildlcsol_
#define CSSMildlc           cssmildlc_
#define CSSMildlcsol        cssmildlcsol_
#define CGNLpiluc           cgnlpiluc_
#define CGNLspiluc          cgnlspiluc_
#define CGNLmpiluc          cgnlmpiluc_
#define CHPDpiluc           chpdpiluc_
#define CHPDmpiluc          chpdmpiluc_
#define CSYMpiluc           csympiluc_
#define CSYMiluc            csymiluc_
#define CSYMmpiluc          csymmpiluc_
#define CSYMpilucsol        csympilucsol_
#define CHERpiluc           cherpiluc_
#define CHERiluc            cheriluc_
#define CHERmpiluc          chermpiluc_
#define CHERpilucsol        cherpilucsol_
#define CSYMpiluclsol       csympiluclsol_
#define CSYMpilucusol       csympilucusol_
#define CHERpiluclsol       cherpiluclsol_
#define CHERpilucusol       cherpilucusol_


#define Cpcg                cpcg_
#define Cbcg                cbcg_
#define CSYMbcg             csymbcg_
#define CHERbcg             cherbcg_
#define CSYMqmr             csymqmr_
#define CHERqmr             cherqmr_
#define Cgmres              cgmres_
#define Cfgmres             cfgmres_
#define Cdistdotc           cdistdotc_
#define Cdistdotu           cdistdotu_


#define Croscal             croscal_
#define Ccoscal             ccoscal_
#define Crowscale           crowscale_
#define Ccolscale           ccolscale_
#define CHPDscale           chpdscale_
#define CSYMscale           csymscale_
#define CHERscale           cherscale_
#define Ccsrcsc             ccsrcsc_
#define Cqsort              cqsort_
#define Cqqsort             cqqsort_
#define Cqqsort2            cqqsort2_
#define Cqqsorts            cqqsorts_
#define Cqqsorts2           cqqsorts2_

#define Creadmtc            creadmtc_
#define Cwritemtc           cwritemtc_
#define Creadvectors        creadvectors_
#define Cwritevectors       cwritevectors_






/* both are defined */
#elif defined __CAPS__ && defined __UNDERSCORE__ && !defined __2UNDERSCORES__
#define mysdot                    MYSDOT_
#define myddot                    MYDDOT_
#define mycdotc                   MYCDOTC_
#define myzdotc                   MYZDOTC_
#define sdsymamgsavediag          SDSYMAMGSAVEDIAG_	    
#define sdsymamgsavediaggep       SDSYMAMGSAVEDIAGGEP_
#define sdsymamgsol		  SDSYMAMGSOL_		    
#define sdsymamginit		  SDSYMAMGINIT_		    
#define sdsymamgrestorediag	  SDSYMAMGRESTOREDIAG_	    
#define sdamgundoscaling	  SDAMGUNDOSCALING_	    
#define sdsymamgfactor		  SDSYMAMGFACTOR_
#define sdsymamgfactorgep	  SDSYMAMGFACTORGEP_

#define czheramgsavediag	  CZHERAMGSAVEDIAG_	    
#define czheramgsavediaggep	  CZHERAMGSAVEDIAGGEP_
#define czheramgsol		  CZHERAMGSOL_		    
#define czheramginit		  CZHERAMGINIT_		    
#define czheramgrestorediag	  CZHERAMGRESTOREDIAG_	    
#define czamgundoscaling	  CZAMGUNDOSCALING_	    
#define czheramgfactor            CZHERAMGFACTOR_              
#define czheramgfactorgep         CZHERAMGFACTORGEP_


#define iprandom            IPRANDOM_
#define ipsrandom           IPSRANDOM_
#define evaluatetime        EVALUATETIME_
#define dprivatesptrs       DPRIVATESPTRS_
#define sprivatesptrs       SPRIVATESPTRS_
#define cprivatehptrs       CPRIVATEHPTRS_
#define zprivatehptrs       ZPRIVATEHPTRS_

#define sdglprecsetup        SDGLPRECSETUP_
#define sdglprecsol          SDGLPRECSOL_
#define sdglprecdelete       SDGLPRECDELETE_
#define ddglprecsetup        DDGLPRECSETUP_
#define ddglprecsol          DDGLPRECSOL_
#define ddglprecdelete       DDGLPRECDELETE_

#define cdglprecsetup       CDGLPRECSETUP_
#define cdglprecsol         CDGLPRECSOL_
#define cdglprecdelete      CDGLPRECDELETE_
#define zdglprecsetup       ZDGLPRECSETUP_
#define zdglprecsol         ZDGLPRECSOL_
#define zdglprecdelete      ZDGLPRECDELETE_

#define dsymamgsavediag     DSYMAMGSAVEDIAG_
#define dsymamgsavediaggep  DSYMAMGSAVEDIAGGEP_
#define dsymamgrestorediag  DSYMAMGRESTOREDIAG_
#define ssymamgsavediag     SSYMAMGSAVEDIAG_	  
#define ssymamgsavediaggep  SSYMAMGSAVEDIAGGEP_	  
#define ssymamgrestorediag  SSYMAMGRESTOREDIAG_
#define cheramgsavediag     CHERAMGSAVEDIAG_	  
#define cheramgsavediaggep  CHERAMGSAVEDIAGGEP_ 
#define cheramgrestorediag  CHERAMGRESTOREDIAG_
#define zheramgsavediag     ZHERAMGSAVEDIAG_	  
#define zheramgsavediaggep  ZHERAMGSAVEDIAGGEP_
#define zheramgrestorediag  ZHERAMGRESTOREDIAG_

#define samgundoscaling     SAMGUNDOSCALING_
#define damgundoscaling     DAMGUNDOSCALING_
#define camgundoscaling     CAMGUNDOSCALING_
#define zamgundoscaling     ZAMGUNDOSCALING_

#define sgnlamginit         SGNLAMGINIT_           
#define sgnlamgfactor       SGNLAMGFACTOR_        
#define sgnlamgsolver       SGNLAMGSOLVER_  
#define sgnlamgsol          SGNLAMGSOL_   
#define sgnlamgdelete       SGNLAMGDELETE_        
#define sgnlamginfo         SGNLAMGINFO_
#define sgnlamgnnz          SGNLAMGNNZ_
                                              
#define ssymspdamgconvert   SSYMSPDAMGCONVERT_

#define sspdamginit         SSPDAMGINIT_          
#define sspdamgfactor       SSPDAMGFACTOR_        
#define sspdamgsolver       SSPDAMGSOLVER_        
#define sspdamgsol          SSPDAMGSOL_   
#define sspdamgdelete       SSPDAMGDELETE_        
#define sspdamginfo         SSPDAMGINFO_
#define sspdamgnnz          SSPDAMGNNZ_
                                              
#define ssymamginit         SSYMAMGINIT_          
#define ssymamgfactor       SSYMAMGFACTOR_        
#define ssymamgfactorgep    SSYMAMGFACTORGEP_
#define ssymamgsolver       SSYMAMGSOLVER_        
#define ssymamgsol          SSYMAMGSOL_   
#define ssymamgdelete       SSYMAMGDELETE_        
#define ssymamginfo         SSYMAMGINFO_
#define ssymamgnnz          SSYMAMGNNZ_
                                              
                                              
#define dgnlamginit         DGNLAMGINIT_          
#define dgnlamgfactor       DGNLAMGFACTOR_        
#define dgnlamgsolver       DGNLAMGSOLVER_        
#define dgnlamgsol          DGNLAMGSOL_   
#define dgnlamgdelete       DGNLAMGDELETE_        
#define dgnlamginfo         DGNLAMGINFO_
#define dgnlamgnnz          DGNLAMGNNZ_
                                              
#define dsymspdamgconvert   DSYMSPDAMGCONVERT_

#define dspdamginit         DSPDAMGINIT_          
#define dspdamgfactor       DSPDAMGFACTOR_        
#define dspdamgsolver       DSPDAMGSOLVER_        
#define dspdamgsol          DSPDAMGSOL_   
#define dspdamgdelete       DSPDAMGDELETE_        
#define dspdamginfo         DSPDAMGINFO_
#define dspdamgnnz          DSPDAMGNNZ_
                                              
#define dsymamginit         DSYMAMGINIT_          
#define dsymamgfactor       DSYMAMGFACTOR_        
#define dsymamgfactorgep    DSYMAMGFACTORGEP_
#define dsymamgsolver       DSYMAMGSOLVER_        
#define dsymamgsol          DSYMAMGSOL_   
#define dsymamgdelete       DSYMAMGDELETE_        
#define dsymamginfo         DSYMAMGINFO_
#define dsymamgnnz          DSYMAMGNNZ_
                                              
                                              
#define cgnlamginit         CGNLAMGINIT_          
#define cgnlamgfactor       CGNLAMGFACTOR_        
#define cgnlamgsolver       CGNLAMGSOLVER_        
#define cgnlamgsol          CGNLAMGSOL_   
#define cgnlamgdelete       CGNLAMGDELETE_        
#define cgnlamginfo         CGNLAMGINFO_
#define cgnlamgnnz          CGNLAMGNNZ_
                                              
#define cherhpdamgconvert   CHERHPDAMGCONVERT_

#define chpdamginit         CHPDAMGINIT_          
#define chpdamgfactor       CHPDAMGFACTOR_        
#define chpdamgsolver       CHPDAMGSOLVER_        
#define chpdamgsol          CHPDAMGSOL_   
#define chpdamgdelete       CHPDAMGDELETE_        
#define chpdamginfo         CHPDAMGINFO_
#define chpdamgnnz          CHPDAMGNNZ_
                                              
#define cheramginit         CHERAMGINIT_          
#define cheramgfactor       CHERAMGFACTOR_        
#define cheramgfactorgep    CHERAMGFACTORGEP_        
#define cheramgsolver       CHERAMGSOLVER_        
#define cheramgsol          CHERAMGSOL_   
#define cheramgdelete       CHERAMGDELETE_        
#define cheramginfo         CHERAMGINFO_
#define cheramgnnz          CHERAMGNNZ_
                                              
#define csymamginit         CSYMAMGINIT_          
#define csymamgfactor       CSYMAMGFACTOR_        
#define csymamgsolver       CSYMAMGSOLVER_        
#define csymamgsol          CSYMAMGSOL_   
#define csymamgdelete       CSYMAMGDELETE_        
#define csymamginfo         CSYMAMGINFO_
#define csymamgnnz          CSYMAMGNNZ_
                                              
                                              
#define zgnlamginit         ZGNLAMGINIT_          
#define zgnlamgfactor       ZGNLAMGFACTOR_        
#define zgnlamgsolver       ZGNLAMGSOLVER_        
#define zgnlamgsol          ZGNLAMGSOL_   
#define zgnlamgdelete       ZGNLAMGDELETE_        
#define zgnlamginfo         ZGNLAMGINFO_
#define zgnlamgnnz          ZGNLAMGNNZ_
                                              
#define zherhpdamgconvert   ZHERHPDAMGCONVERT_

#define zhpdamginit         ZHPDAMGINIT_          
#define zhpdamgfactor       ZHPDAMGFACTOR_        
#define zhpdamgsolver       ZHPDAMGSOLVER_        
#define zhpdamgsol          ZHPDAMGSOL_   
#define zhpdamgdelete       ZHPDAMGDELETE_        
#define zhpdamginfo         ZHPDAMGINFO_
#define zhpdamgnnz          ZHPDAMGNNZ_
                                              
#define zheramginit         ZHERAMGINIT_          
#define zheramgfactor       ZHERAMGFACTOR_        
#define zheramgfactorgep    ZHERAMGFACTORGEP_
#define zheramgsolver       ZHERAMGSOLVER_        
#define zheramgsol          ZHERAMGSOL_   
#define zheramgdelete       ZHERAMGDELETE_        
#define zheramginfo         ZHERAMGINFO_
#define zheramgnnz          ZHERAMGNNZ_
                                              
#define zsymamginit         ZSYMAMGINIT_          
#define zsymamgfactor       ZSYMAMGFACTOR_        
#define zsymamgsolver       ZSYMAMGSOLVER_        
#define zsymamgsol          ZSYMAMGSOL_   
#define zsymamgdelete       ZSYMAMGDELETE_     
#define zsymamginfo         ZSYMAMGINFO_
#define zsymamgnnz          ZSYMAMGNNZ_


#define qqsorti             QQSORTI_

#define dsymilupack         DSYMILUPACK_
#define dsymilupackfac      DSYMILUPACKFAC_
#define dsymilupacksol      DSYMILUPACKSOL_
#define dsymilupackdel      DSYMILUPACKDEL_

#define DGNLlupq            DLUPQ_
#define DGNLlupqsol         DLUPQSOL_
#define DGNLlupqtsol        DGNLLUPQTSOL_
#define DGNLlupqlsol        DGNLLUPQLSOL_
#define DGNLlupqtlsol       DGNLLUPQTLSOL_
#define DGNLlupqusol        DGNLLUPQUSOL_
#define DGNLlupqtusol       DGNLLUPQTUSOL_
#define DGNLlupqdlsol       DGNLLUPQDLSOL_
#define DGNLlupqtdlsol      DGNLLUPQTDLSOL_
#define DGNLlupqdusol       DGNLLUPQDUSOL_
#define DGNLlupqtdusol      DGNLLUPQTDUSOL_
#define DSPDldlp            DSPDLDLP_
#define DSPDldlpsol         DSPDLDLPSOL_

#define DGNLilutp           DGNLILUTP_
#define DGNLilut            DGNLILUT_
#define DGNLlusol           DGNLLUSOL_
#define DGNLlutsol          DGNLLUTSOL_
#define DGNLlulsol          DGNLLULSOL_
#define DGNLlutlsol         DGNLLUTLSOL_
#define DGNLluusol          DGNLLUUSOL_
#define DGNLlutusol         DGNLLUTUSOL_
#define DGNLludlsol         DGNLLUDLSOL_
#define DGNLlutdlsol        DGNLLUTDLSOL_
#define DGNLludusol         DGNLLUDUSOL_
#define DGNLlutdusol        DGNLLUTDUSOL_

#define DGNLiluc            DGNLILUC_
#define DGNLilucsol         DGNLILUCSOL_
#define DGNLiluctsol        DGNLILUCTSOL_
#define DGNLilucdlsol       DGNLILUCDLSOL_
#define DGNLiluctdlsol      DGNLILUCTDLSOL_
#define DGNLilucdusol       DGNLILUCDUSOL_
#define DGNLiluctdusol      DGNLILUCTDUSOL_
#define DGNLiluclsol        DGNLILUCLSOL_
#define DGNLiluctlsol       DGNLILUCTLSOL_
#define DGNLilucusol        DGNLILUCUSOL_
#define DGNLiluctusol       DGNLILUCTUSOL_

#define DGNLpilucdlsol      DGNLPILUCDLSOL_
#define DGNLpiluctdlsol     DGNLPILUCTDLSOL_
#define DGNLpilucdusol      DGNLPILUCDUSOL_
#define DGNLpiluctdusol     DGNLPILUCTDUSOL_
#define DGNLpiluclsol       DGNLPILUCLSOL_
#define DGNLpiluctlsol      DGNLPILUCTLSOL_
#define DGNLpilucusol       DGNLPILUCUSOL_
#define DGNLpiluctusol      DGNLPILUCTUSOL_

#define DSYMildlc           DSYMILDLC_
#define DSYMildlcsol        DSYMILDLCSOL_
#define DSSMildlc           DSSMILDLC_
#define DSSMildlcsol        DSSMILDLCSOL_
#define DGNLpiluc           DGNLPILUC_
#define DGNLspiluc          DGNLSPILUC_
#define DGNLmpiluc          DGNLMPILUC_
#define DSPDpiluc           DSPDPILUC_
#define DSPDmpiluc          DSPDMPILUC_
#define DSYMpiluc           DSYMPILUC_
#define DSYMiluc            DSYMILUC_
#define DSYMmpiluc          DSYMMPILUC_
#define DSYMpilucsol        DSYMPILUCSOL_
#define DSYMpiluclsol       DSYMPILUCLSOL_
#define DSYMpilucusol       DSYMPILUCUSOL_


#define Dpcg                DPCG_
#define Dbcg                DBCG_
#define DSYMbcg             DSYMBCG_
#define DSYMqmr             DSYMQMR_
#define Dgmres              DGMRES_
#define Dfgmres             DFGMRES_
#define Ddistdot            DDISTDOT_


#define Droscal             DROSCAL_
#define Dcoscal             DCOSCAL_
#define Drowscale           DROWSCALE_
#define Dcolscale           DCOLSCALE_
#define DSPDscale           DSPDSCALE_
#define DSYMscale           DSYMSCALE_
#define Dcsrcsc             DCSRCSC_
#define Dqsort              DQSORT_
#define Dqqsort             DQQSORT_
#define Dqqsort2            DQQSORT2_
#define Dqqsorts            DQQSORTS_
#define Dqqsorts2           DQQSORTS2_

#define Dreadmtc            DREADMTC_
#define Dwritemtc           DWRITEMTC_
#define Dreadvectors        DREADVECTORS_
#define Dwritevectors       DWRITEVECTORS_



#define ssymilupack         SSYMILUPACK_
#define ssymilupackfac      SSYMILUPACKFAC_
#define ssymilupacksol      SSYMILUPACKSOL_
#define ssymilupackdel      SSYMILUPACKDEL_

#define SGNLlupq            SLUPQ_
#define SGNLlupqsol         SLUPQSOL_
#define SGNLlupqtsol        SGNLLUPQTSOL_
#define SGNLlupqlsol        SGNLLUPQLSOL_
#define SGNLlupqtlsol       SGNLLUPQTLSOL_
#define SGNLlupqusol        SGNLLUPQUSOL_
#define SGNLlupqtusol       SGNLLUPQTUSOL_
#define SGNLlupqdlsol       SGNLLUPQDLSOL_
#define SGNLlupqtdlsol      SGNLLUPQTDLSOL_
#define SGNLlupqdusol       SGNLLUPQDUSOL_
#define SGNLlupqtdusol      SGNLLUPQTDUSOL_
#define SSPDldlp            SSPDLDLP_
#define SSPDldlpsol         SSPDLDLPSOL_

#define SGNLilutp           SGNLILUTP_
#define SGNLilut            SGNLILUT_
#define SGNLlusol           SGNLLUSOL_
#define SGNLlutsol          SGNLLUTSOL_
#define SGNLlulsol          SGNLLULSOL_
#define SGNLlutlsol         SGNLLUTLSOL_
#define SGNLluusol          SGNLLUUSOL_
#define SGNLlutusol         SGNLLUTUSOL_
#define SGNLludlsol         SGNLLUDLSOL_
#define SGNLlutdlsol        SGNLLUTDLSOL_
#define SGNLludusol         SGNLLUDUSOL_
#define SGNLlutdusol        SGNLLUTDUSOL_

#define SGNLiluc            SGNLILUC_
#define SGNLilucsol         SGNLILUCSOL_
#define SGNLiluctsol        SGNLILUCTSOL_
#define SGNLilucdlsol       SGNLILUCDLSOL_
#define SGNLiluctdlsol      SGNLILUCTDLSOL_
#define SGNLilucdusol       SGNLILUCDUSOL_
#define SGNLiluctdusol      SGNLILUCTDUSOL_
#define SGNLiluclsol        SGNLILUCLSOL_
#define SGNLiluctlsol       SGNLILUCTLSOL_
#define SGNLilucusol        SGNLILUCUSOL_
#define SGNLiluctusol       SGNLILUCTUSOL_

#define SGNLpilucdlsol      SGNLPILUCDLSOL_
#define SGNLpiluctdlsol     SGNLPILUCTDLSOL_
#define SGNLpilucdusol      SGNLPILUCDUSOL_
#define SGNLpiluctdusol     SGNLPILUCTDUSOL_
#define SGNLpiluclsol       SGNLPILUCLSOL_
#define SGNLpiluctlsol      SGNLPILUCTLSOL_
#define SGNLpilucusol       SGNLPILUCUSOL_
#define SGNLpiluctusol      SGNLPILUCTUSOL_

#define SSYMildlc           SSYMILDLC_
#define SSYMildlcsol        SSYMILDLCSOL_
#define SSSMildlc           SSSMILDLC_
#define SSSMildlcsol        SSSMILDLCSOL_
#define SGNLpiluc           SGNLPILUC_
#define SGNLspiluc          SGNLSPILUC_
#define SGNLmpiluc          SGNLMPILUC_
#define SSPDpiluc           SSPDPILUC_
#define SSPDmpiluc          SSPDMPILUC_
#define SSYMpiluc           SSYMPILUC_
#define SSYMiluc            SSYMILUC_
#define SSYMmpiluc          SSYMMPILUC_
#define SSYMpilucsol        SSYMPILUCSOL_
#define SSYMpiluclsol       SSYMPILUCLSOL_
#define SSYMpilucusol       SSYMPILUCUSOL_


#define Spcg                SPCG_
#define Sbcg                SBCG_
#define SSYMbcg             SSYMBCG_
#define SSYMqmr             SSYMQMR_
#define Sgmres              SGMRES_
#define Sfgmres             SFGMRES_
#define Sdistdot            SDISTDOT_


#define Sroscal             SROSCAL_
#define Scoscal             SCOSCAL_
#define Srowscale           SROWSCALE_
#define Scolscale           SCOLSCALE_
#define SSPDscale           SSPDSCALE_
#define SSYMscale           SSYMSCALE_
#define Scsrcsc             SCSRCSC_
#define Sqsort              SQSORT_
#define Sqqsort             SQQSORT_
#define Sqqsort2            SQQSORT2_
#define Sqqsorts            SQQSORTS_
#define Sqqsorts2           SQQSORTS2_

#define Sreadmtc            SREADMTC_
#define Swritemtc           SWRITEMTC_
#define Sreadvectors        SREADVECTORS_
#define Swritevectors       SWRITEVECTORS_



#define zsymilupack         ZSYMILUPACK_
#define zsymilupackfac      ZSYMILUPACKFAC_
#define zsymilupacksol      ZSYMILUPACKSOL_
#define zsymilupackdel      ZSYMILUPACKDEL_
#define zherilupack         ZHERILUPACK_
#define zherilupackfac      ZHERILUPACKFAC_
#define zherilupacksol      ZHERILUPACKSOL_
#define zherilupackdel      ZHERILUPACKDEL_

#define ZGNLlupq            ZGNLLUPQ_
#define ZGNLlupqsol         ZGNLLUPQSOL_
#define ZGNLlupqtsol        ZGNLLUPQTSOL_
#define ZGNLlupqlsol        ZGNLLUPQLSOL_
#define ZGNLlupqtlsol       ZGNLLUPQTLSOL_
#define ZGNLlupqusol        ZGNLLUPQUSOL_
#define ZGNLlupqtusol       ZGNLLUPQTUSOL_
#define ZGNLlupqdlsol       ZGNLLUPQDLSOL_
#define ZGNLlupqtdlsol      ZGNLLUPQTDLSOL_
#define ZGNLlupqdusol       ZGNLLUPQDUSOL_
#define ZGNLlupqtdusol      ZGNLLUPQTDUSOL_
#define ZHPDldlp            ZHPDLDLP_
#define ZHPDldlpsol         ZHPDLDLPSOL_

#define ZGNLilutp           ZGNLILUTP_
#define ZGNLilut            ZGNLILUT_
#define ZGNLlusol           ZGNLLUSOL_
#define ZGNLlutsol          ZGNLLUTSOL_
#define ZGNLlulsol          ZGNLLULSOL_
#define ZGNLlutlsol         ZGNLLUTLSOL_
#define ZGNLluusol          ZGNLLUUSOL_
#define ZGNLlutusol         ZGNLLUTUSOL_
#define ZGNLludlsol         ZGNLLUDLSOL_
#define ZGNLlutdlsol        ZGNLLUTDLSOL_
#define ZGNLludusol         ZGNLLUDUSOL_
#define ZGNLlutdusol        ZGNLLUTDUSOL_

#define ZGNLiluc            ZGNLILUC_
#define ZGNLilucsol         ZGNLILUCSOL_
#define ZGNLiluctsol        ZGNLILUCTSOL_
#define ZGNLilucdlsol       ZGNLILUCDLSOL_
#define ZGNLiluctdlsol      ZGNLILUCTDLSOL_
#define ZGNLilucdusol       ZGNLILUCDUSOL_
#define ZGNLiluctdusol      ZGNLILUCTDUSOL_
#define ZGNLiluclsol        ZGNLILUCLSOL_
#define ZGNLiluctlsol       ZGNLILUCTLSOL_
#define ZGNLilucusol        ZGNLILUCUSOL_
#define ZGNLiluctusol       ZGNLILUCTUSOL_

#define ZGNLpilucdlsol      ZGNLPILUCDLSOL_
#define ZGNLpiluctdlsol     ZGNLPILUCTDLSOL_
#define ZGNLpilucdusol      ZGNLPILUCDUSOL_
#define ZGNLpiluctdusol     ZGNLPILUCTDUSOL_
#define ZGNLpiluclsol       ZGNLPILUCLSOL_
#define ZGNLpiluctlsol      ZGNLPILUCTLSOL_
#define ZGNLpilucusol       ZGNLPILUCUSOL_
#define ZGNLpiluctusol      ZGNLPILUCTUSOL_

#define ZHERildlc           ZHERILDLC_
#define ZHERildlcsol        ZHERILDLCSOL_
#define ZSHRildlc           ZSHRILDLC_
#define ZSHRildlcsol        ZSHRILDLCSOL_
#define ZSYMildlc           ZSYMILDLC_
#define ZSYMildlcsol        ZSYMILDLCSOL_
#define ZSSMildlc           ZSSMILDLC_
#define ZSSMildlcsol        ZSSMILDLCSOL_
#define ZGNLpiluc           ZGNLPILUC_
#define ZGNLspiluc          ZGNLSPILUC_
#define ZGNLmpiluc          ZGNLMPILUC_
#define ZHPDpiluc           ZHPDPILUC_
#define ZHPDmpiluc          ZHPDMPILUC_
#define ZSYMpiluc           ZSYMPILUC_
#define ZSYMiluc            ZSYMILUC_
#define ZSYMmpiluc          ZSYMMPILUC_
#define ZSYMpilucsol        ZSYMPILUCSOL_
#define ZHERpiluc           ZHERPILUC_
#define ZHERiluc            ZHERILUC_
#define ZHERmpiluc          ZHERMPILUC_
#define ZHERpilucsol        ZHERPILUCSOL_
#define ZSYMpiluclsol       ZSYMPILUCLSOL_
#define ZSYMpilucusol       ZSYMPILUCUSOL_
#define ZHERpiluclsol       ZHERPILUCLSOL_
#define ZHERpilucusol       ZHERPILUCUSOL_


#define Zpcg                ZPCG_
#define Zbcg                ZBCG_
#define ZSYMbcg             ZSYMBCG_
#define ZHERbcg             ZHERBCG_
#define ZSYMqmr             ZSYMQMR_
#define ZHERqmr             ZHERQMR_
#define Zgmres              ZGMRES_
#define Zfgmres             ZFGMRES_
#define Zdistdotc           ZDISTDOTC_
#define Zdistdotu           ZDISTDOTU_


#define Zroscal             ZROSCAL_
#define Zcoscal             ZCOSCAL_
#define Zrowscale           ZROWSCALE_
#define Zcolscale           ZCOLSCALE_
#define ZHPDscale           ZHPDSCALE_
#define ZSYMscale           ZSYMSCALE_
#define ZHERscale           ZHERSCALE_
#define Zcsrcsc             ZCSRCSC_
#define Zqsort              ZQSORT_
#define Zqqsort             ZQQSORT_
#define Zqqsort2            ZQQSORT2_
#define Zqqsorts            ZQQSORTS_
#define Zqqsorts2           ZQQSORTS2_

#define Zreadmtc            ZREADMTC_
#define Zwritemtc           ZWRITEMTC_
#define Zreadvectors        ZREADVECTORS_
#define Zwritevectors       ZWRITEVECTORS_



#define csymilupack         CSYMILUPACK_
#define csymilupackfac      CSYMILUPACKFAC_
#define csymilupacksol      CSYMILUPACKSOL_
#define csymilupackdel      CSYMILUPACKDEL_
#define cherilupack         CHERILUPACK_
#define cherilupackfac      CHERILUPACKFAC_
#define cherilupacksol      CHERILUPACKSOL_
#define cherilupackdel      CHERILUPACKDEL_

#define CGNLlupq            CGNLLUPQ_
#define CGNLlupqsol         CGNLLUPQSOL_
#define CGNLlupqtsol        CGNLLUPQTSOL_
#define CGNLlupqlsol        CGNLLUPQLSOL_
#define CGNLlupqtlsol       CGNLLUPQTLSOL_
#define CGNLlupqusol        CGNLLUPQUSOL_
#define CGNLlupqtusol       CGNLLUPQTUSOL_
#define CGNLlupqdlsol       CGNLLUPQDLSOL_
#define CGNLlupqtdlsol      CGNLLUPQTDLSOL_
#define CGNLlupqdusol       CGNLLUPQDUSOL_
#define CGNLlupqtdusol      CGNLLUPQTDUSOL_
#define CHPDldlp            CHPDLDLP_
#define CHPDldlpsol         CHPDLDLPSOL_

#define CGNLilutp           CGNLILUTP_
#define CGNLilut            CGNLILUT_
#define CGNLlusol           CGNLLUSOL_
#define CGNLlutsol          CGNLLUTSOL_
#define CGNLlulsol          CGNLLULSOL_
#define CGNLlutlsol         CGNLLUTLSOL_
#define CGNLluusol          CGNLLUUSOL_
#define CGNLlutusol         CGNLLUTUSOL_
#define CGNLludlsol         CGNLLUDLSOL_
#define CGNLlutdlsol        CGNLLUTDLSOL_
#define CGNLludusol         CGNLLUDUSOL_
#define CGNLlutdusol        CGNLLUTDUSOL_

#define CGNLiluc            CGNLILUC_
#define CGNLilucsol         CGNLILUCSOL_
#define CGNLiluctsol        CGNLILUCTSOL_
#define CGNLilucdlsol       CGNLILUCDLSOL_
#define CGNLiluctdlsol      CGNLILUCTDLSOL_
#define CGNLilucdusol       CGNLILUCDUSOL_
#define CGNLiluctdusol      CGNLILUCTDUSOL_
#define CGNLiluclsol        CGNLILUCLSOL_
#define CGNLiluctlsol       CGNLILUCTLSOL_
#define CGNLilucusol        CGNLILUCUSOL_
#define CGNLiluctusol       CGNLILUCTUSOL_

#define CGNLpilucdlsol      CGNLPILUCDLSOL_
#define CGNLpiluctdlsol     CGNLPILUCTDLSOL_
#define CGNLpilucdusol      CGNLPILUCDUSOL_
#define CGNLpiluctdusol     CGNLPILUCTDUSOL_
#define CGNLpiluclsol       CGNLPILUCLSOL_
#define CGNLpiluctlsol      CGNLPILUCTLSOL_
#define CGNLpilucusol       CGNLPILUCUSOL_
#define CGNLpiluctusol      CGNLPILUCTUSOL_

#define CHERildlc           CHERILDLC_
#define CHERildlcsol        CHERILDLCSOL_
#define CSHRildlc           CSHRILDLC_
#define CSHRildlcsol        CSHRILDLCSOL_
#define CSYMildlc           CSYMILDLC_
#define CSYMildlcsol        CSYMILDLCSOL_
#define CSSMildlc           CSSMILDLC_
#define CSSMildlcsol        CSSMILDLCSOL_
#define CGNLpiluc           CGNLPILUC_
#define CGNLspiluc          CGNLSPILUC_
#define CGNLmpiluc          CGNLMPILUC_
#define CHPDpiluc           CHPDPILUC_
#define CHPDmpiluc          CHPDMPILUC_
#define CSYMpiluc           CSYMPILUC_
#define CSYMiluc            CSYMILUC_
#define CSYMmpiluc          CSYMMPILUC_
#define CSYMpilucsol        CSYMPILUCSOL_
#define CHERpiluc           CHERPILUC_
#define CHERiluc            CHERILUC_
#define CHERmpiluc          CHERMPILUC_
#define CHERpilucsol        CHERPILUCSOL_
#define CSYMpiluclsol       CSYMPILUCLSOL_
#define CSYMpilucusol       CSYMPILUCUSOL_
#define CHERpiluclsol       CHERPILUCLSOL_
#define CHERpilucusol       CHERPILUCUSOL_


#define Cpcg                CPCG_
#define Cbcg                CBCG_
#define CSYMbcg             CSYMBCG_
#define CHERbcg             CHERBCG_
#define CSYMqmr             CSYMQMR_
#define CHERqmr             CHERQMR_
#define Cgmres              CGMRES_
#define Cfgmres             CFGMRES_
#define Cdistdotc           CDISTDOTC_
#define Cdistdotu           CDISTDOTU_


#define Croscal             CROSCAL_
#define Ccoscal             CCOSCAL_
#define Crowscale           CROWSCALE_
#define Ccolscale           CCOLSCALE_
#define CHPDscale           CHPDSCALE_
#define CSYMscale           CSYMSCALE_
#define CHERscale           CHERSCALE_
#define Ccsrcsc             CCSRCSC_
#define Cqsort              CQSORT_
#define Cqqsort             CQQSORT_
#define Cqqsort2            CQQSORT2_
#define Cqqsorts            CQQSORTS_
#define Cqqsorts2           CQQSORTS2_

#define Creadmtc            CREADMTC_
#define Cwritemtc           CWRITEMTC_
#define Creadvectors        CREADVECTORS_
#define Cwritevectors       CWRITEVECTORS_





/* CAPS and 2 underscores are defined */
#elif defined __CAPS__ && defined __2UNDERSCORES__
#define mysdot                    MYSDOT__
#define myddot                    MYDDOT__
#define mycdotc                   MYCDOTC__
#define myzdotc                   MYZDOTC__
#define sdsymamgsavediag          SDSYMAMGSAVEDIAG__	    
#define sdsymamgsavediaggep       SDSYMAMGSAVEDIAGGEP__
#define sdsymamgsol		  SDSYMAMGSOL__		    
#define sdsymamginit		  SDSYMAMGINIT__		    
#define sdsymamgrestorediag	  SDSYMAMGRESTOREDIAG__	    
#define sdamgundoscaling	  SDAMGUNDOSCALING__	    
#define sdsymamgfactor		  SDSYMAMGFACTOR__
#define sdsymamgfactorgep	  SDSYMAMGFACTORGEP__

#define czheramgsavediag	  CZHERAMGSAVEDIAG__	    
#define czheramgsavediaggep	  CZHERAMGSAVEDIAGGEP__
#define czheramgsol		  CZHERAMGSOL__		    
#define czheramginit		  CZHERAMGINIT__		    
#define czheramgrestorediag	  CZHERAMGRESTOREDIAG__	    
#define czamgundoscaling	  CZAMGUNDOSCALING__	    
#define czheramgfactor            CZHERAMGFACTOR__
#define czheramgfactorgep         CZHERAMGFACTORGEP__


#define iprandom            IPRANDOM__
#define ipsrandom           IPSRANDOM__
#define evaluatetime        EVALUATETIME__
#define dprivatesptrs       DPRIVATESPTRS__
#define sprivatesptrs       SPRIVATESPTRS__
#define cprivatehptrs       CPRIVATEHPTRS__
#define zprivatehptrs       ZPRIVATEHPTRS__

#define sdglprecsetup        SDGLPRECSETUP__
#define sdglprecsol          SDGLPRECSOL__
#define sdglprecdelete       SDGLPRECDELETE__
#define ddglprecsetup        DDGLPRECSETUP__
#define ddglprecsol          DDGLPRECSOL__
#define ddglprecdelete       DDGLPRECDELETE__

#define cdglprecsetup       CDGLPRECSETUP__
#define cdglprecsol         CDGLPRECSOL__
#define cdglprecdelete      CDGLPRECDELETE__
#define zdglprecsetup       ZDGLPRECSETUP__
#define zdglprecsol         ZDGLPRECSOL__
#define zdglprecdelete      ZDGLPRECDELETE__

#define dsymamgsavediag     DSYMAMGSAVEDIAG__	  
#define dsymamgsavediaggep  DSYMAMGSAVEDIAGGEP__
#define dsymamgrestorediag  DSYMAMGRESTOREDIAG__
#define ssymamgsavediag     SSYMAMGSAVEDIAG__	  
#define ssymamgsavediaggep  SSYMAMGSAVEDIAGGEP__
#define ssymamgrestorediag  SSYMAMGRESTOREDIAG__
#define cheramgsavediag     CHERAMGSAVEDIAG__	  
#define cheramgsavediaggep  CHERAMGSAVEDIAGGEP__
#define cheramgrestorediag  CHERAMGRESTOREDIAG__
#define zheramgsavediag     ZHERAMGSAVEDIAG__	  
#define zheramgsavediaggep  ZHERAMGSAVEDIAGGEP__
#define zheramgrestorediag  ZHERAMGRESTOREDIAG__

#define samgundoscaling     SAMGUNDOSCALING__
#define damgundoscaling     DAMGUNDOSCALING__
#define camgundoscaling     CAMGUNDOSCALING__
#define zamgundoscaling     ZAMGUNDOSCALING__

#define sgnlamginit         SGNLAMGINIT__           
#define sgnlamgfactor       SGNLAMGFACTOR__  
#define sgnlamgsolver       SGNLAMGSOLVER__  
#define sgnlamgsol          SGNLAMGSOL__   
#define sgnlamgdelete       SGNLAMGDELETE__        
#define sgnlamginfo         SGNLAMGINFO__
#define sgnlamgnnz          SGNLAMGNNZ__
                                              
#define ssymspdamgconvert   SSYMSPDAMGCONVERT__

#define sspdamginit         SSPDAMGINIT__          
#define sspdamgfactor       SSPDAMGFACTOR__        
#define sspdamgsolver       SSPDAMGSOLVER__        
#define sspdamgsol          SSPDAMGSOL__   
#define sspdamgdelete       SSPDAMGDELETE__        
#define sspdamginfo         SSPDAMGINFO__
#define sspdamgnnz          SSPDAMGNNZ__
                                              
#define ssymamginit         SSYMAMGINIT__          
#define ssymamgfactor       SSYMAMGFACTOR__        
#define ssymamgfactorgep    SSYMAMGFACTORGEP__
#define ssymamgsolver       SSYMAMGSOLVER__        
#define ssymamgsol          SSYMAMGSOL__   
#define ssymamgdelete       SSYMAMGDELETE__        
#define ssymamginfo         SSYMAMGINFO__
#define ssymamgnnz          SSYMAMGNNZ__
                                              
                                              
#define dgnlamginit         DGNLAMGINIT__          
#define dgnlamgfactor       DGNLAMGFACTOR__        
#define dgnlamgsolver       DGNLAMGSOLVER__        
#define dgnlamgsol          DGNLAMGSOL__   
#define dgnlamgdelete       DGNLAMGDELETE__        
#define dgnlamginfo         DGNLAMGINFO__
#define dgnlamgnnz          DGNLAMGNNZ__
                                              
#define dsymspdamgconvert   DSYMSPDAMGCONVERT__

#define dspdamginit         DSPDAMGINIT__          
#define dspdamgfactor       DSPDAMGFACTOR__        
#define dspdamgsolver       DSPDAMGSOLVER__       
#define dspdamgsol          DSPDAMGSOL__   
#define dspdamgdelete       DSPDAMGDELETE__        
#define dspdamginfo         DSPDAMGINFO__
#define dspdamgnnz          DSPDAMGNNZ__
                                              
#define dsymamginit         DSYMAMGINIT__          
#define dsymamgfactor       DSYMAMGFACTOR__        
#define dsymamgfactorgep    DSYMAMGFACTORGEP__
#define dsymamgsolver       DSYMAMGSOLVER__        
#define dsymamgsol          DSYMAMGSOL__   
#define dsymamgdelete       DSYMAMGDELETE__        
#define dsymamginfo         DSYMAMGINFO__
#define dsymamgnnz          DSYMAMGNNZ__
                                              
                                              
#define cgnlamginit         CGNLAMGINIT__          
#define cgnlamgfactor       CGNLAMGFACTOR__        
#define cgnlamgsolver       CGNLAMGSOLVER__        
#define cgnlamgsol          CGNLAMGSOL__   
#define cgnlamgdelete       CGNLAMGDELETE__        
#define cgnlamginfo         CGNLAMGINFO__
#define cgnlamgnnz          CGNLAMGNNZ__
                                              
#define cherhpdamgconvert   CHERHPDAMGCONVERT__

#define chpdamginit         CHPDAMGINIT__          
#define chpdamgfactor       CHPDAMGFACTOR__        
#define chpdamgsolver       CHPDAMGSOLVER__        
#define chpdamgsol          CHPDAMGSOL__   
#define chpdamgdelete       CHPDAMGDELETE__        
#define chpdamginfo         CHPDAMGINFO__
#define chpdamgnnz          CHPDAMGNNZ__
                                              
#define cheramginit         CHERAMGINIT__          
#define cheramgfactor       CHERAMGFACTOR__        
#define cheramgfactorgep    CHERAMGFACTORGEP__
#define cheramgsolver       CHERAMGSOLVER__        
#define cheramgsol          CHERAMGSOL__   
#define cheramgdelete       CHERAMGDELETE__        
#define cheramginfo         CHERAMGINFO__
#define cheramgnnz          CHERAMGNNZ__
                                              
#define csymamginit         CSYMAMGINIT__          
#define csymamgfactor       CSYMAMGFACTOR__        
#define csymamgsolver       CSYMAMGSOLVER__        
#define csymamgsol          CSYMAMGSOL__   
#define csymamgdelete       CSYMAMGDELETE__        
#define csymamginfo         CSYMAMGINFO__
#define csymamgnnz          CSYMAMGNNZ__
                                              
                                              
#define zgnlamginit         ZGNLAMGINIT__          
#define zgnlamgfactor       ZGNLAMGFACTOR__        
#define zgnlamgsolver       ZGNLAMGSOLVER__        
#define zgnlamgsol          ZGNLAMGSOL__   
#define zgnlamgdelete       ZGNLAMGDELETE__        
#define zgnlamginfo         ZGNLAMGINFO__
#define zgnlamgnnz          ZGNLAMGNNZ__
                                              
#define zherhpdamgconvert   ZHERHPDAMGCONVERT__

#define zhpdamginit         ZHPDAMGINIT__          
#define zhpdamgfactor       ZHPDAMGFACTOR__        
#define zhpdamgsolver       ZHPDAMGSOLVER__        
#define zhpdamgsol          ZHPDAMGSOL__   
#define zhpdamgdelete       ZHPDAMGDELETE__        
#define zhpdamginfo         ZHPDAMGINFO__
#define zhpdamgnnz          ZHPDAMGNNZ__
                                              
#define zheramginit         ZHERAMGINIT__          
#define zheramgfactor       ZHERAMGFACTOR__        
#define zheramgfactorgep    ZHERAMGFACTORGEP__
#define zheramgsolver       ZHERAMGSOLVER__        
#define zheramgsol          ZHERAMGSOL__   
#define zheramgdelete       ZHERAMGDELETE__        
#define zheramginfo         ZHERAMGINFO__
#define zheramgnnz          ZHERAMGNNZ__
                                              
#define zsymamginit         ZSYMAMGINIT__          
#define zsymamgfactor       ZSYMAMGFACTOR__        
#define zsymamgsolver       ZSYMAMGSOLVER__        
#define zsymamgsol          ZSYMAMGSOL__   
#define zsymamgdelete       ZSYMAMGDELETE__     
#define zsymamginfo         ZSYMAMGINFO__
#define zsymamgnnz          ZSYMAMGNNZ__



#define qqsorti             QQSORTI__

#define dsymilupack         DSYMILUPACK__
#define dsymilupackfac      DSYMILUPACKFAC__
#define dsymilupacksol      DSYMILUPACKSOL__
#define dsymilupackdel      DSYMILUPACKDEL__

#define DGNLlupq            DGNLLUPQ__
#define DGNLlupqsol         DGNLLUPQSOL__
#define DGNLlupqtsol        DGNLLUPQTSOL__
#define DGNLlupqlsol        DGNLLUPQLSOL__
#define DGNLlupqtlsol       DGNLLUPQTLSOL__
#define DGNLlupqusol        DGNLLUPQUSOL__
#define DGNLlupqtusol       DGNLLUPQTUSOL__
#define DGNLlupqdlsol       DGNLLUPQDLSOL__
#define DGNLlupqtdlsol      DGNLLUPQTDLSOL__
#define DGNLlupqdusol       DGNLLUPQDUSOL__
#define DGNLlupqtdusol      DGNLLUPQTDUSOL__
#define DSPDldlp            DSPDLDLP__
#define DSPDldlpsol         DSPDLDLPSOL__

#define DGNLilutp           DGNLILUTP__
#define DGNLilut            DGNLILUT__
#define DGNLlusol           DGNLLUSOL__
#define DGNLlutsol          DGNLLUTSOL__
#define DGNLlulsol          DGNLLULSOL__
#define DGNLlutlsol         DGNLLUTLSOL__
#define DGNLluusol          DGNLLUUSOL__
#define DGNLlutusol         DGNLLUTUSOL__
#define DGNLludlsol         DGNLLUDLSOL__
#define DGNLlutdlsol        DGNLLUTDLSOL__
#define DGNLludusol         DGNLLUDUSOL__
#define DGNLlutdusol        DGNLLUTDUSOL__

#define DGNLiluc            DGNLILUC__
#define DGNLilucsol         DGNLILUCSOL__
#define DGNLiluctsol        DGNLILUCTSOL__
#define DGNLilucdlsol       DGNLILUCDLSOL__
#define DGNLiluctdlsol      DGNLILUCTDLSOL__
#define DGNLilucdusol       DGNLILUCDUSOL__
#define DGNLiluctdusol      DGNLILUCTDUSOL__
#define DGNLiluclsol        DGNLILUCLSOL__
#define DGNLiluctlsol       DGNLILUCTLSOL__
#define DGNLilucusol        DGNLILUCUSOL__
#define DGNLiluctusol       DGNLILUCTUSOL__

#define DGNLpilucdlsol      DGNLPILUCDLSOL__
#define DGNLpiluctdlsol     DGNLPILUCTDLSOL__
#define DGNLpilucdusol      DGNLPILUCDUSOL__
#define DGNLpiluctdusol     DGNLPILUCTDUSOL__
#define DGNLpiluclsol       DGNLPILUCLSOL__
#define DGNLpiluctlsol      DGNLPILUCTLSOL__
#define DGNLpilucusol       DGNLPILUCUSOL__
#define DGNLpiluctusol      DGNLPILUCTUSOL__

#define DSYMildlc           DSYMILDLC__
#define DSYMildlcsol        DSYMILDLCSOL__
#define DSSMildlc           DSSMILDLC__
#define DSSMildlcsol        DSSMILDLCSOL__
#define DGNLpiluc           DGNLPILUC__
#define DGNLspiluc          DGNLSPILUC__
#define DGNLmpiluc          DGNLMPILUC__
#define DSPDpiluc           DSPDPILUC__
#define DSPDmpiluc          DSPDMPILUC__
#define DSYMpiluc           DSYMPILUC__
#define DSYMiluc            DSYMILUC__
#define DSYMmpiluc          DSYMMPILUC__
#define DSYMpilucsol        DSYMPILUCSOL__
#define DSYMpiluclsol       DSYMPILUCLSOL__
#define DSYMpilucusol       DSYMPILUCUSOL__


#define Dpcg                DPCG__
#define Dbcg                DBCG__
#define DSYMbcg             DSYMBCG__
#define DSYMqmr             DSYMQMR__
#define Dgmres              DGMRES__
#define Dfgmres             DFGMRES__
#define Ddistdot            DDISTDOT__


#define Droscal             DROSCAL__
#define Dcoscal             DCOSCAL__
#define Drowscale           DROWSCALE__
#define Dcolscale           DCOLSCALE__
#define DSPDscale           DSPDSCALE__
#define DSYMscale           DSYMSCALE__
#define Dcsrcsc             DCSRCSC__
#define Dqsort              DQSORT__
#define Dqqsort             DQQSORT__
#define Dqqsort2            DQQSORT2__
#define Dqqsorts            DQQSORTS__
#define Dqqsorts2           DQQSORTS2__

#define Dreadmtc            DREADMTC__
#define Dwritemtc           DWRITEMTC__
#define Dreadvectors        DREADVECTORS__
#define Dwritevectors       DWRITEVECTORS__



#define ssymilupack         SSYMILUPACK__
#define ssymilupackfac      SSYMILUPACKFAC__
#define ssymilupacksol      SSYMILUPACKSOL__
#define ssymilupackdel      SSYMILUPACKDEL__

#define SGNLlupq            SGNLLUPQ__
#define SGNLlupqsol         SGNLLUPQSOL__
#define SGNLlupqtsol        SGNLLUPQTSOL__
#define SGNLlupqlsol        SGNLLUPQLSOL__
#define SGNLlupqtlsol       SGNLLUPQTLSOL__
#define SGNLlupqusol        SGNLLUPQUSOL__
#define SGNLlupqtusol       SGNLLUPQTUSOL__
#define SGNLlupqdlsol       SGNLLUPQDLSOL__
#define SGNLlupqtdlsol      SGNLLUPQTDLSOL__
#define SGNLlupqdusol       SGNLLUPQDUSOL__
#define SGNLlupqtdusol      SGNLLUPQTDUSOL__
#define SSPDldlp            SSPDLDLP__
#define SSPDldlpsol         SSPDLDLPSOL__

#define SGNLilutp           SGNLILUTP__
#define SGNLilut            SGNLILUT__
#define SGNLlusol           SGNLLUSOL__
#define SGNLlutsol          SGNLLUTSOL__
#define SGNLlulsol          SGNLLULSOL__
#define SGNLlutlsol         SGNLLUTLSOL__
#define SGNLluusol          SGNLLUUSOL__
#define SGNLlutusol         SGNLLUTUSOL__
#define SGNLludlsol         SGNLLUDLSOL__
#define SGNLlutdlsol        SGNLLUTDLSOL__
#define SGNLludusol         SGNLLUDUSOL__
#define SGNLlutdusol        SGNLLUTDUSOL__

#define SGNLiluc            SGNLILUC__
#define SGNLilucsol         SGNLILUCSOL__
#define SGNLiluctsol        SGNLILUCTSOL__
#define SGNLilucdlsol       SGNLILUCDLSOL__
#define SGNLiluctdlsol      SGNLILUCTDLSOL__
#define SGNLilucdusol       SGNLILUCDUSOL__
#define SGNLiluctdusol      SGNLILUCTDUSOL__
#define SGNLiluclsol        SGNLILUCLSOL__
#define SGNLiluctlsol       SGNLILUCTLSOL__
#define SGNLilucusol        SGNLILUCUSOL__
#define SGNLiluctusol       SGNLILUCTUSOL__

#define DGNLpilucdlsol      DGNLPILUCDLSOL__
#define DGNLpiluctdlsol     DGNLPILUCTDLSOL__
#define DGNLpilucdusol      DGNLPILUCDUSOL__
#define DGNLpiluctdusol     DGNLPILUCTDUSOL__
#define DGNLpiluclsol       DGNLPILUCLSOL__
#define DGNLpiluctlsol      DGNLPILUCTLSOL__
#define DGNLpilucusol       DGNLPILUCUSOL__
#define DGNLpiluctusol      DGNLPILUCTUSOL__

#define SSYMildlc           SSYMILDLC__
#define SSYMildlcsol        SSYMILDLCSOL__
#define SSSMildlc           SSSMILDLC__
#define SSSMildlcsol        SSSMILDLCSOL__
#define SGNLpiluc           SGNLPILUC__
#define SGNLspiluc          SGNLSPILUC__
#define SGNLmpiluc          SGNLMPILUC__
#define SSPDpiluc           SSPDPILUC__
#define SSPDmpiluc          SSPDMPILUC__
#define SSYMpiluc           SSYMPILUC__
#define SSYMiluc            SSYMILUC__
#define SSYMmpiluc          SSYMMPILUC__
#define SSYMpilucsol        SSYMPILUCSOL__
#define SSYMpiluclsol       SSYMPILUCLSOL__
#define SSYMpilucusol       SSYMPILUCUSOL__


#define Spcg                SPCG__
#define Sbcg                SBCG__
#define SSYMbcg             SSYMBCG__
#define SSYMqmr             SSYMQMR__
#define Sgmres              SGMRES__
#define Sfgmres             SFGMRES__
#define Sdistdot            SDISTDOT__


#define Sroscal             SROSCAL__
#define Scoscal             SCOSCAL__
#define Srowscale           SROWSCALE__
#define Scolscale           SCOLSCALE__
#define SSPDscale           SSPDSCALE__
#define SSYMscale           SSYMSCALE__
#define Scsrcsc             SCSRCSC__
#define Sqsort              SQSORT__
#define Sqqsort             SQQSORT__
#define Sqqsort2            SQQSORT2__
#define Sqqsorts            SQQSORTS__
#define Sqqsorts2           SQQSORTS2__

#define Sreadmtc            SREADMTC__
#define Swritemtc           SWRITEMTC__
#define Sreadvectors        SREADVECTORS__
#define Swritevectors       SWRITEVECTORS__



#define zsymilupack         ZSYMILUPACK__
#define zsymilupackfac      ZSYMILUPACKFAC__
#define zsymilupacksol      ZSYMILUPACKSOL__
#define zsymilupackdel      ZSYMILUPACKDEL__
#define zherilupack         ZHERILUPACK__
#define zherilupackfac      ZHERILUPACKFAC__
#define zherilupacksol      ZHERILUPACKSOL__
#define zherilupackdel      ZHERILUPACKDEL__

#define ZGNLlupq            ZGNLLUPQ__
#define ZGNLlupqsol         ZGNLLUPQSOL__
#define ZGNLlupqtsol        ZGNLLUPQTSOL__
#define ZGNLlupqlsol        ZGNLLUPQLSOL__
#define ZGNLlupqtlsol       ZGNLLUPQTLSOL__
#define ZGNLlupqusol        ZGNLLUPQUSOL__
#define ZGNLlupqtusol       ZGNLLUPQTUSOL__
#define ZGNLlupqdlsol       ZGNLLUPQDLSOL__
#define ZGNLlupqtdlsol      ZGNLLUPQTDLSOL__
#define ZGNLlupqdusol       ZGNLLUPQDUSOL__
#define ZGNLlupqtdusol      ZGNLLUPQTDUSOL__
#define ZHPDldlp            ZHPDLDLP__
#define ZHPDldlpsol         ZHPDLDLPSOL__

#define ZGNLilutp           ZGNLILUTP__
#define ZGNLilut            ZGNLILUT__
#define ZGNLlusol           ZGNLLUSOL__
#define ZGNLlutsol          ZGNLLUTSOL__
#define ZGNLlulsol          ZGNLLULSOL__
#define ZGNLlutlsol         ZGNLLUTLSOL__
#define ZGNLluusol          ZGNLLUUSOL__
#define ZGNLlutusol         ZGNLLUTUSOL__
#define ZGNLludlsol         ZGNLLUDLSOL__
#define ZGNLlutdlsol        ZGNLLUTDLSOL__
#define ZGNLludusol         ZGNLLUDUSOL__
#define ZGNLlutdusol        ZGNLLUTDUSOL__

#define ZGNLiluc            ZGNLILUC__
#define ZGNLilucsol         ZGNLILUCSOL__
#define ZGNLiluctsol        ZGNLILUCTSOL__
#define ZGNLilucdlsol       ZGNLILUCDLSOL__
#define ZGNLiluctdlsol      ZGNLILUCTDLSOL__
#define ZGNLilucdusol       ZGNLILUCDUSOL__
#define ZGNLiluctdusol      ZGNLILUCTDUSOL__
#define ZGNLiluclsol        ZGNLILUCLSOL__
#define ZGNLiluctlsol       ZGNLILUCTLSOL__
#define ZGNLilucusol        ZGNLILUCUSOL__
#define ZGNLiluctusol       ZGNLILUCTUSOL__

#define ZGNLpilucdlsol      ZGNLPILUCDLSOL__
#define ZGNLpiluctdlsol     ZGNLPILUCTDLSOL__
#define ZGNLpilucdusol      ZGNLPILUCDUSOL__
#define ZGNLpiluctdusol     ZGNLPILUCTDUSOL__
#define ZGNLpiluclsol       ZGNLPILUCLSOL__
#define ZGNLpiluctlsol      ZGNLPILUCTLSOL__
#define ZGNLpilucusol       ZGNLPILUCUSOL__
#define ZGNLpiluctusol      ZGNLPILUCTUSOL__

#define ZHERildlc           ZHERILDLC__
#define ZHERildlcsol        ZHERILDLCSOL__
#define ZSHRildlc           ZSHRILDLC__
#define ZSHRildlcsol        ZSHRILDLCSOL__
#define ZSYMildlc           ZSYMILDLC__
#define ZSYMildlcsol        ZSYMILDLCSOL__
#define ZSSMildlc           ZSSMILDLC__
#define ZSSMildlcsol        ZSSMILDLCSOL__
#define ZGNLpiluc           ZGNLPILUC__
#define ZGNLspiluc          ZGNLSPILUC__
#define ZGNLmpiluc          ZGNLMPILUC__
#define ZHPDpiluc           ZHPDPILUC__
#define ZHPDmpiluc          ZHPDMPILUC__
#define ZSYMiluc            ZSYMILUC__
#define ZSYMpiluc           ZSYMPILUC__
#define ZSYMmpiluc          ZSYMMPILUC__
#define ZSYMpilucsol        ZSYMPILUCSOL__
#define ZHERiluc            ZHERILUC__
#define ZHERpiluc           ZHERPILUC__
#define ZHERmpiluc          ZHERMPILUC__
#define ZHERpilucsol        ZHERPILUCSOL__
#define ZSYMpiluclsol       ZSYMPILUCLSOL__
#define ZSYMpilucusol       ZSYMPILUCUSOL__
#define ZHERpiluclsol       ZHERPILUCLSOL__
#define ZHERpilucusol       ZHERPILUCUSOL__


#define Zpcg                ZPCG__
#define Zbcg                ZBCG__
#define ZSYMbcg             ZSYMBCG__
#define ZHERbcg             ZHERBCG__
#define ZSYMqmr             ZSYMQMR__
#define ZHERqmr             ZHERQMR__
#define Zgmres              ZGMRES__
#define Zfgmres             ZFGMRES__
#define Zdistdotc           ZDISTDOTC__
#define Zdistdotu           ZDISTDOTU__


#define Zroscal             ZROSCAL__
#define Zcoscal             ZCOSCAL__
#define Zrowscale           ZROWSCALE__
#define Zcolscale           ZCOLSCALE__
#define ZHPDscale           ZHPDSCALE__
#define ZSYMscale           ZSYMSCALE__
#define ZHERscale           ZHERSCALE__
#define Zcsrcsc             ZCSRCSC__
#define Zqsort              ZQSORT__
#define Zqqsort             ZQQSORT__
#define Zqqsort2            ZQQSORT2__
#define Zqqsorts            ZQQSORTS__
#define Zqqsorts2           ZQQSORTS2__

#define Zreadmtc            ZREADMTC__
#define Zwritemtc           ZWRITEMTC__
#define Zreadvectors        ZREADVECTORS__
#define Zwritevectors       ZWRITEVECTORS__



#define csymilupack         CSYMILUPACK__
#define csymilupackfac      CSYMILUPACKFAC__
#define csymilupacksol      CSYMILUPACKSOL__
#define csymilupackdel      CSYMILUPACKDEL__
#define cherilupack         CHERILUPACK__
#define cherilupackfac      CHERILUPACKFAC__
#define cherilupacksol      CHERILUPACKSOL__
#define cherilupackdel      CHERILUPACKDEL__

#define CGNLlupq            CGNLLUPQ__
#define CGNLlupqsol         CGNLLUPQSOL__
#define CGNLlupqtsol        CGNLLUPQTSOL__
#define CGNLlupqlsol        CGNLLUPQLSOL__
#define CGNLlupqtlsol       CGNLLUPQTLSOL__
#define CGNLlupqusol        CGNLLUPQUSOL__
#define CGNLlupqtusol       CGNLLUPQTUSOL__
#define CGNLlupqdlsol       CGNLLUPQDLSOL__
#define CGNLlupqtdlsol      CGNLLUPQTDLSOL__
#define CGNLlupqdusol       CGNLLUPQDUSOL__
#define CGNLlupqtdusol      CGNLLUPQTDUSOL__
#define CHPDldlp            CHPDLDLP__
#define CHPDldlpsol         CHPDLDLPSOL__

#define CGNLilutp           CGNLILUTP__
#define CGNLilut            CGNLILUT__
#define CGNLlusol           CGNLLUSOL__
#define CGNLlutsol          CGNLLUTSOL__
#define CGNLlulsol          CGNLLULSOL__
#define CGNLlutlsol         CGNLLUTLSOL__
#define CGNLluusol          CGNLLUUSOL__
#define CGNLlutusol         CGNLLUTUSOL__
#define CGNLludlsol         CGNLLUDLSOL__
#define CGNLlutdlsol        CGNLLUTDLSOL__
#define CGNLludusol         CGNLLUDUSOL__
#define CGNLlutdusol        CGNLLUTDUSOL__

#define CGNLiluc            CGNLILUC__
#define CGNLilucsol         CGNLILUCSOL__
#define CGNLiluctsol        CGNLILUCTSOL__
#define CGNLilucdlsol       CGNLLILUCDLSOL__
#define CGNLiluctdlsol      CGNLILUCTDLSOL__
#define CGNLilucdusol       CGNLILUCDUSOL__
#define CGNLiluctdusol      CGNLILUCTDUSOL__
#define CGNLiluclsol        CGNLILUCLSOL__
#define CGNLiluctlsol       CGNLILUCTLSOL__
#define CGNLilucusol        CGNLILUCUSOL__
#define CGNLiluctusol       CGNLILUCTUSOL__

#define CGNLpilucdlsol      CGNLPILUCDLSOL__
#define CGNLpiluctdlsol     CGNLPILUCTDLSOL__
#define CGNLpilucdusol      CGNLPILUCDUSOL__
#define CGNLpiluctdusol     CGNLPILUCTDUSOL__
#define CGNLpiluclsol       CGNLPILUCLSOL__
#define CGNLpiluctlsol      CGNLPILUCTLSOL__
#define CGNLpilucusol       CGNLPILUCUSOL__
#define CGNLpiluctusol      CGNLPILUCTUSOL__

#define CHERildlc           CHERILDLC__
#define CHERildlcsol        CHERILDLCSOL__
#define CSHRildlc           CSHRILDLC__
#define CSHRildlcsol        CSHRILDLCSOL__
#define CSYMildlc           CSYMILDLC__
#define CSYMildlcsol        CSYMILDLCSOL__
#define CSSMildlc           CSSMILDLC__
#define CSSMildlcsol        CSSMILDLCSOL__
#define CGNLpiluc           CGNLPILUC__
#define CGNLspiluc          CGNLSPILUC__
#define CGNLmpiluc          CGNLMPILUC__
#define CHPDpiluc           CHPDPILUC__
#define CHPDmpiluc          CHPDMPILUC__
#define CSYMiluc            CSYMILUC__
#define CSYMpiluc           CSYMPILUC__
#define CSYMmpiluc          CSYMMPILUC__
#define CSYMpilucsol        CSYMPILUCSOL__
#define CHERiluc            CHERILUC__
#define CHERpiluc           CHERPILUC__
#define CHERmpiluc          CHERMPILUC__
#define CHERpilucsol        CHERPILUCSOL__
#define CSYMpiluclsol       CSYMPILUCLSOL__
#define CSYMpilucusol       CSYMPILUCUSOL__
#define CHERpiluclsol       CHERPILUCLSOL__
#define CHERpilucusol       CHERPILUCUSOL__


#define Cpcg                CPCG__
#define Cbcg                CBCG__
#define CSYMbcg             CSYMBCG__
#define CHERbcg             CHERBCG__
#define CSYMqmr             CSYMQMR__
#define CHERqmr             CHERQMR__
#define Cgmres              CGMRES__
#define Cfgmres             CFGMRES__
#define Cdistdotc           CDISTDOTC__
#define Cdistdotu           CDISTDOTU__


#define Croscal             CROSCAL__
#define Ccoscal             CCOSCAL__
#define Crowscale           CROWSCALE__
#define Ccolscale           CCOLSCALE__
#define CHPDscale           CHPDSCALE__
#define CSYMscale           CSYMSCALE__
#define CHERscale           CHERSCALE__
#define Ccsrcsc             CCSRCSC__
#define Cqsort              CQSORT__
#define Cqqsort             CQQSORT__
#define Cqqsort2            CQQSORT2__
#define Cqqsorts            CQQSORTS__
#define Cqqsorts2           CQQSORTS2__

#define Creadmtc            CREADMTC__
#define Cwritemtc           CWRITEMTC__
#define Creadvectors        CREADVECTORS__
#define Cwritevectors       CWRITEVECTORS__




/* no capital letters but 2 underscores */
#elif defined __2UNDERSCORES__ && !defined __CAPS__
#define mysdot                    mysdot__
#define myddot                    myddot__
#define mycdotc                   mycdotc__
#define myzdotc                   myzdotc__
#define sdsymamgsavediag          sdsymamgsavediag__	   
#define sdsymamgsavediaggep       sdsymamgsavediaggep__
#define sdsymamgsol		  sdsymamgsol__		   
#define sdsymamginit		  sdsymamginit__		   
#define sdsymamgrestorediag	  sdsymamgrestorediag__	   
#define sdamgundoscaling	  sdamgundoscaling__	   
#define sdsymamgfactor		  sdsymamgfactor__		   
#define sdsymamgfactorgep	  sdsymamgfactorgep__

#define czheramgsavediag	  czheramgsavediag__	   
#define czheramgsavediaggep	  czheramgsavediaggep__
#define czheramgsol		  czheramgsol__		   
#define czheramginit		  czheramginit__		   
#define czheramgrestorediag	  czheramgrestorediag__	   
#define czamgundoscaling	  czamgundoscaling__	   
#define czheramgfactor            czheramgfactor__              
#define czheramgfactorgep         czheramgfactorgep__

#define iprandom            iprandom__
#define ipsrandom           ipsrandom__
#define evaluatetime        evaluatetime__
#define dprivatesptrs       dprivatesptrs__
#define sprivatesptrs       sprivatesptrs__
#define cprivatehptrs       cprivatehptrs__
#define zprivatehptrs       zprivatehptrs__

#define sdglprecsetup        sdglprecsetup__
#define sdglprecsol          sdglprecsol__
#define sdglprecdelete       sdglprecdelete__
#define ddglprecsetup        ddglprecsetup__
#define ddglprecsol          ddglprecsol__
#define ddglprecdelete       ddglprecdelete__

#define cdglprecsetup       cdglprecsetup__
#define cdglprecsol         cdglprecsol__
#define cdglprecdelete      cdglprecdelete__
#define zdglprecsetup       zdglprecsetup__
#define zdglprecsol         zdglprecsol__
#define zdglprecdelete      zdglprecdelete__

#define dsymamgsavediag     dsymamgsavediag__	  
#define dsymamgsavediaggep  dsymamgsavediaggep__
#define dsymamgrestorediag  dsymamgrestorediag__
#define ssymamgsavediag     ssymamgsavediag__	  
#define ssymamgsavediaggep  ssymamgsavediaggep__
#define ssymamgrestorediag  ssymamgrestorediag__
#define cheramgsavediag     cheramgsavediag__	  
#define cheramgsavediaggep  cheramgsavediaggep__
#define cheramgrestorediag  cheramgrestorediag__
#define zheramgsavediag     zheramgsavediag__	  
#define zheramgsavediaggep  zheramgsavediaggep__
#define zheramgrestorediag  zheramgrestorediag__

#define samgundoscaling     samgundoscaling__
#define damgundoscaling     damgundoscaling__
#define camgundoscaling     camgundoscaling__
#define zamgundoscaling     zamgundoscaling__

#define sgnlamginit         sgnlamginit__             
#define sgnlamgfactor       sgnlamgfactor__          
#define sgnlamgsolver       sgnlamgsolver__       
#define sgnlamgsol          sgnlamgsol__          
#define sgnlamgdelete       sgnlamgdelete__          
#define sgnlamginfo         sgnlamginfo__
#define sgnlamgnnz          sgnlamgnnz__
                                                
#define ssymspdamgconvert   ssymspdamgconvert__

#define sspdamginit         sspdamginit__            
#define sspdamgfactor       sspdamgfactor__          
#define sspdamgsolver       sspdamgsolver__          
#define sspdamgsol          sspdamgsol__          
#define sspdamgdelete       sspdamgdelete__          
#define sspdamginfo         sspdamginfo__
#define sspdamgnnz          sspdamgnnz__
                                                
#define ssymamginit         ssymamginit__            
#define ssymamgfactor       ssymamgfactor__          
#define ssymamgfactorgep    ssymamgfactorgep__
#define ssymamgsolver       ssymamgsolver__          
#define ssymamgsol          ssymamgsol__          
#define ssymamgdelete       ssymamgdelete__          
#define ssymamginfo         ssymamginfo__
#define ssymamgnnz          ssymamgnnz__
                                                
                                                
#define dgnlamginit         dgnlamginit__            
#define dgnlamgfactor       dgnlamgfactor__          
#define dgnlamgsolver       dgnlamgsolver__          
#define dgnlamgsol          dgnlamgsol__          
#define dgnlamgdelete       dgnlamgdelete__          
#define dgnlamginfo         dgnlamginfo__
#define dgnlamgnnz          dgnlamgnnz__
                                                
#define dsymspdamgconvert   dsymspdamgconvert__

#define dspdamginit         dspdamginit__            
#define dspdamgfactor       dspdamgfactor__          
#define dspdamgsolver       dspdamgsolver__          
#define dspdamgsol          dspdamgsol__          
#define dspdamgdelete       dspdamgdelete__          
#define dspdamginfo         dspdamginfo__
#define dspdamgnnz          dspdamgnnz__
                                                
#define dsymamginit         dsymamginit__            
#define dsymamgfactor       dsymamgfactor__          
#define dsymamgfactorgep    dsymamgfactorgep__
#define dsymamgsolver       dsymamgsolver__          
#define dsymamgsol          dsymamgsol__          
#define dsymamgdelete       dsymamgdelete__          
#define dsymamginfo         dsymamginfo__
#define dsymamgnnz          dsymamgnnz__
                                                
                                                
#define cgnlamginit         cgnlamginit__            
#define cgnlamgfactor       cgnlamgfactor__          
#define cgnlamgsolver       cgnlamgsolver__          
#define cgnlamgsol          cgnlamgsol__          
#define cgnlamgdelete       cgnlamgdelete__          
#define cgnlamginfo         cgnlamginfo__
#define cgnlamgnnz          cgnlamgnnz__
                                                
#define cherhpdamgconvert   cherhpdamgconvert__

#define chpdamginit         chpdamginit__            
#define chpdamgfactor       chpdamgfactor__          
#define chpdamgsolver       chpdamgsolver__          
#define chpdamgsol          chpdamgsol__          
#define chpdamgdelete       chpdamgdelete__          
#define chpdamginfo         chpdamginfo__
#define chpdamgnnz          chpdamgnnz__
                                                
#define cheramginit         cheramginit__            
#define cheramgfactor       cheramgfactor__          
#define cheramgfactorgep    cheramgfactorgep__
#define cheramgsolver       cheramgsolver__          
#define cheramgsol          cheramgsol__          
#define cheramgdelete       cheramgdelete__          
#define cheramginfo         cheramginfo__
#define cheramgnnz          cheramgnnz__
                                                
#define csymamginit         csymamginit__            
#define csymamgfactor       csymamgfactor__          
#define csymamgsolver       csymamgsolver__          
#define csymamgsol          csymamgsol__          
#define csymamgdelete       csymamgdelete__          
#define csymamginfo         csymamginfo__
#define csymamgnnz          csymamgnnz__
                                                
                                                
#define zgnlamginit         zgnlamginit__            
#define zgnlamgfactor       zgnlamgfactor__          
#define zgnlamgsolver       zgnlamgsolver__          
#define zgnlamgsol          zgnlamgsol__          
#define zgnlamgdelete       zgnlamgdelete__          
#define zgnlamginfo         zgnlamginfo__
#define zgnlamgnnz          zgnlamgnnz__
                                                
#define zherhpdamgconvert   zherhpdamgconvert__

#define zhpdamginit         zhpdamginit__            
#define zhpdamgfactor       zhpdamgfactor__          
#define zhpdamgsolver       zhpdamgsolver__          
#define zhpdamgsol          zhpdamgsol__          
#define zhpdamgdelete       zhpdamgdelete__          
#define zhpdamginfo         zhpdamginfo__
#define zhpdamgnnz          zhpdamgnnz__
                                                
#define zheramginit         zheramginit__            
#define zheramgfactor       zheramgfactor__          
#define zheramgfactorgep    zheramgfactorgep__
#define zheramgsolver       zheramgsolver__          
#define zheramgsol          zheramgsol__          
#define zheramgdelete       zheramgdelete__          
#define zheramginfo         zheramginfo__
#define zheramgnnz          zheramgnnz__
                                                
#define zsymamginit         zsymamginit__            
#define zsymamgfactor       zsymamgfactor__          
#define zsymamgsolver       zsymamgsolver__          
#define zsymamgsol          zsymamgsol__          
#define zsymamgdelete       zsymamgdelete__       
#define zsymamginfo         zsymamginfo__
#define zsymamgnnz          zsymamgnnz__



#define qqsorti             qqsorti__

#define dsymilupack         dsymilupack__
#define dsymilupackfac      dsymilupackfac__
#define dsymilupacksol      dsymilupacksol__
#define dsymilupackdel      dsymilupackdel__

#define DGNLlupq            dgnllupq__
#define DGNLlupqsol         dgnllupqsol__
#define DGNLlupqtsol        dgnllupqtsol__
#define DGNLlupqlsol        dgnllupqlsol__
#define DGNLlupqtlsol       dgnllupqtlsol__
#define DGNLlupqusol        dgnllupqusol__
#define DGNLlupqtusol       dgnllupqtusol__
#define DGNLlupqdlsol       dgnllupqdlsol__
#define DGNLlupqtdlsol      dgnllupqtdlsol__
#define DGNLlupqdusol       dgnllupqdusol__
#define DGNLlupqtdusol      dgnllupqtdusol__
#define DSPDldlp            dspdldlp__
#define DSPDldlpsol         dspdldlpsol__


#define DGNLilutp           dgnlilutp__
#define DGNLilut            dgnlilut__
#define DGNLlusol           dgnllusol__
#define DGNLlutsol          dgnllutsol__
#define DGNLlulsol          dgnllulsol__
#define DGNLlutlsol         dgnllutlsol__
#define DGNLluusol          dgnlluusol__
#define DGNLlutusol         dgnllutusol__
#define DGNLludlsol         dgnlludlsol__
#define DGNLlutdlsol        dgnllutdlsol__
#define DGNLludusol         dgnlludusol__
#define DGNLlutdusol        dgnllutdusol__

#define DGNLiluc            dgnliluc__
#define DGNLilucsol         dgnlilucsol__
#define DGNLiluctsol        dgnliluctsol__
#define DGNLilucdlsol       dgnlilucdlsol__
#define DGNLiluctdlsol      dgnliluctdlsol__
#define DGNLilucdusol       dgnlilucdusol__
#define DGNLiluctdusol      dgnliluctdusol__
#define DGNLiluclsol        dgnliluclsol__
#define DGNLiluctlsol       dgnliluctlsol__
#define DGNLilucusol        dgnlilucusol__
#define DGNLiluctusol       dgnliluctusol__

#define DGNLpilucdlsol      dgnlpilucdlsol__
#define DGNLpiluctdlsol     dgnlpiluctdlsol__
#define DGNLpilucdusol      dgnlpilucdusol__
#define DGNLpiluctdusol     dgnlpiluctdusol__
#define DGNLpiluclsol       dgnlpiluclsol__
#define DGNLpiluctlsol      dgnlpiluctlsol__
#define DGNLpilucusol       dgnlpilucusol__
#define DGNLpiluctusol      dgnlpiluctusol__

#define DSYMildlc           dsymildlc__
#define DSYMildlcsol        dsymildlcsol__
#define DSSMildlc           dssmildlc__
#define DSSMildlcsol        dssmildlcsol__
#define DGNLpiluc           dgnlpiluc__
#define DGNLspiluc          dgnlspiluc__
#define DGNLmpiluc          dgnlmpiluc__
#define DSPDpiluc           dspdpiluc__
#define DSPDmpiluc          dspdmpiluc__
#define DSYMiluc            dsymiluc__
#define DSYMpiluc           dsympiluc__
#define DSYMmpiluc          dsymmpiluc__
#define DSYMpilucsol        dsympilucsol__
#define DSYMpiluclsol       dsympiluclsol__
#define DSYMpilucusol       dsympilucusol__


#define Dpcg                dpcg__
#define Dbcg                dbcg__
#define DSYMbcg             dsymbcg__
#define DSYMqmr             dsymqmr__
#define Dgmres              dgmres__
#define Dfgmres             dfgmres__
#define Ddistdot            ddistdot__


#define Droscal             droscal__
#define Dcoscal             dcoscal__
#define Drowscale           drowscale__
#define Dcolscale           dcolscale__
#define DSPDscale           dspdscale__
#define DSYMscale           dsymscale__
#define Dcsrcsc             dcsrcsc__
#define Dqsort              dqsort__
#define Dqqsort             dqqsort__
#define Dqqsort2            dqqsort2__
#define Dqqsorts            dqqsorts__
#define Dqqsorts2           dqqsorts2__

#define Dreadmtc            dreadmtc__
#define Dwritemtc           dwritemtc__
#define Dreadvectors        dreadvectors__
#define Dwritevectors       dwritevectors__



#define ssymilupack         ssymilupack__
#define ssymilupackfac      ssymilupackfac__
#define ssymilupacksol      ssymilupacksol__
#define ssymilupackdel      ssymilupackdel__

#define SGNLlupq            sgnllupq__
#define SGNLlupqsol         sgnllupqsol__
#define SGNLlupqtsol        sgnllupqtsol__
#define SGNLlupqlsol        sgnllupqlsol__
#define SGNLlupqtlsol       sgnllupqtlsol__
#define SGNLlupqusol        sgnllupqusol__
#define SGNLlupqtusol       sgnllupqtusol__
#define SGNLlupqdlsol       sgnllupqdlsol__
#define SGNLlupqtdlsol      sgnllupqtdlsol__
#define SGNLlupqdusol       sgnllupqdusol__
#define SGNLlupqtdusol      sgnllupqtdusol__
#define SSPDldlp            sspdldlp__
#define SSPDldlpsol         sspdldlpsol__


#define SGNLilutp           sgnlilutp__
#define SGNLilut            sgnlilut__
#define SGNLlusol           sgnllusol__
#define SGNLlutsol          sgnllutsol__
#define SGNLlulsol          sgnllulsol__
#define SGNLlutlsol         sgnllutlsol__
#define SGNLluusol          sgnlluusol__
#define SGNLlutusol         sgnllutusol__
#define SGNLludlsol         sgnlludlsol__
#define SGNLlutdlsol        sgnllutdlsol__
#define SGNLludusol         sgnlludusol__
#define SGNLlutdusol        sgnllutdusol__

#define SGNLiluc            sgnliluc__
#define SGNLilucsol         sgnlilucsol__
#define SGNLiluctsol        sgnliluctsol__
#define SGNLilucdlsol       sgnlilucdlsol__
#define SGNLiluctdlsol      sgnliluctdlsol__
#define SGNLilucdusol       sgnlilucdusol__
#define SGNLiluctdusol      sgnliluctdusol__
#define SGNLiluclsol        sgnliluclsol__
#define SGNLiluctlsol       sgnliluctlsol__
#define SGNLilucusol        sgnlilucusol__
#define SGNLiluctusol       sgnliluctusol__

#define SGNLpilucdlsol      sgnlpilucdlsol__
#define SGNLpiluctdlsol     sgnlpiluctdlsol__
#define SGNLpilucdusol      sgnlpilucdusol__
#define SGNLpiluctdusol     sgnlpiluctdusol__
#define SGNLpiluclsol       sgnlpiluclsol__
#define SGNLpiluctlsol      sgnlpiluctlsol__
#define SGNLpilucusol       sgnlpilucusol__
#define SGNLpiluctusol      sgnlpiluctusol__

#define SSYMildlc           ssymildlc__
#define SSYMildlcsol        ssymildlcsol__
#define SSSMildlc           sssmildlc__
#define SSSMildlcsol        sssmildlcsol__
#define SGNLpiluc           sgnlpiluc__
#define SGNLspiluc          sgnlspiluc__
#define SGNLmpiluc          sgnlmpiluc__
#define SSPDpiluc           sspdpiluc__
#define SSPDmpiluc          sspdmpiluc__
#define SSYMpiluc           ssympiluc__
#define SSYMiluc            ssymiluc__
#define SSYMmpiluc          ssymmpiluc__
#define SSYMpilucsol        ssympilucsol__
#define SSYMpiluclsol       ssympiluclsol__
#define SSYMpilucusol       ssympilucusol__


#define Spcg                spcg__
#define Sbcg                sbcg__
#define SSYMbcg             ssymbcg__
#define SSYMqmr             ssymqmr__
#define Sgmres              sgmres__
#define Sfgmres             sfgmres__
#define Sdistdot            sdistdot__


#define Sroscal             sroscal__
#define Scoscal             scoscal__
#define Srowscale           srowscale__
#define Scolscale           scolscale__
#define SSPDscale           sspdscale__
#define SSYMscale           ssymscale__
#define Scsrcsc             scsrcsc__
#define Sqsort              sqsort__
#define Sqqsort             sqqsort__
#define Sqqsort2            sqqsort2__
#define Sqqsorts            sqqsorts__
#define Sqqsorts2           sqqsorts2__

#define Sreadmtc            sreadmtc__
#define Swritemtc           swritemtc__
#define Sreadvectors        sreadvectors__
#define Swritevectors       swritevectors__



#define zsymilupack         zsymilupack__
#define zsymilupackfac      zsymilupackfac__
#define zsymilupacksol      zsymilupacksol__
#define zsymilupackdel      zsymilupackdel__
#define zherilupack         zherilupack__
#define zherilupackfac      zherilupackfac__
#define zherilupacksol      zherilupacksol__
#define zherilupackdel      zherilupackdel__

#define ZGNLlupq            zgnllupq__
#define ZGNLlupqsol         zgnllupqsol__
#define ZGNLlupqtsol        zgnllupqtsol__
#define ZGNLlupqlsol        zgnllupqlsol__
#define ZGNLlupqtlsol       zgnllupqtlsol__
#define ZGNLlupqusol        zgnllupqusol__
#define ZGNLlupqtusol       zgnllupqtusol__
#define ZGNLlupqdlsol       zgnllupqdlsol__
#define ZGNLlupqtdlsol      zgnllupqtdlsol__
#define ZGNLlupqdusol       zgnllupqdusol__
#define ZGNLlupqtdusol      zgnllupqtdusol__
#define ZHPDldlp            zhpdldlp__
#define ZHPDldlpsol         zhpdldlpsol__

#define ZGNLilutp           zgnlilutp__
#define ZGNLilut            zgnlilut__
#define ZGNLlusol           zgnllusol__
#define ZGNLlutsol          zgnllutsol__
#define ZGNLlulsol          zgnllulsol__
#define ZGNLlutlsol         zgnllutlsol__
#define ZGNLluusol          zgnlluusol__
#define ZGNLlutusol         zgnllutusol__
#define ZGNLludlsol         zgnlludlsol__
#define ZGNLlutdlsol        zgnllutdlsol__
#define ZGNLludusol         zgnlludusol__
#define ZGNLlutdusol        zgnllutdusol__

#define ZGNLiluc            zgnliluc__
#define ZGNLilucsol         zgnlilucsol__
#define ZGNLiluctsol        zgnliluctsol__
#define ZGNLilucdlsol       zgnlilucdlsol__
#define ZGNLiluctdlsol      zgnliluctdlsol__
#define ZGNLilucdusol       zgnlilucdusol__
#define ZGNLiluctdusol      zgnliluctdusol__
#define ZGNLiluclsol        zgnliluclsol__
#define ZGNLiluctlsol       zgnliluctlsol__
#define ZGNLilucusol        zgnlilucusol__
#define ZGNLiluctusol       zgnliluctusol__

#define ZGNLpilucdlsol      zgnlpilucdlsol__
#define ZGNLpiluctdlsol     zgnlpiluctdlsol__
#define ZGNLpilucdusol      zgnlpilucdusol__
#define ZGNLpiluctdusol     zgnlpiluctdusol__
#define ZGNLpiluclsol       zgnlpiluclsol__
#define ZGNLpiluctlsol      zgnlpiluctlsol__
#define ZGNLpilucusol       zgnlpilucusol__
#define ZGNLpiluctusol      zgnlpiluctusol__

#define ZHERildlc           zherildlc__
#define ZHERildlcsol        zherildlcsol__
#define ZSHRildlc           zshrildlc__
#define ZSHRildlcsol        zshrildlcsol__
#define ZSYMildlc           zsymildlc__
#define ZSYMildlcsol        zsymildlcsol__
#define ZSSMildlc           zssmildlc__
#define ZSSMildlcsol        zssmildlcsol__
#define ZGNLpiluc           zgnlpiluc__
#define ZGNLspiluc          zgnlspiluc__
#define ZGNLmpiluc          zgnlmpiluc__
#define ZHPDpiluc           zhpdpiluc__
#define ZHPDmpiluc          zhpdmpiluc__
#define ZSYMpiluc           zsympiluc__
#define ZSYMiluc            zsymiluc__
#define ZSYMmpiluc          zsymmpiluc__
#define ZSYMpilucsol        zsympilucsol__
#define ZHERpiluc           zherpiluc__
#define ZHERiluc            zheriluc__
#define ZHERmpiluc          zhermpiluc__
#define ZHERpilucsol        zherpilucsol__
#define ZSYMpiluclsol       zsympiluclsol__
#define ZSYMpilucusol       zsympilucusol__
#define ZHERpiluclsol       zherpiluclsol__
#define ZHERpilucusol       zherpilucusol__


#define Zpcg                zpcg__
#define Zbcg                zbcg__
#define ZSYMbcg             zsymbcg__
#define ZHERbcg             zherbcg__
#define ZSYMqmr             zsymqmr__
#define ZHERqmr             zherqmr__
#define Zgmres              zgmres__
#define Zfgmres             zfgmres__
#define Zdistdotc           zdistdotc__
#define Zdistdotu           zdistdotu__


#define Zroscal             zroscal__
#define Zcoscal             zcoscal__
#define Zrowscale           zrowscale__
#define Zcolscale           zcolscale__
#define ZHPDscale           zhpdscale__
#define ZSYMscale           zsymscale__
#define ZHERscale           zherscale__
#define Zcsrcsc             zcsrcsc__
#define Zqsort              zqsort__
#define Zqqsort             zqqsort__
#define Zqqsort2            zqqsort2__
#define Zqqsorts            zqqsorts__
#define Zqqsorts2           zqqsorts2__


#define Zreadmtc            zreadmtc__
#define Zwritemtc           zwritemtc__
#define Zreadvectors        zreadvectors__
#define Zwritevectors       zwritevectors__



#define csymilupack         csymilupack__
#define csymilupackfac      csymilupackfac__
#define csymilupacksol      csymilupacksol__
#define csymilupackdel      csymilupackdel__
#define cherilupack         cherilupack__
#define cherilupackfac      cherilupackfac__
#define cherilupacksol      cherilupacksol__
#define cherilupackdel      cherilupackdel__

#define CGNLlupq            cgnllupq__
#define CGNLlupqsol         cgnllupqsol__
#define CGNLlupqtsol        cgnllupqtsol__
#define CGNLlupqlsol        cgnllupqlsol__
#define CGNLlupqtlsol       cgnllupqtlsol__
#define CGNLlupqusol        cgnllupqusol__
#define CGNLlupqtusol       cgnllupqtusol__
#define CGNLlupqdlsol       cgnllupqdlsol__
#define CGNLlupqtdlsol      cgnllupqtdlsol__
#define CGNLlupqdusol       cgnllupqdusol__
#define CGNLlupqtdusol      cgnllupqtdusol__
#define CHPDldlp            chpdldlp__
#define CHPDldlpsol         chpdldlpsol__

#define CGNLilutp           cgnlilutp__
#define CGNLilut            cgnlilut__
#define CGNLlusol           cgnllusol__
#define CGNLlutsol          cgnllutsol__
#define CGNLlulsol          cgnllulsol__
#define CGNLlutlsol         cgnllutlsol__
#define CGNLluusol          cgnlluusol__
#define CGNLlutusol         cgnllutusol__
#define CGNLludlsol         cgnlludlsol__
#define CGNLlutdlsol        cgnllutdlsol__
#define CGNLludusol         cgnlludusol__
#define CGNLlutdusol        cgnllutdusol__

#define CGNLiluc            cgnliluc__
#define CGNLilucsol         cgnlilucsol__
#define CGNLiluctsol        cgnliluctsol__
#define CGNLilucdlsol       cgnlilucdlsol__
#define CGNLiluctdlsol      cgnliluctdlsol__
#define CGNLilucdusol       cgnlilucdusol__
#define CGNLiluctdusol      cgnliluctdusol__
#define CGNLiluclsol        cgnliluclsol__
#define CGNLiluctlsol       cgnliluctlsol__
#define CGNLilucusol        cgnlilucusol__
#define CGNLiluctusol       cgnliluctusol__

#define CGNLpilucdlsol      cgnlpilucdlsol__
#define CGNLpiluctdlsol     cgnlpiluctdlsol__
#define CGNLpilucdusol      cgnlpilucdusol__
#define CGNLpiluctdusol     cgnlpiluctdusol__
#define CGNLpiluclsol       cgnlpiluclsol__
#define CGNLpiluctlsol      cgnlpiluctlsol__
#define CGNLpilucusol       cgnlpilucusol__
#define CGNLpiluctusol      cgnlpiluctusol__

#define CHERildlc           cherildlc__
#define CHERildlcsol        cherildlcsol__
#define CSHRildlc           cshrildlc__
#define CSHRildlcsol        cshrildlcsol__
#define CSYMildlc           csymildlc__
#define CSYMildlcsol        csymildlcsol__
#define CSSMildlc           cssmildlc__
#define CSSMildlcsol        cssmildlcsol__
#define CGNLpiluc           cgnlpiluc__
#define CGNLspiluc          cgnlspiluc__
#define CGNLmpiluc          cgnlmpiluc__
#define CHPDpiluc           chpdpiluc__
#define CHPDmpiluc          chpdmpiluc__
#define CSYMpiluc           csympiluc__
#define CSYMiluc            csymiluc__
#define CSYMmpiluc          csymmpiluc__
#define CSYMpilucsol        csympilucsol__
#define CHERpiluc           cherpiluc__
#define CHERiluc            cheriluc__
#define CHERmpiluc          chermpiluc__
#define CHERpilucsol        cherpilucsol__
#define CSYMpiluclsol       csympiluclsol__
#define CSYMpilucusol       csympilucusol__
#define CHERpiluclsol       cherpiluclsol__
#define CHERpilucusol       cherpilucusol__


#define Cpcg                cpcg__
#define Cbcg                cbcg__
#define CSYMbcg             csymbcg__
#define CHERbcg             cherbcg__
#define CSYMqmr             csymqmr__
#define CHERqmr             cherqmr__
#define Cgmres              cgmres__
#define Cfgmres             cfgmres__
#define Cdistdotc           cdistdotc__
#define Cdistdotu           cdistdotu__


#define Croscal             croscal__
#define Ccoscal             ccoscal__
#define Crowscale           crowscale__
#define Ccolscale           ccolscale__
#define CHPDscale           chpdscale__
#define CSYMscale           csymscale__
#define CHERscale           cherscale__
#define Ccsrcsc             ccsrcsc__
#define Cqsort              cqsort__
#define Cqqsort             cqqsort__
#define Cqqsort2            cqqsort2__
#define Cqqsorts            cqqsorts__
#define Cqqsorts2           cqqsorts2__


#define Creadmtc            creadmtc__
#define Cwritemtc           cwritemtc__
#define Creadvectors        creadvectors__
#define Cwritevectors       cwritevectors__



// no switch defined use lower case letters in FORTRAN
#else
#define mysdot                    mysdot
#define myddot                    myddot
#define mycdotc                   mycdotc
#define myzdotc                   myzdotc
#define sdsymamgsavediag          sdsymamgsavediag	   
#define sdsymamgsavediaggep       sdsymamgsavediaggep
#define sdsymamgsol		  sdsymamgsol		   
#define sdsymamginit		  sdsymamginit		   
#define sdsymamgrestorediag	  sdsymamgrestorediag	   
#define sdamgundoscaling	  sdamgundoscaling	   
#define sdsymamgfactor		  sdsymamgfactor		   
#define sdsymamgfactorgep	  sdsymamgfactorgep

#define czheramgsavediag	  czheramgsavediag	   
#define czheramgsavediaggep	  czheramgsavediaggep
#define czheramgsol		  czheramgsol		   
#define czheramginit		  czheramginit		   
#define czheramgrestorediag	  czheramgrestorediag	   
#define czamgundoscaling	  czamgundoscaling	   
#define czheramgfactor            czheramgfactor              
#define czheramgfactorgep         czheramgfactorgep

#define iprandom            iprandom
#define ipsrandom           ipsrandom
#define evaluatetime        evaluatetime
#define dprivatesptrs       dprivatesptrs
#define sprivatesptrs       sprivatesptrs
#define cprivatehptrs       cprivatehptrs
#define zprivatehptrs       zprivatehptrs

#define sdglprecsetup        sdglprecsetup
#define sdglprecsol          sdglprecsol
#define sdglprecdelete       sdglprecdelete
#define ddglprecsetup        ddglprecsetup
#define ddglprecsol          ddglprecsol
#define ddglprecdelete       ddglprecdelete

#define cdglprecsetup       cdglprecsetup
#define cdglprecsol         cdglprecsol
#define cdglprecdelete      cdglprecdelete
#define zdglprecsetup       zdglprecsetup
#define zdglprecsol         zdglprecsol
#define zdglprecdelete      zdglprecdelete

#define dsymamgsavediag     dsymamgsavediag
#define dsymamgsavediaggep  dsymamgsavediaggep
#define dsymamgrestorediag  dsymamgrestorediag
#define ssymamgsavediag     ssymamgsavediag  
#define ssymamgsavediaggep  ssymamgsavediaggep
#define ssymamgrestorediag  ssymamgrestorediag
#define cheramgsavediag     cheramgsavediag  
#define cheramgsavediaggep  cheramgsavediaggep
#define cheramgrestorediag  cheramgrestorediag
#define zheramgsavediag     zheramgsavediag
#define zheramgsavediaggep  zheramgsavediaggep
#define zheramgrestorediag  zheramgrestorediag

#define samgundoscaling     samgundoscaling
#define damgundoscaling     damgundoscaling
#define camgundoscaling     camgundoscaling
#define zamgundoscaling     zamgundoscaling

#define sgnlamginit         sgnlamginit             
#define sgnlamgfactor       sgnlamgfactor          
#define sgnlamgsolver       sgnlamgsolver       
#define sgnlamgsol          sgnlamgsol          
#define sgnlamgdelete       sgnlamgdelete          
#define sgnlamginfo         sgnlamginfo
#define sgnlamgnnz          sgnlamgnnz
                                                
#define ssymspdamgconvert   ssymspdamgconvert

#define sspdamginit         sspdamginit            
#define sspdamgfactor       sspdamgfactor          
#define sspdamgsolver       sspdamgsolver          
#define sspdamgsol          sspdamgsol          
#define sspdamgdelete       sspdamgdelete          
#define sspdamginfo         sspdamginfo
#define sspdamgnnz          sspdamgnnz
                                                
#define ssymamginit         ssymamginit            
#define ssymamgfactor       ssymamgfactor          
#define ssymamgfactorgep    ssymamgfactorgep
#define ssymamgsolver       ssymamgsolver          
#define ssymamgsol          ssymamgsol          
#define ssymamgdelete       ssymamgdelete          
#define ssymamginfo         ssymamginfo
#define ssymamgnnz          ssymamgnnz
                                                
                                                
#define dgnlamginit         dgnlamginit            
#define dgnlamgfactor       dgnlamgfactor          
#define dgnlamgsolver       dgnlamgsolver          
#define dgnlamgsol          dgnlamgsol          
#define dgnlamgdelete       dgnlamgdelete          
#define dgnlamginfo         dgnlamginfo
#define dgnlamgnnz          dgnlamgnnz
                                                
#define dsymspdamgconvert   dsymspdamgconvert

#define dspdamginit         dspdamginit            
#define dspdamgfactor       dspdamgfactor          
#define dspdamgsolver       dspdamgsolver          
#define dspdamgsol          dspdamgsol          
#define dspdamgdelete       dspdamgdelete          
#define dspdamginfo         dspdamginfo
#define dspdamgnnz          dspdamgnnz
                                                
#define dsymamginit         dsymamginit            
#define dsymamgfactor       dsymamgfactor          
#define dsymamgfactorgep    dsymamgfactorgep
#define dsymamgsolver       dsymamgsolver          
#define dsymamgsol          dsymamgsol          
#define dsymamgdelete       dsymamgdelete          
#define dsymamginfo         dsymamginfo
#define dsymamgnnz          dsymamgnnz
                                                
                                                
#define cgnlamginit         cgnlamginit            
#define cgnlamgfactor       cgnlamgfactor          
#define cgnlamgsolver       cgnlamgsolver          
#define cgnlamgsol          cgnlamgsol          
#define cgnlamgdelete       cgnlamgdelete          
#define cgnlamginfo         cgnlamginfo
#define cgnlamgnnz          cgnlamgnnz
                                                
#define cherhpdamgconvert   cherhpdamgconvert

#define chpdamginit         chpdamginit            
#define chpdamgfactor       chpdamgfactor          
#define chpdamgsolver       chpdamgsolver          
#define chpdamgsol          chpdamgsol          
#define chpdamgdelete       chpdamgdelete          
#define chpdamginfo         chpdamginfo
#define chpdamgnnz          chpdamgnnz
                                                
#define cheramginit         cheramginit            
#define cheramgfactor       cheramgfactor          
#define cheramgfactorgep    cheramgfactorgep
#define cheramgsolver       cheramgsolver          
#define cheramgsol          cheramgsol          
#define cheramgdelete       cheramgdelete          
#define cheramginfo         cheramginfo
#define cheramgnnz          cheramgnnz
                                                
#define csymamginit         csymamginit            
#define csymamgfactor       csymamgfactor          
#define csymamgsolver       csymamgsolver          
#define csymamgsol          csymamgsol          
#define csymamgdelete       csymamgdelete          
#define csymamginfo         csymamginfo
#define csymamgnnz          csymamgnnz
                                                
                                                
#define zgnlamginit         zgnlamginit            
#define zgnlamgfactor       zgnlamgfactor          
#define zgnlamgsolver       zgnlamgsolver          
#define zgnlamgsol          zgnlamgsol          
#define zgnlamgdelete       zgnlamgdelete          
#define zgnlamginfo         zgnlamginfo
#define zgnlamgnnz          zgnlamgnnz
                                                
#define zherhpdamgconvert   zherhpdamgconvert

#define zhpdamginit         zhpdamginit            
#define zhpdamgfactor       zhpdamgfactor          
#define zhpdamgsolver       zhpdamgsolver          
#define zhpdamgsol          zhpdamgsol          
#define zhpdamgdelete       zhpdamgdelete          
#define zhpdamginfo         zhpdamginfo
#define zhpdamgnnz          zhpdamgnnz
                                                
#define zheramginit         zheramginit            
#define zheramgfactor       zheramgfactor          
#define zheramgfactorgep    zheramgfactorgep
#define zheramgsolver       zheramgsolver          
#define zheramgsol          zheramgsol          
#define zheramgdelete       zheramgdelete          
#define zheramginfo         zheramginfo
#define zheramgnnz          zheramgnnz
                                                
#define zsymamginit         zsymamginit            
#define zsymamgfactor       zsymamgfactor          
#define zsymamgsolver       zsymamgsolver          
#define zsymamgsol          zsymamgsol          
#define zsymamgdelete       zsymamgdelete       
#define zsymamginfo         zsymamginfo
#define zsymamgnnz          zsymamgnnz



#define qqsorti             qqsorti

#define dsymilupack         dsymilupack
#define dsymilupackfac      dsymilupackfac
#define dsymilupacksol      dsymilupacksol
#define dsymilupackdel      dsymilupackdel

#define DGNLlupq            dgnllupq
#define DGNLlupqsol         dgnllupqsol
#define DGNLlupqtsol        dgnllupqtsol
#define DGNLlupqlsol        dgnllupqlsol
#define DGNLlupqtlsol       dgnllupqtlsol
#define DGNLlupqusol        dgnllupqusol
#define DGNLlupqtusol       dgnllupqtusol
#define DGNLlupqdlsol       dgnllupqdlsol
#define DGNLlupqtdlsol      dgnllupqtdlsol
#define DGNLlupqdusol       dgnllupqdusol
#define DGNLlupqtdusol      dgnllupqtdusol
#define DSPDldlp            dspdldlp
#define DSPDldlpsol         dspdldlpsol

#define DGNLilutp           dgnlilutp
#define DGNLilut            dgnlilut
#define DGNLlusol           dgnllusol
#define DGNLlutsol          dgnllutsol
#define DGNLlulsol          dgnllulsol
#define DGNLlutlsol         dgnllutlsol
#define DGNLluusol          dgnlluusol
#define DGNLlutusol         dgnllutusol
#define DGNLludlsol         dgnlludlsol
#define DGNLlutdlsol        dgnllutdlsol
#define DGNLludusol         dgnlludusol
#define DGNLlutdusol        dgnllutdusol

#define DGNLiluc            dgnliluc
#define DGNLilucsol         dgnlilucsol
#define DGNLiluctsol        dgnliluctsol
#define DGNLilucdlsol       dgnlilucdlsol
#define DGNLiluctdlsol      dgnliluctdlsol
#define DGNLilucdusol       dgnlilucdusol
#define DGNLiluctdusol      dgnliluctdusol
#define DGNLiluclsol        dgnliluclsol
#define DGNLiluctlsol       dgnliluctlsol
#define DGNLilucusol        dgnlilucusol
#define DGNLiluctusol       dgnliluctusol

#define DGNLpilucdlsol      dgnlpilucdlsol
#define DGNLpiluctdlsol     dgnlpiluctdlsol
#define DGNLpilucdusol      dgnlpilucdusol
#define DGNLpiluctdusol     dgnlpiluctdusol
#define DGNLpiluclsol       dgnlpiluclsol
#define DGNLpiluctlsol      dgnlpiluctlsol
#define DGNLpilucusol       dgnlpilucusol
#define DGNLpiluctusol      dgnlpiluctusol

#define DSYMildlc           dsymildlc
#define DSYMildlcsol        dsymildlcsol
#define DSSMildlc           dssmildlc
#define DSSMildlcsol        dssmildlcsol
#define DGNLpiluc           dgnlpiluc
#define DGNLspiluc          dgnlspiluc
#define DGNLmpiluc          dgnlmpiluc
#define DSPDpiluc           dspdpiluc
#define DSPDmpiluc          dspdmpiluc
#define DSYMpiluc           dsympiluc
#define DSYMiluc            dsymiluc
#define DSYMmpiluc          dsymmpiluc
#define DSYMpilucsol        dsympilucsol
#define DSYMpiluclsol       dsympiluclsol
#define DSYMpilucusol       dsympilucusol


#define Dpcg                dpcg
#define Dbcg                dbcg
#define DSYMbcg             dsymbcg
#define DSYMqmr             dsymqmr
#define Dgmres              dgmres
#define Dfgmres             dfgmres
#define Ddistdot            ddistdot


#define Droscal             droscal
#define Dcoscal             dcoscal
#define Drowscale           drowscale
#define Dcolscale           dcolscale
#define DSPDscale           dspdscale
#define DSYMscale           dsymscale
#define Dcsrcsc             dcsrcsc
#define Dqsort              dqsort
#define Dqqsort             dqqsort
#define Dqqsort2            dqqsort2
#define Dqqsorts            dqqsorts
#define Dqqsorts2           dqqsorts2

#define Dreadmtc            dreadmtc
#define Dwritemtc           dwritemtc
#define Dreadvectors        dreadvectors
#define Dwritevectors       dwritevectors



#define ssymilupack         ssymilupack
#define ssymilupackfac      ssymilupackfac
#define ssymilupacksol      ssymilupacksol
#define ssymilupackdel      ssymilupackdel

#define SGNLlupq            sgnllupq
#define SGNLlupqsol         sgnllupqsol
#define SGNLlupqtsol        sgnllupqtsol
#define SGNLlupqlsol        sgnllupqlsol
#define SGNLlupqtlsol       sgnllupqtlsol
#define SGNLlupqusol        sgnllupqusol
#define SGNLlupqtusol       sgnllupqtusol
#define SGNLlupqdlsol       sgnllupqdlsol
#define SGNLlupqtdlsol      sgnllupqtdlsol
#define SGNLlupqdusol       sgnllupqdusol
#define SGNLlupqtdusol      sgnllupqtdusol
#define SSPDldlp            sspdldlp
#define SSPDldlpsol         sspdldlpsol

#define SGNLilutp           sgnlilutp
#define SGNLilut            sgnlilut
#define SGNLlusol           sgnllusol
#define SGNLlutsol          sgnllutsol
#define SGNLlulsol          sgnllulsol
#define SGNLlutlsol         sgnllutlsol
#define SGNLluusol          sgnlluusol
#define SGNLlutusol         sgnllutusol
#define SGNLludlsol         sgnlludlsol
#define SGNLlutdlsol        sgnllutdlsol
#define SGNLludusol         sgnlludusol
#define SGNLlutdusol        sgnllutdusol

#define SGNLiluc            sgnliluc
#define SGNLilucsol         sgnlilucsol
#define SGNLiluctsol        sgnliluctsol
#define SGNLilucdlsol       sgnlilucdlsol
#define SGNLiluctdlsol      sgnliluctdlsol
#define SGNLilucdusol       sgnlilucdusol
#define SGNLiluctdusol      sgnliluctdusol
#define SGNLiluclsol        sgnliluclsol
#define SGNLiluctlsol       sgnliluctlsol
#define SGNLilucusol        sgnlilucusol
#define SGNLiluctusol       sgnliluctusol

#define SGNLpilucdlsol      sgnlpilucdlsol
#define SGNLpiluctdlsol     sgnlpiluctdlsol
#define SGNLpilucdusol      sgnlpilucdusol
#define SGNLpiluctdusol     sgnlpiluctdusol
#define SGNLpiluclsol       sgnlpiluclsol
#define SGNLpiluctlsol      sgnlpiluctlsol
#define SGNLpilucusol       sgnlpilucusol
#define SGNLpiluctusol      sgnlpiluctusol

#define SSYMildlc           ssymildlc
#define SSYMildlcsol        ssymildlcsol
#define SSSMildlc           sssmildlc
#define SSSMildlcsol        sssmildlcsol
#define SGNLpiluc           sgnlpiluc
#define SGNLspiluc          sgnlspiluc
#define SGNLmpiluc          sgnlmpiluc
#define SSPDpiluc           sspdpiluc
#define SSPDmpiluc          sspdmpiluc
#define SSYMpiluc           ssympiluc
#define SSYMiluc            ssymiluc
#define SSYMmpiluc          ssymmpiluc
#define SSYMpilucsol        ssympilucsol
#define SSYMpiluclsol       ssympiluclsol
#define SSYMpilucusol       ssympilucusol


#define Spcg                spcg
#define Sbcg                sbcg
#define SSYMbcg             ssymbcg
#define SSYMqmr             ssymqmr
#define Sgmres              sgmres
#define Sfgmres             sfgmres
#define Sdistdot            sdistdot


#define Sroscal             sroscal
#define Scoscal             scoscal
#define Srowscale           srowscale
#define Scolscale           scolscale
#define SSPDscale           sspdscale
#define SSYMscale           ssymscale
#define Scsrcsc             scsrcsc
#define Sqsort              sqsort
#define Sqqsort             sqqsort
#define Sqqsort2            sqqsort2
#define Sqqsorts            sqqsorts
#define Sqqsorts2           sqqsorts2

#define Sreadmtc            sreadmtc
#define Swritemtc           swritemtc
#define Sreadvectors        sreadvectors
#define Swritevectors       swritevectors


#define zsymilupack         zsymilupack
#define zsymilupackfac      zsymilupackfac
#define zsymilupacksol      zsymilupacksol
#define zsymilupackdel      zsymilupackdel
#define zherilupack         zherilupack
#define zherilupackfac      zherilupackfac
#define zherilupacksol      zherilupacksol
#define zherilupackdel      zherilupackdel

#define ZGNLlupq            zgnllupq
#define ZGNLlupqsol         zgnllupqsol
#define ZGNLlupqtsol        zgnllupqtsol
#define ZGNLlupqhsol        zgnllupqhsol
#define ZGNLlupqlsol        zgnllupqlsol
#define ZGNLlupqtlsol       zgnllupqtlsol
#define ZGNLlupqusol        zgnllupqusol
#define ZGNLlupqtusol       zgnllupqtusol
#define ZGNLlupqdlsol       zgnllupqdlsol
#define ZGNLlupqtdlsol      zgnllupqtdlsol
#define ZGNLlupqdusol       zgnllupqdusol
#define ZGNLlupqtdusol      zgnllupqtdusol
#define ZHPDldlp            zhpdldlp
#define ZHPDldlpsol         zhpdldlpsol

#define ZGNLilutp           zgnlilutp
#define ZGNLilut            zgnlilut
#define ZGNLlusol           zgnllusol
#define ZGNLlutsol          zgnllutsol
#define ZGNLlulsol          zgnllulsol
#define ZGNLlutlsol         zgnllutlsol
#define ZGNLluusol          zgnlluusol
#define ZGNLlutusol         zgnllutusol
#define ZGNLludlsol         zgnlludlsol
#define ZGNLlutdlsol        zgnllutdlsol
#define ZGNLludusol         zgnlludusol
#define ZGNLlutdusol        zgnllutdusol

#define ZGNLiluc            zgnliluc
#define ZGNLilucsol         zgnlilucsol
#define ZGNLiluctsol        zgnliluctsol
#define ZGNLilucdlsol       zgnlilucdlsol
#define ZGNLiluctdlsol      zgnliluctdlsol
#define ZGNLilucdusol       zgnlilucdusol
#define ZGNLiluctdusol      zgnliluctdusol
#define ZGNLiluclsol        zgnliluclsol
#define ZGNLiluctlsol       zgnliluctlsol
#define ZGNLilucusol        zgnlilucusol
#define ZGNLiluctusol       zgnliluctusol

#define ZGNLpilucdlsol      zgnlpilucdlsol
#define ZGNLpiluctdlsol     zgnlpiluctdlsol
#define ZGNLpilucdusol      zgnlpilucdusol
#define ZGNLpiluctdusol     zgnlpiluctdusol
#define ZGNLpiluclsol       zgnlpiluclsol
#define ZGNLpiluctlsol      zgnlpiluctlsol
#define ZGNLpilucusol       zgnlpilucusol
#define ZGNLpiluctusol      zgnlpiluctusol

#define ZHERildlc           zherildlc
#define ZHERildlcsol        zherildlcsol
#define ZSHRildlc           zshrildlc
#define ZSHRildlcsol        zshrildlcsol
#define ZSYMildlc           zsymildlc
#define ZSYMildlcsol        zsymildlcsol
#define ZSSMildlc           zssmildlc
#define ZSSMildlcsol        zssmildlcsol
#define ZGNLpiluc           zgnlpiluc
#define ZGNLspiluc          zgnlspiluc
#define ZGNLmpiluc          zgnlmpiluc
#define ZHPDpiluc           zhpdpiluc
#define ZHPDmpiluc          zhpdmpiluc
#define ZSYMpiluc           zsympiluc
#define ZSYMiluc            zsymiluc
#define ZSYMmpiluc          zsymmpiluc
#define ZSYMpilucsol        zsympilucsol
#define ZHERpiluc           zherpiluc
#define ZHERiluc            zheriluc
#define ZHERmpiluc          zhermpiluc
#define ZHERpilucsol        zherpilucsol
#define ZSYMpiluclsol       zsympiluclsol
#define ZSYMpilucusol       zsympilucusol
#define ZHERpiluclsol       zherpiluclsol
#define ZHERpilucusol       zherpilucusol


#define Zpcg                zpcg
#define Zbcg                zbcg
#define ZSYMbcg             zsymbcg
#define ZHERbcg             zherbcg
#define ZSYMqmr             zsymqmr
#define ZHERqmr             zherqmr
#define Zgmres              zgmres
#define Zfgmres             zfgmres
#define Zdistdotc           zdistdotc
#define Zdistdotu           zdistdotu


#define Zroscal             zroscal
#define Zcoscal             zcoscal
#define Zrowscale           zrowscale
#define Zcolscale           zcolscale
#define ZHPDscale           zhpdscale
#define ZSYMscale           zsymscale
#define ZHERscale           zherscale
#define Zcsrcsc             zcsrcsc
#define Zqsort              zqsort
#define Zqqsort             zqqsort
#define Zqqsort2            zqqsort2
#define Zqqsorts            zqqsorts
#define Zqqsorts2           zqqsorts2

#define Zreadmtc            zreadmtc
#define Zwritemtc           zwritemtc
#define Zreadvectors        zreadvectors
#define Zwritevectors       zwritevectors



#define csymilupack         csymilupack
#define csymilupackfac      csymilupackfac
#define csymilupacksol      csymilupacksol
#define csymilupackdel      csymilupackdel
#define cherilupack         cherilupack
#define cherilupackfac      cherilupackfac
#define cherilupacksol      cherilupacksol
#define cherilupackdel      cherilupackdel

#define CGNLlupq            cgnllupq
#define CGNLlupqsol         cgnllupqsol
#define CGNLlupqtsol        cgnllupqtsol
#define CGNLlupqhsol        cgnllupqhsol
#define CGNLlupqlsol        cgnllupqlsol
#define CGNLlupqtlsol       cgnllupqtlsol
#define CGNLlupqusol        cgnllupqusol
#define CGNLlupqtusol       cgnllupqtusol
#define CGNLlupqdlsol       cgnllupqdlsol
#define CGNLlupqtdlsol      cgnllupqtdlsol
#define CGNLlupqdusol       cgnllupqdusol
#define CGNLlupqtdusol      cgnllupqtdusol
#define CHPDldlp            chpdldlp
#define CHPDldlpsol         chpdldlpsol

#define CGNLilutp           cgnlilutp
#define CGNLilut            cgnlilut
#define CGNLlusol           cgnllusol
#define CGNLlutsol          cgnllutsol
#define CGNLlulsol          cgnllulsol
#define CGNLlutlsol         cgnllutlsol
#define CGNLluusol          cgnlluusol
#define CGNLlutusol         cgnllutusol
#define CGNLludlsol         cgnlludlsol
#define CGNLlutdlsol        cgnllutdlsol
#define CGNLludusol         cgnlludusol
#define CGNLlutdusol        cgnllutdusol

#define CGNLiluc            cgnliluc
#define CGNLilucsol         cgnlilucsol
#define CGNLiluctsol        cgnliluctsol
#define CGNLilucdlsol       cgnlilucdlsol
#define CGNLiluctdlsol      cgnliluctdlsol
#define CGNLilucdusol       cgnlilucdusol
#define CGNLiluctdusol      cgnliluctdusol
#define CGNLiluclsol        cgnliluclsol
#define CGNLiluctlsol       cgnliluctlsol
#define CGNLilucusol        cgnlilucusol
#define CGNLiluctusol       cgnliluctusol

#define CGNLpilucdlsol      cgnlpilucdlsol
#define CGNLpiluctdlsol     cgnlpiluctdlsol
#define CGNLpilucdusol      cgnlpilucdusol
#define CGNLpiluctdusol     cgnlpiluctdusol
#define CGNLpiluclsol       cgnlpiluclsol
#define CGNLpiluctlsol      cgnlpiluctlsol
#define CGNLpilucusol       cgnlpilucusol
#define CGNLpiluctusol      cgnlpiluctusol

#define CHERildlc           cherildlc
#define CHERildlcsol        cherildlcsol
#define CSHRildlc           cshrildlc
#define CSHRildlcsol        cshrildlcsol
#define CSYMildlc           csymildlc
#define CSYMildlcsol        csymildlcsol
#define CSSMildlc           cssmildlc
#define CSSMildlcsol        cssmildlcsol
#define CGNLpiluc           cgnlpiluc
#define CGNLspiluc          cgnlspiluc
#define CGNLmpiluc          cgnlmpiluc
#define CHPDpiluc           chpdpiluc
#define CHPDmpiluc          chpdmpiluc
#define CSYMpiluc           csympiluc
#define CSYMiluc            csymiluc
#define CSYMmpiluc          csymmpiluc
#define CSYMpilucsol        csympilucsol
#define CHERpiluc           cherpiluc
#define CHERiluc            cheriluc
#define CHERmpiluc          chermpiluc
#define CHERpilucsol        cherpilucsol
#define CSYMpiluclsol       csympiluclsol
#define CSYMpilucusol       csympilucusol
#define CHERpiluclsol       cherpiluclsol
#define CHERpilucusol       cherpilucusol


#define Cpcg                cpcg
#define Cbcg                cbcg
#define CSYMbcg             csymbcg
#define CHERbcg             cherbcg
#define CSYMqmr             csymqmr
#define CHERqmr             cherqmr
#define Cgmres              cgmres
#define Cfgmres             cfgmres
#define Cdistdotc           cdistdotc
#define Cdistdotu           cdistdotu


#define Croscal             croscal
#define Ccoscal             ccoscal
#define Crowscale           crowscale
#define Ccolscale           ccolscale
#define CHPDscale           chpdscale
#define CSYMscale           csymscale
#define CHERscale           cherscale
#define Ccsrcsc             ccsrcsc
#define Cqsort              cqsort
#define Cqqsort             cqqsort
#define Cqqsort2            cqqsort2
#define Cqqsorts            cqqsorts
#define Cqqsorts2           cqqsorts2

#define Creadmtc            creadmtc
#define Cwritemtc           cwritemtc
#define Creadvectors        creadvectors
#define Cwritevectors       cwritevectors



#endif /* defined __CAPS__ && ... */


#endif /* _NAMES_ILU_PACK_H */


