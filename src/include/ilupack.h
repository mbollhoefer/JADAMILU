/*! \file ilupack.h
   \brief main header for ILUPACK

   This header contains all definitions of functions as well as those of the
   constants
*/
#ifndef _ILU_PACK_H
#define _ILU_PACK_H


#include <stdlib.h>

#include "long_integer.h"
#include "namesilupack.h"



#define LONG_INT integer
#define MEDIUM_INT int

/*! switch to indicate inverse-based dropping. It is used in AMGINIT
    AMGGETPARAMS and AMGSETPARAMS. The parameter "flag" is bitwise
    modified by flag|=DROP_INVERSE to set and flag&=~DROP_INVERSE to
    turn off inverse-based dropping.
    In AMGINIT, DROP_INVERSE is set by default
 */
#define DROP_INVERSE                     1

/*! switch for not shifting away zero pivots. This switch is used in ILUC
    which does not have pivoting prevent small diagonal entries.
    The parameter "param" is bitwise modified by param|=NO_SHIFT to 
    suppress shifts and param&=~NO_SHIFT to allow shifts.
 */
#define NO_SHIFT                         2

/*! switch for using Tismenetsky update. 
 */
#define TISMENETSKY_SC                   4
/* switch for repeated ILU */
#define REPEAT_FACT                      8
/* switch for enhanced estimate for the norm of the inverses */
#define IMPROVED_ESTIMATE               16
/* switch for using diagonal compensation */
#define DIAGONAL_COMPENSATION           32
/* switch for reducing the partial factorization to the non-coarse part */
#define COARSE_REDUCE                   64

/* switch for using a different pivoting strategy, if the regular reordering
   fails and before we switch to ILUTP
*/
#define FINAL_PIVOTING                 128
/* enforce the positve definite property */
#define ENSURE_SPD                     256

/* switch for the most simple Schur complement update */
#define SIMPLE_SC                      512


#define PREPROCESS_INITIAL_SYSTEM     1024
#define PREPROCESS_SUBSYSTEMS         2048
#define MULTI_PILUC                   4096


#define RE_FACTOR                     8192
#define AGGRESSIVE_DROPPING          16384
#define DISCARD_MATRIX               32768
#define SYMMETRIC_STRUCTURE          65536
/*
                                    131072
                                    262144
                                    524288
                                   1048576
                                   2097152
                                   4194304
                                   8388608
                                  16777216
                                  33554432
                                  67108864
                                 134217728 
                                 268435456 
                                 536870912
                                1073741824
                                2147483648
*/



#define _D_REAL_MAX_        1.7e+308
#define _S_REAL_MAX_        1.7e+38

/* ***************************************************** */
/* ******      Definitions for preconditioners     ***** */
typedef struct {
   integer nr;
   integer nc;
   integer *ia;
   integer *ja;
   doubleprecision *a;
} Dmat;

typedef struct {
   integer nr;
   integer nc;
   integer *ia;
   integer *ja;
   real *a;
} Smat;

typedef struct {
   integer nr;
   integer nc;
   integer *ia;
   integer *ja;
   doublecomplex *a;
} Zmat;

typedef struct {
   integer nr;
   integer nc;
   integer *ia;
   integer *ja;
   complex *a;
} Cmat;



#define ILUPACK_NIPAR   40
#define ILUPACK_NFPAR   40


typedef struct {
   integer              ipar[ILUPACK_NIPAR];
   doubleprecision  fpar[ILUPACK_NFPAR];
   integer              type;
   integer              *ibuff;
   integer              *iaux;
   doubleprecision  *dbuff;
   doubleprecision  *daux;
   integer              *ju;
   integer              *jlu;
   doubleprecision  *alu;
   size_t           nibuff, ndbuff, nju,njlu,nalu, ndaux,niaux;
   doubleprecision  elbow;
   doubleprecision  *diag;
} DILUPACKparam;

typedef struct {
   integer     ipar[ILUPACK_NIPAR];
   real    fpar[ILUPACK_NFPAR];
   integer     type;
   integer     *ibuff;
   integer     *iaux;
   real    *dbuff;
   real    *daux;
   integer     *ju;
   integer     *jlu;
   real    *alu;
   size_t  nibuff, ndbuff, nju,njlu,nalu, ndaux,niaux;
   real  elbow;
   real *diag;
} SILUPACKparam;

typedef struct {
   integer              ipar[ILUPACK_NIPAR];
   doubleprecision  fpar[ILUPACK_NFPAR];
   integer              type;
   integer              *ibuff;
   integer              *iaux;
   doublecomplex    *dbuff;
   doublecomplex    *daux;
   integer              *ju;
   integer              *jlu;
   doublecomplex    *alu;
   size_t           nibuff, ndbuff, nju,njlu,nalu, ndaux,niaux;
   doubleprecision  elbow;
   doubleprecision  *diag;
} ZILUPACKparam;

typedef struct {
   integer     ipar[ILUPACK_NIPAR];
   real    fpar[ILUPACK_NFPAR];
   integer     type;
   integer     *ibuff;
   integer     *iaux;
   complex *dbuff;
   complex *daux;
   integer     *ju;
   integer     *jlu;
   complex *alu;
   size_t  nibuff, ndbuff, nju,njlu,nalu, ndaux,niaux;
   real  elbow;
   real *diag;
} CILUPACKparam;



typedef struct  DAMGLM {
  integer n;                  
  integer nB; 
  Dmat LU;
  integer *LUperm;
  Dmat E;
  Dmat F;
  integer *p;
  integer *invq;
  doubleprecision *rowscal;
  doubleprecision *colscal;
  doubleprecision *absdiag;
  struct DAMGLM *prev;
  struct DAMGLM *next;
} DAMGlevelmat; 

typedef struct  SAMGLM {
  integer n;                  
  integer nB; 
  Smat LU;
  integer *LUperm;
  Smat E;
  Smat F;
  integer *p;
  integer *invq;
  real *rowscal;
  real *colscal;
  real *absdiag;
  struct SAMGLM *prev;
  struct SAMGLM *next;
} SAMGlevelmat; 

typedef struct  ZAMGLM {
  integer n;                  
  integer nB; 
  Zmat LU;
  integer *LUperm;
  Zmat E;
  Zmat F;
  integer *p;
  integer *invq;
  doublecomplex *rowscal;
  doublecomplex *colscal;
  doublecomplex *absdiag;
  struct ZAMGLM *prev;
  struct ZAMGLM *next;
} ZAMGlevelmat; 

typedef struct CAMGLM {
  integer n;                  
  integer nB; 
  Cmat LU;
  integer *LUperm;
  Cmat E;
  Cmat F;
  integer *p;
  integer *invq;
  complex *rowscal;
  complex *colscal;
  complex *absdiag;
  struct CAMGLM *prev;
  struct CAMGLM *next;
} CAMGlevelmat; 



void DGNLAMGsol(DAMGlevelmat, integer, DILUPACKparam *, doubleprecision *, doubleprecision *, doubleprecision *);
void DGNLAMGdlsol(DAMGlevelmat, integer, DILUPACKparam *, doubleprecision *, doubleprecision *, doubleprecision *);
void DGNLAMGdusol(DAMGlevelmat, integer, DILUPACKparam *, doubleprecision *, doubleprecision *, doubleprecision *);
void DGNLAMGlsol(DAMGlevelmat, integer, DILUPACKparam *, doubleprecision *, doubleprecision *, doubleprecision *);
void DGNLAMGusol(DAMGlevelmat, integer, DILUPACKparam *, doubleprecision *, doubleprecision *, doubleprecision *);
void DGNLAMGtdlsol(DAMGlevelmat, integer, DILUPACKparam *, doubleprecision *, doubleprecision *, doubleprecision *);
void DGNLAMGtdusol(DAMGlevelmat, integer, DILUPACKparam *, doubleprecision *, doubleprecision *, doubleprecision *);
void DGNLAMGtlsol(DAMGlevelmat, integer, DILUPACKparam *, doubleprecision *, doubleprecision *, doubleprecision *);
void DGNLAMGtusol(DAMGlevelmat, integer, DILUPACKparam *, doubleprecision *, doubleprecision *, doubleprecision *);
void DGNLAMGtsol(DAMGlevelmat, integer, DILUPACKparam *, doubleprecision *, doubleprecision *, doubleprecision *);
void DSPDAMGsol(DAMGlevelmat, integer, DILUPACKparam *, doubleprecision *, doubleprecision *, doubleprecision *);
void DSYMAMGsol(DAMGlevelmat, integer, DILUPACKparam *, doubleprecision *, doubleprecision *, doubleprecision *);
void DGNLAMGextract(Dmat *,Dmat *, Dmat, integer *,integer *,  integer);
void DSYMAMGextract(Dmat *, Dmat, integer *,integer *,  integer);
void DSSMAMGextract(Dmat *, Dmat, integer *,integer *,  integer);

void SGNLAMGsol(SAMGlevelmat, integer, SILUPACKparam *, real *, real *, real *);
void SGNLAMGdlsol(SAMGlevelmat, integer, SILUPACKparam *, real *, real *, real *);
void SGNLAMGdusol(SAMGlevelmat, integer, SILUPACKparam *, real *, real *, real *);
void SGNLAMGusol(SAMGlevelmat, integer, SILUPACKparam *, real *, real *, real *);
void SGNLAMGlsol(SAMGlevelmat, integer, SILUPACKparam *, real *, real *, real *);
void SGNLAMGtdlsol(SAMGlevelmat, integer, SILUPACKparam *, real *, real *, real *);
void SGNLAMGtdusol(SAMGlevelmat, integer, SILUPACKparam *, real *, real *, real *);
void SGNLAMGtusol(SAMGlevelmat, integer, SILUPACKparam *, real *, real *, real *);
void SGNLAMGtlsol(SAMGlevelmat, integer, SILUPACKparam *, real *, real *, real *);
void SGNLAMGtsol(SAMGlevelmat, integer, SILUPACKparam *, real *, real *, real *);
void SSPDAMGsol(SAMGlevelmat, integer, SILUPACKparam *, real *, real *, real *);
void SSYMAMGsol(SAMGlevelmat, integer, SILUPACKparam *, real *, real *, real *);
void SGNLAMGextract(Smat *,Smat *, Smat, integer *,integer *,  integer);
void SSYMAMGextract(Smat *, Smat, integer *,integer *,  integer);
void SSSMAMGextract(Smat *, Smat, integer *,integer *,  integer);

void ZGNLAMGsol(ZAMGlevelmat, integer, ZILUPACKparam *, doublecomplex *, doublecomplex *, doublecomplex *);
void ZGNLAMGdlsol(ZAMGlevelmat, integer, ZILUPACKparam *, doublecomplex *, doublecomplex *, doublecomplex *);
void ZGNLAMGdusol(ZAMGlevelmat, integer, ZILUPACKparam *, doublecomplex *, doublecomplex *, doublecomplex *);
void ZGNLAMGlsol(ZAMGlevelmat, integer, ZILUPACKparam *, doublecomplex *, doublecomplex *, doublecomplex *);
void ZGNLAMGusol(ZAMGlevelmat, integer, ZILUPACKparam *, doublecomplex *, doublecomplex *, doublecomplex *);
void ZGNLAMGtdlsol(ZAMGlevelmat, integer, ZILUPACKparam *, doublecomplex *, doublecomplex *, doublecomplex *);
void ZGNLAMGtdusol(ZAMGlevelmat, integer, ZILUPACKparam *, doublecomplex *, doublecomplex *, doublecomplex *);
void ZGNLAMGtlsol(ZAMGlevelmat, integer, ZILUPACKparam *, doublecomplex *, doublecomplex *, doublecomplex *);
void ZGNLAMGtusol(ZAMGlevelmat, integer, ZILUPACKparam *, doublecomplex *, doublecomplex *, doublecomplex *);
void ZGNLAMGtsol(ZAMGlevelmat, integer, ZILUPACKparam *, doublecomplex *, doublecomplex *, doublecomplex *);
void ZHPDAMGsol(ZAMGlevelmat, integer, ZILUPACKparam *, doublecomplex *, doublecomplex *, doublecomplex *);
void ZHERAMGsol(ZAMGlevelmat, integer, ZILUPACKparam *, doublecomplex *, doublecomplex *, doublecomplex *);
void ZSYMAMGsol(ZAMGlevelmat, integer, ZILUPACKparam *, doublecomplex *, doublecomplex *, doublecomplex *);
void ZGNLAMGextract(Zmat *,Zmat *, Zmat, integer *,integer *,  integer);
void ZHERAMGextract(Zmat *, Zmat, integer *,integer *,  integer);
void ZSHRAMGextract(Zmat *, Zmat, integer *,integer *,  integer);
void ZSYMAMGextract(Zmat *, Zmat, integer *,integer *,  integer);
void ZSSMAMGextract(Zmat *, Zmat, integer *,integer *,  integer);

void CGNLAMGsol(CAMGlevelmat, integer, CILUPACKparam *, complex *, complex *, complex *);
void CGNLAMGdlsol(CAMGlevelmat, integer, CILUPACKparam *, complex *, complex *, complex *);
void CGNLAMGdusol(CAMGlevelmat, integer, CILUPACKparam *, complex *, complex *, complex *);
void CGNLAMGlsol(CAMGlevelmat, integer, CILUPACKparam *, complex *, complex *, complex *);
void CGNLAMGusol(CAMGlevelmat, integer, CILUPACKparam *, complex *, complex *, complex *);
void CGNLAMGtdlsol(CAMGlevelmat, integer, CILUPACKparam *, complex *, complex *, complex *);
void CGNLAMGtdusol(CAMGlevelmat, integer, CILUPACKparam *, complex *, complex *, complex *);
void CGNLAMGtusol(CAMGlevelmat, integer, CILUPACKparam *, complex *, complex *, complex *);
void CGNLAMGtlsol(CAMGlevelmat, integer, CILUPACKparam *, complex *, complex *, complex *);
void CGNLAMGtsol(CAMGlevelmat, integer, CILUPACKparam *, complex *, complex *, complex *);
void CHPDAMGsol(CAMGlevelmat, integer, CILUPACKparam *, complex *, complex *, complex *);
void CHERAMGsol(CAMGlevelmat, integer, CILUPACKparam *, complex *, complex *, complex *);
void CSYMAMGsol(CAMGlevelmat, integer, CILUPACKparam *, complex *, complex *, complex *);
void CGNLAMGextract(Cmat *,Cmat *, Cmat, integer *,integer *,  integer);
void CHERAMGextract(Cmat *, Cmat, integer *,integer *,  integer);
void CSHRAMGextract(Cmat *, Cmat, integer *,integer *,  integer);
void CSYMAMGextract(Cmat *, Cmat, integer *,integer *,  integer);
void CSSMAMGextract(Cmat *, Cmat, integer *,integer *,  integer);


void DGNLlupq(integer *, doubleprecision *, integer *, integer *, integer *);
void DGNLlupqsol (integer *, doubleprecision *, integer *, integer *, 
		  doubleprecision *, doubleprecision *, doubleprecision *);
void DGNLlupqtsol(integer *, doubleprecision *, integer *, integer *, 
		  doubleprecision *, doubleprecision *, doubleprecision *);
void DGNLlupqlsol (integer *, doubleprecision *, integer *, integer *, 
		  doubleprecision *, doubleprecision *, doubleprecision *);
void DGNLlupqtlsol(integer *, doubleprecision *, integer *, integer *, 
		  doubleprecision *, doubleprecision *, doubleprecision *);
void DGNLlupqusol (integer *, doubleprecision *, integer *, integer *, 
		  doubleprecision *, doubleprecision *, doubleprecision *);
void DGNLlupqtusol(integer *, doubleprecision *, integer *, integer *, 
		  doubleprecision *, doubleprecision *, doubleprecision *);
void DGNLlupqdlsol (integer *, doubleprecision *, integer *, integer *, 
		  doubleprecision *, doubleprecision *, doubleprecision *);
void DGNLlupqtdlsol(integer *, doubleprecision *, integer *, integer *, 
		  doubleprecision *, doubleprecision *, doubleprecision *);
void DGNLlupqdusol (integer *, doubleprecision *, integer *, integer *, 
		  doubleprecision *, doubleprecision *, doubleprecision *);
void DGNLlupqtdusol(integer *, doubleprecision *, integer *, integer *, 
		  doubleprecision *, doubleprecision *, doubleprecision *);

void SGNLlupq(integer *, real *, integer *, integer *, integer *);
void SGNLlupqsol (integer *, real *, integer *, integer *, real *, real *, 
		  real *);
void SGNLlupqtsol(integer *, real *, integer *, integer *, real *, real *, 
		  real *);
void SGNLlupqlsol (integer *, real *, integer *, integer *, real *, real *, 
		  real *);
void SGNLlupqtlsol(integer *, real *, integer *, integer *, real *, real *, 
		  real *);
void SGNLlupqusol (integer *, real *, integer *, integer *, real *, real *, 
		  real *);
void SGNLlupqtusol(integer *, real *, integer *, integer *, real *, real *, 
		  real *);
void SGNLlupqdlsol (integer *, real *, integer *, integer *, real *, real *, 
		  real *);
void SGNLlupqtdlsol(integer *, real *, integer *, integer *, real *, real *, 
		  real *);
void SGNLlupqdusol (integer *, real *, integer *, integer *, real *, real *, 
		  real *);
void SGNLlupqtdusol(integer *, real *, integer *, integer *, real *, real *, 
		  real *);

void ZGNLlupq(integer *, doublecomplex *, integer *, integer *, integer *);
void ZGNLlupqsol (integer *, doublecomplex *, integer *, integer *, doublecomplex *, doublecomplex *, 
		  doublecomplex *);
void ZGNLlupqtsol(integer *, doublecomplex *, integer *, integer *, doublecomplex *, doublecomplex *, 
		  doublecomplex *);
void ZGNLlupqlsol (integer *, doublecomplex *, integer *, integer *, doublecomplex *, doublecomplex *, 
		  doublecomplex *);
void ZGNLlupqtlsol(integer *, doublecomplex *, integer *, integer *, doublecomplex *, doublecomplex *, 
		  doublecomplex *);
void ZGNLlupqusol (integer *, doublecomplex *, integer *, integer *, doublecomplex *, doublecomplex *, 
		  doublecomplex *);
void ZGNLlupqtusol(integer *, doublecomplex *, integer *, integer *, doublecomplex *, doublecomplex *, 
		  doublecomplex *);
void ZGNLlupqdlsol (integer *, doublecomplex *, integer *, integer *, doublecomplex *, doublecomplex *, 
		  doublecomplex *);
void ZGNLlupqtdlsol(integer *, doublecomplex *, integer *, integer *, doublecomplex *, doublecomplex *, 
		  doublecomplex *);
void ZGNLlupqdusol (integer *, doublecomplex *, integer *, integer *, doublecomplex *, doublecomplex *, 
		  doublecomplex *);
void ZGNLlupqtdusol(integer *, doublecomplex *, integer *, integer *, doublecomplex *, doublecomplex *, 
		  doublecomplex *);

void CGNLlupq(integer *, complex *, integer *, integer *, integer *);
void CGNLlupqsol (integer *, complex *, integer *, integer *, complex *, complex *, 
		  complex *);
void CGNLlupqtsol(integer *, complex *, integer *, integer *, complex *, complex *, 
		  complex *);
void CGNLlupqlsol (integer *, complex *, integer *, integer *, complex *, complex *, 
		  complex *);
void CGNLlupqtlsol(integer *, complex *, integer *, integer *, complex *, complex *, 
		  complex *);
void CGNLlupqusol (integer *, complex *, integer *, integer *, complex *, complex *, 
		  complex *);
void CGNLlupqtusol(integer *, complex *, integer *, integer *, complex *, complex *, 
		  complex *);
void CGNLlupqdlsol (integer *, complex *, integer *, integer *, complex *, complex *, 
		  complex *);
void CGNLlupqtdlsol(integer *, complex *, integer *, integer *, complex *, complex *, 
		  complex *);
void CGNLlupqdusol (integer *, complex *, integer *, integer *, complex *, complex *, 
		  complex *);
void CGNLlupqtdusol(integer *, complex *, integer *, integer *, complex *, complex *, 
		  complex *);


void DSPDldlp(integer *, doubleprecision *, integer *, integer *, integer *);
void DSPDldlpsol (integer *, doubleprecision *, integer *,
		  doubleprecision *, doubleprecision *, integer *);

void SSPDldlp(integer *, real *, integer *, integer *, integer *);
void SSPDldlpsol (integer *, real *, integer *,
		  real *, real *, integer *);

void ZHPDldlp(integer *, doublecomplex *, integer *, integer *, integer *);
void ZHPDldlpsol (integer *, doublecomplex *, integer *,
		  doublecomplex *, doublecomplex *, integer *);

void CHPDldlp(integer *, complex *, integer *, integer *, integer *);
void CHPDldlpsol (integer *, complex *, integer *,
		  complex *, complex *, integer *);


integer DGNLAMGfactor(Dmat *, DAMGlevelmat *, integer *, DILUPACKparam *,
		  integer (*p0)(Dmat,doubleprecision *,doubleprecision *,integer *, 
			    integer *,integer *, DILUPACKparam *),
		  integer (*p) (Dmat, doubleprecision *, doubleprecision *, integer *,
			    integer *,integer *, DILUPACKparam *),
		  integer (*pf)(Dmat,doubleprecision *,doubleprecision *,integer *, 
			    integer *,integer *, DILUPACKparam *));
integer DSPDAMGfactor(Dmat *, DAMGlevelmat *, integer *, DILUPACKparam *,
		  integer (*p0)(Dmat, doubleprecision *,doubleprecision *, integer *, 
			    integer *,integer *, DILUPACKparam *),
		  integer (*p) (Dmat, doubleprecision *,doubleprecision *, integer *,
			    integer *,integer *, DILUPACKparam *),
		  integer (*pf)(Dmat, doubleprecision *,doubleprecision *, integer *, 
			    integer *,integer *, DILUPACKparam *));
integer DSYMAMGfactor(Dmat *, DAMGlevelmat *, integer *, DILUPACKparam *,
		  integer (*p0)(Dmat, doubleprecision *,doubleprecision *, integer *, 
			    integer *,integer *, DILUPACKparam *),
		  integer (*p) (Dmat, doubleprecision *,doubleprecision *, integer *,
			    integer *,integer *, DILUPACKparam *),
		  integer (*pf)(Dmat, doubleprecision *,doubleprecision *, integer *, 
			    integer *,integer *, DILUPACKparam *));

integer SGNLAMGfactor(Smat *, SAMGlevelmat *, integer *, SILUPACKparam *,
		  integer (*p0)(Smat, real *,real *,integer *, 
			    integer *,integer *, SILUPACKparam *),
		  integer (*p) (Smat, real *,real *, integer *,
			    integer *,integer *, SILUPACKparam *),
		  integer (*pf)(Smat, real *,real *, integer *, 
			    integer *,integer *, SILUPACKparam *));
integer SSPDAMGfactor(Smat *, SAMGlevelmat *, integer *, SILUPACKparam *,
		  integer (*p0)(Smat, real *,real *, integer *, 
			    integer *,integer *, SILUPACKparam *),
		  integer (*p) (Smat, real *,real *, integer *,
			    integer *,integer *, SILUPACKparam *),
		  integer (*pf)(Smat, real *,real *, integer *, 
			    integer *,integer *, SILUPACKparam *));
integer SSYMAMGfactor(Smat *, SAMGlevelmat *, integer *, SILUPACKparam *,
		  integer (*p0)(Smat, real *,real *, integer *, 
			    integer *,integer *, SILUPACKparam *),
		  integer (*p) (Smat, real *,real *, integer *,
			    integer *,integer *, SILUPACKparam *),
		  integer (*pf)(Smat, real *,real *, integer *, 
			    integer *,integer *, SILUPACKparam *));

integer ZGNLAMGfactor(Zmat *, ZAMGlevelmat *, integer *, ZILUPACKparam *,
		  integer (*p0)(Zmat, doublecomplex *, doublecomplex *, integer *,
			    integer *,integer *, ZILUPACKparam *),
		  integer (*p) (Zmat, doublecomplex *, doublecomplex *, integer *,
			    integer *,integer *, ZILUPACKparam *),
		  integer (*pf)(Zmat, doublecomplex *, doublecomplex *, integer *,
			    integer *,integer *, ZILUPACKparam *));
integer ZHPDAMGfactor(Zmat *, ZAMGlevelmat *, integer *, ZILUPACKparam *,
		  integer (*p0)(Zmat, doublecomplex *, doublecomplex *, integer *,
			    integer *,integer *, ZILUPACKparam *),
		  integer (*p) (Zmat, doublecomplex *, doublecomplex *, integer *,
			    integer *,integer *, ZILUPACKparam *),
		  integer (*pf)(Zmat, doublecomplex *,  doublecomplex *, integer *,
			    integer *,integer *, ZILUPACKparam *));
integer ZHERAMGfactor(Zmat *, ZAMGlevelmat *, integer *, ZILUPACKparam *,
		  integer (*p0)(Zmat, doublecomplex *, doublecomplex *, integer *,
			    integer *,integer *, ZILUPACKparam *),
		  integer (*p) (Zmat, doublecomplex *, doublecomplex *, integer *,
			    integer *,integer *, ZILUPACKparam *),
		  integer (*pf)(Zmat, doublecomplex *,  doublecomplex *, integer *,
			    integer *,integer *, ZILUPACKparam *));
integer ZSYMAMGfactor(Zmat *, ZAMGlevelmat *, integer *, ZILUPACKparam *,
		  integer (*p0)(Zmat, doublecomplex *, doublecomplex *, integer *,
			    integer *,integer *, ZILUPACKparam *),
		  integer (*p) (Zmat, doublecomplex *, doublecomplex *, integer *,
			    integer *,integer *, ZILUPACKparam *),
		  integer (*pf)(Zmat, doublecomplex *,  doublecomplex *, integer *,
			    integer *,integer *, ZILUPACKparam *));

integer CGNLAMGfactor(Cmat *, CAMGlevelmat *, integer *, CILUPACKparam *,
		  integer (*p0)(Cmat, complex *, complex *, integer *,
			    integer *,integer *, CILUPACKparam *),
		  integer (*p) (Cmat, complex *, complex *, integer *,
			    integer *,integer *, CILUPACKparam *),
		  integer (*pf)(Cmat, complex *, complex *, integer *,
			    integer *,integer *, CILUPACKparam *));
integer CHPDAMGfactor(Cmat *, CAMGlevelmat *, integer *, CILUPACKparam *,
		  integer (*p0)(Cmat, complex *, complex *, integer *,
			    integer *,integer *, CILUPACKparam *),
		  integer (*p) (Cmat, complex *, complex *, integer *,
			    integer *,integer *, CILUPACKparam *),
		  integer (*pf)(Cmat, complex *,  complex *, integer *,
			    integer *,integer *, CILUPACKparam *));
integer CHERAMGfactor(Cmat *, CAMGlevelmat *, integer *, CILUPACKparam *,
		  integer (*p0)(Cmat, complex *, complex *, integer *,
			    integer *,integer *, CILUPACKparam *),
		  integer (*p) (Cmat, complex *, complex *, integer *,
			    integer *,integer *, CILUPACKparam *),
		  integer (*pf)(Cmat, complex *,  complex *, integer *,
			    integer *,integer *, CILUPACKparam *));
integer CSYMAMGfactor(Cmat *, CAMGlevelmat *, integer *, CILUPACKparam *,
		  integer (*p0)(Cmat, complex *, complex *, integer *,
			    integer *,integer *, CILUPACKparam *),
		  integer (*p) (Cmat, complex *, complex *, integer *,
			    integer *,integer *, CILUPACKparam *),
		  integer (*pf)(Cmat, complex *,  complex *, integer *,
			    integer *,integer *, CILUPACKparam *));


void       DGNLilut(integer *,doubleprecision *,integer *,integer *,integer *,
		    doubleprecision *,
		    doubleprecision *,integer *,integer *,integer *, 
		    doubleprecision *,integer *, integer *);
void       DGNLilutp(integer *,doubleprecision *,integer *,integer *,integer *,
		     doubleprecision *,doubleprecision *,integer *,
		     doubleprecision *,integer *,integer *,integer *, 
		     doubleprecision *,integer *,integer *,integer *);
void       DGNLlusol (integer *,doubleprecision *,doubleprecision *,doubleprecision *,integer *,integer *);
void       DGNLlutsol(integer *,doubleprecision *,doubleprecision *,doubleprecision *,integer *,integer *);
void       DGNLludlsol (integer *,doubleprecision *,doubleprecision *,doubleprecision *,integer *,integer *);
void       DGNLlutdlsol(integer *,doubleprecision *,doubleprecision *,doubleprecision *,integer *,integer *);
void       DGNLludusol (integer *,doubleprecision *,doubleprecision *,doubleprecision *,integer *,integer *);
void       DGNLlutdusol(integer *,doubleprecision *,doubleprecision *,doubleprecision *,integer *,integer *);
void       DGNLlulsol (integer *,doubleprecision *,doubleprecision *,doubleprecision *,integer *,integer *);
void       DGNLlutlsol(integer *,doubleprecision *,doubleprecision *,doubleprecision *,integer *,integer *);
void       DGNLluusol (integer *,doubleprecision *,doubleprecision *,doubleprecision *,integer *,integer *);
void       DGNLlutusol(integer *,doubleprecision *,doubleprecision *,doubleprecision *,integer *,integer *);

void       SGNLilut(integer *,real *,integer *,integer *,integer *,
		    real *,
		    real *,integer *,integer *,integer *, 
		    real *,integer *, integer *);
void       SGNLilutp(integer *,real *,integer *,integer *,integer *,
		     real *,real *,integer *,
		     real *,integer *,integer *,integer *, 
		     real *,integer *,integer *,integer *);
void       SGNLlusol (integer *,real *,real *,real *,integer *,integer *);
void       SGNLlutsol(integer *,real *,real *,real *,integer *,integer *);
void       SGNLlulsol (integer *,real *,real *,real *,integer *,integer *);
void       SGNLlutlsol(integer *,real *,real *,real *,integer *,integer *);
void       SGNLluusol (integer *,real *,real *,real *,integer *,integer *);
void       SGNLlutusol(integer *,real *,real *,real *,integer *,integer *);
void       SGNLludlsol (integer *,real *,real *,real *,integer *,integer *);
void       SGNLlutdlsol(integer *,real *,real *,real *,integer *,integer *);
void       SGNLludusol (integer *,real *,real *,real *,integer *,integer *);
void       SGNLlutdusol(integer *,real *,real *,real *,integer *,integer *);

void       ZGNLilut (integer *,doublecomplex *,integer *,integer *,integer *,
		     doubleprecision *,
		     doublecomplex *,integer *,integer *,integer *, 
		     doublecomplex *,integer *,integer *);
void       ZGNLilutp(integer *,doublecomplex *,integer *,integer *,integer *,
		     doubleprecision *,doubleprecision *,integer *,
		     doublecomplex *,integer *,integer *,integer *, 
		     doublecomplex *,integer *,integer *,integer *);
void       ZGNLlusol (integer *,doublecomplex *,doublecomplex *,doublecomplex *,integer *,integer *);
void       ZGNLlutsol(integer *,doublecomplex *,doublecomplex *,doublecomplex *,integer *,integer *);
void       ZGNLlulsol (integer *,doublecomplex *,doublecomplex *,doublecomplex *,integer *,integer *);
void       ZGNLlutlsol(integer *,doublecomplex *,doublecomplex *,doublecomplex *,integer *,integer *);
void       ZGNLluusol (integer *,doublecomplex *,doublecomplex *,doublecomplex *,integer *,integer *);
void       ZGNLlutusol(integer *,doublecomplex *,doublecomplex *,doublecomplex *,integer *,integer *);
void       ZGNLludlsol (integer *,doublecomplex *,doublecomplex *,doublecomplex *,integer *,integer *);
void       ZGNLlutdlsol(integer *,doublecomplex *,doublecomplex *,doublecomplex *,integer *,integer *);
void       ZGNLludusol (integer *,doublecomplex *,doublecomplex *,doublecomplex *,integer *,integer *);
void       ZGNLlutdusol(integer *,doublecomplex *,doublecomplex *,doublecomplex *,integer *,integer *);

void       CGNLilut (integer *,complex *,integer *,integer *,integer *,
		     real *,
		     complex *,integer *,integer *,integer *, 
		     complex *,integer *,integer *);
void       CGNLilutp(integer *,complex *,integer *,integer *,integer *,
		     real *,real *,integer *,
		     complex *,integer *,integer *,integer *, 
		     complex *,integer *,integer *,integer *);
void       CGNLlusol (integer *,complex *,complex *,complex *,integer *,integer *);
void       CGNLlutsol(integer *,complex *,complex *,complex *,integer *,integer *);
void       CGNLlulsol (integer *,complex *,complex *,complex *,integer *,integer *);
void       CGNLlutlsol(integer *,complex *,complex *,complex *,integer *,integer *);
void       CGNLluusol (integer *,complex *,complex *,complex *,integer *,integer *);
void       CGNLlutusol(integer *,complex *,complex *,complex *,integer *,integer *);
void       CGNLludlsol (integer *,complex *,complex *,complex *,integer *,integer *);
void       CGNLlutdlsol(integer *,complex *,complex *,complex *,integer *,integer *);
void       CGNLludusol (integer *,complex *,complex *,complex *,integer *,integer *);
void       CGNLlutdusol(integer *,complex *,complex *,complex *,integer *,integer *);


void DGNLiluc(integer *,doubleprecision *,integer *,integer *,integer *,
	      doubleprecision *,integer *,doubleprecision *,integer *,integer *,
	      integer *,doubleprecision *,integer *,integer *);
void DGNLilucsol (integer *,doubleprecision *,doubleprecision *,doubleprecision *,integer *,
		  integer *);
void DGNLiluctsol(integer *,doubleprecision *,doubleprecision *,doubleprecision *,integer *,
		  integer *);
void DGNLilucdlsol (integer *,doubleprecision *,doubleprecision *,doubleprecision *,integer *,
		  integer *);
void DGNLiluctdlsol(integer *,doubleprecision *,doubleprecision *,doubleprecision *,integer *,
		  integer *);
void DGNLilucdusol (integer *,doubleprecision *,doubleprecision *,doubleprecision *,integer *,
		  integer *);
void DGNLiluctdusol(integer *,doubleprecision *,doubleprecision *,doubleprecision *,integer *,
		  integer *);
void DGNLiluclsol (integer *,doubleprecision *,doubleprecision *,doubleprecision *,integer *,
		  integer *);
void DGNLiluctlsol(integer *,doubleprecision *,doubleprecision *,doubleprecision *,integer *,
		  integer *);
void DGNLilucusol (integer *,doubleprecision *,doubleprecision *,doubleprecision *,integer *,
		  integer *);
void DGNLiluctusol(integer *,doubleprecision *,doubleprecision *,doubleprecision *,integer *,
		  integer *);

void DGNLpiluclsol  (integer *,integer *, doubleprecision *,doubleprecision *,
		     doubleprecision *,integer *,integer *);
void DGNLpilucdlsol (integer *,integer *, doubleprecision *,doubleprecision *,
		     doubleprecision *,integer *,integer *);
void DGNLpilucusol  (integer *,integer *, doubleprecision *,doubleprecision *,
		     doubleprecision *,integer *,integer *);
void DGNLpilucdusol (integer *,integer *, doubleprecision *,doubleprecision *,
		     doubleprecision *,integer *,integer *);
void DGNLpiluctlsol (integer *,integer *, doubleprecision *,doubleprecision *,
		     doubleprecision *,integer *,integer *);
void DGNLpiluctdlsol(integer *,integer *, doubleprecision *,doubleprecision *,
		     doubleprecision *,integer *,integer *);
void DGNLpiluctusol (integer *,integer *, doubleprecision *,doubleprecision *,
		     doubleprecision *,integer *,integer *);
void DGNLpiluctdusol(integer *,integer *, doubleprecision *,doubleprecision *,
		     doubleprecision *,integer *,integer *);

void DSYMildlc(integer *,doubleprecision *,integer *,integer *,integer *,
	       doubleprecision *,integer *,doubleprecision *,integer *,integer *,
	       doubleprecision *,integer *,integer *);
void DSYMildlcsol(integer *,doubleprecision *,doubleprecision *,doubleprecision *,
		 integer *);
void DSYMpilucsol(integer *,doubleprecision *,doubleprecision *,doubleprecision *,
		 integer *);
void DSSMildlc(integer *,doubleprecision *,integer *,integer *,integer *,
	       doubleprecision *,integer *,doubleprecision *,integer *,integer *,
	       doubleprecision *,integer *,integer *);
void DSSMildlcsol(integer *,doubleprecision *,doubleprecision *,doubleprecision *,
		 integer *);
void DGNLpiluc(integer *,doubleprecision *,integer *,integer *,integer *,doubleprecision *,
	       doubleprecision *,integer *,integer *,integer *,integer *,
	       doubleprecision *,integer *,integer *,integer *,doubleprecision *,integer *,
	       integer *, doubleprecision *, integer*);
void DGNLspiluc(integer *,doubleprecision *,integer *,integer *,integer *,doubleprecision *,
		doubleprecision *,integer *,integer *,integer *,integer *,
		doubleprecision *,integer *,integer *,integer *,doubleprecision *,integer *,
		integer *, doubleprecision *, integer*);
void DGNLmpiluc(integer *,doubleprecision *,integer *,integer *,integer *,doubleprecision *,
		doubleprecision *,doubleprecision *,integer *,integer *,integer *,integer *,
		doubleprecision *,integer *,integer *,integer *,doubleprecision *,integer *,
		integer *, doubleprecision *, integer *);
void DSPDpiluc(integer *,doubleprecision *,integer *,integer *,integer *,doubleprecision *,
	       doubleprecision *,integer *,integer *,integer *,integer *,
	       doubleprecision *,integer *,integer *,doubleprecision *,integer *,
	       integer *, doubleprecision *, integer *);
void DSPDmpiluc(integer *,doubleprecision *,integer *,integer *,integer *,doubleprecision *,
		doubleprecision *,doubleprecision *,integer *,integer *,integer *,integer *,
		doubleprecision *,integer *,integer *,doubleprecision *,integer *,
		integer *, doubleprecision *, integer *);
void DSYMpiluc(integer *,doubleprecision *,integer *,integer *,integer *,doubleprecision *,
	       doubleprecision *,integer *,integer *,integer *,integer *,
	       doubleprecision *,integer *,integer *,doubleprecision *,integer *,
	       integer *, doubleprecision *, integer *);
void DSYMmpiluc(integer *,doubleprecision *,integer *,integer *,integer *,doubleprecision *,
		doubleprecision *,doubleprecision *,integer *,integer *,integer *,integer *,
		doubleprecision *,integer *,integer *,doubleprecision *,integer *,
		integer *, doubleprecision *, integer *);

void DSYMiluc(integer *,doubleprecision *,integer *,integer *,integer *,doubleprecision *,
	       integer *,integer *,integer *,
	       doubleprecision *,integer *,integer *,doubleprecision *,integer *,
	       integer *);
void SGNLiluc(integer *,real *,integer *,integer *,integer *,
	      real *,integer *,real *,integer *,integer *,
	      integer *,real *,integer *,integer *);
void SGNLilucsol (integer *,real *,real *,real *,integer *,
		  integer *);
void SGNLiluctsol(integer *,real *,real *,real *,integer *,
		  integer *);
void SGNLilucdlsol (integer *,real *,real *,real *,integer *,
		  integer *);
void SGNLiluctdlsol(integer *,real *,real *,real *,integer *,
		  integer *);
void SGNLilucdusol (integer *,real *,real *,real *,integer *,
		  integer *);
void SGNLiluctdusol(integer *,real *,real *,real *,integer *,
		  integer *);
void SGNLiluclsol (integer *,real *,real *,real *,integer *,
		  integer *);
void SGNLiluctlsol(integer *,real *,real *,real *,integer *,
		  integer *);
void SGNLilucusol (integer *,real *,real *,real *,integer *,
		  integer *);
void SGNLiluctusol(integer *,real *,real *,real *,integer *,
		  integer *);

void SGNLpiluclsol  (integer *,integer *, real *,real *,
		     real *,integer *,integer *);
void SGNLpilucdlsol (integer *,integer *, real *,real *,
		     real *,integer *,integer *);
void SGNLpilucusol  (integer *,integer *, real *,real *,
		     real *,integer *,integer *);
void SGNLpilucdusol (integer *,integer *, real *,real *,
		     real *,integer *,integer *);
void SGNLpiluctlsol (integer *,integer *, real *,real *,
		     real *,integer *,integer *);
void SGNLpiluctdlsol(integer *,integer *, real *,real *,
		     real *,integer *,integer *);
void SGNLpiluctusol (integer *,integer *, real *,real *,
		     real *,integer *,integer *);
void SGNLpiluctdusol(integer *,integer *, real *,real *,
		     real *,integer *,integer *);

void SSYMildlc(integer *,real *,integer *,integer *,integer *,
	       real *,integer *,real *,integer *,integer *,
	       real *,integer *,integer *);
void SSYMildlcsol(integer *,real *,real *,real *,
		 integer *);
void SSYMpilucsol(integer *,real *,real *,real *,
		 integer *);
void SSSMildlc(integer *,real *,integer *,integer *,integer *,
	       real *,integer *,real *,integer *,integer *,
	       real *,integer *,integer *);
void SSSMildlcsol(integer *,real *,real *,real *,
		 integer *);
void SGNLpiluc(integer *,real *,integer *,integer *,integer *,real *,
	       real *,integer *,integer *,integer *,integer *,
	       real *,integer *,integer *,integer *,real *,integer *,
	       integer *, real *, integer *);
void SGNLspiluc(integer *,real *,integer *,integer *,integer *,real *,
		real *,integer *,integer *,integer *,integer *,
		real *,integer *,integer *,integer *,real *,integer *,
		integer *, real *, integer *);
void SGNLmpiluc(integer *,real *,integer *,integer *,integer *,real *,
		real *,real *,integer *,integer *,integer *,integer *,
		real *,integer *,integer *,integer *,real *,integer *,
		integer *, real *, integer *);
void SSPDpiluc(integer *,real *,integer *,integer *,integer *,real *,
	       real *,integer *,integer *,integer *,integer *,
	       real *,integer *,integer *,real *,integer *,
	       integer *, real *, integer *);
void SSPDmpiluc(integer *,real *,integer *,integer *,integer *,real *,
		real *,real *,integer *,integer *,integer *,integer *,
		real *,integer *,integer *,real *,integer *,
		integer *, real *, integer *);
void SSYMpiluc(integer *,real *,integer *,integer *,integer *,real *,
	       real *,integer *,integer *,integer *,integer *,
	       real *,integer *,integer *,real *,integer *,
	       integer *, real *, integer *);
void SSYMmpiluc(integer *,real *,integer *,integer *,integer *,real *,
		real *,real *,integer *,integer *,integer *,integer *,
		real *,integer *,integer *,real *,integer *,
		integer *, real *, integer *);
void SSYMiluc(integer *,real *,integer *,integer *,integer *,real *,
	       integer *,integer *,integer *,
	       real *,integer *,integer *,real *,integer *,
	       integer *);

void ZGNLiluc(integer *,doublecomplex *,integer *,integer *,integer *,
	      doubleprecision *,integer *,doublecomplex *,integer *,integer *,
	      integer *,doublecomplex *,integer *,integer *);
void ZGNLilucsol (integer *,doublecomplex *,doublecomplex *,doublecomplex *,integer *,
		  integer *);
void ZGNLiluctsol(integer *,doublecomplex *,doublecomplex *,doublecomplex *,integer *,
		  integer *);
void ZGNLilucdlsol (integer *,doublecomplex *,doublecomplex *,doublecomplex *,integer *,
		  integer *);
void ZGNLiluctdlsol(integer *,doublecomplex *,doublecomplex *,doublecomplex *,integer *,
		  integer *);
void ZGNLilucdusol (integer *,doublecomplex *,doublecomplex *,doublecomplex *,integer *,
		  integer *);
void ZGNLiluctdusol(integer *,doublecomplex *,doublecomplex *,doublecomplex *,integer *,
		  integer *);
void ZGNLiluclsol (integer *,doublecomplex *,doublecomplex *,doublecomplex *,integer *,
		  integer *);
void ZGNLiluctlsol(integer *,doublecomplex *,doublecomplex *,doublecomplex *,integer *,
		  integer *);
void ZGNLilucusol (integer *,doublecomplex *,doublecomplex *,doublecomplex *,integer *,
		  integer *);
void ZGNLiluctusol(integer *,doublecomplex *,doublecomplex *,doublecomplex *,integer *,
		  integer *);

void ZGNLpiluclsol  (integer *,integer *, doublecomplex *,doublecomplex *,
		     doublecomplex *,integer *,integer *);
void ZGNLpilucdlsol (integer *,integer *, doublecomplex *,doublecomplex *,
		     doublecomplex *,integer *,integer *);
void ZGNLpilucusol  (integer *,integer *, doublecomplex *,doublecomplex *,
		     doublecomplex *,integer *,integer *);
void ZGNLpilucdusol (integer *,integer *, doublecomplex *,doublecomplex *,
		     doublecomplex *,integer *,integer *);
void ZGNLpiluctlsol (integer *,integer *, doublecomplex *,doublecomplex *,
		     doublecomplex *,integer *,integer *);
void ZGNLpiluctdlsol(integer *,integer *, doublecomplex *,doublecomplex *,
		     doublecomplex *,integer *,integer *);
void ZGNLpiluctusol (integer *,integer *, doublecomplex *,doublecomplex *,
		     doublecomplex *,integer *,integer *);
void ZGNLpiluctdusol(integer *,integer *, doublecomplex *,doublecomplex *,
		     doublecomplex *,integer *,integer *);

void ZHERildlc(integer *,doublecomplex *,integer *,integer *,integer *,
	       doubleprecision *,integer *,doublecomplex *,integer *,integer *,
	       doublecomplex *,integer *,integer *);
void ZHERildlcsol(integer *,doublecomplex *,doublecomplex *,doublecomplex *,
		 integer *);
void ZHERpilucsol(integer *,doublecomplex *,doublecomplex *,doublecomplex *,
		 integer *);
void ZSYMpilucsol(integer *,doublecomplex *,doublecomplex *,doublecomplex *,
		 integer *);
void ZSYMildlc(integer *,doublecomplex *,integer *,integer *,integer *,
	       doubleprecision *,integer *,doublecomplex *,integer *,integer *,
	       doublecomplex *,integer *,integer *);
void ZSYMildlcsol(integer *,doublecomplex *,doublecomplex *,doublecomplex *,
		 integer *);
void ZSHRildlc(integer *,doublecomplex *,integer *,integer *,integer *,
	       doubleprecision *,integer *,doublecomplex *,integer *,integer *,
	       doublecomplex *,integer *,integer *);
void ZSHRildlcsol(integer *,doublecomplex *,doublecomplex *,doublecomplex *,
		 integer *);
void ZSSMildlc(integer *,doublecomplex *,integer *,integer *,integer *,
	       doubleprecision *,integer *,doublecomplex *,integer *,integer *,
	       doublecomplex *,integer *,integer *);
void ZSSMildlcsol(integer *,doublecomplex *,doublecomplex *,doublecomplex *,
		 integer *);
void ZGNLpiluc(integer *,doublecomplex *,integer *,integer *,integer *,doubleprecision *,
	       doubleprecision *,integer *,integer *,integer *,integer *,
	       doublecomplex *,integer *,integer *,integer *,doublecomplex *,integer *,
	       integer *, doubleprecision *, integer *);
void ZGNLspiluc(integer *,doublecomplex *,integer *,integer *,integer *,doubleprecision *,
		doubleprecision *,integer *,integer *,integer *,integer *,
		doublecomplex *,integer *,integer *,integer *,doublecomplex *,integer *,
		integer *, doubleprecision *, integer *);
void ZGNLmpiluc(integer *,doublecomplex *,integer *,integer *,integer *,doubleprecision *,
		doubleprecision *,doubleprecision *,integer *,integer *,integer *,integer *,
		doublecomplex *,integer *,integer *,integer *,doublecomplex *,integer *,
		integer *, doubleprecision *, integer *);
void ZHPDpiluc(integer *,doublecomplex *,integer *,integer *,integer *,doubleprecision *,
	       doubleprecision *,integer *,integer *,integer *,integer *,
	       doublecomplex *,integer *,integer *,doublecomplex *,integer *,
	       integer *, doubleprecision *, integer *);
void ZHPDmpiluc(integer *,doublecomplex *,integer *,integer *,integer *,doubleprecision *,
		doubleprecision *,doubleprecision *,integer *,integer *,integer *,integer *,
		doublecomplex *,integer *,integer *,doublecomplex *,integer *,
		integer *, doubleprecision *, integer *);
void ZHERpiluc(integer *,doublecomplex *,integer *,integer *,integer *,doubleprecision *,
	       doubleprecision *,integer *,integer *,integer *,integer *,
	       doublecomplex *,integer *,integer *,doublecomplex *,integer *,
	       integer *, doubleprecision *, integer *);
void ZHERmpiluc(integer *,doublecomplex *,integer *,integer *,integer *,doubleprecision *,
		doubleprecision *,doubleprecision *,integer *,integer *,integer *,integer *,
		doublecomplex *,integer *,integer *,doublecomplex *,integer *,
		integer *, doubleprecision *, integer *);
void ZHERiluc(integer *,doublecomplex *,integer *,integer *,integer *,doubleprecision *,
	       integer *,integer *,integer *,
	       doublecomplex *,integer *,integer *,doublecomplex *,integer *,
	       integer *);

void ZSYMpiluc(integer *,doublecomplex *,integer *,integer *,integer *,doubleprecision *,
	       doubleprecision *,integer *,integer *,integer *,integer *,
	       doublecomplex *,integer *,integer *,doublecomplex *,integer *,
	       integer *, doubleprecision *, integer *);
void ZSYMmpiluc(integer *,doublecomplex *,integer *,integer *,integer *,doubleprecision *,
		doubleprecision *,doubleprecision *,integer *,integer *,integer *,integer *,
		doublecomplex *,integer *,integer *,doublecomplex *,integer *,
		integer *, doubleprecision *, integer *);

void ZSYMiluc(integer *,doublecomplex *,integer *,integer *,integer *,doubleprecision *,
	       integer *,integer *,integer *,
	       doublecomplex *,integer *,integer *,doublecomplex *,integer *,
	       integer *);

void CGNLiluc(integer *,complex *,integer *,integer *,integer *,
	      real *,integer *,complex *,integer *,integer *,
	      integer *,complex *,integer *,integer *);
void CGNLilucsol (integer *,complex *,complex *,complex *,integer *,
		  integer *);
void CGNLiluctsol(integer *,complex *,complex *,complex *,integer *,
		  integer *);
void CGNLilucdlsol (integer *,complex *,complex *,complex *,integer *,
		  integer *);
void CGNLiluctdlsol(integer *,complex *,complex *,complex *,integer *,
		  integer *);
void CGNLilucdusol (integer *,complex *,complex *,complex *,integer *,
		  integer *);
void CGNLiluctdusol(integer *,complex *,complex *,complex *,integer *,
		  integer *);
void CGNLilucusol (integer *,complex *,complex *,complex *,integer *,
		  integer *);
void CGNLiluctusol(integer *,complex *,complex *,complex *,integer *,
		  integer *);
void CGNLiluclsol (integer *,complex *,complex *,complex *,integer *,
		  integer *);
void CGNLiluctlsol(integer *,complex *,complex *,complex *,integer *,
		  integer *);

void CGNLpiluclsol  (integer *,integer *, complex *,complex *,
		     complex *,integer *,integer *);
void CGNLpilucdlsol (integer *,integer *, complex *,complex *,
		     complex *,integer *,integer *);
void CGNLpilucusol  (integer *,integer *, complex *,complex *,
		     complex *,integer *,integer *);
void CGNLpilucdusol (integer *,integer *, complex *,complex *,
		     complex *,integer *,integer *);
void CGNLpiluctlsol (integer *,integer *, complex *,complex *,
		     complex *,integer *,integer *);
void CGNLpiluctdlsol(integer *,integer *, complex *,complex *,
		     complex *,integer *,integer *);
void CGNLpiluctusol (integer *,integer *, complex *,complex *,
		     complex *,integer *,integer *);
void CGNLpiluctdusol(integer *,integer *, complex *,complex *,
		     complex *,integer *,integer *);

void CHERildlc(integer *,complex *,integer *,integer *,integer *,
	       real *,integer *,complex *,integer *,integer *,
	       complex *,integer *,integer *);
void CHERildlcsol(integer *,complex *,complex *,complex *,
		  integer *);
void CHERpilucsol(integer *,complex *,complex *,complex *,
		  integer *);
void CSYMpilucsol(integer *,complex *,complex *,complex *,
		  integer *);
void CSYMpiluc(integer *,complex *,integer *,integer *,integer *,real *,
               real *,integer *,integer *,integer *,integer *,
               complex *,integer *,integer *,complex *,integer *,
               integer *, real *, integer *);
void CHERpiluc(integer *,complex *,integer *,integer *,integer *,real *,
               real *,integer *,integer *,integer *,integer *,
               complex *,integer *,integer *,complex *,integer *,
               integer *, real *, integer *);
void CSYMiluc(integer *,complex *,integer *,integer *,integer *,real *,
	       integer *,integer *,integer *,
	       complex *,integer *,integer *,complex *,integer *,
	       integer *);
void CHERiluc(integer *,complex *,integer *,integer *,integer *,real *,
	       integer *,integer *,integer *,
	       complex *,integer *,integer *,complex *,integer *,
	       integer *);

void CSYMildlc(integer *,complex *,integer *,integer *,integer *,
	       real *,integer *,complex *,integer *,integer *,
	       complex *,integer *,integer *);
void CSYMildlcsol(integer *,complex *,complex *,complex *,
		 integer *);
void CSHRildlc(integer *,complex *,integer *,integer *,integer *,
	       real *,integer *,complex *,integer *,integer *,
	       complex *,integer *,integer *);
void CSHRildlcsol(integer *,complex *,complex *,complex *,
		 integer *);
void CSSMildlc(integer *,complex *,integer *,integer *,integer *,
	       real *,integer *,complex *,integer *,integer *,
	       complex *,integer *,integer *);
void CSSMildlcsol(integer *,complex *,complex *,complex *,
		 integer *);
void CGNLpiluc(integer *,complex *,integer *,integer *,integer *,real *,
	       real *,integer *,integer *,integer *,integer *,
	       complex *,integer *,integer *,integer *,complex *,integer *,
	       integer *, real *, integer *);
void CGNLspiluc(integer *,complex *,integer *,integer *,integer *,real *,
		real *,integer *,integer *,integer *,integer *,
		complex *,integer *,integer *,integer *,complex *,integer *,
		integer *, real *, integer *);
void CGNLmpiluc(integer *,complex *,integer *,integer *,integer *,real *,
		real *,real *,integer *,integer *,integer *,integer *,
		complex *,integer *,integer *,integer *,complex *,integer *,
		integer *, real *, integer *);
void CHPDpiluc(integer *,complex *,integer *,integer *,integer *,real *,
	       real *,integer *,integer *,integer *,integer *,
	       complex *,integer *,integer *,complex *,integer *,
	       integer *, real *, integer *);
void CHPDmpiluc(integer *,complex *,integer *,integer *,integer *,real *,
		real *,real *,integer *,integer *,integer *,integer *,
		complex *,integer *,integer *,complex *,integer *,
		integer *, real *, integer *);


/* *********************************************** */
/* ******      Definitions for orderings     ***** */

integer    DGNLperm_null        (Dmat, doubleprecision *,doubleprecision *, 
			     integer *,integer *, integer *, DILUPACKparam *);
integer    DGNLperm_nd          (Dmat, doubleprecision *,doubleprecision *, 
			     integer *,integer *, integer *, DILUPACKparam *);
integer    DGNLperm_rcm         (Dmat, doubleprecision *,doubleprecision *,
			     integer *,integer *, integer *, DILUPACKparam *);
integer    DGNLperm_mmd         (Dmat, doubleprecision *,doubleprecision *, 
			     integer *,integer *, integer *, DILUPACKparam *);
integer    DGNLperm_amf         (Dmat, doubleprecision *,doubleprecision *, 
			     integer *,integer *, integer *, DILUPACKparam *);
integer    DGNLperm_amd         (Dmat, doubleprecision *,doubleprecision *, 
			     integer *,integer *, integer *, DILUPACKparam *);
integer    DGNLperm_metis_e     (Dmat, doubleprecision *,doubleprecision *, 
			     integer *,integer *, integer *, DILUPACKparam *);
integer    DGNLperm_metis_n     (Dmat, doubleprecision *,doubleprecision *, 
			     integer *,integer *, integer *, DILUPACKparam *);
integer    DGNLperm_pq          (Dmat, doubleprecision *,doubleprecision *, 
			     integer *,integer *, integer *, DILUPACKparam *);
integer    DGNLperm_fc          (Dmat, doubleprecision *,doubleprecision *, 
			     integer *,integer *, integer *, DILUPACKparam *);
integer    DSYMperm_fc          (Dmat, doubleprecision *,doubleprecision *, 
			     integer *,integer *, integer *, DILUPACKparam *);
integer    DGNLperm_p           (Dmat, doubleprecision *,doubleprecision *, 
			     integer *,integer *, integer *, DILUPACKparam *);
integer    DGNLperm_indset      (Dmat, doubleprecision *,doubleprecision *, 
			     integer *,integer *, integer *, DILUPACKparam *);


integer    DGNLperm_mwm_rcm     (Dmat, doubleprecision *,doubleprecision *, 
			     integer *,integer *, integer *, DILUPACKparam *);
integer    DGNLperm_mwm_mmd     (Dmat, doubleprecision *,doubleprecision *, 
			     integer *,integer *, integer *, DILUPACKparam *);
integer    DGNLperm_mwm_amf     (Dmat, doubleprecision *,doubleprecision *, 
			     integer *,integer *, integer *, DILUPACKparam *);
integer    DGNLperm_mwm_amd     (Dmat, doubleprecision *,doubleprecision *, 
			     integer *,integer *, integer *, DILUPACKparam *);
integer    DGNLperm_mwm_metis_e (Dmat, doubleprecision *,doubleprecision *, 
			     integer *,integer *, integer *, DILUPACKparam *);
integer    DGNLperm_mwm_metis_n (Dmat, doubleprecision *,doubleprecision *, 
			     integer *,integer *, integer *, DILUPACKparam *);

integer    DSYMperm_mwm_rcm     (Dmat, doubleprecision *,doubleprecision *, 
			     integer *,integer *, integer *, DILUPACKparam *);
integer    DSYMperm_mwm_mmd     (Dmat, doubleprecision *,doubleprecision *, 
			     integer *,integer *, integer *, DILUPACKparam *);
integer    DSYMperm_mwm_amf     (Dmat, doubleprecision *,doubleprecision *, 
			     integer *,integer *, integer *, DILUPACKparam *);
integer    DSYMperm_mwm_amd     (Dmat, doubleprecision *,doubleprecision *, 
			     integer *,integer *, integer *, DILUPACKparam *);
integer    DSYMperm_mwm_metis_e (Dmat, doubleprecision *,doubleprecision *, 
			     integer *,integer *, integer *, DILUPACKparam *);
integer    DSYMperm_mwm_metis_n (Dmat, doubleprecision *,doubleprecision *, 
			     integer *,integer *, integer *, DILUPACKparam *);


integer    DSYMperm_mc64_rcm     (Dmat, doubleprecision *,doubleprecision *, 
			     integer *,integer *, integer *, DILUPACKparam *);
integer    DSYMperm_mc64_mmd     (Dmat, doubleprecision *,doubleprecision *, 
			     integer *,integer *, integer *, DILUPACKparam *);
integer    DSYMperm_mc64_amf     (Dmat, doubleprecision *,doubleprecision *, 
			     integer *,integer *, integer *, DILUPACKparam *);
integer    DSYMperm_mc64_amd     (Dmat, doubleprecision *,doubleprecision *, 
			     integer *,integer *, integer *, DILUPACKparam *);
integer    DSYMperm_mc64_metis_e (Dmat, doubleprecision *,doubleprecision *, 
			     integer *,integer *, integer *, DILUPACKparam *);
integer    DSYMperm_mc64_metis_n (Dmat, doubleprecision *,doubleprecision *, 
			     integer *,integer *, integer *, DILUPACKparam *);



integer    DGNLperm_mc64_rcm    (Dmat, doubleprecision *,doubleprecision *, 
			     integer *,integer *, integer *, DILUPACKparam *);
integer    DGNLperm_mc64_mmd    (Dmat, doubleprecision *,doubleprecision *, 
			     integer *,integer *, integer *, DILUPACKparam *);
integer    DGNLperm_mc64_amf    (Dmat, doubleprecision *,doubleprecision *, 
			     integer *,integer *, integer *, DILUPACKparam *);
integer    DGNLperm_mc64_amd    (Dmat, doubleprecision *,doubleprecision *, 
			     integer *,integer *, integer *, DILUPACKparam *);
integer    DGNLperm_mc64_metis_e(Dmat, doubleprecision *,doubleprecision *, 
			     integer *,integer *, integer *, DILUPACKparam *);
integer    DGNLperm_mc64_metis_n(Dmat, doubleprecision *,doubleprecision *, 
			     integer *,integer *, integer *, DILUPACKparam *);


integer    SGNLperm_null        (Smat, real *,real *, integer *,integer *,
			     integer *, SILUPACKparam *);
integer    SGNLperm_nd          (Smat, real *,real *, integer *,integer *,
			     integer *, SILUPACKparam *);
integer    SGNLperm_rcm         (Smat, real *,real *, integer *,integer *,
			     integer *, SILUPACKparam *);
integer    SGNLperm_mmd         (Smat, real *,real *, integer *,integer *,
			     integer *, SILUPACKparam *);
integer    SGNLperm_amf         (Smat, real *,real *, integer *,integer *,
			     integer *, SILUPACKparam *);
integer    SGNLperm_amd         (Smat, real *,real *, integer *,integer *,
			     integer *, SILUPACKparam *);
integer    SGNLperm_metis_e     (Smat, real *,real *, integer *,integer *,
			     integer *, SILUPACKparam *);
integer    SGNLperm_metis_n     (Smat, real *,real *, integer *,integer *,
			     integer *, SILUPACKparam *);
integer    SGNLperm_pq          (Smat, real *,real *, integer *,integer *,
			     integer *, SILUPACKparam *);
integer    SGNLperm_fc          (Smat, real *,real *, integer *,integer *,
			     integer *, SILUPACKparam *);
integer    SSYMperm_fc          (Smat, real *,real *, integer *,integer *,
			     integer *, SILUPACKparam *);
integer    SGNLperm_indset      (Smat, real *,real *, integer *,integer *,
			     integer *, SILUPACKparam *);
integer    SGNLperm_p           (Smat, real *,real *, integer *,integer *,
			     integer *, SILUPACKparam *);

integer    SGNLperm_mwm_rcm     (Smat, real *,real *, integer *,integer *,
			     integer *, SILUPACKparam *);
integer    SGNLperm_mwm_mmd     (Smat, real *,real *, integer *,integer *,
			     integer *, SILUPACKparam *);
integer    SGNLperm_mwm_amf     (Smat, real *,real *, integer *,integer *,
			     integer *, SILUPACKparam *);
integer    SGNLperm_mwm_amd     (Smat, real *,real *, integer *,integer *,
			     integer *, SILUPACKparam *);
integer    SGNLperm_mwm_metis_e (Smat, real *,real *, integer *,integer *,
			     integer *, SILUPACKparam *);
integer    SGNLperm_mwm_metis_n (Smat, real *,real *, integer *,integer *,
			     integer *, SILUPACKparam *);

integer    SSYMperm_mwm_rcm     (Smat, real *,real *, integer *,integer *,
			     integer *, SILUPACKparam *);
integer    SSYMperm_mwm_mmd     (Smat, real *,real *, integer *,integer *,
			     integer *, SILUPACKparam *);
integer    SSYMperm_mwm_amf     (Smat, real *,real *, integer *,integer *,
			     integer *, SILUPACKparam *);
integer    SSYMperm_mwm_amd     (Smat, real *,real *, integer *,integer *,
			     integer *, SILUPACKparam *);
integer    SSYMperm_mwm_metis_e (Smat, real *,real *, integer *,integer *,
			     integer *, SILUPACKparam *);
integer    SSYMperm_mwm_metis_n (Smat, real *,real *, integer *,integer *,
			     integer *, SILUPACKparam *);


integer    SSYMperm_mc64_rcm     (Smat, real *,real *, integer *,integer *,
			     integer *, SILUPACKparam *);
integer    SSYMperm_mc64_mmd     (Smat, real *,real *, integer *,integer *,
			     integer *, SILUPACKparam *);
integer    SSYMperm_mc64_amf     (Smat, real *,real *, integer *,integer *,
			     integer *, SILUPACKparam *);
integer    SSYMperm_mc64_amd     (Smat, real *,real *, integer *,integer *,
			     integer *, SILUPACKparam *);
integer    SSYMperm_mc64_metis_e (Smat, real *,real *, integer *,integer *,
			     integer *, SILUPACKparam *);
integer    SSYMperm_mc64_metis_n (Smat, real *,real *, integer *,integer *,
			     integer *, SILUPACKparam *);



integer    SGNLperm_mc64_rcm    (Smat, real *,real *, integer *,integer *,
			     integer *, SILUPACKparam *);
integer    SGNLperm_mc64_mmd    (Smat, real *,real *, integer *,integer *,
			     integer *, SILUPACKparam *);
integer    SGNLperm_mc64_amf    (Smat, real *,real *, integer *,integer *,
			     integer *, SILUPACKparam *);
integer    SGNLperm_mc64_amd    (Smat, real *,real *, integer *,integer *,
			     integer *, SILUPACKparam *);
integer    SGNLperm_mc64_metis_e(Smat, real *,real *, integer *,integer *,
			     integer *, SILUPACKparam *);
integer    SGNLperm_mc64_metis_n(Smat, real *,real *, integer *,integer *,
			     integer *, SILUPACKparam *);


integer    ZGNLperm_null        (Zmat, doublecomplex *,doublecomplex *,
			     integer *,integer *, integer *, ZILUPACKparam *);
integer    ZGNLperm_nd          (Zmat, doublecomplex *,doublecomplex *, 
			     integer *,integer *, integer *, ZILUPACKparam *);
integer    ZGNLperm_rcm         (Zmat, doublecomplex *,doublecomplex *,
			     integer *,integer *, integer *, ZILUPACKparam *);
integer    ZGNLperm_mmd         (Zmat, doublecomplex *,doublecomplex *, 
			     integer *,integer *, integer *, ZILUPACKparam *);
integer    ZGNLperm_amf         (Zmat, doublecomplex *,doublecomplex *, 
			     integer *,integer *, integer *, ZILUPACKparam *);
integer    ZGNLperm_amd         (Zmat, doublecomplex *,doublecomplex *, 
			     integer *,integer *, integer *, ZILUPACKparam *);
integer    ZGNLperm_metis_e     (Zmat, doublecomplex *,doublecomplex *, 
			     integer *,integer *, integer *, ZILUPACKparam *);
integer    ZGNLperm_metis_n     (Zmat, doublecomplex *,doublecomplex *, 
			     integer *,integer *, integer *, ZILUPACKparam *);
integer    ZGNLperm_pq          (Zmat, doublecomplex *,doublecomplex *, 
			     integer *,integer *, integer *, ZILUPACKparam *);
integer    ZGNLperm_fc          (Zmat, doublecomplex *,doublecomplex *, 
			     integer *,integer *, integer *, ZILUPACKparam *);
integer    ZSYMperm_fc          (Zmat, doublecomplex *,doublecomplex *, 
			     integer *,integer *, integer *, ZILUPACKparam *);
integer    ZGNLperm_indset      (Zmat, doublecomplex *,doublecomplex *,
			     integer *,integer *, integer *, ZILUPACKparam *);
integer    ZGNLperm_p           (Zmat, doublecomplex *,doublecomplex *, 
			     integer *,integer *, integer *, ZILUPACKparam *);

integer    ZGNLperm_mwm_rcm     (Zmat, doublecomplex *,doublecomplex *, 
			     integer *,integer *, integer *, ZILUPACKparam *);
integer    ZGNLperm_mwm_mmd     (Zmat, doublecomplex *,doublecomplex *, 
			     integer *,integer *, integer *, ZILUPACKparam *);
integer    ZGNLperm_mwm_amf     (Zmat, doublecomplex *,doublecomplex *,
			     integer *,integer *, integer *, ZILUPACKparam *);
integer    ZGNLperm_mwm_amd     (Zmat, doublecomplex *,doublecomplex *,
			     integer *,integer *, integer *, ZILUPACKparam *);
integer    ZGNLperm_mwm_metis_e (Zmat, doublecomplex *,doublecomplex *,
			     integer *,integer *, integer *, ZILUPACKparam *);
integer    ZGNLperm_mwm_metis_n (Zmat, doublecomplex *,doublecomplex *,
			     integer *,integer *, integer *, ZILUPACKparam *);

integer    ZHERperm_mwm_rcm     (Zmat, doublecomplex *,doublecomplex *, 
			     integer *,integer *, integer *, ZILUPACKparam *);
integer    ZHERperm_mwm_mmd     (Zmat, doublecomplex *,doublecomplex *, 
			     integer *,integer *, integer *, ZILUPACKparam *);
integer    ZHERperm_mwm_amf     (Zmat, doublecomplex *,doublecomplex *,
			     integer *,integer *, integer *, ZILUPACKparam *);
integer    ZHERperm_mwm_amd     (Zmat, doublecomplex *,doublecomplex *,
			     integer *,integer *, integer *, ZILUPACKparam *);
integer    ZHERperm_mwm_metis_e (Zmat, doublecomplex *,doublecomplex *,
			     integer *,integer *, integer *, ZILUPACKparam *);
integer    ZHERperm_mwm_metis_n (Zmat, doublecomplex *,doublecomplex *,
			     integer *,integer *, integer *, ZILUPACKparam *);

integer    ZSYMperm_mwm_rcm     (Zmat, doublecomplex *,doublecomplex *, 
			     integer *,integer *, integer *, ZILUPACKparam *);
integer    ZSYMperm_mwm_mmd     (Zmat, doublecomplex *,doublecomplex *, 
			     integer *,integer *, integer *, ZILUPACKparam *);
integer    ZSYMperm_mwm_amf     (Zmat, doublecomplex *,doublecomplex *,
			     integer *,integer *, integer *, ZILUPACKparam *);
integer    ZSYMperm_mwm_amd     (Zmat, doublecomplex *,doublecomplex *,
			     integer *,integer *, integer *, ZILUPACKparam *);
integer    ZSYMperm_mwm_metis_e (Zmat, doublecomplex *,doublecomplex *,
			     integer *,integer *, integer *, ZILUPACKparam *);
integer    ZSYMperm_mwm_metis_n (Zmat, doublecomplex *,doublecomplex *,
			     integer *,integer *, integer *, ZILUPACKparam *);

integer    ZHERperm_mc64_rcm     (Zmat, doublecomplex *,doublecomplex *, 
			     integer *,integer *, integer *, ZILUPACKparam *);
integer    ZHERperm_mc64_mmd     (Zmat, doublecomplex *,doublecomplex *, 
			     integer *,integer *, integer *, ZILUPACKparam *);
integer    ZHERperm_mc64_amf     (Zmat, doublecomplex *,doublecomplex *,
			     integer *,integer *, integer *, ZILUPACKparam *);
integer    ZHERperm_mc64_amd     (Zmat, doublecomplex *,doublecomplex *,
			     integer *,integer *, integer *, ZILUPACKparam *);
integer    ZHERperm_mc64_metis_e (Zmat, doublecomplex *,doublecomplex *,
			     integer *,integer *, integer *, ZILUPACKparam *);
integer    ZHERperm_mc64_metis_n (Zmat, doublecomplex *,doublecomplex *,
			     integer *,integer *, integer *, ZILUPACKparam *);

integer    ZSYMperm_mc64_rcm     (Zmat, doublecomplex *,doublecomplex *, 
			     integer *,integer *, integer *, ZILUPACKparam *);
integer    ZSYMperm_mc64_mmd     (Zmat, doublecomplex *,doublecomplex *, 
			     integer *,integer *, integer *, ZILUPACKparam *);
integer    ZSYMperm_mc64_amf     (Zmat, doublecomplex *,doublecomplex *,
			     integer *,integer *, integer *, ZILUPACKparam *);
integer    ZSYMperm_mc64_amd     (Zmat, doublecomplex *,doublecomplex *,
			     integer *,integer *, integer *, ZILUPACKparam *);
integer    ZSYMperm_mc64_metis_e (Zmat, doublecomplex *,doublecomplex *,
			     integer *,integer *, integer *, ZILUPACKparam *);
integer    ZSYMperm_mc64_metis_n (Zmat, doublecomplex *,doublecomplex *,
			     integer *,integer *, integer *, ZILUPACKparam *);


integer    ZGNLperm_mc64_rcm    (Zmat, doublecomplex *,doublecomplex *,
			     integer *,integer *, integer *, ZILUPACKparam *);
integer    ZGNLperm_mc64_mmd    (Zmat, doublecomplex *,doublecomplex *,
			     integer *,integer *, integer *, ZILUPACKparam *);
integer    ZGNLperm_mc64_amf    (Zmat, doublecomplex *,doublecomplex *,
			     integer *,integer *, integer *, ZILUPACKparam *);
integer    ZGNLperm_mc64_amd    (Zmat, doublecomplex *,doublecomplex *,
			     integer *,integer *, integer *, ZILUPACKparam *);
integer    ZGNLperm_mc64_metis_e(Zmat, doublecomplex *,doublecomplex *,
			     integer *,integer *, integer *, ZILUPACKparam *);
integer    ZGNLperm_mc64_metis_n(Zmat, doublecomplex *,doublecomplex *,
			     integer *,integer *, integer *, ZILUPACKparam *);


integer    CGNLperm_null        (Cmat, complex *,complex *, integer *,integer *,
			     integer *, CILUPACKparam *);
integer    CGNLperm_nd          (Cmat, complex *,complex *, integer *,integer *,
			     integer *, CILUPACKparam *);
integer    CGNLperm_rcm         (Cmat, complex *,complex *, integer *,integer *, 
			     integer *, CILUPACKparam *);
integer    CGNLperm_mmd         (Cmat, complex *,complex *, integer *,integer *,
			     integer *, CILUPACKparam *);
integer    CGNLperm_amf         (Cmat, complex *,complex *, integer *,integer *,
			     integer *, CILUPACKparam *);
integer    CGNLperm_amd         (Cmat, complex *,complex *, integer *,integer *,
			     integer *, CILUPACKparam *);
integer    CGNLperm_metis_e     (Cmat, complex *,complex *, integer *,integer *,
			     integer *, CILUPACKparam *);
integer    CGNLperm_metis_n     (Cmat, complex *,complex *, integer *,integer *,
			     integer *, CILUPACKparam *);
integer    CGNLperm_pq          (Cmat, complex *,complex *, integer *,integer *,
			     integer *, CILUPACKparam *);
integer    CGNLperm_fc          (Cmat, complex *,complex *, integer *,integer *,
			     integer *, CILUPACKparam *);
integer    CSYMperm_fc          (Cmat, complex *,complex *, integer *,integer *,
			     integer *, CILUPACKparam *);
integer    CGNLperm_indset      (Cmat, complex *,complex *, integer *,integer *,
			     integer *, CILUPACKparam *);
integer    CGNLperm_p           (Cmat, complex *,complex *, integer *,integer *,
			     integer *, CILUPACKparam *);

integer    CGNLperm_mwm_rcm     (Cmat, complex *,complex *, integer *,integer *,
			     integer *, CILUPACKparam *);
integer    CGNLperm_mwm_mmd     (Cmat, complex *,complex *, integer *,integer *,
			     integer *, CILUPACKparam *);
integer    CGNLperm_mwm_amf     (Cmat, complex *,complex *, integer *,integer *,
			     integer *, CILUPACKparam *);
integer    CGNLperm_mwm_amd     (Cmat, complex *,complex *, integer *,integer *,
			     integer *, CILUPACKparam *);
integer    CGNLperm_mwm_metis_e (Cmat, complex *,complex *, integer *,integer *,
			     integer *, CILUPACKparam *);
integer    CGNLperm_mwm_metis_n (Cmat, complex *,complex *, integer *,integer *,
			     integer *, CILUPACKparam *);

integer    CHERperm_mwm_rcm     (Cmat, complex *,complex *, integer *,integer *,
			     integer *, CILUPACKparam *);
integer    CHERperm_mwm_mmd     (Cmat, complex *,complex *, integer *,integer *,
			     integer *, CILUPACKparam *);
integer    CHERperm_mwm_amf     (Cmat, complex *,complex *, integer *,integer *,
			     integer *, CILUPACKparam *);
integer    CHERperm_mwm_amd     (Cmat, complex *,complex *, integer *,integer *,
			     integer *, CILUPACKparam *);
integer    CHERperm_mwm_metis_e (Cmat, complex *,complex *, integer *,integer *,
			     integer *, CILUPACKparam *);
integer    CHERperm_mwm_metis_n (Cmat, complex *,complex *, integer *,integer *,
			     integer *, CILUPACKparam *);

integer    CSYMperm_mwm_rcm     (Cmat, complex *,complex *, integer *,integer *,
			     integer *, CILUPACKparam *);
integer    CSYMperm_mwm_mmd     (Cmat, complex *,complex *, integer *,integer *,
			     integer *, CILUPACKparam *);
integer    CSYMperm_mwm_amf     (Cmat, complex *,complex *, integer *,integer *,
			     integer *, CILUPACKparam *);
integer    CSYMperm_mwm_amd     (Cmat, complex *,complex *, integer *,integer *,
			     integer *, CILUPACKparam *);
integer    CSYMperm_mwm_metis_e (Cmat, complex *,complex *, integer *,integer *,
			     integer *, CILUPACKparam *);
integer    CSYMperm_mwm_metis_n (Cmat, complex *,complex *, integer *,integer *,
			     integer *, CILUPACKparam *);

integer    CHERperm_mc64_rcm     (Cmat, complex *,complex *, integer *,integer *,
			     integer *, CILUPACKparam *);
integer    CHERperm_mc64_mmd     (Cmat, complex *,complex *, integer *,integer *,
			     integer *, CILUPACKparam *);
integer    CHERperm_mc64_amf     (Cmat, complex *,complex *, integer *,integer *,
			     integer *, CILUPACKparam *);
integer    CHERperm_mc64_amd     (Cmat, complex *,complex *, integer *,integer *,
			     integer *, CILUPACKparam *);
integer    CHERperm_mc64_metis_e (Cmat, complex *,complex *, integer *,integer *,
			     integer *, CILUPACKparam *);
integer    CHERperm_mc64_metis_n (Cmat, complex *,complex *, integer *,integer *,
			     integer *, CILUPACKparam *);

integer    CSYMperm_mc64_rcm     (Cmat, complex *,complex *, integer *,integer *,
			     integer *, CILUPACKparam *);
integer    CSYMperm_mc64_mmd     (Cmat, complex *,complex *, integer *,integer *,
			     integer *, CILUPACKparam *);
integer    CSYMperm_mc64_amf     (Cmat, complex *,complex *, integer *,integer *,
			     integer *, CILUPACKparam *);
integer    CSYMperm_mc64_amd     (Cmat, complex *,complex *, integer *,integer *,
			     integer *, CILUPACKparam *);
integer    CSYMperm_mc64_metis_e (Cmat, complex *,complex *, integer *,integer *,
			     integer *, CILUPACKparam *);
integer    CSYMperm_mc64_metis_n (Cmat, complex *,complex *, integer *,integer *,
			     integer *, CILUPACKparam *);


integer    CGNLperm_mc64_rcm    (Cmat, complex *,complex *, integer *,integer *,
			     integer *, CILUPACKparam *);
integer    CGNLperm_mc64_mmd    (Cmat, complex *,complex *, integer *,integer *,
			     integer *, CILUPACKparam *);
integer    CGNLperm_mc64_amf    (Cmat, complex *,complex *, integer *,integer *,
			     integer *, CILUPACKparam *);
integer    CGNLperm_mc64_amd    (Cmat, complex *,complex *, integer *,integer *,
			     integer *, CILUPACKparam *);
integer    CGNLperm_mc64_metis_e(Cmat, complex *,complex *, integer *,integer *,
			     integer *, CILUPACKparam *);
integer    CGNLperm_mc64_metis_n(Cmat, complex *,complex *, integer *,integer *,
			     integer *, CILUPACKparam *);


#define DSPDperm_null  DGNLperm_null
#define SSPDperm_null  SGNLperm_null
#define ZHPDperm_null  ZGNLperm_null
#define CHPDperm_null  CGNLperm_null

#define DSPDpermnull  DGNLperm_null
#define SSPDpermnull  SGNLperm_null
#define ZHPDpermnull  ZGNLperm_null
#define CHPDpermnull  CGNLperm_null

#define DSYMperm_null  DGNLperm_null
#define SSYMperm_null  SGNLperm_null
#define ZSYMperm_null  ZGNLperm_null
#define CSYMperm_null  CGNLperm_null
#define ZHERperm_null  ZGNLperm_null
#define CHERperm_null  CGNLperm_null


#define DSPDperm_nd    DGNLperm_nd
#define SSPDperm_nd    SGNLperm_nd
#define ZHPDperm_nd    ZGNLperm_nd
#define CHPDperm_nd    CGNLperm_nd

#define DSPDpermnd    DGNLperm_nd
#define SSPDpermnd    SGNLperm_nd
#define ZHPDpermnd    ZGNLperm_nd
#define CHPDpermnd    CGNLperm_nd

#define DSYMperm_nd    DGNLperm_nd
#define SSYMperm_nd    SGNLperm_nd
#define ZSYMperm_nd    ZGNLperm_nd
#define CSYMperm_nd    CGNLperm_nd
#define ZHERperm_nd    ZGNLperm_nd
#define CHERperm_nd    CGNLperm_nd


#define DSPDperm_amf   DGNLperm_amf
#define SSPDperm_amf   SGNLperm_amf
#define ZHPDperm_amf   ZGNLperm_amf
#define CHPDperm_amf   CGNLperm_amf

#define DSPDpermamf   DGNLperm_amf
#define SSPDpermamf   SGNLperm_amf
#define ZHPDpermamf   ZGNLperm_amf
#define CHPDpermamf   CGNLperm_amf

#define DSYMperm_amf   DGNLperm_amf
#define SSYMperm_amf   SGNLperm_amf
#define ZSYMperm_amf   ZGNLperm_amf
#define CSYMperm_amf   CGNLperm_amf
#define ZHERperm_amf   ZGNLperm_amf
#define CHERperm_amf   CGNLperm_amf


#define DSPDperm_amd   DGNLperm_amd
#define SSPDperm_amd   SGNLperm_amd
#define ZHPDperm_amd   ZGNLperm_amd
#define CHPDperm_amd   CGNLperm_amd

#define DSPDpermamd   DGNLperm_amd
#define SSPDpermamd   SGNLperm_amd
#define ZHPDpermamd   ZGNLperm_amd
#define CHPDpermamd   CGNLperm_amd

#define DSYMperm_amd   DGNLperm_amd
#define SSYMperm_amd   SGNLperm_amd
#define ZSYMperm_amd   ZGNLperm_amd
#define CSYMperm_amd   CGNLperm_amd
#define ZHERperm_amd   ZGNLperm_amd
#define CHERperm_amd   CGNLperm_amd


#define DSPDperm_metis_e   DGNLperm_metis_e
#define SSPDperm_metis_e   SGNLperm_metis_e
#define ZHPDperm_metis_e   ZGNLperm_metis_e
#define CHPDperm_metis_e   CGNLperm_metis_e

#define DSPDpermmetis_e   DGNLperm_metis_e
#define SSPDpermmetis_e   SGNLperm_metis_e
#define ZHPDpermmetis_e   ZGNLperm_metis_e
#define CHPDpermmetis_e   CGNLperm_metis_e

#define DSYMperm_metis_e   DGNLperm_metis_e
#define SSYMperm_metis_e   SGNLperm_metis_e
#define ZSYMperm_metis_e   ZGNLperm_metis_e
#define CSYMperm_metis_e   CGNLperm_metis_e
#define ZHERperm_metis_e   ZGNLperm_metis_e
#define CHERperm_metis_e   CGNLperm_metis_e


#define DSPDperm_metis_n   DGNLperm_metis_n
#define SSPDperm_metis_n   SGNLperm_metis_n
#define ZHPDperm_metis_n   ZGNLperm_metis_n
#define CHPDperm_metis_n   CGNLperm_metis_n

#define DSPDpermmetis_n   DGNLperm_metis_n
#define SSPDpermmetis_n   SGNLperm_metis_n
#define ZHPDpermmetis_n   ZGNLperm_metis_n
#define CHPDpermmetis_n   CGNLperm_metis_n

#define DSYMperm_metis_n   DGNLperm_metis_n
#define SSYMperm_metis_n   SGNLperm_metis_n
#define ZSYMperm_metis_n   ZGNLperm_metis_n
#define CSYMperm_metis_n   CGNLperm_metis_n
#define ZHERperm_metis_n   ZGNLperm_metis_n
#define CHERperm_metis_n   CGNLperm_metis_n


#define DSPDperm_fc    DSYMperm_fc
#define SSPDperm_fc    SSYMperm_fc
#define ZHPDperm_fc    ZSYMperm_fc
#define CHPDperm_fc    CSYMperm_fc

#define DSPDpermfc    DSYMperm_fc
#define SSPDpermfc    SSYMperm_fc
#define ZHPDpermfc    ZSYMperm_fc
#define CHPDpermfc    CSYMperm_fc

#define DSSMperm_fc    DSYMperm_fc
#define SSSMperm_fc    SSYMperm_fc
#define ZSSMperm_fc    ZSYMperm_fc
#define CSSMperm_fc    CSYMperm_fc

#define DSSMpermfc    DSYMperm_fc
#define SSSMpermfc    SSYMperm_fc
#define ZSSMpermfc    ZSYMperm_fc
#define CSSMpermfc    CSYMperm_fc

#define ZHERperm_fc    ZSYMperm_fc
#define CHERperm_fc    CSYMperm_fc

#define ZHERpermfc    ZSYMperm_fc
#define CHERpermfc    CSYMperm_fc

#define ZSHRperm_fc    ZSYMperm_fc
#define CSHRperm_fc    CSYMperm_fc

#define ZSHRpermfc    ZSYMperm_fc
#define CSHRpermfc    CSYMperm_fc



integer    DSPDperm_rcm   (Dmat, doubleprecision *, doubleprecision *, integer *,integer *,
		       integer *, DILUPACKparam *);

integer    SSPDperm_rcm   (Smat, real *, real *, integer *,integer *,
		       integer *, SILUPACKparam *);

integer    ZHPDperm_rcm   (Zmat, doublecomplex *,   doublecomplex *,   integer *,integer *, 
		       integer *, ZILUPACKparam *);

integer    CHPDperm_rcm   (Cmat, complex *,   complex *,   integer *,integer *, 
		       integer *, CILUPACKparam *);

#define DSPDpermrcm    DSPDperm_rcm   
#define SSPDpermrcm    SSPDperm_rcm   
#define ZHPDpermrcm    ZHPDperm_rcm   
#define CHPDpermrcm    CHPDperm_rcm   

#define DSYMperm_rcm   DSPDperm_rcm
#define SSYMperm_rcm   SSPDperm_rcm
#define CSYMperm_rcm   CHPDperm_rcm
#define ZSYMperm_rcm   ZHPDperm_rcm
#define CHERperm_rcm   CHPDperm_rcm
#define ZHERperm_rcm   ZHPDperm_rcm



#define DSPDperm_mmd   DGNLperm_mmd
#define SSPDperm_mmd   SGNLperm_mmd
#define ZHPDperm_mmd   ZGNLperm_mmd
#define CHPDperm_mmd   CGNLperm_mmd

#define DSPDpermmmd   DGNLperm_mmd
#define SSPDpermmmd   SGNLperm_mmd
#define ZHPDpermmmd   ZGNLperm_mmd
#define CHPDpermmmd   CGNLperm_mmd

#define DSYMperm_mmd   DGNLperm_mmd
#define SSYMperm_mmd   SGNLperm_mmd
#define ZSYMperm_mmd   ZGNLperm_mmd
#define CSYMperm_mmd   CGNLperm_mmd
#define ZHERperm_mmd   ZGNLperm_mmd
#define CHERperm_mmd   CGNLperm_mmd


#define DSPDperm_indset DGNLperm_indset
#define SSPDperm_indset SGNLperm_indset
#define ZHPDperm_indset ZGNLperm_indset
#define CHPDperm_indset CGNLperm_indset

#define DSPDpermindset DGNLperm_indset
#define SSPDpermindset SGNLperm_indset
#define ZHPDpermindset ZGNLperm_indset
#define CHPDpermindset CGNLperm_indset

#define DSYMperm_indset DGNLperm_indset
#define SSYMperm_indset SGNLperm_indset
#define ZSYMperm_indset ZGNLperm_indset
#define CSYMperm_indset CGNLperm_indset
#define ZHERperm_indset ZGNLperm_indset
#define CHERperm_indset CGNLperm_indset


integer    DSPDperm_pp    (Dmat, doubleprecision *, doubleprecision *, integer *,integer *,
		      integer *, DILUPACKparam *);

integer    SSPDperm_pp    (Smat, real *, real *, integer *,integer *,
		      integer *, SILUPACKparam *);

integer    ZHPDperm_pp    (Zmat, doublecomplex *,   doublecomplex *, integer *,integer *,
		      integer *, ZILUPACKparam *);

integer    CHPDperm_pp    (Cmat, complex *,   complex *, integer *,integer *,
		      integer *, CILUPACKparam *);

#define DSPDpermpp     DSPDperm_pp    
#define SSPDpermpp     SSPDperm_pp    
#define ZHPDpermpp     ZHPDperm_pp    
#define CHPDpermpp     CHPDperm_pp    




#define DGNLpermnull      DGNLperm_null   
#define DGNLpermnd        DGNLperm_nd     
#define DGNLpermrcm       DGNLperm_rcm    
#define DGNLpermamf       DGNLperm_amf    
#define DGNLpermamd       DGNLperm_amd 
#define DGNLpermmmd       DGNLperm_mmd    
#define DGNLpermpq        DGNLperm_pq     
#define DGNLpermfc        DGNLperm_fc     
#define DSYMpermfc        DSYMperm_fc     
#define DGNLpermp         DGNLperm_p      
#define DGNLpermindset    DGNLperm_indset         


#define DGNLpermmwm_rcm          DGNLperm_mwm_rcm       
#define DGNLpermmwm_mmd          DGNLperm_mwm_mmd       
#define DGNLpermmwm_amf          DGNLperm_mwm_amf       
#define DGNLpermmwm_amd          DGNLperm_mwm_amd
#define DGNLpermmwm_metis_e      DGNLperm_mwm_metis_e    
#define DGNLpermmwm_metis_n      DGNLperm_mwm_metis_n    
                                                       
#define DGNLpermmc64_rcm         DGNLperm_mc64_rcm      
#define DGNLpermmc64_mmd         DGNLperm_mc64_mmd      
#define DGNLpermmc64_amf         DGNLperm_mc64_amf      
#define DGNLpermmc64_amd         DGNLperm_mc64_amd
#define DGNLpermmc64_metis_e     DGNLperm_mc64_metis_e  
#define DGNLpermmc64_metis_n     DGNLperm_mc64_metis_n  


#define SGNLpermnull    SGNLperm_null    
#define SGNLpermnd      SGNLperm_nd      
#define SGNLpermrcm     SGNLperm_rcm     
#define SGNLpermamf     SGNLperm_amf     
#define SGNLpermamd     SGNLperm_amd
#define SGNLpermmmd     SGNLperm_mmd     
#define SGNLpermpq      SGNLperm_pq      
#define SGNLpermfc      SGNLperm_fc      
#define SSYMpermfc      SSYMperm_fc      
#define SGNLpermp       SGNLperm_p       
#define SGNLpermindset  SGNLperm_indset  

#define SSPDpermrcm	    SSPDperm_rcm	
#define SSPDpermpp	    SSPDperm_pp	

#define SGNLpermmwm_rcm       SGNLperm_mwm_rcm    
#define SGNLpermmwm_mmd       SGNLperm_mwm_mmd    
#define SGNLpermmwm_amf       SGNLperm_mwm_amf    
#define SGNLpermmwm_amd       SGNLperm_mwm_amd
#define SGNLpermmwm_metis_e   SGNLperm_mwm_metis_e
#define SGNLpermmwm_metis_n   SGNLperm_mwm_metis_n

#define SGNLpermmc64_rcm          SGNLperm_mc64_rcm    
#define SGNLpermmc64_mmd	  SGNLperm_mc64_mmd    
#define SGNLpermmc64_amf	  SGNLperm_mc64_amf    
#define SGNLpermmc64_amd	  SGNLperm_mc64_amd
#define SGNLpermmc64_metis_e	  SGNLperm_mc64_metis_e
#define SGNLpermmc64_metis_n	  SGNLperm_mc64_metis_n


#define CGNLpermnull       CGNLperm_null  
#define CGNLpermnd	   CGNLperm_nd    
#define CGNLpermrcm	   CGNLperm_rcm   
#define CGNLpermamf	   CGNLperm_amf   
#define CGNLpermamd	   CGNLperm_amd
#define CGNLpermmmd	   CGNLperm_mmd   
#define CGNLpermpq	   CGNLperm_pq    
#define CGNLpermfc	   CGNLperm_fc    
#define CSYMpermfc	   CSYMperm_fc    
#define CGNLpermp	   CGNLperm_p     
#define CGNLpermindset	   CGNLperm_indset

#define CGNLpermmwm_rcm      CGNLperm_mwm_rcm	   
#define CGNLpermmwm_mmd	     CGNLperm_mwm_mmd	   
#define CGNLpermmwm_amf	     CGNLperm_mwm_amf	   
#define CGNLpermmwm_amd	     CGNLperm_mwm_amd
#define CGNLpermmwm_metis_e  CGNLperm_mwm_metis_e
#define CGNLpermmwm_metis_n  CGNLperm_mwm_metis_n

#define CGNLpermmc64_rcm      CGNLperm_mc64_rcm    
#define CGNLpermmc64_mmd      CGNLperm_mc64_mmd    
#define CGNLpermmc64_amf      CGNLperm_mc64_amf    
#define CGNLpermmc64_amd      CGNLperm_mc64_amd 
#define CGNLpermmc64_metis_e  CGNLperm_mc64_metis_e
#define CGNLpermmc64_metis_n  CGNLperm_mc64_metis_n

#define CHPDpermrcm	 CHPDperm_rcm   
#define CHPDpermpp	 CHPDperm_pp    


#define ZGNLpermnull      ZGNLperm_null	 
#define ZGNLpermnd	  ZGNLperm_nd	 
#define ZGNLpermrcm	  ZGNLperm_rcm	 
#define ZGNLpermamf	  ZGNLperm_amf	 
#define ZGNLpermamd	  ZGNLperm_amd
#define ZGNLpermmmd	  ZGNLperm_mmd	 
#define ZGNLpermpq	  ZGNLperm_pq	 
#define ZGNLpermfc	  ZGNLperm_fc	 
#define ZSYMpermfc	  ZSYMperm_fc	 
#define ZGNLpermp	  ZGNLperm_p	 
#define ZGNLpermindset    ZGNLperm_indset   

#define ZGNLpermmwm_rcm         ZGNLperm_mwm_rcm	       
#define ZGNLpermmwm_mmd		ZGNLperm_mwm_mmd	       
#define ZGNLpermmwm_amf		ZGNLperm_mwm_amf	       
#define ZGNLpermmwm_amd		ZGNLperm_mwm_amd
#define ZGNLpermmwm_metis_e	ZGNLperm_mwm_metis_e    
#define ZGNLpermmwm_metis_n	ZGNLperm_mwm_metis_n    
				                       
#define ZGNLpermmc64_rcm	ZGNLperm_mc64_rcm       
#define ZGNLpermmc64_mmd	ZGNLperm_mc64_mmd       
#define ZGNLpermmc64_amf	ZGNLperm_mc64_amf       
#define ZGNLpermmc64_amd	ZGNLperm_mc64_amd
#define ZGNLpermmc64_metis_e	ZGNLperm_mc64_metis_e   
#define ZGNLpermmc64_metis_n	ZGNLperm_mc64_metis_n   
				                       
#define ZHPDpermrcm		ZHPDperm_rcm	       
#define ZHPDpermpp		ZHPDperm_pp	       




void swapj(integer *, integer, integer);
void dswapm(doubleprecision *, integer, integer);
void sswapm(real *, integer, integer);
integer DPQpermF(Dmat, integer, integer *, integer *, integer *, doubleprecision,doubleprecision *,    integer *);
integer ZPQpermF(Zmat, integer, integer *, integer *, integer *, doubleprecision,doublecomplex *, integer *);
integer indAMF(Dmat, integer, integer *, integer *, doubleprecision);

integer Dindset(Dmat, integer, integer *, integer *, doubleprecision);
integer Sindset(Smat, integer, integer *, integer *, real);
integer Zindset(Zmat, integer, integer *, integer *, doubleprecision);
integer Cindset(Cmat, integer, integer *, integer *, real);

void Dindfc(Dmat, integer *, integer *, integer *, doubleprecision, doubleprecision *, integer *);
void Sindfc(Smat, integer *, integer *, integer *, real, real *, integer *);
void Zindfc(Zmat, integer *, integer *, integer *, doubleprecision, doubleprecision *, integer *);
void Cindfc(Cmat, integer *, integer *, integer *, real, real *, integer *);

void DSYMindfc(Dmat, integer *, integer *, integer *, doubleprecision, doubleprecision *, integer *);
void SSYMindfc(Smat, integer *, integer *, integer *, real, real *, integer *);
void ZSYMindfc(Zmat, integer *, integer *, integer *, doubleprecision, doubleprecision *, integer *);
void CSYMindfc(Cmat, integer *, integer *, integer *, real, real *, integer *);

void Dindfcv(Dmat, integer *, integer *, integer *, doubleprecision, doubleprecision *, integer, doubleprecision *, integer *);
void Sindfcv(Smat, integer *, integer *, integer *, real,            real *,            integer, real *,            integer *);
void Zindfcv(Zmat, integer *, integer *, integer *, doubleprecision, doublecomplex *,   integer, doubleprecision *, integer *);
void Cindfcv(Cmat, integer *, integer *, integer *, real,            complex *,         integer, real *,            integer *);

void DSYMindfcv(Dmat, integer *, integer *, integer *, doubleprecision, doubleprecision *, integer, doubleprecision *, integer *);
void SSYMindfcv(Smat, integer *, integer *, integer *, real,            real *,            integer, real *,            integer *);
void ZSYMindfcv(Zmat, integer *, integer *, integer *, doubleprecision, doublecomplex *,   integer, doubleprecision *, integer *);
void CSYMindfcv(Cmat, integer *, integer *, integer *, real,            complex *,         integer, real *,            integer *);


void dqsortr2i(doubleprecision *, integer *, integer *, integer, integer);
void sqsortr2i(real *, integer *, integer *, integer, integer);

void Dclear(integer, doubleprecision *, integer);
void Sclear(integer, real *,            integer);
void Zclear(integer, doublecomplex *,   integer);
void Cclear(integer, complex *,         integer);



/* ********************************************* */
/* ******      Definitions for solvers     ***** */
void      Dpcg(integer *,doubleprecision *,   doubleprecision *,   integer *,doubleprecision *,doubleprecision *);
void      Dbcg(integer *,doubleprecision *,   doubleprecision *,   integer *,doubleprecision *,doubleprecision *);
void      DSYMbcg(integer *,doubleprecision *,   doubleprecision *,   integer *,doubleprecision *,doubleprecision *);
void      DSYMqmr(integer *,doubleprecision *,   doubleprecision *,   integer *,doubleprecision *,doubleprecision *);
void      Dgmres(integer *,doubleprecision *,doubleprecision *,integer *,doubleprecision *,doubleprecision *);
void      Dfgmres(integer *,doubleprecision *,doubleprecision *,integer *,doubleprecision *,doubleprecision *);

void      Spcg(integer *,real *,   real *,   integer *,real *,real *);
void      Sbcg(integer *,real *,   real *,   integer *,real *,real *);
void      SSYMbcg(integer *,real *,   real *,   integer *,real *,real *);
void      SSYMqmr(integer *,real *,   real *,   integer *,real *,real *);
void      Sgmres(integer *,real *,real *,integer *,real *,real *);
void      Sfgmres(integer *,real *,real *,integer *,real *,real *);

void      Zpcg(integer *,doublecomplex *,doublecomplex *,integer *,doubleprecision *,doublecomplex *);
void      Zbcg(integer *,doublecomplex *,doublecomplex *,integer *,doubleprecision *,doublecomplex *);
void      ZSYMbcg(integer *,doublecomplex *,doublecomplex *,integer *,doubleprecision *,doublecomplex *);
void      ZHERbcg(integer *,doublecomplex *,doublecomplex *,integer *,doubleprecision *,doublecomplex *);
void      ZSYMqmr(integer *,doublecomplex *,doublecomplex *,integer *,doubleprecision *,doublecomplex *);
void      ZHERqmr(integer *,doublecomplex *,doublecomplex *,integer *,doubleprecision *,doublecomplex *);
void      Zgmres(integer *,doublecomplex *,doublecomplex *,integer *,doubleprecision *,doublecomplex *);
void      Zfgmres(integer *,doublecomplex *,doublecomplex *,integer *,doubleprecision *,doublecomplex *);

void      Cpcg(integer *,complex *,complex *,integer *,real *,complex *);
void      Cbcg(integer *,complex *,complex *,integer *,real *,complex *);
void      CSYMbcg(integer *,complex *,complex *,integer *,real *,complex *);
void      CHERbcg(integer *,complex *,complex *,integer *,real *,complex *);
void      CSYMqmr(integer *,complex *,complex *,integer *,real *,complex *);
void      CHERqmr(integer *,complex *,complex *,integer *,real *,complex *);
void      Cgmres(integer *,complex *,complex *,integer *,real *,complex *);
void      Cfgmres(integer *,complex *,complex *,integer *,real *,complex *);


doubleprecision Ddistdot(integer *,doubleprecision *,integer *,doubleprecision *,integer *);

real            Sdistdot(integer *,real *,integer *,real *,integer *);

doublecomplex   Zdistdotc(integer *,doublecomplex *,integer *,doublecomplex *,integer *);
doublecomplex   Zdistdotu(integer *,doublecomplex *,integer *,doublecomplex *,integer *);

complex         Cdistdotc(integer *,complex *,integer *,complex *,integer *);
complex         Cdistdotu(integer *,complex *,integer *,complex *,integer *);


integer DGNLAMGsolver(Dmat, DAMGlevelmat, integer, 
		  DILUPACKparam *, 
		  doubleprecision *, doubleprecision *);
integer DSPDAMGsolver(Dmat, DAMGlevelmat, integer, 
		  DILUPACKparam *, 
		  doubleprecision *, doubleprecision *);
integer DSYMAMGsolver(Dmat, DAMGlevelmat, integer, 
		  DILUPACKparam *, 
		  doubleprecision *, doubleprecision *);

integer SGNLAMGsolver(Smat, SAMGlevelmat, integer, 
		  SILUPACKparam *, 
		  real *, real *);
integer SSPDAMGsolver(Smat, SAMGlevelmat, integer, 
		  SILUPACKparam *, 
		  real *, real *);
integer SSYMAMGsolver(Smat, SAMGlevelmat, integer, 
		  SILUPACKparam *, 
		  real *, real *);

integer ZGNLAMGsolver(Zmat, ZAMGlevelmat, integer, 
		  ZILUPACKparam *, 
		  doublecomplex *, doublecomplex *);
integer ZHPDAMGsolver(Zmat, ZAMGlevelmat, integer, 
		  ZILUPACKparam *, 
		  doublecomplex *, doublecomplex *);
integer ZHERAMGsolver(Zmat, ZAMGlevelmat, integer, 
		  ZILUPACKparam *, 
		  doublecomplex *, doublecomplex *);
integer ZSYMAMGsolver(Zmat, ZAMGlevelmat, integer, 
		  ZILUPACKparam *, 
		  doublecomplex *, doublecomplex *);

integer CGNLAMGsolver(Cmat, CAMGlevelmat, integer, 
		  CILUPACKparam *, 
		  complex *, complex *);
integer CHPDAMGsolver(Cmat, CAMGlevelmat, integer, 
		  CILUPACKparam *, 
		  complex *, complex *);
integer CHERAMGsolver(Cmat, CAMGlevelmat, integer, 
		  CILUPACKparam *, 
		  complex *, complex *);
integer CSYMAMGsolver(Cmat, CAMGlevelmat, integer, 
		  CILUPACKparam *, 
		  complex *, complex *);


void DGNLAMGinit(Dmat, DILUPACKparam *);
void DGNLAMGgetparams(DILUPACKparam,
		      integer *, integer *, doubleprecision *, doubleprecision *,
		      doubleprecision *, integer *, integer *);
void DGNLAMGsetparams(Dmat A, DILUPACKparam *,
		      integer , integer , doubleprecision *, doubleprecision ,
		      doubleprecision , integer , integer );

void SGNLAMGinit(Smat, SILUPACKparam *);
void SGNLAMGgetparams(SILUPACKparam,
		     integer *, integer *, real *, real *,
		     real *, integer *, integer *);
void SGNLAMGsetparams(Smat A, SILUPACKparam *,
		      integer , integer , real *, real ,
		      real , integer , integer );

void ZGNLAMGinit(Zmat, ZILUPACKparam *);
void ZGNLAMGgetparams(ZILUPACKparam,
		     integer *, integer *, doubleprecision *, doubleprecision *,
		     doubleprecision *, integer *, integer *);
void ZGNLAMGsetparams(Zmat A, ZILUPACKparam *,
		      integer , integer , doubleprecision *, doubleprecision ,
		      doubleprecision , integer , integer );

void CGNLAMGinit(Cmat, CILUPACKparam *);
void CGNLAMGgetparams(CILUPACKparam,
		     integer *, integer *, real *, real *,
		     real *, integer *, integer *);
void CGNLAMGsetparams(Cmat A, CILUPACKparam *,
		      integer , integer , real *, real ,
		      real , integer , integer );


void DSPDAMGinit(Dmat, DILUPACKparam *);
void DSPDAMGgetparams(DILUPACKparam,
		     integer *, integer *, doubleprecision *, doubleprecision *,
		     doubleprecision *, integer *);
void DSPDAMGsetparams(Dmat A, DILUPACKparam *,
		      integer , integer , doubleprecision *, doubleprecision ,
		      doubleprecision , integer );

void DSYMAMGinit(Dmat, DILUPACKparam *);
void DSYMAMGgetparams(DILUPACKparam,
		     integer *, doubleprecision *, doubleprecision *,
		     doubleprecision *, doubleprecision *, integer *);


void DSYMAMGsetparams(Dmat A, DILUPACKparam *,
		      integer , doubleprecision , doubleprecision *, doubleprecision ,
		      doubleprecision , integer );

void SSPDAMGinit(Smat, SILUPACKparam *);
void SSPDAMGgetparams(SILUPACKparam,
		     integer *, integer *, real *, real *,
		     real *, integer *);
void SSPDAMGsetparams(Smat A, SILUPACKparam *,
		      integer , integer , real *, real ,
		      real , integer );

void SSYMAMGinit(Smat, SILUPACKparam *);
void SSYMAMGgetparams(SILUPACKparam,
		     integer *, real *, real *, real *,
		     real *, integer *);
void SSYMAMGsetparams(Smat A, SILUPACKparam *,
		      integer , real , real *, real ,
		      real , integer );

void ZHPDAMGinit(Zmat, ZILUPACKparam *);
void ZHERAMGinit(Zmat, ZILUPACKparam *);
void ZSYMAMGinit(Zmat, ZILUPACKparam *);
void ZHPDAMGgetparams(ZILUPACKparam,
		      integer *, integer *, doubleprecision *, doubleprecision *,
		      doubleprecision *, integer *);
void ZHPDAMGsetparams(Zmat A, ZILUPACKparam *,
		      integer , integer , doubleprecision *, doubleprecision ,
		      doubleprecision , integer );
void ZHERAMGgetparams(ZILUPACKparam,
		      integer *, doubleprecision *, doubleprecision *, doubleprecision *,
		      doubleprecision *, integer *);
void ZHERAMGsetparams(Zmat A, ZILUPACKparam *,
		      integer , doubleprecision , doubleprecision *, doubleprecision ,
		      doubleprecision , integer );
void ZSYMAMGgetparams(ZILUPACKparam,
		      integer *, doubleprecision *, doubleprecision *, doubleprecision *,
		      doubleprecision *, integer *);
void ZSYMAMGsetparams(Zmat A, ZILUPACKparam *,
		      integer , doubleprecision , doubleprecision *, doubleprecision ,
		      doubleprecision , integer );

void CHPDAMGinit(Cmat, CILUPACKparam *);
void CHERAMGinit(Cmat, CILUPACKparam *);
void CSYMAMGinit(Cmat, CILUPACKparam *);
void CHPDAMGgetparams(CILUPACKparam,
		      integer *, integer *, real *, real *,
		      real *, integer *);
void CHPDAMGsetparams(Cmat A, CILUPACKparam *,
		      integer , integer , real *, real ,
		      real , integer );
void CHERAMGgetparams(CILUPACKparam,
		      integer *, real *, real *, real *,
		      real *, integer *);
void CHERAMGsetparams(Cmat A, CILUPACKparam *,
		      integer , real , real *, real ,
		      real , integer );
void CSYMAMGgetparams(CILUPACKparam,
		      integer *, real *, real *, real *,
		      real *, integer *);
void CSYMAMGsetparams(Cmat A, CILUPACKparam *,
		      integer , real , real *, real ,
		      real , integer );



/* ********************************************* */
/* ******      Definitions for sparskit    ***** */
void      Dcsrcsc(integer *, integer *, integer *, doubleprecision *, integer *, integer *, 
		  doubleprecision *, integer *, integer *);
void      Dcsrcsc(integer *,integer *,integer *,doubleprecision *,
		  integer *,integer *,doubleprecision *,integer *,integer *);

void      Scsrcsc(integer *, integer *, integer *, real *, integer *, integer *, 
		  real *, integer *, integer *);
void      Scsrcsc(integer *,integer *,integer *,real *,
		  integer *,integer *,real *,integer *,integer *);

void      Zcsrcsc(integer *, integer *, integer *, doublecomplex *, integer *, integer *, 
		  doublecomplex *, integer *, integer *);
void      Zcsrcsc (integer *,integer *,integer *,doublecomplex *,
		   integer *,integer *,doublecomplex *,integer *,integer *);

void      Ccsrcsc(integer *, integer *, integer *, complex *, integer *, integer *, 
		  complex *, integer *, integer *);
void      Ccsrcsc(integer *,integer *,integer *,complex *,
		  integer *,integer *,complex *,integer *,integer *);




/* ******************************************* */
/* ******      Definitions for tools     ***** */
void DGNLmatvec(Dmat, doubleprecision *, doubleprecision *);
void DGNLmattvec(Dmat, doubleprecision *, doubleprecision *);
void DGNLmathvec(Dmat, doubleprecision *, doubleprecision *);

void SGNLmatvec(Smat, real *, real *);
void SGNLmattvec(Smat, real *, real *);
void SGNLmathvec(Smat, real *, real *);

void ZGNLmatvec(Zmat, doublecomplex *, doublecomplex *);
void ZGNLmattvec(Zmat, doublecomplex *, doublecomplex *);
void ZGNLmathvec(Zmat, doublecomplex *, doublecomplex *);

void CGNLmatvec(Cmat, complex *, complex *);
void CGNLmattvec(Cmat, complex *, complex *);
void CGNLmathvec(Cmat, complex *, complex *);


void DSYMmatvec(Dmat, doubleprecision *, doubleprecision *);
void DSSMmatvec(Dmat, doubleprecision *, doubleprecision *);

void SSYMmatvec(Smat, real *, real *);
void SSSMmatvec(Smat, real *, real *);

void ZHERmatvec(Zmat, doublecomplex *, doublecomplex *);
void ZSHRmatvec(Zmat, doublecomplex *, doublecomplex *);
void ZSYMmatvec(Zmat, doublecomplex *, doublecomplex *);
void ZSSMmatvec(Zmat, doublecomplex *, doublecomplex *);

void CHERmatvec(Cmat, complex *, complex *);
void CSHRmatvec(Cmat, complex *, complex *);
void CSYMmatvec(Cmat, complex *, complex *);
void CSSMmatvec(Cmat, complex *, complex *);


void qqsorti(integer *, integer *, integer *); 

integer  Dspartran(Dmat, Dmat *, integer, integer);
void Dsetupgraph(Dmat, Dmat *, integer *, integer *, size_t);
void Dsetupgraph_epsilon(Dmat, Dmat *, doubleprecision, doubleprecision *, integer *, integer *, size_t);
void Dqqsort(doubleprecision *, integer *, integer *, integer *, integer *); 
void Dqqsort2(doubleprecision *, integer *, integer *, integer *, integer *); 
void Dqqsorts(doubleprecision *, integer *, integer *, integer *); 
void Dqqsorts2(doubleprecision *, integer *, integer *, integer *); 
void Dcperm(Dmat *, integer *);
void Drperm(Dmat *, integer *);

integer  Sspartran(Smat, Smat *, integer, integer);
void Ssetupgraph(Smat, Smat *, integer *, integer *, size_t);
void Ssetupgraph_epsilon(Smat, Smat *, real, real *, integer *, integer *, size_t);
void Sqqsort(real *, integer *, integer *, integer *, integer *); 
void Scperm(Smat *, integer *);
void Srperm(Smat *, integer *);

integer  Zspartran(Zmat, Zmat *, integer, integer);
void Zsetupgraph(Zmat, Zmat *, integer *, integer *, size_t);
void Zsetupgraph_epsilon(Zmat, Zmat *, doubleprecision, doubleprecision *, integer *, integer *, size_t);
void Zqqsort(doublecomplex *, integer *, integer *, integer *, integer *); 
void Zcperm(Zmat *, integer *);
void Zrperm(Zmat *, integer *);

integer  Cspartran(Cmat, Cmat *, integer, integer);
void Csetupgraph(Cmat, Cmat *, integer *, integer *, size_t);
void Csetupgraph_epsilon(Cmat, Cmat *, real, real *, integer *, integer *, size_t);
void Cqqsort(complex *, integer *, integer *, integer *, integer *); 
void Ccperm(Cmat *, integer *);
void Crperm(Cmat *, integer *);

void Sqsort(real *,            integer *, integer *, integer *);
void Dqsort(doubleprecision *, integer *, integer *, integer *);
void Cqsort(complex *,         integer *, integer *, integer *);
void Zqsort(doublecomplex *,   integer *, integer *, integer *);



void *MAlloc(size_t, char *);
void *ReAlloc(void *, size_t, char *);
doubleprecision dgeteps();
real            sgeteps();
float evaluate_time(float *, float *);
void evaluatetime(float *);


void Droscal(integer *,integer *,integer *,doubleprecision *,integer *,
	    integer *,doubleprecision *,doubleprecision *,integer *,integer *,
	    integer *);
void Dcoscal(integer *,integer *,integer *,doubleprecision *,integer *,
	    integer *,doubleprecision *,doubleprecision *,integer *,integer *,
	    integer *);
void Drowscale(integer *,integer *,doubleprecision *,integer *,
	       integer *,doubleprecision *,integer *);
void Dcolscale(integer *,integer *,doubleprecision *,integer *,
	       integer *,doubleprecision *,integer *);
void DSPDscale(integer *, doubleprecision *,integer *,
	       integer *,doubleprecision *,integer *);
void DSYMscale(integer *, doubleprecision *,integer *,
	       integer *,doubleprecision *,doubleprecision *,integer *);

void Sroscal(integer *,integer *,integer *,real *,integer *,
	    integer *,real *,real *,integer *,integer *,
	    integer *);
void Scoscal(integer *,integer *,integer *,real *,integer *,
	    integer *,real *,real *,integer *,integer *,
	    integer *);
void Srowscale(integer *,integer *,real *,integer *,
	       integer *,real *,integer *);
void Scolscale(integer *,integer *,real *,integer *,
	       integer *,real *,integer *);
void SSPDscale(integer *, real *,integer *,
	       integer *,real *,integer *);
void SSYMscale(integer *, real *,integer *,
	       integer *,real *,real *,integer *);

void Zroscal(integer *,integer *,integer *,doublecomplex *,integer *,
	    integer *,doublecomplex *,doublecomplex *,integer *,integer *,
	    integer *);
void Zcoscal(integer *,integer *,integer *,doublecomplex *,integer *,
	    integer *,doublecomplex *,doublecomplex *,integer *,integer *,
	    integer *);
void Zrowscale(integer *,integer *,doublecomplex *,integer *,
	       integer *,doublecomplex *,integer *);
void Zcolscale(integer *,integer *,doublecomplex *,integer *,
	       integer *,doublecomplex *,integer *);
void ZHPDscale(integer *, doublecomplex *,integer *,
	       integer *,doublecomplex *,integer *);
void ZSYMscale(integer *, doublecomplex *,integer *,
	       integer *,doublecomplex *,doublecomplex *,integer *);
void ZHERscale(integer *, doublecomplex *,integer *,
	       integer *,doublecomplex *,doublecomplex *,integer *);

void Croscal(integer *,integer *,integer *,complex *,integer *,
	    integer *,complex *,complex *,integer *,integer *,
	    integer *);
void Ccoscal(integer *,integer *,integer *,complex *,integer *,
	    integer *,complex *,complex *,integer *,integer *,
	    integer *);
void Crowscale(integer *,integer *,complex *,integer *,
	       integer *,complex *,integer *);
void Ccolscale(integer *,integer *,complex *,integer *,
	       integer *,complex *,integer *);
void CHPDscale(integer *, complex *,integer *,
	       integer *,complex *,integer *);
void CSYMscale(integer *, complex *,integer *,
	       integer *, complex *,complex *,integer *);
void CHERscale(integer *, complex *,integer *,
	       integer *,complex *,complex *,integer *);


integer DPQpermF(Dmat, integer, integer *, integer *, integer *, doubleprecision, doubleprecision *, integer *);
integer SPQpermF(Smat, integer, integer *, integer *, integer *, real,            real *,            integer *);
integer ZPQpermF(Zmat, integer, integer *, integer *, integer *, doubleprecision, doublecomplex *,   integer *);
integer CPQpermF(Cmat, integer, integer *, integer *, integer *, real,            complex *,         integer *);

integer DPPpermF(Dmat, integer, integer *, integer *, doubleprecision, doubleprecision *, integer *);
integer SPPpermF(Smat, integer, integer *, integer *, real,            real *,            integer *);
integer ZPPpermF(Zmat, integer, integer *, integer *, doubleprecision, doublecomplex *,   integer *);
integer CPPpermF(Cmat, integer, integer *, integer *, real,            complex *,         integer *);





void      Dreadmtc(integer *,integer *,integer *,character *,doubleprecision *,integer *,
		   integer *,doubleprecision *,integer *,character *,integer *,integer *,
		   integer *,character *,character *,character *,
		   integer *, integer *, integer *, doubleprecision *,
		   integer *,ftnlen,ftnlen, ftnlen,ftnlen,ftnlen);
void      Sreadmtc(integer *,integer *,integer *,character *,real *,integer *,
		   integer *,real *,integer *,character *,integer *,integer *,
		   integer *,character *,character *,character *,
		   integer *, integer *, integer *, real *,
		   integer *,ftnlen,ftnlen, ftnlen,ftnlen,ftnlen);
void      Zreadmtc(integer *,integer *,integer *,character *,doublecomplex *,integer *,
		   integer *,doublecomplex *,integer *,character *,integer *,integer *,
		   integer *,character *,character *,character *,
		   integer *, integer *, integer *, doublecomplex *,
		   integer *,ftnlen,ftnlen,ftnlen,ftnlen,ftnlen);
void      Creadmtc(integer *,integer *,integer *,character *,complex *,integer *,
		   integer *,complex *,integer *,character *,integer *,integer *,
		   integer *,character *,character *,character *,
		   integer *, integer *, integer *, complex *,
		   integer *,ftnlen,ftnlen,ftnlen,ftnlen,ftnlen);
void      Dwritemtc(character *, doubleprecision *,integer *,integer *,
		    doubleprecision *,integer *, character *,
		    integer *,integer *,integer *,
		    character *,character *,character *,
		    ftnlen,ftnlen,ftnlen,ftnlen,ftnlen);
void      Swritemtc(character *, real *,integer *,integer *,
		    real *,integer *, character *,
		    integer *,integer *,integer *,
		    character *,character *,character *,
		    ftnlen,ftnlen,ftnlen,ftnlen,ftnlen);
void      Zwritemtc(character *, doublecomplex *,integer *,integer *,
		    doublecomplex *,integer *, character *,
		    integer *,integer *,integer *,
		    character *,character *,character *,
		    ftnlen,ftnlen,ftnlen,ftnlen,ftnlen);
void      Cwritemtc(character *, complex *,integer *,integer *,
		    complex *,integer *, character *,
		    integer *,integer *,integer *,
		    character *,character *,character *,
		    ftnlen,ftnlen,ftnlen,ftnlen,ftnlen);

void      Dreadvectors(character *,doubleprecision *,integer *,integer *,
		       character *,character *, ftnlen,ftnlen,ftnlen);
void      Sreadvectors(character *,real *,integer *,integer *,
		       character *,character *, ftnlen,ftnlen,ftnlen);
void      Zreadvectors(character *,doublecomplex *,integer *,integer *,
		       character *,character *, ftnlen,ftnlen,ftnlen);
void      Creadvectors(character *,complex *,integer *,integer *,
		       character *,character *, ftnlen,ftnlen,ftnlen);

void      Dwritevectors(character *,doubleprecision *,integer *,integer *,
			character *,character *, ftnlen,ftnlen,ftnlen);
void      Swritevectors(character *,real *,integer *,integer *,
			character *,character *, ftnlen,ftnlen,ftnlen);
void      Zwritevectors(character *,doublecomplex *,integer *,integer *,
			character *,character *, ftnlen,ftnlen,ftnlen);
void      Cwritevectors(character *,complex *,integer *,integer *,
			character *,character *, ftnlen,ftnlen,ftnlen);


integer  DSSMsmwm(Dmat, integer *, integer *, doubleprecision *),
     SSSMsmwm(Smat, integer *, integer *, real *),
     DSYMsmwm(Dmat, integer *, integer *, doubleprecision *),
     SSYMsmwm(Smat, integer *, integer *, real *),
     CSSMsmwm(Cmat, integer *, integer *, complex *),
     ZSSMsmwm(Zmat, integer *, integer *, doublecomplex *),
     CSYMsmwm(Cmat, integer *, integer *, complex *),
     ZSYMsmwm(Zmat, integer *, integer *, doublecomplex *),
     CSHRsmwm(Cmat, integer *, integer *, complex *),
     ZSHRsmwm(Zmat, integer *, integer *, doublecomplex *),
     CHERsmwm(Cmat, integer *, integer *, complex *),
     ZHERsmwm(Zmat, integer *, integer *, doublecomplex *);


void SSYMAMGdelete(Smat, SAMGlevelmat, integer, SILUPACKparam *);
void DSYMAMGdelete(Dmat, DAMGlevelmat, integer, DILUPACKparam *);
void CSYMAMGdelete(Cmat, CAMGlevelmat, integer, CILUPACKparam *);
void ZSYMAMGdelete(Zmat, ZAMGlevelmat, integer, ZILUPACKparam *);
void CHERAMGdelete(Cmat, CAMGlevelmat, integer, CILUPACKparam *);
void ZHERAMGdelete(Zmat, ZAMGlevelmat, integer, ZILUPACKparam *);


void SSPDAMGdelete(Smat, SAMGlevelmat, integer, SILUPACKparam *);
void DSPDAMGdelete(Dmat, DAMGlevelmat, integer, DILUPACKparam *);
void CHPDAMGdelete(Cmat, CAMGlevelmat, integer, CILUPACKparam *);
void ZHPDAMGdelete(Zmat, ZAMGlevelmat, integer, ZILUPACKparam *);


void SGNLAMGdelete(Smat, SAMGlevelmat, integer, SILUPACKparam *);
void DGNLAMGdelete(Dmat, DAMGlevelmat, integer, DILUPACKparam *);
void CGNLAMGdelete(Cmat, CAMGlevelmat, integer, CILUPACKparam *);
void ZGNLAMGdelete(Zmat, ZAMGlevelmat, integer, ZILUPACKparam *);

integer dsymilupack   (integer *, integer *, integer *, doubleprecision *, doubleprecision *, doubleprecision *,
		   doubleprecision *, doubleprecision *, integer *, integer *, 
		   doubleprecision *, integer *);
integer dsymilupackfac(long *, long *, 
		   integer *, integer *, integer *, doubleprecision *, doubleprecision *, doubleprecision *,
		   doubleprecision *, doubleprecision *, integer *, integer *, integer *, 
		   doubleprecision *, integer *);
integer dsymilupacksol(long *, long *, 
		   integer *, integer *, integer *, doubleprecision *, doubleprecision *, doubleprecision *,
		   doubleprecision *, doubleprecision *, integer *, integer *, integer *, 
		   doubleprecision *, integer *);
integer dsymilupackdel(long *, long *, 
		   integer *, integer *, integer *, doubleprecision *, doubleprecision *, doubleprecision *,
		   doubleprecision *, doubleprecision *, integer *, integer *, integer *, 
		   doubleprecision *, integer *);


integer ssymilupack   (integer *, integer *, integer *, real *, real *, real *,
		   real *, real *, integer *, integer *, 
		   real *, integer *);
integer ssymilupackfac(long *, long *, 
		   integer *, integer *, integer *, real *, real *, real *,
		   real *, real *, integer *, integer *, integer *, 
		   real *, integer *);
integer ssymilupacksol(long *, long *, 
		   integer *, integer *, integer *, real *, real *, real *,
		   real *, real *, integer *, integer *, integer *, 
		   real *, integer *);
integer ssymilupackdel(long *, long *, 
		   integer *, integer *, integer *, real *, real *, real *,
		   real *, real *, integer *, integer *, integer *, 
		   real *, integer *);


integer csymilupack   (integer *, integer *, integer *, complex *, complex *, complex *,
		   real *, real *, integer *, integer *, 
		   real *, integer *);
integer csymilupackfac(long *, long *, 
		   integer *, integer *, integer *, complex *, complex *, complex *,
		   real *, real *, integer *, integer *, integer *, 
		   real *, integer *);
integer csymilupacksol(long *, long *, 
		   integer *, integer *, integer *, complex *, complex *, complex *,
		   real *, real *, integer *, integer *, integer *, 
		   real *, integer *);
integer csymilupackdel(long *, long *, 
		   integer *, integer *, integer *, complex *, complex *, complex *,
		   real *, real *, integer *, integer *, integer *, 
		   real *, integer *);
integer chermilupack   (integer *, integer *, integer *, complex *, complex *, complex *,
		   real *, real *, integer *, integer *, 
		   real *, integer *);
integer cherilupackfac(long *, long *, 
		   integer *, integer *, integer *, complex *, complex *, complex *,
		   real *, real *, integer *, integer *, integer *, 
		   real *, integer *);
integer cherilupacksol(long *, long *, 
		   integer *, integer *, integer *, complex *, complex *, complex *,
		   real *, real *, integer *, integer *, integer *, 
		   real *, integer *);
integer cherilupackdel(long *, long *, 
		   integer *, integer *, integer *, complex *, complex *, complex *,
		   real *, real *, integer *, integer *, integer *, 
		   real *, integer *);


integer zsymilupack   (integer *, integer *, integer *, doublecomplex *, doublecomplex *, doublecomplex *,
		   doubleprecision *, doubleprecision *, integer *, integer *, 
		   doubleprecision *, integer *);
integer zsymilupackfac(long *, long *, 
		   integer *, integer *, integer *, doublecomplex *, doublecomplex *, doublecomplex *,
		   doubleprecision *, doubleprecision *, integer *, integer *, integer *, 
		   doubleprecision *, integer *);
integer zsymilupacksol(long *, long *, 
		   integer *, integer *, integer *, doublecomplex *, doublecomplex *, doublecomplex *,
		   doubleprecision *, doubleprecision *, integer *, integer *, integer *, 
		   doubleprecision *, integer *);
integer zsymilupackdel(long *, long *, 
		   integer *, integer *, integer *, doublecomplex *, doublecomplex *, doublecomplex *,
		   doubleprecision *, doubleprecision *, integer *, integer *, integer *, 
		   doubleprecision *, integer *);

integer zherilupack   (integer *, integer *, integer *, doublecomplex *, doublecomplex *, doublecomplex *,
		   doubleprecision *, doubleprecision *, integer *, integer *, 
		   doubleprecision *, integer *);
integer zherilupackfac(long *, long *, 
		   integer *, integer *, integer *, doublecomplex *, doublecomplex *, doublecomplex *,
		   doubleprecision *, doubleprecision *, integer *, integer *, integer *, 
		   doubleprecision *, integer *);
integer zherilupacksol(long *, long *, 
		   integer *, integer *, integer *, doublecomplex *, doublecomplex *, doublecomplex *,
		   doubleprecision *, doubleprecision *, integer *, integer *, integer *, 
		   doubleprecision *, integer *);
integer zherilupackdel(long *, long *, 
		   integer *, integer *, integer *, doublecomplex *, doublecomplex *, doublecomplex *,
		   doubleprecision *, doubleprecision *, integer *, integer *, integer *, 
		   doubleprecision *, integer *);




void sgnlamginit(integer *, integer *, integer *,
		real *, integer *, character *, real *,
		real *, real *, integer *, integer *,
		integer *, integer *, integer *);
void dgnlamginit(integer *, integer *, integer *,
		doubleprecision *, integer *, character *, doubleprecision *,
		doubleprecision *, doubleprecision *, integer *, integer *,
		integer *, integer *, integer *);
void cgnlamginit(integer *, integer *, integer *,
		complex *, integer *, character *, real *,
		real *, real *, integer *, integer *,
		integer *, integer *, integer *);
void zgnlamginit(integer *, integer *, integer *,
		doublecomplex *, integer *, character *, doubleprecision *,
		doubleprecision *, doubleprecision *, integer *, integer *,
		integer *, integer *, integer *);

void sspdamginit(integer *, integer *, integer *,
		real *, integer *, character *, real *,
		real *, real *, integer *, integer *,
		integer *, integer *, integer *);
void dspdamginit(integer *, integer *, integer *,
		doubleprecision *, integer *, character *, doubleprecision *,
		doubleprecision *, doubleprecision *, integer *, integer *,
		integer *, integer *, integer *);
void chpdamginit(integer *, integer *, integer *,
		complex *, integer *, character *, real *,
		real *, real *, integer *, integer *,
		integer *, integer *, integer *);
void zhpdamginit(integer *, integer *, integer *,
		doublecomplex *, integer *, character *, doubleprecision *,
		doubleprecision *, doubleprecision *, integer *, integer *,
		integer *, integer *, integer *);

void ssymamginit(integer *, integer *, integer *,
		 real *, integer *, character *, real *,
		 real *, real *, integer *, real *,
		 integer *, integer *, integer *);
void dsymamginit(integer *, integer *, integer *,
		 doubleprecision *, integer *, character *, doubleprecision *,
		 doubleprecision *, doubleprecision *, integer *, doubleprecision *,
		 integer *, integer *, integer *);
void cheramginit(integer *, integer *, integer *,
		 complex *, integer *, character *, real *,
		 real *, real *, integer *, real *,
		 integer *, integer *, integer *);
void zheramginit(integer *, integer *, integer *,
		 doublecomplex *, integer *, character *, doubleprecision *,
		 doubleprecision *, doubleprecision *, integer *, doubleprecision *,
		 integer *, integer *, integer *);
void csymamginit(integer *, integer *, integer *,
		 complex *, integer *, character *, real *,
		 real *, real *, integer *, real *,
		 integer *, integer *, integer *);
void zsymamginit(integer *, integer *, integer *,
		 doublecomplex *, integer *, character *, doubleprecision *,
		 doubleprecision *, doubleprecision *, integer *, doubleprecision *,
		 integer *, integer *, integer *);


void ddglprecsetup(size_t *,  doubleprecision *, integer *, 
		   integer *, doubleprecision *, doubleprecision *,
		   integer *, integer *, doubleprecision *);
void ddglprecsol(size_t *,  integer *, 
		 doubleprecision *, doubleprecision *);
void ddglprecdelete(size_t *);

void sdglprecsetup(size_t *,  real *, integer *, 
		   integer *, real *, real *,
		   integer *, integer *, real *);
void sdglprecsol(size_t *,  integer *, 
		 real *, real *);
void sdglprecdelete(size_t *);

void zdglprecsetup(size_t *,  doublecomplex *, integer *, 
		   integer *, doubleprecision *, doubleprecision *,
		   integer *, integer *, doubleprecision *);
void zdglprecsol(size_t *,  integer *, 
		 doublecomplex *, doublecomplex *);
void zdglprecdelete(size_t *);

void cdglprecsetup(size_t *,  complex *, integer *, 
		   integer *, real *, real *,
		   integer *, integer *, real *);
void cdglprecsol(size_t *,  integer *, 
		 complex *, complex *);
void cdglprecdelete(size_t *);


int sgnlamgfactor(size_t *, size_t *, integer *,
		  integer *, integer *, integer *,
		  real *, integer *, character *, real *,
		  real *, real *, integer *, integer *,
		  integer *, integer *, integer *);
int dgnlamgfactor(size_t *, size_t *, integer *,
		  integer *, integer *, integer *,
		  doubleprecision *, integer *, character *, doubleprecision *,
		  doubleprecision *, doubleprecision *, integer *, integer *,
		  integer *, integer *, integer *);
int cgnlamgfactor(size_t *, size_t *, integer *,
		  integer *, integer *, integer *,
		  complex *, integer *, character *, real *,
		  real *, real *, integer *, integer *,
		  integer *, integer *, integer *);
int zgnlamgfactor(size_t *, size_t *, integer *,
		  integer *, integer *, integer *,
		  doublecomplex *, integer *, character *, doubleprecision *,
		  doubleprecision *, doubleprecision *, integer *, integer *,
		  integer *, integer *, integer *);

int sspdamgfactor(size_t *, size_t *, integer *,
		  integer *, integer *, integer *,
		  real *, integer *, character *, real *,
		  real *, real *, integer *, integer *,
		  integer *, integer *, integer *);
int dspdamgfactor(size_t *, size_t *, integer *,
		  integer *, integer *, integer *,
		  doubleprecision *, integer *, character *, doubleprecision *,
		  doubleprecision *, doubleprecision *, integer *, integer *,
		  integer *, integer *, integer *);
int chpdamgfactor(size_t *, size_t *, integer *,
		  integer *, integer *, integer *,
		  complex *, integer *, character *, real *,
		  real *, real *, integer *, integer *,
		  integer *, integer *, integer *);
int zhpdamgfactor(size_t *, size_t *, integer *,
		  integer *, integer *, integer *,
		  doublecomplex *, integer *, character *, doubleprecision *,
		  doubleprecision *, doubleprecision *, integer *, integer *,
		  integer *, integer *, integer *);

doubleprecision dsymamgsavediag(size_t *, integer *, integer *, integer *,
		       doubleprecision *);
doubleprecision dsymamgsavediaggep(size_t *, integer *, integer *, integer *,
		       doubleprecision *, integer *, integer *,
		       doubleprecision *);
real ssymamgsavediag(size_t *, integer *, integer *, integer *,
		       real *);
real ssymamgsavediaggep(size_t *, integer *, integer *, integer *,
		       real *, integer *, integer *,
		       real *);
doubleprecision zheramgsavediag(size_t *, integer *, integer *, integer *,
		       doublecomplex *);
doubleprecision zheramgsavediaggep(size_t *, integer *, integer *, integer *,
		       doublecomplex *, integer *, integer *,
		       doublecomplex *);
real cheramgsavediag(size_t *, integer *, integer *, integer *,
		       complex *);
real cheramgsavediaggep(size_t *, integer *, integer *, integer *,
		       complex *, integer *, integer *,
		       complex *);
void ssymamgrestorediag(size_t *, integer *, integer *, integer *,
			real *);
void dsymamgrestorediag(size_t *, integer *, integer *, integer *,
			doubleprecision *);
void cheramgrestorediag(size_t *, integer *, integer *, integer *,
			complex *);
void zheramgrestorediag(size_t *, integer *, integer *, integer *,
			doublecomplex *);


integer ssymamgfactor(size_t *, size_t *, integer *, integer *,
		  integer *, integer *, integer *,
		  real *, integer *, character *, real *,
		  real *, real *, integer *, real *,
		  integer *, integer *, integer *);
integer dsymamgfactor(size_t *, size_t *, integer *, integer *,
		  integer *, integer *, integer *,
		  doubleprecision *, integer *, character *, doubleprecision *, 
		  doubleprecision *, doubleprecision *, integer *, doubleprecision *,
		  integer *, integer *, integer *);
integer cheramgfactor(size_t *, size_t *, integer *, integer *,
		  integer *, integer *, integer *,
		  complex *, integer *, character *, real *,
		  real *, real *, integer *, real *,
		  integer *, integer *, integer *);
integer zheramgfactor(size_t *, size_t *, integer *, integer *,
		  integer *, integer *, integer *,
		      doublecomplex *, integer *, character *, doubleprecision *,
		  doubleprecision *, doubleprecision *, integer *, doubleprecision *,
		  integer *, integer *, integer *);
integer csymamgfactor(size_t *, size_t *, integer *, integer *,
		  integer *, integer *, integer *,
		  complex *, integer *, real *,
		  real *, real *, integer *, integer *,
		  integer *, integer *, integer *);
integer zsymamgfactor(size_t *, size_t *, integer *, integer *,
		  integer *, integer *, integer *,
		  doublecomplex *, integer *, doubleprecision *,
		  doubleprecision *, doubleprecision *, integer *, integer *,
		  integer *, integer *, integer *);

integer ssymamgfactorgep(size_t *, size_t *, integer *, integer *,
		  integer *, integer *, integer *,
		  real *, integer *, integer *,
		  real *, real *, integer *, character *, real *,
		  real *, real *, integer *, real *,
		  integer *, integer *, integer *);
integer dsymamgfactorgep(size_t *, size_t *, integer *, integer *,
		  integer *, integer *, integer *,
		  doubleprecision *,  integer *, integer *,
		  doubleprecision *, doubleprecision *, 
                  integer *, character *, doubleprecision *, 
		  doubleprecision *, doubleprecision *, integer *, doubleprecision *,
		  integer *, integer *, integer *);
integer cheramgfactorgep(size_t *, size_t *, integer *, integer *,
		  integer *, integer *, integer *,
		  complex *, integer *, integer *,
		  complex *, real *, integer *, character *, real *,
		  real *, real *, integer *, real *,
		  integer *, integer *, integer *);
integer zheramgfactorgep(size_t *, size_t *, integer *, integer *,
		  integer *, integer *, integer *,
 	          doublecomplex *, integer *, integer *,
		  doublecomplex *, doubleprecision *, integer *, character *, doubleprecision *,
		  doubleprecision *, doubleprecision *, integer *, doubleprecision *,
		  integer *, integer *, integer *);
integer csymamgfactorgep(size_t *, size_t *, integer *, integer *,
		  integer *, integer *, integer *,
		  complex *, integer *, integer *,
		  complex *, real *, integer *, character *, real *,
		  real *, real *, integer *, real *,
		  integer *, integer *, integer *);
integer zsymamgfactorgep(size_t *, size_t *, integer *, integer *,
		  integer *, integer *, integer *,
 	          doublecomplex *, integer *, integer *,
		  doublecomplex *, doubleprecision *, integer *, character *, doubleprecision *,
		  doubleprecision *, doubleprecision *, integer *, doubleprecision *,
		  integer *, integer *, integer *);


integer sgnlamgsolver(size_t *, size_t *, integer *,
		  real *, real *, integer *, integer *, integer *,
		  real *, integer *, character *, real *,
		  real *, real *, integer *, integer *,
		  integer *, integer *, integer *);
integer dgnlamgsolver(size_t *, size_t *, integer *,
		  doubleprecision *, doubleprecision *, integer *, integer *, integer *,
		  doubleprecision *, integer *, character *, doubleprecision *,
		  doubleprecision *, doubleprecision *, integer *, integer *,
		  integer *, integer *, integer *);
integer cgnlamgsolver(size_t *, size_t *, integer *,
		  complex *, complex *, integer *, integer *, integer *,
		  complex *, integer *, character *, real *,
		  real *, real *, integer *, integer *,
		  integer *, integer *, integer *);
integer zgnlamgsolver(size_t *, size_t *, integer *,
		  doublecomplex *, doublecomplex *, integer *, integer *, integer *,
		  doublecomplex *, integer *, character *, doubleprecision *,
		  doubleprecision *, doubleprecision *, integer *, integer *,
		  integer *, integer *, integer *);

integer sspdamgsolver(size_t *, size_t *, integer *,
		  real *, real *, integer *, integer *, integer *,
		  real *, integer *, character *, real *,
		  real *, real *, integer *, integer *,
		  integer *, integer *, integer *);
integer dspdamgsolver(size_t *, size_t *, integer *,
		  doubleprecision *, doubleprecision *, integer *, integer *, integer *,
		  doubleprecision *, integer *, character *, doubleprecision *,
		  doubleprecision *, doubleprecision *, integer *, integer *,
		  integer *, integer *, integer *);
integer chpdamgsolver(size_t *, size_t *, integer *,
		  complex *, complex *, integer *, integer *, integer *,
		  complex *, integer *, character *, real *,
		  real *, real *, integer *, integer *,
		  integer *, integer *, integer *);
integer zhpdamgsolver(size_t *, size_t *, integer *,
		  doublecomplex *, doublecomplex *, integer *, integer *, integer *,
		  doublecomplex *, integer *, character *, doubleprecision *,
		  doubleprecision *, doubleprecision *, integer *, integer *,
		  integer *, integer *, integer *);

integer ssymamgsolver(size_t *, size_t *, integer *,
		  real *, real *, integer *, integer *, integer *,
		  real *, integer *, character *, real *,
		  real *, real *, integer *, integer *,
		  integer *, integer *, integer *);
integer dsymamgsolver(size_t *, size_t *, integer *,
		  doubleprecision *, doubleprecision *, integer *, integer *, integer *,
		  doubleprecision *, integer *, character *, doubleprecision *,
		  doubleprecision *, doubleprecision *, integer *, integer *,
		  integer *, integer *, integer *);
integer cheramgsolver(size_t *, size_t *, integer *,
		  complex *, complex *, integer *, integer *, integer *,
		  complex *, integer *, character *, real *,
		  real *, real *, integer *, integer *,
		  integer *, integer *, integer *);
integer zheramgsolver(size_t *, size_t *, integer *,
		  doublecomplex *, doublecomplex *, integer *, integer *, integer *,
		  doublecomplex *, integer *, character *, doubleprecision *,
		  doubleprecision *, doubleprecision *, integer *, integer *,
		  integer *, integer *, integer *);
integer csymamgsolver(size_t *, size_t *, integer *,
		  complex *, complex *, integer *, integer *, integer *,
		  complex *, integer *, character *, real *,
		  real *, real *, integer *, integer *,
		  integer *, integer *, integer *);
integer zsymamgsolver(size_t *, size_t *, integer *,
		  doublecomplex *, doublecomplex *, integer *, integer *, integer *,
		  doublecomplex *, integer *, character *, doubleprecision *,
		  doubleprecision *, doubleprecision *, integer *, integer *,
		  integer *, integer *, integer *);


void sgnlamgsol(size_t *, size_t *, integer *,
		real *, real *, integer *);
void dgnlamgsol(size_t *, size_t *, integer *,
		doubleprecision *, doubleprecision *, integer *);
void cgnlamgsol(size_t *, size_t *, integer *,
		complex *, complex *, integer *);
void zgnlamgsol(size_t *, size_t *, integer *,
		doublecomplex *, doublecomplex *, integer *);

void sspdamgsol(size_t *, size_t *, integer *,
		real *, real *, integer *);
void dspdamgsol(size_t *, size_t *, integer *,
		doubleprecision *, doubleprecision *, integer *);
void chpdamgsol(size_t *, size_t *, integer *,
		complex *, complex *, integer *);
void zhpdamgsol(size_t *, size_t *, integer *,
		doublecomplex *, doublecomplex *, integer *);

void ssymamgsol(size_t *, size_t *, integer *,
		real *, real *, integer *);
void dsymamgsol(size_t *, size_t *, integer *,
		doubleprecision *, doubleprecision *, integer *);
void csymamgsol(size_t *, size_t *, integer *,
		complex *, complex *, integer *);
void zsymamgsol(size_t *, size_t *, integer *,
		doublecomplex *, doublecomplex *, integer *);
void cheramgsol(size_t *, size_t *, integer *,
		complex *, complex *, integer *);
void zheramgsol(size_t *, size_t *, integer *,
		doublecomplex *, doublecomplex *, integer *);


void sgnlamgdelete(size_t *, size_t *, integer *);
void dgnlamgdelete(size_t *, size_t *, integer *);
void cgnlamgdelete(size_t *, size_t *, integer *);
void zgnlamgdelete(size_t *, size_t *, integer *);

void sspdamgdelete(size_t *, size_t *, integer *);
void dspdamgdelete(size_t *, size_t *, integer *);
void chpdamgdelete(size_t *, size_t *, integer *);
void zhpdamgdelete(size_t *, size_t *, integer *);

void ssymamgdelete(size_t *, size_t *, integer *, integer *);
void dsymamgdelete(size_t *, size_t *, integer *, integer *);
void cheramgdelete(size_t *, size_t *, integer *, integer *);
void zheramgdelete(size_t *, size_t *, integer *, integer *);
void csymamgdelete(size_t *, size_t *, integer *, integer *);
void zsymamgdelete(size_t *, size_t *, integer *, integer *);



void sspdamginfo(size_t *, size_t *, integer *, integer *, integer *, 
	         integer *, real *);
void dspdamginfo(size_t *, size_t *, integer *,
		  integer *, integer *, integer *, doubleprecision *);
void chpdamginfo(size_t *, size_t *, integer *,
		  integer *, integer *, integer *, complex *);
void zhpdamginfo(size_t *, size_t *, integer *,
		  integer *, integer *, integer *, doublecomplex *);

void ssymamginfo(size_t *, size_t *, integer *,
		  integer *, integer *, integer *, real *);
void dsymamginfo(size_t *, size_t *, integer *,
		  integer *, integer *, integer *, doubleprecision *);
void cheramginfo(size_t *, size_t *, integer *,
		  integer *, integer *, integer *, complex *);
void zheramginfo(size_t *, size_t *, integer *,
		  integer *, integer *, integer *, doublecomplex *);
void csymamginfo(size_t *, size_t *, integer *,
		  integer *, integer *, integer *, complex *);
void zsymamginfo(size_t *, size_t *, integer *,
		  integer *, integer *, integer *, doublecomplex *);

void sgnlamginfo(size_t *, size_t *, integer *,
		  integer *, integer *, integer *, real *);
void dgnlamginfo(size_t *, size_t *, integer *,
		  integer *, integer *, integer *, doubleprecision *);
void cgnlamginfo(size_t *, size_t *, integer *,
		  integer *, integer *, integer *, complex *);
void zgnlamginfo(size_t *, size_t *, integer *,
		  integer *, integer *, integer *, doublecomplex *);


size_t sspdamgnnz(size_t *, size_t *, integer *);
size_t dspdamgnnz(size_t *, size_t *, integer *);
size_t chpdamgnnz(size_t *, size_t *, integer *);
size_t zhpdamgnnz(size_t *, size_t *, integer *);

size_t ssymamgnnz(size_t *, size_t *, integer *);
size_t dsymamgnnz(size_t *, size_t *, integer *);
size_t cheramgnnz(size_t *, size_t *, integer *);
size_t zheramgnnz(size_t *, size_t *, integer *);
size_t csymamgnnz(size_t *, size_t *, integer *);
size_t zsymamgnnz(size_t *, size_t *, integer *);

size_t sgnlamgnnz(size_t *, size_t *, integer *);
size_t dgnlamgnnz(size_t *, size_t *, integer *);
size_t cgnlamgnnz(size_t *, size_t *, integer *);
size_t zgnlamgnnz(size_t *, size_t *, integer *);

integer ssymspdamgconvert(size_t *, size_t *, integer *);
integer dsymspdamgconvert(size_t *, size_t *, integer *);
integer cherhpdamgconvert(size_t *, size_t *, integer *);
integer zherhpdamgconvert(size_t *, size_t *, integer *);

void samgundoscaling(size_t *, size_t *, integer *, integer *, integer *, integer *, real *);
void damgundoscaling(size_t *, size_t *, integer *, integer *, integer *, integer *, doubleprecision *);
void camgundoscaling(size_t *, size_t *, integer *, integer *, integer *, integer *, complex *);
void zamgundoscaling(size_t *, size_t *, integer *, integer *, integer *, integer *, doublecomplex *);


/*
void DGNLSYM(Dmat , Dmat *, integer *);
void SGNLSYM(Smat , Smat *, integer *);
void CGNLSYM(Cmat , Cmat *, integer *);
void ZGNLSYM(Zmat , Zmat *, integer *);
void CGNLHER(Cmat , Cmat *, integer *);
void ZGNLHER(Zmat , Zmat *, integer *);
*/
integer SSYMSPDAMGconvert(SAMGlevelmat *, integer);
integer DSYMSPDAMGconvert(DAMGlevelmat *, integer);
integer CHERHPDAMGconvert(CAMGlevelmat *, integer);
integer ZHERHPDAMGconvert(ZAMGlevelmat *, integer);



void SSYMpiluclsol(integer *,real *,real *,real *, integer *);
void SSYMpilucusol(integer *,real *,real *,real *, integer *);
void DSYMpiluclsol(integer *,doubleprecision *,doubleprecision *,doubleprecision *,
		   integer *);
void DSYMpilucusol(integer *,doubleprecision *,doubleprecision *,doubleprecision *,
		   integer *);
void CSYMpiluclsol(integer *,complex *,complex *,complex *, integer *);
void CSYMpilucusol(integer *,complex *,complex *,complex *, integer *);
void CHERpiluclsol(integer *,complex *,complex *,complex *, integer *);
void CHERpilucusol(integer *,complex *,complex *,complex *, integer *);
void ZSYMpiluclsol(integer *,doublecomplex *,doublecomplex *,doublecomplex *, integer *);
void ZSYMpilucusol(integer *,doublecomplex *,doublecomplex *,doublecomplex *, integer *);
void ZHERpiluclsol(integer *,doublecomplex *,doublecomplex *,doublecomplex *, integer *);
void ZHERpilucusol(integer *,doublecomplex *,doublecomplex *,doublecomplex *, integer *);

void sprivatesptrs(character *uplo, integer *n, integer *nrhs, 
		   real *ap, integer *ipiv, real *b, 
		   integer *ldb, integer *info, ftnlen uplolen);
void dprivatesptrs(character *uplo, integer *n, integer *nrhs, 
		   doubleprecision *ap, integer *ipiv, 
		   doubleprecision *b, integer *ldb, 
		   integer *info, ftnlen uplolen);
void cprivatehptrs(character *uplo, integer *n, integer *nrhs, 
		   complex *ap, integer *ipiv, complex *b, 
		   integer *ldb, integer *info, ftnlen uplolen);
void zprivatehptrs(character *uplo, integer *n, integer *nrhs, 
		   doublecomplex *ap, integer *ipiv, 
		   doublecomplex *b, integer *ldb, integer *info,
		   ftnlen uplolen);

real          mysdot (integer *n, real *x,          integer *ix, real *y,          integer *iy);
double        myddot (integer *n, double *x,        integer *ix, double *y,        integer *iy);
complex       mycdotc(integer *n, complex *x,       integer *ix, complex *y,       integer *iy);
doublecomplex myzdotc(integer *n, doublecomplex *x, integer *ix, doublecomplex *y, integer *iy);


doubleprecision iprandom();
void   ipsrandom(unsigned integer *);

/* global variable to measure the timings of the components within ILUPACK */
#define ILUPACK_secnds_length   10
#define ILUPACK_mem_length      20
#ifdef _ILUPACK_DEFINE_GLOBALS_
    doubleprecision ILUPACK_secnds[ILUPACK_secnds_length];
    size_t ILUPACK_mem[ILUPACK_mem_length];
#else
    extern doubleprecision ILUPACK_secnds[ILUPACK_secnds_length];
    extern size_t ILUPACK_mem[ILUPACK_mem_length];
#endif
/* ILUPACK_mem[0]      maxmimum logical size of ibuff             (integer)
   ILUPACK_mem[1]      maxmimum logical size of dbuff             (FLOAT)

   ILUPACK_mem[2]      final logical size of jlu                  (integer)
   ILUPACK_mem[3]      final logical size of alu                  (FLOAT)

   ILUPACK_mem[4]      maximum (peek) logical size of jlu         (integer)
   ILUPACK_mem[5]      maximum (peek) logical size of alu         (FLOAT)

   ILUPACK_mem[6]      accumulated memory for E,F                 (integer)
   ILUPACK_mem[7]      accumulated memory for E,F                 (FLOAT)

   ILUPACK_mem[8]      accumulated memory for scaling/permutation (integer)
   ILUPACK_mem[9]      accumulated memory for scaling/permutation (FLOAT)

   ILUPACK_mem[10]     maximum temporary memory for external 
                       preprocessing drivers, as far as known     (integer)
   ILUPACK_mem[11]     maximum temporary memory for external 
                       preprocessing drivers, as far as known     (FLOAT)

   ILUPACK_mem[12]     jlu used as auxilliary memory              (integer)
   ILUPACK_mem[13]     alu used as auxilliary memory              (FLOAT)
*/

#endif /* _ILU_PACK_H */





