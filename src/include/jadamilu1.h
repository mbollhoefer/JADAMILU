#ifndef _JADAMILU_H
#define _JADAMILU_H


#include "long_integer.h"
#include "namesjadamilu.h"



#define LONG_INT integer
#define MEDIUM_INT int


_CPP_PREFIX void dpjd(integer *n, doubleprecision *a, integer *ja, integer *ia, 
		      doubleprecision *eigs, doubleprecision *res, doubleprecision *x, 
		      integer *lx, integer *neig, doubleprecision *sigma, 
		      integer *isearch, integer *ninit, integer *madspace, 
		      integer *iter, doubleprecision *tol, doubleprecision *shift, 
		      doubleprecision *droptol, doubleprecision *mem, integer *icntl, 
		      integer *iprint, integer *info, doubleprecision *gap);
_CPP_PREFIX void dpjdrevcom(integer *n, doubleprecision *a, integer *ja, integer *ia, 
			    doubleprecision *eigs, doubleprecision *res, doubleprecision *x, 
			    integer *lx, integer *neig, doubleprecision *sigma, 
			    integer *isearch, integer *ninit, integer *madspace, 
			    integer *iter, doubleprecision *tol, doubleprecision *shift, 
			    doubleprecision *droptol, doubleprecision *mem, integer *icntl,
			    integer *ijob, integer *ndx1, integer *ndx2, 
			    integer *iprint, integer *info, doubleprecision *gap);
_CPP_PREFIX void dpjdcleanup(void);
_CPP_PREFIX void djdrevcom(integer *n, doubleprecision *eigs, doubleprecision *res, 
			   doubleprecision *x, integer *lx, integer *neig, 
			   doubleprecision *sigma, integer *isearch, integer *ninit,
			   integer *madspace, integer *iter, doubleprecision *tol, 
			   integer *ijob, integer *ndx1, integer *ndx2, 
			   integer *iprint, integer *info, doubleprecision *gap);
_CPP_PREFIX void dpjd_gep(integer *n, doubleprecision *a, integer *ja, integer *ia,
			  doubleprecision *b, integer *jb, integer *ib, 
			  doubleprecision *eigs, doubleprecision *res, doubleprecision *x, 
			  integer *lx, integer *neig, doubleprecision *sigma, 
			  integer *isearch, integer *ninit, integer *madspace,
			  integer *iter, doubleprecision *tol, doubleprecision *shift, 
			  doubleprecision *droptol, doubleprecision *mem, integer *icntl,
			  integer *iprint, integer *info, doubleprecision *gap);
_CPP_PREFIX void dpjdrevcom_gep(integer *n, doubleprecision *a, integer *ja, integer *ia,
			  doubleprecision *b, integer *jb, integer *ib,
			  doubleprecision *eigs, doubleprecision *res, doubleprecision *x,
			  integer *lx, integer *neig, doubleprecision *sigma,
			  integer *isearch, integer *ninit, integer *madspace,
			  integer *iter, doubleprecision *tol, doubleprecision *shift,
			  doubleprecision *droptol, doubleprecision *mem, integer *icntl,
			  integer *ijob, integer *ndx1, integer *ndx2,
			  integer *ndx3, integer *iprint, integer *info,
			  doubleprecision *gap);
_CPP_PREFIX void djdrevcom_gep(integer *n, doubleprecision *eigs, doubleprecision *res,
			 doubleprecision *x, integer *lx, integer *neig,
			 doubleprecision *sigma, integer *isearch, integer *ninit,
			 integer *madspace, integer *iter, doubleprecision *tol,
			 integer *ijob, integer *ndx1, integer *ndx2,
			 integer *ndx3, integer *iprint, integer *info,
			 doubleprecision *gap);

_CPP_PREFIX void zpjd(integer *n, doublecomplex *a, integer *ja, integer *ia, 
		      doubleprecision *eigs, doubleprecision *res, doublecomplex *x, 
		      integer *lx, integer *neig, doubleprecision *sigma, 
		      integer *isearch, integer *ninit, integer *madspace, 
		      integer *iter, doubleprecision *tol, doubleprecision *shift, 
		      doubleprecision *droptol, doubleprecision *mem, integer *icntl, 
		      integer *iprint, integer *info, doubleprecision *gap);
_CPP_PREFIX void zpjdrevcom(integer *n, doublecomplex *a, integer *ja, 
			    integer *ia, doubleprecision *eigs, doubleprecision *res, 
			    doublecomplex *x, integer *lx, integer *neig, 
			    doubleprecision *sigma, integer *isearch, integer *ninit,
			    integer *madspace, integer *iter, doubleprecision *tol, 
			    doubleprecision *shift, doubleprecision *droptol, 
			    doubleprecision *mem, integer *icntl, integer *ijob, 
			    integer *ndx1, integer *ndx2, integer *iprint, 
			    integer *info, doubleprecision *gap);
_CPP_PREFIX void zpjdcleanup(void);
_CPP_PREFIX void zjdrevcom(integer *n, doubleprecision *eigs, doubleprecision *res, 
			   doublecomplex *x, integer *lx, integer *neig, 
			   doubleprecision *sigma, integer *isearch, integer *ninit,
			   integer *madspace, integer *iter, doubleprecision *tol, 
			   integer *ijob, integer *ndx1, integer *ndx2, 
			   integer *iprint, integer *info, doubleprecision *gap);
_CPP_PREFIX void zpjd_gep(integer *n, doublecomplex *a, integer *ja, integer *ia,
			  doublecomplex *b, integer *jb, integer *ib, 
			  doubleprecision *eigs, doubleprecision *res, doublecomplex *x, 
			  integer *lx, integer *neig, doubleprecision *sigma, 
			  integer *isearch, integer *ninit, integer *madspace, 
			  integer *iter, doubleprecision *tol, doubleprecision *shift, 
			  doubleprecision *droptol, doubleprecision *mem, integer *icntl,
			  integer *iprint, integer *info, doubleprecision *gap);
_CPP_PREFIX void zpjdrevcom_gep(integer *n, doublecomplex *a, integer *ja,
				integer *ia, doublecomplex *b, integer *jb,
				integer *ib, doubleprecision *eigs, doubleprecision *res,
				doublecomplex *x, integer *lx, integer *neig,
				doubleprecision *sigma, integer *isearch,
				integer *ninit, integer *madspace, integer *iter,
				doubleprecision *tol, doubleprecision *shift,
				doubleprecision *droptol, doubleprecision *mem, 
				integer *icntl, integer *ijob, integer *ndx1,
				integer *ndx2, integer *ndx3, integer *iprint,
				integer *info, doubleprecision *gap);
_CPP_PREFIX void zjdrevcom_gep(integer *n, doubleprecision *eigs, doubleprecision *res,
			       doublecomplex *x, integer *lx, integer *neig,
			       doubleprecision *sigma, integer *isearch,
			       integer *ninit, integer *madspace, integer *iter,
			       doubleprecision *tol, integer *ijob, integer *ndx1,
			       integer *ndx2, integer *ndx3, integer *iprint,
			       integer *info, doubleprecision *gap);
#endif /* _JADAMILU_H */
