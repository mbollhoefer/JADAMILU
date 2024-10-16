#ifndef _NAMESBLAS_H
#define _NAMESBLAS_H

#include "f2c.h"


/* on several architectures names of fortran routines are passed to C in 
   different ways. To cover this different architectures use in C only lower
   letters for the fortran names. Dependent on the switch you use they are
   replaced by the correct function name
*/

/* only use capital letters */
#if defined __CAPS__ && !defined __UNDERSCORE__ && !defined __2UNDERSCORES__
#define caxpy           CAXPY
#define ccopy           CCOPY
#ifndef _USE_MKL_
#define cdotc           CDOTC
#define cdotu           CDOTU
#else
#define cdotc           cblas_cdotc_sub
#define cdotu           cblas_cdotu_sub
#endif
#define cgbmv           CGBMV
#define cgemm           CGEMM
#define cgemv           CGEMV
#define cgerc           CGERC
#define cgeru           CGERU
#define chbmv           CHBMV
#define chemm           CHEMM
#define chemv           CHEMV
#define cher            CHER
#define cher2           CHER2
#define cher2k          CHER2K
#define cherk           CHERK
#define chpmv           CHPMV
#define chpr            CHPR
#define chpr2           CHPR2
#define crotg           CROTG
#define cscal           CSCAL
#define csscal          CSSCAL
#define cswap           CSWAP
#define csymm           CSYMM
#define csyr2k          CSYR2K
#define csyrk           CSYRK
#define ctbmv           CTBMV
#define ctbsv           CTBSV
#define ctpmv           CTPMV
#define ctpsv           CTPSV
#define ctrmm           CTRMM
#define ctrmv           CTRMV
#define ctrsm           CTRSM
#define ctrsv           CTRSV

#define dasum           DASUM
#define daxpy           DAXPY
#define dcabs1          DCABS1
#define dcopy           DCOPY
#define ddot            DDOT
#define dgbmv           DGBMV
#define dgemm           DGEMM
#define dgemv           DGEMV
#define dger            DGER
#define dnrm2           DNRM2
#define drot            DROT
#define drotg           DROTG
#define dsbmv           DSBMV
#define dscal           DSCAL
#define dspmv           DSPMV
#define dspr            DSPR
#define dspr2           DSPR2
#define dswap           DSWAP
#define dsymm           DSYMM
#define dsymv           DSYMV
#define dsyr            DSYR
#define dsyr2           DSYR2
#define dsyr2k          DSYR2K
#define dsyrk           DSYRK
#define dtbmv           DTBMV
#define dtbsv           DTBSV
#define dtpmv           DTPMV
#define dtpsv           DTPSV
#define dtrmm           DTRMM
#define dtrmv           DTRMV
#define dtrsm           DTRSM
#define dtrsv           DTRSV
#define dzasum          DZASUM
#define dznrm2          DZNRM2

#define sasum           SASUM
#define sgemm           SGEMM
#define sscal           SSCAL
#define ssyr            SSYR
#define stpsv           STPSV
#define saxpy           SAXPY 
#define sgemv           SGEMV 
#define sspmv           SSPMV    
#define ssyr2           SSYR2  
#define strmm           STRMM   
#define scasum          SCASUM    
#define sger            SGER    
#define sspr            SSPR
#define ssyr2k          SSYR2K
#define strmv           STRMV  
#define scnrm2          SCNRM2    
#define snrm2           SNRM2   
#define sspr2           SSPR2   
#define ssyrk           SSYRK   
#define strsm           STRSM   
#define scopy           SCOPY   
#define srot            SROT  
#define sswap           SSWAP    
#define stbmv           STBMV
#define strsv           STRSV
#define sdot            SDOT
#define srotg           SROTG
#define ssymm           SSYMM
#define stbsv           STBSV 
#define sgbmv           SGBMV   
#define ssbmv           SSBMV
#define ssymv           SSYMV  
#define stpmv           STPMV

#define zaxpy           ZAXPY
#define zgemv           ZGEMV
#define zher2           ZHER2
#define zscal           ZSCAL
#define ztpmv           ZTPMV
#define zcopy           ZCOPY
#define zgerc           ZGERC
#define zher2k          ZHER2K
#define zswap           ZSWAP
#define ztpsv           ZTPSV
#ifndef _USE_MKL_
#define zdotc           ZDOTC
#define zdotu           ZDOTU
#else
#define zdotc           cblas_zdotc_sub
#define zdotu           cblas_zdotu_sub
#endif
#define zgeru           ZGERU
#define zherk           ZHERK
#define zsymm           ZSYMM
#define ztrmm           ZTRMM
#define zhbmv           ZHBMV
#define zhpmv           ZHPMV
#define zsyr2k          ZSYR2K
#define ztrmv           ZTRMV
#define zdscal          ZDSCAL
#define zhemm           ZHEMM
#define zhpr            ZHPR
#define zsyrk           ZSYRK
#define ztrsm           ZTRSM
#define zgbmv           ZGBMV
#define zhemv           ZHEMV
#define zhpr2           ZHPR2 
#define ztbmv           ZTBMV
#define ztrsv           ZTRSV
#define zgemm           ZGEMM
#define zher            ZHER
#define zrot            ZROT
#define zrotg           ZROTG
#define ztbsv           ZTBSV

#define icamax          ICAMAX
#define idamax          IDAMAX
#define isamax          ISAMAX
#define izamax          IZAMAX

#define lsame           LSAME
#define xerbla          XERBLA


/* no capital letters */
#elif defined __UNDERSCORE__ && !defined __CAPS__ && !defined __2UNDERSCORES__
#define caxpy           caxpy_
#define ccopy           ccopy_
#ifndef _USE_MKL_
#define cdotc           cdotc_
#define cdotu           cdotu_
#else
#define cdotc           cblas_cdotc_sub
#define cdotu           cblas_cdotu_sub
#endif
#define cgbmv           cgbmv_
#define cgemm           cgemm_
#define cgemv           cgemv_
#define cgerc           cgerc_
#define cgeru           cgeru_
#define chbmv           chbmv_
#define chemm           chemm_
#define chemv           chemv_
#define cher            cher_
#define cher2           cher2_
#define cher2k          cher2k_
#define cherk           cherk_
#define chpmv           chpmv_
#define chpr            chpr_
#define chpr2           chpr2_
#define crotg           crotg_
#define cscal           cscal_
#define csscal          csscal_
#define cswap           cswap_
#define csymm           csymm_
#define csyr2k          csyr2k_
#define csyrk           csyrk_
#define ctbmv           ctbmv_
#define ctbsv           ctbsv_
#define ctpmv           ctpmv_
#define ctpsv           ctpsv_
#define ctrmm           ctrmm_
#define ctrmv           ctrmv_
#define ctrsm           ctrsm_
#define ctrsv           ctrsv_

#define dasum           dasum_
#define daxpy           daxpy_
#define dcabs1          dcabs1_
#define dcopy           dcopy_
#define ddot            ddot_
#define dgbmv           dgbmv_
#define dgemm           dgemm_
#define dgemv           dgemv_
#define dger            dger_
#define dnrm2           dnrm2_
#define drot            drot_
#define drotg           drotg_
#define dsbmv           dsbmv_
#define dscal           dscal_
#define dspmv           dspmv_
#define dspr            dspr_
#define dspr2           dspr2_
#define dswap           dswap_
#define dsymm           dsymm_
#define dsymv           dsymv_
#define dsyr            dsyr_
#define dsyr2           dsyr2_
#define dsyr2k          dsyr2k_
#define dsyrk           dsyrk_
#define dtbmv           dtbmv_
#define dtbsv           dtbsv_
#define dtpmv           dtpmv_
#define dtpsv           dtpsv_
#define dtrmm           dtrmm_
#define dtrmv           dtrmv_
#define dtrsm           dtrsm_
#define dtrsv           dtrsv_
#define dzasum          dzasum_
#define dznrm2          dznrm2_

#define sasum           sasum_
#define sgemm           sgemm_
#define sscal           sscal_
#define ssyr            ssyr_
#define stpsv           stpsv_
#define saxpy           saxpy_
#define sgemv           sgemv_
#define sspmv           sspmv_    
#define ssyr2           ssyr2_ 
#define strmm           strmm_ 
#define scasum          scasum_  
#define sger            sger_   
#define sspr            sspr_
#define ssyr2k          ssyr2k_
#define strmv           strmv_
#define scnrm2          scnrm2_    
#define snrm2           snrm2_ 
#define sspr2           sspr2_  
#define ssyrk           ssyrk_  
#define strsm           strsm_  
#define scopy           scopy_  
#define srot            srot_
#define sswap           sswap_    
#define stbmv           stbmv_
#define strsv           strsv_
#define sdot            sdot_
#define srotg           srotg_
#define ssymm           ssymm_
#define stbsv           stbsv_
#define sgbmv           sgbmv_   
#define ssbmv           ssbmv_
#define ssymv           ssymv_ 
#define stpmv           stpmv_

#define zaxpy           zaxpy_
#define zgemv           zgemv_
#define zher2           zher2_
#define zscal           zscal_
#define ztpmv           ztpmv_
#define zcopy           zcopy_
#define zgerc           zgerc_
#define zher2k          zher2k_
#define zswap           zswap_
#define ztpsv           ztpsv_
#ifndef _USE_MKL_
#define zdotc           zdotc_
#define zdotu           zdotu_
#else
#define zdotc           cblas_zdotc_sub
#define zdotu           cblas_zdotu_sub
#endif
#define zgeru           zgeru_
#define zherk           zherk_
#define zsymm           zsymm_
#define ztrmm           ztrmm_
#define zhbmv           zhbmv_
#define zhpmv           zhpmv_
#define zsyr2k          zsyr2k_
#define ztrmv           ztrmv_
#define zdscal          zdscal_
#define zhemm           zhemm_
#define zhpr            zhpr_
#define zsyrk           zsyrk_
#define ztrsm           ztrsm_
#define zgbmv           zgbmv_
#define zhemv           zhemv_
#define zhpr2           zhpr2_
#define ztbmv           ztbmv_
#define ztrsv           ztrsv_
#define zgemm           zgemm_
#define zher            zher_
#define zrot            zrot_
#define zrotg           zrotg_
#define ztbsv           ztbsv_

#define icamax          icamax_
#define idamax          idamax_
#define isamax          isamax_
#define izamax          izamax_

#define lsame           lsame_
#define xerbla          xerbla_

/* both are defined */
#elif defined __CAPS__ && defined __UNDERSCORE__ && !defined __2UNDERSCORES__
#define caxpy           CAXPY_
#define ccopy           CCOPY_
#ifndef _USE_MKL_
#define cdotc           CDOTC_
#define cdotu           CDOTU_
#else
#define cdotc           cblas_cdotc_sub
#define cdotu           cblas_cdotu_sub
#endif
#define cgbmv           CGBMV_
#define cgemm           CGEMM_
#define cgemv           CGEMV_
#define cgerc           CGERC_
#define cgeru           CGERU_
#define chbmv           CHBMV_
#define chemm           CHEMM_
#define chemv           CHEMV_
#define cher            CHER_
#define cher2           CHER2_
#define cher2k          CHER2K_
#define cherk           CHERK_
#define chpmv           CHPMV_
#define chpr            CHPR_
#define chpr2           CHPR2_
#define crotg           CROTG_
#define cscal           CSCAL_
#define csscal          CSSCAL_
#define cswap           CSWAP_
#define csymm           CSYMM_
#define csyr2k          CSYR2K_
#define csyrk           CSYRK_
#define ctbmv           CTBMV_
#define ctbsv           CTBSV_
#define ctpmv           CTPMV_
#define ctpsv           CTPSV_
#define ctrmm           CTRMM_
#define ctrmv           CTRMV_
#define ctrsm           CTRSM_
#define ctrsv           CTRSV_

#define dasum           DASUM_
#define daxpy           DAXPY_
#define dcabs1          DCABS1_
#define dcopy           DCOPY_
#define ddot            DDOT_
#define dgbmv           DGBMV_
#define dgemm           DGEMM_
#define dgemv           DGEMV_
#define dger            DGER_
#define dnrm2           DNRM2_
#define drot            DROT_
#define drotg           DROTG_
#define dsbmv           DSBMV_
#define dscal           DSCAL_
#define dspmv           DSPMV_
#define dspr            DSPR_
#define dspr2           DSPR2_
#define dswap           DSWAP_
#define dsymm           DSYMM_
#define dsymv           DSYMV_
#define dsyr            DSYR_
#define dsyr2           DSYR2_
#define dsyr2k          DSYR2K_
#define dsyrk           DSYRK_
#define dtbmv           DTBMV_
#define dtbsv           DTBSV_
#define dtpmv           DTPMV_
#define dtpsv           DTPSV_
#define dtrmm           DTRMM_
#define dtrmv           DTRMV_
#define dtrsm           DTRSM_
#define dtrsv           DTRSV_
#define dzasum          DZASUM_
#define dznrm2          DZNRM2_

#define sasum           SASUM_
#define sgemm           SGEMM_
#define sscal           SSCAL_
#define ssyr            SSYR_
#define stpsv           STPSV_
#define saxpy           SAXPY_
#define sgemv           SGEMV_
#define sspmv           SSPMV_  
#define ssyr2           SSYR2_
#define strmm           STRMM_
#define scasum          SCASUM_ 
#define sger            SGER_
#define sspr            SSPR_
#define ssyr2k          SSYR2K_
#define strmv           STRMV_
#define scnrm2          SCNRM2_  
#define snrm2           SNRM2_
#define sspr2           SSPR2_
#define ssyrk           SSYRK_
#define strsm           STRSM_
#define scopy           SCOPY_
#define srot            SROT_
#define sswap           SSWAP_  
#define stbmv           STBMV_
#define strsv           STRSV_
#define sdot            SDOT_
#define srotg           SROTG_
#define ssymm           SSYMM_
#define stbsv           STBSV_
#define sgbmv           SGBMV_ 
#define ssbmv           SSBMV_
#define ssymv           SSYMV_ 
#define stpmv           STPMV_

#define zaxpy           ZAXPY_
#define zgemv           ZGEMV_
#define zher2           ZHER2_
#define zscal           ZSCAL_
#define ztpmv           ZTPMV_
#define zcopy           ZCOPY_
#define zgerc           ZGERC_
#define zher2k          ZHER2K_
#define zswap           ZSWAP_
#define ztpsv           ZTPSV_
#ifndef _USE_MKL_
#define zdotc           ZDOTC_
#define zdotu           ZDOTU_
#else
#define zdotc           cblas_zdotc_sub
#define zdotu           cblas_zdotu_sub
#endif
#define zgeru           ZGERU_
#define zherk           ZHERK_
#define zsymm           ZSYMM_
#define ztrmm           ZTRMM_
#define zhbmv           ZHBMV_
#define zhpmv           ZHPMV_
#define zsyr2k          ZSYR2K_
#define ztrmv           ZTRMV_
#define zdscal          ZDSCAL_
#define zhemm           ZHEMM_
#define zhpr            ZHPR_
#define zsyrk           ZSYRK_
#define ztrsm           ZTRSM_
#define zgbmv           ZGBMV_
#define zhemv           ZHEMV_
#define zhpr2           ZHPR2_
#define ztbmv           ZTBMV_
#define ztrsv           ZTRSV_
#define zgemm           ZGEMM_
#define zher            ZHER_
#define zrot            ZROT_
#define zrotg           ZROTG_
#define ztbsv           ZTBSV_

#define icamax          ICAMAX_
#define idamax          IDAMAX_
#define isamax          ISAMAX_
#define izamax          IZAMAX_

#define lsame           LSAME_
#define xerbla          XERBLA_

/* CAPS and 2 underscores are defined */
#elif defined __CAPS__ && defined __2UNDERSCORES__
#define caxpy           CAXPY__
#define ccopy           CCOPY__
#ifndef _USE_MKL_
#define cdotc           CDOTC__
#define cdotu           CDOTU__
#else
#define cdotc           cblas_cdotc_sub
#define cdotu           cblas_cdotu_sub
#endif
#define cgbmv           CGBMV__
#define cgemm           CGEMM__
#define cgemv           CGEMV__
#define cgerc           CGERC__
#define cgeru           CGERU__
#define chbmv           CHBMV__
#define chemm           CHEMM__
#define chemv           CHEMV__
#define cher            CHER__
#define cher2           CHER2__
#define cher2k          CHER2K__
#define cherk           CHERK__
#define chpmv           CHPMV__
#define chpr            CHPR__
#define chpr2           CHPR2__
#define crotg           CROTG__
#define cscal           CSCAL__
#define csscal          CSSCAL__
#define cswap           CSWAP__
#define csymm           CSYMM__
#define csyr2k          CSYR2K__
#define csyrk           CSYRK__
#define ctbmv           CTBMV__
#define ctbsv           CTBSV__
#define ctpmv           CTPMV__
#define ctpsv           CTPSV__
#define ctrmm           CTRMM__
#define ctrmv           CTRMV__
#define ctrsm           CTRSM__
#define ctrsv           CTRSV__

#define dasum           DASUM__
#define daxpy           DAXPY__
#define dcabs1          DCABS1__
#define dcopy           DCOPY__
#define ddot            DDOT__
#define dgbmv           DGBMV__
#define dgemm           DGEMM__
#define dgemv           DGEMV__
#define dger            DGER__
#define dnrm2           DNRM2__
#define drot            DROT__
#define drotg           DROTG__
#define dsbmv           DSBMV__
#define dscal           DSCAL__
#define dspmv           DSPMV__
#define dspr            DSPR__
#define dspr2           DSPR2__
#define dswap           DSWAP__
#define dsymm           DSYMM__
#define dsymv           DSYMV__
#define dsyr            DSYR__
#define dsyr2           DSYR2__
#define dsyr2k          DSYR2K__
#define dsyrk           DSYRK__
#define dtbmv           DTBMV__
#define dtbsv           DTBSV__
#define dtpmv           DTPMV__
#define dtpsv           DTPSV__
#define dtrmm           DTRMM__
#define dtrmv           DTRMV__
#define dtrsm           DTRSM__
#define dtrsv           DTRSV__
#define dzasum          DZASUM__
#define dznrm2          DZNRM2__

#define sasum           SASUM__
#define sgemm           SGEMM__
#define sscal           SSCAL__
#define ssyr            SSYR__
#define stpsv           STPSV__
#define saxpy           SAXPY__
#define sgemv           SGEMV__
#define sspmv           SSPMV__  
#define ssyr2           SSYR2__
#define strmm           STRMM__
#define scasum          SCASUM__ 
#define sger            SGER__
#define sspr            SSPR__
#define ssyr2k          SSYR2K__
#define strmv           STRMV__
#define scnrm2          SCNRM2__  
#define snrm2           SNRM2__
#define sspr2           SSPR2__
#define ssyrk           SSYRK__
#define strsm           STRSM__
#define scopy           SCOPY__
#define srot            SROT__
#define sswap           SSWAP__  
#define stbmv           STBMV__
#define strsv           STRSV__
#define sdot            SDOT__
#define srotg           SROTG__
#define ssymm           SSYMM__
#define stbsv           STBSV__
#define sgbmv           SGBMV__ 
#define ssbmv           SSBMV__
#define ssymv           SSYMV__ 
#define stpmv           STPMV__

#define zaxpy           ZAXPY__
#define zgemv           ZGEMV__
#define zher2           ZHER2__
#define zscal           ZSCAL__
#define ztpmv           ZTPMV__
#define zcopy           ZCOPY__
#define zgerc           ZGERC__
#define zher2k          ZHER2K__
#define zswap           ZSWAP__
#define ztpsv           ZTPSV__
#ifndef _USE_MKL_
#define zdotc           ZDOTC__
#define zdotu           ZDOTU__
#else
#define zdotc           cblas_zdotc_sub
#define zdotu           cblas_zdotu_sub
#endif
#define zgeru           ZGERU__
#define zherk           ZHERK__
#define zsymm           ZSYMM__
#define ztrmm           ZTRMM__
#define zhbmv           ZHBMV__
#define zhpmv           ZHPMV__
#define zsyr2k          ZSYR2K__
#define ztrmv           ZTRMV__
#define zdscal          ZDSCAL__
#define zhemm           ZHEMM__
#define zhpr            ZHPR__
#define zsyrk           ZSYRK__
#define ztrsm           ZTRSM__
#define zgbmv           ZGBMV__
#define zhemv           ZHEMV__
#define zhpr2           ZHPR2__
#define ztbmv           ZTBMV__
#define ztrsv           ZTRSV__
#define zgemm           ZGEMM__
#define zher            ZHER__
#define zrot            ZROT__
#define zrotg           ZROTG__
#define ztbsv           ZTBSV__

#define icamax          ICAMAX__
#define idamax          IDAMAX__
#define isamax          ISAMAX__
#define izamax          IZAMAX__

#define lsame           LSAME__
#define xerbla          XERBLA__


/* no capital letters but 2 underscores */
#elif defined __2UNDERSCORES__ && !defined __CAPS__
#define caxpy           caxpy__
#define ccopy           ccopy__
#ifndef _USE_MKL_
#define cdotc           cdotc__
#define cdotu           cdotu__
#else
#define cdotc           cblas_cdotc_sub
#define cdotu           cblas_cdotu_sub
#endif
#define cgbmv           cgbmv__
#define cgemm           cgemm__
#define cgemv           cgemv__
#define cgerc           cgerc__
#define cgeru           cgeru__
#define chbmv           chbmv__
#define chemm           chemm__
#define chemv           chemv__
#define cher            cher__
#define cher2           cher2__
#define cher2k          cher2k__
#define cherk           cherk__
#define chpmv           chpmv__
#define chpr            chpr__
#define chpr2           chpr2__
#define crotg           crotg__
#define cscal           cscal__
#define csscal          csscal__
#define cswap           cswap__
#define csymm           csymm__
#define csyr2k          csyr2k__
#define csyrk           csyrk__
#define ctbmv           ctbmv__
#define ctbsv           ctbsv__
#define ctpmv           ctpmv__
#define ctpsv           ctpsv__
#define ctrmm           ctrmm__
#define ctrmv           ctrmv__
#define ctrsm           ctrsm__
#define ctrsv           ctrsv__

#define dasum           dasum__
#define daxpy           daxpy__
#define dcabs1          dcabs1__
#define dcopy           dcopy__
#define ddot            ddot__
#define dgbmv           dgbmv__
#define dgemm           dgemm__
#define dgemv           dgemv__
#define dger            dger__
#define dnrm2           dnrm2__
#define drot            drot__
#define drotg           drotg__
#define dsbmv           dsbmv__
#define dscal           dscal__
#define dspmv           dspmv__
#define dspr            dspr__
#define dspr2           dspr2__
#define dswap           dswap__
#define dsymm           dsymm__
#define dsymv           dsymv__
#define dsyr            dsyr__
#define dsyr2           dsyr2__
#define dsyr2k          dsyr2k__
#define dsyrk           dsyrk__
#define dtbmv           dtbmv__
#define dtbsv           dtbsv__
#define dtpmv           dtpmv__
#define dtpsv           dtpsv__
#define dtrmm           dtrmm__
#define dtrmv           dtrmv__
#define dtrsm           dtrsm__
#define dtrsv           dtrsv__
#define dzasum          dzasum__
#define dznrm2          dznrm2__

#define sasum           sasum__
#define sgemm           sgemm__
#define sscal           sscal__
#define ssyr            ssyr__
#define stpsv           stpsv__
#define saxpy           saxpy__
#define sgemv           sgemv__
#define sspmv           sspmv__    
#define ssyr2           ssyr2__ 
#define strmm           strmm__ 
#define scasum          scasum__  
#define sger            sger__   
#define sspr            sspr__
#define ssyr2k          ssyr2k__
#define strmv           strmv__
#define scnrm2          scnrm2__    
#define snrm2           snrm2__ 
#define sspr2           sspr2__  
#define ssyrk           ssyrk__  
#define strsm           strsm__  
#define scopy           scopy__  
#define srot            srot__
#define sswap           sswap__    
#define stbmv           stbmv__
#define strsv           strsv__
#define sdot            sdot__
#define srotg           srotg__
#define ssymm           ssymm__
#define stbsv           stbsv__
#define sgbmv           sgbmv__   
#define ssbmv           ssbmv__
#define ssymv           ssymv__ 
#define stpmv           stpmv__

#define zaxpy           zaxpy__
#define zgemv           zgemv__
#define zher2           zher2__
#define zscal           zscal__
#define ztpmv           ztpmv__
#define zcopy           zcopy__
#define zgerc           zgerc__
#define zher2k          zher2k__
#define zswap           zswap__
#define ztpsv           ztpsv__
#ifndef _USE_MKL_
#define zdotc           zdotc__
#define zdotu           zdotu__
#else
#define zdotc           cblas_zdotc_sub
#define zdotu           cblas_zdotu_sub
#endif
#define zgeru           zgeru__
#define zherk           zherk__
#define zsymm           zsymm__
#define ztrmm           ztrmm__
#define zhbmv           zhbmv__
#define zhpmv           zhpmv__
#define zsyr2k          zsyr2k__
#define ztrmv           ztrmv__
#define zdscal          zdscal__
#define zhemm           zhemm__
#define zhpr            zhpr__
#define zsyrk           zsyrk__
#define ztrsm           ztrsm__
#define zgbmv           zgbmv__
#define zhemv           zhemv__
#define zhpr2           zhpr2__
#define ztbmv           ztbmv__
#define ztrsv           ztrsv__
#define zgemm           zgemm__
#define zher            zher__
#define zrot            zrot__
#define zrotg           zrotg__
#define ztbsv           ztbsv__

#define icamax          icamax__
#define idamax          idamax__
#define isamax          isamax__
#define izamax          izamax__

#define lsame           lsame__
#define xerbla          xerbla__

// nothing at all
#else

#define caxpy           caxpy
#define ccopy           ccopy
#ifndef _USE_MKL_
#define cdotc           cdotc
#define cdotu           cdotu
#else
#define cdotc           cblas_cdotc_sub
#define cdotu           cblas_cdotu_sub
#endif
#define cgbmv           cgbmv
#define cgemm           cgemm
#define cgemv           cgemv
#define cgerc           cgerc
#define cgeru           cgeru
#define chbmv           chbmv
#define chemm           chemm
#define chemv           chemv
#define cher            cher
#define cher2           cher2
#define cher2k          cher2k
#define cherk           cherk
#define chpmv           chpmv
#define chpr            chpr
#define chpr2           chpr2
#define crotg           crotg
#define cscal           cscal
#define csscal          csscal
#define cswap           cswap
#define csymm           csymm
#define csyr2k          csyr2k
#define csyrk           csyrk
#define ctbmv           ctbmv
#define ctbsv           ctbsv
#define ctpmv           ctpmv
#define ctpsv           ctpsv
#define ctrmm           ctrmm
#define ctrmv           ctrmv
#define ctrsm           ctrsm
#define ctrsv           ctrsv

#define dasum           dasum
#define daxpy           daxpy
#define dcabs1          dcabs1
#define dcopy           dcopy
#define ddot            ddot
#define dgbmv           dgbmv
#define dgemm           dgemm
#define dgemv           dgemv
#define dger            dger
#define dnrm2           dnrm2
#define drot            drot
#define drotg           drotg
#define dsbmv           dsbmv
#define dscal           dscal
#define dspmv           dspmv
#define dspr            dspr
#define dspr2           dspr2
#define dswap           dswap
#define dsymm           dsymm
#define dsymv           dsymv
#define dsyr            dsyr
#define dsyr2           dsyr2
#define dsyr2k          dsyr2k
#define dsyrk           dsyrk
#define dtbmv           dtbmv
#define dtbsv           dtbsv
#define dtpmv           dtpmv
#define dtpsv           dtpsv
#define dtrmm           dtrmm
#define dtrmv           dtrmv
#define dtrsm           dtrsm
#define dtrsv           dtrsv
#define dzasum          dzasum
#define dznrm2          dznrm2

#define sasum           sasum
#define sgemm           sgemm
#define sscal           sscal
#define ssyr            ssyr
#define stpsv           stpsv
#define saxpy           saxpy
#define sgemv           sgemv
#define sspmv           sspmv    
#define ssyr2           ssyr2 
#define strmm           strmm 
#define scasum          scasum  
#define sger            sger   
#define sspr            sspr
#define ssyr2k          ssyr2k
#define strmv           strmv
#define scnrm2          scnrm2    
#define snrm2           snrm2 
#define sspr2           sspr2  
#define ssyrk           ssyrk  
#define strsm           strsm  
#define scopy           scopy  
#define srot            srot
#define sswap           sswap    
#define stbmv           stbmv
#define strsv           strsv
#define sdot            sdot
#define srotg           srotg
#define ssymm           ssymm
#define stbsv           stbsv
#define sgbmv           sgbmv   
#define ssbmv           ssbmv
#define ssymv           ssymv 
#define stpmv           stpmv

#define zaxpy           zaxpy
#define zgemv           zgemv
#define zher2           zher2
#define zscal           zscal
#define ztpmv           ztpmv
#define zcopy           zcopy
#define zgerc           zgerc
#define zher2k          zher2k
#define zswap           zswap
#define ztpsv           ztpsv
#ifndef _USE_MKL_
#define zdotc           zdotc
#define zdotu           zdotu
#else
#define zdotc           cblas_zdotc_sub
#define zdotu           cblas_zdotu_sub
#endif
#define zgeru           zgeru
#define zherk           zherk
#define zsymm           zsymm
#define ztrmm           ztrmm
#define zhbmv           zhbmv
#define zhpmv           zhpmv
#define zsyr2k          zsyr2k
#define ztrmv           ztrmv
#define zdscal          zdscal
#define zhemm           zhemm
#define zhpr            zhpr
#define zsyrk           zsyrk
#define ztrsm           ztrsm
#define zgbmv           zgbmv
#define zhemv           zhemv
#define zhpr2           zhpr2
#define ztbmv           ztbmv
#define ztrsv           ztrsv
#define zgemm           zgemm
#define zher            zher
#define zrot            zrot
#define zrotg           zrotg
#define ztbsv           ztbsv

#define icamax          icamax
#define idamax          idamax
#define isamax          isamax
#define izamax          izamax

#define lsame           lsame
#define xerbla          xerbla

#endif /* defined __CAPS__ && ... */



#endif /* _NAMESBLAS_H */
