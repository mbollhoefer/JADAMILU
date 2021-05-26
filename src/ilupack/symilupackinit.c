#include <ilupack.h>
#include <ilupackmacros.h>

// #define PRINT_INFO


#ifdef _DOUBLE_REAL_
#define MYSYMILUPACKINIT     dsymamginit
#define MYSYMAMGINIT         DSYMAMGinit
#define MYSYMAMGGETPARAMS    DSYMAMGgetparams


#elif defined _SINGLE_REAL_
#define MYSYMILUPACKINIT     ssymamginit
#define MYSYMAMGINIT         SSYMAMGinit
#define MYSYMAMGGETPARAMS    SSYMAMGgetparams


#elif defined _SINGLE_COMPLEX_

#ifdef _COMPLEX_SYMMETRIC_
#define MYSYMILUPACKINIT     csymamginit
#define MYSYMAMGINIT         CSYMAMGinit
#define MYSYMAMGGETPARAMS    CSYMAMGgetparams
#else
#define MYSYMILUPACKINIT     cheramginit
#define MYSYMAMGINIT         CHERAMGinit
#define MYSYMAMGGETPARAMS    CHERAMGgetparams
#endif


#else

#ifdef _COMPLEX_SYMMETRIC_
#define MYSYMILUPACKINIT     zsymamginit
#define MYSYMAMGINIT         ZSYMAMGinit
#define MYSYMAMGGETPARAMS    ZSYMAMGgetparams
#else
#define MYSYMILUPACKINIT     zheramginit
#define MYSYMAMGINIT         ZHERAMGinit
#define MYSYMAMGGETPARAMS    ZHERAMGgetparams
#endif
#endif


void MYSYMILUPACKINIT(integer   *n,
		      integer   *ia,
		      integer   *ja,
		      FLOAT     *a,
		      integer   *matching,
		      character *ordering,
		      REALS     *droptol,
		      REALS     *condest,
		      REALS     *restol,
		      integer   *maxit,
		      REALS     *elbow,
		      integer   *lfil,
		      integer   *lfilS,
		      integer   *nrestart) {
  /*
     ILUPACK FORTRAN interface for initialization

     N           size of the system

     ia          pointer         \
     ja          column indices  | of an NxN matrix in sparse row format
     a           numerical values/

     matching    logical switch whether to turn on matching or not

     ordering    string indicating which reordering to apply

     droptol     threshold for ILU

     condest     bound for ||L^{-1}||, ||U^{-1}||

     restol      stopping tolerance for the iterative solver
                 In general this will be the backward error, for
                 the SPD case this will be the relative error in the
                 energy norm

     maxit       maximum number of iteration steps

     elbow       elbow space factor for ILU

     lfil        number of fill elements per column in L resp. rows in U

     lfilS       number of fill elements pre row in in the coarse grid 
                 system (Schur complement)

     nrestart    number of restart steps in the unsymmetric case
  */

  CSRMAT       A;
  ILUPACKPARAM param;
  integer      flags,i;
  REALS        droptols[2];

  A.nr=A.nc=*n;
  A.ia=ia;
  A.ja=ja;
  A.a =a;
  MYSYMAMGINIT(A, &param);

  // modify the default settings
  MYSYMAMGGETPARAMS(param, &flags, elbow, droptols, condest,
		    restol, maxit);
  *droptol=droptols[1];
  *nrestart=0;

  *matching=1;
  ordering[0]='a';
  ordering[1]='m';
  ordering[2]='d';
  ordering[3]='\0';
  ordering[4]='\0';
  ordering[5]='\0';


  *lfil =param.ipar[3];
  *lfilS=param.ipar[9];


} // ILUPACKINIT
