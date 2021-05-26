#ifndef _NAMES_JADAMILU_H
#define _NAMES_JADAMILU_H

#include "f2c.h"


/* on several architectures names of fortran routines are passed to C in 
   different ways. To cover this different architectures use in C only lower
   letters for the fortran names. Dependent on the switch you use they are
   replaced by the correct function name
*/

/* only use capital letters */
#if defined __CAPS__ && !defined __UNDERSCORE__ && !defined __2UNDERSCORES__                                            
                                              
#define spjd                SPJD
#define dpjd                DPJD
#define cpjd                CPJD
#define zpjd                ZPJD

#define spjdrevcom          SPJDREVCOM
#define dpjdrevcom          DPJDREVCOM
#define cpjdrevcom          CPJDREVCOM
#define zpjdrevcom          ZPJDREVCOM

#define sjdrevcom          SJDREVCOM
#define djdrevcom          DJDREVCOM
#define cjdrevcom          CJDREVCOM
#define zjdrevcom          ZJDREVCOM

#define spjd_gep            SPJD_GEP
#define dpjd_gep            DPJD_GEP
#define cpjd_gep            CPJD_GEP
#define zpjd_gep            ZPJD_GEP

#define spjdrevcom_gep      SPJDREVCOM_GEP
#define dpjdrevcom_gep      DPJDREVCOM_GEP
#define cpjdrevcom_gep      CPJDREVCOM_GEP
#define zpjdrevcom_gep      ZPJDREVCOM_GEP

#define sjdrevcom_gep      SJDREVCOM_GEP
#define djdrevcom_gep      DJDREVCOM_GEP
#define cjdrevcom_gep      CJDREVCOM_GEP
#define zjdrevcom_gep      ZJDREVCOM_GEP

#define spjdcleanup         SPJDCLEANUP
#define dpjdcleanup         DPJDCLEANUP
#define cpjdcleanup         CPJDCLEANUP
#define zpjdcleanup         ZPJDCLEANUP


/* no capital letters */
#elif defined __UNDERSCORE__ && !defined __CAPS__ && !defined __2UNDERSCORES__

#define spjd                spjd_
#define dpjd                dpjd_
#define cpjd                cpjd_
#define zpjd                zpjd_

#define spjdrevcom          spjdrevcom_
#define dpjdrevcom          dpjdrevcom_
#define cpjdrevcom          cpjdrevcom_
#define zpjdrevcom          zpjdrevcom_

#define sjdrevcom          sjdrevcom_
#define djdrevcom          djdrevcom_
#define cjdrevcom          cjdrevcom_
#define zjdrevcom          zjdrevcom_

#define spjd_gep            spjd_gep_
#define dpjd_gep            dpjd_gep_
#define cpjd_gep            cpjd_gep_
#define zpjd_gep            zpjd_gep_

#define spjdrevcom_gep      spjdrevcom_gep_
#define dpjdrevcom_gep      dpjdrevcom_gep_
#define cpjdrevcom_gep      cpjdrevcom_gep_
#define zpjdrevcom_gep      zpjdrevcom_gep_

#define sjdrevcom_gep      sjdrevcom_gep_
#define djdrevcom_gep      djdrevcom_gep_
#define cjdrevcom_gep      cjdrevcom_gep_
#define zjdrevcom_gep      zjdrevcom_gep_

#define spjdcleanup         spjdcleanup_
#define dpjdcleanup         dpjdcleanup_
#define cpjdcleanup         cpjdcleanup_
#define zpjdcleanup         zpjdcleanup_


/* both are defined */
#elif defined __CAPS__ && defined __UNDERSCORE__ && !defined __2UNDERSCORES__

#define spjd                SPJD_
#define dpjd                DPJD_
#define cpjd                CPJD_
#define zpjd                ZPJD_

#define spjdrevcom          SPJDREVCOM_
#define dpjdrevcom          DPJDREVCOM_
#define cpjdrevcom          CPJDREVCOM_
#define zpjdrevcom          ZPJDREVCOM_

#define sjdrevcom          SJDREVCOM_
#define djdrevcom          DJDREVCOM_
#define cjdrevcom          CJDREVCOM_
#define zjdrevcom          ZJDREVCOM_

#define spjd_gep            SPJD_GEP_
#define dpjd_gep            DPJD_GEP_
#define cpjd_gep            CPJD_GEP_
#define zpjd_gep            ZPJD_GEP_

#define spjdrevcom_gep      SPJDREVCOM_GEP_
#define dpjdrevcom_gep      DPJDREVCOM_GEP_
#define cpjdrevcom_gep      CPJDREVCOM_GEP_
#define zpjdrevcom_gep      ZPJDREVCOM_GEP_

#define sjdrevcom_gep      SJDREVCOM_GEP_
#define djdrevcom_gep      DJDREVCOM_GEP_
#define cjdrevcom_gep      CJDREVCOM_GEP_
#define zjdrevcom_gep      ZJDREVCOM_GEP_

#define spjdcleanup         SPJDCLEANUP_
#define dpjdcleanup         DPJDCLEANUP_
#define cpjdcleanup         CPJDCLEANUP_
#define zpjdcleanup         ZPJDCLEANUP_


/* CAPS and 2 underscores are defined */
#elif defined __CAPS__ && defined __2UNDERSCORES__

#define spjd                SPJD__
#define dpjd                DPJD__
#define cpjd                CPJD__
#define zpjd                ZPJD__

#define spjdrevcom          SPJDREVCOM__
#define dpjdrevcom          DPJDREVCOM__
#define cpjdrevcom          CPJDREVCOM__
#define zpjdrevcom          ZPJDREVCOM__

#define sjdrevcom          SJDREVCOM__
#define djdrevcom          DJDREVCOM__
#define cjdrevcom          CJDREVCOM__
#define zjdrevcom          ZJDREVCOM__

#define spjd_gep            SPJD_GEP__
#define dpjd_gep            DPJD_GEP__
#define cpjd_gep            CPJD_GEP__
#define zpjd_gep            ZPJD_GEP__

#define spjdrevcom_gep      SPJDREVCOM_GEP__
#define dpjdrevcom_gep      DPJDREVCOM_GEP__
#define cpjdrevcom_gep      CPJDREVCOM_GEP__
#define zpjdrevcom_gep      ZPJDREVCOM_GEP__

#define sjdrevcom_gep      SJDREVCOM_GEP__
#define djdrevcom_gep      DJDREVCOM_GEP__
#define cjdrevcom_gep      CJDREVCOM_GEP__
#define zjdrevcom_gep      ZJDREVCOM_GEP__

#define spjdcleanup         SPJDCLEANUP__
#define dpjdcleanup         DPJDCLEANUP__
#define cpjdcleanup         CPJDCLEANUP__
#define zpjdcleanup         ZPJDCLEANUP__


/* no capital letters but 2 underscores */
#elif defined __2UNDERSCORES__ && !defined __CAPS__

#define spjd                spjd__
#define dpjd                dpjd__
#define cpjd                cpjd__
#define zpjd                zpjd__

#define spjdrevcom          spjdrevcom__
#define dpjdrevcom          dpjdrevcom__
#define cpjdrevcom          cpjdrevcom__
#define zpjdrevcom          zpjdrevcom__

#define sjdrevcom          sjdrevcom__
#define djdrevcom          djdrevcom__
#define cjdrevcom          cjdrevcom__
#define zjdrevcom          zjdrevcom__

#define spjd_gep            spjd_gep__
#define dpjd_gep            dpjd_gep__
#define cpjd_gep            cpjd_gep__
#define zpjd_gep            zpjd_gep__

#define spjdrevcom_gep      spjdrevcom_gep__
#define dpjdrevcom_gep      dpjdrevcom_gep__
#define cpjdrevcom_gep      cpjdrevcom_gep__
#define zpjdrevcom_gep      zpjdrevcom_gep__

#define sjdrevcom_gep      sjdrevcom_gep__
#define djdrevcom_gep      djdrevcom_gep__
#define cjdrevcom_gep      cjdrevcom_gep__
#define zjdrevcom_gep      zjdrevcom_gep__

#define spjdcleanup         spjdcleanup__
#define dpjdcleanup         dpjdcleanup__
#define cpjdcleanup         cpjdcleanup__
#define zpjdcleanup         zpjdcleanup__


// no switch defined use lower case letters in FORTRAN
#else

#define spjd                spjd
#define dpjd                dpjd
#define cpjd                cpjd
#define zpjd                zpjd

#define spjdrevcom          spjdrevcom
#define dpjdrevcom          dpjdrevcom
#define cpjdrevcom          cpjdrevcom
#define zpjdrevcom          zpjdrevcom

#define sjdrevcom          sjdrevcom
#define djdrevcom          djdrevcom
#define cjdrevcom          cjdrevcom
#define zjdrevcom          zjdrevcom

#define spjd_gep            spjd_gep
#define dpjd_gep            dpjd_gep
#define cpjd_gep            cpjd_gep
#define zpjd_gep            zpjd_gep

#define spjdrevcom_gep      spjdrevcom_gep
#define dpjdrevcom_gep      dpjdrevcom_gep
#define cpjdrevcom_gep      cpjdrevcom_gep
#define zpjdrevcom_gep      zpjdrevcom_gep

#define sjdrevcom_gep      sjdrevcom_gep
#define djdrevcom_gep      djdrevcom_gep
#define cjdrevcom_gep      cjdrevcom_gep
#define zjdrevcom_gep      zjdrevcom_gep

#define spjdcleanup         spjdcleanup
#define dpjdcleanup         dpjdcleanup
#define cpjdcleanup         cpjdcleanup
#define zpjdcleanup         zpjdcleanup


#endif /* defined __CAPS__ && ... */


#endif /* _NAMES_JADAMILU_H */
