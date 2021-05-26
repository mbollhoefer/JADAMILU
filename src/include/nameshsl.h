#ifndef _NAMESHSL_H
#define _NAMESHSL_H

#include "f2c.h"

/* on several architectures names of fortran routines are passed to C in 
   different ways. To cover this different architectures use in C only lower
   letters for the fortran names. Dependent on the switch you use they are
   replaced by the correct function name
*/

/* only use capital letters */
#if defined __CAPS__ && !defined __UNDERSCORE__ && !defined __2UNDERSCORES__
#define mc64ad          MC64AD
#define mc64id          MC64ID
#define fd05ad          FD05AD
#define mc64as          MC64AS
#define mc64is          MC64IS
#define fd64as          FD64AS
#define mc64a           MC64A
#define mc64i           MC64I
#define fd05a           FD05A

#define mc64az          MC64AZ
#define mc64iz          MC64IZ
#define mc64ac          MC64AC
#define mc64ic          MC64IC

/* no capital letters */
#elif defined __UNDERSCORE__ && !defined __CAPS__ && !defined __2UNDERSCORES__
#define mc64ad          mc64ad_
#define mc64id          mc64id_
#define fd05ad          fd05ad_
#define mc64as          mc64as_
#define mc64is          mc64is_
#define fd64as          fd64as_
#define mc64a           mc64a_
#define mc64i           mc64i_
#define fd05a           fd05a_

#define mc64az          mc64az_
#define mc64iz          mc64iz_
#define mc64ac          mc64ac_
#define mc64ic          mc64ic_

/* both are defined */
#elif defined __CAPS__ && defined __UNDERSCORE__ && !defined __2UNDERSCORES__
#define mc64ad          MC64AD_
#define mc64id          MC64ID_
#define fd05ad          FD05AD_
#define mc64as          MC64AS_
#define mc64is          MC64IS_
#define fd64as          FD64AS_
#define mc64a           MC64A_
#define mc64i           MC64I_
#define fd05a           FD05A_

#define mc64az          MC64AZ_
#define mc64iz          MC64IZ_
#define mc64ac          MC64AC_
#define mc64ic          MC64IC_


/* CAPS and 2 underscores are defined */
#elif defined __CAPS__ && defined __2UNDERSCORES__
#define mc64ad          MC64AD__
#define mc64id          MC64ID__
#define fd05ad          FD05AD__
#define mc64as          MC64AS__
#define mc64is          MC64IS__
#define fd64as          FD64AS__
#define mc64a           MC64A__
#define mc64i           MC64I__
#define fd05a           FD05A__

#define mc64az          MC64AZ__
#define mc64iz          MC64IZ__
#define mc64ac          MC64AC__
#define mc64ic          MC64IC__


/* no capital letters but 2 underscores */
#elif defined __2UNDERSCORES__ && !defined __CAPS__
#define mc64ad          mc64ad__
#define mc64id          mc64id__
#define fd05ad          fd05ad__
#define mc64as          mc64as__
#define mc64is          mc64is__
#define fd64as          fd64as__
#define mc64a           mc64a__
#define mc64i           mc64i__
#define fd05a           fd05a__

#define mc64az          mc64az__
#define mc64iz          mc64iz__
#define mc64ac          mc64ac__
#define mc64ic          mc64ic__

#else
#define mc64ad          mc64ad
#define mc64id          mc64id
#define fd05ad          fd05ad
#define mc64as          mc64as
#define mc64is          mc64is
#define fd05as          fd05as
#define mc64a           mc64a
#define mc64i           mc64i
#define fd64a           fd64a

#define mc64az          mc64az
#define mc64iz          mc64iz
#define mc64ac          mc64ac
#define mc64ic          mc64ic

#endif /* defined __CAPS__ && ... */

#endif /* _NAMESHSL_H */


