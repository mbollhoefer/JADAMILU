#include <ilupack.h>
#include <ilupackmacros.h>


double dgeteps()

{
   double eps=1, eps_old=1, one_eps=2.0;
   integer   i=0; 
         

   while (one_eps>(double)1.0)
   {
         eps_old=(double)eps;
         eps/=2;
         i--;
	 one_eps=(double)(1+eps);
   }
   eps=(double)eps_old;


   return(eps);
} /* end dgeteps */



float sgeteps()

{
   float eps=1, eps_old=1, one_eps=2.0;
   integer   i=0; 
         

   while (one_eps>(float)1.0)
   {
         eps_old=(float)eps;
         eps/=2;
         i--;
	 one_eps=(float)(1+eps);
   }
   eps=(float)eps_old;


   return(eps);
} /* end sgeteps */


