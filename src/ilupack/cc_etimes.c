#include <sys/times.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#include <ilupack.h>
#include <ilupackmacros.h>

#if 0
NAME
       times - get process times

SYNOPSIS
       #include <sys/times.h>

       clock_t times(struct tms *buf);

DESCRIPTION
       The  times()  function stores the current process times in the struct tms that
       buf points to.  The struct tms is as defined in <sys/times.h>:

       struct tms {
              clock_t tms_utime;  /* user time */
              clock_t tms_stime;  /* system time */
              clock_t tms_cutime; /* user time of children */
              clock_t tms_cstime; /* system time of children */
       };

       The tms_utime field contains the CPU time spent executing instructions of  the
       calling  process.  The tms_stime field contains the CPU time spent in the sys­
       tem while executing tasks on behalf of the calling  process.   The  tms_cutime
       field  contains the sum of the tms_utime and tms_cutime values for all waited-
       for terminated children.   The  tms_cstime  field  contains  the  sum  of  the
       tms_stime and tms_cstime values for all waited-for terminated children.

       Times  for  terminated  children  (and  their  descendants) is added in at the
       moment wait(2) or waitpid(2) returns their process ID. In particular, times of
       grandchildren that the children did not wait for are never seen.

       All times reported are in clock ticks.

RETURN VALUE
       The  function  times returns the number of clock ticks that have elapsed since
       an arbitrary point in the past. For Linux this point is the moment the  system
       was  booted.   This  return  value  may  overflow  the  possible range of type
       clock_t.  On error, (clock_t) -1 is returned, and errno is set  appropriately.

#endif
           
float evaluate_time( float *user, float *sys  )
{
  /*
     code written by Georg Fuss, September 2003
  */
    
    clock_t ttt, time_u, time_s;
    long    div;
    struct tms buf;

#if 0
      struct tms {
              clock_t tms_utime;  /* user time */
              clock_t tms_stime;  /* system time */
              clock_t tms_cutime; /* user time of children */
              clock_t tms_cstime; /* system time of children */
       };
#endif
      

      /*--------------------------------------------------------------------------*/
    
    div = sysconf( _SC_CLK_TCK );
    
    ttt = times( &buf );
    time_u = buf.tms_utime;
    time_s = buf.tms_stime;

    *user = (float)time_u / (float)div;
    *sys  = (float)time_s / (float)div;

    return ttt/ div; 

}
