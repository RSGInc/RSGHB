#include <R.h>
#include <Rmath.h>

void aggregation(
     double *gIDS, 
     double *gNOBs, 
     double *gNP, 
     double *p, 
     double *uniqueIDs, 
     double *aggP
)
{
	int i, k, start;
     start = 0;
	for(i=0; i < *gNP; i++) {
          for(k=start; k < *gNOBs; k++) {          
	          if(gIDS[k] == uniqueIDs[i])
               {
                    aggP[i] = aggP[i] * p[k];
               }
               else if(gIDS[k] > uniqueIDs[i]) 
               {
                    start=k;
                    break;
               }
          }
     }

}
