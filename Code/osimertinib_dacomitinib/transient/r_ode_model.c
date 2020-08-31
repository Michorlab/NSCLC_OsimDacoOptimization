#include <R.h>

static double parms[12];

#define kao          parms[0]
#define CL1o         parms[1]
#define V1o          parms[2]
#define CL2o         parms[3]
#define V2o          parms[4]
#define MWo          parms[5]
#define kad          parms[6]
#define CLd          parms[7]
#define V2d          parms[8]
#define Qd           parms[9]
#define V3d          parms[10]
#define MWd          parms[11]




/* initializing the parameters and forcing functions  */
void initparams(void (* odeparms)(int *, double *))
{
    int N=12;
    odeparms(&N, parms);
}



/* Derivatives and outputs  */
void derivs (int *neq, 
             double *t, 
             double *y, 
             double *ydot,
             double *yout, 
             int *ip)
{

if (ip[0] <1) error("nout should be at least 1");

/* Start Initializing variables 
*/
double SIMINT_TIME = 0.0;

/* States and ODEs */
double Ato          = 0.0;
double SIMINT_dAto  = 0.0;
double C1o          = 0.0;
double SIMINT_dC1o  = 0.0;
double C2o          = 0.0;
double SIMINT_dC2o  = 0.0;
double Atd          = 0.0;
double SIMINT_dAtd  = 0.0;
double C2d          = 0.0;
double SIMINT_dC2d  = 0.0;
double C3d          = 0.0;
double SIMINT_dC3d  = 0.0;
/* Outputs*/
double C_osi        = 0.0;
double C_osi_met    = 0.0;
double C_daco_c     = 0.0;
double C_daco_p     = 0.0;
double SIMINT_TS_time = 0.0;
double SIMINT_TS_hour = 0.0;
double SIMINT_TS_days = 0.0;
double SIMINT_TS_weeks = 0.0;

/* Done Initializing variables */

/* System time and time scales*/
SIMINT_TIME = *t; 
SIMINT_TS_time    = SIMINT_TIME;
SIMINT_TS_hour = SIMINT_TIME*1;
SIMINT_TS_days = SIMINT_TIME*1/24;
SIMINT_TS_weeks = SIMINT_TIME*1/168;




/* Mapping states to named variables */
Ato          = y[0];
C1o          = y[1];
C2o          = y[2];
Atd          = y[3];
C2d          = y[4];
C3d          = y[5];




/* Defining the ODEs*/
SIMINT_dAto          = (- kao*Ato);
SIMINT_dC1o          = (kao*Ato/V1o 		- CL1o*C1o/V1o);
SIMINT_dC2o          = (CL1o*C1o/V2o	 	- CL2o*C2o/V2o);
SIMINT_dAtd          = (- kad*Atd);
SIMINT_dC2d          = (kad*Atd/V2d 		- Qd*C2d/V2d);
SIMINT_dC3d          = (Qd*C2d/V3d 		- CLd*C3d/V3d);


/* Mapping back to ydot variables */
ydot[0] = SIMINT_dAto;
ydot[1] = SIMINT_dC1o;
ydot[2] = SIMINT_dC2o;
ydot[3] = SIMINT_dAtd;
ydot[4] = SIMINT_dC2d;
ydot[5] = SIMINT_dC3d;


/* Defining the outputs*/
C_osi        = C1o;
C_osi_met    = C2o;
C_daco_c     = C2d;
C_daco_p     = C3d;


/* Mapping back to yout variables */
/* States */
/* Model Outputs */
yout[0] = C_osi;
yout[1] = C_osi_met;
yout[2] = C_daco_c;
yout[3] = C_daco_p;
/* System Parameters */
yout[4] = kao;
yout[5] = CL1o;
yout[6] = V1o;
yout[7] = CL2o;
yout[8] = V2o;
yout[9] = MWo;
yout[10] = kad;
yout[11] = CLd;
yout[12] = V2d;
yout[13] = Qd;
yout[14] = V3d;
yout[15] = MWd;
/* Covariates */
/* Time Scales */
yout[16] = SIMINT_TS_time;
yout[17] = SIMINT_TS_hour;
yout[18] = SIMINT_TS_days;
yout[19] = SIMINT_TS_weeks;

}
