/* Internal Variables */
double SIMINT_TIME = 0.0;
/* Initializing parameters */
double kao         = 0.0; 
double CL1o        = 0.0; 
double V1o         = 0.0; 
double CL2o        = 0.0; 
double V2o         = 0.0; 
double MWo         = 0.0; 
double kad         = 0.0; 
double CLd         = 0.0; 
double V2d         = 0.0; 
double Qd          = 0.0; 
double V3d         = 0.0; 
double MWd         = 0.0; 



/* Initializing derivatives  */
double SIMINT_dAto  = 0.0; 
double SIMINT_dC1o  = 0.0; 
double SIMINT_dC2o  = 0.0; 
double SIMINT_dAtd  = 0.0; 
double SIMINT_dC2d  = 0.0; 
double SIMINT_dC3d  = 0.0; 



/* Mapping states to their common names */
double Ato          = 0.0;
double C1o          = 0.0;
double C2o          = 0.0;
double Atd          = 0.0;
double C2d          = 0.0;
double C3d          = 0.0;



/* Defining internal values */
SIMINT_TIME = ssGetT(S);



/* 
 * Mapping C inputs to variable names and 
 * defining secondary parameters  
 */


/* Defining parameters */
kao         = u[0]; 
CL1o        = u[1]; 
V1o         = u[2]; 
CL2o        = u[3]; 
V2o         = u[4]; 
MWo         = u[5]; 
kad         = u[6]; 
CLd         = u[7]; 
V2d         = u[8]; 
Qd          = u[9]; 
V3d         = u[10]; 
MWd         = u[11]; 



/* Mapping states to their common names */
Ato          = x[0];
C1o          = x[1];
C2o          = x[2];
Atd          = x[3];
C2d          = x[4];
C3d          = x[5];
