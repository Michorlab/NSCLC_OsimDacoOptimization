$PARAM @annotated
TV_kao               : 0.24       : 1/hour
TV_CL1o              : 11.76134   : L/hour
TV_V1o               : 792.3074   : L
TV_CL2o              : 22.57583   : L/hour
TV_V2o               : 207        : L
MWo                  : 500        : kD
TV_kad               : 0.067      : 1/hour
TV_CLd               : 16.94379   : L/hour
TV_V2d               : 899.371    : L
TV_Qd                : 17.32      : L/hour
TV_V3d               : 908.308    : L
MWd                  : 469.945    : kD



$CMT @annotated
Ato                  : -- 
C1o                  : -- 
C2o                  : -- 
Atd                  : -- 
C2d                  : -- 
C3d                  : -- 


$MAIN
double kao                     = TV_kao*exp(ETAkao);
double CL1o                    = TV_CL1o*exp(ETACL1o);
double V1o                     = TV_V1o*exp(ETAV1o);
double CL2o                    = TV_CL2o*exp(ETACL2o);
double V2o                     = TV_V2o*exp(ETAV2o);
double kad                     = TV_kad*exp(ETAkad);
double CLd                     = TV_CLd*exp(ETACLd);
double V2d                     = TV_V2d*exp(ETAV2d);
double Qd                      = TV_Qd*exp(ETAQd);
double V3d                     = TV_V3d*exp(ETAV3d);





$OMEGA @annotated @block
ETAkao     : 0.7921                                                                                               : ETA on  kao
ETACL1o    : 0         0.2116                                                                                     : ETA on  CL1o
ETAV1o     : 0         0         0.2704                                                                           : ETA on  V1o
ETACL2o    : 0         0.2152800000         0.2704                                                                 : ETA on  CL2o
ETAV2o     : 0         0         0         0         0.3844                                                       : ETA on  V2o
ETACLd     : 0         0.014763060         0         0         0.103                                              : ETA on  CLd
ETAV2d     : 0         0         0.1340652 0         0         0         0.092                                    : ETA on  V2d
ETAkad     : 0         0         0         0         0         0         0         1.466                          : ETA on  kad
ETAQd      : 0         0         0         0.0082219220         0.0355211200         0         0.025                : ETA on  Qd
0         ETAV3d     : 0         



$ODE
double SIMINT_TIME  = SOLVERTIME;
 
//Defining the differential equations
dxdt_Ato             = (- kao*Ato);
dxdt_C1o             = (kao*Ato/V1o 		- CL1o*C1o/V1o);
dxdt_C2o             = (CL1o*C1o/V2o	 	- CL2o*C2o/V2o);
dxdt_Atd             = (- kad*Atd);
dxdt_C2d             = (kad*Atd/V2d 		- Qd*C2d/V2d);
dxdt_C3d             = (Qd*C2d/V3d 		- CLd*C3d/V3d);



$TABLE
SIMINT_TIME  = TIME;
// Initialzing model outputs
double C_osi                   = 0.0; 
double C_osi_met               = 0.0; 
double C_daco_c                = 0.0; 
double C_daco_p                = 0.0; 
 
C_osi                          = C1o;
C_osi_met                      = C2o;
C_daco_c                       = C2d;
C_daco_p                       = C3d;


$CAPTURE C_osi C_osi_met C_daco_c C_daco_p
