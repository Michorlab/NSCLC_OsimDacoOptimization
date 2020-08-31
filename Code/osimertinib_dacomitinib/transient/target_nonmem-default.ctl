$PROBLEM problem placeholder
; Currently the NONMEM target is being developed
; and it is not considered usable.

$INPUT
; No data file specified 


$DATA ; No data file specified 


$SUB ADVAN13 TOL=4 

$MODEL 

; Defining the compartment names
COMP=(Ato)                    ; # 1 
COMP=(C1o)                    ; # 2 
COMP=(C2o)                    ; # 3 
COMP=(Atd)                    ; # 4 
COMP=(C2d)                    ; # 5 
COMP=(C3d)                    ; # 6 



$PK
; Defining the system parameters
SIMINT_TV_kao  = THETA(1)
SIMINT_TV_CL1o  = THETA(2)
SIMINT_TV_V1o  = THETA(3)
SIMINT_TV_CL2o  = THETA(4)
SIMINT_TV_V2o  = THETA(5)
MWo           = THETA(6)
SIMINT_TV_kad  = THETA(7)
SIMINT_TV_CLd  = THETA(8)
SIMINT_TV_V2d  = THETA(9)
SIMINT_TV_Qd  = THETA(10)
SIMINT_TV_V3d  = THETA(11)
MWd           = THETA(12)
 
; Defining the iiv variables
ETAkao        = ETA(1)
ETACL1o       = ETA(2)
ETAV1o        = ETA(3)
ETACL2o       = ETA(4)
ETAV2o        = ETA(5)
ETACLd        = ETA(6)
ETAV2d        = ETA(7)
ETAkad        = ETA(8)
ETAQd         = ETA(9)
ETAV3d        = ETA(10)
 
kao           = SIMINT_TV_kao*exp(ETAkao)
CL1o          = SIMINT_TV_CL1o*exp(ETACL1o)
V1o           = SIMINT_TV_V1o*exp(ETAV1o)
CL2o          = SIMINT_TV_CL2o*exp(ETACL2o)
V2o           = SIMINT_TV_V2o*exp(ETAV2o)
kad           = SIMINT_TV_kad*exp(ETAkad)
CLd           = SIMINT_TV_CLd*exp(ETACLd)
V2d           = SIMINT_TV_V2d*exp(ETAV2d)
Qd            = SIMINT_TV_Qd*exp(ETAQd)
V3d           = SIMINT_TV_V3d*exp(ETAV3d)
   

; Defining the static 
; secondary parmaeters
 

; Initial conditions
A_0(1) = 0.0          ; Ato 
A_0(2) = 0.0          ; C1o 
A_0(3) = 0.0          ; C2o 
A_0(4) = 0.0          ; Atd 
A_0(5) = 0.0          ; C2d 
A_0(6) = 0.0          ; C3d 



$DES 

; creating the default internal time variable
SIMINT_TIME = TIME
; Mapping the amounts to meaningful names
Ato         =  A(1) 
C1o         =  A(2) 
C2o         =  A(3) 
Atd         =  A(4) 
C2d         =  A(5) 
C3d         =  A(6) 


; Defining secondary paraemters that can
; change with time


; Differential equations for each compartment
SIMINT_dAto           = (- kao*Ato)
SIMINT_dC1o           = (kao*Ato/V1o 		- CL1o*C1o/V1o)
SIMINT_dC2o           = (CL1o*C1o/V2o	 	- CL2o*C2o/V2o)
SIMINT_dAtd           = (- kad*Atd)
SIMINT_dC2d           = (kad*Atd/V2d 		- Qd*C2d/V2d)
SIMINT_dC3d           = (Qd*C2d/V3d 		- CLd*C3d/V3d)


; Mapping thes named ODEs above back to the 
; appropriate DADT variables
DADT(1) = SIMINT_dAto 
DADT(2) = SIMINT_dC1o 
DADT(3) = SIMINT_dC2o 
DADT(4) = SIMINT_dAtd 
DADT(5) = SIMINT_dC2d 
DADT(6) = SIMINT_dC3d 


$ERROR
; The SIEB (Simulation Internal Error Block) prefix
; is added to variables that are used in both
; the DES and ERROR blocks

; creating the default internal time variable
SIEB_TIME = TIME

; Mapping the states to their names
SIEB_Ato         =  A(1) 
SIEB_C1o         =  A(2) 
SIEB_C2o         =  A(3) 
SIEB_Atd         =  A(4) 
SIEB_C2d         =  A(5) 
SIEB_C3d         =  A(6) 


; Defining the dynamic
; secondary parameters


;mapping variance parameters to named values


$THETA
(0.0,         0.24,        INF         );  1 kao           1/hour 
(0.0,         11.76134,    INF         );  2 CL1o          L/hour 
(0.0,         792.3074,    INF         );  3 V1o           L 
(0.0,         22.57583,    INF         );  4 CL2o          L/hour 
(0.0,         207,         INF         );  5 V2o           L 
(0.0,         500,         INF         );  6 MWo           kD 
(0.0,         0.067,       INF         );  7 kad           1/hour 
(0.0,         16.94379,    INF         );  8 CLd           L/hour 
(0.0,         899.371,     INF         );  9 V2d           L 
(0.0,         17.32,       INF         ); 10 Qd            L/hour 
(0.0,         908.308,     INF         ); 11 V3d           L 
(0.0,         469.945,     INF         ); 12 MWd           kD 



$OMEGA BLOCK(10)
0.7921                                                                                                                  ; ETAkao
0           0.2116                                                                                                      ; ETACL1o
0           0           0.2704                                                                                          ; ETAV1o
0           0.215280000 0           0.2704                                                                              ; ETACL2o
0           0           0           0           0.3844                                                                  ; ETAV2o
0           0.01476306  0           0           0           0.103                                                       ; ETACLd
0           0           0.1340652   0           0           0           0.092                                           ; ETAV2d
0           0           0           0           0           0           0           1.466                               ; ETAkad
0           0           0           0.008221922 0           0.035521120 0           0           0.025                   ; ETAQd
0           0           

$SIGMA
; No variance parameters defined
; See: <VP> 


