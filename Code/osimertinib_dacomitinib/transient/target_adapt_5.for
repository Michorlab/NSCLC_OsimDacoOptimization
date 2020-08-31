C To customize this template edit the file
C adapt_system.for
C**********************************************************************
C                           ADAPT                                     *
C                         Version 5                                   *
C**********************************************************************
C                                                                     *
C                       MODEL - AUTO GENERATED                        *
C                                                                     *
C    This file contains Fortran subroutines into which the user       *
C    must enter the relevant model equations and constants.           *
C    Consult the User's Guide for details concerning the format for   *
C    entered equations and definition of symbols.                     *
C                                                                     *
C       1. Symbol-  Parameter symbols and model constants             *
C       2. DiffEq-  System differential equations                     *
C       3. Output-  System output equations                           *
C       4. Varmod-  Error variance model equations                    *
C       5. Covmod-  Covariate model equations (ITS,MLEM)              *
C       6. Popinit- Population parameter initial values (ITS,MLEM)    *
C       7. Prior -  Parameter mean and covariance values (ID,NPD,STS) *
C       8. Sparam-  Secondary parameters                              *
C       9. Amat  -  System state matrix                               *
C                                                                     *
C**********************************************************************

C######################################################################C

        Subroutine SYMBOL
        Implicit None

        Include 'globals.inc'
        Include 'model.inc'

CC
C----------------------------------------------------------------------C
C                   Enter as Indicated                                 C
C----------------------------------------------------------------------C

      NDEqs   =  6   ! Enter # of Diff. Eqs.
      NSParam =  12   ! Enter # of System Parameters.
      NVParam =  0   ! Enter # of Variance Model Parameters.
      NSecPar =  0   ! Enter # of Secondary Parameters.
      NSecOut =  0   ! Enter # of Secondary Outputs (not used).
      Ieqsol  =  1   ! Model type: 1 - DIFFEQ, 2 - AMAT, 3 - OUTPUT only.
      Descr = 'Autogenerated Adapt 5 Model Target'

CC
C----------------------------------------------------------------------C
C     Enter Symbol for Each System Parameter (eg. Psym(1)='Kel')       C
C----c-----------------------------------------------------------------C

       Psym(1) = 'kao'
       Psym(2) = 'CL1o'
       Psym(3) = 'V1o'
       Psym(4) = 'CL2o'
       Psym(5) = 'V2o'
       Psym(6) = 'MWo'
       Psym(7) = 'kad'
       Psym(8) = 'CLd'
       Psym(9) = 'V2d'
       Psym(10) = 'Qd'
       Psym(11) = 'V3d'
       Psym(12) = 'MWd'


CC
C----------------------------------------------------------------------C
C    Enter Symbol for Each Variance Parameter {eg: PVsym(1)='Sigma'}   C
C----c-----------------------------------------------------------------C



CC
C----------------------------------------------------------------------C
C    Enter Symbol for Each Secondary Parameter {eg: PSsym(1)='CLt'}    C
C----c-----------------------------------------------------------------C



C----------------------------------------------------------------------C
C----------------------------------------------------------------------C
C
      Return
      End

C######################################################################C

      Subroutine DIFFEQ(T,X,XP)
      Implicit None

      Include 'globals.inc'
      Include 'model.inc'

      Real*8 T,X(MaxNDE),XP(MaxNDE)
C---->Declaring System Parameters
       Real*8 kao, CL1o, V1o, CL2o, V2o, MWo, kad, CLd, V2d, Qd, V3d,
     c  MWd


C---->Declaring State and Derivative Names 
       Real*8 Ato, C1o, C2o, Atd, C2d, C3d
       Real*8 SIMINT_dAto, SIMINT_dC1o, SIMINT_dC2o, SIMINT_dAtd,
     c  SIMINT_dC2d, SIMINT_dC3d

 
C---->Declaring Dynamic Secondary Parameters
       Real*8 SIMINT_TIME


C---->System Parameters
       kao          = P(1)
       CL1o         = P(2)
       V1o          = P(3)
       CL2o         = P(4)
       V2o          = P(5)
       MWo          = P(6)
       kad          = P(7)
       CLd          = P(8)
       V2d          = P(9)
       Qd           = P(10)
       V3d          = P(11)
       MWd          = P(12)


C---->States
       Ato          = X(1)
       C1o          = X(2)
       C2o          = X(3)
       Atd          = X(4)
       C2d          = X(5)
       C3d          = X(6)

 
C---->Dynamic Secondary Parameters
       SIMINT_TIME  = T



CC
C----------------------------------------------------------------------C
C     Enter Differential Equations Below  {e.g.  XP(1) = -P(1)*X(1) }  C
C----c-----------------------------------------------------------------C


C---->Assigning the ODEs
       SIMINT_dAto  = (- kao*Ato)
       SIMINT_dC1o  = (kao*Ato/V1o 		- CL1o*C1o/V1o)
       SIMINT_dC2o  = (CL1o*C1o/V2o	 	- CL2o*C2o/V2o)
       SIMINT_dAtd  = (- kad*Atd)
       SIMINT_dC2d  = (kad*Atd/V2d 		- Qd*C2d/V2d)
       SIMINT_dC3d  = (Qd*C2d/V3d 		- CLd*C3d/V3d)


C---->Mapping Named ODEs to the XP variables 
       XP(1)   = SIMINT_dAto
       XP(2)   = SIMINT_dC1o
       XP(3)   = SIMINT_dC2o
       XP(4)   = SIMINT_dAtd
       XP(5)   = SIMINT_dC2d
       XP(6)   = SIMINT_dC3d


C----------------------------------------------------------------------C
C----------------------------------------------------------------------C
C
      Return
      End

C######################################################################C

      Subroutine OUTPUT(Y,T,X)
      Implicit None

      Include 'globals.inc'
      Include 'model.inc'

      Real*8 Y(MaxNOE),T,X(MaxNDE)
      Integer DelayPar, I
C---->Declaring System Parameters
       Real*8 kao, CL1o, V1o, CL2o, V2o, MWo, kad, CLd, V2d, Qd, V3d,
     c  MWd


C---->Declaring State and Derivative Names 
       Real*8 Ato, C1o, C2o, Atd, C2d, C3d
       Real*8 SIMINT_dAto, SIMINT_dC1o, SIMINT_dC2o, SIMINT_dAtd,
     c  SIMINT_dC2d, SIMINT_dC3d

 
C---->Declaring Dynamic Secondary Parameters
       Real*8 SIMINT_TIME

C---->Declaring Outputs 
       Real*8 C_osi, C_osi_met, C_daco_c, C_daco_p


C---->System Parameters
       kao          = P(1)
       CL1o         = P(2)
       V1o          = P(3)
       CL2o         = P(4)
       V2o          = P(5)
       MWo          = P(6)
       kad          = P(7)
       CLd          = P(8)
       V2d          = P(9)
       Qd           = P(10)
       V3d          = P(11)
       MWd          = P(12)


C---->States
       Ato          = X(1)
       C1o          = X(2)
       C2o          = X(3)
       Atd          = X(4)
       C2d          = X(5)
       C3d          = X(6)

 
C---->Dynamic Secondary Parameters
       SIMINT_TIME  = T



CC
C----------------------------------------------------------------------C
C     Enter Output Equations Below   {e.g.  Y(1) = X(1)/P(2) }         C
C----c-----------------------------------------------------------------C

C---->Assigning Outputs 
       C_osi       = C1o
       C_osi_met   = C2o
       C_daco_c    = C2d
       C_daco_p    = C3d
 

C---->Mapping Outputs to Y variables 
       Y(1)    = C_osi
       Y(2)    = C_osi_met
       Y(3)    = C_daco_c
       Y(4)    = C_daco_p

C----------------------------------------------------------------------C
C----------------------------------------------------------------------C
C
      Return
      End

C######################################################################C

      Subroutine VARMOD(V,T,X,Y)
      Implicit None

      Include 'globals.inc'
      Include 'model.inc'

      Real*8 V(MaxNOE),T,X(MaxNDE),Y(MaxNOE)
C---->Declaring Outputs 
       Real*8 C_osi, C_osi_met, C_daco_c, C_daco_p

 
C---->Declaring Variance Equation Variables 
       Real*8 SIMINT_VAR_C_osi, SIMINT_VAR_C_osi_met,
     c  SIMINT_VAR_C_daco_c, SIMINT_VAR_C_daco_p
 

C---->Defining Outputs 
       C_osi       = Y(1)
       C_osi_met   = Y(2)
       C_daco_c    = Y(3)
       C_daco_p    = Y(4)

 
 


CC
C----------------------------------------------------------------------C
C       Enter Variance Model Equations Below                           C
C        {e.g. V(1) = (PV(1) + PV(2)*Y(1))**2 }                        C
C----c-----------------------------------------------------------------C

C---->Assigning Variances 
       SIMINT_VAR_C_osi    = 1.0
       SIMINT_VAR_C_osi_met= 1.0
       SIMINT_VAR_C_daco_c = 1.0
       SIMINT_VAR_C_daco_p = 1.0


C---->Mapping Variance to V variables 
       V(1)    = SIMINT_VAR_C_osi
       V(2)    = SIMINT_VAR_C_osi_met
       V(3)    = SIMINT_VAR_C_daco_c
       V(4)    = SIMINT_VAR_C_daco_p


C----------------------------------------------------------------------C
C----------------------------------------------------------------------C
C
      Return
      End

C######################################################################C

      Subroutine COVMOD(Pmean, ICmean, PC)
C  Defines any covariate model equations (MLEM, ITS)
      Implicit None

      Include 'globals.inc'
      Include 'model.inc'

      Real*8 PC(MaxNCP)
      Real*8 Pmean(MaxNSP+MaxNDE), ICmean(MaxNDE)

CC
C----------------------------------------------------------------------C
C     Enter # of Covariate Parameters                                  C
C----c-----------------------------------------------------------------C

      NCparam = 0    ! Enter # of Covariate Parameters.

CC
C----------------------------------------------------------------------C
C   Enter Symbol for Covariate Params {eg: PCsym(1)='CLRenal'}         C
C----c-----------------------------------------------------------------C


CC
C----------------------------------------------------------------------C
C   For the Model Params. that Depend on Covariates Enter the Equation C
C         {e.g. Pmean(1) =  PC(1)*R(2) }                               C
C----c-----------------------------------------------------------------C


C----------------------------------------------------------------------C
C----------------------------------------------------------------------C
C
      Return
      End

C######################################################################C

      Subroutine POPINIT(PmeanI,ICmeanI,PcovI,ICcovI, PCI)
C  Initial parameter values for population program parameters (ITS, MLEM)

      Implicit None

      Include 'globals.inc'
      Include 'model.inc'

      Integer I,J
      Real*8 PmeanI(MaxNSP+MaxNDE), ICmeanI(MaxNDE)
      Real*8 PcovI(MaxNSP+MaxNDE,MaxNSP+MaxNDE), ICcovI(MaxNDE,MaxNDE)
      Real*8 PCI(MaxNCP)

CC
C----------------------------------------------------------------------C
C  Enter Initial Values for Population Means                           C
C          {  e.g. PmeanI(1) = 10.0    }                               C
C----c-----------------------------------------------------------------C


CC
C----------------------------------------------------------------------C
C   Enter Initial Values for Pop. Covariance Matrix (Lower Triang.)    C
C         {  e.g. PcovI(2,1) = 0.25    }                               C
C----c-----------------------------------------------------------------C


CC
C----------------------------------------------------------------------C
C   Enter Values for Covariate Model Parameters                        C
C         {  e.g. PCI(1) = 2.0    }                                    C
C----c-----------------------------------------------------------------C


C----------------------------------------------------------------------C
C----------------------------------------------------------------------C
C
      Return
      End

C######################################################################C

      Subroutine PRIOR(Pmean,Pcov,ICmean,ICcov)
C  Parameter mean and covariance values for MAP estimation (ID,NPD,STS)
      Implicit None

      Include 'globals.inc'
      Include 'model.inc'

      Integer I,J
      Real*8 Pmean(MaxNSP+MaxNDE), ICmean(MaxNDE)
      Real*8 Pcov(MaxNSP+MaxNDE,MaxNSP+MaxNDE), ICcov(MaxNDE,MaxNDE)

CC
C----------------------------------------------------------------------C
C  Enter Nonzero Elements of Prior Mean Vector                         C
C          {  e.g. Pmean(1) = 10.0    }                                C
C----c-----------------------------------------------------------------C


CC
C----------------------------------------------------------------------C
C   Enter Nonzero Elements of Covariance Matrix (Lower Triang.)       C
C         {  e.g. Pcov(2,1) = 0.25    }                                C
C----c-----------------------------------------------------------------C


C----------------------------------------------------------------------C
C----------------------------------------------------------------------C
C
      Return
      End

C######################################################################C

      Subroutine SPARAM(PS,P,IC)
      Implicit None

      Include 'globals.inc'

      Real*8 PS(MaxNSECP), P(MaxNSP+MaxNDE), IC(MaxNDE) 
C---->Declaring System Parameters
       Real*8 kao, CL1o, V1o, CL2o, V2o, MWo, kad, CLd, V2d, Qd, V3d,
     c  MWd



C---->System Parameters
       kao          = P(1)
       CL1o         = P(2)
       V1o          = P(3)
       CL2o         = P(4)
       V2o          = P(5)
       MWo          = P(6)
       kad          = P(7)
       CLd          = P(8)
       V2d          = P(9)
       Qd           = P(10)
       V3d          = P(11)
       MWd          = P(12)


CC
C----------------------------------------------------------------------C
C       Enter Equations Defining Secondary Paramters                   C
C           {  e.g.  PS(1) = P(1)*P(2)   }                             C
C----c-----------------------------------------------------------------C
      




C----------------------------------------------------------------------C
C----------------------------------------------------------------------C
C
        Return
        End
        
C######################################################################C

        Subroutine AMAT(A)
        Implicit None

        Include 'globals.inc'
        Include 'model.inc'

        Integer I,J
        Real*8 A(MaxNDE,MaxNDE)

        DO I=1,Ndeqs
           Do J=1,Ndeqs
              A(I,J)=0.0D0
           End Do
        End Do

CC
C----------------------------------------------------------------------C
C    Enter non zero elements of state matrix  {e.g.  A(1,1) = -P(1) }  C
C----c-----------------------------------------------------------------C



C----------------------------------------------------------------------C
C----------------------------------------------------------------------C
C
        Return
        End

C######################################################################C';
