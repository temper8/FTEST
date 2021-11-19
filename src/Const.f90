! **********************************************************************
module Constants
use Types
real(DPT),parameter ::  ZERO = 0.0_DPT
real(DPT),parameter ::  HALF = 0.5_DPT
real(DPT),parameter ::   ONE = 1.0_DPT
real(DPT),parameter ::   TWO = 2.0_DPT
real(DPT),parameter :: THREE = 3.0_DPT
real(DPT),parameter ::  FOUR = 4.0_DPT
complex(DPT),parameter :: ZEROC = (ZERO,ZERO)
complex(DPT),parameter ::  ONEC = (ONE,ZERO)
complex(DPT),parameter ::    IU = (ZERO,ONE)
real(DPT),parameter :: PI    = 3.141592653589793238462643383279502884197_DPT
real(DPT),parameter :: PISQRT  = 1.7724538509055160272981674833411_DPT
real(DPT),parameter :: TWOPI = TWO*PI
real(DPT),parameter :: R2D = 180.0_DPT/PI
real(DPT),parameter :: VC = 2.99792458e10_DPT
real(DPT),parameter :: VCO8PI = VC/8.0_DPT/PI
real(DPT),parameter :: amr = 1836.1_DPT
real(DPT),parameter :: CGS_Pscale=1.0e13_DPT       ! 1 MW power in CGS (erg/sec)
real(DPT),parameter :: CGS_Escale=10.0_DPT/3.0_DPT ! 1 kV/cm in CGS system
real(DPT),parameter :: CGS_Hscale=PI/2.5_DPT       ! 1 A/cm in CGS system

real(DPT),parameter::EPS_length=1.0e-8_DPT ! used in detecting identical WG vectors
end module Constants
