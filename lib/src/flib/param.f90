! Constants from:
! CODATA recommended values of the fundamental physical constants:2006
! Peter J. Mohr, Barry N. Taylor, and David B. Newell
! Reviews of Modern Physics, vol. 80, pages: 633-730, year: 2008
! DOI: 10.1103/RevModPhys.80.633
! http://users.mccammon.ucsd.edu/~dzhang/energy-unit-conv-table.html
!
module Param
	use kinds
	!----------------------------------------------------------------------------
	! Mathematical constants
	!
	real(DP), parameter :: PI      = acos(-1.0_DP)
	real(DP), parameter :: TPI     = PI * 2.0_DP
	real(DP), parameter :: FPI     = PI * 4.0_DP
	real(DP), parameter :: PI2     = TPI
	real(DP), parameter :: SQRTPI  = sqrt(PI)
	real(DP), parameter :: SQRTPM1 = 1.0_DP / SQRTPI
	real(DP), parameter :: SQRT2   = sqrt(2.0_DP)
	!----------------------------------------------------------------------------
	! Physical constants, SI (NIST CODATA 2006), Web Version 5.1
	! http://physics.nist.gov/constants
	!
	real(DP), parameter :: H_PLANCK_SI      = 6.62606896E-34_DP    ! J s
	real(DP), parameter :: K_BOLTZMANN_SI   = 1.3806504E-23_DP     ! J K^-1 
	real(DP), parameter :: ELECTRON_SI      = 1.602176487E-19_DP   ! C 
	real(DP), parameter :: ELECTRONVOLT_SI  = 1.602176487E-19_DP   ! J  
	real(DP), parameter :: ELECTRONMASS_SI  = 9.10938215E-31_DP    ! Kg
	real(DP), parameter :: HARTREE_SI       = 4.35974394E-18_DP    ! J 
	real(DP), parameter :: RYDBERG_SI       = HARTREE_SI/2.0_DP    ! J 
	real(DP), parameter :: BOHR_RADIUS_SI   = 0.52917720859E-10_DP ! m 
	real(DP), parameter :: AMU_SI           = 1.660538782E-27_DP   ! Kg
	real(DP), parameter :: C_SI             = 2.99792458E+8_DP     ! m sec^-1
	real(DP), parameter :: EPSILON0_SI      = 8.854187817E-12_DP   ! F·m−1
	!----------------------------------------------------------------------------
	! Physical constants, atomic units:
	! AU for "Hartree" atomic units (e = m = hbar = 1)
	! RY for "Rydberg" atomic units (e^2=2, m=1/2, hbar=1)
	!
	real(DP), parameter :: K_BOLTZMANN_AU   = K_BOLTZMANN_SI / HARTREE_SI
	real(DP), parameter :: K_BOLTZMANN_RY   = K_BOLTZMANN_SI / RYDBERG_SI
	!----------------------------------------------------------------------------
	! Unit conversion factors: energy and masses
	!
	real(DP), parameter :: AUTOEV           = HARTREE_SI / ELECTRONVOLT_SI
	real(DP), parameter :: RYTOEV           = AUTOEV / 2.0_DP
	real(DP), parameter :: AMU_AU           = AMU_SI / ELECTRONMASS_SI
	real(DP), parameter :: AMU_RY           = AMU_AU / 2.0_DP
	!----------------------------------------------------------------------------
	! Unit conversion factors: atomic unit of time, in s and ps
	!
	real(DP), parameter :: AU_SEC           = H_PLANCK_SI/tpi/HARTREE_SI
	real(DP), parameter :: AU_PS            = AU_SEC * 1.0E+12_DP
	!----------------------------------------------------------------------------
	! Unit conversion factors: pressure (1 Pa = 1 J/m^3, 1GPa = 10 Kbar )
	!
	real(DP), parameter :: AU_GPA           = HARTREE_SI / BOHR_RADIUS_SI ** 3 &
	                                          / 1.0E+9_DP
	real(DP), parameter :: RY_KBAR          = 10.0_DP * AU_GPA / 2.0_DP
	!----------------------------------------------------------------------------
	! Unit conversion factors: 1 debye = 10^-18 esu*cm 
	!                                  = 3.3356409519*10^-30 C*m 
	!                                  = 0.208194346 e*A
	! ( 1 esu = (0.1/c) Am, c=299792458 m/s)
	!
	real(DP), parameter :: DEBYE_SI         = 3.3356409519_DP * 1.0E-30_DP ! C*m 
	real(DP), parameter :: AU_DEBYE         = ELECTRON_SI * BOHR_RADIUS_SI / &
                                             DEBYE_SI
	!
	real(DP), parameter :: eV_to_kelvin = ELECTRONVOLT_SI / K_BOLTZMANN_SI
	real(DP), parameter :: ry_to_kelvin = RYDBERG_SI / K_BOLTZMANN_SI
	real(DP), parameter :: eV_to_kJmol  = 96.4869_DP
	real(DP), parameter :: kJmol_to_eV  = 0.0103641_DP
	!----------------------------------------------------------------------------
	! Unit conversion factors: Energy to wavelength
	!
	real(DP), parameter :: EVTONM = 1E+9_DP * H_PLANCK_SI * C_SI / ELECTRONVOLT_SI
	real(DP), parameter :: RYTONM = 1E+9_DP * H_PLANCK_SI * C_SI / RYDBERG_SI
	!----------------------------------------------------------------------------
	!  Speed of light in atomic units
	!
	real(DP), parameter :: C_AU             = C_SI / BOHR_RADIUS_SI * AU_SEC
	!----------------------------------------------------------------------------
	! Compatibility
	!
	real(DP), parameter :: BOHR_RADIUS_CM   = BOHR_RADIUS_SI * 100.0_DP
	real(DP), parameter :: BOHR_RADIUS_ANGS = BOHR_RADIUS_CM * 1.0E8_DP
	real(DP), parameter :: ANGSTROM_AU      = 1.0_DP/BOHR_RADIUS_ANGS
	real(DP), parameter :: DIP_DEBYE        = AU_DEBYE
	real(DP), parameter :: AU_TERAHERTZ     = AU_PS
	real(DP), parameter :: AU_TO_OHMCMM1    = 46000.0_DP ! (ohm cm)^-1
	real(DP), parameter :: RY_TO_THZ        = 1.0_DP / AU_TERAHERTZ / FPI
	real(DP), parameter :: RY_TO_CMM1       = 1.E+10_DP * RY_TO_THZ / C_SI
	!----------------------------------------------------------------------------
	!
	real(DP), parameter :: au2angs = BOHR_RADIUS_ANGS
	!----------------------------------------------------------------------------
	real(DP), parameter ::                &
		Cconst  = 2.99792458000000E10_DP,  &
		au2t    = 2.4188843265000E-17_DP,  &
		au2m    = 0.5291772490000E-10_DP,  &
		au2a    = 0.52917724900000000_DP,  &
		au2ev   = 27.2113961700000000_DP,  &
		au2kjm  = 2625.50000000000000_DP,  &
		au2kcm  = 627.503000000000000_DP,  &
		au2cm   = 219474.630700000000_DP,  &
		ev2cm   = 8065.54097100000000_DP,  &
		MassAmu = 1822.88853_DP,           &
		KB      = 3.166828E-6_DP,          &
		InvKB   = 3.1577498E5_DP,          &
		cphoton = 137.036053694531232_DP,  &
		fs2au   = 0.413413733366468E2_DP,  &
		au2fs   = 0.024188843265_DP,       &
		au2ps   = 0.024188843265E-3_DP,    &
		au2db   = 2.541580252938_DP,       &
		db2au   = 0.393456_DP
	real(DP)   , parameter :: au2nN            = HARTREE_SI / BOHR_RADIUS_SI * 1.0E9_DP
	real(DP)   , parameter :: au2aJ            = HARTREE_SI * 1.0E18_DP
	real(DP)   , parameter :: ZERO             = 0.0_DP
	real(DP)   , parameter :: ONE              = 1.0_DP
	real(DP)   , parameter :: TWO              = 2.0_DP
	complex(DP), parameter :: CZERO            = dcmplx(0.0_DP,0.0_DP)
	complex(DP), parameter :: CONE             = dcmplx(1.0_DP,0.0_DP)
	complex(DP), parameter :: II               = dcmplx(0.0_DP,1.0_DP)
end module
