subroutine nat_to_mass(n,nat,atmas)
	!----------------------------------------------------------------------------
	!
	! The masses are from A. H. Wapstra and K. Bos,
	! Atomic and Nuclear Data Tables, 1977, 19, 185.
	! Masses of atoms after Kr were also taken from CRC.
	!
	!----------------------------------------------------------------------------
	use kinds
	!----------------------------------------------------------------------------
	implicit none
	!----------------------------------------------------------------------------
	! Input  variables
	!
	integer , intent( in) :: n
	integer , intent( in) :: nat(n)
	!----------------------------------------------------------------------------
	! Output variables
	!----------------------------------------------------------------------------
	real(DP), intent(out) :: atmas(n)
	!----------------------------------------------------------------------------
	! Local  variables
	!
	integer, parameter    :: na=118
	integer               :: i,j
	real(DP)              :: mass(1:na)
	!----------------------------------------------------------------------------
	data mass / &
	!    1:Hydrogen        2:Helium          3:Lithium         4:Beryllium       5:Boron
	!      H                 He                Li                Be                B
		  1.007825037d0,    4.002603250d0,    7.016004500d0,    9.012182500d0,   11.009305300d0, &
	!    6:Carbon          7:Nitrogen        8:Oxygen          9:Fluorine       10:Neon
	!      C                 N                 O                 F                 Ne
		 12.000000000d0,   14.003074008d0,   15.994914640d0,   18.998403250d0,   19.992439100d0, &
	!   11:Sodium         12:Magnesium      13:Aluminium      14:Silicon        15:Phosphorous
	!      Na                Mg                Al                Si                P
		 22.989769700d0,   23.985045000d0,   26.981541300d0,   27.976928400d0,   30.973763400d0, &
	!   16:Sulphur        17:Chlorine       18:Argon          19:Potassium      20:Calcium
	!      S                 Cl                Ar                K                 Ca
		 31.972071800d0,   34.968852729d0,   39.962383100d0,   38.963707900d0,   39.962590700d0, &
	!   21:Scandium       22:Titanium       23:Vanadium       24:Chromium       25:Manganese
	!      Sc                Ti                V                 Cr                Mn
		 44.955913600d0,   47.947946700d0,   50.943962500d0,  51.940509700d0,    54.938046300d0,&
	!   26:Iron           27:Cobalt         28:Nickel         29:Copper         30:Zinc
	!      Fe                Co                Ni                Cu                Zn
		55.934939300d0,    58.933197800d0,   57.935347100d0,   62.929599200d0,   63.929145400d0,&
	!   31:Gallium        32:Germanium      33:Arsenic        34:Selenium       35:Bromine
	!      Ga                Ge                As                Se                Br
		68.925580900d0,    73.921178800d0,   74.921595500d0,   79.916520500d0,   78.918336100d0,&
	!   36:Krypton        37:Rubidium       38:Strontium      39:Yttrium        40:Zirconium
	!      Kr                Rb                Sr                Y                 Zr
		83.911506400d0,    84.911700000d0,   87.905600000d0,   88.905400000d0,   89.904300000d0,&
	!   41:Niobium        42:Molybdenum     43:Technetium     44:Ruthenium      45:Rhodium
	!      Nb                Mo                Tc                Ru                Rh
		92.906000000d0,    97.905500000d0,   98.906300000d0,  101.903700000d0,  102.904800000d0,&
	!   46:Palladium      47:Silver         48:Cadmiumc       49:Indium         50:Tin
	!      Pd                Ag                Cd                In                Sn
		105.903200000d0,  106.905090000d0,  113.903600000d0,  114.904100000d0,  117.901800000d0,&
	!   51:Antinomy       52:Tellurium      53:Iodine         54:Xenon          55:Caesium
	!      Sb                Te                I                 Xe                Cs
		120.903800000d0,  129.906700000d0,  126.900400000d0,  131.904200000d0,  133.905100000d0,&
	!   56:Barium         57:Lanthanum      58:Cerium         59:Praseodymium   60:Neodymiumium
	!      Ba                La                Ce                Pr                Nd
		137.905000000d0,  138.906100000d0,  139.905300000d0,  140.907400000d0,  141.907500000d0,&
	!   61:Promethium     62:Samarium       63:Europium       64:Gadolinium     65:Terbium
	!      Pm                Sm                Eu                Gd                Tb
		0.000000000d0,    151.919500000d0,  152.920900000d0,  157.924100000d0,  158.925000000d0,&
	!   66:Dysprosium     67:Holmium        68:Erbium         69:Thulium        70:Ytterbium
	!      Dy                Ho                Er                Tm                Yb
		163.928800000d0,  164.930300000d0,  165.930400000d0,  168.934400000d0,  173.939000000d0,&
	!   71:Lutetium       72:Hafnium        73:Tantalum       74:Tungsten       75:Rhenium
	!      Lu                Hf                Ta                W                 Re
		174.940900000d0,  179.946800000d0,  180.948000000d0,  183.951000000d0,  186.956000000d0,&
	!   76:Osmium         77:Iridium        78:Platinumc      79:Gold           80:Mercury
	!      Os                Ir                Pt                Au                Hg
		189.958600000d0,  192.963300000d0,  194.964800000d0,  196.966600000d0,  201.970600000d0,&
	!   81:Thallium       82:Lead    c      83:Bismuth        84:Polonium       85:Astatine
	!      Tl                Pb                Bi                Po                At
		204.974500000d0,  207.976600000d0,  208.980400000d0,  208.982500000d0,  210.987500000d0,&
	!   86:Radon          87:Francium       88:Radium         89:Actinium       90:Thorium
	!      Rn                Fr                Ra                Ac                Th
		222.017500000d0,  223.019800000d0,  226.025400000d0,  227.027800000d0,  232.038200000d0,&
	!   91:Protoactinium  92:Uranium        93:Neptunium      94:Plutonium      95:Americium
	!      Pa                U                 Np                Pu                Am
		231.035900000d0,  238.050800000d0,  237.048000000d0,  242.058700000d0,  243.061400000d0,&
	!   96:Curium         97:Berkelium      98:Californium    99:Einsteinium   100:Fermium
	!      Cm                Bk                Cf                Es                Fm
		246.067400000d0,  247.070200000d0,  249.074800000d0,  252.082900000d0,  252.082700000d0,&
	!  101:Mendelevium   102:Nobeliumc     103:Lawrencium    104:Rutherfordium 105:Dubnium
	!      Md                No                Lr                Rf                Db
		255.090600000d0,    0.000000000d0,    0.000000000d0,    0.000000000d0,    0.000000000d0,&
	!  106:Seaborgium    107:Bohrium       108:Hassium       109:Meitnerium    110:Darmstadtium
	!      Sg                Bh                Hs                Mt                Ds
		  0.000000000d0,    0.000000000d0,    0.000000000d0,    0.000000000d0,    0.000000000d0,&
	!  111:Roentgenium   112:Ununbium      113:Ununtrium     114:Ununquadium   115:Ununpentium
	!      Rg                Cn                Uut               Fl                Uup
		  0.000000000d0,    0.000000000d0,    0.000000000d0,    0.000000000d0,   0.000000000d0,&
	!  116:Ununhexium    117:Ununseptium   118:Ununoctium
	!      Lv                Uus               Uuo
		  0.000000000d0,    0.000000000d0,    0.000000000d0 /
	!----------------------------------------------------------------------------
	do i=1, n
		do j=1, na
			if (nat(i)==j) then
				atmas(i) = mass(j)
				exit
			end if
		end do
	end do
	!----------------------------------------------------------------------------
	return
end subroutine