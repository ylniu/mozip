subroutine nat_to_symbol(n,nat,symbol)
	use kinds
	implicit none
	!----------------------------------------------------------------------------
	! Input  variables
	!
	integer     , intent( in) :: n
	integer     , intent( in) :: nat(n)
	!----------------------------------------------------------------------------
	! Output variable
	!
	character(*), intent(out) :: symbol(n)
	!----------------------------------------------------------------------------
	! Local  variables
	!
	integer     , parameter   :: na=118
	integer                   :: i
	character(3)              :: symbols(0:na)
	!----------------------------------------------------------------------------
	data symbols / &
	!    0
		"X"  ,                                                                &
	!    1      2      3      4      5      6      7      8      9     10
		"H"  , "He" , "Li" , "Be" , "B"  , "C"  , "N"  , "O"  , "F"  , "Ne" , &
	!   11     12     13     14     15     16     17     18     19     20
		"Na" , "Mg" , "Al" , "Si" , "P"  , "S"  , "Cl" , "Ar" , "K"  , "Ca" , &
	!   21     22     23     24     25     26     27     28     29     30
		"Sc" , "Ti" , "V"  , "Cr" , "Mn" , "Fe" , "Co" , "Ni" , "Cu" , "Zn" , &
	!   31     32     33     34     35     36     37     38     39     40
		"Ga" , "Ge" , "As" , "Se" , "Br" , "Kr" , "Rb" , "Sr" ,  "Y" , "Zr" , &
	!   41     42     43     44     45     46     47     48     49     50
		"Nb" , "Mo" , "Tc" , "Ru" , "Rh" , "Pd" , "Ag" , "Cd" , "In" , "Sn" , &
	!   51     52     53     54     55     56     57     58     59     60
		"Sb" , "Te" , "I"  , "Xe" , "Cs" , "Ba" , "La" , "Ce" , "Pr" , "Nd" , &
	!   61     62     63     64     65     66     67     68     69     70
		"Pm" , "Sm" , "Eu" , "Gd" , "Tb" , "Dy" , "Ho" , "Er" , "Tm" , "Yb" , &
	!   71     72     73     74     75     76     77     78     79     80
		"Lu" , "Hf" , "Ta" , "W"  , "Re" , "Os" , "Ir" , "Pt" , "Au" , "Hg" , &
	!   81     82     83     84     85     86     87     88     89     90
		"Tl" , "Pb" , "Bi" , "Po" , "At" , "Rn" , "Fr" , "Ra" , "Ac" , "Th" , &
	!   91     92     93     94     95     96     97     98     99    100
		"Pa" , "U"  , "Np" , "Pu" , "Am" , "Cm" , "Bk" , "Cf" , "Es" , "Fm" , &
	!  101    102    103    104    105    106    107    108    109    110
		"Md" , "No" , "Lr" , "Rf" , "Db" , "Sg" , "Bh" , "Hs" , "Mt" , "Ds" , &
	!  111    112    113    114    115    116    117    118
		"Rg" , "Cn" , "Uut", "Fl" , "Uup", "Lv" , "Uus", "Uuo" /
	do i=1, n
		if (nat(i)>=0 .and. nat(i)<=na ) then
			symbol(i) = symbols ( nat(i) )
		else
			write(*,'("Error!")')
			write(*,'("nat(",i0,") = ",i0," is out of [1,118]")') i, nat(i)
			stop
		end if
	end do
	!----------------------------------------------------------------------------
	return
end subroutine
