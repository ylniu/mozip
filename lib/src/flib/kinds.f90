module kinds
	integer, PARAMETER :: DP=kind(1.D0)
	!-------------------------------------------------------------------
	! DP =  4, precision =  6, range = 37
	!
	! integer, parameter :: DP=select_real_kind(6)
	!-------------------------------------------------------------------
	! DP =  8, precision = 15, range = 307;
	!
	! integer, parameter :: DP=select_real_kind(10,100)
	!-------------------------------------------------------------------
	! DP = 10, precision = 18, range = 4931;
	!
	! integer, parameter :: DP=select_real_kind(r=400)
	!-------------------------------------------------------------------
	! DP = 10, precision = 18, range = 4931;
	!
	! integer, parameter :: DP=select_real_kind(p=10,r=400)
	!-------------------------------------------------------------------
	! DP = 16, precision = 33, range = 4931;
	!
	! integer, parameter :: DP=select_real_kind(p=20,r=400)
	!-------------------------------------------------------------------
	!
end module
