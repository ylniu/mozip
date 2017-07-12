function in_cry_cell(ng,x)
	use kinds, only: DP
	implicit none
	!----------------------------------------------------------------------------
	integer  :: ng(3)
	real(DP) :: x(3)
	logical  :: in_cry_cell
	!----------------------------------------------------------------------------
	logical  :: condition
	!----------------------------------------------------------------------------
	condition = .true.
	condition = condition .and. x(1) >= real(ng(1)    , DP)
	condition = condition .and. x(1) <  real(ng(1) + 1, DP)
	condition = condition .and. x(2) >= real(ng(2)    , DP)
	condition = condition .and. x(2) <  real(ng(2) + 1, DP)
	condition = condition .and. x(3) >= real(ng(3)    , DP)
	condition = condition .and. x(3) <  real(ng(3) + 1, DP)
	!----------------------------------------------------------------------------
	in_cry_cell = condition
	!----------------------------------------------------------------------------
	return
end function
