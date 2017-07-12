subroutine search_pg(nop, op, npg)
	implicit none
	!----------------------------------------------------------------------------
	integer, intent( in) :: nop
	logical, intent( in) :: op(8)
	!----------------------------------------------------------------------------
	integer, intent(out) :: npg
	!----------------------------------------------------------------------------
	npg=1
	if      (nop==1) then
		npg=1
	else if (nop==8) then
		npg=16
	else if (nop==2) then
		if      (op(1) .and. op(5)) then
		!-------------------------------------------------------------------------
		! CI
		!
			npg=2
		!-------------------------------------------------------------------------
		! CS
		!
		else if (op(1) .and. op(6)) then
			npg=3
		else if (op(1) .and. op(7)) then
			npg=4
		else if (op(1) .and. op(8)) then
			npg=5
		!-------------------------------------------------------------------------
		! C2
		!
		else if (op(1) .and. op(2)) then
			npg=6
		else if (op(1) .and. op(3)) then
			npg=7
		else if (op(1) .and. op(4)) then
			npg=8
		!-------------------------------------------------------------------------
		end if
	else if (nop==4) then
		!-------------------------------------------------------------------------
		! D2
		!
		if      (op(1) .and. op(2) .and. op(3) .and. op(4)) then
			npg=9
		!-------------------------------------------------------------------------
		! C2V
		!
		else if (op(1) .and. op(2) .and. op(7) .and. op(8)) then
			npg=10
		else if (op(1) .and. op(3) .and. op(6) .and. op(8)) then
			npg=11
		else if (op(1) .and. op(4) .and. op(6) .and. op(7)) then
			npg=12
		!-------------------------------------------------------------------------
		! C2H
		!
		else if (op(1) .and. op(2) .and. op(5) .and. op(6)) then
			npg=13
		else if (op(1) .and. op(3) .and. op(5) .and. op(7)) then
			npg=14
		else if (op(1) .and. op(4) .and. op(5) .and. op(8)) then
			npg=15
		end if
	end if
	!----------------------------------------------------------------------------
	return
end subroutine
