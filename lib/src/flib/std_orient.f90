subroutine std_orient(n, x, nop, nptg, op, std)
	use kinds
	use symmetry, only: symm_op
	implicit none
	!----------------------------------------------------------------------------
	! Input  variables
	!
	integer , intent(in   ) :: n
	integer , intent(in   ) :: nop
	logical , intent(in   ) :: std
	logical , intent(in   ) :: op(8)
	!----------------------------------------------------------------------------
	! Output variables
	!
	integer , intent(  out) :: nptg
	real(DP), intent(inout) :: x(3,n)
	!----------------------------------------------------------------------------
	integer                 :: i
	integer                 :: isplxy, isplzx
	integer                 :: ifnoy , ifnoz
	!----------------------------------------------------------------------------
	!************************ determine point group ********************
	! C4x : symm_op(1, 1, 10)
	! C4y : symm_op(1, 1,  9)
	! C4z : symm_op(1, 1,  8)
	!
	if      (nop.eq.1) then
		! no symmetry
		nptg= 1
	else if (nop.eq.8) then
		!**** D2H ****
		nptg= 5
	else if (nop.eq.2) then
		if      (op(8)) then
			!*** Cs symmetry, is yz plane, ensure that its the xy plane ****
			nptg= 2
			if (std) call rotn(n, symm_op(1,1,10), x)
		else if (op(7)) then
			!*** Cs symmetry, is xz plane, ensure that its the xy plane ****
			nptg= 2
			if (std) call rotn(n, symm_op(1,1,11), x)
		else if (op(6)) then
			!*** Cs symmetry ****
			nptg= 2
		else if (op(5)) then
			!*** Ci symmetry ****
			nptg= 6
		else if (op(4)) then
			!**** C2 symmetry, is x axis, ensure that its about z axix ****
			nptg= 3
			if (std) call rotn(n, symm_op(1,1,10), x)
		else if (op(3)) then
			!**** C2 symmetry, is y axis, ensure that its about z axix ****
			nptg= 3
			if (std) call rotn(n, symm_op(1,1,11), x)
		else if (op(2)) then
			!**** C2 symmetry ****
			nptg= 3
		end if
	else ! nop = 4
		if (op(5)) then
			!**** C2h symmetry, ensure C2 axis is z axis ****
			nptg= 7
			if      (op(3)) then
				if (std) call rotn(n, symm_op(1,1,11), x)
			else if (op(4)) then
				if (std) call rotn(n, symm_op(1,1,10), x)
			end if
		else if (op(7).or.op(8)) then
			!**** C2V symmetry, ensure that C2 axis is Z axis ****
			nptg= 4
			if      (op(3)) then
				if (std) call rotn(n, symm_op(1,1,11), x)
			else if (op(4)) then
				if (std) call rotn(n, symm_op(1,1,10), x)
			end if
			!**** if molecule is in zx planar, ensure its the yz plane ****
			isplzx = 1
			isplxy = 1
			do i= 1, n
				if (abs(x(2,i)).gt.1.D-6) isplzx = 0
				if (abs(x(3,i)).gt.1.d-6) isplxy = 0
			end do
			if (isplzx.eq.1) then
				if (std) call rotn(n, symm_op(1,1,9), x)
			end if
			if (isplxy.eq.1) then
				if (std) call rotn(n, symm_op(1,1,10), x)
			end if
		else
			!**** d2 symmetry ****
			nptg= 8
		end if
		!**** d2h/c2v if planar make sure its yz plane ****
		if (nptg.eq.4 .or. nptg.eq.5) then
			ifnoy= 1
			ifnoz= 1
			do i= 1, n
				if (abs(x(2,i)).gt.1.d-6) ifnoy= 0
				if (abs(x(3,i)).gt.1.d-6) ifnoz= 0
			end do
			if (ifnoy.eq.1) then
				if (std) call rotn(n, symm_op(1,1,9), x)
			end if
			if (ifnoz.eq.1) then
				if (std) call rotn(n, symm_op(1,1,10), x)
			end if
		end if
	end if
	!----------------------------------------------------------------------------
	return
end subroutine
