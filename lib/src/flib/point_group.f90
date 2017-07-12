subroutine point_group(disp_sign, n, x, nat, mass, tol, std, &
	n6, n_calc, n_calc_n6, m_calc, f_calc, m_symm, nptg, op, jrels)
	use kinds
	use symmetry
	implicit none
	!----------------------------------------------------------------------------
	! Input variables
	!
	integer     , intent(in ) :: disp_sign
	integer     , intent(in ) :: n
	integer     , intent(in ) :: n6
	integer     , intent(in ) :: nat(n)
	real(DP)    , intent(in ) :: mass(n)
	real(DP)    , intent(in ) :: tol
	real(DP)    , intent(in ) :: x(3,n)
	logical     , intent(in ) :: std
	!----------------------------------------------------------------------------
	! Output variables
	!
	integer     , intent(out) :: n_calc, nptg
	integer     , intent(out) :: n_calc_n6(n6)
	integer     , intent(out) :: m_symm(n6)
	integer     , intent(out) :: m_calc(n6)
	logical     , intent(out) :: f_calc(n6)
	logical                   :: op(8)
	!----------------------------------------------------------------------------
	! Logical variables
	!
	integer                   :: i
	integer                   :: nop
	integer                   :: jrels(n,8)
	!----------------------------------------------------------------------------
	nop   = 0
	jrels = 0
	do i= 1, 8
		!-------------------------------------------------------------------------
		! op(1) = .T., E
		! op(2) = .T., C2z
		! op(3) = .T., C2y
		! op(4) = .T., C2x
		! op(5) = .T., I
		! op(6) = .T., Sxy
		! op(7) = .T., Szx
		! op(8) = .T., Syz
		!
		! There 11 operations, but we can only search 8 operations.
		!
		call search_op (n, x, nat, mass, symm_op(1,1,i), jrels(1,i), op(i), tol)
		if (op(i)) nop= nop + 1
	end do
	!----------------------------------------------------------------------------
	call std_orient(n, x, nop, nptg, op, std)
	!----------------------------------------------------------------------------
	do i= 1,8
		call search_op (n, x, nat, mass, symm_op(1,1,i), jrels(1,i), op(i), tol)
	end do
	!----------------------------------------------------------------------------
	call search_symm_xyz (disp_sign, n, x, symm_op, jrels, op, tol, n6, &
		n_calc, f_calc, n_calc_n6, m_calc, m_symm)
	!----------------------------------------------------------------------------
	return
end subroutine
