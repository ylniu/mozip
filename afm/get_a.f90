subroutine get_a(x1, x2, x3, a)
	use kinds, only:DP
	implicit none
	!--------------------------------------------------------------------
	real(DP) :: PI
	real(DP) :: x1(3), x2(3), x3(3), a
	real(DP) :: dx12(3), dx32(3), r12, r32, r13
	!--------------------------------------------------------------------
	PI = acos(-1.0_DP)
	!--------------------------------------------------------------------
	dx12 = x1 - x2
	dx32 = x3 - x2
	!--------------------------------------------------------------------
	r12  = sqrt(dot_product(dx12, dx12))
	r32  = sqrt(dot_product(dx32, dx32))
	r13  =      dot_product(dx12, dx32)
	!--------------------------------------------------------------------
	! dx12 . dx32 = |dx12| |dx32| cos a
	!
	a = r13 / ( r12 * r32 )
	a = acos(a)
	a = a * 180.0_DP / PI
	!--------------------------------------------------------------------
end subroutine