subroutine swapn(n,v1,v2)
	use kinds
	implicit none
	integer  :: n
	real(DP) :: v1(n), v2(n), v(n)
	!
	v=v1
	v1=v2
	v2=v
	!
	return
end subroutine
