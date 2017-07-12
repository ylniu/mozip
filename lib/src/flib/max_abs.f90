function max_abs(n,v)
	use kinds
	implicit none
	integer   :: n
	real(DP)  :: v(n)
	real(DP)  :: max_abs
	integer   :: i, maxi
	real(DP)  :: maxv1, mabs
	mabs = 0.D0
	do i=1, n
		if(mabs < abs(v(i))) then
			mabs = abs(v(i))
			maxv1 = v(i)
			maxi  = i
		end if
	end do
	max_abs = maxv1
	return
end function