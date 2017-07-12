subroutine relaxTip(n, x, fb, ft, cy, conv, fm, fma, f1_eps, v, dt, acoef)
	use kinds     , only: DP
	use module_afm, only: relaxAlg, tip
	use param     , only: au2a
	use math      , only: dot_product2
	use module_afm, only: maxIters
	implicit none
	!--------------------------------------------------------------------
	! Input  variables
	!
	integer     , intent(in )   :: n
	real(DP)    , intent(in )   :: f1_eps
	!--------------------------------------------------------------------
	! Output variables
	!
	integer     , intent(out)   :: cy
	logical     , intent(out)   :: conv
	real(DP)    , intent(out)   :: dt
	real(DP)    , intent(out)   :: acoef
	real(DP)    , intent(out)   :: x (3,n)
	real(DP)    , intent(out)   :: v (3,n)
	real(DP)    , intent(out)   :: fb(3,n)
	real(DP)    , intent(out)   :: ft(3,n)
	real(DP)    , intent(out)   :: fm(3  )
	real(DP)    , intent(out)   :: fma(3 )
	!--------------------------------------------------------------------
	! Local  variables
	!
	integer                     :: i, j
	real(DP)                    :: fn
	real(DP)    , allocatable   :: f  (:,:)
	real(DP)    , allocatable   :: f1 (:,:)
	real(DP)    , allocatable   :: f2 (:,:)
	!--------------------------------------------------------------------
	! Debug  variables
	!
	!--------------------------------------------------------------------
	allocate(f2(3,n))
	allocate(f (3,n))
	allocate(f1(3,n))
	!--------------------------------------------------------------------
	conv = .false.
	!--------------------------------------------------------------------
	do i=0, maxIters
		call get_force(n, x, fb, ft, f2)
		f = fb + ft
		if (all(tip%opt=="F")) return
		!-----------------------------------------------------------------
		f1 = 0.0_DP
		do j=1, n
			if (tip%opt(j)=="T") f1(:,j) = f(:,j)
		end do
		!-----------------------------------------------------------------
		if (relaxAlg == 1) then
			!--------------------------------------------------------------
			! m3 is the mass(3,n) of tip
			!
			call move_fire(n, tip%m3, f1, x, v, dt, acoef)
		else
			call move_relax()
		end if
		!-----------------------------------------------------------------
		fn = sqrt(dot_product2(f1,f1))
		!-----------------------------------------------------------------
		if ( fn < f1_eps ) then
			cy = i
			conv=.true.
			exit
		end if
	end do
	!--------------------------------------------------------------------
	if ( .not. conv ) then
		cy = i - 1
	end if
	!--------------------------------------------------------------------
	fm =0.0_DP
	fma=0.0_DP
	do i=1, n
		if (tip%opt_sum(i)=="T") then
			fm  = fm  + fb(:,i)
			fma = fma + fb(:,i) + ft(:,i)
		end if
	end do
	!--------------------------------------------------------------------
	deallocate(f1)
	deallocate(f2)
	deallocate(f )
	!--------------------------------------------------------------------
	return
end subroutine
