subroutine std_to_inp(natom, mass, x_std, x_inp, rot, d)
	use kinds, only: DP
	use math,  only: inverse3
	implicit none
	!----------------------------------------------------------------------------
	integer , intent( in) :: natom
	real(DP), intent( in) :: x_inp(3,natom)
	real(DP), intent( in) :: x_std(3,natom)
	real(DP), intent( in) :: mass (  natom)
	real(DP), intent(out) :: rot(3,3), d(3)
	!----------------------------------------------------------------------------
	integer               :: i
	real(DP)              :: rot_inp(3,3), d_inp(3)
	real(DP)              :: rot_std(3,3), d_std(3), eps
	real(DP)              :: invrot(3,3), dd(3), dr, dx, dy, dz, aa(3,3)
	real(DP)              :: x_inp1(3,natom)
	real(DP)              :: x_std1(3,natom)
	!----------------------------------------------------------------------------
	eps = 1.D-4
	!----------------------------------------------------------------------------
	call get_inertm_rot_d(natom, mass, x_inp, rot_inp, d_inp)
	call get_inertm_rot_d(natom, mass, x_std, rot_std, d_std)
	!----------------------------------------------------------------------------
	x_inp1 = x_inp
	x_std1 = x_std
	do i=1, natom
		x_inp1(:,i) = x_inp1(:,i) - d_inp
		x_std1(:,i) = x_std1(:,i) - d_std
	end do
	!----------------------------------------------------------------------------
	call rotn(natom, rot_inp, x_inp1)
	call rotn(natom, rot_std, x_std1)
	dx = 0.D0
	dy = 0.D0
	dz = 0.D0
	dr = 0.D0
	do i=1, natom
		dd = x_inp1(:,i) - x_std1(:,i)
		dx = dx + dd(1)**2
		dy = dy + dd(2)**2
		dz = dz + dd(3)**2
		dr = dr + dot_product(dd, dd)
	end do
	dx = sqrt(dx) / natom
	dy = sqrt(dy) / natom
	dz = sqrt(dz) / natom
	dr = sqrt(dr) / natom
	if (dr >= eps) then
! 		do i=1, natom
! 			write(*,'(i6, 3f15.6, 4x, 3f15.6)') i, x_inp1(:,i), x_std1(:,i)
! 		end do
! 		write(*,'(4f15.6)') dx, dy, dz, dr
		aa      =  0.D0
		aa(1,1) = -1.D0
		aa(2,2) = -1.D0
		aa(3,3) = -1.D0
		if ( dx < eps ) aa(1,1) = 1.D0
		if ( dy < eps ) aa(2,2) = 1.D0
		if ( dz < eps ) aa(3,3) = 1.D0
! 		do i=1, 3
! 			write(*,'(3f15.6)') (aa(i,j), j=1, 3)
! 		end do
		rot_std = matmul(aa, rot_std)
	end if	
	!----------------------------------------------------------------------------
	invrot = inverse3(rot_inp)
	rot    = matmul(invrot, rot_std)
	d_std  = matmul(rot, d_std)
	d      = d_inp - d_std
	!----------------------------------------------------------------------------
	return
end subroutine
