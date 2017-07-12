subroutine GetMPINodePoints(np,sn,en,cn,n1,n2)
	implicit none
	!-----------------------------------------------------------------------------
	integer(4) :: np
	integer(4) :: n1,n2
	integer(4) :: sn(0:np-1),en(0:np-1),cn(0:np-1)
	integer(4) :: i,t,n,m,m1
	!-----------------------------------------------------------------------------
	n=n2-n1+1
	!----------------------------------------------------------------------------
	!
	! 1. numprocs is the number of cpus !
	!
	!****************************************************************************
	!
	! 2. If you want to calculate with time 1,2,...,NTau,
	!    then the actual total number of time points is NTau + 1.
	!
	!****************************************************************************
	!
	! 3. cal_num(0:numprocs-1) is an array.
	!
	!****************************************************************************
	!
	! 4. If ( NTau = 99 ) and ( numprocs = 10 ) , then NTau + 1 = 100,
	!    cal_num(0) = 100 / 10 = 10
	!    cal_num(1) = 100 / 10 = 10
	!    ...
	!    cal_num(9) = 100 / 10 = 10
	!
	!****************************************************************************
	!
	! 5. If ( NTau = 101 ) and ( numprocs = 10 ) , then NTau + 1 = 102,
	!    MOD(102,10)=2
	!    cal_num(0) = 101 / 10     = 10
	!    cal_num(1) = 101 / 10     = 10
	!    ...
	!    cal_num(7) = 101 / 10     = 10
	!    cal_num(8) = 101 / 10 + 1 = 11
	!    cal_num(9) = 101 / 10 + 1 = 11
	!
	!    cal_num(i) is the number of calculating points on node i.
	!    This code makes master node 0 calculate less points.
	!
	!----------------------------------------------------------------------------
	!
	m  = n/np
	m1 = m+1
	cn = m
	!
	if ( MOD(n,np) == 0) then
		t=n1
		do i = 0,np - 1
			sn(i) = t
			en(i) = sn(i) + cn(i) - 1
			t = t + m
		end do
	else
		t=n1
		do i = 0,np - MOD(n,np) - 1
			sn(i) = t
			en(i) = sn(i) + cn(i) - 1
			t = t + m
		end do
		!
		do i = np - MOD(n,np), np - 1
			cn(i) = m1
			sn(i) = t
			en(i) = sn(i) + cn(i) - 1
			t = t + m1
		end do
	end if
	!
	return
end subroutine
