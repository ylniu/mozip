program mol_shg
	use kinds, only:DP
	!----------------------------------------------------------------------------
	integer                  :: fid, istat
	integer                  :: nstate
	integer                  :: i, j, k
	integer                  :: n, n1
	integer                  :: iw, nw
	real(DP)                 :: wmin, wmax, dw, w, w2
	real(DP)   , allocatable :: ene(:)
	complex(DP), allocatable :: dam(:)
	real(DP)   , allocatable :: dipd(:,:)
	real(DP)   , allocatable :: dip(:,:,:)
	real(DP)   , allocatable :: dip3(:,:,:,:,:)
	real(DP)   , allocatable :: dipd3(:,:,:,:)
	complex(DP)              :: II
	complex(DP), allocatable :: chi(:,:,:,:,:)
	complex(DP), allocatable :: chit(:,:,:,:)
	character(200)           :: finp, fdip, fchi
	character(200)           :: line, tmp
	!----------------------------------------------------------------------------
	namelist /control/ fdip, wmin, wmax, dw, damp, fchi
	!----------------------------------------------------------------------------
	fid = 1
	!----------------------------------------------------------------------------
	call getarg(1,finp)
	!----------------------------------------------------------------------------
	nw = nint((wmax - wmin) / dw) + 1
	!----------------------------------------------------------------------------
	open(fid, file=finp, status="old")
		read(fid, control)
	close(fid)
	!----------------------------------------------------------------------------
	II = cmplx(0.D0, 1.D0, DP)
	dam=damp
	!----------------------------------------------------------------------------
	open(fid, file=fdip, status="old")
		read(fid,*) tmp, tmp, tmp, nstate
		read(fid,*)
		!-------------------------------------------------------------------------
		allocate(ene  (                 nstate    ))
		allocate(dip  (3,       nstate, nstate    ))
		allocate(dipd (3,               nstate    ))
		allocate(dam  (                 nstate    ))
		allocate(dip3 (3, 3, 3, nstate, nstate    ))
		allocate(dipd3(3, 3, 3,         nstate    ))
		allocate(chi  (3, 3, 3,         nstate, nw))
		allocate(chit (3, 3, 3                , nw))
		!-------------------------------------------------------------------------
		ene = 0.D0
		dip = 0.D0
		!-------------------------------------------------------------------------
		do while (.true.)
			read(fid, '(a)', iostat=istat) line
			if (istat==0) then
				read(line,*) tmp
				if (trim(tmp)=="state") then
					read(line,*) tmp, tmp, i, tmp, tmp, tmp, ene(i)
				else
					read(line,*) i, j, (dip(k,j,i), k=1, 3)
					do k=1, 3
						dip(k,i,j) = dip(k,j,i)
					end do
				end if
			else
				exit
			end if
		end do
		!-------------------------------------------------------------------------
	close(fid)
	!----------------------------------------------------------------------------
	do n=nstate, 1, -1
		dipd(:,n) = dip(:,n,n) - dip(:,1,1)
		ene (  n) = ene(    n) - ene(    1)
		dam (  n) = II * damp
	end do
	!----------------------------------------------------------------------------
	do n=1, nstate
		do n1=1, nstate
			do i=1, 3
				do j=1, 3
					do k=1, 3
						dip3 (i,j,k,n,n1) = dip(i,1,n1) * dip(j,n1,n) * dip(k,n,1)
					end do
				end do
			end do
		end do
	end do
	!----------------------------------------------------------------------------
	do n=1, nstate
		do i=1, 3
			do j=1, 3
				do k=1, 3
					dipd3(i,j,k,n) = dip(i,1,n) * dip(j,1,n) * dipd(k,n)
				end do
			end do
		end do
	end do
	!----------------------------------------------------------------------------
	chi  = 0.D0
	chit = 0.D0
	do iw=1, nw
		w  = wmin+(iw-1) * dw
		w2 = 2.D0 * w
		do n1=1, nstate
			!----------------------------------------------------------------------
			do n=1, nstate
				!-------------------------------------------------------------------
				chi = chi + ( dip3 (j,i,k,n,n1) + dip3 (k,i,j,n,n1) ) &
					& / ( ( ene(n1) - w  - dam(n1) ) * ( ene(n) + w  - dam(n) ))
				chi = chi + ( dip3 (j,i,k,n,n1) + dip3 (k,i,j,n,n1) ) &
					& / ( ( ene(n1) + w  - dam(n1) ) * ( ene(n) - w  - dam(n) ))
				!-------------------------------------------------------------------
				chi = chi + ( dip3 (i,j,k,n,n1) + dip3 (k,i,j,n,n1) ) &
					& / ( ( ene(n1) + w2 - dam(n1) ) * ( ene(n) + w  - dam(n) ))
				chi = chi + ( dip3 (i,j,k,n,n1) + dip3 (k,i,j,n,n1) ) &
					& / ( ( ene(n1) - w2 - dam(n1) ) * ( ene(n) - w  - dam(n) ))
				!-------------------------------------------------------------------
				chi = chi + ( dip3 (j,k,i,n,n1) + dip3 (k,j,i,n,n1) ) &
					& / ( ( ene(n1) - w  - dam(n1) ) * ( ene(n) - w2 - dam(n) ))
				chi = chi + ( dip3 (j,k,i,n,n1) + dip3 (k,j,i,n,n1) ) &
					& / ( ( ene(n1) + w  - dam(n1) ) * ( ene(n) + w2 - dam(n) ))
				!-------------------------------------------------------------------
			end do
			!----------------------------------------------------------------------
			chi = chi + ( dipd3 (j,k,i,n1) * ( ene(n1)**2 - w2**2 ) &
				& + ( dipd3(i,k,j,n1) + dipd3(i,i,k,n1) ) * ( ene(n1)**2 + 2 * w**2 ) ) &
				& / ( ( ( ene(n1) - dam(n1) )**2 - w**2 ) * ( ( ene(n1) - dam(n1) )**2 - w2**2 ) )
			!----------------------------------------------------------------------
		end do
	end do
	!----------------------------------------------------------------------------
	open(fid, file=fchi)
		
	close(fid)
	!----------------------------------------------------------------------------
	deallocate(ene  )
	deallocate(dip  )
	deallocate(dipd )
	deallocate(dam  )
	deallocate(dip3 )
	deallocate(dipd3)
	deallocate(chi  )
	deallocate(chit )
	!----------------------------------------------------------------------------
	stop
end
