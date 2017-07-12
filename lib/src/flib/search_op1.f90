subroutine search_op1 (natom, x, nat, mass, jreln, jreln3, jreln9, nrs, tol)
	!--------------------------------------------------------------------
	! Group Theory and Chemistry
	! David M. Bishop
	! 1973
	! P172, equation (9-4.2)
	!
	use kinds
	use symmetry1, only: symm_op, nop, npg, op
	! use symmetry1, only: pg_name
	!--------------------------------------------------------------------
	implicit none
	!--------------------------------------------------------------------
	! Input  variables
	!
	integer,  intent(in ) :: natom
	integer,  intent(in ) :: nat(natom)
	real(DP), intent(in ) :: x(3, natom)
	real(DP), intent(in ) :: mass(natom)
	real(DP), intent(in ) :: tol
	!--------------------------------------------------------------------
	! Output variables
	!
	integer,  intent(out) :: jreln (natom  ,8)
	integer,  intent(out) :: jreln3(natom*3,8)
	integer,  intent(out) :: jreln9(natom*3,natom*3,8)
	integer,  intent(out) :: nrs(natom)
	!--------------------------------------------------------------------
	! Local variables
	!
	integer               :: iop
	integer               :: iat, jat
	integer               :: iar, ip, iq, ix
	integer               :: n3, i3, j3
	real(DP)              :: dr
	real(DP)              :: dx(3)
	real(DP)              :: x1(3, natom)
	logical               :: find
	!--------------------------------------------------------------------
	op    = .true.
	jreln = 0
	nop   = 0
	nrs   = 1
	n3    = natom * 3
	!--------------------------------------------------------------------
	do iop=1, 8
		x1 = x
		call rotn(natom, symm_op(1,1,iop), x1)
		!-------------------------------------------------------------------------
		do iat=1, natom
			!----------------------------------------------------------------------
			! find transformation from iat to jat through iop
			!
			find = .false.
			do jat=1, natom
				dx = x1(:,iat) - x(:,jat)
				dr = sqrt( dot_product( dx, dx ) )
				if ( dr<=tol .and. mass(jat)==mass(iat) .and. nat(jat)==nat(iat) ) then
					jreln(iat, iop) = jat
					jreln(jat, iop) = iat
					if (iat/=jat) nrs(iat) = 2
					find=.true.
					exit
				end if
			end do
			!----------------------------------------------------------------------
			if (.not. find) then
				jreln(:,iop) = 0
				op   (  iop) = .false.
				exit
			end if
			!----------------------------------------------------------------------
		end do
		if (op(iop)) nop = nop + 1
	end do
	!--------------------------------------------------------------------
	call search_pg(nop, op, npg)
	!--------------------------------------------------------------------
	! Group Theory and Chemistry
	! David M. Bishop
	! 1973
	! P172, equation (9-4.2)
	!
	! q1  q2  q3  ... ... qN
	! q1' q2' q3' ... ... qN'
	!
	jreln3= 0
	do iop=1, 8
		if (op(iop)) then
			do iat=1, natom
				iar=jreln(iat, iop)
				do ix=1, 3
					ip=(iat-1)*3 + ix
					iq=(iar-1)*3 + ix
					jreln3(ip,iop) = nint(symm_op(ix,ix,iop)) * iq
				end do
			end do
		end if
	end do
	!--------------------------------------------------------------------
	!
	jreln9= 0
	do iop=1, 8
		if (op(iop)) then
			do i3=1, n3
				do j3=1, n3
					jreln9(i3,j3,iop) = jreln3(i3,iop) * jreln3(j3,iop)
					!write(*,*) i3, j3, jreln3(i3,iop), jreln3(j3,iop), trim(pg_name(iop))
				end do
			end do
		end if
	end do
	!--------------------------------------------------------------------
	return
end
