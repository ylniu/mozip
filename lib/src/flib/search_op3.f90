subroutine search_op3 (n3, jreln3, name_n3, ir_lw, ir_hi, c)
	!----------------------------------------------------------------------------
	! Group Theory and Chemistry
	! David M. Bishop
	! 1973
	! P172, equation (9-4.2)
	!
	use kinds    , only: DP
	use symmetry1, only: ir_op, nrep, ir_table, pg_name, ir_name
	use symmetry1, only: nop, npg
	!----------------------------------------------------------------------------
	implicit none
	!----------------------------------------------------------------------------
	! Input  variables
	!
	integer     , intent(in ) :: n3
	character(*), intent(in ) :: name_n3(n3)
	!----------------------------------------------------------------------------
	! Output variables
	!
	integer     , intent(in ) :: jreln3(n3,8)
	integer     , intent(out) :: ir_lw(8)
	integer     , intent(out) :: ir_hi(8)
	
	!----------------------------------------------------------------------------
	! Local variables
	!
	integer                   :: iop, jop
	integer                   :: ibas, iout
	integer                   :: i3,j3,r3,k
	integer                   :: istr
	integer                   :: ir, irs, nrs, ig, nr, ng
	integer                   :: bas_n3(n3)
	integer                   :: basis (8, n3)
	integer                   :: ibas_iat(8)
	integer                   :: nbasis(n3)
	integer                   :: nstr(n3)
	real(DP)                  :: stwt(n3)
	integer                   :: ir_n3(n3)
	integer                   :: irs_n3(n3)
	integer                   :: ir_n(nop)
	integer                   :: ci(n3,n3)
	real(DP)                  :: c(n3,n3)
	character( 1)             :: sgn(-1:1)
	character(80)             :: fmt, fsymm
	logical                   :: find, dbg, isinc(n3)
	!----------------------------------------------------------------------------
	dbg    = .false.
	!----------------------------------------------------------------------------
	sgn(-1)="-"
	sgn( 1)="+"
	!----------------------------------------------------------------------------
	k      = 0
	bas_n3 = 0
	nr     = nrep(npg)
	ng     = nr
	ir_lw  = 0
	ir_hi  = 0
	nrs    = 0
	iout   = 56
	basis  = 0
	fsymm  = "symm.log"
	!----------------------------------------------------------------------------
	! Project to each representation
	!
	do ir=1, nr
		irs=0
		isinc=.false.
		iop=ir_op(ir,npg)
		nbasis=0
		do i3=1, n3
			bas_n3=0
			do ig=1, ng
				jop        = ir_op(ig,npg)
				j3         = jreln3(i3,jop)
				r3         = abs(j3)
				bas_n3(r3) = bas_n3(r3) + sign(1,j3) * ir_table(ig,ir,npg)
			end do
			find=.false.
			ibas=0
			if (.not.isinc(i3)) then
				do r3=1, n3
					if (bas_n3(r3)/=0) then
						find=.true.
						ibas=ibas+1
						ibas_iat(ibas) = sign(1,bas_n3(r3)) * r3
						isinc(r3) = .true.
					end if
				end do
			end if
			if (find) then
				irs=irs+1
				nrs=nrs+1
				!-------------------------------------------------------------------
				nstr  (nrs)  = ibas
				ir_n3 (nrs)  = ir
				irs_n3(nrs)  = irs
				basis(:,nrs) = ibas_iat
				!-------------------------------------------------------------------
				ir_hi(ir) = nrs
				if (ir_lw(ir)==0) then
					if (ir==1) then
						ir_lw(ir) = 1
					else
						ir_lw(ir) = ir_hi(ir-1) + 1
					end if
				end if
			end if
		end do
	end do
	!----------------------------------------------------------------------------
	do ir=1, nr
		ir_n(ir) = ir_hi(ir) - ir_lw(ir) + 1
	end do
	!----------------------------------------------------------------------------
	! Weights for transform vectors
	!
	do i3= 1, n3
		stwt(i3)= sqrt (1.D0/nstr(i3))
	end do
	!----------------------------------------------------------------------------
	c=0.D0
	ci=0
	do ir=1, nr
		do i3=ir_lw(ir), ir_hi(ir)
			if (ir_lw(ir)>0 .and. ir_hi(ir)>0) then
				do istr=1, nstr(i3)
					j3 = abs(basis(istr,i3))
					c(i3,j3)  = stwt(i3) * sign(1, basis(istr,i3))
					ci(i3,j3) =            sign(1, basis(istr,i3))
				end do
			end if
		end do
	end do
	!----------------------------------------------------------------------------
	if (dbg) then
		open(iout, file=fsymm)
		write(iout,'("Point group", a8)') pg_name(npg)
		do ir=1, nr
			write(iout,'("IRREP", i6, x, a4)') ir, ir_name(ir,npg)
		end do
		!-------------------------------------------------------------------------
		write(iout,*)
		write(iout,'("Basis:")')
		do ir=1, nr
			write(iout,*)
			write(iout,'("IRREP", i3, 4x, a4)') ir, ir_name(ir,npg)
			do i3=ir_lw(ir), ir_hi(ir)
				irs=irs_n3(i3)
				write(fmt,*) nstr(i3)
				!-------------------------------------------------------------------
				write(iout,'("Project on", i8)') i3
				write(iout,'("Project on", a8)') trim(name_n3(i3))
				!-------------------------------------------------------------------
				write(iout,'(i4,2i4, 2x, '//trim(fmt)//'i8)') i3, irs, nstr(i3), &
					(basis(istr,i3), istr=1, nstr(i3))
				!-------------------------------------------------------------------
				write(iout,'(i4,2i2, 2x, '//trim(fmt)//'a8)') i3, irs, nstr(i3), &
					(trim(sgn(sign(1,basis(istr,i3)))//name_n3(abs(basis(istr,i3)))), istr=1, nstr(i3))
				!-------------------------------------------------------------------
			end do
		end do
		write(iout,*)
		write(iout,'(   a)') "Transformation matrix: c"
		write(iout,'(2x,a)') "c . x  = x_symm"
		write(iout,'(2x,a)') "x      = [1x , 1y , 1z , 2x , 2y , 2z , ......]^T"
		write(iout,'(2x,a)') "x_symm = [1x', 1y', 1z', 2x', 2y', 2z', ......]^T"
		write(iout,*)
		write(fmt,*) n3
		do i3=1, n3
			write(iout, '("i = ", i0)') i3
			write(iout, '(10i8  )') (ci(i3,j3), j3=1, n3)
			write(iout, '(10f8.3)') (c (i3,j3), j3=1, n3)
		end do
		close(iout)
	end if
	!----------------------------------------------------------------------------
	return
end
