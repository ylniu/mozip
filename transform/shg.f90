program shg
	use kinds, only: DP
	implicit none
	!----------------------------------------------------------------------------
	integer               :: fid, nl, nw, i, j, k, iw, irecl
	integer               :: nq, iq
	real(DP)              :: PI
	real(DP)              :: dq, qmin, qmax, fa(3), cosa(3)
	real(DP), allocatable :: q(:), p(:,:,:), pr(:,:), pd(:,:)
	real(DP), allocatable :: sq2(:), cq2(:), cs(:)
	real(DP), allocatable :: w(:), chi2i(:,:,:,:)
	!----------------------------------------------------------------------------
	character(200) :: finp, foutr, fmt, foutx, fouty, foutz, foutd
	!----------------------------------------------------------------------------
	integer, external :: number_of_lines
	!----------------------------------------------------------------------------
	! initialization
	!
	PI = 2.D0 * acos(-1.D0)
	foutd = "shg_d.out"
	foutr = "shg_r.out"
	foutx = "shg_x.out"
	fouty = "shg_y.out"
	foutz = "shg_z.out"
	!----------------------------------------------------------------------------
	call getarg(1, finp)
	!----------------------------------------------------------------------------
	dq    = 1.D0
	qmin  = 0.D0
	qmax  = 360.D0
	fa(1) = 30.D0
	fa(2) = 90.D0
	fa(3) = 60.D0
	!----------------------------------------------------------------------------
	dq    = dq   * PI / 180.D0
	qmin  = qmin * PI / 180.D0
	qmax  = qmax * PI / 180.D0
	nq    = (qmax - qmin ) / dq + 1
	!----------------------------------------------------------------------------
	fa    = fa   * PI / 180.D0
	!----------------------------------------------------------------------------
	cosa  = cos(fa)
	!----------------------------------------------------------------------------
	allocate(q  (nq))
	allocate(sq2(nq))
	allocate(cq2(nq))
	allocate(cs (nq))
	!----------------------------------------------------------------------------
	do i=1, nq
		q  (i) = qmin + (i-1) * dq
		sq2(i) = sin(q(i))**2
		cq2(i) = cos(q(i))**2
		cs (i) = cos(q(i)) * sin(q(i))
	end do
	!----------------------------------------------------------------------------
	open(fid, file=finp, status="old") 
		nl=number_of_lines(fid)
		nw = ( nl - 1 ) / 4
		allocate(w(nw))
		allocate(chi2i(3,3,3,nw))
		allocate(p(3,nq,nw))
		allocate(pr (nq,nw))
		allocate(pd (nq,nw))
		!-------------------------------------------------------------------------
		rewind(fid)
		read(fid,*)
		do iw=1, nw
			read(fid,*)w(iw)
			do j=1, 3
				read(fid,*) chi2i(:,:,j, iw)
			end do
		end do
		!-------------------------------------------------------------------------
	close(fid)
	!----------------------------------------------------------------------------
	pr=0.D0
	pd=0.D0
	do iw=1, nw
		do iq=1, nq
			do k=1, 3
				if (w(iw)==1.75D0 .and. iq==1) then
					!write(*,*) k, chi2i(k,1,1,iw), chi2i(k,2,2,iw)
				end if
				p(k, iq, iw) = chi2i(k,1,1,iw) * cq2(iq) &
								&+ chi2i(k,1,2,iw) * cs (iq) &
								&+ chi2i(k,2,1,iw) * cs (iq) &
								&+ chi2i(k,2,2,iw) * sq2(iq)
				pr(  iq, iw) = pr(iq, iw) + p(k, iq, iw)**2
				pd(  iq, iw) = pd(iq, iw) + p(k, iq, iw) * cosa(k)
			end do
			pr(iq, iw) = pr(iq, iw)**2
			pd(iq, iw) = pd(iq, iw)**2
		end do
	end do
	!----------------------------------------------------------------------------
	irecl = 16 * (nw + 2)
	write(fmt,*) nw
	!----------------------------------------------------------------------------
	open(fid, file=foutd, recl=irecl)
		write(fid, '(a10, '//trim(fmt)//'f16.6)') "#q", (w(iw), iw=1, nw)
		do iq=1, nq
			write(fid, '(f10.2, '//trim(fmt)//'es16.7)') &
				& q(iq) * 180.D0 / PI, (pd(iq, iw), iw=1, nw)
		end do
	close(fid)
	!----------------------------------------------------------------------------
	open(fid, file=foutr, recl=irecl)
		write(fid, '(a10, '//trim(fmt)//'f16.6)') "#q", (w(iw), iw=1, nw)
		do iq=1, nq
			write(fid, '(f10.2, '//trim(fmt)//'es16.7)') &
				& q(iq) * 180.D0 / PI, (pr(iq, iw), iw=1, nw)
		end do
	close(fid)
	!----------------------------------------------------------------------------
	open(fid, file=foutx, recl=irecl)
		write(fid, '(a10, '//trim(fmt)//'f16.6)') "#q", (w(iw), iw=1, nw)
		do iq=1, nq
			write(fid, '(f10.2, '//trim(fmt)//'es16.7)') &
				& q(iq) * 180.D0 / PI, (p(1,iq, iw), iw=1, nw)
		end do
	close(fid)
	!----------------------------------------------------------------------------
	open(fid, file=fouty, recl=irecl)
		write(fid, '(a10, '//trim(fmt)//'f16.6)') "#q", (w(iw), iw=1, nw)
		do iq=1, nq
			write(fid, '(f10.2, '//trim(fmt)//'es16.7)') &
				& q(iq) * 180.D0 / PI, (p(2,iq, iw), iw=1, nw)
		end do
	close(fid)
	!----------------------------------------------------------------------------
	open(fid, file=foutz, recl=irecl)
		write(fid, '(a10, '//trim(fmt)//'f16.6)') "#q", (w(iw), iw=1, nw)
		do iq=1, nq
			write(fid, '(f10.2, '//trim(fmt)//'es16.7)') &
				& q(iq) * 180.D0 / PI, (p(3,iq, iw), iw=1, nw)
		end do
	close(fid)
	!----------------------------------------------------------------------------
	deallocate(w    )
	deallocate(p    )
	deallocate(pr   )
	deallocate(pd   )
	deallocate(q    )
	deallocate(sq2  )
	deallocate(cq2  )
	deallocate(cs   )
	deallocate(chi2i)
	!----------------------------------------------------------------------------
	stop
	!----------------------------------------------------------------------------
end
