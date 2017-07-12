program build_cluster
	use kinds, only: DP
	use math , only: rotmat3_seq
	implicit none
	!----------------------------------------------------------------------------
	integer       , parameter   :: MMAX=10000
	integer       , parameter   :: NMAX=1000
	integer       , parameter   :: NATS=1000
	integer                     :: fid, ios, iat, j, natomt, info
	integer                     :: imol
	integer                     :: nmol
	!----------------------------------------------------------------------------
	integer                     :: ord1(3, NMAX)
	integer                     :: ord2(3, NMAX)
	real(DP)                    :: rot1(3, NMAX)
	real(DP)                    :: rot2(3, NMAX)
	real(DP)                    :: vec1(3, NMAX)
	!----------------------------------------------------------------------------
	integer                     :: natom(  NMAX)
	real(DP)                    :: v1(  3)
	real(DP)                    :: c1(3,3)
	real(DP)                    :: c2(3,3)
	!----------------------------------------------------------------------------
	real(DP)                    :: x      (3, NATS, NMAX)
	character(  2)              :: symbol (   NATS, NMAX)
	real(DP)                    :: mass   (   NATS, NMAX)
	real(DP)                    :: xt     (3,       MMAX)
	character(  2)              :: symbolt(         NMAX)
	character(  3)              :: cent   (         NMAX)
	!----------------------------------------------------------------------------
	character(200)              :: finp, tmp1, tmp2, line
	character(200)              :: fname(NMAX)
	!----------------------------------------------------------------------------
	fid=1
	call getarg(1,finp)
	open(fid, file=finp, status="old")
		nmol = 0
		read(fid,'(a)',iostat=ios) line
		do while(ios==0)
			if(len(trim(line))/=0) then
				read(line,*) tmp1
				if (trim(tmp1)=="GEOM") then
					read(line,*) tmp1, tmp2
					if( trim(tmp2) == "yes" ) then
						nmol = nmol + 1
						read(fid,*,iostat=ios) tmp1, fname(nmol)
						read(fid,*,iostat=ios) tmp1, cent (nmol)
						read(fid,*,iostat=ios) tmp1, ord1(:,nmol)
						read(fid,*,iostat=ios) tmp1, rot1(:,nmol)
						read(fid,*,iostat=ios) tmp1, vec1(:,nmol)
						read(fid,*,iostat=ios) tmp1, ord2(:,nmol)
						read(fid,*,iostat=ios) tmp1, rot2(:,nmol)
					end if
				end if
			end if
			read(fid,'(a)',iostat=ios) line
		end do
	close(fid)
	!----------------------------------------------------------------------------
	j=0
	do imol=1, nmol
		c1 = rotmat3_seq(rot1(1,imol),ord1(1,imol))
		c2 = rotmat3_seq(rot2(1,imol),ord2(1,imol))
		v1 = vec1(:,imol)
		!-------------------------------------------------------------------------
		call get_gjf_natom(fname(imol), natom(imol))
		call get_gjf_coord(fname(imol), natom(imol), x(1,1,imol), symbol(1,imol))
		call get_mass     (             natom(imol), symbol(1,imol),mass(1,imol))
		!-------------------------------------------------------------------------
		if (trim(cent(imol))=="yes") then
			call com(natom(imol), mass(1,imol), x(1,1,imol), info)
		end if
		!-------------------------------------------------------------------------
		call rotn(natom(imol), c1, x(1,1,imol))
		call disp(natom(imol), v1, x(1,1,imol))
		call rotn(natom(imol), c2, x(1,1,imol))
		!-------------------------------------------------------------------------
		do iat=1, natom(imol)
			j=j+1
			xt     (:,j) = x     (:,iat,imol)
			symbolt(  j) = symbol(  iat,imol)
		end do
		!-------------------------------------------------------------------------
	end do
	natomt = j
	!----------------------------------------------------------------------------
	call write_gjf("cluster.gjf", natomt, symbolt, xt)
	!----------------------------------------------------------------------------
	stop
end
