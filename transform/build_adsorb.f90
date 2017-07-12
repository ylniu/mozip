module mod_build_adsorb
	use kinds, only: DP
	use math , only: sort_array
	type, public :: gmol
		integer       , public              :: n
		real(DP)      , public, allocatable :: x(:,:)
		character(1)  , public, allocatable :: fix(:,:)
		character(2)  , public              :: symbol
		character(100), public, allocatable :: sname(:)
	end type
	!---------------------------------------------------------------------------
	contains
	!---------------------------------------------------------------------------
	subroutine allocate_gmol(this)
		type(gmol) :: this
		allocate(this%sname(   this%n))
		allocate(this%x    (3, this%n))
		allocate(this%fix  (3, this%n))
	end subroutine allocate_gmol
	!
	!---------------------------------------------------------------------------
	!
	subroutine sort_z_gmol(this)
		type(gmol)            :: this
		integer               :: i
		integer , allocatable :: idx(:)
		real(DP), allocatable :: a(:)
		!------------------------------------------------------------------------
		allocate(a  (this%n))
		allocate(idx(this%n))
		!------------------------------------------------------------------------
		do i=1, this%n
			a(i) = this%x(3,i)
		end do
		!------------------------------------------------------------------------
		call sort_index(this%n, a, idx, 1)
		call sort_array(this%n, this%x, idx)
		!------------------------------------------------------------------------
		deallocate(a  )
		deallocate(idx)
		!------------------------------------------------------------------------
	end subroutine sort_z_gmol
	!
	!---------------------------------------------------------------------------
	!
	subroutine deallocate_gmol(this)
		type(gmol) :: this
		deallocate(this%x    )
		deallocate(this%fix  )
		deallocate(this%sname)
	end subroutine deallocate_gmol
end module
!------------------------------------------------------------------------------
program buildadsorb
	use math,  only: rotmat3, sort_array, inverse3
	use array, only: array_group_number, array_group
	use mod_build_adsorb
	implicit none
	!---------------------------------------------------------------------------
	integer                      :: fid, i, j, k, l1,info
	integer                      :: n_line1, n_line2, natom1, natom2, natom
	integer                      :: order(3)
	integer                      :: nsymbol
	integer        , allocatable :: n1(:)
	!---------------------------------------------------------------------------
	real(DP)                     :: rot(3), v(3)
	real(DP)                     :: a, b, c, alpha, beta, gamma
	real(DP)                     :: r(3,3), d(3), dcry(3), dcry_slab(3), d_slab(3)
	real(DP)                     :: aa(3,3), bb(3,3)
	real(DP)                     :: x_edge(3,4)
	real(DP)                     :: slab_zmax, PI
	real(DP)                     :: fix_xy
	real(DP)                     :: fix_z
	real(DP)                     :: alat
	type(gmol)     , allocatable :: grp(:)
	!---------------------------------------------------------------------------
	integer        , allocatable :: idx(:)
	real(DP)       , allocatable :: x1(:,:), x2(:,:), x(:,:)
	real(DP)       , allocatable :: x_cart(:,:), dis_xy(:,:)
	!---------------------------------------------------------------------------
	character(100)               :: finp, fslab, fmole, foutp, tmp, tmp1, tmp2, fmt
	character(200)               :: title
	character(200) , allocatable :: lines(:)
	character(2  ) , allocatable :: symbol1(:), symbol2(:), symbol(:), symbols(:,:), a1(:)
	character(100) , allocatable :: sname(:)
	character(1  ) , allocatable :: if_opt(:)
	logical                      :: sort_name, condition
	!---------------------------------------------------------------------------
	namelist /control/ fslab, fmole, foutp, sort_name, rot, &
		& order, v, d, dcry, dcry_slab, d_slab, fix_xy, fix_z
	!---------------------------------------------------------------------------
	fid       =  1
	fix_xy    = -1.D0
	fix_z     = -1.D0
	r         =  0.D0
	r(1,1)    =  1.D0
	r(2,2)    =  1.D0
	r(3,3)    =  1.D0
	d         =  0.D0
	dcry      =  0.D0
	d_slab    =  0.D0
	dcry_slab =  0.D0
	v         =  0.D0
	PI        = acos(-1.D0)
	title     = "Build_Cluster"
	alat      = 1.D0
	!---------------------------------------------------------------------------
	! Read input parameters
	!
	finp="input"
	i=iargc()
	if (i==1) call getarg(1,finp)
	!---------------------------------------------------------------------------
	open(fid, file=finp, status="old")
		read(fid,control)
	close(fid)
	!---------------------------------------------------------------------------
	! Get rotation matrix of molecule
	!
	r=rotmat3(rot,order)
	!---------------------------------------------------------------------------
	! Read number of atoms to allocate arrays
	!
	call read_slab_num(fslab, natom1, n_line1)
	!call read_mole_num(fmole, natom2, n_line2)
	call get_gjf_natom(fmole, natom2)
	!---------------------------------------------------------------------------
	natom = natom1 + natom2
	allocate( x1      (3,natom1) )
	allocate( x2      (3,natom2) )
	allocate( symbol1 (  natom1) )
	allocate( symbol2 (  natom2) )
	allocate( lines   ( n_line1) )
	allocate( x       (3,natom ) )
	allocate( symbol  (  natom ) )
	allocate( sname   (  natom ) )
	allocate( idx     (  natom ) )
	allocate( dis_xy  (4,natom ) )
	allocate( if_opt  (  natom ) )
	allocate( x_cart  (3,natom ) )
	!---------------------------------------------------------------------------
	if_opt="F"
	!---------------------------------------------------------------------------
	! Read slab information
	!
	call read_slab(fslab, natom1, n_line1, x1, lines, symbol1, &
		a, b, c, alpha, beta, gamma)
	!---------------------------------------------------------------------------
	slab_zmax=0.0
	do i=1, natom1
		if(slab_zmax<x1(3,i)) then
			slab_zmax=x1(3,i)
		end if
	end do
	slab_zmax=slab_zmax * c
	!---------------------------------------------------------------------------
	! Read adsorbed molecule information
	!
	!call read_mole(fmole, natom2, n_line2, x2, symbol2)
	call get_gjf_coord(fmole, natom2, x2, symbol2)
	!---------------------------------------------------------------------------
	! Calculate lattice parameters matrix aa and inverse matrix bb^-1
	!
	call lattice_constants_to_a(aa, a, b, c, alpha, beta, gamma)
	bb=inverse3(aa)
	!---------------------------------------------------------------------------
	d(1)=d(1) * a + d(2) * b * cos(gamma / 180.D0 * PI)
	d(2)=d(2) * b * cos((gamma-90.D0) / 180.D0 * PI)
	!d(3)=d(3) + slab_zmax
	!---------------------------------------------------------------------------
	d_slab(1)=d_slab(1) * a + d_slab(2) * b * cos(gamma / 180.D0 * PI)
	d_slab(2)=d_slab(2) * b * cos((gamma-90.D0) / 180.D0 * PI)
	!d_slab(3)=d_slab(3) + slab_zmax
	!---------------------------------------------------------------------------
	call rot_displ(natom2,x2,r,d)
	!---------------------------------------------------------------------------
	do i=1, natom2
		x2(:,i)   = x2(:,i) + v
		if_opt(natom1+i) = "T"
	end do
	!---------------------------------------------------------------------------
	call x2cry(1,d_slab,bb)
	!---------------------------------------------------------------------------
	dcry_slab = dcry_slab + d_slab
	!---------------------------------------------------------------------------
	call x2cry(natom2,x2,bb)
	!---------------------------------------------------------------------------
	call trans_cry(natom2,x2,dcry)
	!---------------------------------------------------------------------------
	call trans_cry(natom1,x1,dcry_slab)
	!---------------------------------------------------------------------------
	! output
	!----------------------------------------------------------------------------
	x     (:, 1       :natom1) = x1
	x     (:, natom1+1:natom ) = x2
	symbol(   1       :natom1) = symbol1
	symbol(   natom1+1:natom ) = symbol2
	!----------------------------------------------------------------------------
	if(sort_name) then
		call sort_array_string(natom,symbol,idx,1)
	end if
	!---------------------------------------------------------------------------
	do i=1, natom
		write(sname(i),'(a,i0)') trim(symbol(i)), i
	end do
	!---------------------------------------------------------------------------
	call sort_array(natom, symbol, idx)
	call sort_array(natom, x, idx)
	call sort_array(natom, sname, idx)
	nsymbol=array_group_number(natom, symbol)
	allocate(grp(nsymbol))
	allocate(a1 (nsymbol))
	allocate(n1 (nsymbol))
	call array_group(natom, symbol, nsymbol, n1, a1)
	k=0
	do i=1, nsymbol
		grp(i)%n      = n1(i)
		grp(i)%symbol = trim(a1(i))
		call allocate_gmol(grp(i))
		do j=1, grp(i)%n
			k=k+1
			grp(i)%x    (:,j) = x    (:,k)
			write(grp(i)%sname(j),'(a,i0)') trim(grp(i)%symbol),k
		end do
	end do
	!----------------------------------------------------------------------------
	do i=1, nsymbol
		call sort_z_gmol(grp(i))
	end do
	!----------------------------------------------------------------------------
	k=0
	do i=1, nsymbol
		do j=1, grp(i)%n
			k=k+1
			x(:,k)=grp(i)%x(:,j)
		end do
	end do
	!----------------------------------------------------------------------------
	! Fix atom near edge
	!
	!   D(0,1)  --------------   C(1,1)
	!           |            |
	!           |            |
	!           |            |
	!   A(0,0)  --------------   B(1,0)
	!
	x_edge      = 0.D0
	x_edge(1,2) = 1.D0
	x_edge(1,3) = 1.D0
	x_edge(2,3) = 1.D0
	x_edge(2,4) = 1.D0
	x_cart      = x
	!---------------------------------------------------------------------------
	call coord_crys_to_cart(4    , aa, x_edge)
	call coord_crys_to_cart(natom, aa, x_cart)
	call get_dis(natom, x_cart, x_edge, dis_xy)
	!---------------------------------------------------------------------------
	! Fix xy
	!
	if (fix_xy>=0.D0) then
		do i=1, natom
			condition = dis_xy(1,i) <= fix_xy .or. dis_xy(2,i) <= fix_xy &
				&   .or. dis_xy(3,i) <= fix_xy .or. dis_xy(4,i) <= fix_xy
			if (condition) then
				if_opt(i)="F"
			end if
		end do
	end if
	!---------------------------------------------------------------------------
	! Fix z
	!
	if (fix_z>=0.D0) then
		do i=1, natom
			condition = x_cart(3,i) <= fix_z
			if (condition) then
				if_opt(i)="F"
			else
				if_opt(i)="T"
			end if
		end do
	end if
	!---------------------------------------------------------------------------
	k=0
	do i=1, nsymbol
		do j=1, grp(i)%n
			k=k+1
			grp(i)%fix(:,j) = if_opt(k)
		end do
	end do
	!----------------------------------------------------------------------------
	open(fid, file=foutp)
		do i=1, n_line1
			l1=len(trim(lines(i)))
			write(fmt,*) l1
			write(fid,'(a'//trim(fmt)//')') trim(lines(i))
		end do
		!do i=1, natom
			!write(fid,'(a7,a3,3f12.7,"   0.00000  Uiso   1.00")') &
				!adjustl(sname(i)), adjustl(symbol(i)), x(:,i)
		!end do
		do i=1, nsymbol
			do j=1, grp(i)%n
				write(fid,'(a7,a3,3f12.7,"   0.00000  Uiso   1.00")') &
					adjustl(grp(i)%sname(j)), adjustl(grp(i)%symbol), grp(i)%x(:,j)
			end do
		end do
	close(fid)
	!---------------------------------------------------------------------------
	open(fid, file="POSCAR.create")
		if(sort_name) then
			write(fid,'(a)') trim(title)
			write(fid,'(f15.10)') alat
			!---------------------------------------------------------------------
			do i=1, 3
				write(fid,'(3f15.10)') aa(:,i)
			end do
			!---------------------------------------------------------------------
			do i=1, nsymbol
				write(fid,'(a6,$)') grp(i)%symbol
			end do
			write(fid,*)
			!---------------------------------------------------------------------
			do i=1, nsymbol
				write(fid,'(i6,$)') grp(i)%n
			end do
			write(fid,*)
			!---------------------------------------------------------------------
			write(fid,'(a)') "Selective dynamics"
			write(fid,'(a)') "Direct"
		end if
		!do i=1, natom
			!write(fid,'(3f12.7,3(2x,a))') x(:,i), (if_opt(i), k=1, 3)
		!end do
		do i=1, nsymbol
			do j=1, grp(i)%n
				write(fid,'(3f12.7,3(2x,a))') grp(i)%x(:,j), grp(i)%fix(:,j)
			end do
		end do
	close(fid)
	!---------------------------------------------------------------------------
	if(sort_name) then
		do i=1, nsymbol
			call deallocate_gmol(grp(i))
		end do
	end if
	!---------------------------------------------------------------------------
	deallocate( x1      )
	deallocate( x2      )
	deallocate( symbol1 )
	deallocate( symbol2 )
	deallocate( lines   )
	deallocate( x       )
	deallocate( symbol  )
	deallocate( sname   )
	deallocate( idx     )
	deallocate( if_opt  )
	stop
end
!
!==============================================================================
!
subroutine get_dis(natom, x_cart, x_edge, dis_xy)
	use kinds, only: DP
	use math,  only: dis_two_point
	implicit none
	!---------------------------------------------------------------------------
	integer , intent( in) :: natom
	real(DP), intent( in) :: x_cart (3, natom)
	real(DP), intent( in) :: x_edge (3,     4)
	real(DP), intent(out) :: dis_xy (4, natom)
	!---------------------------------------------------------------------------
	real(DP)              :: x_cart1(3, natom)
	real(DP)              :: x_edge1(3,     4)
	!---------------------------------------------------------------------------
	integer               :: i, j
	real(DP)              :: two_point(3,2,4)
	real(DP)              :: a, b, c
	real(DP)              :: eps, theta, cosb
	!---------------------------------------------------------------------------
	eps = 1.D-6
	!---------------------------------------------------------------------------
	x_cart1 = x_cart
	x_edge1 = x_edge
	!---------------------------------------------------------------------------
	do i=1, natom
		x_cart1(3,i) = 0.D0
	end do
	!---------------------------------------------------------------------------
	do i=1, 4
		x_edge1(3,i) = 0.D0
	end do
	!---------------------------------------------------------------------------
	two_point(:,1,1) = x_edge1(:,1)
	two_point(:,2,1) = x_edge1(:,2)
	two_point(:,1,2) = x_edge1(:,2)
	two_point(:,2,2) = x_edge1(:,3)
	two_point(:,1,3) = x_edge1(:,3)
	two_point(:,2,3) = x_edge1(:,4)
	two_point(:,1,4) = x_edge1(:,4)
	two_point(:,2,4) = x_edge1(:,1)
	!---------------------------------------------------------------------------
	do i=1, natom
		do j=1, 4
			a=dis_two_point(x_cart1  (1,  i), two_point(1,1,j))
			b=dis_two_point(x_cart1  (1,  i), two_point(1,2,j))
			c=dis_two_point(two_point(1,1,j), two_point(1,2,j))
			if (abs(a)<eps .or. abs(b)<eps) then
				dis_xy(j,i) = 0.D0
			else
				cosb  = (a**2 + c**2 - b**2) / a  / c / 2.D0
				if (abs(cosb-1.D0)<eps) cosb = 1.D0
				if (abs(cosb+1.D0)<eps) cosb =-1.D0
				theta = acos(cosb)
				dis_xy(j,i) = a * sin(theta)
			end if
		end do
	end do
	!---------------------------------------------------------------------------
	return
end subroutine
!
!==============================================================================
!
subroutine read_slab_num(fslab, natom1, n_line1)
	implicit none
	integer        :: fid, nline, n_line1, natom1, istat
	character(*)   :: fslab
	character(200) :: line
	!---------------------------------------------------------------------------
	fid=10
	!---------------------------------------------------------------------------
	open(fid, file=fslab, status="old")
		call search_word(fid, 1, 20, "_atom_site_occupancy", line, nline)
		n_line1 = nline
		read(fid,'(a)', iostat=istat) line
		natom1=0
		do while (istat==0)
			if (line(1:5)=="loop_") exit
			natom1=natom1+1
			read(fid,'(a)', iostat=istat) line
		end do
	close(fid)
	!---------------------------------------------------------------------------
	return
end subroutine
!
!==============================================================================
!
subroutine read_slab(fslab, natom1, n_line1, x1, lines, symbol1, &
	a, b, c, alpha, beta, gamma)
	use kinds, only: DP
	implicit none
	integer        :: fid, natom1, n_line1, i
	real(DP)        :: x1(3,natom1)
	real(DP)        :: a, b, c, alpha, beta, gamma
	character(*)   :: fslab, lines(n_line1), symbol1(natom1)
	character(200) :: tmp
	!---------------------------------------------------------------------------
	fid=10
	!---------------------------------------------------------------------------
	open(fid, file=fslab, status="old")
		do i=1, n_line1
			read(fid,'(a)') lines(i)
			if (lines(i)(1:14)=="_cell_length_a"   ) read(lines(i),*) tmp, a
			if (lines(i)(1:14)=="_cell_length_b"   ) read(lines(i),*) tmp, b
			if (lines(i)(1:14)=="_cell_length_c"   ) read(lines(i),*) tmp, c
			if (lines(i)(1:17)=="_cell_angle_alpha") read(lines(i),*) tmp, alpha
			if (lines(i)(1:16)=="_cell_angle_beta" ) read(lines(i),*) tmp, beta
			if (lines(i)(1:17)=="_cell_angle_gamma") read(lines(i),*) tmp, gamma
		end do
		do i=1, natom1
			read(fid,*) tmp, symbol1(i), x1(:,i)
		end do
	close(fid)
	!---------------------------------------------------------------------------
	return
end subroutine
!
!==============================================================================
!
subroutine read_mole_num(fmole, natom2, n_line2)
	use kinds, only: DP
	implicit none
	integer      :: fid, natom2, n_line2, nline
	character(*) :: fmole
	!---------------------------------------------------------------------------
	fid=10
	!---------------------------------------------------------------------------
	open(fid, file=fmole, status="old")
		call search_first_num(fid, nline)
		n_line2 = nline
		call search_first_space(fid, nline)
		natom2  = nline - 1
	close(fid)
	!---------------------------------------------------------------------------
	return
end subroutine
!
!==============================================================================
!
subroutine read_mole(fmole, natom2, n_line2, x2, symbol2)
	use kinds, only: DP
	implicit none
	integer      :: fid, natom2, n_line2, i
	real(DP)      :: x2(3,natom2)
	character(*) :: fmole, symbol2(natom2)
	!---------------------------------------------------------------------------
	fid=10
	!---------------------------------------------------------------------------
	open(fid, file=fmole, status="old")
		do i=1, n_line2
			read(fid,*)
		end do
		do i=1, natom2
			read(fid,*) symbol2(i), x2(:,i)
		end do
	close(fid)
	!---------------------------------------------------------------------------
	return
end subroutine
!
!==============================================================================
!
subroutine search_word(fid, ib, ie, word, line, nline)
	use kinds, only: DP
	implicit none
	integer      :: fid, ib, ie, nline
	character(*) :: line, word
	!---------------------------------------------------------------------------
	nline=0
	read(fid,'(a)') line
	nline = nline + 1
	if ( line(ib:ie) == word ) then
		nline=1
		return
	end if
	!
	do while ( line(ib:ie) /= word )
		read(fid,'(a)') line
		nline = nline + 1
	end do
	!---------------------------------------------------------------------------
	return
end subroutine
!
!==============================================================================
!
subroutine search_first_num(fid, nline)
	use kinds, only: DP
	implicit none
	integer        :: fid, nline
	character(200) :: line
	!---------------------------------------------------------------------------
	nline=0
	read(fid,'(a)') line
	line=trim(adjustl(line))
	nline = nline + 1
	if ( iachar(line(1:1)) >= 48 .and. iachar(line(1:1)) <= 57 ) then
		nline=1
		return
	end if
	!
	do while ( iachar(line(1:1)) < 48 .or. iachar(line(1:1)) > 57 )
		read(fid,'(a)') line
		line=trim(adjustl(line))
		nline = nline + 1
	end do
	!---------------------------------------------------------------------------
	return
end subroutine
!
!==============================================================================
!
subroutine search_first_space(fid, nline)
	use kinds, only: DP
	implicit none
	integer        :: fid, nline
	character(200) :: line
	!---------------------------------------------------------------------------
	nline=0
	read(fid,'(a)') line
	nline = nline + 1
	if ( len(trim(line)) == 0 ) then
		nline=1
		return
	end if
	!
	do while ( len(trim(line)) /= 0 )
		read(fid,'(a)') line
		nline = nline + 1
	end do
	!---------------------------------------------------------------------------
	return
end subroutine
!
!==============================================================================
!
subroutine rot_displ(natom, x, r, d)
	use kinds, only: DP
	implicit none
	!---------------------------------------------------------------------------
	integer :: natom, i, j
	real(DP) :: x(3,natom), r(3,3), d(3)
	real(DP) :: z(3,natom)
	!---------------------------------------------------------------------------
	do i=1, natom
		do j=1, 3
			z(j,i) =  r(j,1) * x(1,i) + r(j,2) * x(2,i) + r(j,3) * x(3,i)
		end do
		z(:,i) = z(:,i) + d
	end do
	x = z
	!---------------------------------------------------------------------------
	return
end subroutine
!
!==============================================================================
!
subroutine x2cry(natom, x, b)
	use kinds, only: DP
	implicit none
	!---------------------------------------------------------------------------
	integer :: natom, i, j
	real(DP) :: x(3,natom), b(3,3)
	real(DP) :: z(3,natom)
	!---------------------------------------------------------------------------
	do i=1, natom
		do j=1, 3
			z(j,i) =  b(j,1) * x(1,i) + b(j,2) * x(2,i) + b(j,3) * x(3,i)
		end do
	end do
	!---------------------------------------------------------------------------
	x = z
	!---------------------------------------------------------------------------
	return
end subroutine
!
!==============================================================================
!
subroutine trans_cry(natom, x, dcry)
	use kinds, only: DP
	implicit none
	!---------------------------------------------------------------------------
	integer  :: natom, i
	real(DP)  :: x(3,natom), dcry(3)
	!---------------------------------------------------------------------------
	do i=1, natom
		x(:,i) = x(:,i) + dcry
	end do
	!---------------------------------------------------------------------------
	return
end subroutine
