subroutine qm_file_fc(fname, natom, fc, info)
	use kinds
	use Param, only: au2ev, MassAmu
	implicit none
	!----------------------------------------------------------------------------
	! Input  variable
	!
	character(*), intent( in) :: fname
	integer     , intent( in) :: natom
	!----------------------------------------------------------------------------
	! Output variables
	!
	integer     , intent(out) :: info
	real(DP)    , intent(out) :: fc(natom*3,natom*3)
	!----------------------------------------------------------------------------
	! Local  variables
	!
	integer                   :: fid, i, j, k, ib, jb, ie, je, n3, jn
	integer                   :: ii, jj, n, iimax
	integer                   :: kb, ke, dummy
	character(200)            :: line, tmp, file_type, version, fmt
	logical                   :: scanok
	real(DP)    , allocatable :: pbuff(:), buff(:)
	real(DP)    , allocatable :: mass(:)
	real(DP)    , allocatable :: massau(:)
	real(DP)    , allocatable :: sqtmass(:)
	logical     , external    :: search_word_free
	logical     , external    :: search_word_free_last
	logical     , external    :: search_word
	logical     , external    :: search_word_del_space
	logical     , external    :: search_word_first_number
	!----------------------------------------------------------------------------
	info = 0
	n3=natom*3
	!----------------------------------------------------------------------------
	if (natom<=0) then
		write(*,'(2x,"Error!")')
		write(*,'(2x,"natom =", i20)') natom
		write(*,'(2x,"Stop!")')
		stop
	end if
	!----------------------------------------------------------------------------
	allocate(pbuff  (n3*n3))
	allocate(mass   (natom))
	allocate(massau (natom))
	allocate(sqtmass(n3   ))
	!----------------------------------------------------------------------------
	call qm_file_type(fname, file_type, version, info)
	!----------------------------------------------------------------------------
	call qm_file_mass(fname, natom, mass, info)
	!----------------------------------------------------------------------------
	massau = mass * MassAmu
	massau = mass
	!----------------------------------------------------------------------------
	do i=1, natom
		do j=1, 3
			k=(i-1)*3 + j
			sqtmass(k) = sqrt(massau(i))
		end do
	end do
	!----------------------------------------------------------------------------
	call get_free_fid(fid)
	!----------------------------------------------------------------------------
	open(fid, file=fname, status="old")
	select case(trim(file_type))
		case ("CHEMSHELL")
			!----------------------------------------------------------------------
			! CHEMSHELL
			!----------------------------------------------------------------------
			scanok=search_word(fid,23,53,'cartesian force constant matrix',line)
			if (.not.scanok)then
				write(*,*) 'No force matrix information found'
				stop
			end if
			read(fid,*)
			iimax=(n3-1)/9+1
			kb=1
			do i=1,iimax
				ke=kb+8
				if(ke>n3)  ke=n3
				do dummy = 1,11
					read(fid,*)
				end do
				!
				do j = 1,n3
					read(fid,'(20X,9F12.8)') (fc(j,k),k=kb,ke)
				end do
				kb=kb+9
			end do
			do i=1, n3
				do j=i+1, n3
					fc(i,j) = fc(j,i)
				end do
			end do
			!----------------------------------------------------------------------
		case ("GAUSSIAN LOG")
			!----------------------------------------------------------------------
			! GAUSSIAN LOG
			!----------------------------------------------------------------------
			
			!----------------------------------------------------------------------
		case ("GAUSSIAN FCHK")
			!----------------------------------------------------------------------
			! GAUSSIAN FCHK
			!----------------------------------------------------------------------
			scanok=search_word(fid,1,25,"Cartesian Force Constants",line)
			if (.not. scanok) then
				write(*,*)
				write(*,*) "Error! Can not find 'Cartesian Force Constants'"
				write(*,*) trim(fname)
				write(*,*)
				stop
			end if
			!n = n3*(n3-1)/2 + 243
			read(line,*) (tmp, i=1, 5), n
			!----------------------------------------------------------------------
			allocate  ( buff  (n ) )
			read(fid,*) buff
			k=0
			do i=1, n3
				do j=1, i
					k=k+1
					fc(i,j) = buff(k)
					fc(j,i) = fc(i,j)
				end do
			end do
			deallocate( buff )
		!-------------------------------------------------------------------------
		case ("MOLPRO")
			!----------------------------------------------------------------------
			! MOLPRO
			!----------------------------------------------------------------------
			rewind(fid)
			if (.not.search_word(fid,2,16,'Force Constants',line)) then
				write(*,*) 'No force matrix information found'
				stop
			end if
			ib=1
			do while(ib<=n3)
				read(fid,*)
				jb=ib
				do i=ib, n3
					je=min(min(i, jb+4), n3)
					read(fid,*) tmp, (fc(i,j), j=jb, je)
				end do
				ib=ib+5
			end do
			do i=1, n3
				do j=i+1, n3
					fc(j,i) = fc(i,j)
				end do
			end do
			!----------------------------------------------------------------------
		case ("TURBOMOLE")
			!----------------------------------------------------------------------
			! TURBOMOLE
			!----------------------------------------------------------------------
			if( search_word_free(fid,"CARTESIAN FORCE CONSTANT MATRIX",line) ) then
				ib=1
				jb=ib
				je=jb
				do while(ib<=n3)
					if (search_word_free(fid,"ATOM",line) ) then
						read(fid,*)
						do i=ib, n3
							read(fid,'(a)') line
							ii=index(line,'d')
							line=line(ii+3:)
							!----------------------------------------------------------
							jn=je-jb+1
							! gfortran format
							write(fmt,'( "(", I0, "f10.7)" )') jn 
							read(line,fmt) (fc(i,jj), jj=jb, je)
							! ifort format
							! read(line,'(<jn>f10.7)') (fc(i,jj), jj=jb, je)
							do jj=jb, je
								fc(jj,i)=fc(i,jj)
							end do
							je=min(je+1,jb+5)
							!----------------------------------------------------------
						end do
						ib=ib+6
						jb=ib
						je=jb
					end if
				end do
			else
				write(*,'("Error!")')
				write(*,'("Can not find force constant matrix in turbomole log file. Stop!")')
				stop
			end if
			!----------------------------------------------------------------------
		case ("NWCHEM")
			!----------------------------------------------------------------------
			! NWCHEM
			!----------------------------------------------------------------------
			scanok=search_word_free_last(fid,"MASS-WEIGHTED PROJECTED HESSIAN",line)
			read(fid,'(a)') line
			!----------------------------------------------------------------------
			ib = 1
			ie = ib+9
			ie = min(n3,ie)
			!----------------------------------------------------------------------
			do while (ib <= n3 )
				!-------------------------------------------------------------------
				read(fid,'(a)') line
				read(fid,'(a)') line
				read(fid,'(a)') line
				read(fid,'(a)') line
				!-------------------------------------------------------------------
				do i=ib, n3
					je=min(i,ie)
					read(fid,*) tmp, (fc(i,j), j=ib, je)
				end do
				!-------------------------------------------------------------------
				ib = ib+10
				ie = ie+10
				ie = min(ie, n3)
				!-------------------------------------------------------------------
			end do
			!----------------------------------------------------------------------
			do i=1, n3
				do j=1,i
					fc(i,j) = fc(i,j) * sqtmass(i) * sqtmass(j)
					if (i/=j) fc(j,i) = fc(i,j)
				end do
			end do
			fc = fc / 1000.0_DP
			!----------------------------------------------------------------------
		case ("NUMFORCE")
			!----------------------------------------------------------------------
			! NUMFORCE
			!----------------------------------------------------------------------
			if( search_word_free(fid,"Unprojected Force Constant Matrix",line) ) then
				do i=1, n3
					read(fid,*)
					read(fid,*) (fc(1,j), j=1, n3)
				end do
			end if
		case default
			
	end select
	close(fid)
	!----------------------------------------------------------------------------
	deallocate(mass   )
	deallocate(massau )
	deallocate(sqtmass)
	deallocate(pbuff  )
	!----------------------------------------------------------------------------
	return
end subroutine
