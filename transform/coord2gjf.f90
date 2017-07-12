program coord2gjf
	use kinds, only: DP
	implicit none
	integer        :: i,j,nat
	real(DP)       :: x(3,200)
	character(2)   :: symbol(200)
	character(200) :: line

  !----------------------------------------------------------------
  ! I/O
  integer        :: iun_coord, iun_gauss
  character(20)  :: f_coord="coord", f_gauss="coord.com"
  !----------------------------------------------------------------
	iun_coord=1
	iun_gauss=2
	f_coord="coord"
	f_gauss="coord.com"
  open(iun_coord,file=f_coord, status="old")

  read(iun_coord,'(a)') line
  read(iun_coord,'(a)') line
  nat=0
  do while (line(1:1) /= "$")
    nat=nat+1
    read(line,*) (x(i,nat),i=1,3),symbol(nat)
    read(iun_coord,'(a)') line
  end do
  x=x*0.529177D0

  close(iun_coord)

  open(iun_gauss,file=f_gauss)
    write(iun_gauss,'("%Chk=coord.chk")')
    write(iun_gauss,'("%mem=10GB")')
    write(iun_gauss,'("%nprocs=8")')
    write(iun_gauss,'("#p td b3lyp/6-31g(d) prop=(fitcharge,field) iop(6/22=-4,6/29=1,6/30=0,6/17=2) nosymm")')
    write(iun_gauss,'("")')
    write(iun_gauss,'("title")')
    write(iun_gauss,*)
    write(iun_gauss,'("0 1")')
    do i=1,nat
      write(iun_gauss,'(a2,4x,3f19.13)') symbol(i), (x(j,i),j=1,3)
    end do
    write(iun_gauss,*)
  close(iun_gauss)
end

