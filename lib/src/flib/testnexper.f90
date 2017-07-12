subroutine testnexper(n,m,inn,peven)
	implicit none
	integer :: n,m, inn(n,m), i1, i
	integer :: in(n)
	logical :: mtc
	logical :: even,peven(m)
	i1=0
	!----------------------------------------------------------------------------
	do i = 1, n
		in(i) = i
	end do
	!----------------------------------------------------------------------------
	mtc = .true.
	do while (mtc)
		i1=i1+1
		if (i1==1) then
			mtc=.false.
		else
			mtc=.true.
		end if
		call nexper (n, in, mtc, even)
		inn(:,i1) = in
		peven(i1) = even
	end do
end
