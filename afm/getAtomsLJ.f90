subroutine getAtomsLJ()
	use kinds     , only: DP
	use module_afm, only: LJp, bas, tip
	!--------------------------------------------------------------------
	integer :: i, j
	!--------------------------------------------------------------------
	! For bas
	!
	do i=1, bas%n
		do j=1, LJp%n
			if (bas%nat(i)==j) then
				bas%r0(i) = LJp%r0(j)
				bas%ep(i) = LJp%ep(j)
				exit
			end if
		end do
	end do
	!--------------------------------------------------------------------
	! For tip
	!
	do i=1, tip%n
		do j=1, LJp%n
			if (tip%nat(i)==j) then
				tip%r0(i) = LJp%r0(j)
				tip%ep(i) = LJp%ep(j)
				exit
			end if
		end do
	end do
	!--------------------------------------------------------------------
	return
end subroutine
