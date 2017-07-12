subroutine get_LJ_param()
	use kinds     , only: DP
	use module_afm, only: LJp
	use module_afm, only: nLJ, r0, ep
	use file      , only: num_file_lines
	use module_LJp, only: allocate_LJp
	use param     , only: au2a, au2ev
	implicit none
	!--------------------------------------------------------------------
	! unit 
	!
	LJp%n = nLJ
	call allocate_LJp(LJp)
	LJp%r0 = r0
	LJp%ep = ep
	LJp%r0 = LJp%r0 / au2a
	LJp%ep = LJp%ep / au2ev
	!--------------------------------------------------------------------
	return
end subroutine
