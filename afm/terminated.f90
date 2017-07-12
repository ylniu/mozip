subroutine terminated()
	use module_afm, only: iout, icolor, &
		cymbol, zTips, rTips, rs, fs, rT, rP
	implicit none
	!----------------------------------------------------------------------------
	if (allocated(icolor)) deallocate(icolor)
	if (allocated(cymbol)) deallocate(cymbol)
	if (allocated(zTips )) deallocate(zTips )
	if (allocated(rTips )) deallocate(rTips )
	if (allocated(rs    )) deallocate(rs    )
	if (allocated(fs    )) deallocate(fs    )
	if (allocated(rT    )) deallocate(rT    )
	if (allocated(rP    )) deallocate(rP    )
	close(iout)
	!----------------------------------------------------------------------------
	return
end subroutine
