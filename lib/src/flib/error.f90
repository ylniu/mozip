subroutine error(ntype, fname, word)
	implicit none
	!----------------------------------------------------------------------------
	integer      :: ntype
	character(*) :: fname
	character(*) :: word
	!----------------------------------------------------------------------------
	if (ntype==1) then
		write(*,*) "Can not find '",trim(word),"' in the file '",trim(fname),"'."
		write(*,*) "Stop"
		stop
	else if (ntype==2) then
		write(*,*) "Can not open '",trim(fname),"'"
		write(*,*) "Stop"
		stop
	end if
	!----------------------------------------------------------------------------
	return
end subroutine
