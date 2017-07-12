program g09tomolekel
	use kinds, only: DP
	implicit none
	!----------------------------------------------------------------------------
	integer           :: fid, fid1
	integer           :: i, j, k, n, istat, nw
	integer           :: i1, i2, i3, i4, i5, i6, iv, io
	!----------------------------------------------------------------------------
	character(400)    :: line, line1, tmp, finp, fout
	logical, external :: is_letter
	integer, external :: number_of_words
	!----------------------------------------------------------------------------
	fid  = 1
	fid1 = 2
	!----------------------------------------------------------------------------
	call getarg(1, finp)
	fout="g03-"//trim(finp)
	open(fid , file=finp, status="old")
	open(fid1, file=fout)
		read(fid,'(a)', iostat=istat) line
		do while(istat==0)
			!----------------------------------------------------------------------
			i1=index(line,"Gaussian 09")
			i2=index(line,"Density Matrix:")
			i3=index(line," Atom  AN")
			i4=index(line,"Eigenvalues -- ")
			i5=index(line,"Mulliken charges:")
			if (i1>0) line(i1:i1+10)="Gaussian 03"
			if (i2>0) line(i2:i2+14)="DENSITY MATRIX."
			if (i3>0) line(i3:i3+8)="Atom AN"
			if (i4>0) line(i4:i4+14)="EIGENVALUES -- "
			if (i5>0) line(i5:i5+28)="Mulliken atomic charges:"
			!----------------------------------------------------------------------
! 			i6=index(line,"EIGENVALUES -- ")
! 			if (i6>0) then
! 				backspace(fid)
! 				backspace(fid)
! 				read(fid,'(a)') line1
! 				iv=index(line1,"   V")
! 				do while (iv>0)
! 					line1(iv:iv+5)="(A)--V"
! 					iv=index(line1,"   V")
! 				end do
! 				io=index(line1,"   O")
! 				do while (io>0)
! 					line1(io:io+5)="(A)--O"
! 					io=index(line1,"   O")
! 				end do
! 				backspace(fid1)
! 				write(fid1,'(a)') trim(line1)//char(13)
! 				read(fid,'(a)', iostat=istat) line1
! 			end if
			write(fid1,'(a)') trim(line)//char(13)
			read(fid,'(a)', iostat=istat) line
			!----------------------------------------------------------------------
		end do
	close(fid)
	close(fid1)
	!----------------------------------------------------------------------------
	stop
end
