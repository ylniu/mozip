module symmetry
	use kinds
	implicit none
	!----------------------------------------------------------------------------
	integer      :: nrep(8)
	real(DP)     :: symm_op(3,3,1:11)
	character(3) :: pg_name(8)
	character(3) :: op_name(    1:11)
	character(3) :: irrep  (8,8)
	!----------------------------------------------------------------------------
	data nrep /1,2,2,4,8,2,4,4/
	!----------------------------------------------------------------------------
	! The name of 8 point groups
	!
	data pg_name /' C1',' CS',' C2','C2V','D2H',' CI','C2H',' D2'/
	data op_name /'  E','C2z','C2y','C2x','  I','Sxy','Szx','Syz', &
					  'C4z','C4y','C4x'/
	!----------------------------------------------------------------------------
	data irrep/	'N  ' , 'N  ', 'N  ', 'N  ', 'N  ', 'N  ', 'N  ', 'N  ', & ! C1
					'A'' ', 'A" ', '   ', '   ', '   ', '   ', '   ', '   ', & ! CS
					'A  ' , 'B  ', '   ', '   ', '   ', '   ', '   ', '   ', & ! C2
					'A1 ' , 'A2 ', 'B1 ', 'B2 ', '   ', '   ', '   ', '   ', & ! C2V
					'Ag ' , 'Au ', 'B1g', 'B1u', 'B2g', 'B2u', 'B3g', 'B3u', & ! D2H
					'g  ' , 'u  ', '   ', '   ', '   ', '   ', '   ', '   ', & ! CI
					'Ag ' , 'Au ', 'Bg ', 'Bu ', '   ', '   ', '   ', '   ', & ! C2H
					'A  ' , 'B1 ', 'B2 ', 'B3 ', '   ', '   ', '   ', '   '  / ! D2
	!----------------------------------------------------------------------------
	data symm_op / 				&
	!----------------------------------------------------------------------------
	!  1. E
						 1,  0,  0, &
						 0,  1,  0, &
						 0,  0,  1, &
	!----------------------------------------------------------------------------
	!  2. C2z
						-1,  0,  0, &
						 0, -1,  0, &
						 0,  0,  1, &
	!----------------------------------------------------------------------------
	!  3. C2y
						-1,  0,  0, &
						 0,  1,  0, &
						 0,  0, -1, &
	!----------------------------------------------------------------------------
	!  4. C2x
						 1,  0,  0, &
						 0, -1,  0, &
						 0,  0, -1, &
	!----------------------------------------------------------------------------
	!  5. I
						-1,  0,  0, &
						 0, -1,  0, &
						 0,  0, -1, &
	!----------------------------------------------------------------------------
	!  6. Sxy
						 1,  0,  0, &
						 0,  1,  0, &
						 0,  0, -1, &
	!----------------------------------------------------------------------------
	!  7. Szx
						 1,  0,  0, &
						 0, -1,  0, &
						 0,  0,  1, &
	!----------------------------------------------------------------------------
	!  8. Syz
						-1,  0,  0, &
						 0,  1,  0, &
						 0,  0,  1, &
	!----------------------------------------------------------------------------
	!  9. C4z
						 0, -1,  0, &
						 1,  0,  0, &
						 0,  0,  1, &
	!----------------------------------------------------------------------------
	! 10. C4y
						 0,  0,  1, &
						 0,  1,  0, &
						-1,  0,  0, &
	!----------------------------------------------------------------------------
	! 11. C4x
						 1,  0,  0, &
						 0,  0, -1, &
						 0,  1,  0  /
	!----------------------------------------------------------------------------
end module