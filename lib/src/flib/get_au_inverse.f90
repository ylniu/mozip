subroutine get_au_inverse(value,unit)
	use Kinds
	use Param
	use string_utility
	implicit none
	!----------------------------------------------------------------------------
	! input  variable
	character(*) ,   intent(in   ) :: unit
	!----------------------------------------------------------------------------
	! input and output variable
	real(DP)     ,   intent(inout) :: value
	!----------------------------------------------------------------------------
	select case(trim(StrLowCase(unit)))
		case ( "au"    )
			value = value
		case ( "a.u."  )
			value = value
		case ( "fs"    )
			value = value * au2fs
		case ( "k"     )
			value = value
		case ( "ps"    )
			value = value * au2ps
		case ( "cm-1"  )
			value = value * au2cm
		case ( "ev"    )
			value = value * au2ev
		case ( "debye" )
			value = value * au2db
		case ( "db"    )
			value = value * au2db
	end select
	return
end subroutine
