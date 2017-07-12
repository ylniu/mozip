module file
#ifdef IFORT
use IFPORT
#endif
	implicit none
	private
	public :: name_dir
	public :: name_base
	public :: name_main
	public :: name_ext
	public :: num_file_lines
	public :: num_file_lines_word
	public :: search_word_free
	public :: get_exe_dir
	public :: get_pid
contains
	!
	!--------------------------------------------------------------------
	!
	integer function get_pid()
		get_pid=getpid()
	end function
	!
	!--------------------------------------------------------------------
	!
	function name_base(name) result(base_name)
		implicit none
		!-----------------------------------------------------------------
		! input and output variable
		character(*), intent(in) :: name
		character(len(name))     :: base_name
		!----------------------------------------------------------------------------
		! local  variable
		integer                  :: i,j,k
		character(len(name))     :: name1
		!----------------------------------------------------------------------------
		name1=trim(adjustl(name))
		k=len(trim(name1))
		if (k==0) return
		!
		j=1
		do i=k,1,-1
			if ( iachar(name1(i:i)) == 47 ) then
				!----------------------------------------------------------------------
				! e.g.
				! /home/username/test.txt
				! |             ||      |
				! 1             |15     22
				!               j=15, k=22
				! return name1(j:k)
				!
				j=i+1
				exit
			end if
		end do
		base_name = name1(j:k)
		return
	end function
	!
	!----------------------------------------------------------------------------
	!
	function name_dir(name) result(dir_name)
		implicit none
		!-------------------------------------------------------------------------
		! input and output variable
		character(*), intent(in) :: name
		character(len(name))     :: dir_name
		!-------------------------------------------------------------------------
		! local  variable
		integer                  :: i,j,k
		character(len(name))     :: name1
		!-------------------------------------------------------------------------
		name1=trim(adjustl(name))
		k=len(trim(name1))
		!
		j=0
		do i=k,1,-1
			if ( iachar(name1(i:i)) == 47 ) then
				!----------------------------------------------------------------------
				! e.g.
				! /home/username/test.txt
				! |             |
				! 1             14
				! j=14
				! return name1(1:j) "/home/username/"
				!
				! /test.txt
				! |
				! 1
				! j=1
				! return name1(1:j) "/"
				!
				! ./test.txt
				!  |
				!  2
				! j=2
				! return name1(1:j) "/"
				!
				! test.txt
				! j=0
				! return ''
				!
				j=i
				exit
			end if
		end do
		if (j>=1) then
			dir_name=name1(1:j-1)
		else
			dir_name=''
		end if
		!-------------------------------------------------------------------------
		return
	end function
	!
	!----------------------------------------------------------------------------
	!
	function name_main(fname) result (fmain)
		!-------------------------------------------------------------------------
		character(*), intent( in) :: fname
		character(len(fname))     :: fname1
		character(len(fname))     :: fmain
		!-------------------------------------------------------------------------
		integer                   :: i
		!-------------------------------------------------------------------------
		fname1=name_base(fname)
		!-------------------------------------------------------------------------
		i=index(fname1,".",.true.)
		if (i>0) then
			fmain=fname1(1:i-1)
		else
			fmain=fname1
		end if
		!-------------------------------------------------------------------------
		return
	end function name_main
	!
	!----------------------------------------------------------------------------
	!
	function name_ext(fname) result(fext)
		!-------------------------------------------------------------------------
		character(*), intent( in) :: fname
		character(len(fname))     :: fext
		character(len(fname))     :: ftmp
		!-------------------------------------------------------------------------
		integer                   :: i, lf, idx
		!-------------------------------------------------------------------------
		ftmp=trim(adjustl(fname))
		lf=len(trim(ftmp))
		!-------------------------------------------------------------------------
		idx=-1
		do i=lf, 1, -1
			if (ftmp(i:i)==".") then
				idx=i
				exit
			end if
		end do
		if (idx>=1) then
			fext=ftmp(idx+1:lf)
		else
			fext=''
		end if
		!-------------------------------------------------------------------------
		return
		!-------------------------------------------------------------------------
	end function name_ext
	!
	!----------------------------------------------------------------------------
	!
	function search_word_free(fid, word, line)
		!----------------------------------------------------------------------------
		implicit none
		!-------------------------------------------------------------------------
		! input variables
		!
		integer,      intent( in) :: fid
		character(*), intent( in) :: word
		!-------------------------------------------------------------------------
		! output variables
		!
		logical                   :: search_word_free
		character(*), intent(out) :: line
		!-------------------------------------------------------------------------
		! local variables
		!
		integer                   :: iostat
		!-------------------------------------------------------------------------
		search_word_free = .false.
		read(fid,'(a)',iostat=iostat) line
		!
		if (index(line,word)>0) then
			search_word_free = .true.
			return
		end if
		!
		do while( index(line,word) == 0 .and. iostat==0)
			read(fid,'(a)',iostat=iostat) line
			!
			if (index(line,word)/=0) then
				search_word_free = .true.
				return
			end if
		end do
		!
		return
		!----------------------------------------------------------------------------
	end function
	!
	!----------------------------------------------------------------------------
	!
	function num_file_lines_word(fname,word)
		implicit none
		!-------------------------------------------------------------------------
		! input variables
		!
		character(*), intent( in) :: word, fname
		!-------------------------------------------------------------------------
		! output variables
		!
		integer                   :: num_file_lines_word
		!----------------------------------------------------------------------------
		! local variables
		!
		integer                   :: fid
		integer                   :: istat
		logical, external         :: search_word_free
		character(500)            :: line
		!-------------------------------------------------------------------------
		call get_free_fid(fid)
		open(fid, file=fname, status="old",iostat=istat)
			if(istat/=0) then
				num_file_lines_word=-1
			end if
			istat = 0
			num_file_lines_word   = 0
			do while (istat==0)
				if(search_word_free(fid,word,line)) then
					num_file_lines_word = num_file_lines_word + 1
				else
					exit
				end if
			end do
		close(fid)
		!-------------------------------------------------------------------------
		return
	end function
	!
	!----------------------------------------------------------------------------
	!
	subroutine get_exe_dir(path)
		implicit none
		!----------------------------------------------------------------------------
		character(   *) :: path
		integer         :: pid, fid
		character( 200) :: fproc
		character(1000) :: line
		!----------------------------------------------------------------------------
		pid=getpid()
		call get_free_fid(fid)
		!----------------------------------------------------------------------------
		write(fproc,'("/proc/",i0,"/",a)') pid, "maps"
		!----------------------------------------------------------------------------
		open(fid, file=fproc, status="old")
			read(fid, '(a)') line
		close(fid)
		line = line(74:)
		path = name_dir(line)
		!----------------------------------------------------------------------------
		!  ! For windows
		! 	program www
		! 	character(512) :: GetFileInAppDirectory
		! 	write(*,*) Trim(GetFileInAppDirectory(""))
		! 	end  
		! 
		! 	Character(512) Function GetFileInAppDirectory( cFile )
		! 		use Kernel32 , only : GetModuleFileName
		! 		Character(*) , Intent( IN ) :: cFile
		! 		Integer i
		! 		Character(512) :: cTemp
		! 		i = GetModuleFileName( 0 , cTemp , 512 )
		! 		i = Index( Trim(cTemp) , "\" , back = .true. )
		! 		cTemp( i+1: ) = cFile
		! 		GetFileInAppDirectory = cTemp
		! 	End Function GetFileInAppDirectory
	end subroutine
	!
	!----------------------------------------------------------------------------
	!
	function num_file_lines(fname)
		implicit none
		!-------------------------------------------------------------------------
		! input  variable
		character(*), intent( in) :: fname
		!-------------------------------------------------------------------------
		! output variable
		integer                   :: num_file_lines
		!-------------------------------------------------------------------------
		! local  variable
		integer                   :: fid
		integer                   :: ier
		!-------------------------------------------------------------------------
		call get_free_fid(fid)
		!-------------------------------------------------------------------------
		open(fid, file=fname, status="old",iostat=ier)
		!-------------------------------------------------------------------------
		if (ier/=0) then
			write(*,*) "Error in opening file in function num_file_lines, stop!"
			stop
		end if
		!-------------------------------------------------------------------------
		num_file_lines = 0
		do while ( ier == 0 )
			read(fid,'(a)',iostat=ier)
			if ( ier == 0 ) num_file_lines = num_file_lines + 1
		end do
		close(fid)
		return
	end function
	!
	!----------------------------------------------------------------------------
	!
end module
