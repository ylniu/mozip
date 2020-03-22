import generate_pip_packages as gpp
import sys
import getopt
import getpass

if __name__ == '__main__':
	packages=gpp.get_package()
	user_name = getpass.getuser()  # 获取当前用户名
	PIPDIR="C:%HOMEPATH%\\pip"
	PIPINI=PIPDIR+"\\pip.ini"
	#-------------------------------------------------------------------------------------------------------------------
	f = open('install_python_packages.bat', 'w')
	#-------------------------------------------------------------------------------------------------------------------
	url="pypi.tuna.tsinghua.edu.cn"
	#-------------------------------------------------------------------------------------------------------------------
	address="https://"+url+"/simple"
	#-------------------------------------------------------------------------------------------------------------------
	f.write("mkdir " + PIPDIR + "\n")
	f.write("echo [global]>" + PIPINI + "\n")
	f.write("echo timeout = 6000>>" + PIPINI + "\n")
	f.write("echo index-url = " + address + ">>" + PIPINI + "\n")
	f.write("echo trusted-host = " + url +  ">>" + PIPINI + "\n")
	#-------------------------------------------------------------------------------------------------------------------
	f.write("python -m pip install --upgrade pip\n")
	#-------------------------------------------------------------------------------------------------------------------
	for package in packages:
		cmd="pip install "+package+"\n"
		f.write(cmd)
	f.close()
	#-------------------------------------------------------------------------------------------------------------------