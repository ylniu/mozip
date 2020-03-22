import generate_pip_packages as gpp
import sys
import getopt

if __name__ == '__main__':
	packages=gpp.get_package()
	f = open('install_python_packages.bat', 'w')
	for package in packages:
		cmd="pip install "+package+"\n"
		f.write(cmd)
	f.close()