import os
import string
import re

def get_package():
    ftmp="tmp.pip.packages"
    packages=[]
    result = os.popen("pip list")
    res = result.read()
    i=0
    for line in res.splitlines():
        i=i+1
        if i>=3:
            data=line.split()
            packages.append(data[0])
    return packages