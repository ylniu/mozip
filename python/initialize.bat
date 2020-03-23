:: 1. pip 配置国内源
:: https://blog.csdn.net/sinat_21591675/article/details/82770360
:: 将pip源更换到国内镜像
::
:: (1). 阿里云 http://mirrors.aliyun.com/pypi/simple/
:: (2). 豆瓣http://pypi.douban.com/simple/
:: (3). 清华大学 https://pypi.tuna.tsinghua.edu.cn/simple/
:: (4). 中国科学技术大学 http://pypi.mirrors.ustc.edu.cn/simple/
:: (5). 华中科技大学http://pypi.hustunique.com/
::
:: A. Linux下，修改 ~/.pip/pip.conf (没有就创建一个文件夹及文件。文件夹要加“.”，表示是隐藏文件夹)
:: 内容如下：
:: [global]
:: index-url = https://pypi.tuna.tsinghua.edu.cn/simple
:: [install]
:: trusted-host = https://pypi.tuna.tsinghua.edu.cn
::
:: B. Windows下，直接在user目录中创建一个pip目录，如：C:\Users\xx\pip，然后新建文件pip.ini，
::    即 %HOMEPATH%\pip\pip.ini，在pip.ini文件中输入以下内容（以豆瓣镜像为例）：
:: [global]
:: index-url = http://pypi.douban.com/simple
:: [install]
:: trusted-host = pypi.douban.com
::
:: Niuyingli-P52 : C:\Users\niuyi\pip\pip.ini
:: [global]
:: timeout = 6000
:: index-url = https://pypi.tuna.tsinghua.edu.cn/simple
:: trusted-host = pypi.tuna.tsinghua.edu.cn

:: Add Environment variable
:: PYTHONPATH=D:\Share\Data\GitHub\mozip\python