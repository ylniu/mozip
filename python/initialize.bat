:: 1. pip ���ù���Դ
:: https://blog.csdn.net/sinat_21591675/article/details/82770360
:: ��pipԴ���������ھ���
::
:: (1). ������ http://mirrors.aliyun.com/pypi/simple/
:: (2). ����http://pypi.douban.com/simple/
:: (3). �廪��ѧ https://pypi.tuna.tsinghua.edu.cn/simple/
:: (4). �й���ѧ������ѧ http://pypi.mirrors.ustc.edu.cn/simple/
:: (5). ���пƼ���ѧhttp://pypi.hustunique.com/
::
:: A. Linux�£��޸� ~/.pip/pip.conf (û�оʹ���һ���ļ��м��ļ����ļ���Ҫ�ӡ�.������ʾ�������ļ���)
:: �������£�
:: [global]
:: index-url = https://pypi.tuna.tsinghua.edu.cn/simple
:: [install]
:: trusted-host = https://pypi.tuna.tsinghua.edu.cn
::
:: B. Windows�£�ֱ����userĿ¼�д���һ��pipĿ¼���磺C:\Users\xx\pip��Ȼ���½��ļ�pip.ini��
::    �� %HOMEPATH%\pip\pip.ini����pip.ini�ļ��������������ݣ��Զ��꾵��Ϊ������
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