:: https://blog.csdn.net/yueliang2100/article/details/82190165
:: REG ADD�������
:: REG ADD KeyName [/v ValueName | /ve] [/t Type] [/s Separator] [/d Data] [/f]
:: KeyName [\\Machine\]FullKey
:: Զ�̻����Ļ����� - ����Ĭ�ϵ���ǰ������
:: Զ�̻�����ֻ�� HKLM �� HKU��
:: FullKey ROOTKEY\SubKey
:: ROOTKEY [ HKLM | HKCU | HKCR | HKU | HKCC ]
:: SubKey ��ѡ ROOTKEY ��ע������������
:: /v ��ѡ��֮��Ҫ��ӵ�ֵ��
:: /ve Ϊע�������ӿհ�ֵ��<������>
:: /t RegKey ��������
:: [ REG_SZ | REG_MULTI_SZ | REG_DWORD_BIG_ENDIAN |
:: REG_DWORD | REG_BINARY | REG_DWORD_LITTLE_ENDIAN |
:: REG_NONE | REG_EXPAND_SZ ]
:: ������ԣ������ REG_SZ
:: /s ָ��һ���� REG_MULTI_SZ �����ַ�����
:: �����ָ������ַ�
:: ������ԣ��� "\0" �����ָ���
:: /d Ҫ�������ӵ�ע��� ValueName ������
:: /f ������ʾ��ǿ�и�д����ע�����
::����:
:: REG ADD \\ABC\HKLM\Software\MyCo
:: ���Զ�̻��� ABC �ϵ�һ��ע����� HKLM\Software\MyCo
:: REG ADD HKLM\Software\MyCo /v Data /t REG_BINARY /d fe340ead
 ���һ��ֵ(����: Data������: REG_BINARY������: fe340ead)
:: REG ADD HKLM\Software\MyCo /v MRU /t REG_MULTI_SZ /d fax\0mail
:: ���һ��ֵ(����: MRU������: REG_MUTLI_SZ������: fax\0mail\0\0)
:: REG ADD HKLM\Software\MyCo /v Path /t REG_EXPAND_SZ /d %%systemroot%%
:: ���һ��ֵ(����: Path������: REG_EXPAND_SZ������: %systemroot%)
:: ע��: �������ַ�����ʹ��˫�ٷֱȷ���( %% )
:: ����DOS�ڲ����cd��
:: ���С�/k����ʾִ����󲻹رմ���
::��%1��%1���ǡ�CD������ĵ�һ����%1��������������·����

echo off
echo ��ر�360 �ȷ���ǽ��ɱ�����
reg add "HKCR\*\shell\ms-dos" /ve /d "Open DOS Command" /f
reg add "HKCR\*\shell\ms-dos\command" /ve /d "cmd.exe /k cd %1" /f
reg add "HKCR\Folder\shell\ms-dos" /ve /d "Open DOS Command" /f
reg add "HKCR\Folder\shell\ms-dos\command" /ve /d "cmd.exe /k cd %1" /f
pause