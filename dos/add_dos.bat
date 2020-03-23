:: https://blog.csdn.net/yueliang2100/article/details/82190165
:: REG ADD命令参数
:: REG ADD KeyName [/v ValueName | /ve] [/t Type] [/s Separator] [/d Data] [/f]
:: KeyName [\\Machine\]FullKey
:: 远程机器的机器名 - 忽略默认到当前机器。
:: 远程机器上只有 HKLM 和 HKU。
:: FullKey ROOTKEY\SubKey
:: ROOTKEY [ HKLM | HKCU | HKCR | HKU | HKCC ]
:: SubKey 所选 ROOTKEY 下注册表项的完整名
:: /v 所选项之下要添加的值名
:: /ve 为注册表项添加空白值名<无名称>
:: /t RegKey 数据类型
:: [ REG_SZ | REG_MULTI_SZ | REG_DWORD_BIG_ENDIAN |
:: REG_DWORD | REG_BINARY | REG_DWORD_LITTLE_ENDIAN |
:: REG_NONE | REG_EXPAND_SZ ]
:: 如果忽略，则采用 REG_SZ
:: /s 指定一个在 REG_MULTI_SZ 数据字符串中
:: 用作分隔符的字符
:: 如果忽略，则将 "\0" 用作分隔符
:: /d 要分配给添加的注册表 ValueName 的数据
:: /f 不用提示就强行改写现有注册表项
::例如:
:: REG ADD \\ABC\HKLM\Software\MyCo
:: 添加远程机器 ABC 上的一个注册表项 HKLM\Software\MyCo
:: REG ADD HKLM\Software\MyCo /v Data /t REG_BINARY /d fe340ead
 添加一个值(名称: Data，类型: REG_BINARY，数据: fe340ead)
:: REG ADD HKLM\Software\MyCo /v MRU /t REG_MULTI_SZ /d fax\0mail
:: 添加一个值(名称: MRU，类型: REG_MUTLI_SZ，数据: fax\0mail\0\0)
:: REG ADD HKLM\Software\MyCo /v Path /t REG_EXPAND_SZ /d %%systemroot%%
:: 添加一个值(名称: Path，类型: REG_EXPAND_SZ，数据: %systemroot%)
:: 注意: 在扩充字符串中使用双百分比符号( %% )
:: 启动DOS内部命令“cd”
:: 其中“/k”表示执行完后不关闭窗口
::“%1中%1”是“CD”命令的第一个（%1）参数（这里是路径）

echo off
echo 请关闭360 等防火墙，杀毒软件
reg add "HKCR\*\shell\ms-dos" /ve /d "Open DOS Command" /f
reg add "HKCR\*\shell\ms-dos\command" /ve /d "cmd.exe /k cd %1" /f
reg add "HKCR\Folder\shell\ms-dos" /ve /d "Open DOS Command" /f
reg add "HKCR\Folder\shell\ms-dos\command" /ve /d "cmd.exe /k cd %1" /f
pause