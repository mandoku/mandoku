@echo off
set krp=%1
rem Find the drive where py is installed 
for %%i in (C D E F G H I J K L M N O P Q R S T U V W X Y Z) DO @if exist %%i:\Python27 set pydrive=%%i
rem lets assume the other stuff is on the same directory
rem where \R %pydrive%: runemacs > %%HOME%%\.emacs.d\md\myfiles.txt
echo %pydrive%:\
echo Looking for the necessary files, please wait a moment.
where /R %pydrive%:\ git > "%HOME%\.emacs.d\md\myfiles.txt"

if not exist "%HOME%\.ssh\glkanripo" echo Generating a ssh key for use with Kanripo. 
if not exist "%HOME%\.ssh\glkanripo" echo Please do not enter a passphrase, just press enter.
if not exist "%HOME%\.ssh\" mkdir "%HOME%\.ssh\"
%pydrive%:\Python27\python.exe "%HOME%\.emacs.d\md\postflight.py" %1 
netsh wlan show interface | find /i "WaveLAN" > nul
if %errorlevel% equ 1 goto dontsetproxy
set http_proxy=proxy.kuins.net:8080
echo proxy
:dontsetproxy

%pydrive%:\Python27\python.exe %krp%\system\python\addsshkey.py %1 

rem need to try a connection now..
