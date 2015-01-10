@echo off
set krp=%1
set hom=%1
if exist %HOME% (set hom=%HOME%)
rem if not exist %HOME% (set HOME="%hom%")
rem Find the drive where py is installed 
for %%i in (H: I: J: K: L: M: N: O: P: Q: R: S: T: U: V: W: X: Y: Z: G: F: E: D: C:  %krp%\bin) DO @if exist %%i\Python27 set pydrive=%%i
rem lets assume the other stuff is on the same directory
rem where \R %pydrive%: runemacs > %%hom%%\.emacs.d\md\myfiles.txt
rem set
echo %pydrive%\
rem remove the comment from next line for zip package version.
rem start /wait notepad.exe %krp%\user\mandoku-settings.cfg
rem PAUSE
echo Looking for the necessary files, please wait a moment.
where /R %pydrive%\ git > "%hom%\.emacs.d\md\myfiles.txt"
if not exist "%hom%\.ssh\glkanripo" echo Generating a ssh key for use with Kanripo. 
if not exist "%hom%\.ssh\glkanripo" echo Please do not enter a passphrase, just press enter.
if not exist "%hom%\.ssh\" mkdir "%hom%\.ssh\"
%pydrive%\Python27\python.exe "%hom%\.emacs.d\md\postflight.py" %hom%
netsh wlan show interface | find /i "WaveLAN" > nul
if %errorlevel% equ 1 goto dontsetproxy
set http_proxy=proxy.kuins.net:8080
echo proxy
:dontsetproxy

%pydrive%\Python27\python.exe "%hom%\.emacs.d\md\addsshkey.py" %hom%
PAUSE

