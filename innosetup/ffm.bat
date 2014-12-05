@echo off
set krp=%1
rem Find the drive where py is installed 
for %%i in (C D E F G H I J K L M N O P Q R S T U V W X Y Z) DO @if exist %%i:\Python27 set pydrive=%%i
rem lets assume the other stuff is on the same directory
rem where \R %pydrive%: runemacs > %%HOME%%\.emacs.d\md\myfiles.txt
where \R %pydrive%: git > %%HOME%%\.emacs.d\md\myfiles.txt

%pydrive%:\Python27\python.exe %%HOME%%\.emacs.d\md\postflight.py %1 

%pydrive%:\Python27\python.exe %krp%\system\python\addsshkey.py %1 
