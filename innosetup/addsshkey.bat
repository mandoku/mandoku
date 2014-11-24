@echo off
set md=%CD:~0,1%
set krp=%1
python\App\python.exe %krp%\system\python\addsshkey.py %1

