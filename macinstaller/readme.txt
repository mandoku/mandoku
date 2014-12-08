The layout of the macinstaller is as follows:

On the root of the disc is as in macinstaller.layout:
Emacs, MandokuInstaller and the macinstaller dir on the top,
then in macinstaller

-rwxr-xr-x+  1 chris  staff  1656 Dec  7 13:46 addsshkey.py
-rwxr-xr-x+  1 chris  staff  1106 Dec  8 10:22 ffm.sh
drwxr-xr-x+  5 chris  staff   170 Dec  7 12:57 md
-rw-r--r--+  1 chris  staff  2106 Dec  7 13:07 postflight.py
drwxr-xr-x+ 12 chris  staff   408 Dec  7 13:51 python

This drives the installation.

MandokuInstaller is constructed from the macinstaller.applescript with
the Automator application by creating a "Run Applescript" item there
from the Utilities entry.
