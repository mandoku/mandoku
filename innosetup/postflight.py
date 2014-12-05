# -*- coding: utf-8 -*-
import sys, os, getpass, ConfigParser
from os.path import expanduser, join
from datetime import datetime

emacspath=""
gitpath=""
pypath=sys.executable
emacsinitOK = False

home = expanduser("~")
krp = sys.argv[1]
sshpubkey = os.path.join(home, ".ssh/glkanripo.pub")


#get the token
config = ConfigParser.ConfigParser()
sp = os.path.split(os.path.split(os.path.split(os.path.realpath(__file__))[0])[0])[0]

#read the files, set up emacs init file
myfiles = os.path.join(home, ".emacs.d/md/myfiles.txt")

for line in open(myfiles, 'r', 'utf-8'):
    if "Recycle" in line:
        continue
    if "runemacs" in line.lower():
        emacspath=line[:-1]
    if gitpath == "" and "git" in line.lower() and "bin" in line.lower():
        gitpath=line[:-1]

emacsinit = os.path.join(home, ".emacs.d/init.el")

if os.path.isfile(emacsinit):
    for line in open(emacsinit, 'r', 'utf-8'):
        if "md/md-init" in line.lower():
            emacsinitOK = True
if not emacsinitOK:
    initfile=open(emacsinit, 'a', 'utf-8')
    initfile.write(""";; [%s] added by Mandoku Installer
(setq mandoku-base-dir "%s")
(setq mandoku-git-program "%s")
(setq mandoku-python-program "%s")
(load (expand-file-name "~/.emacs.d/md/md-init.el"))
    
""" % ( (datetime.now().strftime("%Y-%m-%d %H:%M:%S")),
                   krp, gitpath, pypath
                   ))
    initfile.close()
        
if not os.path.isfile(sshpubkey):
#check for windows environment!
    if "win" in sys.platform:
        sshpath="/%s/%s" % (home, ".ssh/glkanripo")
        sshpath=sshpath.replace('\\', '/').replace(':', '')
        os.system('"%s" -f "%s" ' % (os.path.join(os.path.split(gitpath)[0], "ssh-keygen.exe"), sshpath)) 
    else:
        os.system('ssh-keygen -f %s/.ssh/glkanripo' % (home))

