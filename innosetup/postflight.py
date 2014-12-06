# -*- coding: utf-8 -*-
import sys, os, getpass, ConfigParser, codecs, subprocess
from os.path import expanduser, join
from datetime import datetime

emacspath=""
gitpath=""
pypath=sys.executable
emacsinitOK = False

home = expanduser("~")
#krp = sys.argv[1]
sshpubkey = os.path.join(home, ".ssh/glkanripo.pub")

config = ConfigParser.ConfigParser()
config.read(os.path.join(home, ".emacs.d/md/mandoku.cfg"))
krp = config.get("Mandoku", "basedir")

sp = os.path.split(os.path.split(os.path.split(os.path.realpath(__file__))[0])[0])[0]

#read the files, set up emacs init file
myfiles = os.path.join(home, ".emacs.d/md/myfiles.txt")

for line in open(myfiles, 'r'):
    if "Recycle" in line:
        continue
    if "runemacs" in line.lower():
        emacspath=line[:-1]
    if gitpath == "" and "git" in line.lower() and "bin" in line.lower():
        gitpath=line[:-1]

#write the ini file
try:
    config.add_section('Paths')
except:
    pass
config.set("Paths", "Python",  os.path.split(pypath)[0])
config.set("Paths", "Git",  os.path.split(gitpath)[0])
with open(os.path.join(home, ".emacs.d/md/mandoku.cfg"), 'w') as configfile:    # save
    config.write(configfile)

    
emacsinit = os.path.join(home, ".emacs.d/init.el")

if os.path.isfile(emacsinit):
    for line in codecs.open(emacsinit, 'r', 'utf-8'):
        if "md/md-init" in line.lower():
            emacsinitOK = True
if not emacsinitOK:
    initfile=codecs.open(emacsinit, 'a', 'utf-8')
    initfile.write(""";; [%s] added by Mandoku Installer
(setq mandoku-base-dir (concat (expand-file-name "%s") "/"))
(setq mandoku-git-program (expand-file-name "%s"))
(setq mandoku-python-program (expand-file-name "%s"))
(load (expand-file-name "~/.emacs.d/md/md-init.el"))
(add-to-list 'exec-path (expand-file-name "%s"  ))
(add-to-list 'exec-path (expand-file-name "%s"  ))
(require 'mandoku)
(require 'mandoku-link)
(mandoku-initialize)    
""" % ((datetime.now().strftime("%Y-%m-%d %H:%M:%S")),
       krp.replace('\\', '/'),
       gitpath.replace('\\', '/'), pypath.replace('\\', '/'),
       os.path.split(gitpath)[0].replace('\\', '/'),
       os.path.split(pypath)[0].replace('\\', '/')
                   ))
    initfile.close()



if not os.path.isfile(sshpubkey):
#check for windows environment!
    if "win" in sys.platform:
        sshpath="/%s/%s" % (home, ".ssh/glkanripo")
        sshpath=sshpath.replace('\\', '/').replace(':', '')
        keygen=os.path.join(os.path.split(gitpath)[0], "ssh-keygen.exe")
        p = '"%s" -f "%s"' % (keygen, sshpath)
        subprocess.call([keygen, "-f", sshpath]) 
    else:
        os.system('ssh-keygen -f %s/.ssh/glkanripo' % (home))

