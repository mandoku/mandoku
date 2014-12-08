# -*- coding: utf-8 -*-
import sys, os, getpass, ConfigParser, codecs, subprocess
from os.path import expanduser, join
from datetime import datetime

emacspath=""
gitpath=""
pypath=sys.executable
emacsinitOK = False

#home = expanduser("~")
home = os.environ['HOME']
#krp = sys.argv[1]
sshpubkey = os.path.join(home, ".ssh/glkanripo.pub")

config = ConfigParser.ConfigParser()
config.read(os.path.join(home, ".emacs.d/md/mandoku.cfg"))
krp = config.get("Mandoku", "basedir")

sp = os.path.split(os.path.split(os.path.split(os.path.realpath(__file__))[0])[0])[0]

    
emacsinit = os.path.join(home, ".emacs.d/init.el")

if os.path.isfile(emacsinit):
    for line in codecs.open(emacsinit, 'r', 'utf-8'):
        if "md/md-init" in line.lower():
            emacsinitOK = True
if not emacsinitOK:
    initfile=codecs.open(emacsinit, 'a', 'utf-8')
    initfile.write(""";; [%s] added by Mandoku Installer
(setq mandoku-base-dir (concat (expand-file-name "%s") "/"))
(load (expand-file-name "~/.emacs.d/md/md-init.el"))
(require 'mandoku)
(require 'mandoku-link)
(mandoku-initialize)    
""" % ((datetime.now().strftime("%Y-%m-%d %H:%M:%S")),
       krp))
    initfile.close()

#add to config file
sshcfpath=os.path.join(home, ".ssh/config")
ssh_have_config = False
if os.path.isfile(sshcfpath):
    sshcf = codecs.open(sshcfpath, 'r', 'utf-8')
    for line in sshcf:
        if "IdentityFile ~/.ssh/glkanripo" in line:
            ssh_have_config = True
        
if not ssh_have_config:
    sshcf = codecs.open(sshcfpath, 'a', 'utf-8')
    sshcf.write ("""Host gl.kanripo.org
   User git
   IdentityFile ~/.ssh/glkanripo
""")
    sshcf.close()
    
if not os.path.isfile(sshpubkey):
#check for windows environment!
    if "win" in sys.platform:
        sshpath= "/" + os.path.join(home, ".ssh/glkanripo")
        sshpath=sshpath.replace('\\', '/').replace(':', '')
        print "Generating key in: ", sshpath
        keygen=os.path.join(os.path.split(gitpath)[0], "ssh-keygen.exe")
        subprocess.call([keygen, "-f", sshpath]) 
    else:
        os.system('ssh-keygen -f %s/.ssh/glkanripo' % (home))

