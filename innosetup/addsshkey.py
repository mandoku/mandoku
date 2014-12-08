# -*- coding: utf-8 -*-
import sys, os, os.path, getpass, ConfigParser, codecs, subprocess, time
reload(sys)
sys.setdefaultencoding('utf-8')

from os.path import expanduser, join
from datetime import datetime
home = os.environ['HOME']
mdconfig = ConfigParser.ConfigParser()
mdcfgfile = os.path.join(home, ".emacs.d/md/mandoku.cfg")
mdconfig.readfp(open(mdcfgfile))
mdbase = mdconfig.get("Mandoku", "basedir")
print os.path.realpath(mdbase)
gitpath = mdconfig.get("Paths", "Git")
p= os.path.join (mdbase, "system\python") 
sys.path.insert(0, p)
import gitlab
sp = os.path.split(os.path.split(os.path.split(os.path.realpath(__file__))[0])[0])[0]


#sshpubkey = join(home, ".ssh/id_rsa.pub")
glurl="http://gl.kanripo.org"

#get the token
config = ConfigParser.ConfigParser()
cfgfile = os.path.join(mdbase, "user/mandoku-settings.cfg")
config.readfp(open(cfgfile))
gltok = config.get("Gitlab", "Private Token")

#get the key path
sshpubkey = os.path.join(home, ".ssh/glkanripo.pub")
sshprivkey = os.path.join(home, ".ssh/glkanripo")


# Connect to get the current user    
gl = gitlab.Gitlab(glurl, gltok)
#print gl.getsshkeys()
user=getpass.getuser()
fn = codecs.open(sshpubkey, 'r', 'utf-8')
#print user
key = fn.read()
#print "key length", len(key), key
#print sshpubkey
title = u"%s %s" % ("mandoku-key", datetime.now().strftime("%Y-%m-%d %H:%M:%S") )
if gl.addsshkey(title, key):
    print "200 Success: Successfully uploaded the key."
else:
    print "400 Failed: Could not upload the key. Something went wrong. Maybe the key exists already?"


ssh=os.path.join(gitpath, "ssh.exe")
subprocess.call([ssh, "-l", "git",  "-i", sshprivkey,  "gl.kanripo.org"]) 
time.sleep(2)



