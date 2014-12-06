# -*- coding: utf-8 -*-
import sys, os, os.path, getpass, ConfigParser, codecs, subprocess, time
reload(sys)
sys.setdefaultencoding('utf-8')

from os.path import expanduser, join
from datetime import datetime
home = expanduser("~")
mdconfig = ConfigParser.ConfigParser()
mdcfgfile = os.path.join(home, ".emacs.d/md/mandoku.cfg")
mdconfig.readfp(open(mdcfgfile))
mdbase = mdconfig.get("Mandoku", "basedir")
print os.path.realpath(mdbase)
gitpath = mdconfig.get("Paths", "Git")
p= "%s%s" % (mdbase, "\system\python") 
#p = os.path.split(os.path.realpath(__file__))[0]
print p
sys.path.insert(0, p)
import gitlab
sp = os.path.split(os.path.split(os.path.split(os.path.realpath(__file__))[0])[0])[0]


#sshpubkey = join(home, ".ssh/id_rsa.pub")
glurl="http://gl.kanripo.org"

#get the token
config = ConfigParser.ConfigParser()
cfgfile = "%s%s" % (mdbase, "/user/mandoku-settings.cfg")
config.readfp(open(cfgfile))
gltok = config.get("Gitlab", "Private Token")

#get the key path
sshpubkey = os.path.join(home, ".ssh/glkanripo.pub")
sshprivkey = os.path.join(home, ".ssh/glkanripo")


# Connect to get the current user    
proxies = { "http" : "http://proxy.kuins.net:8080"}

gl = gitlab.Gitlab(glurl, gltok)
#print gl.getsshkeys()
user=getpass.getuser()
fn = codecs.open(sshpubkey, 'r', 'utf-8')
#print user
key = fn.read()
print "key length", len(key), key
print sshpubkey
title = u"%s %s" % ("mandoku-key", datetime.now().strftime("%Y-%m-%d %H:%M:%S") )
print "title", title
if gl.addsshkey(title, key):
    print "200 Success: Successfully uploaded the key."
else:
    print "400 Failed: Could not upload the key. Something went wrong. Maybe the key exists already?"


time.sleep(2)
ssh=os.path.join(gitpath, "ssh.exe")
subprocess.call([ssh, "-l", "git",  "-i", sshprivkey,  "gl.kanripo.org"]) 



