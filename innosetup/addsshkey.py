# -*- coding: utf-8 -*-
import gitlab,sys, os, os.path, getpass, ConfigParser
from os.path import expanduser, join
from datetime import datetime

#home = expanduser("~")
#sshpubkey = join(home, ".ssh/id_rsa.pub")
glurl="http://gl.kanripo.org"

#get the token
config = ConfigParser.ConfigParser()
sp = os.path.split(os.path.split(os.path.split(os.path.realpath(__file__))[0])[0])[0]
cfgfile = os.path.join(sp, "user/mandoku-settings.cfg")
config.readfp(open(cfgfile))
gltok = config.get("Gitlab", "Private Token")

#get the key path
sshpubkey = config.get("Git", "Public Key") #join(home, ".ssh/id_rsa.pub")

# Connect to get the current user    
gl = gitlab.Gitlab(glurl, gltok)
print gl.getsshkeys()
user=getpass.getuser()
fn = open(sshpubkey)
print user
key = fn.read()
print "key length", len(key)
title = "%s %s" % (user, datetime.now().strftime("%Y-%m-%d %H:%M:%S") )
print "title", title
if gl.addsshkey(title, key):
    print "200 Success"
else:
    print "400 Failed"
    

