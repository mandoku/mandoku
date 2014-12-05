# -*- coding: utf-8 -*-
import sys, os, os.path, getpass, ConfigParser, codecs
reload(sys)
sys.setdefaultencoding('utf-8')
from os.path import expanduser, join
from datetime import datetime
p = os.path.split(os.path.realpath(__file__))[0]
sys.path.insert(0, p)
import gitlab

sp = os.path.split(os.path.split(os.path.split(os.path.realpath(__file__))[0])[0])[0]

home = expanduser("~")
glurl="http://gl.kanripo.org"

#get the token
config = ConfigParser.ConfigParser()
cfgfile = os.path.join(sp, "user/mandoku-settings.cfg")
config.readfp(open(cfgfile))
gltok = config.get("Gitlab", "Private Token")

#get the key path
sshpubkey = os.path.join(home, ".ssh/glkanripo.pub")


# Connect to get the current user    
#proxies = { "http" : "http://proxy.kuins.net:8080"}

gl = gitlab.Gitlab(glurl, gltok)
#print gl.getsshkeys()
user=getpass.getuser()
fn = codecs.open(sshpubkey, 'r', 'utf-8')
#print user
key = fn.read()
print "key length", len(key), key
print sshpubkey
title = u"%s %s" % ("dummy", datetime.now().strftime("%Y-%m-%d %H:%M:%S") )
print "title", title
if gl.addsshkey(title, key):
    print "200 Success: Successfully uploaded the key."
    sys.exit(0)
else:
    print "400 Failed: Could not upload the key. Something went wrong. Maybe the key exists already?"
    sys.exit(256)


