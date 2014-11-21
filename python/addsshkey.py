# -*- coding: utf-8 -*-
import gitlab,sys, os, getpass
from os.path import expanduser, join
from datetime import datetime

home = expanduser("~")
sshpubkey = join(home, ".ssh/id_dsa.pub")
fn = open(sshpubkey)
glurl="http://gl.kanripo.org"

try:
    gltok=sys.argv[1]
except:
    sys.exit("Please provide the token for gl.kanripo.org on the command line")

# Connect to get the current user    
gl = gitlab.Gitlab(glurl, gltok)

user=getpass.getuser()
if gl.addsshkey("%s %s" % (user, datetime.now().strftime("%Y-%m-%d %H:%M:%S") ), fn.read()):
    print "200 Success"
else:
    print "400 Failed"
    

