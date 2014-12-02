# -*- coding: utf-8 -*-
"make a repository on gitlab for named <name>, token and user are taken from the cfg file."
import sys, os.path
sys.path.insert(0, '/Users/chris/krp/system/python')
sys.path.insert(0, os.path.realpath(__file__))
import gitlab, os, getpass, ConfigParser
#from os.path import expanduser, join
from datetime import datetime

glurl="http://gl.kanripo.org"
text = sys.argv[1]
try:
    desc = sys.argv[2]
except:
    desc = ""
#get the token
config = ConfigParser.ConfigParser()
sp = os.path.split(os.path.split(os.path.split(os.path.realpath(__file__))[0])[0])[0]
cfgfile = os.path.join(sp, "user/mandoku-settings.cfg")
config.readfp(open(cfgfile))
gltok = config.get("Gitlab", "Private Token")
gluser= config.get("Gitlab", "Username")

gl = gitlab.Gitlab(glurl, gltok)


# print "Adding project %s, %s" % (text, desc)

n=gl.createproject(text, None, description=desc)

if n:
    print n['ssh_url_to_repo']
else:
    #lets see if it already exists!
    p = gl.getprojects()
    rep = {}
    for a in p:
        if a['name'] == text:
            rep=a
#            x = a['ssh_url_to_repo']
    if len(rep) > 0:
        print rep['default_branch'], rep['ssh_url_to_repo']
    else:
        print False
        
