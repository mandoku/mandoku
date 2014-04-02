#! /usr/bin/python
# -*- coding: utf-8 -*-

import sys, getpass
user=getpass.getuser()
sys.path.insert(0, '/Users/' + user + '/db/mandoku/python/mandoku')


import mandoku, os, git

try:
    rep = git.Repo('.')
except:
    print "This program needs to be run within a Mandoku text directory und git control"
    exit
    
