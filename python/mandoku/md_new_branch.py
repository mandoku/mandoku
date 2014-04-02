#! /usr/bin/python
# -*- coding: utf-8 -*-

import sys, getpass
user=getpass.getuser()
sys.path.insert(0, '/Users/' + user + '/db/mandoku/python/mandoku')


import mandoku, os, git


def getlayout(rep, src, target=None):
    """Move the layout from the srcbranch to the targetbranch. If
    targetbranch is None, use the current branch."""
    if not target:
        target = rep.active_branch.name
    else:
        target = rep.git.checkout(b=target)
    try:
        rep.git.checkout(src)
    except:
        sys.exit("Branch %s does not exist!\n" % src)
    f1 = mandoku.MandokuText(".", src)
    f1.read()
    rep.git.checkout(target)
    f2 = mandoku.MandokuText(".", target)
    f2.read()
    f=mandoku.MandokuComp(f1)
    f.setothertext(f2)
    ## important: if we need to segment, then do this first in a different run, this assumes that the file names stay the same
    ## ok, now write it out, register in git and commit
    
try:
    rep = git.Repo('.')
except:
    print "This program needs to be run within a Mandoku text directory under git control"
    sys.exit()

if rep.is_dirty():
    print "Please commit your changes before running this program."
    sys.exit()

if len(sys.argv) < 3:
    print "Please give the source of the layout."
    print len(sys.argv), sys.argv
    sys.exit()

srcbranch=sys.argv[1]


    
