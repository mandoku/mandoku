#! /usr/bin/python
# -*- coding: utf-8 -*-

import sys, getpass
user=getpass.getuser()
sys.path.insert(0, '/Users/' + user + '/db/mandoku/python/mandoku')


import mandoku, os, git


def prepare(textpath=".", src="master"):
    """Move the layout from the srcbranch to the targetbranch. If
    targetbranch is None, use the current branch."""
    f1 = mandoku.MandokuText(textpath, src)
    rd = os.realpath(textpath)
    wp = rd + '.wiki'
    try:
        wiki = git.Repo(wp)
    except:
        wiki = git.Repo.init(wp)
        wiki.git.checkout(b="master")
    try:
        os.mkdir(wp + '/varlist')
        wiki.index.add(['varlist'])
    except:
        pass
    f1.read()
    f1.add_metadata()
    f1.addOtherBranches()
    

