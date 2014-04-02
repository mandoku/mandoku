#! /usr/bin/python
# -*- coding: utf-8 -*-

import sys, getpass
user=getpass.getuser()
sys.path.insert(0, '/Users/' + user + '/db/mandoku/python/mandoku')


import mandoku, os, git, codecs


def prepare(textpath=".", src="master", n=3):
    """Move the layout from the srcbranch to the targetbranch. If
    targetbranch is None, use the current branch."""
    l = []
    f1 = mandoku.MandokuText(textpath, src)
    rd = os.path.realpath(textpath)
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
    seq=f1.seq
    r = f1.branches
    br = r.keys()
    pp = f1.pages.keys()
    for i in range(1, len(f1.sections)+1):
        s, f = f1.sections[i-1]
        of = codecs.open(wp + '/varlist/' + f, 'w', 'utf-8') 
        start = s
        try:
            end = f1.sections[i][0]
        except:
            end = len(f1.seq)
        for j in range(start, end):
            varflag = 0
            if j in pp:
                currentpage=f1.pages[j]
                l = f1.lines[currentpage]
                ll = 1
            if j in l:
                ll += 1
                lstart = j
            for v in br:
                if r[v].has_key(j):
                    if varflag == 0:
                        varflag = 1
                        print currentpage, ll, seq[j]
#                        of.write("** %s%2.2d %s *%s* %s\n" % (currentpage.split('_')[-1][:-1], ll, s[j-n:j], s[j], s[j+n]))
                    of.write( " %s: %s\n" % (v, "".join(r[v][j])))
        of.close()


try:
    txpath= sys.argv[1]
except:
    txpath = "."

try:
    ed= sys.argv[2]
except:
    ed="master"

prepare(txpath, ed)
