# -*- coding: utf-8 -*-

import os, sys, codecs, re, git

def proctxt(arg, dirname, names):
    if not (".git" in dirname):
        x = "".join(names)
        if "txt" in x:
            fn = dirname.split('/')[-1]
            of = codecs.open("%s/%s.org" % (dirname, fn), 'w', 'utf-8')
            cnt = 0
            nrep = git.Repo.init(dirname)
            for f in names:
                if f.endswith('txt'):
                    cnt += 1
                    inf = codecs.open("%s/%s" % (dirname, f), 'r', 'utf-8')
                    for line in inf:
                        if line.startswith('#+TITLE:'):
                            title = line[line.index('TITLE')+6:-1]
                            if cnt == 1:
                                of.write(line)
                        elif "JUAN" in line:
                            j = line[line.index('JUAN')+5:-1]
                            j = re.sub(ur'[\[\]]', '', j)
                            of.write("[[mandoku:%s][%s %s]]\n" % (f, title, j))
                            break

            nrep.index.add([fn +'.org'])
            nrep.index.commit("Added index file %s." % (fn+'.org'))  
os.path.walk('.', proctxt, "")
                             


