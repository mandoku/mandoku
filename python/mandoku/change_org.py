# -*- coding: utf-8 -*-

import os, sys, codecs, re, git

def proctxt(arg, dirname, names):
    if not (".git" in dirname):
        x = "".join(names)
        if "txt" in x:
            nrep = git.Repo.init(dirname)
            for f in names:
                if f.endswith('org'):
                    ofn = f[:-3] + 'bak'
                    os.rename(dirname + '/'+ f, dirname + '/' + ofn)
                    nf = f
                    of=codecs.open(dirname + '/' + nf, 'w', 'utf-8')
                    inf = codecs.open("%s/%s" % (dirname, ofn), 'r', 'utf-8')
                    for line in inf:
                        if line.startswith('#+TITLE:'):
                            title = line[line.index('TITLE')+6:-1]
                            if cnt == 1:
                                of.write(line)
                        elif "JUAN" in line:
                            j = line[line.index('JUAN')+5:-1]
                            j = re.sub(ur'[\[\]]', '', j)
                            of.write("[[mandoku:%s][%s %s]]\n" % (f, title.strip(), j))
                            break
            nrep.index.commit("Changed index file %s." % (fn+'.org'))
            
os.path.walk('.', proctxt, "")
                             


