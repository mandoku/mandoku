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
                        line = re.sub("file:", "mandoku:", line)
                        of.write(line)
                    of.close()
                    inf.close()
                    os.remove(dirname + '/' + ofn)
                    nrep.index.add([nf])
                    nrep.index.commit("Changed index file %s." % (nf))
os.path.walk('.', proctxt, "")
                             


