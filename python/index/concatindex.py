#!/usr/bin/env python    -*- coding: utf-8 -*-
#concat index files for subcollections of KR
from __future__ import division
import sys, os, codecs, datetime, git, re, shutil
from collections import defaultdict

try:
    krpbase = sys.argv[1]
except:
    krpbase = "/Users/Shared/krp/"
kridx= "%s%s" % (krpbase, "index_stage")
ccidx= "%s%s" % (krpbase, "index")

try:
    os.makedirs(ccidx)
except:
    shutil.rmtree(ccidx)
    os.makedirs(ccidx)

for d1 in os.listdir(kridx):
    sa = "%s/%s" % ( kridx, d1)
    sal = os.listdir(sa)
    sal.sort()
    for d in sal:
        sd = "%s/%s" % (sa, d)
        if os.path.isdir(sd):
            d2l = os.listdir(sd)
            d2l.sort()
            for d2 in d2l:
                sd2 = "%s/%s" % (sd, d2)
                if os.path.isdir(sd2):
                    try:
                        os.makedirs("%s/%s/%s" % (ccidx, d, d2))
                    except:
                        pass
                    files = [a for a in os.listdir(sd2) if not a.startswith(".")]
                    files.sort()
                    try:
                        c = files[0].split(".")[0]
                    except:
                        print "Error:", files
                        continue
                    df = defaultdict(list)
                    [df[a.split(".")[1][0:4]].append(a) for a in files]
                    for o in df:
                        nf = "%s/%s/%s/%s.%s.idx" % (ccidx, d, d2, c, o)
                        print "Opening", nf
                        with codecs.open(nf,  "w", "utf-8") as outfile:
                            for f in df[o]:
                                #print o, f
                                shutil.copyfileobj(codecs.open("%s/%s" % (sd2 , f), "r", "utf-8"), outfile)
