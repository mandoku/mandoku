#!/bin/env python -*- coding: utf-8 -*-
# lookup occurrences for a file and store them in a heatmap branch
from __future__ import division
from __future__ import absolute_import

import os, sys, codecs, re, datetime, requests, redis
reload(sys)
sys.setdefaultencoding('utf-8')
#sys.path.insert(0, '/Users/chris/projects/mdweb/app')
sys.path.insert(0, '/Users/chris/krp/mandoku/python/citfind')
sys.path.insert(0, '/Users/chris/work/py/')
import ftlib
from sparsedict import *
from difflib import *
from collections import defaultdict
ch_re = re.compile(ur'(\[[^\]]*\]|&[^;]*;|&amp;[CZ][X3-7]-[A-F0-9]+|.)')
img_re = re.compile(ur'<i[^>]*>')
kanji=Ur'\u3000-\u4DFF\u4e00-\u9FFF\uF900-\uFAFF'
astkanji = Ur'\U00020000-\U0002A6DF\U0002A700-\U0002B73F\U0002B740-\U0002B81F\U0002B820-\U0002F7FF'
pua=Ur'\uE000-\uF8FF'
astpua = Ur'\U000F0000-\U000FFFFD\U00100000-\U0010FFFD'
ent=r'\[\[.*?\]\]|\[[^\]]*\]|&[^;]*;|&amp;[CZ][X3-7]-[A-F0-9]+'

kp_re = re.compile(u"([0-9]+|%s|{[%s%s]+:[^}]*}|[%s%s])" % (ent, kanji+astkanji, pua+astpua, kanji+astkanji, pua+astpua))
db=6
CLIENT = redis.Redis(host='localhost', port=6379, db=db, charset='utf-8', errors='strict')
r=CLIENT
#r.flushdb()
#this actually alos includes kanji punctuation
kanji=Ur'\u3000-\u4DFF\u4e00-\u9FFF\uF900-\uFAFF'
astkanji = Ur'\U00020000-\U0002A6DF\U0002A700-\U0002B73F\U0002B740-\U0002B81F\U0002B820-\U0002F7FF'
pua=Ur'\uE000-\uF8FF'
astpua = Ur'\U000F0000-\U000FFFFD\U00100000-\U0010FFFD'
# the length of the ngram
n=4

cnt = 0
phrases=[]
pdict=SparseDict()
files = os.listdir(".")
files.sort()
ox = codecs.open("ngram-%d-raw-1.tab" % (n), "w", "utf-8")
for fx in files:
    if fx.endswith(".txt"):
        print fx
        lines = codecs.open(fx, "r", "utf-8").readlines()
        for line in lines:
            line = re.sub("<[^>]*>", "", line)
            if len(line.strip()) < 1 or not kp_re.match(line.strip()[0]):
                continue
            else:
                line=line.strip().split("\t")[0]
                l=ftlib.md_re.sub("", line.strip())
                cnt += 1
                phrases.append((cnt, l))
# now build the ngrams:

def procng(ng, nl, cutoff=None):
#    res = ",".join([a.split("\t")[1] for a in ftlib.keycmp(ng[0:nl],cutoff=cutoff)])
    # now we go for the whole keys, this will take some time...
    res = ",".join([a.split("\t")[1] for a in ftlib.keycmp(ng[0:nl], klen=1)])
    return res
ng=""
nextp=0
ngcnt = 0
pidx = 1
for i, c in enumerate (phrases):
    p = c[1]
    pn= c[0]
    pdict[pidx]=pn
    pidx += len(p)
    ng += p
    while len(ng) > n:
        ngcnt += 1
        r=procng(ng, n, cutoff=0.99)
        ng=ng[1:]
        ox.write("%d\t%d\t%s\t%s\n" % (pdict[ngcnt], ngcnt, ng[0:n], r))
        print pdict[ngcnt], ngcnt, ng[0:n]

ngcnt += 1
r=procng(ng, n, cutoff=0.99)
ox.write("%d\t%d\t%s\t%s\n" % (pdict[ngcnt], ngcnt, ng[0:n], r))
print pdict[ngcnt], ngcnt, ng
ox.close()
