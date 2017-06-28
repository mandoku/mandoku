#!/bin/env python -*- coding: utf-8 -*-
# lookup occurrences for a file and store them in a heatmap branch
from __future__ import division
from __future__ import absolute_import

import os, sys, codecs, re, datetime, requests, redis
reload(sys)
sys.setdefaultencoding('utf-8')
#sys.path.insert(0, '/Users/chris/projects/mdweb/app')
sys.path.insert(0, '/Users/chris/krp/mandoku/python/citfind')
import ftlib
from difflib import *
from collections import defaultdict
ch_re = re.compile(ur'(\[[^\]]*\]|&[^;]*;|&amp;[CZ][X3-7]-[A-F0-9]+|.)')
img_re = re.compile(ur'<i[^>]*>')
kanji=Ur'\u3000-\u4DFF\u4e00-\u9FFF\uF900-\uFAFF'
astkanji = Ur'\U00020000-\U0002A6DF\U0002A700-\U0002B73F\U0002B740-\U0002B81F\U0002B820-\U0002F7FF'
pua=Ur'\uE000-\uF8FF'
astpua = Ur'\U000F0000-\U000FFFFD\U00100000-\U0010FFFD'
ent=r'\[\[.*?\]\]|\[[^\]]*\]|&[^;]*;|&amp;[CZ][X3-7]-[A-F0-9]+'
kp_re = re.compile(u"(%s|{[%s%s]+:[^}]*}|[%s%s])" % (ent, kanji+astkanji, pua+astpua, kanji+astkanji, pua+astpua))
db=6
CLIENT = redis.Redis(host='localhost', port=6379, db=db, charset='utf-8', errors='strict')
r=CLIENT
#r.flushdb()
#this actually alos includes kanji punctuation
kanji=Ur'\u3000-\u4DFF\u4e00-\u9FFF\uF900-\uFAFF'
astkanji = Ur'\U00020000-\U0002A6DF\U0002A700-\U0002B73F\U0002B740-\U0002B81F\U0002B820-\U0002F7FF'
pua=Ur'\uE000-\uF8FF'
astpua = Ur'\U000F0000-\U000FFFFD\U00100000-\U0010FFFD'


def partition(lst, n):
    division = len(lst) / float(n)
    return [ lst[int(round(division * i)): int(round(division * (i + 1)))] for i in xrange(n) ]
    


def cscore(s, ref, cutoff=0.5):
    m = SequenceMatcher()
    m.set_seq1(ref)
    m.set_seq2(s)
    r = m.quick_ratio()
    if r > cutoff:
        return (r, s)


files = os.listdir(".")
files.sort()
for fx in files:
    if fx.endswith(".txt"):
        print fx
        lines = codecs.open(fx, "r", "utf-8").readlines()
        oxf = fx.replace("txt", "bak")
        ox = codecs.open(oxf, "w", "utf-8")
        for line in lines:
            if len(line.strip()) < 1 or not kp_re.match(line.strip()[0]):
                ox.write(line)
            else:
                l=ftlib.md_re.sub("", line.strip())
                if len(l) < 4:
                    ox.write(line)
                else:
                    res = ",".join([a.split("\t")[1] for a in ftlib.keycmp(l)])
                    ox.write("%s\t%s\n" % (line[:-1], res))
        ox.close()
