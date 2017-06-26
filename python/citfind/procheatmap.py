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
#this actually alos includes kanji punctuation
kanji=Ur'\u3000-\u4DFF\u4e00-\u9FFF\uF900-\uFAFF'
astkanji = Ur'\U00020000-\U0002A6DF\U0002A700-\U0002B73F\U0002B740-\U0002B81F\U0002B820-\U0002F7FF'
pua=Ur'\uE000-\uF8FF'
astpua = Ur'\U000F0000-\U000FFFFD\U00100000-\U0010FFFD'

tabf="heatmap.tab"
netf="heatmap-net.tab"

tab = codecs.open(tabf, "w", "utf-8")
net = codecs.open(netf, "w", "utf-8")
# lineno, line, total, KR1, KR2, KR3, KR4, KR5, KR6
fstr = "%4.4d\t%s\t%s\t%d\t%d\t%d\t%d\t%d\t%d\t%d\n"
files = os.listdir(".")
files.sort()
cnt = 0
chap = ""
txtdic=defaultdict(int)
for fx in files:
    if fx.endswith(".txt"):
        print fx
        lines = codecs.open(fx, "r", "utf-8").readlines()

        for line in lines:
            if line.startswith("#") or line.startswith("*"):
                continue
            fx = line[:-1].split("\t")
            fx[0] = re.sub("<[^>]*>", "", fx[0])
            nx=re.findall("([\d\.]+)", fx[0])
            if len(nx) > 0:
                chap=nx[0]
            fx[0] = ftlib.md_re.sub("", fx[0].strip())
            if len(fx[0]) < 1:
                continue
            cnt += 1
            if len(fx) == 1:
                tab.write(fstr % (cnt, chap, fx[0], 0,0,0,0,0,0,0))
            else:
                mx = fx[1].split(",")
                mx=[a.split("_")[0] for a in mx]
                secdic=defaultdict(int)
                for m in mx:
                    secdic[m[:3]] += 1
                    txtdic[m] +=1
                tab.write(fstr % (cnt, chap, fx[0], len(mx),secdic['KR1'],secdic['KR2'],secdic['KR3'],secdic['KR4'],secdic['KR5'],secdic['KR6']))
        
kx=txtdic.keys()
kx.sort()
for k in kx:
    net.write("%s\t%4.4d\n" % (k, txtdic[k]))
net.close()
tab.close()
