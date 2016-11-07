#!/bin/env python -*- coding: utf-8 -*-
# find citations
from __future__ import division
from __future__ import absolute_import

import os, sys, codecs, re, datetime, requests
from difflib import *
from collections import defaultdict
ch_re = re.compile(ur'(\[[^\]]*\]|&[^;]*;|&amp;[CZ][X3-7]-[A-F0-9]+|.)')
img_re = re.compile(ur'<i[^>]*>')

txtbydateurl = "https://raw.githubusercontent.com/kanripo/KR-Workspace/master/Settings/krp-by-date.txt"

url = u"http://dao3.zinbun.kyoto-u.ac.jp:5000/api/v1.0/search?query=%s"

inp = u"真人問神人吾生不知可謂何等而常喜乎神人言子猶觀昔者博大真人邪所以先生而後老者以其廢邪人而獨好真道真道常保而邪者消凡人盡困窮而我獨長存即是常喜也昭昭獨樂何忿之哉卒爲不能長生當奈何神人言積習近成思善近生夫道者乃無極之經也前古神人治之以真人爲臣以治其民故民不知上之有天子也而以道自然無爲自治其次真人爲治以仙人爲臣不見其民時將知有天子也聞其教敕而尊其主也其次仙人爲治以道人爲臣其治學微有刑被法令彰也而民心動而有畏懼巧詐將生也其次霸治不詳擇其臣民多冤而亂生焉去治漸遠去亂漸近不可復制也是故思神致神思真致真思仙致仙思道致道思智致智聖人之精思賢人致賢人之神來祐之思邪致愚人之鬼來惑之人可思念皆有可致在可思者優劣而已故上士爲君乃思神真中士爲君乃心通而多智下士爲君無可能思隨命可爲"

inp = u"學而時習之不亦說乎有朋自遠方來不亦樂乎人不知而不慍不亦君子乎"
# index parameter:
tbdd={}
tbdt={}
pre = 5
lenx = 20
n = 2
cutoff = 0.6
r = requests.get(txtbydateurl)
if r.ok:
    txtbydate = r.text.split("\n")[1:]
    for tbd in txtbydate:
        f = tbd.split("\t")
        try:
            tbdd[f[0]] = f[1]
            tbdt[f[0]] = f[2]
        except:
            print "oops, ", f
else:
    txtbydate = ""
    
start = 0
tally = []
for start in range(0, len(inp), lenx):
    cur = inp[start:start+lenx]
    print cur[pre:pre+n], cur
    r = requests.get(url % (cur[pre:pre+n]))
    cur = filter(None, re.split(ch_re, cur))
    m = SequenceMatcher()
    m.set_seq1(cur)
    res = []
    if r.ok:
        t = r.text.split("\n")[1:]
        cnt = 0
        for i, l in enumerate(t):
            l=re.sub("<.*title='([^']+)[^>]+>", "&\\1;", l)
            tmp = l.split("\t")
            # skip if we have a variant
            if len(tmp) > 2:
                continue
            s = tmp[0].split(",")
            txfno = tmp[1].split(":")[0]
            # for the first l, I would need only the stuff starting with inp[0]
            if i == 0:
                l1 = "%s%s" % (cur[pre], s[0])
            else:
                try:
                    l1 = "%s%s%s" % (s[1][-pre:], cur[pre], s[0])
                except:
                    l1 = "%s%s" % (cur[pre], s[0])
            # split and remove empty strings.
            l1 = filter(None, re.split(ch_re, l1))
            m.set_seq2(l1)
            if m.quick_ratio() >= 0.5:
                print "--", txfno,  "".join(l1), m.ratio()
                res.append((txfno, l1, m.ratio()))
    tally.append((cur, res))        
        # maybe rather compare lists / sets
        # list(set(temp1) - set(temp2)) # diff
        # set(a).intersection(b)  # intersection
        # or use difflib to compare the sequences...
        # for j, c in enumerate(l1, cnt):
        #     if j >= len(inp):
        #         break
        #     test = inp[j]
        #     if c == test:
        #         res.append([c, 3])
        #     elif (test in l1):
        #         res.append([c, 1, l1.index(test), test])
        #     else:
        #         res.append([c, 0])
        
        # sx = sum([a[1] for a in res])
        # if sx > 15:
        #     ones = sum([a[1] for a in res if a[1] == 1])
        #     tally.append((txfno, res, sx, ones, i))
rs = defaultdict(int)
for ta in tally:
    for e in ta[1]:
        txt = e[0].split("_")[0]
        rt = e[-1]
        if rt > cutoff:
            rs[txt] += 1

for r in rs:
    print rs[r], r
