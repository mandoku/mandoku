#!/bin/env python -*- coding: utf-8 -*-
# find citations
from __future__ import division
from __future__ import absolute_import

import os, sys, codecs, re, datetime, requests
from difflib import *
ch_re = re.compile(ur'(\[[^\]]*\]|&[^;]*;|&amp;[CZ][X3-7]-[A-F0-9]+|.)')
img_re = re.compile(ur'<i[^>]*>')

url = u"http://dao3.zinbun.kyoto-u.ac.jp:5000/api/v1.0/search?query=%s"

inp = u"真人問神人吾生不知可謂何等而常喜乎神人言子猶觀昔者博大真人邪所以先生而後老者以其廢邪人而獨好真道真道常保而邪者消凡人盡困窮而我獨長存即是常喜也昭昭獨樂何忿之哉卒爲不能長生當奈何神人言積習近成思善近生夫道者乃無極之經也前古神人治之以真人爲臣以治其民故民不知上之有天子也而以道自然無爲自治其次真人爲治以仙人爲臣不見其民時將知有天子也聞其教敕而尊其主也其次仙人爲治以道人爲臣其治學微有刑被法令彰也而民心動而有畏懼巧詐將生也其次霸治不詳擇其臣民多冤而亂生焉去治漸遠去亂漸近不可復制也是故思神致神思真致真思仙致仙思道致道思智致智聖人之精思賢人致賢人之神來祐之思邪致愚人之鬼來惑之人可思念皆有可致在可思者優劣而已故上士爲君乃思神真中士爲君乃心通而多智下士爲君無可能思隨命可爲"

r = requests.get(url % (inp[0:2]))
if r.ok:
    t = r.text.split("\n")[1:]
    cnt = 0
    res = []
    for i, l in enumerate(t[0:2]):
        tmp = l.split("\t")
        # skip if we have a variant
        if len(tmp) > 2:
            continue
        s = tmp[0].split(",")
        # for the first l, I would need only the stuff starting with inp[0]
        if i == 0:
            l1 = "%s%s" % (inp[0], s[0])
        else:
            l1 = "%s%s%s" % (s[1], inp[0], s[0])
        for j, c in enumerate(l1, cnt):
            test = inp[j]
            if c == test:
                res.append([c, 3])
            elif (test in l1):
                res.append([c, 1, l1.index(test), test])
            else:
                res.append([c, 0])
        
