#!/bin/env python -*- coding: utf-8 -*-
# find citations
from __future__ import division
from __future__ import absolute_import

import os, sys, codecs, re, datetime, requests, redis
reload(sys)
sys.setdefaultencoding('utf-8')
sys.path.insert(0, '/Users/chris/projects/mdweb/app')
import lib
from difflib import *
from collections import defaultdict
ch_re = re.compile(ur'(\[[^\]]*\]|&[^;]*;|&amp;[CZ][X3-7]-[A-F0-9]+|.)')
img_re = re.compile(ur'<i[^>]*>')

url = u"http://dao3.zinbun.kyoto-u.ac.jp:5000/api/v1.0/search?query=%s"

inp = u"真人問神人吾生不知可謂何等而常喜乎神人言子猶觀昔者博大真人邪所以先生而後老者以其廢邪人而獨好真道真道常保而邪者消凡人盡困窮而我獨長存即是常喜也昭昭獨樂何忿之哉卒爲不能長生當奈何神人言積習近成思善近生夫道者乃無極之經也前古神人治之以真人爲臣以治其民故民不知上之有天子也而以道自然無爲自治其次真人爲治以仙人爲臣不見其民時將知有天子也聞其教敕而尊其主也其次仙人爲治以道人爲臣其治學微有刑被法令彰也而民心動而有畏懼巧詐將生也其次霸治不詳擇其臣民多冤而亂生焉去治漸遠去亂漸近不可復制也是故思神致神思真致真思仙致仙思道致道思智致智聖人之精思賢人致賢人之神來祐之思邪致愚人之鬼來惑之人可思念皆有可致在可思者優劣而已故上士爲君乃思神真中士爲君乃心通而多智下士爲君無可能思隨命可爲"

inp = u"學而時習之不亦說乎有朋自遠方來不亦樂乎人不知而不慍不亦君子乎"

db=6
CLIENT = redis.Redis(host='localhost', port=6379, db=db, charset='utf-8', errors='strict')
r=CLIENT
r.flushdb()

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
