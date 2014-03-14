from __future__ import division
from difflib import *
from itertools import *
sys.path.insert(0, '/Users/chris/db/mandoku/python/mandoku')

import mandoku, os
mddir="/Users/Shared/md/text/cbeta/T/"
kdir = "/tmp/tkout/"
outdir = "/tmp/mdout/"

f = "T01n0001.txt"
fid = f[:-4]
f1 = mandoku.MandokuText(mddir + fid)
f1.read()
f2 = mandoku.MandokuText(kdir + f)
f2.read()
f=mandoku.MandokuComp(f1)
f.setothertext(f2)
f.move_pg_and_xb6tot2(0)
f.segment(f1, f2)

text1=f1
text2=f2

start=53837
limit=59960
ss1=[a[text1.cpos] for a in text1.seq[start:limit]]
ss2=[a[text2.cpos] for a in text2.seq]
s = SequenceMatcher()
s.set_seqs(ss1, ss2)

dx=mandoku.getlimits(s.get_opcodes(), 1, limit-start, 100, 35)
