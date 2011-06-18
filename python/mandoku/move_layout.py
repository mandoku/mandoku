#! /usr/bin/python
# -*- coding: utf-8 -*-

import mandoku, os

mddir="/Users/Shared/md/text/cbeta/T/"
kdir = "/tmp/tkout/"
outdir = "/tmp/mdout/"
try:
    os.rmdir(outdir)
except:
    pass
try:
    os.mkdir(outdir)
except:
    pass
for f in os.listdir(kdir):
    fid = f[:-4]
    print f
    f1 = mandoku.MandokuText(mddir + fid)
    try:
        f1.read()
    except:
        print "Error: ", f
        continue
    f2 = mandoku.MandokuText(kdir + f)
    f2.read()
    f=mandoku.MandokuComp(f1)
    f.setothertext(f2)
    f.move_pg_and_xb6tot2(0)
    f.segment(f1, f2)
    f2.sections = f2.newsections
    f2.defs['LASTPB'] = f1.defs['lastpb'].replace('<pb:', '<md:')
    f2.defs['title'] = f1.defs['title'].split()[-1]
    mkdir(outdir + fid)
    f2.write_to_sections(outdir + fid, True)
