#!/bin/env python -*- coding: utf-8 -*-
# convert the log files produced by mandoku_idx to a format usable for mandoku-el
# usage:  <script> logfile collection subcoll
import sys, codecs, re, os

tab={'a':'1', 'b':'2', 'c':'3', 'd': '4', 'e': '5', 'f': '6', 'g':'7', 'h':'8', 'i':'9'}

inf=codecs.open(sys.argv[1], 'r', 'utf-8')
oldid = ''
textids ={}
titid = {}
oldvol = ''
coll = sys.argv[2]
try:
    subcoll = sys.argv[3]
except:
    subcoll = ""

print """
  (cond
  ((equal subcoll "%s")
   (cond
"""%(subcoll)

for line in inf:
    if line.startswith('some'):
        continue
    f = line[:-1].split('\t')
    textid = f[0]
    if oldid != textid and oldid != '':
        print """	   ((<= page %(page)s) (list "%(textid)s"  "%(title)s"))"""%(textids[oldid])
        vol = textid[1:textid.find('n')]
        if oldvol != vol:
            if len(textids) > 0:
                print '\t\t))\n'
            print "  ((<= vol %s)\n    (cond" % (vol)
            oldvol = vol

    oldid=textid
    try:
        textids[textid] = {'textid': textid, 'title': f[2], 'page':f[5].replace('_', '')}
        print """	   ((<= page %(page)s) (list "%(textid)s"  "%(title)s"))"""%(textids[textid])
    except:
        pass


print "\t\t))))\n"        

print """(defun mandoku-%s-textid-to-title (textid page)
   (cond """%(coll)
pg = ''
oldvol = ''
k = textids.keys()
k.sort()
for t in k:
    p = textids[t]['page']
    vol = p[0:2]
    pg = p[3:]
    if vol != oldvol:
        if oldvol != '':
            print "))"
        print "((<= vol %s)\n\t\t(cond " % (vol)
        oldvol = vol
    print '   ((<=  page %s) (list "%s"  "%s"))'%(pg, t, textids[t]['title'])

print "))))"
