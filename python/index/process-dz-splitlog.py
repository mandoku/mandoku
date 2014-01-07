#!/bin/env python -*- coding: utf-8 -*-
# convert the log files produced by mandoku_idx to a format usable for mandoku-el
# usage:  <script> logfile collection subcoll
import sys, codecs, re, os

tab={'a':'1', 'b':'2', 'c':'3', 'd': '4', 'e': '5', 'f': '6', 'g':'7', 'h':'8', 'i':'9'}

inf=codecs.open(sys.argv[1], 'r', 'utf-8')
oldvol = ''
textids ={}
coll = sys.argv[2]
print """(defun mandoku-%s-vol-page-to-file (vol page)
"converts the text id and page to a file name.  Page is numeric with the ending abc as 123"
  (cond 
"""%(coll)

for line in inf:
    ps = line[:-1].split('\t')
    # try:
    #     textids[ps[0]].append(ps[1])
    # except:
    #     textids[ps[0] = [ps[1]]
    try:
        pb = ps[2][4:-1].split('_')[2]
    except:
        continue
    vol = pb[0:2]
    page = pb[2:6] + tab[pb[-1]]
    if oldvol != vol:
        print '\t\t))\n'
        print "  ((<= vol %s)\n    (cond" % (vol)
        oldvol = vol
    print '	   ((<= page %s%2.2d) "%s")'%(page, int(ps[3]), ps[1])

print "\t\t))))\n"        

# print """(defun mandoku-%s-textid-to-title (textid)
#   (cond """%(coll)
# for t in textids.keys():
#     print '   ((equal textid "%s") "%s")'%(t, textids[t])
# print "))"
