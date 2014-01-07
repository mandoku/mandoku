#!/bin/env python -*- coding: utf-8 -*-
# convert the log files produced by mandoku_idx to a format usable for mandoku-el
# usage:  <script> logfile collection subcoll
import sys, codecs, re, os

tab={'a':'1', 'b':'2', 'c':'3', 'd': '4', 'e': '5', 'f': '6', 'g':'7', 'h':'8', 'i':'9'}

inf=codecs.open(sys.argv[1], 'r', 'utf-8')
oldvol = ''
textids ={}
coll = sys.argv[2]
subcoll = sys.argv[3]
print """(defun mandoku-%s-%s-vol-page-to-file (vol page)
"converts the text id and page to a file name.  Page is numeric with the ending abc as 123"
  (cond 
"""%(coll, subcoll)

for line in inf:
    if line.startswith('#'):
        current=line[1:-1]
        vol = current[1:current.find('n')]
        if oldvol != vol:
            if len(textids) > 0:
                print '\t\t))\n'
            print "  ((<= vol %s)\n    (cond" % (vol)
            oldvol = vol
        textids[current] = []
    elif line.startswith('.'):
        f = line[:-1].split('\t')
        try:
            page = f[1].split('_')[2]
        except:
            sys.stderr.write(f[1]) 
        ## replace the letter at the end with the number from tab
        try:
            page = page[:-1] + tab[page[-1]]
            line = int(f[2])
            page = "%s%2.2d"%(page, line)
        except:
            pass
#        print '	   ((<= page %s) (list "%s%s"  "<title>"))'%(page, subcoll, f[0][1:])
        print '	   ((<= page %s) "%s%s")'%(page, subcoll, f[0][1:])

print "\t\t))))\n"        

# print """(defun mandoku-%s-textid-to-title (textid)
#   (cond """%(coll)
# for t in textids.keys():
#     print '   ((equal textid "%s") "%s")'%(t, textids[t])
# print "))"
