#!/bin/env python    -*- coding: utf-8 -*-
#converts log file from mandoku_idx.py to a collection support file
import codecs, string
coll="sdz"
file=codecs.open('logfile.log', 'r', 'utf-8')


l=""";; mandoku-collection-%(coll)s.el   -*- coding: utf-8 -*-
;; created [2010-01-08T16:08:19+0900]
;; 
(defun mandoku-%(coll)s-volume-page-to-number (vol page)
  "converts the volume and page/line information of a text to a id code for the text
that contains this line.  In difference to the canonic line reference, in this case, the 
upper block \\"a\\" is mapped to 1, \\"b\\" to 4 and \\"c\\" to 7."
	(cond

"""%({'coll': coll })

print l

ovol = ''
tab={'a':'1', 'b':'2', 'c':'3', 'd': '4', 'e': '5', 'f': '6', 'g':'7', 'h':'8', 'i':'9'}


for line in file:
    (id, title, ed, fpage, lpage, date)=line.split('\t')
    vol = fpage[0:2]
    if (vol != ovol):
        ovol = vol
        if vol == '01':
            print "(( <= vol %s)\n(cond" % (vol)
        else:
            print """
            (t "line out of range")
            ))
            ((<= vol %s)
            (cond
"""%(vol)            
            #    page = lpage[2:-1] + tab[lpage[-1]]
    print '(( <= page %s) (list "%s"  "%s" ))'%(lpage[2:], id, title)

print "))))"

