# -*- coding: utf-8 -*-

import os, sys, codecs

def proctxt(arg, dirname, names):
    names = names.sort()
    fn = dirname.split('/')[-1]
    of = codecs.open("%s/%s.org" % (dirname, fn), 'w', 'utf-8')
    for f in names:
        if f.endswith('txt'):
            
    


