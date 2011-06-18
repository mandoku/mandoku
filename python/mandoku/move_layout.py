#! /usr/bin/python
# -*- coding: utf-8 -*-

import mandoku
reload(mandoku)
f1 = mandoku.MandokuText('/Users/chris/db/text/cbeta/T/T01n0053/')
f1.read()
f2 = mandoku.MandokuText('/tmp/tkout/T01n0053.txt')
f2.read()
f=mandoku.MandokuComp(f1)
f.setothertext(f2)
f.move_pg_and_xb6tot2(0)
f.segment(f1, f2)
f2.sections = f2.newsections
f2.defs['LASTPB'] = f1.defs['lastpb'].replace('<pb:', '<md:')
f2.defs['title'] = f1.defs['title'].split()[-1]
f2.write_to_sections('/tmp', True)
