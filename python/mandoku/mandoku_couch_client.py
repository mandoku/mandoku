# -*- coding: utf-8 -*-
import couchdb
from mandoku import mandoku
from mandoku import mandoku_couch
server = couchdb.Server()
db = server['md-tests']
m = server['meta']

sk=u"如是我聞"
if len(sk) > 3:
    sk=sk[0:3]
    ek=sk[0:3]
    rs=sk[4:]
elif len(sk) < 3:
    sk=sk
    ek=sk+u"\u30000"

res = db.view('_design/test/_view/ngram3', startkey=sk, endkey=ek)