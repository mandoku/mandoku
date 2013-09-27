# -*- coding: utf-8 -*-
from __future__ import division
from __future__ import absolute_import
import couchdb, re
from mandoku.mandoku import mandoku
from mandoku.mandoku import mandoku_couch
n=20
server = couchdb.Server()
db = server['cbeta']
m = server['meta']

sk=u"如是我聞"

if len(sk) > 3:
    sk=sk[0:3]
    ek=sk[0:3]
    rs=sk[4:]
elif len(sk) < 3:
    sk=sk
    ek=sk+U"\uffff"

res = db.view('_design/mandoku/_view/ngram', startkey=sk, endkey=ek)

for k in res:
    docid, pos =k.value
    doc = db[docid]
    pre = re.sub(r'\n|\xb6|<[^>]*>', '', "".join(["".join(z) for z in doc['seq'][pos- n :pos-1]]))
    post = re.sub(r'\n|\xb6|<[^>]*>', '', "".join(["".join(z) for z in doc['seq'][pos:pos + n]]))
    out = pre[len(pre) - n:] + "" + post[0:n]
