# -*- coding: utf-8 -*-
import couchdb, re
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
    ek=sk+U"\u30000"

res = db.view('_design/test/_view/ngram3', startkey=sk, endkey=ek)

for k in res:
    docid, pos =k.value
    doc = db[doc]
    s = re.sub(r'\n|\xb6', '', "".join(["".join(z) for z in doc['seq'][pos-15:pos+15]]))