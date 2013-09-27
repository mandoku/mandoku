def fun(doc):
  if (doc.has_key('seq')):
    
    cnt = len(doc['seq'])
    n=3
    for j in range(0, cnt):
        sx="".join([a[0] for a in doc['seq'][j:j+n]])
        yield sx, (doc['_id'], j)
    if doc.has_key('variants') and doc['_id'] == 'test':
        for v in doc['variants'].keys():
            for p in doc['variants'][v].keys():
                pos = int(p)
                pre = "".join(a[0] for a in doc['seq'][pos - (n-1): pos])
                var = doc['variants'][v][p]
                post = "".join(a[0] for a in doc['seq'][pos+1: pos+1 - len(var) + 1 + (n-1) ])
                res = pre + var + post
                for j in range(0, n):
                    yield res[j:j+n], (doc['_id'], pos - n + j + 1, v)
 