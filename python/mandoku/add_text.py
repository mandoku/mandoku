import couchdb
from mandoku import mandoku
from mandoku import mandoku_couch
server = couchdb.Server()
db = server['md-tests']
m = server['meta']
c1 = mandoku_couch.CouchMandoku(db, m, None, 100000, '/Users/chris/db/text/cbeta/T/T01n0005')
c1.connectText()
c1.addOtherBranches()
