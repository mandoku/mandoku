import couchdb, sys
from mandoku import mandoku_couch
server = couchdb.Server()
db = server['cbeta']
m = server['meta']
file = sys.argv[1]
c1 = mandoku_couch.CouchMandoku(db, m, None, 100000, file)
c1.connectText()
c1.addOtherBranches()
