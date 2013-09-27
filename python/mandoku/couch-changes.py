from couchdb import Server                                                                                                                

cdbs = Server('http://localhost:5984/')
db = cdbs['recipes']
# the since parameter defaults to 'last_seq' when using continuous feed
ch = db.changes(feed='continuous',heartbeat='1000')                                                                                       

counter = 0                                                                                                                               

for line in ch:
    counter=counter+1
    if (counter > 10):
       db.view('recipes/by_name')
       counter = 0
