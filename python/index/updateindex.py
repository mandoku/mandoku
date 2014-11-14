#execfile('/Users/chris/.virtualenvs/mdidx/bin/activate_this.py', #dict(__file__='/Users/chris/.virtualenvs/mdidx/bin/activate_this.py'))

import sys, os.path
reload(sys)
sys.setdefaultencoding('utf-8')

sys.path.insert(0, '/tmp/mdidx/lib/python2.7/site-packages')

sys.path.insert(0, '/Users/chris/krp/mandoku/python/index')

import mandoku_idx as md



f="/Users/chris/krp/test/ZB2o/ZB2o0025"
#f="/Users/chris/00scratch/testtxt/ZB6e/ZB6e0001.n/test3.txt"

i = md.StartIndex(f)
#print sys.argv[0]

#print "\n".join(i)
