#execfile('/Users/chris/.virtualenvs/mdidx/bin/activate_this.py', #dict(__file__='/Users/chris/.virtualenvs/mdidx/bin/activate_this.py'))

import sys, os.path, re
reload(sys)
sys.setdefaultencoding('utf-8')

sys.path.insert(0, '/tmp/mdidx/lib/python2.7/site-packages')
sys.path.insert(0, '/Users/chris/.pyenv/versions/mdindex/lib/python2.7/site-packages')
sys.path.insert(0, '/Users/chris/krp/mandoku/python/index')

import mandoku_idx as md

txtid_re=re.compile(r"[A-z]+[0-9][A-z]+[0-9]{4}[a-z]*$")

try:
    mdbase = sys.argv[1]
except:
    if 'system' in sys.argv[0]:
        mdbase = sys.argv[0][:sys.argv[0].find('system')]
    else:
        mdbase = "/Users/chris/krpnew"
# todo: look at mandoku-index-queue
print mdbase
mdtext = "%s/text" % (mdbase)
mdidx =  "%s/index" % (mdbase)
try:
    os.makedirs(mdidx)
except:
    pass

def textwalk(arg, dirname, names):
    """Processing a dir in the mandoku text hierarchy, looking for files to index.:"""
    for f in names:
        if txtid_re.match(f):
            p = "%s/%s" % (dirname, f)
            print p
            md.StartIndex(p, mdidx)
            


# check for  



os.path.walk(mdtext, textwalk, '')

#for path, dirlist, filelist in os.walk(mdtext):
    
        
