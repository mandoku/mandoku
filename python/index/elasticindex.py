#execfile('/Users/chris/.virtualenvs/mdidx/bin/activate_this.py', #dict(__file__='/Users/chris/.virtualenvs/mdidx/bin/activate_this.py'))

import sys, os.path, re
import resource
reload(sys)
sys.setdefaultencoding('utf-8')
idxpath=os.path.split(os.path.realpath(__file__))[0]

sys.path.insert(0, idxpath)
#sys.path.insert(0, '/Users/chris/krp/system/python')

#resource.setrlimit(resource.RLIMIT_NOFILE, (1000, -1))

import mandoku_idx as md

txtid_re=re.compile(r"[A-z]+[0-9][A-z]+[0-9]{4}[a-z]*$")

try:
    mdbase = sys.argv[1]
except:
    print "Please provide mdbase!"

mdtext = "%s/gh" % (mdbase)

def textwalk(arg, dirname, names):
    """Processing a dir in the mandoku text hierarchy, looking for files to index.:"""
    for f in names:
        if txtid_re.match(f):
            p = "%s/%s" % (dirname, f)
            print p
            md.StartIndex(p, None, left=5, length=15, backend="elastic")

# check for index-queue

# just update
# md.debug = True
os.path.walk(mdtext, textwalk, '')

#for path, dirlist, filelist in os.walk(mdtext):
    
        
