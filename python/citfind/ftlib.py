#    -*- coding: utf-8 -*-
## this is lifted from flask_sqlalchemy,
## maybe overkill...
from math import ceil
import re, requests, redis
import subprocess
from collections import defaultdict
from difflib import SequenceMatcher

db=6
redis_store = redis.Redis(host='localhost', port=6379, db=db, charset='utf-8', errors='strict')

zbmeta = "kr:meta:"
kr_user = "kr_user:"
tpref="taisho:"
## dictionary stuff.  really should wrap this in an object?!
img_re = re.compile(ur'<i[^>]*>')
md_re = re.compile(ur"<[^>]*>|[0-9　-㄀＀-￯\n¶\.]+|\t[^\n]+\n|\$[^;]+;")
gaiji = re.compile(r"&([^;]+);")
imgbase = "<img height='20' width='20' alt='{gaiji}' title='{gaiji}' src='https://raw.githubusercontent.com/kanripo/KR-Gaiji/master/images/{gaiji}.png'/>"

brtab = defaultdict(lambda : u"【使用者版】", {
u'DCS' : u'【東禪寺】',
u'GOZAN' : u'【日本五山版】',
u'CK-KZ-jye' : u'【道藏輯要電子版】',
u'ZTDZ' : u'【正統道藏・三家本】', 
u'T@DUN' : u'【大→敦】', 
u'T@XI' : u'【大→西】', 
u'WLG' : u'【四庫全書・文瀾閣】', 
u'T@LIYI' : u'【大→麗乙】', 
u'T@LIUBUBEN' : u'【大→流布本】', 
u'T@HE' : u'【大→和】', 
u'K' : u'【麗】', 
u'T@DUNBING' : u'【大→敦丙】', 
u'T@ZHI' : u'【大→知】', 
u'L' : u'【乾隆大藏經】', 
u'P' : u'【永樂北藏】', 
u'T' : u'【大】', 
u'X' : u'【卍續】', 
u'T@SEN' : u'【大→森】', 
u'T@DE' : u'【大→德】', 
u'T@MINGYI' : u'【大→明異】', 
u'YP-C' : u'【原版道藏輯要】', 
u'T@B' : u'【大→Ｂ】', 
u'T@A' : u'【大→Ａ】', 
u'T@DUNYI' : u'【大→敦乙】', 
u'T@NAN' : u'【大→南】', 
u'T@JIU' : u'【大→久】', 
u'T@JIN' : u'【大→金】', 
u'SBCK' : u'【四部叢刊】', 
u'SBCK-zw' : u'【四部叢刊-別】', 
u'T@SHENGYI' : u'【大→聖乙】', 
u'T@JIA' : u'【大→甲】', 
u'TKD' : u'【高麗藏・東國影印版】', 
u'TK' : u'【高麗藏・東國影印版】', 
u'T@RIGUANG' : u'【大→日光】', 
u'T@BO' : u'【大→博】', 
u'T@WAN' : u'【大→万】', 
u'T@JI' : u'【大→己】', 
u'QISHA' : u'【磧砂】', 
u'T@YI' : u'【大→乙】', 
u'C' : u'【中華大藏經】', 
u'G' : u'【佛教大藏經】', 
u'T@SHENGBING' : u'【大→聖丙】', 
u'T@DING' : u'【大→丁】', 
u'S' : u'【宋藏遺珍】', 
u'W' : u'【藏外佛經】', 
u'T@LIUTONGBEN' : u'【大→流通本】', 
u'T@GONG' : u'【大→宮】', 
u'T@SONG' : u'【大→宋】', 
u'T@DONG' : u'【大→東】', 
u'T@ZHONG' : u'【大→中】', 
u'T@NANZANG' : u'【大→南藏】', 
u'CK-KZ' : u'【重刊道藏輯要】', 
u'ZHWIKI' : u'【維基文庫】', 
u'T@UNKNOWN' : u'【大→unknown】', 
u'WYG' : u'【四庫全書・文淵閣】', 
u'T@GONGYI' : u'【大→宮乙】', 
u'Yan' : u'【柳田版】', 
u'HFL' : u'【正統道藏・涵芬樓版】', 
u'T@JIAXING' : u'【大→嘉興】', 
u'T@LI' : u'【大→麗】', 
u'T@BING' : u'【大→丙】', 
u'T@XUZANG' : u'【大→卍續】', 
u'master' : u'master', 
u'T@SHI' : u'【大→石】', 
u'T@GAO' : u'【大→高】', 
u'T@BEIZANG' : u'【大→北藏】', 
u'F' : u'【房山石徑】', 
u'T@SHENG' : u'【大→聖】', 
u'J' : u'【嘉興】', 
u'YAN' : u'【柳田版】', 
u'T@JIAKAOWEI' : u'【大→甲考偽】', 
u'T@TI' : u'【大→醍】', 
u'T@LONG' : u'【大→龍】', 
u'T@MING' : u'【大→明】', 
u'T@FUYI' : u'【大→福乙】', 
u'ZHW' : u'【維基文庫】', 
u'T@FU' : u'【大→福】', 
u'DALI' : u'【大曆】', 
u'T@NEI' : u'【大→內】', 
u'T@HARA' : u'【大→原】', 
u'T@SUO' : u'【大→縮】', 
u'T@QISHA' : u'【大→磧砂】', 
u'CBETA' : u'【電子佛典集成】', 
u'T@YUAN' : u'【大→元】', 
u'T@WU' : u'【大→戊】', 
u'A' : u'【趙城金藏】', 
u'M' : u'【卍正藏】', 
u'U' : u'【洪武南藏】', 
u'T@LIUBUBIE' : u'【大→流布別本】', 
u'T@DUNFANG' : u'【大→敦方】', 
u'T@SHIGU' : u'【大→獅谷】', 
u'T@LI-CBETA' : u'【大→麗-CB】', 
u'T@BEI' : u'【大→別】', 
})

dictab = {'cik' : u'探典釋辭',
          'hydcd1' : u'漢語大詞典',
          'hydcd' : u'漢語大詞典',
          'yd' : u'漢語大字典',
          'hydzd' : u'漢語大字典',
          'sanli' : u'三禮辭典',
          'daikanwa' : u'大漢和辞典',
          'koga' : u'禅語字典',
          'guoyu' : u'國語辭典',
          'abc' : u'ABC漢英詞典',
          'lyt' : u'林語堂當代漢英詞典',
          'cedict' : u'漢英詞典',
          'daojiao' : u'道教大辭典',
          'fks' : u'佛光佛學大辭典',
          'handedic' : u'漢德詞典',
          'dfb' : u'丁福報佛學大辭典',
          'unihan' : u'Unicode 字典',
          'kanwa' : u'發音',
          'kangxi' : u'康熙字典',
          'pinyin' : u'羅馬拼音',
          'loc' : u'其他詞典',
          'je' : u'日英仏教辞典',
          'kg' : u'葛藤語箋',
          'ina' : u'稲垣久雄:Zen Glossary',
          'iwa' : u'岩波仏教辞典',
          'zgd' : u'禪學大辭典',
          'oda' : u'織田佛教大辭典',
          'mz' : u'望月佛教大辭典',
          'matthews' : u'Matthews Chinese English Dictionary',
          'naka' : u'佛教語大辭典',
          'yo' : u'横井日英禪語辭典',
          'zgo' : u'禅の語録',
          'zhongwen' : u'中文大辭典',
          'bsk' : u'佛書解説大辭典',
          'bcs' : u'佛教漢梵大辭典',
          'zd' : u'Zen Dust',
          'ku' : u'ku',
          'sks' : u'sks',
          'guxun' : u'故訓匯纂',
          } 


## helper routines
# dic
def formatle(l, e, dicurl):
    "formats the location entry"
    ec = e.split('-')
    if l == "daikanwa":
        #V01-p00716-172
        return "[[%sdkw/p%s-%s#%s][%s : %s]]" % (dicurl, ec[0][1:], ec[1][1:], ec[-1], dictab[l], e)
    elif l == "hydzd" :
        return "[[%shydzd/hydzd-%s][%s : %s]]" % (dicurl, ec[1], dictab[l], e)
    #comment the next two lines to link to the cached files on the server
    elif l == "kangxi":
        return "[[http://www.kangxizidian.com/kangxi/%4.4d.gif][%s : %s]]" % (int(e), dictab[l], e)
    elif l in ["koga", "ina", "bcs", "naka", "zgd", "sanli", "kangxi"] :
        if "," in e:
            v = e.split(',')[0]
        else:
            v = e
        v = re.sub('[a-z]', '', v)
        try:
            return "[[%s%s/%s-p%4.4d][%s : %s]]" % (dicurl, l, l, int(v), dictab[l], e)
        except:
            return "%s : %s" % (dictab[l], e)
            
    elif l == "yo":
        ec = e.split(',')
        return "[[%syokoi/yokoi-p%4.4d][%s : %s]]" % (dicurl, int(ec[0]), dictab[l], e)
    elif l == "mz":
        v = e.split(',')[0]
        v = v.split('p')
#        return "[[%smz/vol%2.2d/mz-v%2.2d-p%4.4d][%s : %s]]" % (dicurl, int(v[0][1:]), int(v[0][1:]), int(re.sub('[a-z]', '', v[1])),  dictab[l], e)
        return "[[%smz/mz-v%2.2d-p%4.4d][%s : %s]]" % (dicurl, int(v[0][1:]), int(re.sub('[a-z]', '', v[1])),  dictab[l], e)
    elif l == "je":
        ec = e.split('/')
        if ec[0] == '---':
            v = re.sub('[a-z]', '', ec[1])
        else:
            v = re.sub('[a-z]', '', ec[0])
        return "[[%sjeb/jeb-p%4.4d][%s : %s]]" % (dicurl, int(v), dictab[l], e)
    elif l == "zhongwen":
        # zhongwen : V09-p14425-1
        return "[[%szhwdcd/zhwdcd-p%5.5d][%s : %s]]" % (dicurl, int(ec[1][1:]), dictab[l], e)
    elif l == "oda" :
        ec = e.split('*')
        try:
            pg = int(ec[-1].split('-')[0])
        except:
            pg = 0
        return "[[%soda/oda-p%4.4d][%s : %s]]" % (dicurl, pg, dictab[l], e)
    else:
        try:
            return "%s : %s" % (dictab[l], e)
        except:
            return "%s : %s" % (l, e)
            
def dicentry(key, dicurl):
    if r:
        try:
            d = r.hgetall(key)
        except:
            return "no result"
        try:
            d.pop('dummy')
        except:
            pass
        if len(d) > 0:
            ks = d.keys()
            ks.sort()
            s = "** %s (%s)" % (key, len(d))
            xtr = ""
            ytr = ""
            df=[]
            lc=[]
            hy=[]
            seen=[]
            for a in ks:
                k = a.split('-')
                if k[0] == 'loc':
                    lc.append(formatle(k[1], d[a], dicurl))
                else:
                    if k[1] == 'kanwa':
                        xtr +=  " " + d[a]
                    if k[1] == 'abc':
                        ytr += " " + d[a]
                    if k[1] == 'hydcd1':
                        hy.append("**** %s: %s\n" % ("".join(k[2:]), d[a]))
                    elif k[1] in seen:
                        df.append("%s: %s\n" % ("".join(k[2:]), d[a]))
                    else:
                        if len(k) > 1:
                            df.append("*** %s\n%s: %s\n" % (dictab[k[1]], "".join(k[2:]), d[a]))
                        else:
                            df.append("*** %s\n%s\n" % (dictab[k[1]],  d[a]))
                        seen.append(k[1])
            if len(hy) > 0:
                hyr = "*** %s\n%s\n" % (dictab['hydcd1'],  "".join(hy))
            else:
                hyr = ""
            if len(df) > 0:
                dfr = "%s\n" % ("".join(df))
            else:
                dfr = ""
            if len(s) + len(xtr) + len(ytr) > 100:
                dx = 100 - len(s) - len(xtr) 
#                print dx
                ytr = ytr[0:dx]
            xtr = ytr = ""
            return u"%s%s%s\n%s%s*** %s\n%s\n" % (s, xtr, ytr, hyr , dfr, dictab['loc'] , "\n".join(lc))
        else:
            return ""
    else:
        return "no redis"

def prevnext(page):
    p = page.split('-')
    if p[-1].startswith ('p'):
        n= int(p[-1][1:])
        fn = fn = "%%%d.%dd" % (len(p[-1]) - 1, len(p[-1]) - 1)
        prev = "%s-p%s" % ("-".join(p[:-1]), fn % (n - 1) )
        next = "%s-p%s" % ("-".join(p[:-1]), fn % (n + 1) )
    else:
        n= int(p[-1])
        fn = fn = "%%%d.%dd" % (len(p[-1]), len(p[-1]))
        prev = "%s-%s" % ("-".join(p[:-1]), fn % (n - 1) )
        next = "%s-%s" % ("-".join(p[:-1]), fn % (n + 1) )
    return prev, next

## search

def doftsearch(key, idxdir=None, exp=3600):
    if not idxdir:
        idxdir = current_app.config['IDXDIR']
    try:
#subprocess.call(['bzgrep -H ^龍二  /Users/Shared/md/index/79/795e*.idx*'], stdout=of, shell=True )
#ox = subprocess.check_output(['bzgrep -H ^%s  /Users/Shared/md/index/%s/%s*.idx*' % (key[1:], ("%4.4x" % (ord(key[0])))[0:2], "%4.4x" % (ord(key[0])))], shell=True )
        ox = subprocess.check_output(['bzgrep -H ^%s  %s/%s/%s/%s*.idx* | cut -d : -f 2-' % (key[1:],
              idxdir,  ("%4.4x" % (ord(key[0])))[0:2], ("%4.4x" % (ord(key[0])))[0:4], "%4.4x" % (ord(key[0])))], shell=True )
#        ox = subprocess.check_output(['bzgrep -H ^%s  %s/%s/%s*.idx* | cut -d : -f 2-' % (key[1:],
#              current_app.config['IDXDIR'],  ("%4.4x" % (ord(key[0])))[0:2], "%4.4x" % (ord(key[0])))], shell=True )
    except subprocess.CalledProcessError:
        return False
    ux = ox.decode('utf8')
    ux = gaiji.sub(lambda x : imgbase.format(gaiji=x.group(1)), ux)
    s=ux.split('\n')
    s=[a for a in s if len(a) > 1]
    #do we want to sort right away here?
    s.sort()
    if len(s) > 0:
        try:
            redis_store.rpush(key, *s)
            if exp:
                redis_store.expire(key, exp)
        except:
            return False
        return True
    else:
        return False

## title search
def dotitlesearch(titpref, key, exp=3600):
    try:
        ox = subprocess.check_output(['bzgrep -H %s  %s/*titles.txt | cut -d : -f 2-' % (key,
              current_app.config['MDBASE']+'/system')], shell=True )
    except subprocess.CalledProcessError:
        return False
    ux = ox.decode('utf8')
    s=ux.split('\n')
    # sort on the title
    s.sort(key=lambda t : t.split('\t')[-1])
    s=[a for a in s if len(a) > 1]
    if len(s) > 0:
        redis_store.rpush(titpref+key, *s)
        redis_store.expire(titpref+key, exp)
        return True
    else:
        return False

def applyfilter(key, fs, tpe):
    """key is the query being searched, fs is a list of filters to apply, tpe is the type of the filter. """
    ox = []
    total = redis_store.llen(key)
    for f in fs:
        # apply the filters:
        if len(f) > 0:
            if f.startswith("$"):
                #e.g. cwittern:$Favorites == content of KR-Workspace/Texts/Favorites.txt
                #first, see if we already have this text in redis:
                u=session['user']
                if len(redis_store.keys(kr_user + u + f)) > 0:
                    ls = redis_store.lrange(kr_user + u + f, 0, redis_store.llen(kr_user + u + f))
                else:
                    if ghfilterfile2redis(u+f) > 0:
                        ls = redis_store.lrange(kr_user + u + f, 0, redis_store.llen(kr_user + u + f))
                    else:
                        ls = []
                ox.extend([k for k in redis_store.lrange(key, 1, redis_store.llen(key)) if k.split("\t")[1].split(':')[0].split("_")[0] in ls])
            elif tpe == 'DYNASTY':
                fx = [redis_store.hgetall("%s%s" % (zbmeta, a.split('\t')[1].split(':')[0].split("_")[0])) for a in redis_store.lrange(key, 1, redis_store.llen(key))]
                fx = ([a['ID'] for a in fx if a.has_key('DYNASTY') and a['DYNASTY'] == f])
                ox.extend([k for k in redis_store.lrange(key, 1, redis_store.llen(key)) if k.split()[1].split(':')[0].split("_")[0] in fx])
            else:
                ox.extend([k for k in redis_store.lrange(key, 1, redis_store.llen(key)) if k.split()[1].split(':')[0][0:len(f)] == f])
    if len(ox) > 0:
        ox=list(set(ox))
        ox.sort()
    return ox

def sortres(rkey, sort, rsort):
    #sort the redis contents of rkey by sort, return list of keys
    llen=redis_store.llen(rkey)
    if "date" in sort:
    #retrieve the sort keys
        ks=[(k, redis_store.zrevrank(rsort, k.split()[1].split(':')[0][0:8])) for k in redis_store.lrange(rkey, 0, llen-1)]
    elif "txtid" in sort:
        ks=[(k, k.split()[1].split(':')[0][0:8]) for k in redis_store.lrange(rkey, 0, llen-1)]
    elif "pre" in sort:
        ks=[(k, "".join([k.split("\t")[0].split(',')[1]])[::-1]) for k in redis_store.lrange(rkey, 0, llen-1)]
    elif "term" in sort:
        ks=[(k, "".join([k.split("\t")[0].split(',')[0]])) for k in redis_store.lrange(rkey, 0, llen-1)]
    #then sort and return the sorted list
    return sorted(ks, key=lambda x : x[1], reverse=sort[0] == "-")
    
def ghfilterfile2redis(ffile):
    #ffile is the filter file to be read $username:filterfile, the location is expanded to a github raw url
    l=[]
    user, gfile = ffile.split("$")
#    print user, gfile
    url = "{url}{user}/KR-Workspace/{user}/Texts/{gf}.txt".format(url=current_app.config['GHRAWURL'], user=user, gf=gfile)
#    print url
    r = requests.get(url)
    if r.status_code != 200:
        return -1
#    print r.content
    for line in r.content.split("\n"):
        if line.startswith("#"):
            continue
        try:
            l.append(line.split()[0])
        except:
            pass
    #should we first make sure the list is empty?
#    print l
    return redis_store.lpush(kr_user + ffile, *l)

def ghsave(pathname, content, repo=None, commit_message=None, new=False):
    #we need the PyGithub patched version with the PR of June 2015 from @ahmad88me 
    #with additional patch by CW [2015-10-08T16:01:18+0900]
    if not repo:
        repo = g.get('ws', None)
    if not commit_message:
        if new:
            commit_message="Added file %s." % (pathname)
        else:
            commit_message="Updated file %s." % (pathname)
    if new:
        #we could also test for existence in the library...
        repo.create_content(pathname, commit_message, content, branch=repo.default_branch)
    else:
        repo.update_content(pathname, commit_message, content, branch=repo.default_branch)

def ghlistcontent(repo, dir, branch=None, ext=None):
    #get the content from default branch otherwise branch specified, optionally filtered by extension
    if not session.has_key('user'):
        return -1
    user=session['user']
    token=session['token']
    gh=Github(token)
    ws=gh.get_repo("%s/%s" % (user, repo))
    if branch:
        c=ws.get_contents(dir, branch)
    else:
        c=ws.get_contents(dir)
    if ext:
        ls = [(a['name'], a['html_url'].replace("/blob/", "/edit/"), a['download_url']) for a in c.raw_data if a['name'].endswith(ext)]
    else:
        ls = [(a['name'], a['html_url'].replace("/blob/", "/edit/"), a['download_url']) for a in c.raw_data]
    return ls

def ghclone(user, token, src="kanripo/KR-Workspace", userbranch=True):
    gh = Github(token)
    u=gh.get_user()
    try:
        ws = gh.get_repo("user/%s" % (src.split("/")[-1]))
        dlu = ws.downloads_url
        return ""
    except:
        ws = u.create_fork(gh.get_repo(src))
    if userbranch:
        l=[a for a in ws.get_branches() if a.name=="master"]
        try:
            nb=ws.create_git_ref("refs/heads/%s" % (user), l[0].raw_data["commit"]["sha"])
        except:
            pass
        rs=ws.edit(name=ws.name, default_branch=user)
    return ""
    #return "Created fork of %s for user %s" % (src, user)

def ghtextdates(user, rsort):
    cnt=0
    ud=ghuserdata(user)
    br="master"
    if not "kanripo" in rsort:
        try:
            br=ud["textdates"]
        except:
            pass
    else:
        td = "kanripo"
    #for 
    url = "{url}{td}/KR-Workspace/{br}/Settings/krp-by-date.txt".format(url=current_app.config['GHRAWURL'], td=td, br=br)
    r = requests.get(url)
    if r.status_code == 200:
        try:
            redis_store.delete(rsort)
        except:
            pass
        for line in r.content.split("\n"):
            f=line.split()
            cnt +=1
            if len(f) > 0:
                redis_store.zadd(rsort, f[0], cnt)
    return 1

def ghuserdata(user):
    #this will retrieve the userdata from gh, clone if necessary
    #kanripo/KR-Workspace/master/Settings/kanripo.cfg
    dx = {}
    for d in ["global", "kanripo"]:
        url = "{url}{user}/KR-Workspace/{user}/Settings/{file}.cfg".format(url=current_app.config['GHRAWURL'], user=user, file=d)
        r = requests.get(url)
        if r.status_code == 200:
            for line in r.content.split("\n"):
                if "=" in line:
                    f = line.split("=", 1)
                    dx[f[0]] = f[1]
    try:
        redis_store.hmset("%s%s:settings" % (kr_user, user), dx)
        return 1
    except:
        return -1


## helper object for view, this could at some point be moved into a flask extension
class Pagination(object):
    """Internal helper class returned by :meth:`BaseQuery.paginate`.  You
    can also construct it from any other SQLAlchemy query object if you are
    working with other libraries.  Additionally it is possible to pass `None`
    as query object in which case the :meth:`prev` and :meth:`next` will
    no longer work.
    """

    def __init__(self, query, page, per_page, total, items):
        #: the unlimited query object that was used to create this
        #: pagination object.
        # ie, the query string
        self.query = query
        #: the current page number (1 indexed)
        self.page = page
        #: the number of items to be displayed on a page.
        self.per_page = per_page
        #: the total number of items matching the query
        self.total = total
        #: the items for the current page
        self.items = items

    @property
    def pages(self):
        """The total number of pages"""
        if self.per_page == 0:
            pages = 0
        else:
            pages = int(ceil(self.total / float(self.per_page)))
        return pages

    def prev(self, error_out=False):
        """Returns a :class:`Pagination` object for the previous page."""
        assert self.query is not None, 'a query object is required ' \
                                       'for this method to work'
        return self.query.paginate(self.page - 1, self.per_page, error_out)

    @property
    def prev_num(self):
        """Number of the previous page."""
        return self.page - 1

    @property
    def has_prev(self):
        """True if a previous page exists"""
        return self.page > 1

    def next(self, error_out=False):
        """Returns a :class:`Pagination` object for the next page."""
        assert self.query is not None, 'a query object is required ' \
                                       'for this method to work'
        return self.query.paginate(self.page + 1, self.per_page, error_out)

    @property
    def has_next(self):
        """True if a next page exists."""
        return self.page < self.pages

    @property
    def next_num(self):
        """Number of the next page"""
        return self.page + 1

    def iter_pages(self, left_edge=2, left_current=2,
                   right_current=5, right_edge=2):
        """Iterates over the page numbers in the pagination.  The four
        parameters control the thresholds how many numbers should be produced
        from the sides.  Skipped page numbers are represented as `None`.
        This is how you could render such a pagination in the templates:

        .. sourcecode:: html+jinja

            {% macro render_pagination(pagination, endpoint) %}
              <div class=pagination>
              {%- for page in pagination.iter_pages() %}
                {% if page %}
                  {% if page != pagination.page %}
                    <a href="{{ url_for(endpoint, page=page) }}">{{ page }}</a>
                  {% else %}
                    <strong>{{ page }}</strong>
                  {% endif %}
                {% else %}
                  <span class=ellipsis>…</span>
                {% endif %}
              {%- endfor %}
              </div>
            {% endmacro %}
        """
        last = 0
        for num in xrange(1, self.pages + 1):
            if num <= left_edge or \
               (num > self.page - left_current - 1 and \
                num < self.page + right_current) or \
               num > self.pages - right_edge:
                if last + 1 != num:
                    yield None
                yield num
                last = num

def gettaisho(vol, page):
    "Retrieves the file for a pair of volume, page of the taisho."
    vol=vol.upper()
    page=page.lower()
    page=page.replace("p", "")
    tmp = re.split("([a-z])", page)
    print page, tmp
    if len(tmp)==1:
        tmp.append("a")
    if len(tmp) == 2 or len(tmp[2]) < 2:
        tmp[2] = "00"
    try:
        pn=float(tmp[0] + str(ord(tmp[1]) - 96) + tmp[2])
    except:
        print tmp
        pn=0
    res=redis_store.zrangebyscore(tpref+vol, pn - 20000, pn + 20000, withscores=True)
    if len(res) > 0:
        fn = [p for p in res if p[1] > pn][0][0].split("_")
    else:
        fn = False
    return fn

def partition(lst, n):
    division = len(lst) / float(n)
    return [ lst[int(round(division * i)): int(round(division * (i + 1)))] for i in xrange(n) ]

def cscore(s, ref):
    m = SequenceMatcher()
    m.set_seq1(ref)
    m.set_seq2(s)
    return m.quick_ratio()

def consorted(defdic):
    #This will take a dictionary of lists and consolidate the list
    #elements in the list are tuples: score, text, location, key
    #scores are added, text values aggregated, one location will be
    #used an the key ignored
    out = []
    for d in defdic:
        s = 0
        t = ""
        for l in defdic[d]:
            s += l[0]
            if len(t) > 0:
                t = t + " / " + l[1] + "-(%s)" % (l[3]) 
            else:
                t = l[1] + "-(%s)" % (l[3]) 
        #print s, t, d, defdic[d]
        out.append((s, t, defdic[d][0][2]))
    out = sorted(out, key=lambda x : x[0], reverse=True)
    return out

def keycmp(kstr, idxdir=None, klen=3, exp=None):
    if not idxdir:
        idxdir="/Users/Shared/krp/index"
    cutoff = float(len(kstr) - 1) / len(kstr)
    key = kstr[0:klen]
    if not redis_store.exists(key): 
        doftsearch(key, idxdir=idxdir, exp=exp)
    res = [key[0:1]+a for a in redis_store.lrange(key, 0, -1)]
    res = [img_re.sub(u"〓",a) for a in res]
    res = [(a, cscore(a[:len(kstr)], kstr)) for a in res]
    # get rid of non-matches
    res = [a[0] for a in res if a[1] > cutoff]
    # filter out the matches on other branches:
    res = [a for a in res if a.split("\t")[-1][0] in ["K", "n"]]
    res = sorted(res, key = lambda k : kformat(k.split("\t")[1]))
    return res

def keyngram(kstr, idxdir=None, klen=3, exp=None):
    """This function moves an ngram windown over the string and collects
the output"""
    idxdir="/Users/Shared/krp/index"
    for i in xrange(len(kstr)-klen+1):
        key = kstr[i:i+klen]
        if not redis_store.exists(key, idxdir=idxdir): 
            doftsearch(key, exp=exp)
        res = [key[0:1]+a for a in redis_store.lrange(key, 0, -1)]
        res = [img_re.sub(u"〓",a) for a in res]
        ##what now?
        res = [(a, cscore(a[:len(kstr)], kstr)) for a in res]
    return res


def kformat(a):
    "make an indexloc sortable KR4d0427_006:6b:5:4:8, or KR5i0038_002:02p049b:10:21:20" 
    t1 = a.split(":")
    try:
        a = "%s:%4.4d%s%3.3d%3.3d%6.6d" % (t1[0], int(t1[1][:-1].replace("p", "0")), t1[1][-1], int(t1[2]), int(t1[3]), int(t1[4]))
    except:
        a = "%s:0000%s%s%3.3d%3.3d%6.6d" % (t1[0], t1[1][:-1].replace("p", "0"), t1[1][-1], int(t1[2]), int(t1[3]), int(t1[4]))
    return a

def kclose(a, b):
    """determine if the index location a is close to the index location
b. Close means in the same juan on neighbouring lines."""
    resp=False
    tx =[a.split(":"), b.split(":")]
    # different text/juan
    if tx[0][0] == tx[1][0]:
        if tx[0][1] == tx[1][1]:
            if abs(int(tx[0][2]) - int(tx[1][2])) < 2:
                resp = True
        else:
            #different pages: are we on neighbouring pages?
            try:
                if abs(int(tx[0][1][:-1]) - int(tx[1][1][:-1])) < 2:
                    #one has to be line 1, the other? fairly large?, not one
                    if int(tx[1][2]) == 1 and max(int(tx[0][2]), int(tx[1][2])) > 1:
                        resp = True
            except:
                pass
    return resp

def kcondense (res, kf = lambda x : x.split("\t")[1]):
    """This will return a list with locations that occur in proximity,
grouped in lists of adjoining results. The list has to be sorted by
location."""
    out = []    
    for i, rx in enumerate(res):
        if len(out) > 0:
            if kclose(kf(out[-1][-1]), kf(res[i])):
                if res[i] in out[-1]:
                    pass
                else:
                    out[-1].append(res[i])
                continue
        if i < len(res) - 1:
            if kclose(kf(res[i]), kf(res[i+1])):
                out.append([res[i], res[i+1]])
    return out

def krestore(l):
    """Restores an index line to the original string, removes the location"""
    fx = l.split("\t")[0].split(",")
    return "%s%s" % (fx[1], fx[0])

def kcombine(lx):
    """Combines the index items in the list to one string where possible."""
    res = lx[0]
    for s in lx[1:]:
        tmp = res[-4:]
        if s.find(tmp) > 0:
            res = res + s[s.index(tmp):]
        else:
            res = res + " / " + s
    return res
