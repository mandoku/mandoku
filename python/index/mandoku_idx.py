#!/bin/env python -*- coding: utf-8 -*-
# convert txt repositories to mandoku index format
from __future__ import division
from __future__ import absolute_import

import os, sys, codecs, re, datetime, os.path, git, logging
from difflib import *

debug = False
ch_re = re.compile(ur'(\[[^\]]*\]|&[^;]*;|&amp;[CZ][X3-7]-[A-F0-9]+|.)')
img_re = re.compile(ur'<i[^>]*>')
sys.stdout = codecs.getwriter('utf8')(sys.stdout)
outfiles = {}
tab={'a':'1', 'b':'2', 'c':'3', 'd': '4', 'e': '5', 'f': '6', 'g':'7', 'h':'8', 'i':'9'}
idx={}
pcnt = 0
notes = []
defs = {}
teardown=False
# spaeter auf max setzen
gcharcnt = 0
gbackend = "files"
# file:page:line:char:para$gchar
templ = "%s:%s:%2.2d:%2.2d:%d$%d"  


def PrintToIdxfile(outdir, string, collection ):
    global idx
    global gcharcnt
    try:
        code = (u"%4.4x"%(ord(string[0])))
    except:
        code = u"gj"
    if (not code.startswith('30') and  u"()/¶*".find(string[0]) == -1 ):
        #to speed things up, we separate by bu: KR1, KR2, etc.
        if gbackend == "files":
            #ndir = "%s/%s/%s/%s"%(outdir, collection[0:3], code[0:2], code[0:4])
            ndir = "%s/%s/%s/%s"%(outdir, collection[0:3], code[0:2], code[0:4])
            if outdir:
                #for none file based backends, dont produce this
                try:
                    os.makedirs(ndir)
                except:
                    pass
            # this is the old style
            ofile="%s/%s.%s.idx"%(ndir, code, collection)
            try:
                idx[ofile] += "%s\n" % (string[1:])
            except:
                idx[ofile] = "%s\n" % (string[1:])
        else:
            m1 = string.split("\t")
            try:
                text, prev = m1[0].split(",")
                filen, tmp = m1[1].split(":", 1)
                loc, no = tmp.split("$")
                no = int(no)
            except:
                print m1
                return
            mx={'text' : text, 'prev' : prev, 'file' : filen, 'loc' : loc}
            if len(m1) > 2:
                extra = "\t".join(m1[2:])
                mx.update({'extra' : extra})
                if extra.startswith("n"):
                    gcharcnt += 1
                    mx.update({'id' : no})
                else:
                    #print string
                    mx.update({'var' : no})
            else:
                gcharcnt += 1
                mx.update({'id' : no})
            try:
                idx[no].append(mx)
            except:
                idx[no] = [mx]
                
    
            
def MandokuIndex(file, idlogfile='logfile.log', left=2, right=2, length=3, collection='test', use_vol=0):
    usepb = True
    pcnt = 0
    global notes
    defs['txtfile'] = os.path.splitext(os.path.split(file)[-1])[0]
    defs['line'] = 0
    defs['noteflag'] = 0
    defs['versflag'] = 0
    defs['file'] = file
    #this is the char on the line
    defs['char'] = 0
    #here we count the total chars for the whole file
    defs['charcnt'] = 0
    defs['para'] = 0
    s=[]
    #prefill the pre array    
    pre =  [a for a in ch_re.split(u"　" * left) if len(a) > 0]
    ##the stack for characters.
    chars = []
    ##the stack for inline notes.
    ix = 0

    def addtostack(lx):
        global pcnt
        global notes
        lx = lx.replace(' ', '')
        if len(lx[:-1]) < 1 and pcnt > 1:
            defs['para'] += 1
            pcnt = 0
        elif lx.startswith(u"　　"):
            defs['para'] += 1
            pcnt = 0
        lx = re.sub(u'[~#\u00f1-\u2fff\u3000-\u30FF\uFF00-\uFFEF]', '', lx)
        ##remove the footnote markers in the hist files
        lx = re.sub(u'〔[一二三四五六七八九０]+〕', '', lx)
        lx = re.sub(u'<md[^>]+>', '', lx)
        lx = re.sub(u'<pb[^>]+>', '', lx)
        lx = re.sub(u'@[a-z]+[0-9]?', '', lx)
        for a in ch_re.split(lx[:-1]):
            if len(a) > 0:
                if a == u'¶':
                    defs['line'] +=  1
                    defs['char'] = 0
                else:
                    if a[0] == "(":
                        defs['noteflag'] = 1
                    elif a[0] == ")":
                        defs['noteflag'] = 0
                    else:
                        defs['char'] += 1
                        defs['charcnt'] += 1
                        pcnt += 1
                        if defs['noteflag'] == 1:
                            #if the previous character is a note, add to stack, otherwise print out, empty stack and start afres
                            if len(notes) > 0 and  len(notes[-1]) > 2 and notes[-1][2]+1 < pcnt:
#
                                npre =  [tt for tt in ch_re.split(u"　" * (left - 1)) if len(tt) > 0]
                                npre.append(u"（")
                                notes.append(u"）")
                                notes.extend([tt for tt in ch_re.split(u"　" * (right -1)) if len(tt) > 0])
                                for i in range(1 , len(notes) - right + 1):
                                    s.append((u"%s,%s\t%s\tn"%("".join([tt[0] for tt in notes[0:length+right]]), "".join(npre),  notes[0][1])))
                                    npre.append(notes.pop(0)[0])
                                    npre.pop(0)
                                #procnotes
                                notes = [(a, templ % (defs['txtfile'], defs['page'],
                                                      defs['line'], defs['char'], defs['para'], defs['charcnt']), pcnt)]
                            else:
                                notes.append((a, templ % (defs['txtfile'], defs['page'],
                                            defs['line'], defs['char'], defs['para'], defs['charcnt']), pcnt))
                        else:
                            try:
                                chars.append((a, templ % (defs['txtfile'], defs['page'], defs['line'], defs['char'], defs['para'], defs['charcnt']), pcnt))
                            except:
                                print "error:", defs
                                sys.exit()
        ## need to disentangle the text now, extract notes into a separate stack if there are some
        ## notes start by '(', end by ')' and have a possible linebreak '/' within.
        ## we will collect the notes until we reach a note-end and then spitting them out
        ## [2010-01-07T13:14:14+0900]
        ## this needs to be done before the text is output!
        ## [2010-02-06T17:27:20+0900] FIXME : ZHSJ texts (hist) have the notes where the section is 'b'
        ## [2010-02-18T20:35:34+0900] and now we want to add the same for verse..
        ## [2013-08-22T18:36:33+0900] a problem with this seems to be that it mangles complex gaiji expressions
        ## like [邱-丘+(夸-ㄅ+(万-一))] or maybe the code below handles this?
        ## [2014-10-24T10:55:24+0900] we are missing lines with more than one "(" here
#        if (lx.find('(') > 0):
            
        #if (lx.find('(') > 0):
        #    for i in range(0, len(chars)-1):
        #        #look for the note
        #        if chars[i][0] == '(':
        #            #throw it away!
        #            chars.pop(i)
        #            defs['noteflag']=1
        #            ##make sure the stack is empty
        #            notes=[]
        #            ix = i
        #            break
    

    def setPage(lx):
        r=lx[lx.find(':')+1:lx.find('>')].split('_')
        defs['ed']=r[1]
        defs['id']=r[0]
        #[2014-10-13T17:09:51+0900]
        # we only take the rear part of the page, the leading part is now part of txtfile
        defs['page']=r[-1].split('-')[-1]
        # if defs['line'] == 0:
        #     ##this means we are looking at the first page?!
        #     try:
        #         idlog.write("%(id)s\t%(file)s\t%(title)s\t%(ed)s\t%(page)s\t"%(defs))
        #     except:
        #         idlog.write("%(id)s\t%(file)s\t%(ed)s\t%(page)s\t"%(defs))

        defs['line']=0
        
    idlog = codecs.open(idlogfile, 'a', 'utf-8')
    # lets see if we can open the file
    try:
        f=codecs.open(file, 'r', 'utf-8')
    except:
        idlog.close()
        return s
    for line in f:
        if line.startswith(u'校勘記¶'):
            break
        if line.startswith('No'):
            continue
        line = img_re.sub('', line)
        ##[2018-02-26T20:41:10+0900]
        ## need to get rid of commas!
        line = line.replace(",", "")
        line = re.sub(r'/', '', line)
        if "\t" in line:
            line=line.split('\t')[0] + "\n"
        if line.startswith('#') or line.startswith(':'):
            if line.startswith('#+'):
                r=line[2:-1].split()
            else:
                r=line[1:-1].split()
            if len(r) < 1:
                continue
            if r[0].startswith('PROPERTY'):
                try:
                    defs[r[1].lower()] = r[2]
                    if r[1].startswith('LASTPB'):
                        setPage(r[2])
                except:
                    pass
            else:
                try:
                    defs[r[0][:-1].lower()]=r[-1]
                except:
                    pass
        elif line.startswith('*'):
            pass
        elif line.startswith('<fw'):
            # if there is a lb-marker at the end of the pb, we want to take it into account        
            defs['line'] += line.count(u'¶')
        elif line.startswith('#+BEGIN_VERSE'):
            #verse also resets context?!
            defs['para'] += 1
            defs['versflag']=1
        elif line.startswith('#+END_VERSE'):
            defs['versflag']=0
        elif "<md" in line:
            # use md only if md present!
            usepb = False
            l1, l2 = line.split("<md", 1)
            if not defs.has_key('page'):
                setPage(l2)
                addtostack(l1)
            else:
                addtostack(l1)
                setPage(l2)
                
#            defs['line'] += l2.count(u'¶')
            defs['char'] = 0
            addtostack(l2[l2.find('>')+1:])
            
        elif line.startswith('<pb') and usepb:
            ##this assumes a page format of
            ##<pb:SJB_SDZ0001_010002a>
            setPage(line)
            defs['line'] += line.count(u'¶')
            defs['char'] = 0
        elif defs.has_key('page'):
            ## here we go!
            addtostack(line)
            while (len(chars) > ix + right+length and defs['noteflag'] == 0):
                # for the moment, we keep prose and verse in the same loop and only add a flag to verse outpu
                if chars[0][0] == '(':
                   #throw it away!
                   chars.pop(0)
                   defs['noteflag'] = 1
                   #print "changed noteflag to 1"
                   ##make sure the stack is empty
                   ix = 0
#                   notes=[]
                   break
                if defs['versflag'] == 1:
                    extra = "\tv"
                else:
                    extra = ""
                s.append((u"%s,%s\t%s%s"%("".join([a[0] for a in chars[0:length+right]]), "".join(pre), chars[0][1], extra)))
                #PrintToIdxfile(outdir,
                #u"%s,%s\t%s%s\n"%("".join([a[0] for a in chars[0:length+right]]), "".join(pre), chars[0][1], extra), collection)
                pre.append(chars.pop(0)[0])
                pre.pop(0)
    #todo: check for notes, s.append if necessary
    if len(notes) > 0:
        npre =  [tt for tt in ch_re.split(u"　" * (left - 1)) if len(tt) > 0]
        npre.append(u"（")
        notes.append(u"）")
        notes.extend([tt for tt in ch_re.split(u"　" * (right -1)) if len(tt) > 0])
        for i in range(1 , len(notes) - right + 1):
            s.append((u"%s,%s\t%s\tn"%("".join([tt[0] for tt in notes[0:length+right]]), "".join(npre),  notes[0][1])))
            npre.append(notes.pop(0)[0])
            npre.pop(0)
        notes=[]
    # we are now finished reading the file, need to process the rest
    idlog.close()
    f.close()
    for i in range(right+length, 0, -1):
        #we need to check and make sure that we have a page
        try:
            test = defs['page']
        except:
            
            pass
        if len(chars) < length:
            l=len(chars)
        else:
            l=length
        if l > 0:
            try:
                s.append((u"%s,%s\t%s"%("".join([a[0] for a in chars[0:l]]), "".join(pre), chars[0][1])))
                #PrintToIdxfile(outdir, 
                #u"%s,%s\t%s\n"%("".join([a[0] for a in chars[0:l]]), "".join(pre), chars[0][1]), collection)
                pre.append(chars.pop(0)[0])
                pre.pop(0)
            except:
                print "some error occurred: %s %s"%(line, defs), l, length
    return s

#produce a complete index for one set of branches, return it
def mdIndexGit(txtdir, repo, branches, left, right, length):
    # branches is a dictionary with branchname and sha value or HEAD
    #TODO: what if no master exists?
    alls=[]
    try:
        repo.git.checkout(branches['master'])
    except:
        pass
    for f in os.listdir(txtdir):
        if debug:
            print f
        if f.endswith('txt'):
            #extra 
            varx = []
            m=SequenceMatcher()
            s = MandokuIndex("%s/%s" % (txtdir, f), idlogfile='logfile.log', left=left, right=right, length=length)
            m.set_seq1([a.split('\t')[0] for a in s])
            #now we add the other versions
            k = [a for a in branches.keys() if a == a.upper()]
            k.sort()
            for b in k:
                nx = []
                if b != 'master':
                    #TODO: if working dir is dirty: stash the stuff and restore later
                    repo.git.checkout(branches[b])
                else:
                    continue
                if debug:
                    print "now on branch: ", f,  b.decode('utf-8'), branches[b]
                b=b.decode('utf-8')
                x = MandokuIndex("%s/%s" % (txtdir, f), idlogfile='logfile.log', left=left, right=right, length=length)
                m.set_seq2([a.split('\t')[0] for a in x])
                for tag, i1, i2, j1, j2 in m.get_opcodes():
                    if tag == "replace":
                        for i in range(j1, j2):
                            nx.append(x[i])
#                    print tag, i1, i2, j1, j2
                nxs = SequenceMatcher()
                nxs.set_seqs([a.split('\t')[0] for a in varx], [a.split('\t')[0] for a in nx])
                for tag, i1, i2, j1, j2 in nxs.get_opcodes():
#                    print "merge: ", tag, i1, i2, j1, j2
                    if tag in ("insert", "replace"):
                        for i in range(j1, j2):
#                            print "".join(nx[i]), b
                            varx.append("%s\t%s" % (nx[i], b))
                    elif tag in ("equal"):
                        #we have seen this before, append it to the line in question; do not insert a new line
                        dx = j1 - i1
                        for i in range(i1, i2):
                            varx[i] += " " + b
            s.extend(varx)
            alls.append(s)
            #                print "varx: ", " - ".join(varx)
    try:
        repo.git.checkout(branches['master'])
    except:
        pass
    if debug:
        print len(alls)
    return alls
    

def StartIndex(txtdir, idxdir="/tmp/index", left=3, right=3, length=7, backend="files"):
    global idx
    global gbackend
    gbackend = backend
    bulkcnt = 0
    bulk = []
    bulk_size = 5000
    if backend == "elastic":
        pw = os.environ["ELASTICPW"]
        from elasticsearch import Elasticsearch
        es = Elasticsearch(hosts = ["http://elastic:%s@localhost:9200/" % (pw)])
        INDEX_NAME = 'krp'
        metadata = {'index' : { '_index' : INDEX_NAME, '_type' : 'idx' }}
    elif backend == "rethink":
        pw = os.environ["RETHINKPW"]
        indexdb="krpindex"
        indextb="idx"
        import rethinkdb as r
        #conn=r.connect("localhost", password=pw)
        conn=r.connect("localhost", port=28020)
        if not indexdb in r.db_list().run(conn):
            r.db_create(indexdb).run(conn)
            r.db(indexdb).table_create(indextb).run(conn)
            #ngram index for n=1, n=2 and n=3
            #r.db(indexdb).table(indextb).index_create("ngram", [r.row["text"].slice(0,1),r.row["text"].slice(0,2),r.row["text"].slice(0,3)], multi=True).run(conn) 

        elif teardown:
            r.db(indexdb).table_drop(indextb).run(conn)
            r.db(indexdb).table_create(indextb).run(conn)
            r.db(indexdb).table(indextb).index_create("ngram", [r.row["text"].slice(0,1),r.row["text"].slice(0,2),r.row["text"].slice(0,3)], multi=True).run(conn) 
        rx=r.db(indexdb).table(indextb)
    old = {}
    now = {}
    oldindex = []
    repo = git.Repo(txtdir)
    txtid = os.path.split(txtdir)[-1]
    coll = txtid[0:4]
    #check if a previous run exists:
    for b in [a for a in repo.branches if a.name == a.name.upper() or a.name == "master"]:
        now[b.name] = b.commit.hexsha
    if backend == "files":
        lg = '%s/meta/%s/%s.log' % (idxdir, coll, txtid)
        # if we have a log file, this is an update
        update = os.path.isfile(lg)
    else:
        lg = ""
        update = False
    if debug:
        print lg, update
    changed = True
    if update:
        #        changed = False
        lgf = codecs.open(lg, 'r', 'utf-8')
        for l in lgf:
            if l.startswith('para:'):
                if debug:
                    print "INFO: overwriting parameters from old run: %s" % (l[5:-1])  
                idxdir, left, right, length = eval(l[5:-1])
            elif l.startswith('#'):
                continue
            else:
                branch, version = l[:-1].split('\t', 1)
                old[branch] = version
                if debug:
                    print "Branch check: ", branch, version, now[branch], now[branch] != version
                try:
                    #if a branch exists and has a different version, we need to remake all
                    changed = now[branch] != version
                except:
                    #if we have a new version, we need to do it anyway
                    changed = True
        #if we have a different number of versions, proceed
        lgf.close()
        if not changed:
            changed = len(old) != len(now)
        if changed:
            if debug:
                print "INFO: Something changed, re-indexing."
            oldindex = mdIndexGit(txtdir, repo, old, left, right, length)
    else:
        if idxdir:
            try:
                os.makedirs("%s/meta/%s" % (idxdir, coll))
            except:
                pass
    # now we write the new logfile
    if backend == "files":
        rec = codecs.open(lg, 'w', 'utf-8')
        if changed:
            rec.write(u"# updating index at: %s\n" % (datetime.datetime.now()))
        else:
            rec.write(u"# creating index at %s\n" % (datetime.datetime.now()))
        rec.write(u"para: '%s', %d, %d, %d\n" % (idxdir, left, right, length))
        for b in [a for a in repo.branches if a.name == a.name.upper() or a.name=="master"]:
            rec.write(u"%s\t%s\n" % (b.name.decode('utf-8'), b.commit.hexsha))
            # check for identical hashes:
        rec.close()
    if  len(now) > 0 and changed:
        index = mdIndexGit(txtdir, repo, now, left, right, length)
        if not update:
            if debug:
                dpath=idxdir + "/debug/" + coll
                try:
                    os.makedirs(dpath)
                except:
                    pass
                debfile = codecs.open(dpath + "/" + txtid + ".idx",  "w", "utf-8")
            for ixx in index:
                for i in ixx:
                    if debug:
                        debfile.write("%s\n" % ( i))
                    PrintToIdxfile(idxdir, i, txtid[0:8])
            if debug:
                print "writing index %d keys." % (len(idx))
            for of in idx.keys():                
                if backend == "files":
                    outfile=codecs.open(of, 'a+', 'utf-8')
                    outfile.write("".join(idx[of]))
                    outfile.close()
                elif backend == "elastic" or backend == "rethink":
                    #ch=unichr(int(os.path.split(of)[-1].split(".")[0], 16))
                    #es_out = [u"%s%s" % (ch, a.strip("\n")) for a in idx[of].split("\n")]
                    for mx in idx[of]:
                        if backend == "elastic":
                            bulk.append(metadata)
                        bulk.append(mx)
                        bulkcnt +=1
                    if bulkcnt > bulk_size:
                        if backend == "elastic":
                            resb = es.bulk(index = INDEX_NAME,body = bulk, refresh = True)
                        else:
                            resb = dict(rx.insert(bulk).run(conn))
                            try:
                                print resb['inserted']
                            except:
                                print resb
                        bulk = []
                        bulkcnt = 0
                else:
                    print "No valid backend found, exiting.  Valid backends are:\nfiles\tfile based index_stage\nelastic\telasticsearch index\nrethink\trethinkdb index\n"
                    sys.exit()
            idx={}
        else:
            if len(oldindex) > 0:
                t = SequenceMatcher()
                for x in range(0, len(index) - 1):
                    #this assumes the same number of files...
                    t.set_seqs(index[x], oldindex[x])
                    for tag, i1, i2, j1, j2 in t.get_opcodes():
                        print "update: ", tag, i1, i2, j1, j2
                        if tag == "replace":
                            pass
                            ##write out procedure to replace or insert
                        elif tag == "insert":
                            pass
                            ##write out procedure to replace or insert
                        elif tag == "delete":
                            pass
                             # for i in range(i1, i2):
                             #     print index[i]
        try:
            debfile.close()
        except:
            pass
    else:
        return "INFO: Nothing to do, no new commits in repository."

    
    ## do sth with the index... : either write out new or write patch
#    rec.write("hash: \n")
    repo.git.checkout('master')
    



    
if __name__ == '__main__':
    debug=True
    try:
        idxdir=sys.argv[3]
    except:
        idxdir='/Users/chris/tmp/index'
    idlog = 'index.log'
    try:
        os.makedirs(idxdir)
    except:
        pass
    if debug:
        try:
            os.makedirs(idxdir + "/debug")
        except:
            pass
        
    StartIndex(sys.argv[1], idxdir, 3, 3, 7)
#    MandokuIndex(sys.argv[1], idxdir, idlog, 3, 3, 7, collection, use_vol)
#    MandokuIndex(sys.argv[1], idxdir, idlog, 2, 2, 5, collection, use_vol)
