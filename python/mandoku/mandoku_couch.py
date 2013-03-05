#!/bin/env python -*- coding: utf-8 -*-
# add files in org (mandoku) format to the redis server

# adapting this from the mandoku_idx file
from __future__ import division
from __future__ import absolute_import

import os, sys, codecs, re, datetime, git

from mandoku.mandoku import *
from couchdb import Server
from couchdb.mapping import TextField
#from couchdb import Document , IntegerField, DateField
#from collections import OrderedDict

from difflib import *

def getsigle(branch, db):
    "the sigle is a general, not text dependend mapping from a shorthand version to the identifier used in git; db is the db where the sigles document is stored"
    if db.get('branch'):
        bdoc = db['branch']
    else:
        bdoc={'_id': 'branch', 'sigle': 'dummy', 'type' : 'meta' }
    t = branch.replace(u'【', '')
    t = t.replace(u'】', '')
    if bdoc.has_key(t):
        return bdoc[t]
    else:
        #this means, the branch we are seeing is new, register it
        i = 1
        s1 = t[0:i]
        if db.get('sigle'):
            sdoc = db['sigle']
        else:
            sdoc={'_id': 'sigle', 'branch': 'dummy', 'type' : 'meta' }
        while sdoc.has_key(s1) and i < len(t):
            i += 1
            s1 = t[0:i]
        sdoc[s1] = branch
        db.save(sdoc)
        bdoc[branch]=s1
        db.save(bdoc)
        return s1



class CouchMandoku(MandokuText):
    id = TextField()
    def __init__(self, db, ngram=None, txtid=None, fac=100000,  *args, **kwargs):
        """db ist the database for text data on the server, meta the metadata database (for internal stuff)"""
        self.meta=db
        self.db=db
        self.ngram=ngram
        self.fac = fac
        self.branches={}
        if txtid:
            ## connect to couchdb, get the required data there...
            self.txtid = txtid
        else:
            super(CouchMandoku, self).__init__(*args, **kwargs)
            self.read()
            self.add_metadata()
            #            try:
            self.maketoc()
            # except:
            #     print "maketoc failed."
            try:
                self.txtid = self.defs['id']
            except:
                self.txtid = self.textpath.split('/')[-1].split('.')[0]

                
    def connectText(self):
        """Connect to the DB and get the text, or create a DB document for the text and store it there.
        We assume that the text has been read already with self.read'() and that we have the metadata.
        """
        t = self.db.get(self.txtid)
        if not(t):
            ##new text, so we need to save this to db
            t = {}
            self.defs['date']= datetime.datetime.now()
            sigle = getsigle(self.version, self.meta)
            t['_id'] = self.txtid
            t['type'] = 'base'
            t['baseversion'] = self.version
            try:
                t['title'] = self.defs['title'].split()[-1]
            except:
                t['title'] = self.textpath.split('/')[-1]
            t['textpath'] = self.textpath[self.textpath.find('/db/')+4:]
            t['sigle-%s' % (sigle)] = self.revision
            t['fac'] = self.fac
            t['toc'] = self.toc
            t['coll'] = self.coll
#            t['pages'] = {}
            t['versions'] =self.versions
#            t['pages'] = self.pages
            t['sections'] = []
            for i in range(1, len(self.sections)+1):
                s, f = self.sections[i-1]
                secid=f[0:f.find('.')]
                try:
                    cnt = self.sections[i][0] 
                except(IndexError):
                    cnt = len(self.seq)
#                self.sectocs[secid] = self.makesectoc(s, cnt)
                #lets put something useful here
                t['sections'].append([secid, cnt])
                d = {'type' : 'seq',  
                     'version' : self.version, 
                     'rev' : self.revision, 
                     'sigle' : sigle, 
                     '_id' : f[0:f.find('.')]}
                d['toc'] = self.sectocs[secid]
                d['seq'] = self.seq[s:cnt]
                try:
                    d['juan'] = int(secid.split('-')[-1])
                except:
                    try:
                        d['juan'] = int(secid.split('_')[-1])
                    except:
                        d['juan'] = 0
                d['pages'] = {}
                d['lines'] = {}
                l=0
                if self.defs.has_key('lastpb'):
                    #this makes no sense, lastpb will be the one of the last file
                    #TODO: look for the previous pg in seq!?
                    pg = self.defs['lastpb'][self.defs['lastpb'].find('<'):self.defs['lastpb'].find('>')+1]
                    d['pages'][0] = pg
                    d['lines'][pg] = {'start' : 0}
                for j in range(0, len(d['seq'])):
                    this = "".join(d['seq'][j][self.mpos:])
                    m=re.findall(ur"(<pb:[^>]*>)", this)
                    if len(m) > 0:
                        try:
                            d['lines'][pg]['end'] = j
                        except:
                            pass
                        for pg in m:
                            d['pages'][j] = pg
                            d['lines'][pg] = {'start': j}
                        l = 0
                    #this includes the lbs before the new page!, so get only the part after >
                    this = this.rsplit('>', 1)[-1]
                    x = len(re.findall(u"\xb6", this))
                    if x > 0:
                        l += x
                        try:
                            d['lines'][pg][j] = l
                        except:
                            pass
                #this is the last entry
                try:
                    d['lines'][pg]['end'] = j
                except:
                    pass
                self.db.save(d)
            self.db.save(t)

    def printNgram(self, sqx, sec, pos, extra=None):
        #this is what we overwrite to get the stuff into redis
        if self.ngram:
            print sqx
            sx = "".join(sqx)
            if extra:
                val = (sec, pos, extra)
            else:
                val = (sec, pos)
            if self.ngram.get(sx):
                ng = self.ngram.get(sx)
                ng['ngram'].append(val)
                self.ngram.save(ng)
            else:
                self.ngram.save({'_id' : sx, 'ngram' : [val]})



    def add_metadata(self):
        "this has moved to connecText() for every section"
        pass

    def addOtherBranches(self, add_var_punctuation=False):
        """adds the other branches to couchdb"""
        try:
            repo = git.Repo(self.textpath)
        except:
            return "No git repository found"
        s = SequenceMatcher()
        self.s=s
        self.refs=[]
#        s.set_seq1([a[self.cpos] for a in self.seq])
        for b in repo.heads:
            if b.name != self.version:
                b.checkout()
                self.branches[b.name]={}
                res = self.branches[b.name]
                sig = getsigle(b.name, self.meta)
                t2 = MandokuText(self.textpath, version=b.name)
                self.refs.append(t2)
                t2.read()
                evensec = len(t2.sections) == len(self.sections)
                if evensec:
                    for i in range(0, len(self.sections)):
#                        print self.sections[i]
                        s1start, f =self.sections[i]
                        secid = f[0:f.find('.')]
                        try:
                            s1end = self.sections[i+1][0]
                        except IndexError:
                            s1end = len(self.seq)
                        s2start=t2.sections[i][0]
                        try:
                            s2end = t2.sections[i+1][0]
                        except IndexError:
                            s2end = len(t2.seq)
                        s = SequenceMatcher()
                        s.set_seq1([a[self.cpos] for a in self.seq[s1start:s1end]])
                        s.set_seq2([a[self.cpos] for a in t2.seq[s2start:s2end]])
                        self.procdiffs(s, t2, s1start, s2start, add_var_punctuation, sig, secid)
                else:
                    ##untested!!
                    s.set_seq2([a[self.cpos] for a in t2.seq])
                    self.procdiffs(s, t2, 1, 1, add_var_punctuation, sig, self.txtid)
        for b in repo.heads:
            if b.name == self.version:
                b.checkout()

    def procdiffs (self, s, t2, s1start, s2start, add_var_punctuation, sig, secid):
        unevensec = len(self.sections) != len(t2.sections)
        d=0
        oldseg = 0
        res = {}
        for tag, i1, i2, j1, j2 in s.get_opcodes():
            ##need to find out which seg we are in
            # if unevensec:
            #     seg = self.pos2seg(i1) - 1
            #     if (seg != oldseg):
            #          dummy, f = self.sections[oldseg]
            #          t = self.db.get(f[0:f.find('.')])
            #          if not(t.has_key('variants')):
            #              t['variants'] = {}
            #          t['variants'][sig] = res
            #          self.db.save(t)
            #          res = {}
            #          oldseg = seg
            ##todo: need to update the position, so that it is based on the section, not total charpos
            if  tag == 'equal':
                dx = j1 - i1
                for i in range(i1, i2):
                    if '<pb:' in t2.seq[i+dx][self.mpos]:
                        pb = t2.seq[i+dx][self.mpos]
                        if self.img.has_key(i):
                            self.img[i][sig] = pb[pb.find('<pb:'):pb.find('>', pb.find('<pb:'))+1]
                        else:
                            self.img[i] = {}
                            self.img[i][sig] = pb[pb.find('<pb:'):pb.find('>', pb.find('<pb:'))+1]
                    if add_var_punctuation and t2.seq[s2start+i+dx][self.mpos] != '':
                        res[i+d] = ':' + t2.seq[s2start+i+dx][self.mpos]
            if tag == 'replace':
                a1=self.seq[s1start+i1:s1start+i2]
                b1=[x[self.mpos] for x in t2.seq[s2start+j1:s2start+j2]]
                a=[x[self.cpos] for x in t2.seq[s2start+j1:s2start+j2]]
                if add_var_punctuation:
                    ##untested!!
                    a1=map(lambda xx : xx[self.cpos] + ':' + xx[self.mpos], zip(a,b1))
                a.reverse()
                b1.reverse()
                for i in range(i1, i2):
                    try:
                        res[i+d] = a.pop()
                        pb = b.pop()
                        if '<pb:' in pb:
                            self.img[i+d][sig] = pb[pb.find('<pb:'):pb.find('>', pb.find('<pb:'))+1]
                    except:
                        #b is shorter than a
                        res[i+d] = ''
                if len(a) > 0:
                    #b is longer than a
                    a.reverse()
                    res[i+d] = "%s%s" % (res[i], "".join(["".join(tmp) for tmp in a]))
            elif tag == 'insert':
                k = i1-1+d
                if add_var_punctuation:
                    #here we just grap the original e, munge it together and slab it onto the rest
                    res[k] =  "%s%s%s" % (res.get(k, ''), "".join([x[self.cpos] for x in self.seq[s1start+i1-1:s1start+i1]]), "".join("".join(["".join(x[self.cpos]) for x in t2.seq[s2start+j1:s2start+j2]])))
                else:
                    res[k] =  "%s%s%s" % (res.get(k, ''), "".join([x[self.cpos] for x in self.seq[s1start+i1-1:s1start+i1]]), "".join("".join(["".join(x[self.cpos]) for x in t2.seq[s2start+j1:s2start+j2]])))
                if '<pb:' in res[k]:
                    pb = res[k]
                    self.img[i+d][sig] = pb[pb.find('<pb:'):pb.find('>', pb.find('<pb:'))+1]
            elif tag == 'delete':
                res[i1+d] = ""

        t = self.db.get(secid)
        if t is None:
            print secid, b.name
        else:
            if not(t.has_key('variants')):
                t['variants'] = {}
            t['variants'][sig] = res
            t['img'] = self.img
            self.db.save(t)


    def pos2seg(self, pos):
        #give the section of a given pos
        s=[a[self.cpos] for a in self.sections]
#        s.reverse()
        cnt=len(s)
        x = s.pop()
        while(x > pos):
            cnt -= 1
            try:
                x=s.pop()
            except:
                break
                print pos, x, s
        return cnt
    def pos2facpos(self, pos):
        seg = self.pos2seg(pos) 
        return seg * self.fac + pos - self.sections[seg-1][0]
    def facpos2pos(self, facpos):
        seg = facpos / self.fac
        return facpos - seg * self.fac + self.sections[seg-1][0]
