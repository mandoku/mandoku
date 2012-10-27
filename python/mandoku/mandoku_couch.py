#!/bin/env python -*- coding: utf-8 -*-
# add files in org (mandoku) format to the redis server

# adapting this from the mandoku_idx file

import os, sys, codecs, re, datetime, git

from mandoku import *
from couchdb import Server
from couchdb.mapping import TextField, IntegerField, DateField
from couchdb import Document

from difflib import *

def getsigle(branch, db):
    "the sigle is a general, not text dependend mapping from a shorthand version to the identifier used in git; db is the db where the sigles document is stored"
    bdoc = db['branch']
    t = branch.replace(u'【', '')
    t = t.replace(u'】', '')
    if bdoc.has_key(t):
        return bdoc[t]
    else:
        #this means, the branch we are seeing is new, register it
        i = 1
        s1 = t[0:i]
        sdoc = db['sigle']
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
    def __init__(self, db, meta, txtid=None, fac=100000,  *args, **kwargs):
        """db ist the database for text data on the server, meta the metadata database (for internal stuff)"""
        self.meta=meta
        self.db=db
        self.fac = fac
        self.branches={}
        if txtid:
            ## connect to couchdb, get the required data there...
            self.txtid = txtid
        else:
            super(CouchMandoku, self).__init__(*args, **kwargs)
            self.read()
#            self.add_metadata()
            try:
                self.txtid = self.defs['id']
            except:
                self.txtid = self.textpath.split('/')[-1].split('.')[0]

                
    def connectText(self):
        t = self.db.get(self.txtid)
        if not(t):
            ##new text, so we need to save this to db
            t = {}
            self.defs['date']= datetime.datetime.now()
            sigle = getsigle(self.version, self.meta)
            t['_id'] = self.txtid
            t['type'] = 'base'
            t['baseversion'] = self.version
            t['title'] = self.defs['title'].split()[-1]
            t['textpath'] = self.textpath[self.textpath.find('/db/')+4:]
            t['sigle-%s' % (sigle)] = self.revision
#            t['fac'] = self.fac
#            t['pages'] = {}
            t['versions'] = {}
#            t['pages'] = self.pages
            t['sections'] = self.sections
            for i in range(1, len(self.sections)+1):
                s, f = self.sections[i-1]
                try:
                    cnt = self.sections[i][0] 
                except(IndexError):
                    cnt = len(self.seq) 
                d = {'type' : 'seq',  
                     'version' : self.version, 
                     'rev' : self.revision, 
                     'sigle' : sigle, 
                     '_id' : f[0:f.find('.')]}
                d['seq'] = self.seq[s:cnt]
                d['juan'] = int(f[0:f.find('.')].split('-')[-1])
                d['pages'] = {}
                d['lines'] = {}
                l=0
                if self.defs.has_key('lastpb'):
                    #this makes no sense, lastpb will be the one of the last file
                    #TODO: look for the previous pg in seq!?
                    pg = self.defs['lastpb'][self.defs['lastpb'].find('<'):self.defs['lastpb'].find('>')+1]
                    d['pages'][0] = pg
                    d['lines'][pg] = {}
                for i in range(0, len(d['seq'])):
                    m=re.search(ur"(<pb:[^>]*>)", d['seq'][i][1])
                    if m:
                        pg = m.groups()[0]
                        d['pages'][i] = pg
                        d['lines'][pg] = {}
                        l = 0
                    x = len(re.findall(u"\xb6", d['seq'][i][1]))
                    if x > 0:
                        l += x
                        try:
                            d['lines'][pg][i] = l
                        except:
                            pass
                self.db.save(d)
            self.db.save(t)


    def add_metadata(self):
        "this has moved to connecText() for every section"
        pass

    def addOtherBranches(self, add_var_punctuation=False):
        """adds the other branches to redis"""
        try:
            repo = git.Repo(self.textpath)
        except:
            return "No git repository found"
        s = SequenceMatcher()
        self.s=s
        self.refs=[]
        s.set_seq1([a[0] for a in self.seq])
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
                        s.set_seq1([a[0] for a in self.seq[s1start:s1end]])
                        s.set_seq2([a[0] for a in t2.seq[s2start:s2end]])
                        res = self.procdiffs(s, t2, s1start, s2start, add_var_punctuation)
#                        print "res:", res
                        t = self.db.get(f[0:f.find('.')])
                        if not(t.has_key('variants')):
                            t['variants'] = {}
                        t['variants'][sig] = res
                        self.db.save(t)
                else:
                    s.set_seq2([a[0] for a in t2.seq])
                    res = self.procdiffs(s, t2, 1, 1, add_var_punctuation)
#                    print "res:", res
#                    try:
                    dummy, f = self.sections[seg]
                    t = self.db.get(f[0:f.find('.')])
                    if not(t.has_key('variants')):
                        t['variants'] = {}
                    t['variants'][sig] = res
                    self.db.save(t)
#                    except:
#                        pass
        for b in repo.heads:
            if b.name == self.version:
                b.checkout()

    def procdiffs (self, s, t2, s1start, s2start, add_var_punctuation):
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
            if add_var_punctuation and tag == 'equal':
                dx = j1 - i1
                for i in range(i1, i2):
                    if t2.seq[s2start+i+dx][1] != '':
                        res[i+d] = ':' + t2.seq[s2start+i+dx][1]
            if tag == 'replace':
                a1=self.seq[s1start+i1:s1start+i2]
                a=[x[0] for x in t2.seq[s2start+j1:s2start+j2]]
                if add_var_punctuation:
                    b1=[x[1] for x in t2.seq[s2start+j1:s2start+j2]]
                    a1=map(lambda xx : xx[0] + ':' + xx[1], zip(a,b1))
                a.reverse()
                for i in range(i1, i2):
                    try:
                        res[i+d] = a.pop()
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
                    res[k] =  "%s%s%s" % (res.get(k, ''), "".join([x[0] for x in self.seq[s1start+i1-1:s1start+i1]]), "".join("".join(["".join(x[0]) for x in t2.seq[s2start+j1:s2start+j2]])))
                else:
                    res[k] =  "%s%s%s" % (res.get(k, ''), "".join([x[0] for x in self.seq[s1start+i1-1:s1start+i1]]), "".join("".join(["".join(x[0]) for x in t2.seq[s2start+j1:s2start+j2]])))
            elif tag == 'delete':
                res[i1+d] = ""
        return res

    def pos2seg(self, pos):
        #give the section of a given pos
        s=[a[0] for a in self.sections]
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
