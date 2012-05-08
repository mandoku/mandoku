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
    m = db['branch']
    if m.has_key(branch):
        return s
    else:
        #this means, the branch we are seeing is new, register it
        t = branch.replace(u'【', '')
        i = 1
        s1 = t[0:i]
        while r.hget('sigle', s1):
            s1 = t[o:i]
            i += 1
        r.hset('sigle', s1, branch)
        r.hset('branch', branch, s1)
        return s1



class CouchMandoku(MandokuText):
    id = TextField()
    
    def __init__(self, db, txtid=None, fac=100000, *args, **kwargs):
        self.db=db
        self.fac = fac
        self.branches={}
        if txtid:
            ## connect to redis, get the required data there...
            self.txtid = txtid
        else:
            super(CouchMandoku, self).__init__(*args, **kwargs)
            self.read()
            self.add_metadata()
            try:
                self.txtid = self.defs['id']
            except:
                self.txtid = textpath.split('/')[-1].split('.')[0]
                
    def connectText(self):
        try:
            t = self.db[self.txtid]
        except couchdb.http.ResourceNotFound:
            t = ""
        if len(t) < 1:
            ##new text
            self.defs['date']= datetime.datetime.now()
            sigle = getsigle(self.version, self.db)
            t['baseversion'] = self.version
            t['title'] = self.defs['title']
            t['textpath'] = self.textpath
            t['sigle-%s' % (sigle)] = self.revision
            t['fac'] = self.fac
            t['pages'] = {}
            t['versions'] = {}

            for k in self.pages.keys():
                t['pages'][k] = self.pages[k]
            for i in range(1, len(self.sections)+1):
                s, f = self.sections[i-1]
                try:
                    cnt = self.sections[i][0] - s
                except(IndexError):
                    cnt = len(self.seq) - s
                t['sec'][i] = { 'version' : self.version, 'rev' : self.revision}


    def add_metadata(self):
        """for the redis version, we store the 'location' value, that is
        the section * fac +position"""
        l=0
        sec=0
        prev = 0
        s=[a[0] for a in self.sections]
        s.reverse()
        limit = s.pop()
        for i in range(0, len(self.seq)):
            if i == limit:
                prev = limit
                try:
                    limit = s.pop()
                except:
                    #for the last one, we dont have a limit...
                    pass
                sec +=1
            x = len(re.findall(u"\xb6", self.seq[i][1]))
            if x > 0:
                l += x
                self.lines[i] = l
            m=re.search(ur"(<pb:[^>]*>)", self.seq[i][1])
            if m:
                pos = self.pos2facpos(i)
                self.pages[pos] = m.groups()[0]
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
                sig = getsigle(b.name, self.r)
                t2 = MandokuText(self.textpath, version=b.name)
                self.refs.append(t2)
                t2.read()
                ##todo: add the necessary metadata to redis
                s.set_seq2([a[0] for a in t2.seq])
                d=0
                for tag, i1, i2, j1, j2 in s.get_opcodes():
                    ##need to find out which seg we are in
                    target="%s:%s:%d"%(self.txtid, sig, self.pos2seg(i1))
                    if add_var_punctuation and tag == 'equal':
                        dx = j1 - i1
                        for i in range(i1, i2):
                            if t2.seq[i+dx][1] != '':
                                res[i+d] = ':' + t2.seq[i+dx][1]
                    if tag == 'replace':
                        a=self.seq[j1:j2]
                        if add_var_punctuation:
                            b1=[x[1] for x in t2.seq[j1:j2]]
                            a=map(lambda xx : xx[0] + ':' + xx[1], zip(a,b1))
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
                            res[k] =  "%s%s%s" % (res.get(k, ''), "".join(self.seq[i1-1:i1][0]), "".join("".join(["".join(a) for a in t2.seq[j1:j2]])))
                        else:
                            res[k] =  "%s%s%s" % (res.get(k, ''), "".join(self.seq[i1-1:i1][0]), "".join("".join(["".join(a) for a in t2.seq[j1:j2]])))
                    elif tag == 'delete':
                        res[i1+d] = ""
                try:
                    self.r.hmset(target, res)
                except:
                    pass


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
