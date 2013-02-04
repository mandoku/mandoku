#!/bin/env python -*- coding: utf-8 -*-
# add files in org (mandoku) format to the redis server

# adapting this from the mandoku_idx file
from __future__ import division
from __future__ import absolute_import

import os, sys, codecs, re, datetime, git

from mandoku import *
import redis
import redis_hash_dict
#import redis_config

from difflib import *

def getsigle(branch, r):
    s = r.hget('branch', branch)
    if s:
        return s
    else:
        t = branch.replace(u'【', '')
        i = 1
        s1 = t[0:i]
        while r.hget('sigle', s1):
            s1 = t[o:i]
            i += 1
        r.hset('sigle', s1, branch)
        r.hset('branch', branch, s1)
        return s1



class RedisMandoku(MandokuText):
    def __init__(self, red, txtid=None, fac=100000, *args, **kwargs):
        self.r=red
        self.fac = fac
        self.branches={}
        if txtid:
            ## connect to redis, get the required data there...
            self.txtid = txtid
        else:
            super(RedisMandoku, self).__init__(*args, **kwargs)
            self.read()
            self.add_metadata()
            try:
                self.txtid = self.defs['id']
            except:
                self.txtid = self.textpath.split('/')[-1].split('.')[0]
                
    def connectText(self):
        t=self.r.hgetall(self.txtid)
        if len(t) < 1:
            ##we are connecting a new text, set the metadata etc.
            self.defs['date']= datetime.datetime.now()
            ## it looks like we will have to set the first added version as baseversion, version == branch
            sigle = getsigle(self.version, self.r)
            self.r.hset('%s' % (self.txtid), 'baseversion', self.version)
            self.r.hset('%s' % (self.txtid), 'title', self.defs['title'])
            self.r.hset('%s' % (self.txtid), 'textpath', self.textpath)
            #entbehrlich?
            self.r.hset('%s' % (self.txtid), 'version-%s' % (self.version), self.revision)
            self.r.hset('%s' % (self.txtid), 'fac' , self.fac)
            p=self.r.pipeline()
            for i in range(1, len(self.sections)+1):
                s, f = self.sections[i-1]
                try:
                    cnt = self.sections[i][0] - s
                except(IndexError):
                    cnt = len(self.seq) - s
                
                target="%s" % (f[0:f.find('.')])
                for a in self.seq[s:s+cnt]:
                    p.rpush(target, "".join(a))
                p.execute()
                for j in range(s, s+cnt):
                    m=re.search(ur"(<pb:[^>]*>)", self.seq[j][1])
                    if m:
                        pg = m.groups()[0]
                        p.zadd("%s-pages" % (target), pg, j-s)
                    #     d['lines'][pg] = {}
                    #     l = 0
                    # x = len(re.findall(u"\xb6", d['seq'][j-s][1]))
                    # if x > 0:
                    #     l += x
                    #     try:
                    #         d['lines'][pg][j-s] = l
                    #     except:
                    #         pass
                p.execute()
        else:
            #TODO need to fill in the case when we in fact connect to the db
            pass
    def printNgram(self, sx, sec, pos, extra=None):
        #this is what we overwrite to get the stuff into redis
        p=self.r.pipeline()
        if extra:
            p.rpush("%s:%s"%(sx, sec), "%d:%s" % (pos, extra) )
        else:
            p.rpush("%s:%s"%(sx, sec), pos )
        p.execute()

    def addNgramR(self, action='add', n=3):
        p=self.r.pipeline()
        for i in range(1, len(self.sections)+1):
            s, f = self.sections[i-1]
            inNote = False
            #dn is a counter that is used to bridge the gap of text
            #that is interrupted by notes (or other interrupting
            #features)
            dn = 0
            try:
                cnt = self.sections[i][0]
            except(IndexError):
                cnt = len(self.seq)
            for j in range(s, cnt):
                #first we check if the status of the text changes:
                m=re.search(ur"[\(\)]", self.seq[j+n][1])
                if m and not inNote:
                    inNote = True
                else:
                    inNote = False
                sx="".join([a[0] for a in self.seq[j:j+n]])
                if j % 100 == 0:
                    p.execute()
                ##ngram:txt, position
                p.rpush("%s:%s"%(sx, f[0:f.find('.')]),  j - s )
                if len(sx) == n:
                    ##ngram, occurrences in txt
                    p.zincrby(sx, f[0:f.find('.')])
                    try:
                        p.zincrby(sx[0:2], sx[2])
                    except:
                        pass
                    try:
                        p.zincrby("%s-next" % (sx[0]), sx[1])
                        p.zincrby("%s-prev" % (sx[1]), sx[2])
                    except:
                        pass
            p.execute()

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
                            try:
                                res[k] =  "%s%s%s" % (res.get(k, ''), "".join(self.seq[i1-1:i1][0]), "".join("".join(["".join(a) for a in t2.seq[j1:j2]])))
                            except(IndexError):
                                print k, i1, j1, j2
                                exit
                    elif tag == 'delete':
                        res[i1+d] = ""
                try:
                    self.r.hmset(target, res)
                except:
                    pass


    def pos2seg(self, pos):
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
