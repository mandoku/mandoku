#! /usr/bin/python
# -*- coding: utf-8 -*-
#
# Copyright (c) 2011 Christian Wittern cwittern at gmail.com 
# All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are
# met:
#
#     * Redistributions of source code must retain the above copyright
#     notice, this list of conditions and the following disclaimer.
#
#     * Redistributions in binary form must reproduce the above
#     copyright notice, this list of conditions and the following
#     disclaimer in the documentation and/or other materials provided
#     with the distribution.
#
#     * Neither the name 'Mandoku' nor the names of the contributors
#     may be used to endorse or promote products derived from this
#     software without specific prior written permission.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
# "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
# LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
# A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
# OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
# SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
# LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
# DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
# THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
# (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
# OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
#
"""\
Mandoku module
library routines for reading and parsing mandoku orgmode files.
:author:       Christian Wittern (cwittern[at]gmail.com)
:organization: Mandoku project (http://www.mandoku.org)
:license:      BSD License
"""

import sys, codecs, os ,re, operator, collections, git, time, datetime
from difflib import *
from itertools import *
from sparsedict import *

kanji=Ur'\u3400-\u4DFF\u4e00-\u9FFF\uF900-\uFAFF\uFE30-\uFE4F\U00020000-\U0002A6DF\U0002A700-\U0002B73F\U0002B740-\U0002B81F\U0002B820-\U0002F7FF'
pua=Ur'\uE000-\uF8FF\U000F0000-\U000FFFFD\U00100000-\U0010FFFD'
##this will recognize image links like [[./img]] as 1 kanji --> clear this out later?!
ent=r'\[\[.*?\]\]|\[[^\]]*\]|&[^;]*;|&amp;C[X3-7]-[A-F0-9]+'
#now
#kp_re = re.compile(u"(%s|[%s%s])" % (ent, kanji, pua))
kp_re = re.compile(u"(%s|{[%s%s]+:[^}]*}|[%s%s])" % (ent, kanji, pua, kanji, pua))
ch_re = re.compile(ur'(\[[^\]]*\]|&[^;]*;|&amp;C[X3-7]-[A-F0-9]+|.)')
img_re = re.compile(ur'<i[^>]*>')
punc_re = re.compile(ur"[\u3001-\u33FF\uFF00-\uFF7F]")
meta_re = re.compile(ur'(<[^>]*>|\xb6|\n)')
sys.stdout = codecs.getwriter('utf8')(sys.stdout)


class MandokuText(object):
    def __init__(self, textpath, version='master', starlines=True, encoding='utf-8', ext='.txt'):
        """Read and parse the text.  If we get a directory, read all files in lexical order as parts of the text."""
        self.in_note = False
        self.flags = {}
        self.sections = []
        #a dictionary of sparsedicts:
        #a note for example is registered as follows: self.markup['note']
        self.markup = collections.defaultdict(SparseDict)
        #this allows me to lookup by character position
        self.pages = SparseDict()
        #indexes the page number to a list of character positions, every position gives the beginning of a line
        self.lines = collections.defaultdict(list)
        #Holds various metadata about the text culled from the header
        self.defs = {}
        #holds the text, in this case the first character is in fact position 1
        #the first is a dummy, so that we can dump the beginning of the file and the first item of 'ex'
        #additional items might be placed in the tuple, so the implementation should *not* rely on there being only two items
        #the position at which the character is to be found in the seq tuple
        self.cpos = 0
        #the position at which metadata (u'\b6', <pb etc) is to be found
        self.mpos = 1
        self.seq = [('', '')]
        self.ext = ext
        self.textpath = textpath
        self.encoding = encoding
        ##version is a string that can be used for a branch in git
        try:
            repo = git.Repo(self.textpath)
            self.version = repo.active_branch.name
            self.date = time.strftime("%Y-%m-%d %H:%M +0000", time.gmtime(repo.active_branch.commit.authored_date))
        except:
            self.version = version
        ## the revision of the text in git
        try:
            self.revision = repo.active_branch.commit.hexsha
        except:
            self.revision = ''
        ##lines that begin a star, if False, we ignore them (for CBETA)
        self.starlines = starlines
        ## we use our own re to make it replaceable
        self.re = kp_re

    def read(self):
        self.seq = [('', '')]
        self.cpos=0
        self.mpos=1
        if os.path.isdir(self.textpath):
            files = os.listdir(self.textpath)
            files.sort()
            for f in files:
                if os.path.isfile(self.textpath + '/' +f) and f.endswith(self.ext):
                    infile = codecs.open(self.textpath + '/' + f, 'r', self.encoding)
                    self.sections.append((len(self.seq), f))
                    self.parse(infile)
                    infile.close()
        elif os.path.isfile(self.textpath):
            infile = codecs.open(self.textpath, 'r', self.encoding)
            self.sections.append((len(self.seq), self.textpath))
            self.parse(infile)
            infile.close()
        else:
            sys.stderr.write("missing:  " + self.textpath + '\n')
            raise "No valid file found"

    def punc_reorder(self):
        """Expand the tuple in seq to at least three items,
        punctuation before the char, the char and punctuation after
        the char, additional stuff like pagebreaks goes into the fourth item."""
        #we do this only if it has not been done before...
        if self.cpos > 0:
            return
        for i in range(len(self.seq)-1, -1, -1):
            ##move opening punctuation to the next chartuple; move metadata other than punctuation to a new item in this tuple
            p = np = m = ''
            s=self.seq[i][self.cpos+1]
            ex=meta_re.split(s)
            for e in ex:
                if len(e) > 0:
                    if e[0] in (u'<', u'¶', u'\n'):
                        m += e
                    else:
                        for j in range(0, len(e)):
                            if e[j] in (u'〈', u'《', u'「', u'『', u'【', u'〖', u'〘', u'〚', u'\u3000', '(', '*', ' '):
                                p += e[j]
#                                print "p", i, p
                            else:
                                np += e[j]
#                                print "np", i, np
            self.seq[i] = ('', self.seq[i][self.cpos], np, m)
            if len(p) > 0:
                try:
                    self.seq[i+1] = (p + self.seq[i+1][0], ) + self.seq[i+1][1:]
                except:
                    print i, p, self.seq[i]
            if self.seq[i][1].startswith('{'):
                ts = self.seq[i][1]
                k = ts[1:ts.find(':')]
                rep = ts[ts.find(':')+1:-1]
                if len(k) > 0:
                    try:
                        self.seq[i] = ('{', k[0], ':' + rep[0] + '}', ) + self.seq[i][2:]
                    except:
                        self.seq[i] = ('{', k[0], ':}', ) + self.seq[i][2:]
                    if len(k) > 1:
                        ##this should be the only case where we extend the seq
                        for x in range(1, len(k)):
                            try:
                                self.seq.insert(i+x, ('{', k[x], ':' + rep[x] + '}', '',))
                            except:
                                self.seq.insert(i+x, ('{', k[x], ':}', '',))
                        if len(rep) > x:
                            self.seq[i+x] = ('{', self.seq[i+x][1], ':' + rep[x:] + '}', '')
                elif len(rep) > 0:
                    self.seq[i] = ('{', '', ':' + rep + '}', ) + self.seq[i][2:]
                    
                
        self.cpos = 1
        self.mpos = 3
    def add_metadata(self):
        l=0
        for i in range(0, len(self.seq)):
            x = len(re.findall(u"\xb6", self.seq[i][self.mpos]))
            if x > 0:
                l += x
                self.lines[i] = l
            m=re.search(ur"(<pb:[^>]*>)", self.seq[i][self.ppos])
            if m:
                self.pages[i] = m.groups()[0]
                
    def parse(self, infile):
        for line in infile:
            try:
                line, extra = line.split('\t', 1)
            except:
                extra = ''
            if line.startswith('*') and not(self.starlines):
                ## we add the line always to the last string of the last tuple
                self.seq[-1] = (self.seq[-1][:-1] + (self.seq[-1][-1] + line,))
            elif line.startswith(u'-*-'):
                continue
            elif line.startswith(u'校勘記¶'):
                break
            elif line.startswith('/'):
                self.seq[-1] = (self.seq[-1][:] + (line,))
#                self.seq[-1] = (self.seq[-1][0], self.seq[-1][1] + line)
            elif line.startswith('#') or line.startswith(':'):
#                self.seq[-1] = (self.seq[-1][0], self.seq[-1][1] + line)
                if line.startswith('#-'):
                    pass
                if line.startswith('#+'):
                    rp=line[2:-1].split()
                else:
                    ## '#+' is a singleline prop, '#' and ':' multiline, to next occurence, right?
#                    self.in_note = not (self.in_note)
                    rp=line[1:-1].split()
##[2011-03-11T13:44:09+0900] TODO: handle multiline propertiess
                if rp[0].startswith('PROPERTY'):
                    try:
                        self.defs[rp[1].lower()] = rp[2]
                        if rp[1].startswith('LASTPB'):
                            pass
#                            setPage(rp[2])
                    except:
                        pass
##[2011-03-11T13:44:09+0900] TODO: handle sections
                    if self.defs.has_key('juan') and not self.defs.has_key('sec'):
                        pass
                elif rp[0].startswith('BEGIN_'):
                    flag = rp[0].split('_')[1].lower()
                    self.flags[flag] = len(self.seq)
                    #mark the start of the verse
                    self.markup[flag][self.flags[flag]] = self.flags[flag]
                elif rp[0].startswith('END_'):
                    flag = rp[0].split('_')[1].lower()
                    flag = flag.replace(u'\xb6', u'')
                    #and the end
                    self.markup[flag][self.flags[flag]] = len(self.seq)
                else:
                    try:
                        self.defs[rp[0][:-1].lower()]=" ".join(rp[1:])
                    except:
                        pass
            elif self.in_note:
                self.seq[-1] = (self.seq[-1][:] + (line,))
            else:
                ex = self.re.split(line)
                cs=[a for a in zip(*[iter(ex[1:])]*2)]
                #self.seq[-1] = (self.seq[-1][:] + (ex[0],))
                self.seq[-1] = (self.seq[-1][:-1] + (self.seq[-1][-1] + ex[0],))
                self.seq.extend(cs)
                #what comes beyond the tab is added as extra element to the last tuple of the line
                if extra:
                    self.seq[-1] = (self.seq[-1][:] + ('\t' + extra,))

                
    def write_org(self, outfile, header=False):
        """write the text out in the supplied file object"""
        outfile.write("".join( ["".join(a) for a in self.seq]))

    def write_to_sections(self, path, header=False, alignpb=True):
        """write the text to path using the filename(s) in the sections array."""
        try:
            os.mkdir(path)
        except:
            pass
        for i in range(0, len(self.sections)):
            if len(self.sections[i][1]) < 1:
                of = "%s/%3.3d.txt" % (path, i)
            else:
                of = "%s/%s" % (path, self.sections[i][1])
            start = self.sections[i][0]
            try:
                limit = self.sections[i+1][0]
            except:
                limit = len(self.seq)
            outfile=codecs.open(of, 'w', self.encoding)
            if header:
                self.writeheader(outfile, i)
                #lastpb only if cpos > 0?
                tmp = start
                while tmp > 0:
                    tmp -= 1
                    if self.seq[tmp][self.ppos].find('<') > 0:
                        pb=self.seq[tmp][self.ppos]
                        outfile.write("#+PROPERTY: LASTPB  %s\n" % (pb[pb.find('<'):pb.find('>')+1]))
                        outfile.write("%s¶\n" % (pb[pb.find('<'):pb.find('>')+1]))
                        break
            if header and alignpb:
                ##for the first section, we keep the position at 1
                if tmp > 1:
                    tmp += 1
                outfile.write("".join(["".join(a) for a in self.seq[tmp:limit]]))
            else:
                outfile.write("".join(["".join(a) for a in self.seq[start:limit]]))
            outfile.close()

    def write_xml(self, outfile):
        """write the text out as xml in the supplied file object"""
        pass
    def writeheader(self, out, section):
        """writes the metadata for the section to the file"""
        out.write("#-*- mode: org; -*-\n")
        if self.defs.has_key('title'):
            out.write("#+TITLE: %s\n" % (self.defs['title']))
        out.write("#+DATE: %s\n" % (datetime.datetime.now().strftime("%Y-%m-%d %H:%M:%S")))
#        out.write("#+PROPERTY: ID %s\n" % (self.))
        for dx in self.defs.keys():
            if dx.startswith('#<pb'):
                continue
            if dx in ('lastpb', 'date', 'sec', 'juan'):
                continue
            if not 'mode: ' in self.defs[dx]:
                out.write("#+PROPERTY: %s %s\n" % (dx, self.defs[dx]))
        out.write("#+PROPERTY: JUAN %d\n" % (section))
        
            
class MandokuComp(object):
    #text1 and text2 are MandokuText objects
    def __init__(self, text1):
        self.maintext = text1
        self.othertexts = []
        self.x = {}
        self.s = None
    def setothertext(self, text):
        """text is a MandokuText object"""
        self.othertexts.append(text)
    def calcdiff(self, i):
        """calculate the difference array for text i"""
        self.setsequence(i)
        dres = self.x
        d=0
        t1 = self.maintext
        t2 = self.othertexts[i]
        cpos = t2.cpos
        for tag, i1, i2, j1, j2 in self.s.get_opcodes():
            if tag == 'replace':
                dx = j1 - i1
                for l in range(i1, i2):
                    if not(dres.has_key(l)):
                        dres[l] = {}
                    try:
                        dres[l][i]=t2.seq[l+dx][cpos]
                    except:
                        pass
                        #print l, i, l+dx, cpos
                        #sys.exit()
                if j2 - j1 > i2 - i1:
                    dy = j2 - j1 + i2 - i1
                    if not(dres.has_key(l)):
                        dres[l] = {}
                    dres[l][i]= "%s%s" % (dres[l][i], "".join([tmp[cpos] for tmp in t2.seq[dy:j2]]))
                        
            elif tag == 'insert':
                k = i1-1
                if not(dres.has_key(k)):
                    dres[k] = {}
                dres[k][i] = "%s%s" % (dres[k].get(i, ''), "".join([tmp[cpos] for tmp in t2.seq[j1-1:j2]]))
            elif tag == 'delete':
                for l in range(i1, i2):
                    if not(dres.has_key(l)):
                        dres[l] = {}
                    dres[l][i] = "-"

            
    def setsequence(self, i=0):
        """set othertext in position i as sequence 2 in the sequencematcher"""
        if len(self.othertexts) < 1:
            ##need to raise an error here!!
            print "set the second text first please!"
            return
        if self.s is None:
            self.s = SequenceMatcher()
            self.s.set_seq1([a[self.maintext.cpos] for a in self.maintext.seq])
        self.s.set_seq2([a[self.othertexts[i].cpos] for a in self.othertexts[i].seq])
        self.o = self.s.get_opcodes()
        
    def reorder(self, text1, text2, dx=100, b=35):
        """Text1 must be in the expected order, as one or multiple
        segments. Text2 is the one we want to split at the 'same'
        positions as 1.  We will overwrite its 'sections' property.
        If text2 has several sections, we will also try to bring them
        in the right order. // This is now in two methods: reorder() and segment()"""
        t2s = text2.sections
        text2.newsections = []
        s = SequenceMatcher()
        if len(t2s) > 1:
            for i in range (0, len(t2s)):
                start = t2s[i][0]
                try:
                    limit = t2s[i+1][0]
                except:
                    limit = len(text2.seq)
                s.set_seqs([a[text1.cpos] for a in text1.seq], [a[text2.cpos] for a in text2.seq[start:limit]])
                l=getlimits(s.get_opcodes(), 1, limit - start, dx,b)
                if l[0] == -1 and len(text2.newsections) > 1:
#                    print i, start, limit, l, t2s[i][1].decode('utf-8')
                    ##try a fallback:
                    ## take the upper limit of the previous one
                    s1 = text2.newsections[-1][1][1]
                    l1 = s1 + limit - start
#                    print "s1, l1:", s1, l1, text2.newsections[-1][2]
                    s.set_seqs([a[text1.cpos] for a in text1.seq[s1:l1]], [a[text2.cpos] for a in text2.seq[start:limit]])
                    l=getlimits(s.get_opcodes(), 1, limit - start, dx,b)
                    if l[0] != -1:
                        l = l[0] + s1, l[1] + s1
                    print "second attempt: ", i, start, limit, l, t2s[i][1].decode('utf-8')
                text2.newsections.append((i, l, t2s[i][1].decode('utf-8')))
        else:
            return
            ## this is now done in segment()
            if len(text1.sections) > 1:
                for i in range (0, len(text1.sections)):
                    start = text1.sections[i][0]
                    try:
                        limit = text1.sections[i+1][0]
                    except:
                        limit = len(text1.seq)
#                    print "start, limit:", start, limit
                    s.set_seqs([a[text1.cpos] for a in text1.seq[start:limit]], [a[text2.cpos] for a in text2.seq])        
                    l=getlimits(s.get_opcodes(), 0, limit - start, dx, b)
                    text2.newsections.append((i, l, text1.sections[i][1].decode('utf-8')))
            else:
                ##one file only in both texts, we need to consider
                ## them to correspond to each other??  lets look at
                ## the ratio
                text2.newsections=text2.sections
                return
        text2.newsections= sorted(text2.newsections, key=lambda x: x[1][0])
        ##now we copy over the in the new order text 
        newseq = [('', '')]
        ##this holds files that do not participate in the text (those with -1 as start position)
        tmpseq = []
        tmpsections = []
        for sec in text2.newsections:
            start = text2.sections[sec[0]][0]
            try:
                limit = text2.sections[sec[0]+1][0]
            except:
                limit = len(text2.seq)
            newseq.extend(text2.seq[start:limit])
            tmpsections.append((len(newseq), sec[2]))
        text2.seq = newseq
        text2.sections = tmpsections
#        text2.newsections = None

    def segment(self, text1, text2, dx=100, b=35):
        """Text1 must be in the expected order, as one or multiple
        segments. Text2 is the one we want to split at the 'same'
        positions as 1.  We will overwrite its 'sections' property.
        Text2 has to be in the right order, usually as a result of running reorder()"""
        t2s = text2.sections
        text2.newsections = []
        s = SequenceMatcher()
        ## and finally, we split the newly ordered text according to the sections in text1
        for i in range (0, len(text1.sections)):
            start = text1.sections[i][0]
            try:
                limit = text1.sections[i+1][0]
            except:
                limit = len(text1.seq)
                #                    print "start, limit:", start, limit
            s.set_seqs([a[text1.cpos] for a in text1.seq[start:limit]], [a[text2.cpos] for a in text2.seq])        
            l=getlimits(s.get_opcodes(), 0, limit - start, dx, b)
            text2.newsections.append((l[0], text1.sections[i][1].decode('utf-8')))
        #ok, now we look if there are file borders close, use them
        j=0
        ##pi mal daumen...
        for i in range (0, len(text2.newsections)):
            try:
                sec = text2.sections[j][0]
            except:
                continue
            newpos = text2.newsections[i][0]
            while sec < newpos - 500 and j <= len(text2.sections):
                print sec, newpos - 500
                try:
                    j += 1
                    sec = text2.sections[j][0]
                except:
                    pass
#            print sec, newpos, sec - newpos
            if sec - newpos < 120:
                text2.newsections[i] = sec, text2.newsections[i][1]


    def patch(self, ignore=False, treshold=25):
        """adds differences stored in self.x to the maintext.  Ignore ignores chars deleted in the other text(s)"""
        t1 = self.maintext
        cpos = t1.cpos
        for k in self.x.keys():
            t = t1.seq[k][cpos]
            s=set(self.x[k].values())
            r=[]
            for e in s:
                if len(e) > treshold:
                    r.append(e)
                elif e == t:
                    r.append(e)
                elif '&' in e:
                    r.append(e)
            for e in r:
                try:
                    s.remove(e)
                except:
                    pass
            d=":".join(s).replace('-', '')
            if not(ignore and d==''):
                t1.seq[k] = t1.seq[k][:cpos] + ("{%s:%s}" %(t, d.rstrip(':')),) + t1.seq[k][cpos+1:]

    def unpatch(self):
        t1 = self.maintext
        cpos = t1.cpos
        for k in self.x.keys():
            s=t1.seq[k][cpos]
            if s.find(':') > 0:
                t1.seq[k] = t1.seq[k][:cpos] + (s[1:s.find(':')],) + t1.seq[k][cpos+1:]

            
    def patch1 (self, i, treshold = 25):
        """adds differences from text i to text1"""
        if self.s is None:
            self.setsequence()
        t1 = self.maintext
        t2 = self.othertexts[i]
        for tag, i1, i2, j1, j2 in self.o:
            dx = j1 - i1
            li = i2 - i1
            lj = j2 - j1
            if tag == 'replace' and max(li, lj) < treshold:
                if li == lj:
                    for i in range(i1, i2):
                        k = t1.seq[i][t1.cpos]
                        r = t2.seq[i+dx][t2.cpos]
                        if k != r and not ('&' in k or '&' in r):
                            t1.seq[i-1] = t1.seq[i-1][:-1] + (t1.seq[i-1][-1] + u"{",)
                            t1.seq[i] = t1.seq[i][:-1] + ( u":%s}" % (r) + t1.seq[i][-1],)
                else:
                    xxr = u"".join([a[t2.cpos] for  a in t2.seq[j1:j2]])
                    t1.seq[i1-1] = t1.seq[i1-1][:-1] + (t1.seq[i1-1][-1] + u"{",)
                    t1.seq[i2-1] = t1.seq[i2-1][:-1] + ( u":%s}" % (xxr) + t1.seq[i2-1][-1],)


    def patch3(self):
        """after comparing two separate sets of two files, this will remove those where two have the same value"""
        t=self.maintext
        if t.cpos > 0:
            return
        for i in range(0, len(t.seq)):
            if '{{' in t.seq[i][-1]:
                try:
                    k=t.seq[i+1][0]
                    ll=t.seq[i+1][1]
                    l1=ll.split('}:')[0][1:]
                    l2=ll.split('}:')[1][:ll.split('}:')[1].find('}')]
                except:
                    print i, t.seq[i]
                    print i, t.seq[i+1]
                    continue
                if l1 == l2:
                    t.seq[i] = t.seq[i][:-1] + (t.seq[i][-1][:t.seq[i][-1].find('{{')],)
                    t.seq[i+1] = (l1, ll[ll.find('}:'+l1+'}')+4:],)
                else:
                    t.seq[i] = t.seq[i][:-1] + (t.seq[i][-1][:-1],)
                    t.seq[i+1] = (k, ll.replace('}:',':'))
                    
    def show_replace(self, i):
        t1 = self.maintext
        t2 = self.othertexts[i]
        if self.s is None:
            self.setsequence()
        for tag, i1, i2, j1, j2 in self.s.get_opcodes():
            if tag == 'replace':
                print "".join([a[t1.cpos] for a in t1.seq[i1:i2]]), "".join([a[t2.cpos] for a in t2.seq[j1:j2]])
    def mergelayout(self, i, action='punc'):
        u"""update text2 with layout markers from text1
        target is 'punc' (move punctuation) or 'layout' (move pb and \xb6). """
        ##TODO: what I really want is a separation of target of the action and the action itself,
        ## then I could define various actions that move some features of t1 to t2 etc.
        
        t1 = self.maintext
        t2 = self.othertexts[i]
        if t1.cpos == 0:
            t1.punc_reorder()
        if t2.cpos == 0:
            t2.punc_reorder()
        if self.s is None:
            self.setsequence()
        for tag, i1, i2, j1, j2 in self.o:
            dx = j1 - i1
            li = i2 - i1
            lj = j2 - j1
            #no matter what tag, if they are the same length; we take it
            #i.e. equal or replace
            if li == lj:
                for i in range(i1, i2):
                    if action == 'punc':
                        t2.seq[i+dx] = (t2.seq[i+dx][0], t1.seq[i][1], t2.seq[i+dx][2], t1.seq[i][3],) + t2.seq[i+dx][4:]
                    else:
                        t2.seq[i+dx] = (t1.seq[i][0],) + t2.seq[i+dx][1:3] + t1.seq[i][4:]
            elif tag == 'replace':
#                print dx, i1, i2
                l = min(li, lj)
                for i in range(i1, i1+l):
                    if action == 'punc':
                        t2.seq[i+dx] = (t2.seq[i+dx][0], t1.seq[i][1], t2.seq[i+dx][2], t1.seq[i][3],) + t2.seq[i+dx][4:]
                    else:
                        t2.seq[i+dx] = (t1.seq[i][0],) + t2.seq[i+dx][1:3] + t1.seq[i][4:]
                #only if text1 is longer, we add the rest of 1 add the last char
                if li > lj:
                    if action == 'punc':
                        ##this misses out on the extra stuff in a[1]??
                        t2.seq[i+dx] = (t2.seq[i+dx][0], t1.seq[i][1], t2.seq[i+dx][2], t1.seq[i][3] + + "".join([a[1]+a[3] for  a in t1.seq[i:i2]]),) + t2.seq[i+dx][4:]
                    else:
                        ##TODO
                        pass
            elif tag == 'delete':
                if action == 'punc':
                    t2.seq[j1-1] = t2.seq[j1-1][:t2.mpos] + (t2.seq[j1-1][t2.mpos] + "".join([a[1]+a[3] for  a in t1.seq[i:i2]]), ) + t2.seq[i+dx][t2.mpos+1:]
                else:
                    ##TODO
                    pass
                


    def move_pg_and_xb6tot2(self, i):
        """update text2 with layout markers from text1"""
        ##TODO: what I really want is a separation of target of the action and the action itself,
        ## then I could define various actions that move some features of t1 to t2 etc.
        t1 = self.maintext
        t2 = self.othertexts[i]
        if t1.cpos == 0:
            t1.punc_reorder()
        if t2.cpos == 0:
            t2.punc_reorder()
        if self.s is None:
            self.setsequence()
        for i in range(0, len(t2.seq)):
            if u'\xb6' in t2.seq[i][t2.mpos]:
                t2.seq[i] = t2.seq[i][:t2.mpos] + (t2.seq[i][t2.mpos].replace(u'\xb6', ''), )
        for tag, i1, i2, j1, j2 in self.o:
            dx = j1 - i1
            li = i2 - i1
            lj = j2 - j1
            #no matter what tag, if they are the same length; we take it
            #i.e. equal or replace
            if li == lj:
                for i in range(i1, i2):
                    t2.seq[i+dx] = t2.seq[i+dx][:t2.mpos] + (t2.seq[i+dx][t2.mpos]+t1.seq[i+dx][t1.mpos].replace('\n', ''), )+ t2.seq[i+dx][t2.mpos+1:]
            elif tag == 'replace':
#                print dx, i1, i2
                l = min(li, lj)
                for i in range(i1, i1+l):
                    t2.seq[i+dx] = t2.seq[i+dx][:t2.mpos] + (t2.seq[i+dx][t2.mpos]+t1.seq[i+dx][t1.mpos].replace('\n', ''),) + t2.seq[i+dx][t2.mpos+1:]
                #only if text1 is longer, we add the rest of 1 add the last char
                if li > lj:
                    t2.seq[i+dx] = t2.seq[i+dx][:t2.mpos] +(t2.seq[i+dx][t2.mpos] + t1.seq[i+dx][t1.mpos].replace('\n', '') + "".join([a[t1.mpos].replace('\n', '') for  a in t1.seq[i:i2]]), ) + t2.seq[i+dx][t2.mpos+1:]
            elif tag == 'delete':
                t2.seq[j1-1] = t2.seq[j1-1][:t2.mpos] + (t2.seq[j1-1][t2.mpos] + "".join([a[t1.mpos].replace('\n', '') for  a in t1.seq[i:i2]]), ) + t2.seq[i+dx][t2.mpos+1:]

            
    def mergepunc(self, i=0):
        """update text1 with punctuation from text2"""
        t1 = self.maintext
        t2 = self.othertexts[i]
        if self.s is None:
            self.setsequence()
        if t2.cpos > 1:
            t2.seq = [a[0:t2.cpos-1] + ('', a[t2.cpos], '', ) + a[t2.cpos+2:] for a in t2.seq]
        elif t2.cpos == 1:
            t2.seq = [('', a[t2.cpos], '', ) + a[t2.cpos+2:] for a in t2.seq]
        else:
            t2.seq = [(a[t2.cpos], '', ) + a[2:] for a in t2.seq]
        for tag, i1, i2, j1, j2 in self.s.get_opcodes():
            dx = j1 - i1
            li = i2 - i1
            lj = j2 - j1
            #no matter what tag, if they are the same length; we take it
            #i.e. equal or replace
            if li == lj:
                for i in range(i1, i2):
                    t2.seq[i+dx] = (t2.seq[i+dx][0], t1.seq[i][1],) + t2.seq[i+dx][2:]
            elif tag == 'replace':
#                print dx, i1, i2
                l = min(li, lj)
                for i in range(i1, i1+l):
#                    print i
                    t2.seq[i+dx] = (t2.seq[i+dx][0], t1.seq[i][1],) + t2.seq[i+dx][2:]
                #only if text1 is longer, we add the rest of 1 add the last char
                if li > lj:
                    t2.seq[i+dx] = (t2.seq[i+dx][0], t1.seq[i][1] + "".join([a[1] for  a in t1.seq[i:i2]]), ) + t2.seq[i+dx][2:]
            elif tag == 'delete':
                t2.seq[j1-1] = (t2.seq[j1-1][0], t2.seq[j1-1][1] + "".join([a[1] for  a in t1.seq[i:i2]]), ) + t2.seq[i+dx][2:]

    def test(self, i):
        for tag, i1, i2, j1, j2 in self.s.get_opcodes():
            a= "".join(["".join(z) for z in self.maintext.seq[i1:i2]])
            b= "".join(["".join(z) for z in self.othertexts[i].seq[j1:j2]])
            print tag, i1, i2, j1, j2, a, b
            
    def comp_dict(self, dict, i=0, pr=True):
        for tag, i1, i2, j1, j2 in self.s.get_opcodes():
            if tag == 'replace':
                li = i2 - i1
                lj = j2 - j1
                la = "".join([a[0] for a in self.maintext.seq[i1:i2]])
                lb = "".join([a[0] for a in self.othertexts[i].seq[j1:j2]])
                if li == lj:
                    st = "%s:%s"%(la, lb)
                    try:
                        dict[st] += 1
                    except:
                        dict[st] = 1
        self.rep = sorted(dict.iteritems(), key=operator.itemgetter(1), reverse=True)
        if pr:
            print "\n".join(["%s\t%3.3d" % (a[0], a[1]) for a in self.rep])

def test():
#    f1 = MandokuText('/Users/chris/db/text/dzjy-can/JY128/JY128.org')
#    f2 = MandokuText('/Users/chris/db/text/dz/DZ1421/')
    f1 = MandokuText('/Users/chris/Dropbox/dao/test1.txt')
    f2 = MandokuText('/Users/chris/Dropbox/dao/test2.txt')
    f1.read()
    f2.read()
    f = MandokuComp(f1)
    f.setothertext(f2)
    return f

def testf1():
    f1 = MandokuText('/Users/chris/db/text/dzjy-can/JY002/JY002.org')
    f1.read()
    return f1


def getlimits(d, n, lx, dx=100, b=10):
    """analyses opcodes to determine the upper and lower limits of
    sequence 1 as it maps to sequence 2.  'n' tells is 1 or 2 and
    tells us which sequence to analyse."""
    ##target lenght should be somehow brought in??
    if n == 1:
        i = 1
    else:
        i = 3
    ar = [(a[i], a[2]-a[1]) for a in d if a[0] == 'equal']
    #if the following entry is closer than double the length of the 'equal' secion, we take it to be the start of the parallel part
    buckets=[[]]
    ##arrange stuff in buckets
    for i in range(0, len(ar) - 1):
        buckets[-1].append(ar[i])
        if ar[i][0] + ar[i][1] + dx < ar[i+1][0]:
            buckets.append([])
    ##get the index of the largest bucket
    ix=sorted([(i, len(buckets[i])) for i in range(0, len(buckets))], key=lambda x: x[1], reverse=True)
    index = ix[0][0]
    ## if we have more than 10 buckets, we do not really have a corresponding file, so we return failure
    blen = sum([a[1] for a in buckets[index]])
    res = blen - lx / 5 
#    print len(ix), ix, lx, blen, res
    if len(buckets) > b or res < 0:
        return -1, -1
    else:
    ##return the first and last index of the largest bucket.
        try:
            return buckets[index][0][0], buckets[index][-1][0]
        except:
            return -1, -1