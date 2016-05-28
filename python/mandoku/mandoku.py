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
:license:      CC 3.0 BY
"""
from __future__ import division
from __future__ import absolute_import

import sys, codecs, os ,re, operator, collections, git, time, datetime
from difflib import *
from itertools import *
from sparsedict import *
#removed: \uFE30-\uFE4F  
kanji=Ur'\u3000\u3400-\u4DFF\u4e00-\u9FFF\uF900-\uFAFF'
astkanji = Ur'\U00020000-\U0002A6DF\U0002A700-\U0002B73F\U0002B740-\U0002B81F\U0002B820-\U0002F7FF'
pua=Ur'\uE000-\uF8FF'
astpua = Ur'\U000F0000-\U000FFFFD\U00100000-\U0010FFFD'
##this will recognize image links like [[./img]] as 1 kanji --> clear this out later?!
ent=r'\[\[.*?\]\]|\[[^\]]*\]|&[^;]*;|&amp;[CZ][X3-7]-[A-F0-9]+'
#now
kp_re = re.compile(u"(%s|[%s%s])" % (ent, kanji, pua))
# astpua = ""
# astkanji = ""
#kp_re = re.compile(u"(%s|{[%s%s]+:[^}]*}|[%s%s])" % (ent, kanji+astkanji, pua+astpua, kanji+astkanji, pua+astpua))
ch_re = re.compile(ur'(\[[^\]]*\]|&[^;]*;|&amp;C[X3-7]-[A-F0-9]+|.)')
img_re = re.compile(ur'<i[^>]*>')
punc_re = re.compile(ur"[\u3001-\u33FF\uFE00-\uFF7F]")
meta_re = re.compile(ur'(<[^>]*>|\xb6|\n)')
sys.stdout = codecs.getwriter('utf8')(sys.stdout)
puamagic = 1060864


class MandokuText(object):
    def __init__(self, textpath, version='master', starlines=True, encoding='utf-8', ext='.txt', coll=None):
        """Read and parse the text.  If we get a directory, read all files in lexical order as parts of the text."""
        try:
            v = version.split(',')
            version=v[0]
        except:
            pass
        self.in_note = False
        self.in_zhu = False
        self.flags = {}
        self.baseedition = ""
        #cutoff for some operations.
        self.bigfile = 1500000
        self.sections = []
        self.neworder = []
        #a dictionary of sparsedicts:
        #a note for example is registered as follows: self.markup['note']
        self.markup = collections.defaultdict(SparseDict)
        #this allows me to lookup by character position
        # the number of pb in the input file.
        self.pbcnt = 0
        self.mdcnt = 0
        self.pages = SparseDict()
        #page per sections
        self.pps = {}
        #tocs per sections
        self.sectocs = {}
        #toc
        self.toc = []
        self.img={}
        #indexes the page number to a list of character positions, every position gives the beginning of a line
        #self.lines = collections.defaultdict(list)
        self.lines = {}
        #Holds various metadata about the text culled from the header
        self.defs = {}
        #holds the text, in this case the first character is in fact position 1
        #the first is a dummy, so that we can dump the beginning of the file and the first item of 'ex'
        #additional items might be placed in the tuple, so the implementation should *not* rely on there being only two items
        #the position at which the character is to be found in the seq tuple
        self.cpos = 0
        #the position at which metadata (u'\b6', <pb etc) is to be found
        self.mpos = 1
        self.seq = []
        self.ext = ext
        self.textpath = textpath
        if coll is None:
            self.coll = os.path.split(os.path.dirname(textpath))[1]
        else:
            self.coll = coll
        self.encoding = encoding
        self.versions = []
        ##version is a string that can be used for a branch in git
        try:
            repo = git.Repo(self.textpath)
            ##we want to know what other versions are available
            if not version in repo.heads:
                try:
                    version = v[1]
                except:
                    pass
            for b in repo.heads:
                ## we want to have unicode strings here!!
                self.versions.append(b.name.decode('utf-8'))
                ##make sure we have checked out the right version
                if b.name == version:
                    b.checkout()
            self.version = repo.active_branch.name.decode('utf-8')
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

    def write(self, ofile, str, replace=True):
        "optionally replaces the md before writing out the file, but only if there had not been pbs originally"
        if replace and self.pbcnt == 0:
            ofile.write(str.replace('<md:', '<pb:'))
        else:
            ofile.write(str)
            
    def read(self):
        self.seq = []
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

    def getsigle(branch, db=None):
        """This is a small helper function to get a convenient handle for a
branch.  In most cases, this should be overwritten by a inherited
function with access to a database."""
        return branch


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
                    if e[0] in (u'¶', u'\n'):
                        m += e
                    elif e[0] == "<":
                        p += e
                    else:
                        for j in range(0, len(e)):
                            if e[j] in (u'〈', u'《', u'「', u'『', u'【', u'〖', u'〘', u'〚', u'\u3000', '(', '*', '.', ' '):
#                            if e[j] in (u'〈', u'《', u'「', u'『', u'【', u'〖', u'〘', u'〚',  '(', '*', '.', ' '):
                                p += e[j]
                            elif re.search(r"[@0-9A-Za-z]", e[j]):
                                p += e[j]
#                                print "p", i, p
                            else:
                                np += e[j]
            self.seq[i] = ('', self.seq[i][self.cpos], np, m, ) + self.seq[i][self.cpos+2:]
            if len(p) > 0:
                try:
                    self.seq[i+1] = (p + self.seq[i+1][0], ) + self.seq[i+1][1:]
                except:
                    pass
                    #print i, p, self.seq[i]
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

    def maketoc(self):
        prevlev = 0
        tmptoc={0:[]}
        for i in range(1, len(self.sections)+1):
            s, f = self.sections[i-1]
            secid=f[0:f.find('.')]
#            print secid
            start = s
            try:
                end = self.sections[i][0]
            except:
                end = len(self.seq)
            #print start, end
            toc = self.makesectoc(start, end)
            self.sectocs[secid] = toc
            ky = toc.keys()
            ky.sort()
            for k in ky:
                level, heading, parent = toc[k]
                out = (level, heading, secid, k)
                if level == prevlev or level > prevlev:
                    if not tmptoc.has_key(level):
                        tmptoc[level] = [out]
                    else:
                        tmptoc[level].append((out))
#                elif level > prevlev:
#                    tmp[level] = [out]
                else:
                    while (prevlev > level):
                        #this might fail if there are glitches in the hierarchy, like missing levels etc. so better escape it
                        try:
                            tmptoc[prevlev -1].append(tmptoc[prevlev])
                        except:
                            tmptoc[prevlev -1]=[tmptoc[prevlev]]
                        #need to delete this level after use!
                        tmptoc[prevlev] = []
                        prevlev -= 1
                    try:
                        tmptoc[level].append((out))
                    except:
                        pass
#                        tmptoc[level]=[out]
                prevlev = level
        #add the last sections, if necessary
        if prevlev > 1:
            while (prevlev > 1):
                try:
                    tmptoc[prevlev -1].append(tmptoc[prevlev])
                except:
                    tmptoc[prevlev -1]=[tmptoc[prevlev]]
                tmptoc[prevlev] = []
                prevlev -= 1
        if len(tmptoc) > 1:
            try:
                self.toc = tmptoc[1]
            except:
                sys.stderr.write("Warning: %s; level 1 missing. \n" % ( self.sections))
                try:
                    self.toc = tmptoc[2]
                except:
                    print "maketoc failed!, ", tmptoc
            
    def makesectoc(self, start, end):
        """I'm passing the boundaries of the section here"""
        toc= SparseDict()
        nl=None
        parent = ''
        level = 0
        t = ''
        cur = {}
        for i, tmp in enumerate(self.seq[start:end]):
            a = tmp[self.cpos]
            b = "".join(tmp[self.cpos+1:])
            if nl is not None and not "\n" in b:
                t += a
            elif nl is not None and "\n" in b:
                t += a
                #this works only if the parent is accidentally in the same section
                try:
                    parent = cur[level - 1]
                except:
                    parent = ''
                t = t.strip().split('(')[0]
                toc[nl] = (level, t, parent)
                cur[level] = t
#                print nl, t
                nl = None
            if "*" in b:
                level = b.find(' ', b.index('*')) - b.index('*')
                t = ""
                nl = i
        return toc


    def add_metadata(self, per_section=False):
        l=0
        page="first"
        self.secstart={}
        self.lines[page] = []
        for i in range(1, len(self.sections)+1):
            s, f = self.sections[i-1]
            fx = f[0:f.find('.')]
            self.secstart[fx] = s
            if per_section:
                self.pps[fx] = SparseDict()
            try:
                cnt = self.sections[i][0]
            except(IndexError):
                cnt = len(self.seq)
            for j in range(s, cnt):
                x = len(re.findall(u"\xb6", self.seq[j][self.mpos]))
                if x > 0:
                    l += x
                    ##we say +1 because the line starts on the next character 
                    self.lines[page].append(j+1)
                m=re.search(ur"(<pb:[^>]*>)", self.seq[j][self.mpos])
                if m:
                    page = m.groups()[0]
                    if per_section:
                        #the pb is attached to the last character on the previous page...
                        self.pps[fx][j - s +1] = page
                    else:
                        self.pages[j+1] = page
                    self.lines[page] = [j+1]

    def getpl(self, pos, fx=False):
        "if fx (a section name) is passed, pos is relative, otherwise absolute."
        if len(self.lines) < 1:
            self.add_metadata(fx)
        if fx:
            pg = self.pps[fx][pos]
        else:
            pg = self.pages[pos]
        if pg:
            for i in range(1, len(self.lines[pg])+1):
                l=self.lines[pg][i-1] - self.secstart[fx]
                if l > pos:
                    try:
                        ld = self.lines[pg][max(0,i-2)] - self.secstart[fx]
                        #if ld > pos + 2:
                        #    ld = 0
                    except:
                        ld = 0
                    try:
                        db=self.lines[pg][i-2] - self.secstart[fx]
                    except:
                        db=0
                    return "%s%d,%d,%d,l:%d" % (pg[1:-1].split('_')[-1], i-1, pos - ld + 1, db, l)
        return None

    def setbaseedition(self, be):
        self.baseedition = be

    def charstat(self):
        self.charcnt = collections.defaultdict(int)
        for cx in self.seq:
            c=cx[self.cpos]
            if c== u"　" or len(c) < 1:
                continue
            self.charcnt[c] += 1
    
    def parse(self, infile):
        ##we add an empty seq at the beginning of each file
        zhu_buf = ""
        self.seq.append(('', ''))
        for line in infile:
            if line.upper().startswith(u':END:') and self.in_zhu:
                self.in_zhu = False
                zhu_buf += line
                self.seq[-1] = (self.seq[-1][:-1] + (self.seq[-1][-1] + zhu_buf,))
            elif self.in_zhu:
                zhu_buf += line
                continue
            try:
                line, extra = line.split('\t', 1)
            except:
                extra = ''
            self.pbcnt += len(re.findall(ur"<pb:", line))
            self.mdcnt += len(re.findall(ur"<md:", line))
            #this is basically a hack for the YP-C files of DZJY
            if line.startswith('#<md'):
                continue
            elif line.startswith('*') and not(self.starlines):
                ## we add the line always to the last string of the last tuple
                self.seq[-1] = (self.seq[-1][:-1] + (self.seq[-1][-1] + line,))
            ## parse the zhu annotations
            elif line.startswith(u':zhu:'):
                self.in_zhu = True
                zhu_buf = line
                continue
            elif line.startswith(u'<pb:') and len(self.seq) == 1:
                #this is a pb before the text starts, add to the first element
                #self.seq[-1] = (self.seq[-1][:-1] + (self.seq[-1][-1] + line,))
                self.seq.extend([('', line)])
            elif line.startswith(u'-*-'):
                continue
            elif line.startswith(u'校勘記¶'):
                break
            elif line.startswith('/'):
                self.seq[-1] = (self.seq[-1][:] + (line,))
#                self.seq[-1] = (self.seq[-1][0], self.seq[-1][1] + line)
            elif line.startswith('#') or line.startswith(':'):
#                self.seq[-1] = (self.seq[-1][0], self.seq[-1][1] + line)
#                print "have a #+"
                if line.startswith('#<') or line.startswith('#-'):
                    pass
                elif line.upper().startswith('#+BEGIN') or line.upper().startswith('#+END'):
                    #this is a commented pb, we treat it as a regular pb
                    self.seq[-1] = (self.seq[-1][:] + (line,))
                elif line.startswith('#+'):
                    rp=line[2:-1].split(':', 2)
#                    print "have a #+"
                    ##[2011-03-11T13:44:09+0900] TODO: handle multiline propertiess
#                 else:
#                     ## '#+' is a singleline prop, '#' and ':' multiline, to next occurence, right?
# #                    self.in_note = not (self.in_note)
#                    rp=line[1:-1].split(' ', 2)
                    if rp[0].startswith('PROPERTY'):
                        try:
                            self.defs[rp[1].lower()] = rp[2].strip()
                            if rp[1].startswith('LASTPB'):
                                pass
    #                            setPage(rp[2])
                        except:
                            pass
                    ##[2011-03-11T13:44:09+0900] TODO: handle sections
                        if self.defs.has_key('juan') and not self.defs.has_key('sec'):
                            pass
                    ##[2012-11-17T11:44:52+0900] sorry, my mods yesterday broke this.  Arrgh!
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
                            self.defs[rp[0].lower()]=" ".join(rp[1:]).strip()
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

        #reduce empty         
    def write_org(self, outfile, header=False):
        """write the text out in the supplied file object"""
        self.write(outfile, "".join( ["".join(a) for a in self.seq]))

    def alignpb(self):
        #print "before: ", self.sections
        for i in range(0, len(self.sections)):
            start = self.sections[i][0]+5
            try:
                limit = self.sections[i-1][0]
            except:
                limit = 0
            tmp = start
            while tmp > limit:
                tmp -= 1
                if (self.seq[tmp][self.mpos].find('<pb') > 0) or (self.seq[tmp][self.mpos].find('<md') > 0 ):
                    self.sections[i] = (tmp+1, self.sections[i][1])
                    #print tmp+1, "".join(self.seq[tmp+1])
                    break

        #print "after: ", self.sections
        
            
    def write_to_sections(self, path, header=False, alignpb=True, add=True):
        """write the text to path using the filename(s) in the sections array."""
        # if we got the sections from a different file, we might need to align them to a pb
        if alignpb:
            self.alignpb()
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
                ## [2015-04-23T16:16:39+0900] we now use the above alignpb handling to split the sections more 
                #lastpb only if cpos > 0?
                # tmp = start
                # while tmp > 0:
                #     tmp -= 1
                #     if self.seq[tmp][self.mpos].find('<pb') > 0:
                #         pb=self.seq[tmp][self.mpos]
                #         #self.write(outfile, u"#+PROPERTY: LASTPB  %s\n" % (pb[pb.find('<'):pb.find('>')+1]))
                #         #self.write(outfile, u"%s¶\n" % (pb[pb.find('<'):pb.find('>')+1]))
                #         break
                #     #else:
                #         #print tmp, "".join(self.seq[tmp])
            # this is also switched off now, handled elsewhere. 
            if header and alignpb and False:
                ##for the first section, we keep the position at 1
                if tmp > 1:
                    tmp += 1
                self.write(outfile, "".join(["".join(a) for a in self.seq[tmp:limit]]))
            else:
                #additional stuff after the regular characters in a
                #seq entry belong before the next character, thus the
                #next file
                try:
                    # this is the first line of the new file
                    if add and len(self.seq[start - 1]) > self.cpos + 2:
                        tow = "".join(self.seq[start - 1][self.cpos + 2 :])
                        self.write(outfile, "%s\n" % (tow.replace("\n", "")))
                except:
                    pass
                #self.write(outfile, "\n-- break --\n")
                if add:
                    self.write(outfile, "".join(["".join(a) for a in self.seq[start:limit - 1]]))
                else:
                    #[2016-02-27T20:54:45+0900] dont cut the extra part of the last line
                    self.write(outfile, "".join(["".join(a) for a in self.seq[start:limit]]))
                # try:
                #     self.write(outfile, "".join(["".join(a) for a in self.seq[limit - 1 :limit][0][: self.cpos + 2]]))
                # except:
                #     self.write(outfile, "".join(["".join(a) for a in self.seq[limit - 1 :limit]]))
            outfile.write("\n")    
            outfile.close()

    def write_xml(self, outfile):
        """write the text out as xml in the supplied file object"""
        pass
    def writeheader(self, out, section):
        be=False
        jnx = False
        """writes the metadata for the section to the file"""
        out.write("# -*- mode: mandoku-view -*-\n")
        if self.defs.has_key('title'):
            out.write("#+TITLE: %s\n" % (self.defs['title']))
        out.write("#+DATE: %s\n" % (datetime.datetime.now().strftime("%Y-%m-%d %H:%M:%S")))
#        out.write("#+PROPERTY: ID %s\n" % (self.))
        for dx in self.defs.keys():
            if dx.startswith('#<pb'):
                continue
            if dx in ('lastpb', 'date', 'sec', 'title', 'source'):
                continue
            if dx == "juan":
                try:
                    jx = int(self.defs[dx])
                    out.write("#+PROPERTY: JUAN %d\n" % (section ))
                except:
                    out.write("#+PROPERTY: JUAN %s\n" % (defs[dx]))
                jnx = True
            if dx == "baseedition" and len(self.baseedition) > 0:
                out.write("#+PROPERTY: %s %s\n" % (dx.upper(), self.baseedition))
                out.write("#+PROPERTY: WITNESS %s\n" % ( self.version))
                be=True
            if not 'mode: ' in self.defs[dx]:
                out.write("#+PROPERTY: %s %s\n" % (dx.upper(), self.defs[dx]))
        if not be and len(self.baseedition) > 0:
            out.write("#+PROPERTY: BASEEDITION %s\n" % (self.baseedition))
            out.write("#+PROPERTY: WITNESS %s\n" % ( self.version))
        #write lastpb property for md files!
        if self.mdcnt > 0 and section > 0:
            start = self.sections[section][0]
            tmp = start - 1
            while tmp > 0:
                tmp -= 1
                if self.seq[tmp][self.mpos].find('<md') > 0:
                    pb=self.seq[tmp][self.mpos]
                    out.write(u"#+PROPERTY: LASTPB  %s\n" % (pb[pb.find('<'):pb.find('>')+1]))
                    break
        if not jnx:
            out.write("#+PROPERTY: JUAN %d\n" % (section +1))

    def printNgram(self, sx, sec, pos, extra=None):
        if extra:
            print sx, sec, pos, extra
        else:
            print "".join(sx), sec, pos, self.getpl(pos, sec )

    def addNgram(self, action='add', n=3):
        ##currently no other action is implemented
        notestart = 0
        noteend = 0
        for i in range(1, len(self.sections)+1):
            s, f = self.sections[i-1]
            fx = f[0:f.find('.')]
            print fx
            dn = 0
            try:
                cnt = self.sections[i][0]
            except(IndexError):
                cnt = len(self.seq)
#            print s, cnt
            for j in range(s, cnt):
#                print j, "".join(self.seq[j])
                outseq = []
                #first we check if the status of the text changes:
                try:
                    #the note-marker is on the character preceding the start of
                    #the note!
                    check="".join(self.seq[j+1][self.mpos:])
                except:
                    check=''
                if '(' in check:
                    notestart = j+1
                    noteend = notestart
                    found = False
                    while not found and noteend <= cnt:
                        noteend += 1
                        try:
                            check2 = "".join(self.seq[j+1][self.mpos:])
                        except:
                            check2 = ''
                        found = ')' in check2
                    dn = noteend - j + 1
                    ##[2013-03-02T10:40:07+0900]
                    ## we build the out sequence, but need to skip over u'\3000' characters
                    if not self.seq[notestart][self.cpos] == u'\u3000':
                        outseq = [self.seq[notestart][self.cpos]]
                    dxn = 0
                    while len(outseq) < n:
                        try:
                            val = self.seq[j+dn+dxn][self.cpos]
                        except IndexError:
                            val = ''
                        if not val == u'\u3000':
                            outseq.append(val)
                        dxn += 1
                    if not self.seq[j][self.cpos] == u'\u3000':
                        self.printNgram(outseq, fx, j - s)
                elif notestart+1 > j:
                    dxn = j
                    ## we need to first build the sequence before the note, then the part after the note
                    while len(outseq) < n and dxn < (notestart + 1):
                        try:
                            val = self.seq[dxn][self.cpos]
                        except IndexError:
                            val = ''
                        if not val == u'\u3000':
                            outseq.append(val)
                        dxn += 1
                    while len(outseq) < n:
                        try:
                            val = self.seq[dn+dxn][self.cpos]
                        except IndexError:
                            val = ''
                        if not val == u'\u3000':
                            outseq.append(val)
                        dxn += 1
                    if not self.seq[j][self.cpos] == u'\u3000':
                        self.printNgram(outseq, fx, j - s)
                elif notestart < j and j < noteend+1:
                    e = min(j+n, noteend+1)
                    dxn = j
#                    print dxn, self.seq[dxn]
                    while len(outseq)  < n or dxn <= e:
                        try:
                            val = self.seq[dxn][self.cpos]
                        except IndexError:
                            val = ''
                        if not val == u'\u3000':
                            outseq.append(val)
                        dxn += 1
                        print val, dxn, notestart, n, len(outseq), e

                    if not self.seq[j][self.cpos] == u'\u3000':
                        self.printNgram(outseq, fx, j - s)
                else:
                    dxn = j
                    while len(outseq) < n or n >= cnt:
                        try:
                            val = self.seq[dxn][self.cpos]
                        except IndexError:
                            val = ''
                        if not val == u'\u3000':
                            outseq.append(val)
                        dxn += 1
                    if not self.seq[j][self.cpos] == u'\u3000':
                        self.printNgram(outseq, fx, j - s)

    def _processopcodes(self, s, t2, s1start, s2start, res, bname, add_var_punctuation):
                d=0
                for tag, i1, i2, j1, j2 in s.get_opcodes():
                    ##first we look for pagebreaks, we need only those with a different version
                    ##we want to end up with a list that has text positions and pb per version
                    if  tag == 'equal':
                        dx = j1 - i1
                        for i in range(i1, i2):
#                            print "=", i, t2.seq[i+dx]
                            if '<pb:' in t2.seq[s2start+i+dx][self.mpos]:
                                pb = t2.seq[s2start+i+dx][self.mpos]
#                                print pb
                                self.img[bname][i] = pb[pb.find('<pb:'):pb.find('>', pb.find('<pb:'))+1]
                            if add_var_punctuation and t2.seq[s2start+i+dx][self.mpos] != '':
                                res[s1start+i+d] = ':' + t2.seq[s2start+i+dx][self.mpos]
                    if tag == 'replace':
                        a=[x[t2.cpos] for x in t2.seq[s2start+j1:s2start+j2]]
                        if add_var_punctuation:
                            b1=[x[1] for x in t2.seq[s2start+j1:s2start+j2]]
                            a=map(lambda xx : xx[self.cpos] + ':' + xx[1], zip(a,b1))
                        a.reverse()
                        for i in range(i1, i2):
                            try:
                                res[s1start+i+d] =  a.pop()
                            except:
                                #b is shorter than a
                                res[s1start+i+d] = ''
                        if len(a) > 0:
                            #b is longer than a, so we have left overs.
                            a.reverse()
                            res[s1start+i+d] = "%s%s" % (res[s1start+i], "".join(["".join(tmp) for tmp in a])) 
                    elif tag == 'insert':
                        k = i1-1+d
                        if add_var_punctuation:
                            #here we just grab the original e, munge it together and slab it onto the rest
                            res[s1start+k] =  "%s%s%s" % (res.get(s1start+k, ''), "".join(self.seq[s1start+i1-1:s1start+i1][self.cpos]), "".join("".join(["".join(a[t2.cpos]) for a in t2.seq[s2start+j1:s2start+j2]])))
                        else:
                            try:
                                res[s1start+k] =  u"%s%s%s" % (res.get(s1start+k, ''), u"".join([a[self.cpos] for a in self.seq[s1start+i1-1:s1start+i1]]), u"".join(u"".join(["".join(a[t2.cpos]) for a in t2.seq[s2start+j1:s2start+j2]])))
                            except(IndexError):
                                print "indexerror", k, i1, i2, j1, j2, "a", u"".join(s.a[0:10]), "b", u"".join(s.b[0:10] )
                                print res
                                continue
                                #sys.exit()
                    elif tag == 'delete':
                        res[s1start+i1+d] = ""


    def _getfirstbestmatch(self, s, lmin=20):
        buckets=[[]]
        prev=0
        max=0
        ret=0
        for tag, i1, i2, j1, j2 in s.get_opcodes():
            if tag == "equal":
                if i1 - prev < 30:
                    buckets[-1].append((i1, i2-i1))
                else:
                    buckets.append([(i1, i2-i1)])
                prev = i2
        for i, b in enumerate(buckets):
            tmp=sum([a[1] for a in b])
            #print b, tmp
            if tmp > max:
                max=tmp
                ret=i
        return buckets[ret][0][0]

    def recheck_newsections(self):
        """Looks at the md values to determine better borders for the sections and updates the sections list.
        Potentially dangerous!"""
        df = collections.defaultdict(list)
        if self.mdcnt > 0:
            for i, c in enumerate (self.seq):
                t1 = "".join(c[self.mpos:])
                if "<md" in t1:
                    l=re.split("(<..[^>]+>)", t1)
                    [df[a[1:-1].split("_")[-1].split("-")[0]].append((a[1:-1].split("_")[-1], i)) for a in l if "<md" in a]
            if self.cpos == 0:
                self.punc_reorder()
            self.newsections = []
            for i, s in self.sections:
                k=s.split("_")[1].split(".")[0]
                if df.has_key(k):
                    l = df[k]
                    l=sorted(l, key=lambda x : x[1])
                    newi=l[0][1]
                    if newi != i:
                        self.newsections.append((newi+1, s))
                    else:
                        self.newsections.append((newi, s))
                    #move mdmark to the following char, since it belongs to the beginning
                    if newi != i:
                        try:
                            self.seq[newi+1] = (self.seq[newi][self.mpos] + self.seq[newi+1][0], ) + self.seq[newi+1][self.cpos:]
                            self.seq[newi] = self.seq[newi][:-1] + ('',)
                        except:
                            print "Index error in seq, aborting."
                            return
            if len(self.newsections) == len(self.sections):
                self.sections = self.newsections
            else:
                print "Recheck failed. Section lengths do not match."
        else:
            print "No md markers found."

    def getneworder(self, othertext, enc="utf-8"):
        """Othertext is a MandokuText object, already parsed"""
        repdict={u"】" : ")", u"【" : "(", u"　" : ""}
        otherseq = [('', '')]
        s=SequenceMatcher()
        for line in codecs.open(othertext, "r", enc):
            if line.startswith("#"):
                continue
            line = re.sub("([%s])" % (pua), lambda x : "&ZX-%4.4x;" % (ord(x.group(1)) + puamagic ), line)
            line = re.sub(u"([】【　])", lambda x : repdict[x.group(1)], line)
            ex = self.re.split(line)
            cs=[a for a in zip(*[iter(ex[1:])]*2)]
            #otherseq[-1] = (otherseq[-1][:-1] + (otherseq[-1][-1] + ex[0],))
            otherseq.extend(cs)
        s.set_seq1([a[0] for a in otherseq])
        for i in range(0, len(self.sections)):
            tn, f = self.sections[i]
            secid=f[0:f.find('.')]
            start = tn
            try:
                end = self.sections[i+1][0]
            except:
                end = len(self.seq)
            s.set_seq2([a[self.cpos].replace(u'\u3000', '') for a in self.seq[start:end]])
            m=self._getfirstbestmatch(s)
            print m, i, f
            self.neworder.append((m, i))

    def addOtherBranches(self, add_var_punctuation=False):
        """adds the other branches to MandokuText."""
        #[2012-12-05T21:55:39+0900]
        # todo: currently, the positions are totals for the whole text, but I need to have them by section
        # also: look at mandoku_couch and see how to deal with different sections, number of section etc.
        try:
            repo = git.Repo(self.textpath)
        except:
            return "No git repository found"
        s = SequenceMatcher()
        self.s=s
        self.refs=[]
        self.branches={}
        self.txtid = self.textpath.split('/')[-1]
        #just in case
        s.set_seq1([a[self.cpos].replace(u'\u3000', '') for a in self.seq])
        #if possible, use sections for comparison!
        sseq = {}
        for i in range(1, len(self.sections)+1):
            si, fi = self.sections[i-1]
            start = si
            try:
                end = self.sections[i][0]
            except:
                end = len(self.seq)
            sseq[fi] = (si, [a[self.cpos].replace(u'\u3000', '') for a in self.seq[start:end]])
        for b in [a for a in repo.heads if a.name == a.name.upper()]:
            if b.name != self.version:
                print b.name
                b.checkout()
                self.branches[b.name]={}
                self.img[b.name]={}
                res = self.branches[b.name]
                sig = self.getsigle(b.name)
                t2 = MandokuText(self.textpath, version=b.name)
                self.refs.append(t2)
                t2.read()
                secmatch = 0
                ## check if we can use sections
                for i in range(1, len(t2.sections)+1):
                    si, fi = t2.sections[i-1]
                    if sseq.has_key(fi):
                        secmatch += 1
#                print "secmatch: ", secmatch
                if secmatch > max(len(t2.sections), len(self.sections)) - 2 and secmatch > 0:
                    for i in range(1, len(t2.sections)+1):
                        si, fi = t2.sections[i-1]
                        start = si
                        try:
                            end = t2.sections[i][0]
                        except:
                            end = len(t2.seq)
                        s.set_seq1(sseq[fi][1])
                        s.set_seq2([a[t2.cpos].replace(u'\u3000', '') for a in t2.seq[start:end]])
                        self._processopcodes(s, t2, sseq[fi][0], start, res, b.name, add_var_punctuation)
                else:
                    s.set_seq2([a[self.cpos] for a in t2.seq])
                    self._processopcodes(s, t2, 1, 1,  res, b.name, add_var_punctuation)
        for b in repo.heads:
            if b.name == self.version:
                b.checkout()
                print "reverted to ", b.name

    def otherNgram(self, version, n=3):
        """Adds Ngrams of the specified version."""
        try:
            res = self.branches[version]
        except:
            return "This version does not exist: %s " % (version)
        ks = res.keys()
        ks.sort()
        sd = SparseDict()
        for i in range(1, len(self.sections)+1):
            s, f = self.sections[i-1]
            sd[s] = [s, f[0:f.find('.')]]
        self.sd=sd
        for i in range(0, len(ks)):
            #cant' use negative values here
            lo = max(1, ks[i]-n+1)
            #put the positions of variants in the range here
            vals = []
            #for the result list
            sxr = []
            j = i
            while(ks[j] < ks[i] - n and j > 0):
                vals.append(ks[j])
                j -= 1
            j = i-1
            while(j <= len(ks) and  ks[j] < ks[i] + n):
                vals.append(ks[j])
                j += 1
                if j >= len(ks):
                    break
            for c in range(lo, ks[i]+n):
                if c in vals:
                    try:
                        sxr.append(res[c][self.cpos])
                    except:
                        sxr.append(res[c])
                else:
                    sxr.append(self.seq[c][self.cpos])
                #todo: work out what to do with 0.  Also see if we have a 1 off problem
            ix=0
            for c in range(lo, ks[i]+1):
                #the output has to be relative to the sections
                print c, "".join(sxr)
                s, fx = self.sd[c]
                self.printNgram("".join(sxr[ix:ix+n]), fx, c - s, version)
                ix +=1
        
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
        if text2.mdcnt > 0:
            text2.recheck_newsections()
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
            s.set_seqs([a[text1.cpos] for a in text1.seq[start:limit]], [a[text2.cpos] for a in text2.seq])        
            l=getlimits(s.get_opcodes(), 0, limit - start, dx, b)
            text2.newsections.append((l[0], text1.sections[i][1].decode('utf-8')))
        #ok, now we look if there are file borders close, use them
        j=0
        #starting at the second, we make another run to try to fix -1 positions
        for i in range (1, len(text2.newsections)):
            if text2.newsections[i][0] == -1:
                start = text1.sections[i][0]
                try:
                    limit = text1.sections[i+1][0]
                except:
                    limit = len(text1.seq)
                t2start = text2.newsections[i-1][0]
                try:
                    t2end = text2.newsections[i+1][0]
                except:
                    t2end = len(text2.seq)
                s.set_seqs([a[text1.cpos] for a in text1.seq[start:limit]], [a[text2.cpos] for a in text2.seq[t2start:t2end]])        
                l=getlimits(s.get_opcodes(), 0, limit - start, dx, b)
                #print "segment: start, limit =", start, limit, "l", l 
                text2.newsections[i] = l[0]+t2start, text2.newsections[i][1]
                #print text2.newsections
##pi mal daumen...
        #if we have only one file to begin with, we do not need this.

#         for i in range (0, len(text2.newsections)):
#             try:
#                 sec = text2.sections[j][0]
#             except:
#                 continue
#             newpos = text2.newsections[i][0]
#             while sec < newpos - 500 and j <= len(text2.sections):
#                 print sec, newpos - 500
#                 try:
#                     j += 1
#                     sec = text2.sections[j][0]
#                 except:
#                     pass
# #            print sec, newpos, sec - newpos
#             if sec - newpos < 120:
#                 text2.newsections[i] = sec, text2.newsections[i][1]


    def patch(self, ignore=False, replace=False, treshold=25):
        """adds differences stored in self.x to the maintext.  Ignore ignores chars deleted in the other text(s).  If replace is True, instead of putting both, only the characters from the other text are used."""
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
                if replace:
                    t1.seq[k] = t1.seq[k][:cpos] + ("%s" %(d.rstrip(':')),) + t1.seq[k][cpos+1:]
                else:
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
                
    def extratot1(self, otx=0):
        """Update text 1 with the extra information from text 2.
        Linebreaks in T1 will be removed and replaced with LB from T2"""
        t1 = self.maintext
        t2 = self.othertexts[otx]
        if t1.cpos == 0:
            t1.punc_reorder()
        if t2.cpos == 0:
            t2.punc_reorder()
        if self.s is None:
            self.setsequence()
        # for i in range(0, len(t1.seq)):
        #     if u'\n' in t1.seq[i][t1.mpos]:
        #         t1.seq[i] = t1.seq[i][:t1.mpos] + (t1.seq[i][t1.mpos].replace(u'\n', ''), )
        for tag, i1, i2, j1, j2 in self.o:
            dx = j1 - i1
            li = i2 - i1
            lj = j2 - j1
            if li == lj:
                for i in range(i1, i2):
                    if len(t2.seq[i+dx]) > 4:
                        t1.seq[i] = t1.seq[i][:t1.mpos] + (t1.seq[i][t1.mpos].replace(u'\n', ''), ) + (re.sub(r"<pb[^>]*>|\xb6", "", t2.seq[i+dx][4]),) 
            elif tag == 'replace':
                l = min(li, lj)
                for i in range(i1, i1+l):
                    if len(t2.seq[i+dx]) > 4:
                        t1.seq[i] = t1.seq[i][:t1.mpos] + (t1.seq[i][t1.mpos].replace(u'\n', ''), ) + (re.sub(r"<pb[^>]*>|\xb6", "", t2.seq[i+dx][4]),) 
                #only if text1 is longer, we add the rest of 1 add the last char
                if li < lj:
                    # untested!
                    t1.seq[i1+li] = t1.seq[i1+li][:t1.mpos] + (t1.seq[i1+li][t1.mpos].replace(u'\n', ''), ) + ("".join([re.sub(r"<pb[^>]*>|\xb6", "", a[4]) for a in t2.seq[i+li+dx:i+lj+dx] if len(a) > 4]), )
            elif tag == 'insert':
                add = "\n" + "".join(["".join(a) for a in t2.seq[j1:j2]])
                t1.seq[i2] =  (add.replace("\n", "\n# i#")+"\n",) + t1.seq[i2]
            elif tag == 'delete':
                # nothing in t2, pass
                pass

    def move_pg_and_xb6tot2(self, otx=0):
        """update text2 with layout markers from text1"""
        ##TODO: what I really want is a separation of target of the action and the action itself,
        ## then I could define various actions that move some features of t1 to t2 etc.
        t1 = self.maintext
        t2 = self.othertexts[otx]
        if t1.cpos == 0:
            t1.punc_reorder()
        if t2.cpos == 0:
            t2.punc_reorder()
        #avoid unnecessary processing time with big files if possible
        if len(t1.seq) < t1.bigfile or len(t1.sections) != len(t2.sections):
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
                        if "md" in t2.seq[i+dx][t2.mpos]:
                            pass
                            #print "y", tag, i+dx, t2.seq[i+dx][t2.mpos], " / ", t1.seq[i]
                        else:
                            t2.seq[i+dx] = t2.seq[i+dx][:t2.mpos] + (t1.seq[i][t1.mpos].replace('\n', '').replace('<pb:', '<md:') + t2.seq[i+dx][t2.mpos], )+ t2.seq[i+dx][t2.mpos+1:]
                        # if "md" in "".join(t2.seq[i+dx]):
                        #     print t2.seq[i+dx]
                else:
                    if tag == 'replace':
        #                print dx, i1, i2
                        l = min(li, lj)
                        for i in range(i1, i1+l):
                            t2.seq[i+dx] = t2.seq[i+dx][:t2.mpos] + (t1.seq[i][t1.mpos].replace('\n', '').replace('<pb:', '<md:') + t2.seq[i+dx][t2.mpos], ) + t2.seq[i+dx][t2.mpos+1:]
                        #only if text1 is longer, we add the rest of 1 add the last char
                        if li > lj:
                            #skip punc before  \u3000 
                            inp=""
                            c = t1.seq[i:i2]
                            for z1, a in enumerate(c):
                                try:
                                    if c[z1+1][t1.cpos] != u"\u3000":
                                        inp += a[t1.mpos].replace('\n', '').replace('<pb:', '<md:')
                                        #print "delz", t2.seq[j1-1], "/", inp
                                except:
                                    inp += a[t1.mpos].replace('\n', '').replace('<pb:', '<md:')
                            #t1.seq[i][t1.mpos].replace('\n', '').replace('<pb:', '<md:') + "".join([a[t1.mpos].replace('\n', '').replace('<pb:', '<md:') for  a in t1.seq[i:i2]])
                            t2.seq[i+dx] = t2.seq[i+dx][:t2.mpos] +(inp + t2.seq[i+dx][t2.mpos] , ) + t2.seq[i+dx][t2.mpos+1:]
                    elif tag == 'delete':
                        #skip punc before  \u3000 
                        inp=""
                        c = t1.seq[i1:i2]
                        for z1, a in enumerate(c):
                            try:
                                if c[z1+1][t1.cpos] != u"\u3000":
                                    inp += a[t1.mpos].replace('\n', '').replace('<pb:', '<md:')
                                    #print "dely", t2.seq[j1-1], "/", inp
                            except:
                                inp += a[t1.mpos].replace('\n', '').replace('<pb:', '<md:')
                        t2.seq[j1-1] = t2.seq[j1-1][:t2.mpos] + (inp + t2.seq[j1-1][t2.mpos], ) + t2.seq[j1-1][t2.mpos+1:]
                        #print "x", j1-1, tag, "".join(t2.seq[j1-1]), "/", "$".join(["#".join(a) for  a in t1.seq[i:i2]])
        elif len(t1.sections) == len(t2.sections):
            for sn in range(1, len(t1.sections)+1):
                #for now, we assume the section numbers correspondent to each other
                s, f = t1.sections[sn-1]
                t1start = s
                t2start = t2.sections[sn-1][0]
                try:
                    t1end = t1.sections[sn][0]
                except:
                    t1end = len(t1.seq)
                try:
                    t2end = t2.sections[sn][0]
                except:
                    t2end = len(t2.seq)
                t1seq = t1.seq[t1start:t1end]
                t2seq = t2.seq[t2start:t2end]

                s=SequenceMatcher()
                s.set_seq1([a[t1.cpos] for a in t1seq])
                s.set_seq2([a[t2.cpos] for a in t2seq])

                for i in range(0, len(t2seq)):
                    if u'\xb6' in t2.seq[i+t2start][t2.mpos]:
                        t2.seq[i+t2start] = t2.seq[i+t2start][:t2.mpos] + (t2.seq[i+t2start][t2.mpos].replace(u'\xb6', ''), )
                for tag, i1, i2, j1, j2 in s.get_opcodes():
                    dx = j1 - i1 + t2start
                    li = i2 - i1
                    lj = j2 - j1
                    #no matter what tag, if they are the same length; we take it
                    #i.e. equal or replace
                    if li == lj:
                        for i in range(i1, i2):
                            if "md" in t2.seq[i+dx][t2.mpos]:
                                pass
                                #print "y", tag, i+dx, t2.seq[i+dx][t2.mpos], " / ", t1.seq[i]
                            else:
                                t2.seq[i+dx] = t2.seq[i+dx][:t2.mpos] + (t1.seq[i+t1start][t1.mpos].replace('\n', '').replace('<pb:', '<md:') + t2.seq[i+dx][t2.mpos], )+ t2.seq[i+dx][t2.mpos+1:]
                        # if "md" in "".join(t2.seq[i+dx]):
                        #     print t1.sections[sn][1], i+dx, "".join(t2.seq[i+dx])
                    else:
                        if tag == 'replace':
            #                print dx, i1, i2
                            l = min(li, lj)
                            for i in range(i1, i1+l):
                                t2.seq[i+dx] = t2.seq[i+dx][:t2.mpos] + (t1.seq[i+t1start][t1.mpos].replace('\n', '').replace('<pb:', '<md:') + t2.seq[i+dx][t2.mpos], ) + t2.seq[i+dx][t2.mpos+1:]
                            #only if text1 is longer, we add the rest of 1 add the last char
                            if li > lj:
                                #skip punc before  \u3000 
                                inp=""
                                c = t1.seq[i+t1start:i2+t1start]
                                for z1, a in enumerate(c):
                                    try:
                                        if c[z1+1][t1.cpos] != u"\u3000":
                                            inp += a[t1.mpos].replace('\n', '').replace('<pb:', '<md:')
                                            #print "delz", t2.seq[j1-1], "/", inp
                                    except:
                                        inp += a[t1.mpos].replace('\n', '').replace('<pb:', '<md:')
                                #t1.seq[i][t1.mpos].replace('\n', '').replace('<pb:', '<md:') + "".join([a[t1.mpos].replace('\n', '').replace('<pb:', '<md:') for  a in t1.seq[i:i2]])
                                t2.seq[i+dx] = t2.seq[i+dx][:t2.mpos] +(inp + t2.seq[i+dx][t2.mpos] , ) + t2.seq[i+dx][t2.mpos+1:]
                        elif tag == 'delete':
                            #skip punc before  \u3000 
                            inp=""
                            c = t1.seq[i1+t1start:i2+t1start]
                            for z1, a in enumerate(c):
                                try:
                                    if c[z1+1][t1.cpos] != u"\u3000":
                                        inp += a[t1.mpos].replace('\n', '').replace('<pb:', '<md:')
                                        #print "dely", t2.seq[j1-1], "/", inp
                                except:
                                    inp += a[t1.mpos].replace('\n', '').replace('<pb:', '<md:')
                            t2.seq[j1-1+t2start] = t2.seq[j1-1+t2start][:t2.mpos] + (inp + t2.seq[j1-1+t2start][t2.mpos], ) + t2.seq[j1-1+t2start][t2.mpos+1:]
                            #print "x", j1-1, tag, "".join(t2.seq[j1-1]), "/", "$".join(["#".join(a) for  a in t1.seq[i:i2]])
                        
        else:
            #do nothing?
            pass
        for lz in range(0, len(t2.seq)):
            ts = t2.seq[lz][t2.mpos]
            if "pb" in ts or "md" in ts:
                mdx = ""
                pbx = ""
                #reduce multiple md or pb to 1
                pblx = re.split("(<..[^>]+>)", ts)
                for lx in pblx:
                    if lx[1:3] == "md":
                        mdx = "%s¶" % (lx)
                    elif lx[1:3] == "pb":
                        pbx = "\n%s\n" % (lx)
                t2.seq[lz] = t2.seq[lz][:t2.mpos] + (u"%s%s" % (mdx, pbx), ) + t2.seq[lz][t2.mpos+1:]
            if "md" in ts:
                t2.mdcnt += 1


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
    ##target length should be somehow brought in??
    if n == 1:
        i = 1
    else:
        i = 3
    #ar = [(a[i], a[2]-a[1]) for a in d if a[0] == 'equal']
    ar = [(a[i], a[2]-a[1]) for a in d if a[0] == 'equal' and a[2] - a[1] > 1]
    ##=> maybe ignore those of length 1?
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
    #print "getlimits: ", len(ix), ix, lx, blen, res
    if len(buckets) > b or res < 0:
        return -1, -1
    else:
    ##return the first and last index of the largest bucket.
        try:
            return buckets[index][0][0], buckets[index][-1][0]
        except:
            return -1, -1
