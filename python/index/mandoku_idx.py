#!/bin/env python -*- coding: utf-8 -*-
# convert txt files to mandoku index format

import os, sys, codecs, re, datetime
ch_re = re.compile(ur'(\[[^\]]*\]|&[^;]*;|&amp;C[X3-7]-[A-F0-9]+|.)')
img_re = re.compile(ur'<i[^>]*>')
sys.stdout = codecs.getwriter('utf8')(sys.stdout)
outfiles = {}
tab={'a':'1', 'b':'2', 'c':'3', 'd': '4', 'e': '5', 'f': '6', 'g':'7', 'h':'8', 'i':'9'}
idx={}
def PrintToIdxfile(outdir, string, collection):
    try:
        code = ("%4.4x"%(ord(string[0])))[0:4]
    except:
        code = "gj"
    if (not code.startswith('30') and  "()/¶*".find(string[0]) == -1 ):
        try:
            os.mkdir("%s/%s"%(outdir, code[0:2]))
        except:
            pass
        ofile="%s/%s/%s.%s.idx"%(outdir, code[0:2], code, collection)
#        ofile="%s/%s/%s.%s.idx"%(outdir, 'test', 'test', collection)
        try:
            idx[ofile] += string[1:]
        except:
            idx[ofile] = string[1:]
            
        
#         if (not code.startswith('4e')):
#             code = code[0:4]
#         if outfiles.has_key(code):
#             outfiles[code].write(string)
#         else:
#             ##w: write, a:append, r:read
#             outfiles[code]=codecs.open("%s/%s.idx"%(outdir, code ), 'a+', 'utf-8')
#             outfiles[code].write(string)

def MandokuIndex(file, idxdir='/tmp/index', idlog='logfile.log', left=2, right=2, length=3, collection='test', use_vol=0):
    defs = {'line' : 0, 'noteflag': 0, 'versflag': 0, 'file': file, 'char': 0}
    def setPage(lx):
            r=lx[1:lx.find('>')].split('_')
            defs['ed']=r[0]
            defs['id']=r[1]
            defs['page']=r[-1]
            if defs['line'] == 0:
                ##this means we are looking at the first page?!
                try:
                    idlog.write("%(id)s\t%(file)s\t%(title)s\t%(ed)s\t%(page)s\t"%(defs))
                except:
                    idlog.write("%(id)s\t%(file)s\t%(ed)s\t%(page)s\t"%(defs))
                    
            defs['line']=0
            try:
                defs['page'] = defs['page'][:-1] + tab[defs['page'][-1]]
            except KeyError:
                defs['page'] = re.sub(r'-', '', defs['page'])
                #maybe there is no abcd, so we add a 'a' anyway
                try:
                    test=int(defs['page'])
                    defs['page'] += '1'
                except ValueError:
                    print "error, :", file, line
                    exit
            # if there is a lb-marker at the end of the pb, we want to take it into account        
        
    # outdir = idxdir+'/'+file.split('/')[-1]
    # try:
    #     os.mkdir(outdir)
    # except:
    #     pass
    outdir = idxdir
    pre =  [a for a in ch_re.split(u"　" * left) if len(a) > 0]
    ##the stack for characters.
    chars = []
    ##the stack for inline notes.
    notes = []
    ix = 0
    idlog = codecs.open(idlog, 'a', 'utf-8') 
    f=codecs.open(file, 'r', 'utf-8')
    for line in f:
        if line.startswith(u'校勘記¶'):
            break
        line = img_re.sub('', line)
        line = re.sub(r'/', '', line)
        if "\t" in line:
            line=line.split('\t')[0] + "\n"
        line = line.replace(' ', '')
        if line.startswith('#') or line.startswith(':'):
            if line.startswith('#+'):
                r=line[2:-1].split()
            else:
                r=line[1:-1].split()
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
            defs['versflag']=1
        elif line.startswith('#+END_VERSE'):
            defs['versflag']=0
        elif line.startswith('<pb'):
            ##this assumes a page format of
            ##<pb:SJB_SDZ0001_010002a>
            setPage(line)
            defs['line'] += line.count(u'¶')
            defs['char'] = 0
        elif defs.has_key('page'):
#            defs['line'] += line.count(u'¶')
            line = re.sub(u'[+-~\u3000-\u30FF\uFF00-\uFFEF]', '', line)
            ##remove the footnote markers in the hist files
            line = re.sub(u'〔[一二三四五六七八九０]+〕', '', line)
            ## FIXME: maybe remove punctuation as well?!
            #FIXME: need to consider cases where the lb-marker is not at the end of the line
            ## use_vol is for cases where I want to use the volume, not the id
#            if use_vol == 1:
#                 vol= defs['id'][1:defs['id'].find('n')]
# #                location = "%s:%s_%2.2d"%(vol, defs['page'], defs['line'])
#                 location = "%d:%d:%d"%(int(vol), int(defs['page']), defs['line'])
#             else:
# #                location = "%s:%s_%2.2d"%(defs['id'], defs['page'], defs['line'])
#                 location = "%s:%d:%d"%(defs['id'], int(defs['page']), defs['line'])
#            chars.extend([(a, location) for a in ch_re.split(line[:-1]) if len(a) > 0])
#[2010-02-24T21:43:19+0900] this now works with lb-markers in all positions
            if collection.find('cbeta') > -1:
                vol = defs['id'][1:defs['id'].find('n')]
            elif collection == 'dz':
                vol = defs['page'][0:2]
            for a in ch_re.split(line[:-1]):
                if len(a) > 0:
                    if a == '¶':
                        defs['line'] +=  1
                        defs['char'] = 0
                    else:
                        defs['char'] += 1
#                        print defs
                        if use_vol==1:
                            if collection == 'dz':
                                chars.append((a, "%d:%d:%d:%d"%(int(vol), int(defs['page'][3:]), defs['line'], defs['char'])))
                            else:
                                chars.append((a, "%d:%d:%d:%d"%(int(vol), int(defs['page']), defs['line'], defs['char'])))
                        else:
                            try:
                                chars.append((a, "%s:%d:%d:%d"%(defs['id'], int(defs['page']), defs['line'], defs['char'])))
                            except:
                                #we have section:page pattern
                                #just assume this is skqs
                                try:
                                    chars.append((a, "%d:%s:%d:%d"%(int(defs['id'][3:]), defs['page'], defs['line'], defs['char'])))
                                except:
                                    chars.append((a, "%s:%s:%d:%d"%(defs['id'], defs['page'], defs['line'], defs['char'])))
                                                        
            ## need to disentangle the text now, extract notes into a separate stack if there are some
            ## notes start by '(', end by ')' and have a possible linebreak '/' within.
            ## we will collect the notes until we reach a note-end and then spitting them out
            ## [2010-01-07T13:14:14+0900]
            ## this needs to be done before the text is output!
            ## [2010-02-06T17:27:20+0900] FIXME : ZHSJ texts (hist) have the notes where the section is 'b'
            ## [2010-02-18T20:35:34+0900] and now we want to add the same for verse.. 
            if (line.find('(') > 0):
                for i in range(0, len(chars)-1):
                    #look for the note
                    if chars[i][0] == '(':
                        #through it away!
                        chars.pop(i)
                        defs['noteflag']=1
                        ##make sure the stack is empty
                        notes=[]
                        ix = i
                        break
            while (len(chars) > ix + right + length and defs['noteflag'] == 1):
                #end of note - we will output it immediately, which is not in
                #document order, but does not matter.
                if chars[ix][0] == ')':
                    chars.pop(ix)
                    ix = 0
                    defs['noteflag']=0
                    ##we append left and right padding and then loop through,
                    ##take appropriate slices as we go
                    ##we could of course also append the left and right of chars(ix) to this...
                    npre =  [a for a in ch_re.split(u"　" * (left - 1)) if len(a) > 0]
                    npre.append(u"（")
                    notes.append(u"）")
                    notes.extend([a for a in ch_re.split(u"　" * (right -1)) if len(a) > 0])
                    for i in range(1 , len(notes) - right + 1):
                        PrintToIdxfile (outdir, u"%s,%s\t%s\tn\n"%("".join([a[0] for a in notes[0:length+right]]), "".join(npre),  notes[0][1]), collection)
                        npre.append(notes.pop(0)[0])
                        npre.pop(0)
                else:
                    #we always take the ix-th character
                    notes.append(chars.pop(ix))
            while (len(chars) > ix + right+length and defs['noteflag'] == 0):
                # for the moment, we keep prose and verse in the same loop and only add a flag to verse outpu
                if defs['versflag'] == 1:
                    extra = "\tv"
                else:
                    extra = ""
                PrintToIdxfile(outdir,
                 u"%s,%s\t%s%s\n"%("".join([a[0] for a in chars[0:length+right]]), "".join(pre), chars[0][1], extra), collection)
                pre.append(chars.pop(0)[0])
                pre.pop(0)
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
                PrintToIdxfile(outdir, 
                               u"%s,%s\t%s\n"%("".join([a[0] for a in chars[0:l]]), "".join(pre), chars[0][1]), collection)
                pre.append(chars.pop(0)[0])
                pre.pop(0)
            except:
                print "some error occurred: %s %s"%(line, defs), l, length

    for f in outfiles.keys():
        outfiles[f].close()
        #    os.system('gzip '+ outdir + '/*.idx')
#    defs['page'] = defs['page'][:-1]
    try:
        idlog.write("%(page)s_%(line)2.2d\t%(date)s\n"%(defs))
    except:
        defs['date']= datetime.datetime.now()
        try:
            idlog.write("%(page)s_%(line)2.2d\t%(date)s\n"%(defs))
        except:
            idlog.write("some error occurred\n"%(defs))
        
    idlog.close()

    ##need to write on a per file base.
    for of in idx.keys():
        outfile=codecs.open(of, 'a+', 'utf-8')
        outfile.write(idx[of])
        outfile.close()
        

if __name__ == '__main__':
    try:
        idxdir=sys.argv[3]
    except:
        idxdir='/Users/chris/tmp/index'
    idlog = 'index.log'
    try:
        os.mkdir(idxdir)
    except:
        pass

    try:
        collection = sys.argv[2]
    except:
        
        print "Please provide the collection on the command line"
        exit
#    f=codecs.open(sys.argv[1], 'r', 'utf-8')

    if (collection.find('cbeta') > -1 or collection == 'dz'):
        use_vol = 1
    else:
        use_vol = 0
    MandokuIndex(sys.argv[1], idxdir, idlog, 3, 3, 7, collection, use_vol)
#    MandokuIndex(sys.argv[1], idxdir, idlog, 2, 2, 5, collection, use_vol)
