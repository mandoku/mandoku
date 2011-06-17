#!/bin/usr/env python  -*- encoding: utf-8 -*-
## split the files produced by cb2mandoku.xsl by JUAN
import codecs, sys, os, re
sys.stdout = codecs.getwriter('utf8')(sys.stdout)
## the last part of the path, but without the extension will be considered the text id
fname=sys.argv[1]
fileid=fname.split('/')[-1][:-4]
#give parameter 'test' to see where the split will occur.
#we will then list the number of the juan (potentially calculated) and the following line

try:
    action=sys.argv[2]
except:
    action='split'
    
inf=codecs.open(fname, 'r', 'utf-8')

# if action=='split':
#     try:
#         os.mkdir(fileid)
#     except:
#         pass

#print "#+LINK: dz file:~/db/text/dz/%s"
print "#%s"%(fileid)
header=''
headerflag=0
jcnt = 0
baseed= ''
lastpb=''
title=''
l=0
newfile=''
toc=codecs.open('%s.org'%(fileid), 'w', 'utf-8')
if action != 'split':
    sys.stdout.write ("* [[dz:%s/%s.org][%s "%( fileid[:6], fileid, fileid))
for line in inf:
#    print headerflag
#    print line.find(':JUAN')
#    print line
    if line.find('TITLE') > -1:
        try:
            title=line[:-1].split(':')[1].strip()
            if action != 'split':
                print "%s]]"%(title)
        except:
            print "]]"
    if line.find('BASEEDITION') > -1:
        try:
            baseed=line[line.find('BASEEDITION'):-1].split()[1].strip()
        except:
            print "no valid baseedition found", line
    if line.find('<pb:') > -1:
        pb=line[line.find('pb:')+3 : line.find('>', line.find('pb:') + 1)].split('_')
        if pb[0] == baseed:
            lastpb = pb
            l = 0
    if line.find('JUAN') > -1:
        if headerflag == 0:
            toc.write(header.replace('mandoku-view', 'org-mode'))
#            toc.write(':END:\n')
        jcnt += 1
        try:
            juan = int(line.split(': ')[1].strip())
            jcnt = juan
        except:
            #we do not have a juan number, use the count
            juan = jcnt
        headerflag=1
        h1=re.sub('\n\n', '\n', header)
        header = re.sub('\n<pb[^\n]*', '', h1)
        try:
            of.close()
            print "\t%s\t%s"%( "_".join(lastpb), l)
        except:
            pass
        if action == 'split':
            newfile="%s-%3.3d.txt"%(fileid, juan)
#            sys.stdout.write(newfile.split('/')[-1])
            sys.stdout.write(newfile)
            of = codecs.open(newfile, 'w', 'utf-8')
            of.write(header)
            of.write('#+PROPERTY: LASTPB <pb:%s> %s\n'%("_".join(lastpb), "".join([u'¶' for a in range(1, l)])))
            ## last pb seen
        else:
            nl = inf.next()
            print '#+PROPERTY: JUAN %d\t%s'%(juan, nl[:-1])
    #l is the number of the current line
    l+=line.count(u'¶')
#    l+=len(re.findall(u'¶', line))
    if headerflag == 0:
        header += line
    else:
        if action == 'split':
            of.write(line)
    if line.startswith('*'):
        s= line[:-1].split(' ')
        hd = "".join(s[1:])
        try:
            pg = "::%s%d"%(lastpb[2], l)
        except:
            pg= ''
        toc.write("%s [[file:%s%s][%s]]\n" %(s[0], newfile.split('/')[-1], pg, hd))
print "\t%s\t%s"%( "_".join(lastpb), l)

