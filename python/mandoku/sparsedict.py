#!/usr/bin/env python    -*- coding: utf-8 -*-
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
implements a sparse dictionary, that allows to access to existing keys from non-existing larger keys
:author:       Christian Wittern (cwittern[at]gmail.com)
:organization: Mandoku project (http://www.mandoku.org)
:license:      BSD License
"""
#TODO: might implement error checking, it supports only int keys, but does not check.
#could try to optimize the search for the 'closest' key, but how?

class SparseDict(dict):
#    def __init__(self, *args, **kwargs):
#        self.update(*args, **kwargs)

    def __getitem__(self, key):
        # optional processing here
        n = key
        s = str(n)
        while(not self.has_key(s) and n > 0):
            n -=1
            s = str(n)
        if self.has_key(s):
            return super(SparseDict, self).__getitem__(s)
        else:
            return None

if __name__ == '__main__':

    d = SparseDict()
    d[1] = 'A'
    d[10] = 'B'
    d[12] = 'C'

    print 5, d[5]
    print 11, d[11]
    print 12, d[12]
    print 'a', d['a']
