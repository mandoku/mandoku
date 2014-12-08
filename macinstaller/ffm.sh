#!/bin/sh
# installer for mandoku on mac
# requires python, and git to be installed (this is included since?)
#where are we?
DIR=$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )
#HOME=/tmp/mdhome
#rm -Rf /tmp/mdhome
#rm -Rf /tmp/mdkrp
token=$1
username=$2
mdbase=$3
echo "Mdbase is $mdbase"
mkdir -p $mdbase/user
mkdir -p $mdbase/system/python
mkdir -p $HOME/.emacs.d/md
mkdir -p $HOME/.emacs.d/user
mkdir -p $HOME/.ssh
echo "[Gitlab]" > "$mdbase/user/mandoku-settings.cfg"
echo "Private Token=$token" >> "$mdbase/user/mandoku-settings.cfg"
echo "Username=$username" >> "$mdbase/user/mandoku-settings.cfg"
echo "[Mandoku]" > "$HOME/.emacs.d/md/mandoku.cfg"
echo "basedir = $mdbase" >> "$HOME/.emacs.d/md/mandoku.cfg"

#source=`dirname $DIR`/md
instsrc=$DIR
#this is the key we use in kanripo
krpkey=glkanripo
#this is where the action happens
action=$HOME/.emacs.d/md
# first we need to copy the files to the respective locations:
mkdir -p $action
cp -a $instsrc/md/*.el $action
cp -a $instsrc/python $mdbase/system/
cp -a $instsrc/*.py $action


python $action/postflight.py 
python $action/addsshkey.py
