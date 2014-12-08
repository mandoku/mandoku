#!/bin/sh
# installer for mandoku on mac
# requires python, and git to be installed (this is included since?)
#where are we?
DIR=$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )
HOME=/tmp/mdhome
rm -Rf /tmp/mdhome
token=$1
mdbase=$2
mkdir -p $mdbase/user
mkdir -p $HOME/.emacs.d/md/
echo "[Gitlab]" > "$mdbase/user/mandoku-settings.cfg"
echo "Priave Token=$token" >> "$mdbase/user/mandoku-settings.cfg"
echo "[Mandoku]" > "$HOME/.emacs.d/md/mandoku.cfg"
echo "basedir=$mdbase" >> "$HOME/.emacs.d/md/mandoku.cfg"

source=/Users/chris/mandoku/
instsrc=$source/macinstaller
target=/tmp/krp
HOME=/tmp/mdtest
#this is the key we use in kanripo
krpkey=glkanripo
#this is where the action happens
action=$HOME/.emacs.d/md
# first we need to copy the files to the respective locations:
mkdir -p $action
cp -a $source/md/*.el $action
cp -a $instsrc/*.py $action
echo "[Mandoku]" > $action/mandoku.cfg
echo "basedir = $target" >> $action/mandoku.cfg

python $action/postflight.py
python $action/addsshkey.py
