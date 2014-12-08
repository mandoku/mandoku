#!/bin/sh
#where are we?
DIR=$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )

token=$1
mdbase=/tmp/test
mkdir -p $mdbase/user
mkdir -p $HOME/.emacs.d/md/
echo "[Gitlab]" > "$mdbase/user/mandoku-settings.cfg"
echo "Priave Token=$token" >> "$mdbase/user/mandoku-settings.cfg"
echo "[Mandoku]" > "$HOME/.emacs.d/md/mandoku.cfg"
echo "basedir=$2" >> "$HOME/.emacs.d/md/mandoku.cfg"

