#!/bin/bash
BASEDIR=$(dirname $0)
echo "BASEDIR=$BASEDIR"
PUB_KEY_FILE=$BASEDIR/*.pub
SCREENRC_FILE=$BASEDIR/.screenrc
EMACS_INIT=$BASEDIR/.emacs.d
ME=`whoami`
ME_FILE=$BASEDIR/$ME
PKGS="emacs screen tree git"

if [ ! -f $ME_FILE ]; then
    cp -f $SCREENRC_FILE ~/.screenrc
    cat $PUB_KEY_FILE >> ~/.ssh/authorized_keys
    touch $ME_FILE
fi

#rm -rf ~/.emacs.d || true
rm -rf ~/.emacs || true
#cp -R $EMACS_INIT ~/.emacs.d

if [ "$ME" == "root" ]; then
    if hash yum 2>/dev/null; then
	yum -y install $PKGS
    elif hash apt-get 2>/dev/null; then
	apt-get -y install $PKGS
    elif hash zypper 2>/dev/null; then
	zypper -y install $PKGS
    else
	echo "unknown package manager"
    fi 
else
    echo "not installing anything"
fi
