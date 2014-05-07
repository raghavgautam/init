#!/bin/bash
BASEDIR=$(dirname $0)
echo "BASEDIR=$BASEDIR"
PUB_KEY_FILE=$BASEDIR/*.pub
SCREENRC_FILE=$BASEDIR/.screenrc
ME=`whoami`

if [ -e $SCREENRC_FILE ]; then
    mv $SCREENRC_FILE ~/.screenrc    
    cat $PUB_KEY_FILE >> ~/.ssh/authorized_keys*
fi

if [ $ME="root" ]; then
    yum -f install emacs screen tree git
else
    echo "not installing anything"
fi
