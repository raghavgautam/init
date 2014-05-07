#!/bin/bash
BASEDIR=$(dirname $0)
PUB_KEY_FILE=$BASEDIR/*.pub
SCREENRC_FILE=$BASEDIR/.screenrc
ME=`whoami`

cat PUB_KEY_FILE >> ~/.ssh/authorized_keys*
mv $SCREENRC_FILE ~/.screenrc

if [ $ME="root" ]; then
    yum -f install emacs screen tree git
else

fi
