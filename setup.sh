#!/bin/bash
BASEDIR=$(dirname $0)
echo "BASEDIR=$BASEDIR"
PUB_KEY_FILE=$BASEDIR/*.pub
SCREENRC_FILE=$BASEDIR/.screenrc
EMACS_INIT=$BASEDIR/.emacs.d
ME=`whoami`
ME_FILE=$BASEDIR/$ME

if [ ! -f $ME_FILE ]; then
    cp -f $SCREENRC_FILE ~/.screenrc
    cat $PUB_KEY_FILE >> ~/.ssh/authorized_keys
    touch $ME_FILE
fi

#rm -rf ~/.emacs.d || true
rm -rf ~/.emacs || true
#cp -R $EMACS_INIT ~/.emacs.d
