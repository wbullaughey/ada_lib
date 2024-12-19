#!/bin/zsh
echo ada_lib/build.sh
pwd
echo user $USER
echo home $HOME
source /Users/$USER/.zshrc
echo shell $SHELL
echo path $PATH
export CFLAG="-M"
which alr
which gprbuild
alr -v build -- -j10 -s -k -gnatE
