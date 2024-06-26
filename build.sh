#!/bin/zsh
echo ada_lib/build.sh
pwd
echo user $USER
echo home $HOME
source /Users/$USER/.zshrc
echo shell $SHELL
echo path $PATH
which alr
which gprbuild
alr build
