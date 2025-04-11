#!/bin/zsh
echo ada_lib/build.sh
export BUILD_MODE=$1
echo BUILD_MODE $BUILD_MODE

../global_build.sh $BUILD_MODE

#pwd
#echo user $USER
#echo home $HOME
#source /Users/$USER/.zshrc
#echo shell $SHELL
#echo path $PATH
#export CFLAG="-M"
#which alr
#which gprbuild
#alr -v build -- -j10 -s -k -gnatE
