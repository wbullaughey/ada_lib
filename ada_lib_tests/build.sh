#!/bin/zsh
echo build ada_lib tests parameters $*
export QUICK_BUILD=$1
echo QUICK_BUILD $QUICK_BUILD
export BUILD_PATH=`pwd`
echo BUILD_PATH $BUILD_PATH

../../remote_build.sh $BUILD_PATH ada_lib/ada_lib_tests test_ada_lib $QUICK_BUILD
