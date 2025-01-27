#!/bin/zsh
echo build ada_lib tests parameters $*
#pwd
if [ -z "$2" ]; then
  echo "local build"
else
  echo "remote build"
  cd $2
fi

#ls -l ../../*.sh
../../remote_build.sh ada_lib/ada_lib_tests test_ada_lib
