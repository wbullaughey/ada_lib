#!/bin/zsh
echo build ada_lib tests parameters $*
#pwd
if [ -z "$2" ]; then
  echo "initial build"
else
  echo "remote build"
  cd $2
fi

#ls -l ../../*.sh
../../remote_build.sh alr_environment ada_lib/ada_lib_tests test_ada_lib
