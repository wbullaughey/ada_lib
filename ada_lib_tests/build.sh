source ~/.zshrc
export BUILD_MODE=$1
export DIRECTORY=`pwd`
echo "options $0 BUILD_MODE $BUILD_MODE \
   BUILD_PROFILE $ADA_APPLICATION_PROFILE \
   ADA_OS_INCLUDE $ADA_OS_INCLUDE \
   DIRECTORY $DIRECTORY"

../../global_build.sh $BUILD_MODE ada_lib_tests $ADA_APPLICATION_PROFILE library
