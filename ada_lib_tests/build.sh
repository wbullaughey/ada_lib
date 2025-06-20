source ~/.zshrc
export WHICH=$1
export DIRECTORY=`pwd`
echo "options $0 WHICH $WHICH \
   BUILD_PROFILE $ADA_APPLICATION_PROFILE \
   ADA_OS_INCLUDE $ADA_OS_INCLUDE \
   DIRECTORY $DIRECTORY"

../../global_build.sh $WHICH $ADA_APPLICATION_PROFILE program
