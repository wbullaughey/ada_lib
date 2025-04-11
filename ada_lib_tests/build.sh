source ~/.zshrc
export BUILD_MODE=$1
echo BUILD_MODE $BUILD_MODE

case "$BUILD_MODE" in
   "")
      alr -v build -- -j10 -s -k -gnatE -v -XBUILD_MODE=execute
      ;;

   "help_test")
      alr -v build -- -j10 -s -k -gnatE -XBUILD_MODE=help_test
      ;;

   "all")
      ../../global_build.sh
      ;;

esac

