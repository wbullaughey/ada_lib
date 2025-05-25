#!/bin/bash
export BUILD_MODE=execute
export UNIT_TEST=TRUE
export OUTPUT=list-test_ada_lib.txt
export PROGRAM=bin/test_ada_lib

rm $OUTPUT
echo $OUTPUT deleted 2>&1 | tee $OUTPUT
#echo "arguments $*"  2>&1| tee $OUTPUT

case $1 in

   "gdb")
      shift 1
      export GDB=gdb
      ;;

   *)
      ;;

esac
echo 1st parameter $1 2>&1| tee -a $OUTPUT

case $1 in

   "hide")
      shift 1
      export DISPLAY=false
      ;;

   "ignore")
      shift 1
      export DISPLAY=ignore
      ;;

   *)
      export DISPLAY=true
      ;;

esac

export DATABASE=$1  # local,remote,connect,none
export KILL=true
#export VERBOSE="-v"

echo check database  "$DATABASE"  2>&1| tee -a $OUTPUT
case "$DATABASE" in

   "help")
      shift 1
      $PROGRAM -h $* | tee -a $OUTPUT
      exit
      ;;

   "help_test")
      echo Help Test 2>&1| tee -a $OUTPUT
      export PROGRAM=bin/help_test
      $PROGRAM \
      -h -l -P -r -v -x -@c -@d -@i -@l -@m -@p -@P -@S -@t -@u -@x \
      -a abcCehiIlmMoOpPrRsStT@c@d@D@e@E@l@o@s@t \
      -@D adt \
      -e routine \
      -g aego \
      -G amo \
      -L path \
      -R path \
      -s suite \
      -t acCdhilmorRsStT@d@T@t \
      -T aceElt \
      -u user  \
      -U aAglprstT \
      2>&1 | tee -a $OUTPUT
      exit
      ;;

   "connect")
      export DATABASE_OPTION="-l"
      export KILL=false
      ;;

   "local")
      export DATABASE_OPTION="-l -L /Users/wayne/bin/dbdaemon"
      ;;

   "remote")
      export DATABASE_OPTION="-r localhost -R /home/wayne/bin/dbdaemon -u wayne"
      ;;

   "none")
      ;;

   "suites")
      shift 1
      echo list suites 2>&1| tee -a $OUTPUT
      export COMMAND="$PROGRAM -@l $*"
      echo "command: $COMMAND"  | tee -a $OUTPUT
      $COMMAND 2>&1 | tee -a $OUTPUT
      exit
      ;;

   "")
      shift 1
      echo no database option provided 2>&1| tee -a $OUTPUT
      exit
      ;;

   *)
      echo unrecognize database option \"$DATABASE\" allowed: local,remote,none 2>&1| tee -a $OUTPUT
      exit
      ;;

esac

export SUITE=$2     # mwd - Main_Window_with_DBDaemon
                    # mwn - Main_Window_without_DBDaemon
                    # wrd - Widget_Root_With_DBDaemon
                    # wrn - Widget_Root_Without_DBDaemon
                    # all - all swites
export ROUTINE=$3   # all or test routine name
echo DATABASE $DATABASE 2>&1| tee -a $OUTPUT
echo SUITE $SUITE 2>&1| tee -a $OUTPUT
echo routine $ROUTINE 2>&1| tee -a $OUTPUT

shift 3


case "$SUITE" in

   all)
      ;;

   "")
      echo "missing suite"  2>&1| tee -a $OUTPUT
      exit;
      ;;

   *)
       export SUITE_OPTION="-s $SUITE"
       ;;

esac

case "$ROUTINE" in

   all)
      ;;

   -*)
      echo missing routine 2>&1| tee -a $OUTPUT
      exit;
      ;;

   "")
      echo missing routine 2>&1| tee -a $OUTPUT
      exit;
      ;;

   *)
      echo routine $ROUTINE 2>&1| tee -a $OUTPUT
      export ROUTINE_OPTION="-e $ROUTINE"
      ;;

esac

ps ax | grep dbdaemon
case "$DATABASE" in
   true)
       killall -9 dbdaemon
       ;;

   false)
       ;;

   *)
       echo kill not set 2>&1| tee -a $OUTPUT
       ;;

esac
#ps ax | grep dbdaemon
echo DISPLAY $DISPLAY 2>&1| tee -a $OUTPUT
export COMMAND="$GDB $PROGRAM $* $DATABASE_OPTION $SUITE_OPTION $ROUTINE_OPTION  -p 2300" # -S 1
echo "command: $COMMAND"  | tee -a $OUTPUT

case "$DISPLAY" in

   true)
      $COMMAND 2>&1 | tee -a $OUTPUT
      ;;

   false)
      $COMMAND 2>&1| tee -a $OUTPUT
      ;;

   ignore)
      $COMMAND 2>&1| tee -a $OUTPUT
      ;;

esac

sleep 1
