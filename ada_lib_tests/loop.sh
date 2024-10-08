#!/bin/bash
export COMMAND=$*
echo COMMAND $COMMAND

# Check if the required COMMAND are provided
if [ $# -lt 1 ]; then
  echo "Usage: $0 <text_to_search> "
  exit 1
fi

text_to_search="Successful Tests:  1"

for (( ; ; ))
do
   $COMMAND
   if grep -q "$text_to_search"  < list-ada_lib.txt; then
     echo "test ran"
   else
     echo "test failed."
     exit 1
   fi
done   # Use grep to search for the text in the string

