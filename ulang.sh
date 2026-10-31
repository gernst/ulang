#!/usr/bin/bash

# ulang.jar next to this script, such that it runs from any directory
java -cp "$(dirname "$0")/ulang.jar" ulang.Main "$@"
