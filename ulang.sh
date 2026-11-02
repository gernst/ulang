#!/usr/bin/bash

# ulang.jar next to this script, such that it runs from any directory
java -Dulang.lib="$(dirname "$0")/ulang/resources" -Dulang.self="$(dirname "$0")/self" -cp "$(dirname "$0")/ulang.jar" ulang.Main "$@"
