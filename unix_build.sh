#!/bin/bash

# Note: if you use vscode and want files to build to ./out then
# you need to add the following to your settings.json file:
#
# "cmake.buildDirectory": "${workspaceFolder}/out"

BUILD_DIRECTORY="out"

SCRIPT=$(realpath "$0")
SCRIPTPATH=$(dirname "$SCRIPT")

mkdir -p "$BUILD_DIRECTORY"

# Shove the cmake build files such as the cache files and
# so on into the output directory.
cmake -B "$SCRIPTPATH/$BUILD_DIRECTORY" -S $SCRIPTPATH

# Compile the build.
cmake --build "$SCRIPTPATH/$BUILD_DIRECTORY" -j8
