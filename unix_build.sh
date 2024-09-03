#!/bin/bash

# Note: if you use vscode and want files to build to ./out then
# you need to add the following to your settings.json file:
# 
# "cmake.buildDirectory": "${workspaceFolder}/out"

BUILD_DIRECTORY="out"

# Shove the cmake build files such as the cache files and
# so on into the output directory.
cmake -B $BUILD_DIRECTORY -S .

# Compile the build.
cmake --build $BUILD_DIRECTORY
