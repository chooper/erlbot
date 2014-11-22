#!/bin/bash
set -ex

# install screen if it's not in $PATH
which screen || { apt-get -y -qq update ; apt-get -y -qq install screen ; }

