#!/bin/bash

apt-get update -y
sudo apt-get install -y postgresql postgresql-contrib

cd $SRC_DIR

make build
