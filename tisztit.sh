#!/bin/bash

mv .git/config config

rm -rf .git

sh ./gitbeal.sh

mv config .git/config

git add --all .

git commit -m "monthly cleanup"

git push origin master --force

echo "################################################################"
echo "###################    cleanup  Done      ######################"
echo "################################################################"

