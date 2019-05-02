#!/bin/sh
for i in $(curl -s -L -u "pouar:$bintrayapikey" https://api.bintray.com/packages/pouar/yadfa-chocolatey/yadfa|jq -r .versions[1:][]);do curl -s -X DELETE -L -u "pouar:$bintrayapikey" "https://api.bintray.com/packages/pouar/yadfa-chocolatey/yadfa/versions/$i";done
