#!/bin/sh
for i in $(curl -L -u "pouar:$bintrayapikey" https://api.bintray.com/packages/pouar/yadfa-chocolatey/yadfa|jq -r .versions[1:][]);do curl -X DELETE -L -u "pouar:$bintrayapikey" "https://api.bintray.com/packages/pouar/yadfa-chocolatey/yadfa/versions/$i";done
