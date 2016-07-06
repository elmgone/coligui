#!/bin/bash
#
# try to compile and run the web ui only
#
#   - easiest is to use  eg.sh  from  https://github.com/elmgone/elmgode
#   - alternatively it can also be built using elm
#

# if which eg.sh > /dev/null; then
# 	EG=$(which eg.sh)
# fi

# ( cd wui && eg.sh elm make RSync.elm && firefox index.html )

cd wui &&
	elm make RSync.elm &&
	firefox index.html &&
	cd .. &&
	./build.sh

##	$EG elm make RSync.elm &&

#(cd wui && $EG go generate) &&
#	$EG go install -race &&
#	$UPX &&
#	coligui wui
#
## 	$EG upx --ultra-brute /go/bin/coligui &&
##
