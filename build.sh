#!/bin/bash
#
# build the whole selfcontained binary:
#   - easiest is to use  eg.sh  from  https://github.com/elmgone/elmgode
#   - alternatively it can also be built using elm, go, ego
#

if which eg.sh > /dev/null; then
	EG=$(which eg.sh)
fi

if echo $* | grep upx > /dev/null ; then
	UPX="$EG upx --ultra-brute /go/bin/coligui"
else
	UPX="echo running coligui ..."
fi

(cd wui && $EG go generate) &&
	$EG go install -race &&
	$UPX &&
	coligui wui

## 	$EG upx --ultra-brute /go/bin/coligui &&
##
