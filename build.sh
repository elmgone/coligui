#!/bin/bash
#
# build the whole selfcontained binary:
#   - easiest is to use  eg.sh  from  https://github.com/elmgone/elmgode
#   - alternatively it can also be built using elm, go, ego
#

#if which eg > /dev/null; then
#	EG=$(which eg)
#fi

if echo $* | grep upx > /dev/null ; then
	# UPX="$EG upx --ultra-brute /go/bin/coligui"
	UPX="$EG upx -9 /go/bin/coligui"
	if echo $* | grep -e brute -e ultra > /dev/null ; then
		UPX="$EG upx --ultra-brute /go/bin/coligui"
	fi
else
	UPX="echo running coligui ..."
fi

if echo $* | grep run > /dev/null ; then
##    RUN="( coligui wui & )"
    RUN="coligui wui"
else
	RUN="echo ok."
fi

(cd wui && $EG go generate) &&
	$EG go install -race &&
	$UPX &&
    LOGXI=* $RUN

##	coligui wui
## 	$EG upx --ultra-brute /go/bin/coligui &&
##
