#!/bin/bash
#
# generate wui.ego out of index.html
#

if which eg.sh > /dev/null; then
	EG=$(which eg.sh)
fi

$EG elm package install --yes                                    || exit 10
$EG elm make --yes "$@"                                          || exit 20
# echo "<%! func WriteWuiHtml( w io.Writer ) error %>"  >  wui.ego || exit 30
cat index.html                                        >  wui.ego || exit 30
echo                                                 >>  wui.ego || exit 40
echo "<%! func WriteWuiHtml( w io.Writer ) error %>" >>  wui.ego || exit 50
$EG ego                                                          || exit 60
