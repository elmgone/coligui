#!/bin/bash
#
# generate wui.ego out of index.html
#

# set -x

# TITLE="RSync - powered by (c) CoLiGUI"
# TITLE="RSync - via CoLiGUI"
TITLE="RSync"

elm package install --yes                                        || exit 10
elm make --yes "$@"                                              || exit 20

cat index.html | sed -e \
     "s:<title>Main</title>:<title>$TITLE</title>:"  >   wui.ego || exit 30

echo                                                 >>  wui.ego || exit 40
echo "<%! func WriteWuiHtml( w io.Writer ) error %>" >>  wui.ego || exit 50
ego                                                              || exit 60
