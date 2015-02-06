#!/bin/bash

echo ""
echo "Formatting curl commands..."
cp edges-full.txt tmp-full.tmp
sed --in-place --expression='s/^ *//' tmp-full.tmp
sed --in-place --expression='s/ *$//' tmp-full.tmp
sed --in-place --expression='s!  *!/!' tmp-full.tmp
sed --in-place --expression='s!^!curl localhost:3000/add-edge/!' tmp-full.tmp

echo "Executing..."
curl localhost:3000/reset       # clear out system state
bash tmp-full.tmp >& /dev/null  # add all graph nodes

echo "  done."
echo ""
