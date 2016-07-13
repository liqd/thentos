#!/bin/bash

set -e
cd `dirname $0`/..
export NEW_VERSION="$1"

test -z "$NEW_VERSION" && ( echo "Please provide a version number to move to."; exit 1 )
git tag -l | grep -q v$NEW_VERSION && ( echo "git tag for $NEW_VERSION already exists."; exit 1 )

for f in thentos-{cookie-session,core,tests,adhocracy}/thentos-*.cabal; do
    perl -i -pe 's/^(version:\s*).*$/${1}'"$NEW_VERSION"'/' $f
    perl -i -pe 's/^(\s*, thentos-[-a-z]+ ).*$/${1}=='"$NEW_VERSION"'/' $f
    git add $f
done

git commit -m 'Release '"$NEW_VERSION"'.'
git tag v$NEW_VERSION

echo "created new commit and tag."
echo "to inspect:  git show; git tag -l"
echo "to push:     git push; git push --tags"
