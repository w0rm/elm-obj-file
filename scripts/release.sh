#!/bin/bash
set -euxo pipefail

rm -rf release || exit 0;

elm bump

version=$(grep -m1 version elm.json | awk -F: '{ print $2 }' | sed 's/[", ]//g')

git commit -a -m "Bump to $version"
git push

cleanup=".github examples tests benchmarks"
last_commit=$(git rev-parse HEAD)

git clone git@github.com:w0rm/elm-obj-file.git release
(
  cd release
  git checkout $last_commit
  git rm -rf --ignore-unmatch $cleanup
  sed -i.bak "s+https://unsoundscapes.com/elm-obj-file/+https://unsoundscapes.com/elm-obj-file/$version/+g" README.md
  rm README.md.bak
  git add README.md
  git commit -m "Release $version"
  git tag -a $version -m "Release $version"
  git push origin $version
  elm publish
)

# publish the examples
./scripts/gh-pages.sh $version
