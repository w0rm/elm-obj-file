#!/bin/bash
set -e

rm -rf gh-pages || exit 0;

# call with version to publish the version
if [ ! -z "$1" ]
then
   version="/$1"
fi

git clone git@github.com:w0rm/elm-obj-file.git gh-pages
cd gh-pages
git checkout gh-pages

cd ../examples/src
for example in *.elm; do
  # rename CamelCase to snake-case
  lower=$( echo "${example%.*}" \
         | sed 's/\(.\)\([A-Z]\)/\1-\2/g' \
         | tr '[:upper:]' '[:lower:]' \
         )
  mkdir -p ../../gh-pages$version/examples/$lower
  elm make $example --optimize --output ../../gh-pages$version/examples/$lower/index.html
done
cp Pod.png Pod.obj.txt ../../gh-pages$version/examples/pod
cp ../pod.png ../../gh-pages$version/examples

cd ../../gh-pages
git add .
git commit -m "Deploying $version to GH Pages"
git push
