#!/bin/bash

set -euo pipefail

mkdir .working

cat src/uk/co/tombooth/pollock.cljs | python tools/clojure-to-markdown.py > .working/out.md
pandoc --mathjax --template=tools/pandoc-template.html -s .working/out.md -o .working/index.html
cp -r web/js .working/

git checkout gh-pages

cp .working/index.html index.html
cp -r .working/js .

git add index.html
git add js
git commit -m "Update homepage" || true

git checkout master

rm -rf .working
