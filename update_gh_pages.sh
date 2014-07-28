#!/bin/bash

set -euo pipefail

cat src/uk/co/tombooth/pollock.cljs | python tools/clojure-to-markdown.py > out.md
pandoc --mathjax --template=tools/pandoc-template.html -s out.md -o index.html

git checkout gh-pages

git add index.html
git commit -m "Update homepage" || true

git checkout master

rm out.md
