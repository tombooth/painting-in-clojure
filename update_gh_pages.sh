#!/bin/bash

set -euo pipefail

mkdir .working

cat src/uk/co/tombooth/pollock.cljs | python tools/clojure-to-markdown.py > .working/out.md
pandoc --mathjax --template=tools/pandoc-template.html -s .working/out.md | sed -E 's/<span class="kw">([^<]*)<\/span>/<a href="http:\/\/clojuredocs.org\/clojure_core\/clojure.core\/\1" target="_blank" class="kw">\1<\/a>/g' > .working/index.html
cp -r web/js .working/

mkdir .working/slides
cp -r src/slides/img .working/slides/
cp -r src/slides/revealjs .working/slides/
pandoc --template=tools/pandoc-template-slides.html -t revealjs -s src/slides/index.md -o .working/slides/index.html

git checkout gh-pages

cp .working/index.html index.html
cp -r .working/slides .
cp -r .working/js .

git add index.html
git add js
git add slides
git commit -m "Update homepage" || true

git checkout master

rm -rf .working
