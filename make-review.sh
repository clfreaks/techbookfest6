#!/bin/sh

echo "converting 00-Preface/preface.md"
md2review ./00-Preface/preface.md         > ./src/00-Preface.re
echo "converting 01-Roswell/roswell.md"
md2review ./01-Roswell/roswell.md         > ./src/01-Roswell.re
echo "converting 02-Lem/lem.md"
md2review ./02-Lem/lem.md                 > ./src/02-Lem.re
echo "converting 02-Lem/lem-short.md"
md2review ./02-Lem/lem-short.md           > ./src/02-Lem-short.re
echo "converting 03-Library-Investigation/library-investigation.md"
md2review ./03-Library-Investigation/library-investigation.md > ./src/03-library-investigation.re
echo "converting 04-Make-Project/make-project.md"
md2review ./04-Make-Project/make-project.md > ./src/04-Make-Project.re
echo "converting 05-Web-Scraping/outline.md"
md2review --render-link-in-footnote ./05-Web-Scraping/outline.md    > ./src/05-Web-Scraping.re
echo "converting 06-Test/rove.md"
md2review --render-link-in-footnote ./06-Test/rove.md               > ./src/06-Test.re
echo "converting 08-Deploy/deploy.md"
md2review --render-link-in-footnote ./08-Deploy/deploy.md           > ./src/08-Deploy.re
echo "converting 09-RaspberryPi/RaspberryPi.md"
md2review ./09-RaspberryPi/RaspberryPi.md > ./src/09-RaspberryPi.re
echo "converting 99-Postface/postface.md"
md2review ./99-Postface/postface.md > ./src/99-Postface.re

cd ./src
review-pdfmaker config.yml
