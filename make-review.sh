echo "converting 00-Preface/preface.md"
md2review ./00-Preface/preface.md         > ./src/00-Preface.re
echo "converting 01-Roswell/roswell.md"
md2review ./01-Roswell/roswell.md         > ./src/01-Roswell.re
echo "converting 02-Lem/mode.md"
md2review ./02-Lem/mode.md                > ./src/02-Lem.re
echo "converting 04-Make-Project/about-qlot.md"
md2review ./04-Make-Project/about-qlot.md > ./src/04-Make-Project.re
echo "converting 05-Web-Scraping/outline.md"
md2review ./05-Web-Scraping/outline.md    > ./src/05-Web-Scraping.re
echo "converting 09-RaspberryPi/RaspberryPi.md"
md2review ./09-RaspberryPi/RaspberryPi.md > ./src/09-RaspberryPi.re

cd ./src
review-pdfmaker config.yml
