#!/bin/bash

sed '/# --/q' README.org > tmpReadme

for f in $(find . -name "*.asd"); 
do 
    desc=$(grep description $f | grep -o \".*\" | sed 's,",,g'); 
    echo - [[$(dirname $f)/README.org][$(basename $f .asd)]] :: $desc >> tmpReadme
done
awk '/util/ && !x {print "** Utilities"; x=1} 1' tmpReadme > tmp; mv tmp tmpReadme
awk '/media/ && !x {print "** Media"; x=1} 1' tmpReadme > tmp; mv tmp tmpReadme
awk '/minor-mode/ && !x {print "** Minor Modes"; x=1} 1' tmpReadme > tmp; mv tmp tmpReadme
awk '/modeline/ && !x {print "** Modeline"; x=1} 1' tmpReadme > tmp; mv tmp tmpReadme
mv tmpReadme README.org
