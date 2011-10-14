#!/bin/bash


echo "Removing building information..."
rm -rf output

R -q -f build/createpallete.R
echo "Generate documentation..."
R -q -f build/roxygen.R

mkdir output
cd output
R CMD build ../pkg
for x in *.tar.gz 
do 
    R CMD check $x
done
