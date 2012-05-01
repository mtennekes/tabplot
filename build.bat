@echo off


echo Removing building information...
rm -rf output

echo Create palletes...
R -q -f build/createpallete.R

echo Build documentation...
R -q -f build/roxygen.R

md output
cd output
R CMD build --resave-data ../pkg
FOR %%1 in (*.tar.gz) DO R CMD check %%1
