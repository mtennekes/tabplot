@echo off


echo Removing building information...
rm -rf output

echo Create palletes...
R -q -f build/createpalette.R

echo Build vignette...
R -q -f build/vignette.R

echo Generate documentation...
Rscript roxygen.R

md output
cd output
R CMD build ../pkg
FOR %%1 in (*.tar.gz) DO R CMD check %%1

cd ..