##Problem1
##Part A
##Download the compressed data file to local
wget --output-document data.zip "http://data.un.org/Handlers/DownloadHandler.ashx?DataFilter=itemCode:526&DataMartId=FAO&Format=csv&c=2,3,4,5,6,7&s=countryName:asc,elementCode:asc,year:desc"
##uncompress the file 
unzip data.zip
git mv -k UNdata_Export_20150902_062742765.csv data.csv
##Countries into one file, world to the other.
grep "+" data.csv > regions.csv
grep -v "+" data.csv > countries.csv

##Aviod the comma in the countries names, 
sed "s/, /-/g" countries.csv > countries_aprictos.csv
