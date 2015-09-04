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

## Find five most land-used countries in 2005
grep -i "Area" countries_aprictos.csv | grep "\"2005\"" | sed 's/"//g' | sort -t',' -k6 -n | tail -5

## Automate the analysis for other years
function automate(){
for ((i=1965;i<=2015;i=i+5))
do
    grep -i "Area" countries_aprictos.csv | grep "\"echo $i\"" | sed 's/"//g' | sort -t',' -k6 -n | tail -5 > file$i.txt
done
}
automate(5)