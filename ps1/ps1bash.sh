##Problem1
##Part A
##Download the compressed data file to the local directory
wget --output-document data.zip "http://data.un.org/Handlers/DownloadHandler.ashx?DataFilter=itemCode:526&DataMartId=FAO&Format=csv&c=2,3,4,5,6,7&s=countryName:asc,elementCode:asc,year:desc"
##uncompress the file 
unzip -c data.zip > data.csv
##Countries into one file, world to the other.
grep "+" data.csv > regions.csv
grep -v "+" data.csv > countries.csv

##Aviod the comma in the countries names for the convience of splits of different columns in future
sed "s/, /-/g" countries.csv > countries_aprictos.csv

## Find five most land-used countries in 2005
grep -i "Area" countries_aprictos.csv | grep "\"2005\"" | sed 's/"//g' | sort -t',' -k6 -n -r| head -5 | cut -d"," -f1,6

## Automate the analysis for other years, the default interval of years is 10.
function automate(){
for ((i=1965;i<=2005;i=i+10))
do
	printf "This is the rank for year $i \n"
    grep -i "Area" countries_aprictos.csv | grep "\"$i\"" | sed 's/"//g' | sort -t',' -k6 -n -r | head -5 | cut -d"," -f1,6
    printf "\n"
done
}
automate

##Part B, and x should be the input code
function httpcode(){
	echo -n "what is your code for the Agriculture Product?" 
	read x
	wget --output-document data$x.zip "http://data.un.org/Handlers/DownloadHandler.ashx?DataFilter=itemCode:$x&DataMartId=FAO&Format=csv&c=2,3,4,5,6,7&s=countryName:asc,elementCode:asc,year:desc" 
	unzip -c data$x.zip | less 
}
httpcode

##Part C Input the product name instead of inputing the product code.
#Download the html file
wget --output-document codename.html "http://faostat.fao.org/site/384/default.aspx"
grep "</td><td>" codename.html | sed 's/td//g' | sed -e 's/<.><>/-/g' | cut -d'-' -f2,4 | sort -u -t'-' > codename.csv
function nametocode(){
	x=$(grep $1$ codename.csv | cut -d'-' -f1)
	wget --output-document data$x.zip "http://data.un.org/Handlers/DownloadHandler.ashx?DataFilter=itemCode:$x&DataMartId=FAO&Format=csv&c=2,3,4,5,6,7&s=countryName:asc,elementCode:asc,year:desc" 
	unzip -c data$x.zip | less
}

nametocode Wheat

##Problem2
#First to download the html file, and name it climate.html
wget --output-document climate.html "http://www1.ncdc.noaa.gov/pub/data/ghcn/daily/" 

##Get the name of txt files
txtnames=$(grep .txt climate.html | sed 's/.*href="//g' | sed 's/txt">.*/txt/g')

for i in $txtnames;
do 
	printf "\n You are downloading the text file $i \n"
	wget "http://www1.ncdc.noaa.gov/pub/data/ghcn/daily/$i"
done


