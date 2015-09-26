library(XML)
library(RCurl)
library(curl)
library(stringr)
##A With Vice President
# transcript_url<-"http://www.debates.org/index.php?page=debate-transcripts"
# transcript_html<-getURLContent(transcript_url)
# alllinks<-unique(getHTMLLinks(transcript_url))
# debate_html<-strsplit(transcript_html,"<li>")[[1]]
# debate_html<-debate_html[grep("Transcript? </a>",debate_html)]
# debate_select<-debate_html[grep("1996|2000|2004|2008|2012",debate_html)]
# select_html<-str_replace_all(debate_select,".*http","http")
# select_html<-str_replace_all(select_html,"\\\">.*","")
# select_html<-str_replace_all(select_html,"\\n.*","")
# #print(debate_html)
# str_extract(debate_select,"(September|October) \\d+(,|\\.) \\d{4}")

##A
new_html<-htmlParse("http://www.debates.org/index.php?page=debate-transcripts")
##First observe that the text part of the website starts from <p>
listofnodes<-getNodeSet(new_html,"//p//a")
##toString.XMLNode transforms the list element to string so that
## it could be manipulated using regular expressions
stringnode<-unlist(lapply(listofnodes,toString.XMLNode))
selectyear<-stringnode[grep("1996|2000|2004|2008|2012",stringnode)]
first_html<-selectyear[grep("First",selectyear)]
first_html<-str_replace_all(first_html,".*http","http")
first_html<-str_replace_all(first_html,". title.*","")
Dateinfo<-selectyear[grep("First",selectyear)]
Dateinfo<-as.data.frame.Date(str_extract(Dateinfo,"(September|October) \\d+, \\d{4}"))
Speechdataframe<-cbind(as.data.frame(first_html),Dateinfo)
colnames(Speechdataframe)<-c("first_URL","Date")

##B
speech_data<-htmlParse(Speechdataframe[1,1])
## By inspecting the Xpath Code of the element in Chrome.
text_data<-xpathSApply(speech_data,"//p/text()",xmlValue)
##Good Look
cat(paste(text_data,collapse="\n\n"))

text_data<-as.list(text_data)
# z <- lapply(text_data
#        ,function(x){y <-as.character(str_match(string=x, pattern="^[A-Z]+:"))
#                     return(y)})
# z
speakernames <- as.list(str_replace(as.list(str_match(text_data,"^[A-Z]+:")),":",replacement=""))
# speakernames<-as.list(str_match(text_data,"^[A-Z]+:"))
finalframe<-cbind(speakernames,text_data)

speakerposition<-1
for (i in 2:nrow(finalframe)){
  if(is.na(finalframe[i,1])==FALSE){
    speakerposition<-i
  }
  if(is.na(finalframe[i,1])){
    finalframe[speakerposition,2]<-paste(finalframe[speakerposition,2],finalframe[i,2],collapse="\n\n")
  }
}
##Eliminate those name columns with NA, because the cotent has been concatenated. 
finalframe<-finalframe[is.na(finalframe[,1])==FALSE,]

##This step strip out the useless Laughter Applause and crosstalk tags from the speech.
## Also strip out the speaker name at the very top of each paragraph
finalframe[,2]<-str_replace_all(finalframe[,2],"\\(LAUGHTER\\)|\\(APPLAUSE\\)|\\(CROSSTALK\\)|^[A-Z]+:","")

##Split into sentences and words
a<-str_split(finalframe[,2],pattern = "\\. |? ")

