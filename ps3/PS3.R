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
speech1996<-htmlParse(Speechdataframe[5,1])
# nodes1996<-getNodeSet(speech1996,"//p")
# nodes1996<-sapply(nodes1996,toString.XMLNode)
## By inspecting the Xpath Code of the element in Chrome.
text1996<-xpathSApply(speech1996,"//p/text()",xmlValue)
text1996<-as.list(text1996)
cat(text1996)
length(nodes2012)