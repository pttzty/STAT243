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
Speechdataframe[,2]=str_replace_all(Speechdataframe[,2],"(September|October) \\d+, ","")
colnames(Speechdataframe)<-c("first_URL","Year")

###Write a function about how to extract URL of a year given year as an input
select_url<-function(year){
  return(Speechdataframe[Speechdataframe[,2]==year,1])
}


##B
speech_data<-htmlParse(select_url(2012))
## By inspecting the Xpath Code of the element in Chrome.
text_data<-xpathSApply(speech_data,"//p/text()",xmlValue)
##Good Look
cat(paste(text_data,collapse="\n\n"))

##B
text_data<-as.list(text_data)
# z <- lapply(text_data
#        ,function(x){y <-as.character(str_match(string=x, pattern="^[A-Z]+:"))
#                     return(y)})
# z
speakernames <- as.list(str_replace(as.list(str_match(text_data,"^[A-Z]+:")),":",replacement=""))
##Set up
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
finalframe[,2]<-str_replace_all(finalframe[,2],"^ ","")
##Split into sentences and words
sentencesplit<-str_split(finalframe[,2],pattern = "\\. |\\? |\\, |\\ -- |\\.\\.\\. ")
# b<-str_split(a,pattern=" ")
wordsplit<-lapply(sentencesplit,function(x){return(str_split(x,pattern="\\ "))})
##Count total number of words and characters
finalframe<-cbind(finalframe,wordsplit)

###Write a function that will return the data required for a speech.
Candidate_stat<-function(finalframe){
  ##Store speaker names to a vector
  speaker_unique<-unique(finalframe[finalframe[,1]!="SPEAKERS",1])
  ##Create an empty data frame to store number of words, average length, etc.
  candidate_data<-data.frame(matrix(numeric(0),ncol=15,nrow=3),stringsAsFactors=FALSE)
  colnames(candidate_data)<-c("wordcount","charachtercount","averagelength",
                              "I","we","American","democracy","republic",
                              "Democrat","Republican","freedom",
                              "war","God","God Bless","Jesus")
  rownames(candidate_data)<-speaker_unique
  ##Now all splitting in word is in the third column of the finalframe
  for (i in 1:length(speaker_unique)){
    name=speaker_unique[[i]]
    word_candidate=unlist(finalframe[finalframe[,1]==name,3])
    candidate_data$wordcount[i]<-length(word_candidate)
    candidate_data$charachtercount[i]<-sum(nchar(word_candidate))
    candidate_data$averagelength[i]=candidate_data$charachtercount[i]/candidate_data$wordcount[i]
    candidate_data$American[i]<-sum(str_count(word_candidate,"American?"))
    candidate_data$I[i]<-sum(str_count(word_candidate,"I\\b"))
    candidate_data$we[i]<-sum(str_count(word_candidate,"We\\b|we\\b"))
    candidate_data$democracy[i]<-sum(str_count(word_candidate,"democracy\\b|democratic\\b"))
    candidate_data$republic[i]<-sum(str_count(word_candidate,"republic\\b|Republic\\b"))
    candidate_data$Democrat[i]<-sum(str_count(word_candidate,"Democrats?[ic]?\\b"))
    candidate_data$Republican[i]<-sum(str_count(word_candidate,"Republicans?"))
    candidate_data$freedom[i]<-sum(str_count(word_candidate,"free[dom]?"))
  }
  return(candidate_data)
}