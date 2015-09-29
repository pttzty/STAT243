library(XML)
library(RCurl)
library(curl)
library(stringr)
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


##B and C;
textbody<-function(year){
  speech_data<-htmlParse(select_url(year))
  ## By inspecting the Xpath Code of the element in Chrome.
  ## //p/text() will extract the body of the article
  text_data<-xpathSApply(speech_data,"//p/text()",xmlValue)
  ##Good Look
  # cat(paste(text_data,collapse="\n\n"))
  #This step concatenate all text together, and I extract all speaker names
  ## Then I split the original text by "Speakernames:", and throw out the first elemment of the list
  ## After that I created a data frame with names on the left and text on the right
  text_data<-paste(text_data,collapse=" ")
  snames<-as.list(str_replace(unlist(str_extract_all(text_data,"[A-Z]+:")),":",replacement=""))
  text_data<-str_split(text_data,pattern = "[A-Z]+: ")
  text_data<-unlist(text_data)[-1]
  
  finalframe<-data.frame(cbind(unlist(snames),text_data),stringsAsFactors = FALSE)
  index=1
  index_vec<-c(1)
  for(i in 2:nrow(finalframe)){
    if(finalframe[i,1]!=finalframe[i-1,1]){
      index=i
      index_vec<-c(index_vec,index)
    }
    if(finalframe[i,1]==finalframe[i-1,1]){
      finalframe[index,2]=paste(finalframe[index,2],finalframe[i,2],collapse="\n")
    }
  }
  finalframe<-finalframe[index_vec,]
  rownames(finalframe)<-NULL
  ##This line of code eliminates the non-spoken text. Such as Laughter...
  ## I am, However, willing to retain those information. Thus I place the new text in a new column
  ## By saying that, I will compute the # of tags for each candidate in future steps.
  finalframe[,3]<-str_replace_all(finalframe[,2],"\\(LAUGHTER\\)|\\(APPLAUSE\\)|\\(CROSSTALK\\)","")
  colnames(finalframe)<-c("speakernames","raw text","spoken text")
  return(finalframe)
}

# text_data<-as.list(text_data)
# speakernames <- as.list(str_replace(as.list(str_match(text_data,"^[A-Z]+:")),":",replacement=""))
# ##Set up
# finalframe<-cbind(speakernames,text_data)
# 
# ###concatenate the content of NA to the person.
# 
# speakerposition<-1
# for (i in 2:nrow(finalframe)){
#   if(is.na(finalframe[i,1])==FALSE){
#     speakerposition<-i
#   }
#   if(is.na(finalframe[i,1])){
#     finalframe[speakerposition,2]<-paste(finalframe[speakerposition,2],finalframe[i,2],collapse="\n\n")
#   }
# }
# ##Eliminate those name columns with NA, because the cotent has been concatenated. 
# finalframe<-finalframe[is.na(finalframe[,1])==FALSE,]


# finalframe[,2]<-str_replace_all(finalframe[,2],"\\(LAUGHTER\\)|\\(APPLAUSE\\)|\\(CROSSTALK\\)|^[A-Z]+:","")
# finalframe[,2]<-str_replace_all(finalframe[,2],"^ ","")


###D This step will extract sentences and words;
split_word<-function(finalframe){
  withoutpunc<-str_replace_all(finalframe[,3],pattern="\\.|\\,|\\.\\.\\.|\\?|\\!|\\ --|\\ (?![A-Za-z0-9])","")
  # wordsplit<-lapply(withoutpunc,function(x){return(str_split(x,pattern="\\ "))})
  wordsplit<-str_split(withoutpunc,pattern = "\\ ")
  
#   sentencesplit<-str_split(finalframe[,3],pattern = "\\. |\\? |\\, |\\ -- |\\.\\.\\. ")
#   # b<-str_split(a,pattern=" ")
#   wordsplit<-lapply(sentencesplit,function(x){return(str_split(x,pattern="\\ "))})
  
  newframe<-cbind(as.list(finalframe[,1]),as.list(finalframe[,2]),as.list(finalframe[,3]),wordsplit)
  finalframe<-newframe
  colnames(finalframe)<-c("speakernames","raw text","spoken text","wordsplit")
  finalframe<-data.frame(finalframe,stringsAsFactors = FALSE)
  return(finalframe)
}


##Part E and F, and Also count the number of tags
###Write a function that will return the data required for a speech.
Candidate_stat<-function(finalframe){
  ##Store speaker names to a vector
  speaker_unique<-unlist(unique(finalframe[finalframe[,1]!="SPEAKERS",1]))
  ##Create an empty data frame to store number of words, average length, etc.
  candidate_data<-data.frame(matrix(numeric(0),ncol=17,nrow=3),stringsAsFactors=FALSE)
  colnames(candidate_data)<-c("wordcount","charachtercount","averagelength",
                              "I","we","American","democracy","republic",
                              "Democrat","Republican","freedom",
                              "war","Jesus","God","GodBless","Laughter","Applause")
  rownames(candidate_data)<-speaker_unique
  ##Now all splitting in word is in the third column of the finalframe
  ## for loop looping from 1 to 3, namely moderator and each candidate
  ## The regexvector contains the basic regular expressions for use, some special ones 
  ## will be dealt with seperately.
  regexvector<-c("I[^a-z]","[W|w]e[^a-z]","American?","democracy\\b|democratic\\b",
                 "[R|r]epublic\\b","Democrats?[ic]?","Republicans?",
                 "[F|f]ree[dom]?","[W|w]ars?","Jesus|Christs\\b|Christians?")
  for (i in 1:length(speaker_unique)){
    name=speaker_unique[[i]]
    word_candidate=unlist(finalframe[finalframe[,1]==name,4])
    text_candidate=unlist(finalframe[finalframe[,1]==name,3])
##In order to count Laughters and Applause tags
    raw_candidate=unlist(finalframe[finalframe[,1]==name,2])
    
    candidate_data$wordcount[i]<-length(word_candidate)
    candidate_data$charachtercount[i]<-sum(nchar(word_candidate))
    candidate_data$averagelength[i]=candidate_data$charachtercount[i]/candidate_data$wordcount[i]
    for (k in 1:length(regexvector)){
      candidate_data[i,k+3]<-sum(str_count(word_candidate,pattern=regexvector[k]))
    }
    #### Since God bless has two words, we need to use main text instead of words to count
    #### so I wrote it out of the previous loop. 
    candidate_data$God[i]<-sum(str_count(text_candidate,"[G|g]od (?!bless)"))
    candidate_data$GodBless[i]<-sum(str_count(text_candidate,"[G|g]od bless"))
    ###This is one of part c in the problem.
    candidate_data$Laughter[i]<-sum(str_count(raw_candidate,"\\(LAUGHTER\\)"))
    candidate_data$Applause[i]<-sum(str_count(raw_candidate,"\\(APPLAUSE\\)"))
  }
  return(candidate_data)
}

###Combine all functions together, the stat table is the table of statistics
main<-function(year){
  finalframe<-textbody(year)
  aftersplit<-split_word(finalframe)
  stat_table<-Candidate_stat(aftersplit)
  rownames(stat_table)<-paste(rownames(stat_table),year)
  return(stat_table)
}
a<-lapply(c(2012,2008,2004,2000,1996),main)