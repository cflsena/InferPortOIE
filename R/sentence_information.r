## Initializes The Java Viritual Machine with SentenceAnalyzer.jar
.jinit("/home/cflsena/Documentos/Github/R/InferPortOIE/SentenceAnalyzer/dist/SentenceAnalyzer.jar") 
## Get the class object in the path below
object<-.jnew("sentenceanalyzer/SentenceAnalyzer")

## Function to return the sentence information (words, postag, and chunk).
getSentenceInformation <- function(sentence, iCount) 
{
  ## Results of the getInfoSentence method of the java file
  resultJ<-c(.jcall(object, "S", "getInfoSentence", as.String(sentence[iCount,])))
  
  ## Sets the encoding of the sentence file sentence
  Encoding(resultJ)<-"UTF-8"
  
  ## Pass the sentence information to the String type
  resultJ<-as.String(resultJ)
  
  ## Remove the character "[" of the sentence information
  resultJ<-gsub("[[]", "", resultJ, fixed=F)
  
  ## Remove the character "]]" of the sentence information
  resultJ<-gsub("[]]]", "", resultJ, fixed=F)
  
  ## Split the information into 3 parts (words, postag, chunk)
  resultJ<-str_split_fixed(resultJ, "],", 3)
  
  ## Remove the character "]"
  resultJ[[3]]<-gsub("[]]", "", resultJ[[3]], fixed=F)
  
  ## Split the information into 4 parts (words, postag, chunk, and lemma)
  #resultJ<-str_split_fixed(resultJ, "],", 4)
  
  ## Remove the character "]" of the sentence information
  #resultJ[1,4]<-gsub("[]]", "", resultJ[1,4], fixed=F)
  
  resultJ
}

getLemmas <- function (relation)
{
  ## Results of the getInfoSentence method of the java file
  resultJ<-c(.jcall(object, "S", "getLemma", as.String(relation)))
  
  ## Sets the encoding of the sentence file sentence
  Encoding(resultJ)<-"UTF-8"
  
  ## Pass the sentence information to the String type
  resultJ<-as.String(resultJ)
  
  ## Remove the character "[" of the sentence information
  resultJ<-gsub("[[]", "", resultJ, fixed=F)
  
  ## Remove the character "]]" of the sentence information
  resultJ<-gsub("[]]]", "", resultJ, fixed=F)
  
  ## Remove the character "]," of the sentence information
  resultJ<-gsub("[],]", "", resultJ, fixed=F)
  
  resultJ
   
}