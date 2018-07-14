#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

#Load libraries 
library(shiny)
library(tm)
library(readr)
library(corpus)
library(tidyverse)
library(ggplot2)
library(rJava)
library(RWeka)
library(stringr)
library(stringi)
library(quanteda)
library(DT)

#Load collocations
  c1<-readRDS("collo1.rds")
  c2<-readRDS("collo2.rds")
  c3<-readRDS("collo3.rds")
  c4<-readRDS("collo4.rds")
  
#Predict word function
predictWord<-function(theWord){

  #Get input text and remove punctuation and upper case letters.
  theWord<-removePunctuation(theWord)
  theWord<-tolower(theWord)
  
  #Separate words and create grep pattern to search for
  words <- unlist(strsplit(theWord, split = " "))
  noOfwords <- length(words)
  nGramIndex<-noOfwords+1
  pattern = paste0("^", paste(words[1:noOfwords], collapse = " ")) 
  pattern = paste0(pattern, " ")
 
   if(nGramIndex==2){
    search1word(theWord, pattern)
  } else if(nGramIndex==3){
    search2words(theWord, pattern)
  } else if(nGramIndex==4) {
    search3words(theWord, pattern)
  } else{
    print("Please enter a new phrase.")
  }
  
} 

#Print table function
predictTable<-function(theWord){
  
  #Get input text and remove punctuation and upper case letters.
  theWord<-removePunctuation(theWord)
  theWord<-tolower(theWord)
  
  #Separate words and create grep pattern to search for
  words <- unlist(strsplit(theWord, split = " "))
  noOfwords <- length(words)
  nGramIndex<-noOfwords+1
  pattern = paste0("^", paste(words[1:noOfwords], collapse = " ")) 
  pattern = paste0(pattern, " ")
  
  if(nGramIndex==2){
    search1wordTable(theWord, pattern)
  } else if(nGramIndex==3){
    search2wordsTable(theWord, pattern)
  } else if(nGramIndex==4) {
    search3wordsTable(theWord, pattern)
  } else{
    print("Please enter a new phrase.")
  }
  
} 

#Search word functions
search1word<-function(theWord, pattern){
    subset.df<-c1[grep(pattern, c1$collocation), ]
    maxCount<-max(subset.df$count)
    subset.df<-subset.df[subset.df$count>=maxCount] 
    nextWord<-word(subset.df$collocation, -1)
  }
  
  search2words<-function(theWord, pattern){
    subset.df<-c2[grep(pattern, c2$collocation), ]
    
    if(is.data.frame(subset.df) && nrow(subset.df)==0){
      newInputText<-paste0("^", paste(words[2:2], collapse = " "))
      newInputText <- paste0(newInputText, " ")
      subset.df<-c1[grep(newInputText, c1$collocation), ]
      maxCount<-max(subset.df$count)
      subset.df<-subset.df[subset.df$count>=maxCount]  
      nextWord<-word(subset.df$collocation, -1)
      
    } else {
      maxCount<-max(subset.df$count)
      subset.df<-subset.df[subset.df$count>=maxCount]     
      nextWord<-word(subset.df$collocation, -1)
      
    }
  }
  
  search3words<-function(theWord, pattern){
    subset.df<-c3[grep(pattern, c3$collocation), ]
    
    if(is.data.frame(subset.df) && nrow(subset.df)==0){
      newInputText<-paste0("^", paste(words[2:3], collapse = " ")) 
      newInputText <- paste0(newInputText, " ")
      subset.df<-c2[grep(newInputText, c1$collocation), ]
      
      if(is.data.frame(subset.df) && nrow(subset.df)==0){
        newInputText<-paste0("^", paste(words[2:2], collapse = " ")) 
        newInputText <- paste0(newInputText, " ")
        subset.df<-c1[grep(newInputText, c1$collocation), ]
        maxCount<-max(subset.df$count)
        subset.df<-subset.df[subset.df$count>=maxCount]  
        nextWord<-word(subset.df$collocation, -1)
        
      } else {
        maxCount<-max(subset.df$count)
        subset.df<-subset.df[subset.df$count>=maxCount]     
        nextWord<-word(subset.df$collocation, -1)
        
      }
      
    } else {
      maxCount<-max(subset.df$count)
      subset.df<-subset.df[subset.df$count>=maxCount]     
      nextWord<-word(subset.df$collocation, -1)
    
    }
  }

#Output table functions
  search1wordTable<-function(theWord, pattern){
    subset.df<-c1[grep(pattern, c1$collocation), ]
    sorted<-subset.df[order(-subset.df$count), ]
    topDF<-head(sorted)
  }
  
  search2wordsTable<-function(theWord, pattern){
    subset.df<-c2[grep(pattern, c2$collocation), ]
    if(is.data.frame(subset.df) && nrow(subset.df)==0){
      newInputText<-paste0("^", paste(words[2:2], collapse = " "))
      newInputText <- paste0(newInputText, " ")
      subset.df<-c1[grep(newInputText, c1$collocation), ]
      sorted<-subset.df[order(-subset.df$count), ]
      topDF<-head(sorted)
    } else {
      sorted<-subset.df[order(-subset.df$count), ]
      topDF<-head(sorted)
    }
  }
  
  search3wordsTable<-function(theWord, pattern){
    subset.df<-c3[grep(pattern, c3$collocation), ]
    
    if(is.data.frame(subset.df) && nrow(subset.df)==0){
      newInputText<-paste0("^", paste(words[2:3], collapse = " ")) 
      newInputText <- paste0(newInputText, " ")
      subset.df<-c2[grep(newInputText, c1$collocation), ]
      sorted<-subset.df[order(-subset.df$count), ]
      topDF<-head(sorted)
  
      if(is.data.frame(subset.df) && nrow(subset.df)==0){
        newInputText<-paste0("^", paste(words[2:2], collapse = " ")) 
        newInputText <- paste0(newInputText, " ")
        subset.df<-c1[grep(newInputText, c1$collocation), ]
        sorted<-subset.df[order(-subset.df$count), ]
        topDF<-head(sorted)
        
      } else {
        sorted<-subset.df[order(-subset.df$count), ]
        topDF<-head(sorted)
        
      }
      
    } else {
      sorted<-subset.df[order(-subset.df$count), ]
      topDF<-head(sorted)   
    }
  }
  
  
shinyServer<-function(input, output){  
  observeEvent (input$goButton, {
    output$nextWord = renderText({
      result <- predictWord(input$text)
      result
    });
    output$table = DT::renderDataTable({
      result2<- predictTable(input$text)
      result2
    })
    
    })
    
}





