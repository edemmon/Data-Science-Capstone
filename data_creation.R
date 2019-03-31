#load libraries
library(NLP);library(tm);library(R.utils); library(stringi) 
library(knitr);  library(RWeka); library(caret); library(tidyr)
library(dplyr)

#download the data
setwd("C:/Users/Sam/Documents/R/Capstone work")
if(!file.exists("Coursera-SwiftKey.zip")){
        url <- "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"
        download.file(url,"Coursera-SwiftKey.zip")
        unzip("Coursera-SwiftKey.zip")
}

#create a sample dataset for US Twitter Data
con_file <-file("final/en_US/en_US.twitter.txt","r")
us_twitter <- readLines(con_file, skipNul = TRUE, encoding = "UTF-8")
set.seed(888)
sample_twitter1 <-us_twitter[rbinom(length(us_twitter)*0.025,length(us_twitter),0.1)]
sample_twitter <- iconv(sample_twitter1, 'UTF-8', 'ASCII') #needed to remove emoji
writeLines(sample_twitter, con="US_Twitter_Sample.txt")
close(con_file)
#create a sample dataset for US Blogs Data
con_file <-file("final/en_US/en_US.blogs.txt","r")
us_blogs <- readLines(con_file, skipNul = TRUE, encoding = "UTF-8")
set.seed(888)
sample_blogs <-us_blogs[rbinom(length(us_blogs)*0.025,length(us_blogs),0.1)]
writeLines(sample_blogs, con="US_Blogs_Sample.txt")
close(con_file)
#create a sample dataset for US News Data
con_file <-file("final/en_US/en_US.news.txt","r")
us_news <- readLines(con_file, skipNul = TRUE, encoding = "UTF-8")
set.seed(888)
sample_news <-us_news[rbinom(length(us_news)*0.025,length(us_news),0.1)]
writeLines(sample_news, con="US_News_Sample.txt")
close(con_file)

#merge sample data
sample_data <- paste(sample_blogs, sample_news, sample_twitter)

## Training ratio
trainPercent <- 0.33
validatePercent <- 0.33
testPercent <- 1 - trainPercent - validatePercent
testSplit <- testPercent
trainSplit <- trainPercent/(trainPercent + validatePercent)

## Split the raw data into test and non-test data sets
## *** Require library caret
inTest <- createDataPartition(seq_len(NROW(sample_data)), p=testSplit, list=FALSE)
rawTestData <- sample_data[inTest]
rawNonTestData <- sample_data[-inTest]

## Split the non-test data into training and validation data sets
inTrain <- createDataPartition(seq_len(NROW(rawNonTestData)), p=trainSplit,
                               list=FALSE)
rawTrainData <- rawNonTestData[inTrain]
rawValidateData <- rawNonTestData[-inTrain]


#remove files you don't need
rm(us_twitter,us_blogs,us_news)
rm(sample_twitter,sample_twitter1,sample_blogs,sample_news)
rm(inTest, inTrain, rawNonTestData)

##Make corpus
makeCorpus <- function(sample_data, StopWordsUse=FALSE){
        
        corpus <- VCorpus(VectorSource(sample_data))
        
        corpus <- tm_map(corpus, removePunctuation)                 # Remove punctuation
        corpus <- tm_map(corpus, removeNumbers)                     # Remove numbers
        removeSpecialChars <- function(x) gsub("[^a-zA-Z ]","",x)
        corpus <- tm_map(corpus, removeSpecialChars)
        corpus <- tm_map(corpus, tolower)                           # Convert to lower case
        if(StopWordsUse){
        corpus <- tm_map(corpus, removeWords, stopwords(kind="en")) # Remove (English) stopwords
        }
        #corpus <- tm_map(corpus, stemDocument)                      # Do stemming
        corpus <- tm_map(corpus, stripWhitespace)                   # Remove (remaining) whitespace
        corpus <- tm_map(corpus, PlainTextDocument)                 # Process as text documents
        
        ##Profanity filter
        if(!file.exists("swearWords.txt")){
                url <- "http://www.bannedwordlist.com/lists/swearWords.txt"
                download.file(url,"swearWords.txt")
        }
        profanity_list <- readLines("swearWords.txt")
        corpus <- tm_map(corpus, removeWords, profanity_list)
        
        return(corpus)
}

trainCorpus <- makeCorpus(rawTrainData,StopWordsUse=TRUE)

# trainCorpus2 <- makeCorpus(rawTrainData,StopWordsUse=FALSE) 
# validateCorpus2 <- makeCorpus(rawValidateData,StopWordsUse=FALSE)
# testCorpus <- makeCorpus(rawNonTestData,StopWordsUse=TRUE)
# testCorpus2 <- makeCorpus(rawNonTestData,StopWordsUse=FALSE)



        ##Tokanization
        UnigramTokenizer   <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 1))
        BigramTokenizer   <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
        TrigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
        QuadgramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 4, max = 4))
        
        # Generate the Term document matrix
        dtm_uni   <- DocumentTermMatrix(trainCorpus, control=list(tokenize=UnigramTokenizer))
        dtm_bi   <- DocumentTermMatrix(trainCorpus, control=list(tokenize=BigramTokenizer))
        dtm_tri <- DocumentTermMatrix(trainCorpus, control=list(tokenize=TrigramTokenizer))
        dtm_quad <- DocumentTermMatrix(trainCorpus, control=list(tokenize=QuadgramTokenizer))
        
        ##Remove unnecessary files
        save(trainCorpus, file='trainCorpus.Rdata')
        rm(trainCorpus)
        
        ## Generate frequencies of ngrams
        uni_frequency   <- sort(colSums(as.matrix(dtm_uni)), decreasing=TRUE)
        bi_frequency   <- sort(colSums(as.matrix(removeSparseTerms(dtm_bi,sparse=0.999))), decreasing=TRUE)
        tri_frequency  <- sort(colSums(as.matrix(removeSparseTerms(dtm_tri,sparse=0.999))), decreasing=TRUE)
        quad_frequency  <- sort(colSums(as.matrix(removeSparseTerms(dtm_quad,sparse=0.999))), decreasing=TRUE)

        #ngrams <- list(uni_frequency,bi_frequency,tri_frequency,quad_frequency)
        save(dtm_uni,dtm_bi,dtm_tri,dtm_quad, file='dtm_train.Rdata')
        rm(dtm_uni,dtm_bi,dtm_tri,dtm_quad)
        
#repeat for the validation data

validateCorpus <- makeCorpus(rawValidateData,StopWordsUse=TRUE)
        
        dtm_uni2   <- DocumentTermMatrix(validateCorpus, control=list(tokenize=UnigramTokenizer))
        dtm_bi2   <- DocumentTermMatrix(validateCorpus, control=list(tokenize=BigramTokenizer))
        dtm_tri2 <- DocumentTermMatrix(validateCorpus, control=list(tokenize=TrigramTokenizer))
        dtm_quad2 <- DocumentTermMatrix(validateCorpus, control=list(tokenize=QuadgramTokenizer))
        
        save(validateCorpus, file='validateCorpus.Rdata')
        rm(validateCorpus)
        
        uni_frequency2   <- sort(colSums(as.matrix(dtm_uni2)), decreasing=TRUE)
        bi_frequency2   <- sort(colSums(as.matrix(removeSparseTerms(dtm_bi2,sparse=0.999))), decreasing=TRUE)
        tri_frequency2  <- sort(colSums(as.matrix(removeSparseTerms(dtm_tri2,sparse=0.999))), decreasing=TRUE)
        quad_frequency2  <- sort(colSums(as.matrix(removeSparseTerms(dtm_quad2,sparse=0.999))), decreasing=TRUE)
        
        save(dtm_uni2,dtm_bi2,dtm_tri2,dtm_quad2, file='dtm_validate.Rdata')
        rm(dtm_uni2,dtm_bi2,dtm_tri2,dtm_quad2) 
        
testCorpus <- makeCorpus(rawTestData,StopWordsUse=TRUE)
        
        dtm_uni3   <- DocumentTermMatrix(testCorpus, control=list(tokenize=UnigramTokenizer))
        dtm_bi3   <- DocumentTermMatrix(testCorpus, control=list(tokenize=BigramTokenizer))
        dtm_tri3 <- DocumentTermMatrix(testCorpus, control=list(tokenize=TrigramTokenizer))
        dtm_quad3 <- DocumentTermMatrix(testCorpus, control=list(tokenize=QuadgramTokenizer))
        
        save(testCorpus, file='testCorpus.Rdata')
        rm(testCorpus)
        
        uni_frequency3   <- sort(colSums(as.matrix(dtm_uni3)), decreasing=TRUE)
        bi_frequency3   <- sort(colSums(as.matrix(removeSparseTerms(dtm_bi3,sparse=0.999))), decreasing=TRUE)
        tri_frequency3  <- sort(colSums(as.matrix(removeSparseTerms(dtm_tri3,sparse=0.999))), decreasing=TRUE)
        quad_frequency3  <- sort(colSums(as.matrix(removeSparseTerms(dtm_quad3,sparse=0.999))), decreasing=TRUE)
        
        save(dtm_uni3,dtm_bi3,dtm_tri3,dtm_quad3, file='dtm_test.Rdata')
        rm(dtm_uni3,dtm_bi3,dtm_tri3,dtm_quad3) 

        
trainCorpus2 <- makeCorpus(rawTrainData,StopWordsUse=FALSE)
        
        # trainCorpus2 <- makeCorpus(rawTrainData,StopWordsUse=FALSE) 
        # validateCorpus2 <- makeCorpus(rawValidateData,StopWordsUse=FALSE)
        # testCorpus <- makeCorpus(rawNonTestData,StopWordsUse=TRUE)
        # testCorpus2 <- makeCorpus(rawNonTestData,StopWordsUse=FALSE)
        
        
        
        ##Tokanization
        #UnigramTokenizer   <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 1))
        BigramTokenizer   <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
        TrigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
        QuadgramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 4, max = 4))
        
        # Generate the Term document matrix
        #dtm_uni   <- DocumentTermMatrix(trainCorpus2, control=list(tokenize=UnigramTokenizer))
        dtm_bi4   <- DocumentTermMatrix(trainCorpus2, control=list(tokenize=BigramTokenizer))
        dtm_tri4 <- DocumentTermMatrix(trainCorpus2, control=list(tokenize=TrigramTokenizer))
        dtm_quad4 <- DocumentTermMatrix(trainCorpus2, control=list(tokenize=QuadgramTokenizer))
        
        ##Remove unnecessary files
        save(trainCorpus2, file='trainCorpus2.Rdata')
        rm(trainCorpus2)
        
        ## Generate frequencies of ngrams
        #uni_frequency   <- sort(colSums(as.matrix(dtm_uni)), decreasing=TRUE)
        bi_frequency4   <- sort(colSums(as.matrix(removeSparseTerms(dtm_bi4,sparse=0.99))), decreasing=TRUE)
        tri_frequency4  <- sort(colSums(as.matrix(removeSparseTerms(dtm_tri4,sparse=0.995))), decreasing=TRUE)
        quad_frequency4  <- sort(colSums(as.matrix(removeSparseTerms(dtm_quad4,sparse=0.995))), decreasing=TRUE)
        
        #ngrams <- list(uni_frequency,bi_frequency,tri_frequency,quad_frequency)
        save(dtm_bi4,dtm_tri4,dtm_quad4, file='dtm_train.Rdata')
        rm(dtm_bi4,dtm_tri4,dtm_quad4)
        
        
# get the final n-gram dataframe (Data table)       
         ## Unigram data table
        uni_df <- as.data.frame(uni_frequency)
        uni_df$term <- rownames(uni_df)
        uni_df2 <- as.data.frame(uni_frequency2)
        uni_df2$term <- rownames(uni_df2)
        uni_df3 <- as.data.frame(uni_frequency3)
        uni_df3$term <- rownames(uni_df3)
        
        uni_df <-merge(uni_df, uni_df2, by="term",all=TRUE)
        uni_df <-merge(uni_df, uni_df3, by="term",all=TRUE)
        
        uni_df$freq <-rowSums(uni_df[,c("uni_frequency", "uni_frequency2","uni_frequency3")], na.rm=TRUE)
        uni_df      <- uni_df[order(uni_df$freq, decreasing = T),]
        
        #names(uni_df2)[names(uni_df2)=="uni_frequency2"] <-"uni_frequency"
        #uni_df <-rbind(uni_df,uni_df2)
        #colnames(uni_df) <- c("freq")
        #uni_df$term <- rownames(uni_df)
        
        ## Bigram data table
        bi_df <- as.data.frame(bi_frequency)
        bi_df$term <- rownames(bi_df)
        bi_df2 <- as.data.frame(bi_frequency2)
        bi_df2$term <- rownames(bi_df2)
        bi_df3 <- as.data.frame(bi_frequency3)
        bi_df3$term <- rownames(bi_df3)
        bi_df4 <- as.data.frame(bi_frequency4)
        bi_df4$term <- rownames(bi_df4)
        bi_df4 <-merge(x=bi_df4, y=bi_df, by="term", all.x=TRUE)
        bi_df4 <-bi_df4[is.na(bi_df4$bi_frequency),]
        keeps <- c("term","bi_frequency4")
        bi_df4 <-bi_df4[keeps]
        
        bi_df <-merge(bi_df, bi_df2, by="term",all=TRUE)
        bi_df <-merge(bi_df, bi_df3, by="term",all=TRUE)
        bi_df <-merge(bi_df, bi_df4, by="term",all=TRUE)
        
        bi_df$freq <-rowSums(bi_df[,c("bi_frequency", "bi_frequency2"
                                      ,"bi_frequency3","bi_frequency4")], na.rm=TRUE)
        bi_df      <- bi_df[order(bi_df$freq, decreasing = T),]
        
        bi_df <- separate(bi_df,term,c("firstname","secname"),remove=FALSE)
        bi_df$prior <- bi_df$firstname
        bi_df$nextword <- bi_df$secname
        
        ## Trigram data table
        tri_df <- as.data.frame(tri_frequency)
        tri_df$term <- rownames(tri_df)
        tri_df2 <- as.data.frame(tri_frequency2)
        tri_df2$term <- rownames(tri_df2)
        tri_df3 <- as.data.frame(tri_frequency3)
        tri_df3$term <- rownames(tri_df3)
        tri_df4 <- as.data.frame(tri_frequency4)
        tri_df4$term <- rownames(tri_df4)
        tri_df4 <-merge(x=tri_df4, y=tri_df, by="term", all.x=TRUE)
        tri_df4 <-tri_df4[is.na(tri_df4$tri_frequency),]
        keeps <- c("term","tri_frequency4")
        tri_df4 <-tri_df4[keeps]
        
        tri_df <-merge(tri_df, tri_df2, by="term",all=TRUE)
        tri_df <-merge(tri_df, tri_df3, by="term",all=TRUE)
        tri_df <-merge(tri_df, tri_df4, by="term",all=TRUE)
        
        tri_df$freq <-rowSums(tri_df[,c("tri_frequency", "tri_frequency2",
                                        "tri_frequency3","tri_frequency4")], na.rm=TRUE)
        tri_df      <- tri_df[order(tri_df$freq, decreasing = T),]
        
        tri_df <- separate(tri_df,term,c("firstname","secname","thirdname"),remove=FALSE)
        tri_df$prior <- paste(tri_df$firstname, tri_df$secname, tri_df$thirdname)
        tri_df$nextword <- tri_df$thirdname
        
        
        ## Quadgram data table
        quad_df <- as.data.frame(quad_frequency)
        quad_df$term <- rownames(quad_df)
        quad_df2 <- as.data.frame(quad_frequency2)
        quad_df2$term <- rownames(quad_df2)
        quad_df3 <- as.data.frame(quad_frequency3)
        quad_df3$term <- rownames(quad_df3)
        quad_df4 <- as.data.frame(quad_frequency4)
        quad_df4$term <- rownames(quad_df4)
        quad_df4 <-merge(x=quad_df4, y=quad_df, by="term", all.x=TRUE)
        quad_df4 <-quad_df4[is.na(quad_df4$quad_frequency),]
        keeps <- c("term","quad_frequency4")
        quad_df4 <-quad_df4[keeps]
        
        quad_df <-merge(quad_df, quad_df2, by="term",all=TRUE)
        quad_df <-merge(quad_df, quad_df3, by="term",all=TRUE)
        quad_df <-merge(quad_df, quad_df4, by="term",all=TRUE)
        
        quad_df$freq <-rowSums(quad_df[,c("quad_frequency", "quad_frequency2",
                                          "quad_frequency3","quad_frequency4")], na.rm=TRUE)
        quad_df      <- quad_df[order(quad_df$freq, decreasing = T),]
        
        quad_df <- separate(quad_df,term,c("firstname","secname","thirdname","fourname"),remove=FALSE)
        quad_df$prior <- paste(quad_df$firstname, quad_df$secname, quad_df$thirdname)
        quad_df$t12 <- paste(quad_df$firstname, quad_df$secname)
        quad_df$t23 <- paste(quad_df$secname, quad_df$thirdname)
        quad_df$t13 <- paste(quad_df$firstname, quad_df$thirdname)
        quad_df$nextword <- quad_df$fourname
       
        
        ngrams <- list(uni_df,bi_df,tri_df,quad_df)
        
save(uni_df,bi_df,tri_df,quad_df, file = 'ngrams.RData')
