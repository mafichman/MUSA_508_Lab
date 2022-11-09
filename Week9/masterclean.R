library(dplyr)
library(doBy)
library(plyr)
library(ggplot2)
library(readr)
library(glmnet)
library(stringr)
library(Matrix)
library(tm)
library(SparseM)
library(data.table)
library(doParallel)
library(foreach)
library(hunspell)
library(RecordLinkage)
library(SnowballC)
library(sqldf)

rm(list = ls())
setwd("/storage/coredata")
d1 <- read.table(file = "UNC_Charlotte_300000251938209_Mls_Delivery_20210609.txt",
                 header = TRUE,
                 sep = "|",
                 na.strings = "",
                 stringsAsFactors = FALSE,
                 comment.char = "",
                 quote = "\"",
                 fill = TRUE)

d2 <- subset(d1, PropertyType == "Single Family")

meck <- subset(d2, AddressCountyOrParish == "Mecklenburg")
#ral <- subset(d2, AddressCountyOrParish == "Wake")
#grb <- subset(d2, AddressCountyOrParish == "Guilford")
#wis <- subset(d2, AddressCountyOrParish == "Forsyth")
#dur <- subset(d2, AddressCountyOrParish == "Durham")
ire <- subset(d2, AddressCountyOrParish == "Iredell")
row <- subset(d2, AddressCountyOrParish == "Rowan")
cab <- subset(d2, AddressCountyOrParish == "Cabarrus")
uni <- subset(d2, AddressCountyOrParish == "Union")
ans <- subset(d2, AddressCountyOrParish == "Anson")
lin <- subset(d2, AddressCountyOrParish == "Lincoln")
gas <- subset(d2, AddressCountyOrParish == "Gaston")
che <- subset(d2, AddressCountyOrParish == "Chester")
lan <- subset(d2, AddressCountyOrParish == "Lancaster")
yor <- subset(d2, AddressCountyOrParish == "York")

cltmsa <- rbind(meck,ire,row,cab,uni,ans,lin,gas,che,lan,yor)

# remove listings with PublicRemarks missing
cltmsa1 <- cltmsa[!(is.na(cltmsa$PublicRemarks)), ]

# remove | from PublicRemarks - not sure that we need to in here
#cltmsa1$PublicRemarks <-gsub("|"," ",as.character(cltmsa1$PublicRemarks))

# clean up/offload environment
rm(cab, che, gas, ire, lan, lin, meck, row, uni, yor, ans, cltmsa, d1, d2)

# check whether ListingID will work as ID (i.e., no duplicates)
cltuni <- unique(cltmsa1$ListingID)
length(cltuni)
# will work

# pull out only ListingID and public remarks
clt <- cltmsa1[, c(250,335)]

# recode data to avoid trouble
clt$PublicRemarks <- enc2utf8(clt$PublicRemarks)

# convert remarks to lowercase
clt$PublicRemarks <- trimws(clt$PublicRemarks)
clt$PublicRemarks <- tolower(clt$PublicRemarks)

############ Non-alphanumeric word list for cleaned remarks ###################
sample <- clt$PublicRemarks

wordlist1 <- fread("NonAlpha_Word_List_Run_First.csv", header=T)
wordlist1 <- wordlist1[,lapply(wordlist1,function(x) tolower(x))]

wordlist1$misspell <- paste("",wordlist1$misspell,"")
wordlist1$update <- paste("",wordlist1$update,"")

# Create update list
FROM <- wordlist1$misspell
TO <- wordlist1$update
cbind(FROM,TO)

### update loop
CORES <- detectCores() ; registerDoParallel(CORES)
sample <- paste(" ",sample," ")
head(sample)
oL <- sort((1:length(sample)%%100)+1)
sample <- foreach(i = 1:100 , .combine=c, .packages = "stringi") %dopar%
  {
    xLOOP <- stri_replace_all_fixed(sample[which(oL==i)], FROM , TO ,vectorize_all=F)
    xLOOP
  }
closeAllConnections()
sample <- gsub(" +"," ", sample) #reduce whitespace to one
head(sample)

clt$PublicRemarks <- sample
clt$PublicRemarks <- trimws(clt$PublicRemarks)
rm(sample, FROM, TO, wordlist1)

############ Non-transportation word list for cleaned remarks ###################
# take out walk-in closet, walkway, and other "walk" words that do not pertain
# to walkable environment 
walksdf <- sqldf("select phrase_, sum(cnt_) cnt_ from (select substring(PublicRemarks,(INSTR(PublicRemarks,'walk')-10), 20) phrase_,1 as cnt_ from clt where PublicRemarks like '%walk%') a group by phrase_")

sample <- clt$PublicRemarks

wordlist0 <- fread("Alpha_Word_List_NonTransport.csv", header=T)
wordlist0 <- wordlist0[,lapply(wordlist0,function(x) tolower(x))]

wordlist0$misspell <- paste("",wordlist0$misspell,"")
wordlist0$update <- paste("",wordlist0$update,"")

# Create update list
FROM <- wordlist0$misspell
TO <- wordlist0$update
cbind(FROM,TO)

### update loop
CORES <- detectCores() ; registerDoParallel(CORES)
sample <- paste(" ",sample," ")
head(sample)
oL <- sort((1:length(sample)%%100)+1)
sample <- foreach(i = 1:100 , .combine=c, .packages = "stringi") %dopar%
  {
    xLOOP <- stri_replace_all_fixed(sample[which(oL==i)], FROM , TO ,vectorize_all=F)
    xLOOP
  }
closeAllConnections()
sample <- gsub(" +"," ", sample) #reduce whitespace to one
head(sample)

clt$PublicRemarks <- sample
clt$PublicRemarks <- trimws(clt$PublicRemarks)
rm(sample, FROM, TO, wordlist0)

############ Transportation word list for cleaned remarks ###################
# running it here before taking out single letters, numbers & special characters
# such as i-485 would otherwise go lost

sample <- clt$PublicRemarks

wordlist0 <- fread("Alpha_Word_List_Transport.csv", header=T)
wordlist0 <- wordlist0[,lapply(wordlist0,function(x) tolower(x))]

wordlist0$misspell <- paste("",wordlist0$misspell,"")
wordlist0$update <- paste("",wordlist0$update,"")

# Create update list
FROM <- wordlist0$misspell
TO <- wordlist0$update
cbind(FROM,TO)

### update loop
CORES <- detectCores() ; registerDoParallel(CORES)
sample <- paste(" ",sample," ")
head(sample)
oL <- sort((1:length(sample)%%100)+1)
sample <- foreach(i = 1:100 , .combine=c, .packages = "stringi") %dopar%
  {
    xLOOP <- stri_replace_all_fixed(sample[which(oL==i)], FROM , TO ,vectorize_all=F)
    xLOOP
  }
closeAllConnections()
sample <- gsub(" +"," ", sample) #reduce whitespace to one
head(sample)

clt$PublicRemarks <- sample
clt$PublicRemarks <- trimws(clt$PublicRemarks)
rm(sample, FROM, TO, wordlist0)

##################### Remove non-alphabetic characters ########################
clt$PublicRemarks <- gsub("'","",clt$PublicRemarks)
clt$PublicRemarks <- gsub("[0-9]","",clt$PublicRemarks)
clt$PublicRemarks <- gsub("[^[:alnum:]]"," ",clt$PublicRemarks)
clt$PublicRemarks <- gsub("^[a-z] ","",clt$PublicRemarks)
clt$PublicRemarks <- gsub(" [a-z]$","",clt$PublicRemarks)
clt$PublicRemarks <- gsub(" +"," ",clt$PublicRemarks)
clt$PublicRemarks <- trimws(clt$PublicRemarks)
clt$PublicRemarks <- paste(" ",clt$PublicRemarks," ")
head(clt$PublicRemarks)

########################## Alphabetic word list ###############################
sample <- clt$PublicRemarks

wordlist2 <- fread("Alpha_Word_List_NC.csv", header=T)
wordlist2 <- wordlist2[,lapply(wordlist2,function(x) tolower(x))]

wordlist2$misspell <- paste("",wordlist2$misspell,"")
wordlist2$update <- paste("",wordlist2$update,"")

# create update list
FROM <- wordlist2$misspell
TO <- wordlist2$update
cbind(FROM,TO)

### update loop
CORES <- detectCores() ; registerDoParallel(CORES)
sample <- paste(" ",sample," ")
head(sample)
N <- length(sample)
sample <- foreach(i = 1:N , .combine=c, .packages = "stringi") %dopar%
  {
    #if(i%%10000==0) cat(paste0(i," of ",N) , file="log.txt")
    xLOOP <- stri_replace_all_fixed(sample[i], FROM , TO ,vectorize_all=F)
    xLOOP
  }
closeAllConnections()
sample <- gsub(" +"," ", sample) #reduce whitespace to one
head(sample)

clt$PublicRemarks <- sample
clt$PublicRemarks <- trimws(clt$PublicRemarks)
rm(sample, FROM, TO, wordlist2)
head(clt$PublicRemarks)

##################### Neighborhood and town word list 1 #######################
# changes misspelled neighborhood names to their original and/or truncates
# e.g. plaza midwood to plazamidwood

sample <- clt$PublicRemarks

wordlist3 <- fread("Alpha_Word_List_CLTMSA_run1.csv", header=T)
wordlist3 <- wordlist3[,lapply(wordlist3,function(x) tolower(x))]

wordlist3$misspell <- paste("",wordlist3$misspell,"")
wordlist3$update <- paste("",wordlist3$update,"")

# create update list
FROM <- wordlist3$misspell
TO <- wordlist3$update
cbind(FROM,TO)

### update loop
CORES <- detectCores() ; registerDoParallel(CORES)
sample <- paste(" ",sample," ")
head(sample)
N <- length(sample)
sample <- foreach(i = 1:N , .combine=c, .packages = "stringi") %dopar%
  {
    #if(i%%10000==0) cat(paste0(i," of ",N) , file="log.txt")
    xLOOP <- stri_replace_all_fixed(sample[i], FROM , TO ,vectorize_all=F)
    xLOOP
  }
closeAllConnections()
sample <- gsub(" +"," ", sample) #reduce whitespace to one
head(sample)

clt$PublicRemarks <- sample
clt$PublicRemarks <- trimws(clt$PublicRemarks)
rm(sample, FROM, TO, wordlist3)
head(clt$PublicRemarks)

###################### Neighborhood and town word list 2 ######################
# converts all neighborhood names to generic "neighborhoodname"

sample <- clt$PublicRemarks

wordlist4 <- fread("Alpha_Word_List_CLTMSA_run2.csv", header=T)
wordlist4 <- wordlist4[,lapply(wordlist4,function(x) tolower(x))]

wordlist4$misspell <- paste("",wordlist4$misspell,"")
wordlist4$update <- paste("",wordlist4$update,"")

# create update list
FROM <- wordlist4$misspell
TO <- wordlist4$update
cbind(FROM,TO)

### update loop
CORES <- detectCores() ; registerDoParallel(CORES)
sample <- paste(" ",sample," ")
head(sample)
N <- length(sample)
sample <- foreach(i = 1:N , .combine=c, .packages = "stringi") %dopar%
  {
    #if(i%%10000==0) cat(paste0(i," of ",N) , file="log.txt")
    xLOOP <- stri_replace_all_fixed(sample[i], FROM , TO ,vectorize_all=F)
    xLOOP
  }
closeAllConnections()
sample <- gsub(" +"," ", sample) #reduce whitespace to one
head(sample)

clt$PublicRemarks <- sample
clt$PublicRemarks <- trimws(clt$PublicRemarks)
rm(sample, FROM, TO, wordlist4)
head(clt$PublicRemarks)

###################### Misspell wordlist ######################
# removes misspellings and updates a list of words 

sample <- clt$PublicRemarks

wordlist5 <- fread("misspellings.csv", header=T)
wordlist5 <- wordlist5[,lapply(wordlist5,function(x) tolower(x))]

wordlist5$misspell <- paste("",wordlist5$misspell,"")
wordlist5$update <- paste("",wordlist5$update,"")

# create update list
FROM <- wordlist5$misspell
TO <- wordlist5$update
cbind(FROM,TO)

### update loop
CORES <- detectCores() ; registerDoParallel(CORES)
sample <- paste(" ",sample," ")
head(sample)
N <- length(sample)
sample <- foreach(i = 1:N , .combine=c, .packages = "stringi") %dopar%
  {
    #if(i%%10000==0) cat(paste0(i," of ",N) , file="log.txt")
    xLOOP <- stri_replace_all_fixed(sample[i], FROM , TO ,vectorize_all=F)
    xLOOP
  }
closeAllConnections()
sample <- gsub(" +"," ", sample) #reduce whitespace to one
head(sample)

clt$PublicRemarks <- sample
clt$PublicRemarks <- trimws(clt$PublicRemarks)
rm(sample, FROM, TO, wordlist5)
head(clt$PublicRemarks)

#################### Remove Stop Words Cleaned Remarks ########################

sample <- clt$PublicRemarks

stop.words.mls <- c(stopwords(kind='en'),"a","b","c","d","e","f","g","h","i","j","k",
                    "l","m","n","o","p","q","r","s","t","u","v","w","x","y","z","es",
                    "od","le","di","th","mis","st","etc","cc","fl","rd","as","wi","nd",
                    "ro","dec","en","te","ne","com","ho","bat","ca","fi","ooo","oh","ohh",
                    "la","fo","app","pas","li","mi","wa","se","lt","op","til","bac","ex",
                    "fe","gra","pa","ms","pr","su","ar","sh","cl","in","ins","and","ie",
                    "ove","pl","al","bo","est","lo","ou","sept","ct","don","fa","si",
                    "stai","ea","fea","har","hr","one","two","three","four","five",
                    "six","seven","eight","nine","ten")
stop.words.mls <- gsub("[^[:alnum:]]","",stop.words.mls)
stop.words.mls <- paste("",stop.words.mls,"")

# create update list
FROM <- stop.words.mls
TO <- " "
cbind(FROM,TO)

### update loop
CORES <- detectCores() ; registerDoParallel(CORES)
sample <- paste(" ",sample," ")
head(sample)
oL <- sort((1:length(sample)%%100)+1)
sample <- foreach(i = 1:100 , .combine=c, .packages = "stringi") %dopar%
  {
    xLOOP <- stri_replace_all_fixed(sample[which(oL==i)], FROM , TO ,vectorize_all=F)
    xLOOP
  }
closeAllConnections()
sample <- gsub(" +"," ", sample) #reduce whitespace to one
head(sample)

clt$PublicRemarks <- sample
clt$PublicRemarks <- trimws(clt$PublicRemarks)
rm(sample, FROM, TO)

clt$PublicRemarks <- gsub(" +"," ", clt$PublicRemarks) #reduce whitespace to one
clt$PublicRemarks <- trimws(clt$PublicRemarks)
head(clt$PublicRemarks)
tail(clt$PublicRemarks)

####################### Flag troublesome remarks #############################
troublesomeWordList <- c("addendum","alloffer","auction","bank","casenumber","countrywide","expresspath",
                         "fannie","fargo","fha","fmac","foreclosure","freddie","hud","hmpth","homepathfinancing",
                         "homepath","homesteps","lenderapproval","lenderapproved","fnma","fhmc","ginnie",
                         "lenderowned","mae","mac","nsp","preforeclosure","reo","repo","indymac",
                         "shortsale","subjectto","seller has directed","bofa")

troublesomeWordList <- paste(" ",trimws(troublesomeWordList)," ",sep="")
clt$PublicRemarks <- paste(" ",trimws(clt$PublicRemarks)," ",sep="")
clt$distressFlag <- 0
clt$badWord <- ""
for(WORD in troublesomeWordList)
{
  print(WORD)
  flush.console()
  oW <- which(grepl(WORD,clt$PublicRemarks))
  clt$distressFlag[oW] <- clt$distressFlag[oW]+1
  clt$badWord[oW] <- paste(clt$badWord[oW] , WORD)
}
clt$distressFlag <- sign(clt$distressFlag)

clt$PublicRemarks <- trimws(clt$PublicRemarks)
head(clt$PublicRemarks)

write.table(clt, file="CLEANREMARKS_CLTMSA.txt", row.names=F, sep="\t")

#test import
d4 <- read.table(file = "CLEANREMARKS_CLTMSA.txt",
                 header = TRUE,
                 sep = "\t",
                 na.strings = "",
                 stringsAsFactors = FALSE,
                 comment.char = "",
                 quote = "",
                 fill = TRUE)
d5 <- d4[complete.cases(d4), ]

################# PULL LISTING ID AND XY COORD ###############################
# will join 1990s census tract geoid to this and then join it with clean
# remarks so that we can join cluster groups with clean remarks later

rm(list = ls())
setwd("/storage/coredata")
d1 <- read.table(file = "UNC_Charlotte_300000251938209_Mls_Delivery_20210609.txt",
                 header = TRUE,
                 sep = "|",
                 na.strings = "",
                 stringsAsFactors = FALSE,
                 comment.char = "",
                 quote = "\"",
                 fill = TRUE)

d2 <- subset(d1, PropertyType == "Single Family")

meck <- subset(d2, AddressCountyOrParish == "Mecklenburg")
ire <- subset(d2, AddressCountyOrParish == "Iredell")
row <- subset(d2, AddressCountyOrParish == "Rowan")
cab <- subset(d2, AddressCountyOrParish == "Cabarrus")
uni <- subset(d2, AddressCountyOrParish == "Union")
ans <- subset(d2, AddressCountyOrParish == "Anson")
lin <- subset(d2, AddressCountyOrParish == "Lincoln")
gas <- subset(d2, AddressCountyOrParish == "Gaston")
che <- subset(d2, AddressCountyOrParish == "Chester")
lan <- subset(d2, AddressCountyOrParish == "Lancaster")
yor <- subset(d2, AddressCountyOrParish == "York")

cltmsa <- rbind(meck,ire,row,cab,uni,ans,lin,gas,che,lan,yor)

# remove listings with PublicRemarks missing
cltmsa1 <- cltmsa[!(is.na(cltmsa$PublicRemarks)), ]

# remove | from PublicRemarks - not sure that we need to in here
#cltmsa1$PublicRemarks <-gsub("|"," ",as.character(cltmsa1$PublicRemarks))

# clean up/offload environment
rm(cab, che, gas, ire, lan, lin, meck, row, uni, yor, ans, cltmsa, d1, d2)

# check whether ListingID will work as ID (i.e., no duplicates)
cltuni <- unique(cltmsa1$ListingID)
length(cltuni)
# will work - same used for remarks

# pull out only ListingID and address information
listings <- cltmsa1[, c(250,14,27,53,57,56,58,46,49,51,48)]

# pull out only homes for sale (not for rent)
listingss <- subset(listings, FA_Rent_Sale_ind == "S")
#didn't take out many (went from 397K to 395K)
length(unique(listingss$ListingID)) #395,366

#write
write.table(listingss, file="addresslistings.csv", row.names=F, sep=",")

## go to geocodelistings.R for geocoding

############################# STOP HERE ######################################
############################# Hunspell Remarks ###############################
### Hunspell to find typos and replacements - DO NOT RUN - ALREADY RUN
### was used in beginning on raw remarks to find misspelled words
### list updated to not remove neighborhood names, flagged words, etc.

########## Hunspell on raw remarks to find typos & update wordlist ############
clt$raw.remarks <- clt$PublicRemarks

### Non-Alphanumeric Word List for raw remarks
sample <- clt$raw.remarks  

wordlist1 <- fread("NonAlpha_Word_List_Run_First.csv", header=T)
wordlist1 <- wordlist1[,lapply(wordlist1,function(x) tolower(x))]

wordlist1$misspell <- paste("",wordlist1$misspell,"")
wordlist1$update <- paste("",wordlist1$update,"")

# create update list
FROM <- wordlist1$misspell
TO <- wordlist1$update
cbind(FROM,TO)

### update loop
CORES <- detectCores() ; registerDoParallel(CORES)
sample <- paste(" ",sample," ")
head(sample)
oL <- sort((1:length(sample)%%100)+1)
sample <- foreach(i = 1:100 , .combine=c, .packages = "stringi") %dopar%
  {
    xLOOP <- stri_replace_all_fixed(sample[which(oL==i)], FROM , TO ,vectorize_all=F)
    xLOOP
  }
closeAllConnections()
sample <- gsub(" +"," ", sample) #reduce whitespace to one
head(sample)

clt$raw.remarks <- sample
clt$raw.remarks <- trimws(clt$raw.remarks)
rm(sample, FROM, TO, wordlist1)
head(clt$raw.remarks)

### create unique remarks_id
raw <- data.table(clt, key="PublicRemarks")
raw[, remark_id:=.GRP, by=key(raw)]                  #remark_id
raw[, remarkN:= .N, by=remark_id]                    #remarkN    
table(raw$remarkN)

### drop if remarkN > 3
raw <- subset(raw , remarkN<4)

################### Remove Plurals from Cleaned Remarks - NOT WORKING #######################
#sample <- clt$PublicRemarks
#sample <- paste("",sample,"")

##################
### remove plurals
#s <- str_split(sample," ")
#word <- unlist(s)
#wordCounts <- table(word)
#wordCounts <- wordCounts[names(wordCounts)!="s"]
#nc <- str_length(names(wordCounts))
#s2 <- which(substr(names(wordCounts),nc,nc)=="s")
#plurals <- names(wordCounts)[s2]
#singles <- gsub("s$","",plurals)

#updates <- data.frame(plurals,singles)
#updates$pluralok <- hunspell_check(updates$plurals)
#updates$singleok <- hunspell_check(updates$singles)
#keep if singleok is "TRUE"
#updates<-updates[which(updates$singleok=="TRUE"),]
#remove is length 3 or less (as, has, his, etc)
#updates$len <- str_length(updates$plurals)
#updates <- updates[!(updates$plurals=="ss" | updates$plurals=="s" | updates$len<=3),]
#updates <- updates[,1:2]
#add space
#updates$singles <- paste("",updates$singles,"")
#updates$plurals <- paste("",updates$plurals,"")

##################
### remove plurals
#SINGLES <- updates$singles
#PLURALS <- updates$plurals
#cbind(PLURALS,SINGLES)

### plural loop
#CORES <- detectCores() ; registerDoParallel(CORES)
#sample <- paste(" ",sample," ")
#head(sample)
#N <- length(sample)
#sample <- foreach(i = 1:N , .combine=c, .packages = "stringi") %dopar%
  #{
    #if(i%%100000==0) cat(paste0(i," of ",N) , file="log.txt")
   # xLOOP <- stri_replace_all_fixed(sample[i],PLURALS, SINGLES,vectorize_all=F)
  #  xLOOP
 # }
#closeAllConnections()
#sample <- gsub(" +"," ", sample)
#rm(updates)

#head(clt$PublicRemarks)
#clt$PublicRemarks <- sample
#head(clt$PublicRemarks)

#clt$PublicRemarks <- trimws(clt$PublicRemarks)
#head(clt$PublicRemarks)