#getting CMUdict Phonetics dictionary
#http://svn.code.sf.net/p/cmusphinx/code/trunk/cmudict/cmudict-0.7b
#saved in a csv file
CMUdict <- read.csv("CMUphonetics2.csv", sep = ",", header = FALSE, stringsAsFactors = FALSE, strip.white = TRUE, na.strings=c(""," ","NA"))
#concreteness ratings taken from crr.ugent.be/archives/1330
concRatings <- read.csv("concreteness.csv", sep = ",", stringsAsFactors = FALSE, strip.white = TRUE, na.strings=c(""," ","NA"))
 
#Getting concreteness rating of each word
getConcreteness <- function(book, concRatings) {
bookD <- as.data.frame(book)
colnames(bookD) <- "Word"
concreteness <- vector(mode = "numeric", length=0)
merged <- left_join(bookD, concRatings)
concreteness <- sum(merged$Concreteness, na.rm = TRUE)
concreteness <- concreteness/length(book)
return(concreteness)
}
 
 
getAlliteration <- function(book, CMUdict) {
allitScore <- vector(mode="numeric", length=0)
book <- toupper(book)
bookD <- as.data.frame(book, header=FALSE)
bookP <- left_join(bookD, CMUdict, by = c("book" = "V1"))
bookP <- bookP[,1:2]
allitScore <- sapply(1: (nrow(bookP) - 2),
alliterscore <- function(i_row)
{
sum(match(bookP[i_row,2], bookP[i_row + 1,2], incomparables = NA, nomatch = 0) > 0, na.rm = TRUE) +
sum(match(bookP[i_row,2], bookP[i_row + 2,2], incomparables = NA, nomatch = 0) > 0, na.rm = TRUE)
})
AScore <- sum(allitScore, na.rm = TRUE) / length(book)
return (AScore)
}
# 0.6799756
 
# Time difference of 21.53423 secs
getConsonanceScore <- function(book, CMUdict) {
iterations <- length(book) - 2
consonanceScore <- vector(mode="numeric", length=0)
book <- toupper(book)
bookD <- as.data.frame(book, header=FALSE)
bookP <- left_join(bookD, CMUdict, by = c("book" = "V1"))
consonanceScore <- sapply(1: (nrow(bookP) - 2),
conscore <- function(i_row)
{
sum(match(bookP[i_row,][,-1], bookP[i_row + 1,][,-1], incomparables = NA, nomatch = 0) > 0, na.rm = TRUE) +
sum(match(bookP[i_row,][,-1], bookP[i_row + 2,][,-1], incomparables = NA, nomatch = 0) > 0, na.rm = TRUE)
 
})
 
# consonanceScore <-
# sapply(1:(nrow(bookP)-2),
# conScore <- function(i_row)
# {
# word1 <- bookP[i_row,][,-1]
# word2 <- bookP[i_row+1,][,-1]
# word3 <- bookP[i_row+2,][,-1]
#
# word1 <- unlist( word1[which(!is.na(word1) & word1 != "")] )
# word2 <- unlist( word2[which(!is.na(word2) & word2 != "")] )
# word3 <- unlist( word3[which(!is.na(word3) & word3 != "")] )
#
# sum(word1 %in% word2) + sum(word1 %in% word3)
# })
 
#Time difference of 37.63815 secs; 0.6690034
cS <- sum(consonanceScore, na.rm = TRUE)/length(book)
 
return (cS)
}
 
 
input.dir <- "F:/corpus"
filenames.v <- dir(path=input.dir, pattern=".*txt")
file.path(input.dir, filenames.v)
books <- list()
filenames <- character()
alliterationScore <- vector(mode="numeric", length=0)
consonanceScore <- vector(mode="numeric", length=0)
concretenessScore <- vector(mode="numeric", length=0)
 
for (i in seq_along(filenames.v))
{
filepath <- file.path(input.dir, filenames.v[i])
#getting books one by one
book.lines.v <- scan(filepath, what = "character", sep ="\n")
book <- paste(book.lines.v, " ") #removing lines
book.lower.v <- tolower(book)
book.words.v <- unlist(strsplit(book.lower.v, "\\W")) #remove all non-words
book.words.v <- unlist(strsplit(book.words.v, " ")) #remove all blanks; book.words.v = character vector of all words
 
alliterationScore [i] <- getAlliteration(book.words.v, CMUdict)
filenames[i] <- filenames.v[i]
consonanceScore [i] <- getConsonanceScore(book.words.v, CMUdict)
concretenessScore [i] <- getConcreteness(book.words.v, concRatings)
# books[[filenames.v[i]]] <- book.words.v
}
 
features.dataf <- data.frame(filenames, alliterationScore, consonanceScore, concretenessScore) 
