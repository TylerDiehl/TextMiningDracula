# Loading in all relevant/necessary packages for the analysis

library(gutenbergr)
library(tidyverse)
library(splitstackshape)
library(sjmisc)
library(gdata)
library(qdapRegex)
library(stopwords)
library(mgsub)
library(textstem)
library(tidytext)
library(wordcloud)

## Loading in Dracula From the Gutenberg Project

DraculaID <- gutenberg_metadata$gutenberg_id[which(gutenberg_metadata$title == "Dracula")]
gutenberg_metadata[which(gutenberg_metadata$title == "Dracula"),]

# Selecting the Dracula ID for the book/format we want to use
DraculaID <- DraculaID[1]
my_mirror <- "http://mirrors.xmission.com/gutenberg/"
# downloading book into DF
Dracula <- gutenberg_download(DraculaID, mirror = my_mirror)
Dracula[80:90,]

# This becomes a problem later in the the analysis but we'll deal with it now.
# This portion of chapter 7 is weirdly formmated, so we remove it
Dracula <- Dracula %>% slice(-3027:-3493)

#### Determining the Line Number of Each New Entry

# dates to look for
drac_dates <- "May|June|July|August|September|October|November"
# find the dates text between _ and take the postion- add 70 because
# we are looking 70 places past the start of the df
NewEntryPlace <- grep(TRUE, str_detect(rm_between(Dracula$text[71:nrow(Dracula)], '_', '_', extract=TRUE), drac_dates))
NewEntryPlace <- NewEntryPlace + 70

#### Determining the Line Number of Each New Diary/Letter Entry

# Authors to look for 
drac_auths <- "Jonathan|Mina|Seward|Lucy|Quincey|Arthur|Samuel|Carter|Agatha|Van Helsing|Gazette|Patrick|Mrs. Harker|Mitchell"
NewAuthPlace <- grep(TRUE, str_detect(rm_between(Dracula$text[71:nrow(Dracula)], '_', '_', extract=TRUE), drac_auths))
NewAuthPlace <- NewAuthPlace + 70
# Book ends with "Note" - we need to find its place for the loops we'll do
grep("NOTE", Dracula$text)
NewAuthPlace <- append(NewAuthPlace, 14897)

#### Determining the Line Number of Each Chapter

DraculaChapters <- data.frame("Chapters" = Dracula$text[44:70])

# reformatting so that we have the df how we like it
DraculaChapters <- cSplit(DraculaChapters, "Chapters", sep = ".")

DraculaChapters <- sapply(DraculaChapters, as.character)
DraculaChapters[which(is.na(DraculaChapters))] <- ""
DraculaChapters <- as.data.frame(DraculaChapters)
DraculaChapters <- data.frame('Chapter' = DraculaChapters$Chapters_1, 'Title' = paste(DraculaChapters$Chapters_2, DraculaChapters$Chapters_3))

DraculaChapters$Title <- gsub("Dr", "Dr.", DraculaChapters$Title)

head(DraculaChapters)

# Finding the line of the chapter 
ChapterPlace <- which(Dracula$text[71:nrow(Dracula)] %in% DraculaChapters$Chapter)
ChapterPlace <- ChapterPlace + 70
ChapterPlace <- append(ChapterPlace, 14897)
ChapterPlace

#### Putting it all Together

# create a df with the entry line numbers as our base, we will add
# all the supplementary info to new columns
DraculaEntries <- data.frame("NewEntryPlace" = NewEntryPlace)
# 0 at the start because of the min value
NewAuthPlace <- append(NewAuthPlace, 0, after = 0)
# define objects to be used in the for loop
MinValueHolder <- vector()
difference <- integer()
MinVal <- integer()

# for loop to find the closest new author entry line number, as long as it is
# within the same chapter
# we will take the minimum difference of each new author entry before the new dated entry
# as long as it is within the same chapter
# return chapter name if there is none
for(val in 1:length(ChapterPlace))
{
  for(val2 in 1:nrow(DraculaEntries))
  {
    if(DraculaEntries$NewEntryPlace[val2] > ChapterPlace[val] & DraculaEntries$NewEntryPlace[val2] < ChapterPlace[val + 1])
    {
      for(val3 in 1:length(NewAuthPlace))
      {
        difference <- DraculaEntries$NewEntryPlace[val2] - NewAuthPlace[val3]
        MinValueHolder <- append(MinValueHolder, difference)
        MinVal <- min(MinValueHolder[MinValueHolder > 0])
      }
      if(MinVal != DraculaEntries$NewEntryPlace[val2])
      {
        DraculaEntries$AuthorPlace[val2] <- DraculaEntries$NewEntryPlace[val2] - MinVal
      }
      else {DraculaEntries$AuthorPlace[val2] <- 0}
    }
    MinValueHolder <- vector()
  }
}
DraculaEntries[20:25,]

# same method but with chapter - find the closest chapter before
for(val in 1:nrow(DraculaEntries))
{
  for(val2 in 1:length(ChapterPlace))
  {
    difference <- DraculaEntries$NewEntryPlace[val] - ChapterPlace[val2]
    MinValueHolder <- append(MinValueHolder, difference)
  }
  MinVal <- min(MinValueHolder[MinValueHolder > 0])
  DraculaEntries$ChapterPlace[val] <-DraculaEntries$NewEntryPlace[val] - MinVal
  MinValueHolder <- vector()
}
head(DraculaEntries)

# when the author place is less than the chapter place - not in the same chapter as the entry
# we can assign it 0
DraculaEntries$AuthorPlace[which(DraculaEntries$AuthorPlace < DraculaEntries$ChapterPlace)] <- 0

#### Assigning Dates to Entries

# take the dated entries between underscores
for(val in 1:nrow(DraculaEntries))
{
  DraculaEntries$date[val] <- rm_between(Dracula$text[DraculaEntries$NewEntryPlace[val]], '_', '_', extract=TRUE)[[1]][1]
}
head(DraculaEntries)

# split the original date string we had to find entries
drac_dates_separate <- strsplit(drac_dates, "\\|")
drac_dates_separate <- unlist(drac_dates_separate)

DraculaEntries[118:127,]

# replace periods with blanks and then column split to separate columns
DraculaEntries$date <- gsub("\\.", "", DraculaEntries$date)
DraculaEntries <- cSplit(DraculaEntries, "date", sep = ",")
DraculaEntries$date_1[118] <- "2 October"
DraculaEntries$date_1[127] <- "3 October"

# for loop to find the dates by taking the month and day number
for(val in 1:nrow(DraculaEntries))
{
  for(val2 in 1:length(drac_dates_separate))
  {
  if(str_contains(DraculaEntries$date_1[val], drac_dates_separate[val2], ignore.case = T))
     {
       DraculaEntries$month[val] <- drac_dates_separate[val2]
       DraculaEntries$day[val] <- regmatches(DraculaEntries$date_1[val], gregexpr("[[:digit:]]+", DraculaEntries$date_1[val]))
     }
    else if(str_contains(DraculaEntries$date_2[val], drac_dates_separate[val2], ignore.case = T))
    {
      DraculaEntries$month[val] <- drac_dates_separate[val2]
      DraculaEntries$day[val] <- regmatches(DraculaEntries$date_2[val], gregexpr("[[:digit:]]+", DraculaEntries$date_2[val]))
    }
  }
}

# format as a date
DraculaEntries$date <- paste(DraculaEntries$month, DraculaEntries$day)
DraculaEntries$date <- as.Date(DraculaEntries$date, format = "%b %d")
DraculaEntries$date <- format(DraculaEntries$date, format = "%b-%d")
head(DraculaEntries)

#### Assigning Authors to Entries

# grabbing the auhotr for the entry
for(val in 1:nrow(DraculaEntries))
{
  if(DraculaEntries$AuthorPlace[val] != 0)
  {
    DraculaEntries$textauth[val] <- Dracula$text[DraculaEntries$AuthorPlace[val]]
  }
  else
  {DraculaEntries$textauth[val] <- Dracula$text[DraculaEntries$ChapterPlace[val]]}
}
DraculaEntries[20:25,]

# assign the chapter
DraculaEntries$Chapter <- Dracula$text[DraculaEntries$ChapterPlace]
for(val in 1:nrow(DraculaEntries))
{
  for(val2 in 1:nrow(DraculaChapters))
  {
    if(str_contains(DraculaEntries$Chapter[val], DraculaChapters$Chapter[val2]))
    {
      chap <- val2
    }
  }
  DraculaEntries$Chapter[val] <- paste(DraculaEntries$Chapter[val], DraculaChapters$Title[chap])
}
DraculaEntries[20:25,]

# list of names to find the author of each entry
DracNames <- c("Jonathan", "Mina", "Seward", "Lucy", "Quincey", "Arthur", "Samuel",
               "Carter", "Agatha", "Van Helsing", "Gazette", "Patrick", "Mrs. Harker",
               "Mitchell")
# grabs the author entry for each dated entry by looking at the author journal
# associated with the entry
for(val in 1:nrow(DraculaEntries))
{
  NameHolder <- vector()
  for(val2 in 1:length(DracNames))
  {
    if(str_contains(DraculaEntries$textauth[val], DracNames[val2]))
    {
      NameHolder <- append(NameHolder, DracNames[val2])
    }
  }
  if(length(NameHolder) > 1)
  {
    spot <- vector()
    for(val3 in 1:length(NameHolder))
    {
      spot <- append(spot, gregexpr(NameHolder[val3], DraculaEntries$textauth[val]))
    }
    if(spot[[1]][1] > spot[[2]][1])
    {
      DraculaEntries$Author[val] <- NameHolder[2]
    }
    else
    {
      DraculaEntries$Author[val] <- NameHolder[1]
    }
  }
  else
  {
    DraculaEntries$Author[val] <- NameHolder[1]
  }
}

for(val in 1:nrow(DraculaEntries))
{
  if(is.na(DraculaEntries$Author[val]))
  {
    for(val2 in 1:length(DracNames))
    {
      if(str_contains(DraculaEntries$Chapter[val], DracNames[val2]))
      {
        DraculaEntries$Author[val] <- DracNames[val2]
      }
    }
  }
}

DraculaEntries[20:25,]

#### Grabbing the Text for Each Entry

# grabbing the place of the next entry so we know where to grab text from
DraculaChapterPlace <- data.frame("Chapter" = paste(DraculaChapters$Chapter, DraculaChapters$Title, sep = " "), "Place" = ChapterPlace[-28])
for(val in 1:nrow(DraculaEntries))
{
  for(val2 in 1:nrow(DraculaChapterPlace))
  {
    if(DraculaEntries$Chapter[val] == DraculaChapterPlace$Chapter[val2])
    {
      if(DraculaEntries$ChapterPlace[val] != 14221)
      {
      DraculaEntries$NextChapterPlace[val] <- DraculaChapterPlace$Place[val2 + 1]
      }
      else
      {
        DraculaEntries$NextChapterPlace[val] <- 14897
      }
    }
  }
}

for(val in 1:(nrow(DraculaEntries)-1))
{
  Place <- ""
  for(val2 in 1:length(NewAuthPlace))
  {
    if(DraculaEntries$AuthorPlace[val] == NewAuthPlace[val2])
    {
      Place <- NewAuthPlace[val2+1]
    }
  }
  if(Place != "")
  {
    DraculaEntries$NextAuthPlace[val] <- Place
  }
  else {DraculaEntries$NextAuthPlace[val] <- DraculaEntries$AuthorPlace[val]}
}
tail(DraculaEntries)

# grabs the text from entry to entry
DraculaEntries$AuthorPlace[which(DraculaEntries$AuthorPlace == 0)] <- 99999
for(val in 1:(nrow(DraculaEntries)-1))
{
  DraculaEntries$Text[val] <- ""
  MinVal <- min(DraculaEntries$NewAuthPlace[val], DraculaEntries$NewEntryPlace[val+1],DraculaEntries$NextChapterPlace[val])
  for(val2 in DraculaEntries$NewEntryPlace[val]:(MinVal-1))
  {
    DraculaEntries$Text[val] <- paste(DraculaEntries$Text[val], Dracula$text[val2])
  }
}

DraculaEntries$Text[167] <- Dracula$text[14666:14897]

#### Cleaning the Text for Analysis

# remove starts of entries, remove symbols, lowercase everything, etc.
DraculaEntries$Text <- rm_between(DraculaEntries$Text, '_', '_', extract=F)
DraculaEntries$Text <- gsub("-|,", "", DraculaEntries$Text)
DraculaEntries$Text <- gsub("[()]", "", DraculaEntries$Text)
DraculaEntries$Text <- gsub("\'", "", DraculaEntries$Text)
DraculaEntries$Text <- gsub("P. M.", "", DraculaEntries$Text)
DraculaEntries$Text <- gsub("A. M.", "", DraculaEntries$Text)
DraculaEntriesDuplicate <- DraculaEntries

DraculaEntries$Text <- str_to_lower(DraculaEntries$Text)
DraculaEntries$Text <- gsub("[^A-Za-z///' ]", "", DraculaEntries$Text)
DraculaEntries$Text <- gsub("0|1|2|3|4|5|6|7|8|9", "", DraculaEntries$Text)

# removes stop words
stopwords <- stopwords("en")
for(val in 1:nrow(DraculaEntries))
{
  for(val2 in 1:length(stopwords))
  {
    DraculaEntries$Text[val] <- gsub(paste0("\\<", stopwords[val2], "\\>"), "", DraculaEntries$Text[val], ignore.case = T)
  }
}

DraculaEntries$Text <- str_squish(DraculaEntries$Text)

#### Lemmatization

# leverage each word to its root
# For our dataframe of the entries with the full text
for(val in 1:nrow(DraculaEntries))
{
  Lem <- str_split(paste(DraculaEntries$Text)[val], " ")[[1]]
  DraculaEntries$Text[val] <- paste(lemmatize_words(Lem), collapse = " ")
}
# For individual words
DracAuthWords <- DraculaEntries %>% mutate(
  linenumber = row_number()) %>%
  ungroup() %>%
  unnest_tokens(word, Text)
DracAuthWords$word <- lemmatize_words(DracAuthWords$word)

#### Removing Frequent Words

# count frequent words
dracwords <- DracAuthWords %>% count(word, sort=TRUE)
head(dracwords,15)

# object with most frequent words we want to remove, then removes them
mostfreqwords <- dracwords$word[c(1:4, 6:10)]
# For the words and count
dracwords <- dracwords[-c(1:4, 6:10),]
# For the df of individual words
for(val in 1:nrow(DracAuthWords))
{
  if(sum(DracAuthWords$word[val] == mostfreqwords) == 1)
  {
    DracAuthWords$word[val] <- "Remove Me"
  }
}
DracAuthWords <- DracAuthWords[which(DracAuthWords$word != "Remove Me"),]
# For the entries and full text
for(val in 1:nrow(DraculaEntries))
{
  for(val2 in 1:length(mostfreqwords))
  {
    DraculaEntries$Text[val] <- gsub(paste0("\\<", mostfreqwords[val2], "\\>"), "", DraculaEntries$Text[val], ignore.case = T)
  }
}

DraculaEntries$Text <- str_squish(DraculaEntries$Text)

## Analysis and Visualization

### Average Word Length

# finds the mean word length for each author
JAWL <- mean(nchar(DracAuthWords$word[which(DracAuthWords$Author == "Jonathan")]))
LAWL <- mean(nchar(DracAuthWords$word[which(DracAuthWords$Author == "Lucy")]))
SAWL <- mean(nchar(DracAuthWords$word[which(DracAuthWords$Author == "Seward")]))
MAWL <- mean(nchar(DracAuthWords$word[which(DracAuthWords$Author == "Mina")]))

# binds together to form a table
AWLTable <- cbind(JAWL, LAWL, SAWL, MAWL)
colnames(AWLTable) <- c("Jonathan", "Lucy", "Seward", "Mina")
row.names(AWLTable) <- "Average Word Length"
AWLTable

#### Testing Word Length

# one way anova test of average word length between authors
DracAuthWords$WL <- nchar(DracAuthWords$word)
AWL.aov <- aov(WL ~ Author, data = DracAuthWords[which(DracAuthWords$Author == "Jonathan" | 
                                                         DracAuthWords$Author == "Lucy" | 
                                                         DracAuthWords$Author == "Seward" |
                                                         DracAuthWords$Author == "Mina"),])
summary(AWL.aov)

### Average Sentence Length

# get the sentences of all the text
DracSentences <- DraculaEntriesDuplicate %>% mutate(
  linenumber = row_number()) %>%
  ungroup() %>%
  unnest_sentences(Sentence, Text)
DracSentences$Sentence <- str_squish(DracSentences$Sentence)

# find the sentences pertaining to each author, 
# then separate each word into its own row
# count length of each sentence and take mean
JonathanSentences <- DracSentences[which(DracSentences$Author == "Jonathan"),]
JonathanSentences <- JonathanSentences %>% mutate(
  sentencenumber = row_number()) %>%
  ungroup() %>%
  unnest_tokens(word, Sentence)
JonathanWPS <- JonathanSentences %>% group_by(sentencenumber) %>% 
  summarise(n = length(word))
JWPS <- mean(JonathanWPS$n)

LucySentences <- DracSentences[which(DracSentences$Author == "Lucy"),]
LucySentences <- LucySentences %>% mutate(
  sentencenumber = row_number()) %>%
  ungroup() %>%
  unnest_tokens(word, Sentence)
LucyWPS <- LucySentences %>% group_by(sentencenumber) %>% 
  summarise(n = length(word))
LWPS <- mean(LucyWPS$n)

SewardSentences <- DracSentences[which(DracSentences$Author == "Seward"),]
SewardSentences <- SewardSentences %>% mutate(
  sentencenumber = row_number()) %>%
  ungroup() %>%
  unnest_tokens(word, Sentence)
SewardWPS <- SewardSentences %>% group_by(sentencenumber) %>% 
  summarise(n = length(word))
SWPS <- mean(SewardWPS$n)

MinaSentences <- DracSentences[which(DracSentences$Author == "Mina"),]
MinaSentences <- MinaSentences %>% mutate(
  sentencenumber = row_number()) %>%
  ungroup() %>%
  unnest_tokens(word, Sentence)
MinaWPS <- MinaSentences %>% group_by(sentencenumber) %>% 
  summarise(n = length(word))
MWPS <- mean(MinaWPS$n)

# join each author's words per sentence into a table
WPSTable <- cbind(JWPS, LWPS, SWPS, MWPS)
colnames(WPSTable) <- c("Jonathan", "Lucy", "Seward", "Mina")
row.names(WPSTable) <- "Words Per Sentence"
WPSTable


#### Testing Average Sentence Length

# one way anova test for WPS for the authors
JonathanWPS$Author <- "Jonathan"
LucyWPS$Author <- "Lucy"
SewardWPS$Author <- "Seward"
MinaWPS$Author <-"Mina"

DracWPS <- bind_rows(JonathanWPS, LucyWPS, SewardWPS, MinaWPS)

ASL.aov <- aov(n ~ Author, data = DracWPS[which(DracWPS$Author == "Jonathan" | 
                                                  DracWPS$Author == "Lucy" | 
                                                  DracWPS$Author == "Seward" |
                                                  DracWPS$Author == "Mina"),])
summary(ASL.aov)

# plot the average sentence length vs average word length for each author
plot(as.vector(WPSTable), as.vector(AWLTable), pch = 19, main = "Average Sentence Length vs. Average Word Length",
     xlab = "Average World Length", ylab = "Average Sentence Length", xlim = c(17, 20))
text(x=as.vector(WPSTable), y = as.vector(AWLTable), labels = c("Jonathan", "Lucy", "Seward", "Mina"), pos = 4)

# correlation
cor(as.vector(WPSTable), as.vector(AWLTable))

# correlation without seward
cor(as.vector(WPSTable)[-3], as.vector(AWLTable)[-3])

#### Graphing the Words Per Sentence

# histogram of words per sentence 
JH <- hist(JonathanWPS$n, col = "red", xlim = c(0,100), main = "Jonathan Words Per Sentence", xlab = "Words Per Sentence", breaks = 20)
LH <- hist(LucyWPS$n, col = "blue", xlim = c(0,100), main = "Lucy Words Per Sentence", xlab = "Words Per Sentence", breaks = 20)
SH <- hist(SewardWPS$n, col = "orange", xlim = c(0,100), main = "Seward Words Per Sentence", xlab = "Words Per Sentence", breaks = 20)
MH <- hist(MinaWPS$n, col = "purple", xlim = c(0,100), main = "Mina Words Per Sentence", xlab = "Words Per Sentence", breaks = 40)

# overlapping histogram
plot(SH, col = "orange", main = "Histogram of Words Per Sentence by Author", xlab = "Words Per Sentence")
plot(JH, col = "red", add = T)
plot(MH, col = "purple", add = T)
plot(LH, col = "blue", add = T)
legend("topright", legend = c("Seward", "Mina", "Jonathan", "Lucy"), col = c("orange", "purple", "red", "blue"), pch = 19)

### Sentence Type

# take the total number of sentences for each type
AuthorSentenceType <- DracSentences %>%
  group_by(Author) %>%
  summarise(Declarative = sum(endsWith(Sentence, ".")),
            Interrogative = sum(endsWith(Sentence, "?")),
            Exclamatory = sum(endsWith(Sentence, "!")))
# only grab 4 main authors
AuthorSentenceType <- AuthorSentenceType[which(AuthorSentenceType$Author == "Jonathan" | 
                                                 AuthorSentenceType$Author == "Lucy" |
                                                 AuthorSentenceType$Author == "Seward" |
                                                 AuthorSentenceType$Author == "Mina"),]
AuthorSentenceType

# convert to percentages
sumrows <- rowSums(AuthorSentenceType[,-1])
AuthorSentenceType$Declarative <- AuthorSentenceType$Declarative / sumrows
AuthorSentenceType$Interrogative <- AuthorSentenceType$Interrogative / sumrows
AuthorSentenceType$Exclamatory <- AuthorSentenceType$Exclamatory / sumrows
AuthorSentenceType

#### Visualizing Sentence Type
barplot(as.matrix(t(AuthorSentenceType[,-1])), main = "Sentence Type", col = c("aquamarine", "antiquewhite", "aliceblue" ),
names.arg = c("Jonathan", "Lucy", "Mina", "Seward"), ylab = "Proportion", 
legend.text = colnames(AuthorSentenceType[,-1]), args.legend = list(x = "bottomright", inset = c(.4, .4)))

### Visualizing with Wordclouds

##### Full Text

wordcloud(words = dracwords$word, freq = dracwords$n, random.order = F,
          rot.per = .35 , colors=brewer.pal(8, "Reds"), max.words = 100)

##### Jonathan

JonathanText <- DraculaEntries[which(DraculaEntries$Author == "Jonathan"),] %>% mutate(
  linenumber = row_number()) %>%
  ungroup() %>%
  unnest_tokens(word, Text)
JonathanWords <- JonathanText %>% count(word, sort=TRUE)
wordcloud(words = JonathanWords$word, freq = JonathanWords$n, random.order = F,
          rot.per = .35 , colors=brewer.pal(8, "OrRd"), max.words = 100)

#### Most Used Words For Jonathan


head(JonathanWords, 10)

##### Lucy

LucyText <- DraculaEntries[which(DraculaEntries$Author == "Lucy"),] %>% mutate(
  linenumber = row_number()) %>%
  ungroup() %>%
  unnest_tokens(word, Text)
LucyWords <- LucyText %>% count(word, sort=TRUE)
wordcloud(words = LucyWords$word, freq = LucyWords$n, random.order = F,
          rot.per = .35 , colors=brewer.pal(8, "YlGnBu"), max.words = 100)

#### Most Used Words For Lucy

head(LucyWords, 10)

##### Seward

SewardText <- DraculaEntries[which(DraculaEntries$Author == "Seward"),] %>% mutate(
  linenumber = row_number()) %>%
  ungroup() %>%
  unnest_tokens(word, Text)
SewardWords <- SewardText %>% count(word, sort=TRUE)
wordcloud(words = SewardWords$word, freq = SewardWords$n, random.order = F,
          rot.per = .35 , colors=brewer.pal(8, "RdPu"), max.words = 100)

#### Most Used Words For Seward

head(SewardWords, 10)

##### Mina

MinaText <- DraculaEntries[which(DraculaEntries$Author == "Mina"),] %>% mutate(
  linenumber = row_number()) %>%
  ungroup() %>%
  unnest_tokens(word, Text)
MinaWords <- MinaText %>% count(word, sort=TRUE)
wordcloud(words = MinaWords$word, freq = MinaWords$n, random.order = F,
          rot.per = .35 , colors=brewer.pal(8, "PuBuGn"), max.words = 100)

#### Most Used Words For Mina

head(MinaWords, 10)

### Sentiment Analysis

#### Full Text

# grab the sentiment of each word , sum for each entry
Drac_sentiment <- DracAuthWords %>%
  inner_join(get_sentiments("bing")) %>%
  count(linenumber, sentiment) %>%
  spread(sentiment, n, fill = 0) %>% 
  mutate(sentiment = positive - negative)
Drac_sentiment$date <- DraculaEntries$date[Drac_sentiment$linenumber]
Drac_sentiment$date <- as.Date(Drac_sentiment$date, format = "%b-%d")
Drac_sentiment <- Drac_sentiment[order(Drac_sentiment$date),]

# plot the sentiment over each entry
max(Drac_sentiment$sentiment)
min(Drac_sentiment$sentiment)
ggplot(Drac_sentiment, aes(x = linenumber, y = sentiment, fill = sentiment)) + geom_col(show.legend = FALSE) + 
  scale_fill_gradient2(low = "blue", mid = "white" , high = "red", limit = c(-134,34)) + theme_dark() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  xlab("Date") + ylab("Sentiment")

#### Jonathan

Jonathan_sentiment <- DracAuthWords[which(DracAuthWords$Author == "Jonathan"),] %>%
  inner_join(get_sentiments("bing")) %>%
  count(linenumber, sentiment) %>%
  spread(sentiment, n, fill = 0) %>% 
  mutate(sentiment = positive - negative)
Jonathan_sentiment$row <- 1:nrow(Jonathan_sentiment)

max(Jonathan_sentiment$sentiment)
min(Jonathan_sentiment$sentiment)
ggplot(Jonathan_sentiment, aes(row, sentiment, fill = sentiment)) + geom_col(show.legend = FALSE) + 
  scale_fill_gradient2(low = "blue", mid = "white" , high = "red", limit = c(-94,24)) + theme_dark() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  xlab("Entry") + ylab("Sentiment")

#### Lucy

Lucy_sentiment <- DracAuthWords[which(DracAuthWords$Author == "Lucy"),] %>%
  inner_join(get_sentiments("bing")) %>%
  count(linenumber, sentiment) %>%
  spread(sentiment, n, fill = 0) %>% 
  mutate(sentiment = positive - negative)
Lucy_sentiment$row <- 1:nrow(Lucy_sentiment)

max(Lucy_sentiment$sentiment)
min(Lucy_sentiment$sentiment)
ggplot(Lucy_sentiment, aes(row, sentiment, fill = sentiment)) + geom_col(show.legend = FALSE) + 
  scale_fill_gradient2(low = "blue", mid = "white" , high = "red", limit = c(-59,34)) + theme_dark() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  xlab("Entry") + ylab("Sentiment")

#### Seward

Seward_sentiment <- DracAuthWords[which(DracAuthWords$Author == "Seward"),] %>%
  inner_join(get_sentiments("bing")) %>%
  count(linenumber, sentiment) %>%
  spread(sentiment, n, fill = 0) %>% 
  mutate(sentiment = positive - negative)
Seward_sentiment$row <- 1:nrow(Seward_sentiment)

max(Seward_sentiment$sentiment)
min(Seward_sentiment$sentiment)
ggplot(Seward_sentiment, aes(row, sentiment, fill = sentiment)) + geom_col(show.legend = FALSE) + 
  scale_fill_gradient2(low = "blue", mid = "white" , high = "red", limit = c(-134,26)) + theme_dark() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  xlab("Entry") + ylab("Sentiment")

#### Mina

Mina_sentiment <- DracAuthWords[which(DracAuthWords$Author == "Mina"),] %>%
  inner_join(get_sentiments("bing")) %>%
  count(linenumber, sentiment) %>%
  spread(sentiment, n, fill = 0) %>% 
  mutate(sentiment = positive - negative)
Mina_sentiment$row <- 1:nrow(Mina_sentiment)

max(Mina_sentiment$sentiment)
min(Mina_sentiment$sentiment)
ggplot(Mina_sentiment, aes(row, sentiment, fill = sentiment)) + geom_col(show.legend = FALSE) + 
  scale_fill_gradient2(low = "blue", mid = "white" , high = "red", limit = c(-39,27)) + theme_dark() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  xlab("Entry") + ylab("Sentiment")

### Sentiment Towards Dracula

# find sentences with dracula
dracplace <- vector()
for(val in 1:nrow(DracSentences))
{
  if(str_detect(DracSentences$Sentence[val], " count |\\count | dracula |\\dracula ")
     | str_detect(DracSentences$Sentence[val], fixed("count.")) 
     | str_detect(DracSentences$Sentence[val], fixed("dracula.")))
  {
    dracplace <- append(dracplace, val)
  }
}

# sentiment for dracula
DracAppears <- DracSentences[dracplace,]
DracAppears <- DracAppears %>% mutate(
  sentencenumber = dracplace) %>%
  ungroup() %>%
  unnest_tokens(word, Sentence)
DracAppears_sentiment <- DracAppears %>%
  inner_join(get_sentiments("bing")) %>%
  count(sentencenumber, sentiment) %>%
  spread(sentiment, n, fill = 0) %>% 
  mutate(sentiment = positive - negative)
DracAppears_sentiment$n <- 1:nrow(DracAppears_sentiment)

max(DracAppears_sentiment$sentiment)
min(DracAppears_sentiment$sentiment)
ggplot(DracAppears_sentiment, aes(n, sentiment, fill = sentiment)) + geom_col(show.legend = FALSE) + 
  scale_fill_gradient2(low = "blue", mid = "white" , high = "red", limit = c(-5,3)) + theme_dark() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  xlab("Point in Time") + ylab("Sentiment")

DracAppears_sentiment[which.min(DracAppears_sentiment$sentiment),]
DracSentences$Sentence[5923]

#### Jonathan

JonathanToDrac <- DracAppears[which(DracAppears$Author == "Jonathan"),]
JonathanToDracSent <- JonathanToDrac %>%
  inner_join(get_sentiments("bing")) %>%
  count(sentencenumber, sentiment) %>%
  spread(sentiment, n, fill = 0) %>% 
  mutate(sentiment = positive - negative)
JonathanToDracSent$n <- 1:nrow(JonathanToDracSent)

max(JonathanToDracSent$sentiment)
min(JonathanToDracSent$sentiment)
ggplot(JonathanToDracSent, aes(n, sentiment, fill = sentiment)) + geom_col(show.legend = FALSE) + 
  scale_fill_gradient2(low = "blue", mid = "white" , high = "red", limit = c(-5,3)) + theme_dark() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  xlab("Point in Time") + ylab("Sentiment")

DracSentences$Sentence[6876]

#### Seward

SewardToDrac <- DracAppears[which(DracAppears$Author == "Seward"),]
SewardToDracSent <- SewardToDrac %>%
  inner_join(get_sentiments("bing")) %>%
  count(sentencenumber, sentiment) %>%
  spread(sentiment, n, fill = 0) %>% 
  mutate(sentiment = positive - negative)
SewardToDracSent$n <- 1:nrow(SewardToDracSent)

max(SewardToDracSent$sentiment)
min(SewardToDracSent$sentiment)
ggplot(SewardToDracSent, aes(n, sentiment, fill = sentiment)) + geom_col(show.legend = FALSE) + 
  scale_fill_gradient2(low = "blue", mid = "white" , high = "red", limit = c(-5,3)) + theme_dark() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  xlab("Point in Time") + ylab("Sentiment")

#### Mina

MinaToDrac <- DracAppears[which(DracAppears$Author == "Mina"),]
MinaToDracSent <- MinaToDrac %>%
  inner_join(get_sentiments("bing")) %>%
  count(sentencenumber, sentiment) %>%
  spread(sentiment, n, fill = 0) %>% 
  mutate(sentiment = positive - negative)
MinaToDracSent$n <- 1:nrow(MinaToDracSent)

max(MinaToDracSent$sentiment)
min(MinaToDracSent$sentiment)
ggplot(MinaToDracSent, aes(n, sentiment, fill = sentiment)) + geom_col(show.legend = FALSE) + 
  scale_fill_gradient2(low = "blue", mid = "white" , high = "red", limit = c(-5,3)) + theme_dark() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  xlab("Point in Time") + ylab("Sentiment")

JonathanSentences.v2 <- JonathanSentences %>% 
  group_by(sentencenumber) %>% summarise(n = length(word), EntryLineNumber = NewEntryPlace)
JonathanSentences.v2 <- unique(JonathanSentences.v2)
JonathanSentences.v2 <- JonathanSentences.v2 %>%
  group_by(EntryLineNumber) %>% summarise(mean = mean(n))
plot(1:42, JonathanSentences.v2$mean, type = "l", xlab = "Entry", 
     ylab = "Average Words Per Sentence", xaxt = "n",
     main = "Average Words Per Sentence for Jonathan")
t = 1:42
model <- lm(mean ~ t, data = JonathanSentences.v2)
lines(t, model$fitted.values, col = "red")

SewardSentences.v2 <- SewardSentences %>% 
  group_by(sentencenumber) %>% summarise(n = length(word), EntryLineNumber = NewEntryPlace)
SewardSentences.v2 <- unique(SewardSentences.v2)
SewardSentences.v2 <- SewardSentences.v2 %>%
  group_by(EntryLineNumber) %>% summarise(mean = mean(n))
plot(1:60, SewardSentences.v2$mean, type = "l", xlab = "Entry", 
     ylab = "Average Words Per Sentence", xaxt = "n",
     main = "Average Words Per Sentence for Seward")
t2 = 1:60
model2 <- lm(mean ~ t2, data = SewardSentences.v2)
lines(t2, model2$fitted.values, col = "red")

MinaSentences.v2 <- MinaSentences %>% 
  group_by(sentencenumber) %>% summarise(n = length(word), EntryLineNumber = NewEntryPlace)
MinaSentences.v2 <- unique(MinaSentences.v2)
MinaSentences.v2 <- MinaSentences.v2 %>%
  group_by(EntryLineNumber) %>% summarise(mean = mean(n))
plot(1:41, MinaSentences.v2$mean, type = "l", xlab = "Entry", 
     ylab = "Average Words Per Sentence", xaxt = "n",
     main = "Average Words Per Sentence for Mina")
t3 = 1:41
model2 <- lm(mean ~ t3, data = MinaSentences.v2)
lines(t3, model2$fitted.values, col = "red")
# maybe variations are due to who she's writing to

