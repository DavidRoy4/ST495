library(tidyverse)
library(tm)
library(wordcloud)

setwd("C:\\Users\\Sherlock\\Documents")
data <- read_csv(".//Fall 2022//ST495//final.csv")

filtered <- data %>% 
  filter(is.na(ERROR_DESCRIPTION) == FALSE) %>% 
  group_by(DEVID) %>%
  summarize(Desc = ERROR_DESCRIPTION, Name = ERROR_LEVEL_NAME)

filtered <- filtered %>% distinct(Desc, .keep_all = TRUE)

## Data Cleaning
filtered$Desc <- gsub('[.,(,),,]','', filtered$Desc)
filtered$Desc <- tolower(filtered$Desc)

ggplot(filtered, aes(Name)) + 
  geom_bar()



dtm1 <- DocumentTermMatrix(filtered$Desc, control = list(weighting = function(x)
  weightTfIdf(x, normalize = TRUE), stopwords = TRUE))
data1 <- data.frame(as.matrix(dtm1))

findFreqTerms(dtm1, 0.7)

findAssocs(dtm1, "radiation", 0.7)

dtm1.1 <- removeSparseTerms(dtm1, 0.5)

# construct wordcloud
m <- as.matrix(dtm1.1)
v <- sort(colSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)

set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=20, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

# LSA
library(lsa)
tdm = TermDocumentMatrix(filtered, control = list(weighting = function(x) weightTfIdf(x, normalize = TRUE), wordLengths=c(2,Inf)))
lsaSpace <- lsa(tdm)  # create LSA space
lsak = lsaSpace$dk #Docs  decomposed to k dimensions
dim(lsak)
lsaSpace$sk # these are like the variance
plot(lsaSpace$sk) # scree plot

final <- data.frame(tdm)

# classification
