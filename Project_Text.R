library(tidyverse)
library(tm)
library(wordcloud)
library(e1071)

data <- read_csv(".//Fall 2022//ST495//final.csv")

filtered <- data %>% 
  filter(is.na(ERROR_DESCRIPTION) == FALSE) %>% 
  group_by(DEVID) %>%
  summarize(Desc = ERROR_DESCRIPTION, Name = ERROR_LEVEL_NAME)

filtered <- filtered %>% distinct(Desc, .keep_all = TRUE)

## Data Cleaning
filtered$Desc <- gsub('[.,(,),,]','', filtered$Desc)
filtered$Desc <- tolower(filtered$Desc)

filtered <- filtered %>% mutate(Cat = case_when(
                                              Name == "Billing" ~ "Standards",
                                              Name == "Quality Assurance" ~ "Standards",
                                              Name == "Radiation Safety" ~ "Standards",
                                              Name == "Scheduling" ~ "Administrative",
                                              Name == "Registration" ~ "Administrative",
                                              Name == "Patient Docs/Notes" ~ "Administrative",
                                              Name == "Computer Tx Planning" ~ "Treatment Preparation",
                                              Name == "CT Simulation" ~ "Treatment Preparation",
                                              Name == "Diagnostic CT" ~ "Treatment Preparation"
))


ggplot(filtered, aes(Name)) + 
  geom_bar()

ggplot(filtered, aes(Cat)) + 
  geom_bar()


dtm1 <- DocumentTermMatrix(filtered$Desc, control = list(weighting = function(x)
  weightTfIdf(x, normalize = TRUE), stopwords = TRUE))
data1 <- data.frame(as.matrix(dtm1))

findFreqTerms(dtm1, 0.7)

findAssocs(dtm1, "radiation", 0.7)

dtm1.1 <- removeSparseTerms(dtm1, 0.92)

#Model without LSA
true.labs <- filtered$Cat
foo <- as.matrix(dtm1.1)
data_final <- data.frame(true.labs, foo)
data_final$true.labs <- as.factor(data_final$true.labs)


results <- matrix(nrow = 4, ncol = 100)

set.seed(2022)
for(i in 1:100){
  ind <- sample(2, nrow(filtered), replace=TRUE, prob=c(0.70, 0.30))
  train <- data_final[ind == 1,]
  test <- data_final[ind==2,]

  svm.model = svm(true.labs~., data = train, type="C")
  svm.pred = predict(svm.model, test[,-1])

  results[1,i] <- 1 - mean(svm.pred != test$true.labs)
  results[2,i] <- table(svm.pred, test$true.labs)[1]/sum(table(svm.pred, test$true.labs)[1:3])
  results[3,i] <- table(svm.pred, test$true.labs)[5]/sum(table(svm.pred, test$true.labs)[4:6])
  results[4,i] <- table(svm.pred, test$true.labs)[9]/sum(table(svm.pred, test$true.labs)[7:9])
}

#Accuracy
mean(results[1,])
mean(results[2,])
mean(results[3,])
mean(results[4,], na.rm = TRUE)

#Recall

# LSA
library(lsa)
tdm = DocumentTermMatrix(filtered, control = list(weighting = function(x) weightTfIdf(x, normalize = TRUE), wordLengths=c(2,Inf)))
lsaSpace <- lsa(tdm)  # create LSA space
lsak = lsaSpace$tk #Docs  decomposed to k dimensions
dim(lsak)
lsaSpace$sk # these are like the variance
plot(lsaSpace$sk) # scree plot

final <- data.frame(true.labs,lsak)

# construct wordcloud
m <- as.matrix(dtm1.1)
v <- sort(colSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)

wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=20, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))
