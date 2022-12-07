library(tidyverse)
library(tm)
#library(wordcloud)
library(e1071)
library(caret)
library(lsa)

data <- read_csv(".//Fall 2022//ST495//final.csv")

filtered <- data %>% 
  filter(is.na(ERROR_DESCRIPTION) == FALSE) %>% 
  group_by(DEVID) %>%
  summarize(Desc = ERROR_DESCRIPTION, Name = ERROR_LEVEL_NAME)

filtered <- filtered %>% 
  distinct(Desc, .keep_all = TRUE)

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


#Plotting Categories
ggplot(filtered, aes(Name)) + 
  geom_bar()

ggplot(filtered, aes(Cat)) + 
  geom_bar()


#Document-Term Matrix
dtm1 <- DocumentTermMatrix(filtered$Desc, control = list(weighting = function(x)
  weightTfIdf(x, normalize = TRUE), stopwords = TRUE))

data1 <- data.frame(as.matrix(dtm1))

#Finding Frequent Terms
findFreqTerms(dtm1, 0.7)

#Removing Sparse Terms
dtm1.1 <- removeSparseTerms(dtm1, 0.90)


## Model without LSA
true.labs <- filtered$Cat
foo <- as.matrix(dtm1.1)
data_final <- data.frame(true.labs, foo)
data_final$true.labs <- as.factor(data_final$true.labs)


precision_mtx <- matrix(nrow = 3, ncol = 100)
recall_mtx <- matrix(nrow = 3, ncol = 100)
mtx_F1 <- matrix(nrow = 3, ncol = 100)
accuracy <- matrix(nrow = 3, ncol = 100)

#Model Creation
set.seed(2022)
for(i in 1:100){
  ind <- sample(2, nrow(filtered), replace=TRUE, prob=c(0.70, 0.30))
  train <- data_final[ind == 1,]
  test <- data_final[ind==2,]
  
  svm.model = svm(true.labs~., data = train, type="C")
  svm.pred = predict(svm.model, test[,-1])
  
  z <- confusionMatrix(svm.pred, test$true.labs,
                  mode = "everything",
                  positive="1")
  precision_mtx[1,i] <- z$byClass[13]
  precision_mtx[2,i] <- z$byClass[14]
  precision_mtx[3,i] <- z$byClass[15]
  
  recall_mtx[1,i] <- z$byClass[16]
  recall_mtx[2,i] <- z$byClass[17]
  recall_mtx[3,i] <- z$byClass[18]
  
  mtx_F1[1,i] <- z$byClass[19]
  mtx_F1[2,i] <- z$byClass[20]
  mtx_F1[3,i] <- z$byClass[21]
  
  accuracy[1,i] <- z$byClass[31]
  accuracy[2,i] <- z$byClass[32]
  accuracy[3,i] <- z$byClass[33]
}


## Results

#Accuracy 
mean(accuracy[1,], na.rm = TRUE)
mean(accuracy[2,], na.rm = TRUE)
mean(accuracy[3,], na.rm = TRUE)

#Precision
mean(precision_mtx[1,], na.rm = TRUE)
mean(precision_mtx[2,], na.rm = TRUE)
mean(precision_mtx[3,], na.rm = TRUE)

#Recall
mean(recall_mtx[1,], na.rm = TRUE)
mean(recall_mtx[2,], na.rm = TRUE)
mean(recall_mtx[3,], na.rm = TRUE)

#F1
mean(mtx_F1[1,], na.rm = TRUE)
mean(mtx_F1[2,], na.rm = TRUE)
mean(mtx_F1[3,], na.rm = TRUE)




#Create Term-document Matrix
tdm = TermDocumentMatrix(filtered$Desc, control = list(weighting = function(x) weightTfIdf(x, normalize = TRUE)))
lsaSpace <- lsa(tdm)  # create LSA space
lsak = lsaSpace$dk #Docs  decomposed to k dimensions
dim(lsak)
lsaSpace$sk # these are like the variance
plot(lsaSpace$sk) # scree plot

final <- data.frame(true.labs,lsak)
final <- subset(final, select = -X1)
final$true.labs <- as.factor(final$true.labs)

precision_mtx2 <- matrix(nrow = 3, ncol = 100)
recall_mtx2 <- matrix(nrow = 3, ncol = 100)
mtx_F12 <- matrix(nrow = 3, ncol = 100)
accuracy2 <- matrix(nrow = 3, ncol = 100)

#Model Creation
set.seed(2022)
for(i in 1:100){
  ind <- sample(2, nrow(filtered), replace=TRUE, prob=c(0.70, 0.30))
  train <- final[ind == 1,]
  test <- final[ind==2,]
  
  svm.model = svm(true.labs~., data = train, type="C")
  svm.pred = predict(svm.model, test[,-1])
  
  z <- confusionMatrix(svm.pred, test$true.labs,
                       mode = "everything",
                       positive="1")
  
  precision_mtx2[1,i] <- z$byClass[13]
  precision_mtx2[2,i] <- z$byClass[14]
  precision_mtx2[3,i] <- z$byClass[15]
  
  recall_mtx2[1,i] <- z$byClass[16]
  recall_mtx2[2,i] <- z$byClass[17]
  recall_mtx2[3,i] <- z$byClass[18]
  
  mtx_F12[1,i] <- z$byClass[19]
  mtx_F12[2,i] <- z$byClass[20]
  mtx_F12[3,i] <- z$byClass[21]
  
  accuracy2[1,i] <- z$byClass[31]
  accuracy2[2,i] <- z$byClass[32]
  accuracy2[3,i] <- z$byClass[33]
}


## Results

#Accuracy 
mean(accuracy2[1,], na.rm = TRUE)
mean(accuracy2[2,], na.rm = TRUE)
mean(accuracy2[3,], na.rm = TRUE)

#Precision
mean(precision_mtx2[1,], na.rm = TRUE)
mean(precision_mtx2[2,], na.rm = TRUE)
mean(precision_mtx2[3,], na.rm = TRUE)

#Recall
mean(recall_mtx2[1,], na.rm = TRUE)
mean(recall_mtx2[2,], na.rm = TRUE)
mean(recall_mtx2[3,], na.rm = TRUE)

#F1
mean(mtx_F12[1,], na.rm = TRUE)
mean(mtx_F12[2,], na.rm = TRUE)
mean(mtx_F12[3,], na.rm = TRUE)

