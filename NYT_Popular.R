library(tm)
library(SnowballC)
library(tm)
library(ROCR)
library(rpart)
library(rpart.plot)
library(caret)
library(e1071)
library(randomForest)

#Reading Train and Test set
NewsTrain = read.csv("NYTimesBlogTrain.csv", stringsAsFactors=FALSE)
NewsTest = read.csv("NYTimesBlogTest.csv", stringsAsFactors=FALSE)

#Formatting of date using strptime function
NewsTrain$PubDate = strptime(NewsTrain$PubDate, "%Y-%m-%d %H:%M:%S")
NewsTest$PubDate = strptime(NewsTest$PubDate, "%Y-%m-%d %H:%M:%S")

#Imputation Train
vector = which(NewsTrain[NewsTrain$NewsDesk=="",][,2] == "Business Day")
NewsTrain[NewsTrain$NewsDesk=="",][,1][vector] = "Business"
vector = which(NewsTrain[NewsTrain$NewsDesk=="",][,2] == "Crosswords/Games")
NewsTrain[NewsTrain$NewsDesk=="",][,1][vector] = "Business"
#vector = which(NewsTrain[NewsTrain$NewsDesk=="",][,2] == "Health")
#NewsTrain[NewsTrain$NewsDesk=="",][,1][vector] = "Science"
vector = which(NewsTrain[NewsTrain$NewsDesk=="",][,2] == "Opinion")
NewsTrain[NewsTrain$NewsDesk=="",][,1][vector] = "OpEd"
vector = which(NewsTrain[NewsTrain$NewsDesk=="",][,2] == "Travel")
NewsTrain[NewsTrain$NewsDesk=="",][,1][vector] = "Travel"

for (i in seq_len(nrow(NewsTrain)))
{
  if(NewsTrain$NewsDesk[i]=="Styles" & NewsTrain$SectionName[i]=="")
  {
    NewsTrain$SectionName[i]="U.S."
  }  
}

for (i in seq_len(nrow(NewsTrain)))
{
  if(NewsTrain$NewsDesk[i]=="Sports")
  {
    NewsTrain$SectionName[i]="Sports"
  }  
}

for (i in seq_len(nrow(NewsTrain)))
{
  if(NewsTrain$NewsDesk[i]=="Science")
  {
    NewsTrain$SectionName[i]="Health"
  }  
}

for (i in seq_len(nrow(NewsTrain)))
{
  if(NewsTrain$NewsDesk[i]=="OpEd")
  {
    NewsTrain$SectionName[i]="Opinion"
  }  
}

for (i in seq_len(nrow(NewsTrain)))
{
  if(NewsTrain$NewsDesk[i]=="National")
  {
    NewsTrain$SectionName[i]="U.S."
    NewsTrain$SubsectionName[i] = "Politics"
  }  
}

for (i in seq_len(nrow(NewsTrain)))
{
  if(NewsTrain$NewsDesk[i]=="Foreign")
  {
    NewsTrain$SectionName[i]="World"
    NewsTrain$SubsectionName[i]="Asia Pacific"
  }  
}

for (i in seq_len(nrow(NewsTrain)))
{
  if(NewsTrain$NewsDesk[i]=="Culture")
  {
    NewsTrain$SectionName[i]="Arts"
  }  
}

#Imputation Test
vector = which(NewsTest[NewsTest$NewsDesk=="",][,2] == "Business Day")
NewsTest[NewsTest$NewsDesk=="",][,1][vector] = "Business"
vector = which(NewsTest[NewsTest$NewsDesk=="",][,2] == "Crosswords/Games")
NewsTest[NewsTest$NewsDesk=="",][,1][vector] = "Business"
#vector = which(NewsTest[NewsTest$NewsDesk=="",][,2] == "Health")
#NewsTest[NewsTest$NewsDesk=="",][,1][vector] = "Science"
vector = which(NewsTest[NewsTest$NewsDesk=="",][,2] == "Arts")
NewsTest[NewsTest$NewsDesk=="",][,1][vector] = "Culture"
vector = which(NewsTest[NewsTest$NewsDesk=="",][,2] == "N.Y. / Region")
NewsTest[NewsTest$NewsDesk=="",][,1][vector] = "Metro"
vector = which(NewsTest[NewsTest$NewsDesk=="",][,2] == "Technology")
NewsTest[NewsTest$NewsDesk=="",][,1][vector] = "Business"
vector = which(NewsTest[NewsTest$NewsDesk=="",][,2] == "Travel")
NewsTest[NewsTest$NewsDesk=="",][,1][vector] = "Travel"
vector = which(NewsTest[NewsTest$NewsDesk=="",][,2] == "World")
NewsTest[NewsTest$NewsDesk=="",][,1][vector] = "Foreign"
vector = which(NewsTest[NewsTest$NewsDesk=="",][,2] == "Opinion")
NewsTest[NewsTest$NewsDesk=="",][,1][vector] = "OpEd"
NewsTest$NewsDesk[784] = "Styles"
NewsTest$NewsDesk[944] = "Styles"
vector = which(NewsTest[NewsTest$NewsDesk=="",][,3] == "Room For Debate")
NewsTest[NewsTest$NewsDesk=="",][,1][vector] = "OpEd"
vector = which(NewsTest[NewsTest$NewsDesk=="",][,3] == "The Public Editor")
NewsTest[NewsTest$NewsDesk=="",][,1][vector] = "OpEd"

for (i in seq_len(nrow(NewsTest)))
{
  if(NewsTest$NewsDesk[i]=="Styles" & NewsTest$SectionName[i]=="")
  {
    NewsTest$SectionName[i]="U.S."
  }  
}


for (i in seq_len(nrow(NewsTest)))
{
  if(NewsTest$NewsDesk[i]=="Science")
  {
    NewsTest$SectionName[i]="Health"
  }  
}

for (i in seq_len(nrow(NewsTest)))
{
  if(NewsTest$NewsDesk[i]=="OpEd")
  {
    NewsTest$SectionName[i]="Opinion"
  }  
}


for (i in seq_len(nrow(NewsTest)))
{
  if(NewsTest$NewsDesk[i]=="Foreign")
  {
    NewsTest$SectionName[i]="World"
    NewsTest$SubsectionName[i]="Asia Pacific"
  }  
}

for (i in seq_len(nrow(NewsTest)))
{
  if(NewsTest$NewsDesk[i]=="Culture")
  {
    NewsTest$SectionName[i]="Arts"
  }  
}

NewsTrain$Weekday = (NewsTrain$PubDate$wday)
NewsTest$Weekday = (NewsTest$PubDate$wday)

NewsTrain$Hour=(NewsTrain$PubDate$hour)
NewsTest$Hour=(NewsTest$PubDate$hour)

NewsTrain$WordCount=NewsTrain$WordCount
NewsTest$WordCount=NewsTest$WordCount

#Creating a new Variable Q_Mark_Headine which checks if a question mark is present or not in Headline
for(i in 1:6532)
{
  if(grepl("?",NewsTrain$Headline[i],fixed=TRUE))
  {
    NewsTrain$Q_Mark_Headline [i]=1
  }
  else
  {
    NewsTrain$Q_Mark_Headline [i]=0
  }
}

for(i in 1:1870)
{
  if(grepl("?",NewsTest$Headline[i],fixed=TRUE))
  {
    NewsTest$Q_Mark_Headline [i]=1
  }
  else
  {
    NewsTest$Q_Mark_Headline [i]=0
  }
}

#Creating a new Variable Q_Mark_Abstract which checks if a question mark is present or not in Abstract

for(i in 1:6532)
{
  if(grepl("?",NewsTrain$Abstract[i],fixed=TRUE))
  {
    NewsTrain$Q_Mark_Abstract [i]=1
  }
  else
  {
    NewsTrain$Q_Mark_Abstract [i]=0
  }
}

for(i in 1:1870)
{
  if(grepl("?",NewsTest$Abstract[i],fixed=TRUE))
  {
    NewsTest$Q_Mark_Abstract [i]=1
  }
  else
  {
    NewsTest$Q_Mark_Abstract [i]=0
  }
}

#Using text analytics to find out the top 1% of the words 
CorpusHeadline = Corpus(VectorSource(c(NewsTrain$Headline, NewsTest$Headline)))
CorpusHeadline = tm_map(CorpusHeadline, tolower)
CorpusHeadline = tm_map(CorpusHeadline, PlainTextDocument)
CorpusHeadline = tm_map(CorpusHeadline, removePunctuation)
CorpusHeadline = tm_map(CorpusHeadline, removeWords, stopwords("english"))
CorpusHeadline = tm_map(CorpusHeadline, stemDocument)
dtm = DocumentTermMatrix(CorpusHeadline)
sparse = removeSparseTerms(dtm, 0.99)
HeadlineWords = as.data.frame(as.matrix(sparse))
colnames(HeadlineWords) = make.names(colnames(HeadlineWords))
colnames(HeadlineWords) = paste0("H",colnames(HeadlineWords))
HeadlineWordsTrain = head(HeadlineWords, nrow(NewsTrain))
HeadlineWordsTest = tail(HeadlineWords, nrow(NewsTest))

CorpusHeadline = Corpus(VectorSource(c(NewsTrain$Abstract, NewsTest$Abstract)))
CorpusHeadline = tm_map(CorpusHeadline, tolower)
CorpusHeadline = tm_map(CorpusHeadline, PlainTextDocument)
CorpusHeadline = tm_map(CorpusHeadline, removePunctuation)
CorpusHeadline = tm_map(CorpusHeadline, removeWords, stopwords("english"))
CorpusHeadline = tm_map(CorpusHeadline, stemDocument)
dtm = DocumentTermMatrix(CorpusHeadline)
sparse = removeSparseTerms(dtm, 0.99)
AbstractWords = as.data.frame(as.matrix(sparse))
colnames(AbstractWords) = make.names(colnames(AbstractWords))
colnames(AbstractWords) = paste0("A",colnames(AbstractWords))
AbstractWordsTrain = head(AbstractWords, nrow(NewsTrain))
AbstractWordsTest = tail(AbstractWords, nrow(NewsTest))

#Combining all variables
Train = cbind(HeadlineWordsTrain,AbstractWordsTrain)
Test = cbind(HeadlineWordsTest,AbstractWordsTest)
Train$Weekday = NewsTrain$Weekday
Test$Weekday = NewsTest$Weekday
Train$Hour = NewsTrain$Hour
Test$Hour = NewsTest$Hour
Train$Q_Mark_Headline = NewsTrain$Q_Mark_Headline
Test$Q_Mark_Headline = NewsTest$Q_Mark_Headline
Train$Q_Mark_Abstract = NewsTrain$Q_Mark_Abstract
Test$Q_Mark_Abstract = NewsTest$Q_Mark_Abstract
Train$WordCount = NewsTrain$WordCount
Test$WordCount = NewsTest$WordCount
Train$WordCount = NewsTrain$WordCount
Test$WordCount = NewsTest$WordCount
Train$NewsDesk = NewsTrain$NewsDesk
Test$NewsDesk = NewsTest$NewsDesk
Train$SectionName = NewsTrain$SectionName
Test$SectionName = NewsTest$SectionName
Train$SubsectionName = NewsTrain$SubsectionName
Test$SubsectionName = NewsTest$SubsectionName
Train$Popular = NewsTrain$Popular

train = Train
test= Test

# Train/Test (without log(1+WordCount)) => Random Forest
Train$Popular=as.factor(Train$Popular)
Train$Q_Mark_Headline=as.factor(Train$Q_Mark_Headline)
Train$Q_Mark_Abstract=as.factor(Train$Q_Mark_Abstract)
#Train$Brand=as.factor(Train$Brand)
Train$NewsDesk=as.factor(Train$NewsDesk)
Train$SectionName=as.factor(Train$SectionName)
Train$SubsectionName=as.factor(Train$SubsectionName)
Train$Weekday=as.factor(Train$Weekday)
Train$Hour=as.factor(Train$Hour)
Train$WordCount=Train$WordCount
Test$Q_Mark_Headline=as.factor(Test$Q_Mark_Headline)
Test$Q_Mark_Abstract=as.factor(Test$Q_Mark_Abstract)
Test$NewsDesk=as.factor(Test$NewsDesk)
Test$SectionName=as.factor(Test$SectionName)
Test$SubsectionName=as.factor(Test$SubsectionName)
Test$Weekday=as.factor(Test$Weekday)
Test$Hour=as.factor(Test$Hour)
Test$WordCount=Test$WordCount
Test$NewsDesk <- factor(Test$NewsDesk, levels=levels(Train$NewsDesk))
Test$SectionName <- factor(Test$SectionName, levels=levels(Train$SectionName))
Test$SubsectionName <- factor(Test$SubsectionName, levels=levels(Train$SubsectionName))

# train/test (with log(1+WordCount))    => Logistic Regression
train$Popular=as.factor(train$Popular)
train$Q_Mark_Headline=as.factor(train$Q_Mark_Headline)
train$Q_Mark_Abstract=as.factor(train$Q_Mark_Abstract)
train$NewsDesk=as.factor(train$NewsDesk)
train$SectionName=as.factor(train$SectionName)
train$SubsectionName=as.factor(train$SubsectionName)
train$Weekday=as.factor(train$Weekday)
train$Hour=as.factor(train$Hour)
train$WordCount=log(1+train$WordCount)
test$Q_Mark_Headline=as.factor(test$Q_Mark_Headline)
test$Q_Mark_Abstract=as.factor(test$Q_Mark_Abstract)
test$NewsDesk=as.factor(test$NewsDesk)
test$SectionName=as.factor(test$SectionName)
test$SubsectionName=as.factor(test$SubsectionName)
test$Weekday=as.factor(test$Weekday)
test$Hour=as.factor(test$Hour)
test$WordCount=log(1+test$WordCount)
test$NewsDesk <- factor(test$NewsDesk, levels=levels(train$NewsDesk))
test$SectionName <- factor(test$SectionName, levels=levels(train$SectionName))
test$SubsectionName <- factor(test$SubsectionName, levels=levels(train$SubsectionName))

#Logistic Regression Model
ModelLog = glm(Popular~.,data=train,family=binomial)

#Random Forest Model
set.seed(144)
ModelRF = randomForest(Popular~.,data=Train)

#Predicting the test set on the above models
predictLog = predict(ModelLog,newdata=test,type="response")
predictRF = predict(ModelRF,newdata=Test,type="prob")[,2]

#Combining the predictions of both models(Ensemble Technique)
predictModel = (predictLog+predictRF*2)/3

#Submitting on Kaggle
KaggleSubmission = data.frame(UniqueID = NewsTest$UniqueID, Probability1 = predictModel)

write.csv(KaggleSubmission, "KaggleSubmission.csv", row.names=FALSE)
