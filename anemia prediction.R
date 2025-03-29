## ----message=FALSE-----------------------------------------------------------------------------
library(ggplot2)
library(dplyr)
library(class)


## ----------------------------------------------------------------------------------------------
anaemia <- read.csv("C:/Users/LENOVO/Downloads/d_output.csv")
head(anaemia)


## ----------------------------------------------------------------------------------------------
sum(is.na(anaemia))


## ----------------------------------------------------------------------------------------------
str(anaemia)


## ----------------------------------------------------------------------------------------------
table(anaemia$Sex)


## ----------------------------------------------------------------------------------------------
table(anaemia$Anaemic)


## ----warning=FALSE-----------------------------------------------------------------------------
anaemia <- anaemia %>%
  mutate(Sex = recode(Sex, 'F' = 0, 'F ' = 0, 'M' = 1, 'M ' = 1),
         Anaemic = recode(Anaemic, 'Yes' = 1, 'No' = 0))


## ----------------------------------------------------------------------------------------------
# Anaemic; yes = 1, no = 0
anaemia%>%
    count(Anaemic)

# sex; Female = 0, Male = 1
anaemia %>%
    count(Sex)


## ----------------------------------------------------------------------------------------------
haem <-anaemia %>%
  count(Hb)
nrow(haem)

# getting the minimum and maximum values in the dataset.
min(anaemia$Hb)
max(anaemia$Hb)


## ----------------------------------------------------------------------------------------------
# column names
names(anaemia)


## ----------------------------------------------------------------------------------------------
anaemia$Anaemic <- factor(anaemia$Anaemic)
anaemia$Sex <- factor(anaemia$Sex)
str(anaemia)


## ----------------------------------------------------------------------------------------------
ggplot(anaemia,aes(x= Sex,fill = Anaemic))+ geom_bar(position = "dodge") + labs(title = "Count of gender according to whether the gender is anemic or not") + scale_fill_brewer(palette = "Set1")+ geom_text(stat = "count", aes(label = ..count..), position = position_dodge(width = 0.9),vjust = -0.5, size = 3.5,colour = "black", fontface = "bold")+theme_classic()



## ----------------------------------------------------------------------------------------------
classify_hb <- function(sex, hb) {
  if(sex == 0){
    if(hb >= 12.0 & hb <= 15.0){
      return("Normal")
    } else{
      return("Abnormal")
    }
  } else if(sex == 1){
    if(hb >= 13.5 & hb <= 18.0){
      return("Normal")
    } else{
      return("Abnormal")
    }
  } else{
    return("Unknown")
  }
}

 anaemia <- anaemia %>%
  mutate(Hb_status = mapply(classify_hb, Sex, Hb))


## ----------------------------------------------------------------------------------------------
anaemia %>%
  count(Hb_status)


## ----warning=FALSE-----------------------------------------------------------------------------
ggplot(anaemia,aes(x= Sex,fill = Hb_status))+ geom_bar(position = "dodge") + labs(title = "Hb status by sex") + scale_fill_brewer(palette = "Set1")+ geom_text(stat = "count", aes(label = ..count..), position = position_dodge(width = 0.9),vjust = -0.5, size = 3.5,colour = "black", fontface = "bold")+ theme_minimal()


## ----------------------------------------------------------------------------------------------
# Dropping unnecessary columns or features.
anaemia<- anaemia %>% select(-Number, -Hb_status)


## ----------------------------------------------------------------------------------------------
# Preparing numerical features
set.seed(123)
train <- anaemia%>% sample_frac(0.7)
test <- anaemia %>% anti_join(train,by = "Hb")

train_feature<- train[ ,c(-6,-7)]
train_labels<- train$Anaemic
test_feat<- test[ ,c(-6,-7)]
test_label<-test$Anaemic


## ----------------------------------------------------------------------------------------------
predictedlabels <-knn(train= train_feature,test= test_feat,cl = train_labels,k=3)
predictedlabels


## ----------------------------------------------------------------------------------------------
accuracy <- mean(predictedlabels == test_label)
print(paste("Accuracy is :", accuracy))


## ----warning=FALSE-----------------------------------------------------------------------------
conf<- table(predicted = predictedlabels,actual = test_label)
conf_df <- as.data.frame(as.table(conf))
ggplot(conf_df, aes(x = actual, y = predicted, fill = Freq)) + geom_tile(color = "black") +
  scale_fill_gradient(low = "red", high = "blue")+ geom_text(aes(label = Freq), size = 5)+labs(title = "Confusion Matrix for KNN clasifier", x = "Actual", y = "Predicted") +
  theme_minimal()

