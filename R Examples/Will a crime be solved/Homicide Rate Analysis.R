
require(tidyr)
require(dplyr)
require(ggplot2)
require(rpart)
require(rpart.plot)
require(RColorBrewer)
require(rattle)
require(descr)


homicide <- read.csv("homicide reports.csv",header=TRUE)

attach(homicide)

#************
# Explore the structure of the data
#************
str(homicide) #Structure of the data

dim(homicide) #Dimensions of the data

names(homicide) #Variable names

summary(homicide)

#************
# Hypothesese of interest
#   1) Which cities have the lowest solved crime rate, which have the highest solved crime rate?
#   2) Which agencies have the lowest solved crime rate, which have the highest solved crime rate?
#   3) Let's build a decision tree which predicts crime solving

#************
# Hypothesis (1)
#************

tab.city <- homicide %>%
  group_by(State,City,Crime.Solved) %>%
  summarise(N=n()) %>%
  spread(Crime.Solved,N) %>%
  mutate(p = Yes/(Yes+No)) %>%
  filter(Yes+No > 100) %>%
  arrange(p)
  
head(arrange(tab.city,desc(Yes)),10) #10 Cities with the most solved crimes
head(arrange(tab.city,desc(No)),10) #10 Cities with the most unsolved crimes

head(arrange(tab.city,desc(p)),10) #10 Cities with the highest solved crime rate
head(arrange(tab.city,p),10) #10 Cities with the lowest solved crime rate

#*************
# Hypothesis (2)
#*************

tab.agency <- homicide %>%
  group_by(Agency.Type,Crime.Solved) %>%
  summarise(N=n()) %>%
  spread(Crime.Solved,N) %>%
  mutate(p = Yes/(Yes+No)) %>%
  arrange(desc(p))

tab.agency

#*************
# Hypothesis (3)
#*************

features <- homicide[c("Crime.Solved","Crime.Type","Victim.Race",
                       "Relationship","Weapon","Victim.Count","Perpetrator.Count",
                       "Incident","Victim.Sex")] #Extract features of interest from homicide data set

features <- features[complete.cases(features),] #Remove incomplete cases

unsolved <- filter(features,Crime.Solved=="No") #filter on unsolved cases

solved <- filter(features,Crime.Solved=="Yes") #Filter on solved cases
solved <- solved[sample(seq_len(nrow(unsolved)),size=nrow(unsolved)),] #Randomly select the same number of
  #solved cases as unsolved cases.  This will prevent premature termination of the tree due to 
  # a lack of observed categories

set.seed(1234) #For reproducibility
s.id <- sample(seq_len(nrow(solved)),size=.75*nrow(solved)) #Select rows for splitting to test/train
u.id <- sample(seq_len(nrow(unsolved)),size=.75*nrow(unsolved)) #Select rows for splitting to test/train

s.train <- solved[s.id,]
s.test <- solved[-s.id,]

u.train <- unsolved[u.id,]
u.test <- unsolved[-u.id,]

train <- rbind(s.train,u.train) #Training set from solved training and unsolved training
test <- rbind(s.test,u.test) #Test set from solved testing and unsolved testing

table(train$Crime.Solved) #Confirm an equal number of Yes/No in training set
table(test$Crime.Solved) #Confirm an equal number of Yes/No in test set


fit1 <- rpart(Crime.Solved ~ Crime.Type + Relationship + Victim.Race + Weapon + Victim.Count,data=train,
              maxdepth=6, cp=-1)

fit2 <- rpart(Crime.Solved ~ Crime.Type + Relationship + Victim.Race + Weapon + Victim.Count,data=train,
              maxdepth=5, cp=-1)

fit3 <- rpart(Crime.Solved ~ Crime.Type + Relationship + Victim.Race + Weapon + Victim.Count,data=train,
              maxdepth=4, cp=-1)

fit4 <- rpart(Crime.Solved ~ Crime.Type + Relationship + Victim.Race + Weapon + Victim.Count,data=train,
              maxdepth=3, cp=-1)

fit5 <- rpart(Crime.Solved ~ Crime.Type + Relationship + Victim.Race + Weapon + Victim.Count,data=train,
              maxdepth=2, cp=-1)

fit6 <- rpart(Crime.Solved ~ Crime.Type + Relationship + Victim.Race + Weapon + Victim.Count,data=train,
              maxdepth=1, cp=-1)

fit7 <- rpart(Crime.Solved ~ Crime.Type + Relationship + Victim.Race + Weapon + Victim.Count,data=train, 
              cp=.01)


rpart.plot(fit1)
rpart.plot(fit2)
rpart.plot(fit3)
rpart.plot(fit4)
rpart.plot(fit5)
rpart.plot(fit6)
rpart.plot(fit7)

pred.fit1 <- predict(fit1,test,type="class")
pred.fit2 <- predict(fit2,test,type="class")
pred.fit3 <- predict(fit3,test,type="class")
pred.fit4 <- predict(fit4,test,type="class")
pred.fit5 <- predict(fit5,test,type="class")
pred.fit6 <- predict(fit6,test,type="class")
pred.fit7 <- predict(fit7,test,type="class")


acc <- function(x,y){
  p <- table(x,y)
  acc <- (p[1,1]+p[2,2])/sum(p)
  return(acc)
}

output <- cbind(test[,c("Crime.Type","Victim.Race","Relationship",
                         "Weapon","Victim.Count","Crime.Solved")],pred.fit1,pred.fit2,
                pred.fit3,pred.fit4,pred.fit5,pred.fit6,pred.fit7)

acc(output$pred.fit1,output$Crime.Solved)
acc(output$pred.fit2,output$Crime.Solved)
acc(output$pred.fit3,output$Crime.Solved)
acc(output$pred.fit4,output$Crime.Solved)
acc(output$pred.fit5,output$Crime.Solved)
acc(output$pred.fit6,output$Crime.Solved)
acc(output$pred.fit7,output$Crime.Solved)


# Model 6 is the Winner
rpart.plot(fit6)
fit6
summary(fit6)
CrossTable(output$pred.fit6,output$Crime.Solved,cell.layout=FALSE)





