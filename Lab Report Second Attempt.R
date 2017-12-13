#Reading in the data file ----------------------------------
#Seting working directory
curdir <- getwd()
#Reading in my prefered dataset(Calling it df for short)
df=read.csv(paste(curdir,"/car.data",sep = ""),header=T,
            na.strings="?")
#Pre-Processing ----------------------------
nd <- df


colnames(nd)[which(names(nd) == "vhigh")] <- "Price"
colnames(nd)[which(names(nd) == "vhigh.1")] <- "Maint"
colnames(nd)[which(names(nd) == "X2")] <- "Doors"
colnames(nd)[which(names(nd) == "X2.1")] <- "Persons"
colnames(nd)[which(names(nd) == "small")] <- "Boot"
colnames(nd)[which(names(nd) == "low")] <- "Safety"
colnames(nd)[which(names(nd) == "unacc")] <- "Evaluation"


nd$Price<-as.character(nd$Price)
nd$Price[nd$Price=="vhigh"] <- "4"
nd$Price[nd$Price=="high"] <- "3"
nd$Price[nd$Price=="med"] <- "2"
nd$Price[nd$Price=="low"] <- "1"
nd$Price = as.numeric(nd$Price)

nd$Maint<-as.character(nd$Maint)
nd$Maint[nd$Maint=="vhigh"] <- "4"
nd$Maint[nd$Maint=="high"] <- "3"
nd$Maint[nd$Maint=="med"] <- "2"
nd$Maint[nd$Maint=="low"] <- "1"
nd$Maint = as.numeric(nd$Maint)

nd$Doors<-as.character(nd$Doors)
nd$Doors[nd$Doors=="5more"] <- "5"
nd$Doors[nd$Doors=="4"] <- "4"
nd$Doors[nd$Doors=="3"] <- "3"
nd$Doors[nd$Doors=="2"] <- "2"
nd$Doors[nd$Doors=="1"] <- "1"
nd$Doors = as.numeric(nd$Doors)

nd$Persons<-as.character(nd$Persons)
nd$Persons[nd$Persons=="more"] <- "5"
nd$Persons[nd$Persons=="4"] <- "4"
nd$Persons[nd$Persons=="2"] <- "2"
nd$Persons = as.numeric(nd$Persons)

nd$Boot<-as.character(nd$Boot)
nd$Boot[nd$Boot=="small"] <- "1"
nd$Boot[nd$Boot=="med"] <- "2"
nd$Boot[nd$Boot=="big"] <- "3"
nd$Boot = as.numeric(nd$Boot)

nd$Safety<-as.character(nd$Safety)
nd$Safety[nd$Safety=="low"] <- "1"
nd$Safety[nd$Safety=="med"] <- "2"
nd$Safety[nd$Safety=="high"] <- "3"

nd$Safety = as.numeric(nd$Safety)


nd$Evaluation<-as.character(nd$Evaluation)
nd$Evaluation[nd$Evaluation=="unacc"] <- "1"
nd$Evaluation[nd$Evaluation=="acc"] <- "2"
nd$Evaluation[nd$Evaluation=="good"] <- "3"
nd$Evaluation[nd$Evaluation=="vgood"] <- "4"
nd$Evaluation = as.numeric(nd$Evaluation)




#random fact.counter imnbalence data set for the model


#Dropping irrellevant feautures  ------------------------------------------------------------
# removing data that is not needed-----------------------------
#Scatter Graph of maint and Eval not sure if needed ask -------------------------------------
model_table <- table(nd$Evaluation)
model_table <- prop.table(model_table)
round(model_table, digits = 1)

plot(x = nd$Maint, y = nd$Evaluation,
     main = "Scatterplot of Price vs. Mileage",
     xlab = "Maintenace  (1=low 2 =med 3= high 4=vhigh.)",
     ylab = "Eval (1=low, 2 =med 3= high 4=vhigh.))")


#Modeling classification--------------------------------------------
#splitting the data set in to 2 parts one for training and the other for testing------------------------
#defining the trainning data set from the 1st row to the 1000th 
train <- nd[1:1000,]
#defining the testing data set from the 1001th row to the end of the data set 
test <- nd[1001:nrow(nd),]
#Performing a test to check the the total number of records both train/test are equal to the orgininal set 
nrow(train)+nrow(test)==nrow(nd)
options(scipen=999)
table(train$Evaluation)
table(test$Evaluation)

nd$Evaluation <- as.factor(nd$Evaluation)

table(testing$Evaluation)
table(nd$Evaluation)
table(training$Evaluation)

#my attempt at random forest-------------------
library(caret)
inTrain <- createDataPartition(y=nd$Evaluation,
                               p=.7,list=FALSE)
training <- nd[inTrain,]
testing <- nd [-inTrain,]
nrow(training)+nrow(testing)==nrow(nd)
fitModel <- train(Evaluation ~ .,data=training,
                  method="rf",prox=TRUE)
fitModel$finalModel

df_C <- classCenter(training[,c(3,4)],
                    training$Evaluation,fitModel$finalModel$prox)
df_C <- as.data.frame(df_C)
df_C$Evaluation <- rownames(df_C)

#p <- qplot(Petal.Width, Petal.Length,
#col=Evaluation, data=training)
#p <- p + geom_point(aes(x=nd.Maint,
# y=nd.Saftey,col=Evaluation),size=5,shape=6,data=df_C)
#p +theme_bw()

summary(fitModel)

pred <- predict(fitModel, testing)
testing$correctPred <- pred==testing$Evaluation
results <- table(pred,testing$Evaluation)
results

accuracy <- sum(diag(results)) / sum(results) * 100
accuracy

#table(pred)
#resultss <-round(prop.table (table (Pred = unlist(pred), Actual = testing$Evaluation))*100)
#resultss

summary(testing$Evaluation)




 