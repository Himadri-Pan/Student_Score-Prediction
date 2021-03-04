##Importing Data From Web
student_study=read.csv(url("http://bit.ly/w-data"))

str(student_study)

##Scatter Plot
plot(student_study,col='red',main=paste("Score Vs. Hour"))

###7. Splitting the data into train & test:
trainingsampleindex=sample(1:nrow(student_study),size=0.7*nrow(student_study))
traindata=student_study[trainingsampleindex,]
testdata=student_study[-trainingsampleindex,]
dim(traindata)
dim(testdata)
dim(student_study)

##Model 
model1=lm(Scores~.,data = traindata)
summary(model1)

#Model Accuracy
testdata$pred_lm=predict(model1,testdata)
testdata
testdata$LM_Ape=100*(abs(testdata$Scores-testdata$pred_lm)/testdata$Scores)
print(paste("### Mean Accuracy of Linear Regression Model is:",100-mean(testdata$LM_Ape)))
print(paste("### Median Accuracy of Linear Regression Model is: ",100-median(testdata$LM_Ape)))


##What will be predicted score if a student studies for 9.25 hrs/ day?
incomingdata=data.frame('Hours','Score')
incomingdata$Hours=9.25
incomingdata$X.Hours.=NULL
incomingdata$X.Score.=NULL

incomingdata$pred_score=predict(model1,incomingdata)

