source('hw.R')
source('classeda.R')
source('classdensity.R')
library(ggplot2)
library(randomForest)
library(lattice)
library(DescTools)
library(glmnet)
library(tigris)
library(tmap)
library(shinyjs)  

#read data and make SAT column
data <- read.csv('MA_Public_Schools_2017.csv')
HSonly <- subset(data, Grade == "09,10,11,12")
names(HSonly)
HSonly[,98]<-HSonly[,95]+HSonly[,96]+HSonly[,97]
names(HSonly)[98]<-'SAT_total'
names(HSonly)


cors<-cor(Filter(is.numeric,HSonlyrf[,-c(54:62)]))
SATcors<-cors[,"SAT_total"]
inorder<-order(abs(SATcors),decreasing=TRUE)
orderedcorelations<-SATcors[inorder]
head(data.frame(orderedcorelations))

#Plot ELL vs SAT
ggplot(data=HSonly,aes(x=X..English.Language.Learner,y=SAT_total))+geom_point(color='blue') +
  geom_text(data=HSonly[HSonly$School.Name=="Boston International High School", ], 
           label="Boston International\nHigh School", vjust=.4,hjust=1.04)


#economically disadvantaged lm
reg<-lm(data=HSonlyclean, SAT_total ~ X..Economically.Disadvantaged)
options(scipen=1)
summary(reg)

ggplot(data=HSonlyrf, aes(x=X..Economically.Disadvantaged, y=SAT_total)) + 
  geom_point()+
  geom_smooth(method = lm)+
  labs(title='% Economically Disadvantaged vs. SAT scores', 
       x='% Economically Disadvantaged', y = 'Avg. SAT Score')+hw

lmresults <- predict(reg, newdata=HSonlyclean)
lmRMSE<-mean((lmresults-HSonlyclean[,'SAT_total'])^2)
RMSE(lmresults,HSonlyclean[,'SAT_total'])

#RF to predict SAT
HSonly<- HSonly[!(HSonly$School.Name=="Lunenburg High"),]
HSonlyclean<- HSonly[,colSums(!is.na(HSonly)) > 0]

HSonlyclean<-na.omit(HSonlyclean[,-c(1,4,5,11,12,13,15,16:25)])
names(HSonlyclean)
HSonlyrf<-HSonlyclean[,-c(70:76,78:80,82:109,112:118)]


nr <- nrow(HSonlyrf)
trainingSize <- ceiling(nr/2)

set.seed(124)
train <- sample(1:nr,trainingSize)

rf.train <- HSonlyrf[train,]
rf.test <- HSonlyrf[-train,] 

rf <- randomForest(x=rf.train[,-71],y=rf.train[,71])
rf
varImpPlot(rf)

rf <- randomForest(x=rf.test[,-71],y=rf.test[,71])
rf
varImpPlot(rf,main='Variable Importance in SAT Random Forest')

rfresults <- predict(rf, newdata = rf.test)
rfRMSE<-mean((rfresults-rf.test[,71])^2)
sqrt(rfRMSE)
plot(rfresults,rf.test[,71])

options(scipen=1000)
RMSE(rf.test[,71],rfresults)

#Teacher salary distribution EDA
Salarydata <- na.omit(data[!duplicated(data$District.Name),c(14,54:56,10)])
Salarydata[,5] <- Salarydata[,4]/sum(Salarydata[,4])

options(scipen=100)
classEda(na.omit(samples[,1]),lab1 = "MA Teacher Salary",
         units= "$ per Year")

stat<- summary(samples[,1])

stat
Quantile(Salarydata[,3],weights=Salarydata[,5])
samples<-data.frame(samples = rep(Salarydata$Average.Salary, 
                                  Salarydata$FTE.Count))




#Map Salary
zipsalary <- data[!duplicated(data$Zip),c(55,10)]
somethingmerged<- na.omit(geo_join(zcta,zipsalary,'ZCTA5CE10','Zip'))

zipsalary$Zip = paste(0, zipsalary$Zip, sep = "")
somethingmerged<- na.omit(geo_join(zcta,zipsalary,'ZCTA5CE10','Zip'))

zcta<-zctas(year=2010,state='MA')
mapdf = data.frame(region=Salarydata$Zip, value=Salarydata$Average.Salary)

tm_shape(somethingmerged) + tm_polygons('Average.Salary',palette='inferno',
                                        style='quantile',n=4,
                                        title='Average Teacher Salary\n*Missing Data is Blank*') +
  tm_layout(main.title = "Average Teacher Salary by Zip Code",
            main.title.position = "center",
            main.title.color = "black") + tm_shape(fillers) + 
  tm_borders(col = "dark grey")+tm_add_legend(type='fill')



#Dot plot 

quants<-quantile(HSonlyclean$SAT_total, prob= seq(0,1,.1))
quants

HSonlyclean$dif <- HSonlyclean$SAT_total-quants[1]

HSonlyclean$SATpct<-ecdf(HSonlyclean$SAT_total)(HSonlyclean$SAT_total)
HSonlyclean$High.Needspct<-ecdf(HSonlyclean$X..High.Needs)(HSonlyclean$X..High.Needs)
HSonlyclean$Whitepct<-ecdf(HSonlyclean$X..White)(HSonlyclean$X..White)

for (iter in 1:11){
  HSonlyclean$dif <-HSonlyclean$SAT_total-quants[iter]
  quantdata[iter,c(1,2,3,4)]<-HSonlyclean[which.min(abs(HSonlyclean$dif)),c(1,120:122)]
  
}

quantdata

names(quantdata)[c(2:4)]<-c('SAT Scores','% High Needs','% White')

names(quantdata)

quantdata
quantdata$School.Name <- reorder(as.factor(quantdata$School.Name), 
                                 quantdata$`SAT Scores`, decreasing=TRUE)

reshape<-gather(quantdata,
       key = 'Metric', value='Percentiles', -School.Name)

ord <- order(reshape$Metric,reshape$Percentiles,decreasing = TRUE)

reshape<-reshape[ord,]

reshape
reshape$School.Name <- factor(reshape$School.Name, levels = reshape$School.Name)

reshape$Metric <- factor(reshape$Metric, levels = c("SAT Scores","% High Needs", '% White'))

ggplot(reshape, aes(x = Percentiles, y = School.Name)) +
  geom_point(fill = "blue", shape = 21, size = 2.8) +
  labs(x = "Percentile",y = "",
       title = "Key School Statistic Percentiles") +
  facet_grid(.~Metric) + hw 


#Lasso
x <- as.matrix(Filter(is.numeric,predictors))
y <- HSonlyclean[,81]
nr <- nrow(x)
nr
trainingSize <- ceiling(nr/2) # half case for training

set.seed(124)
train <- sample(1:nr,trainingSize)

x.train <- x[train,]
y.train <- y[train]
head(x.train)
head(y.train)

x.test <- x[-train,] 
y.test <- y[-train]
head(x.test)

lasso.mod <- glmnet(x[train,],y[train], alpha=1)
plot(lasso.mod,las=1)

set.seed(124)
cv.out = cv.glmnet(x[train,], y[train] ,alpha=1)
plot(cv.out)

cbind(lambda=round(cv.out$lambda,1),
      cvm=signif(cv.out$cvm,7),
      cvsd=signif(cv.out$cvsd,5))

cvm <- cv.out$cvm
sub1 <- which.min(cvm)
sub1
cvmTest <- cvm[sub1]+
  cv.out$cvsd[sub1]
sub2 <- which(cvm < cvmTest)[1]
sub2
cv.out$lambda[sub2]
cv.out$lambda.1se 

lambda.min <- cv.out$lambda.min
lambda.1se <- cv.out$lambda.1se
lambda.min
lambda.1se

lasso.predBest <- predict(lasso.mod, s=lambda.min, newx=x.test)

lasso.pred1se <- predict(lasso.mod, s=lambda.1se, newx=x.test)

mean((lasso.predBest-y.test)^2)

mean((lasso.pred1se-y.test)^2)

mean((rfresults-rf.test[,71])^2)

RMSE(lasso.predBest,y.test)
lasso_RMSE
rf_RMSE
lm_RMSE
comparison<-data.frame(lasRMSE,rfRMSE,lmRMSE)

rferror<-abs(rfresults-rf.test[,71])
rfplot<-cbind(rfresults,rf.test[71],rferror)
ggplot(data=rfplot,aes(x=SAT_total,y=rfresults,colour=rferror))+geom_point()+
  scale_colour_gradientn(colours=c('black','red'), name ='Error',
                         limits=c(0,50),oob = scales::squish)+ylim(1000,1850)+
  labs(title='Modeling Error: Random Forest', 
       x='SAT Scores', y='Predicted Scores')+hw


lerror<-abs(lasso.predBest-y.test)
lplot<-data.frame(lasso.predBest,y.test,lerror)
names(lplot)[1:3]<-c('lasso.predBest','SAT_total','lerror')
ggplot(data=lplot,aes(x=SAT_total,y=lasso.predBest,colour=lerror))+geom_point()+
  scale_colour_gradientn(colours=c('black','red'), name ='Error',
                         limits=c(0,50),oob = scales::squish)+ylim(1000,1850)+
  labs(title='Modeling Error: Lasso', 
       x='SAT Scores', y='Predicted Scores')+hw

