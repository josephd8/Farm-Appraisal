setwd("~/3Fall2016/stat330/Exams")

#### Read in the Data ####
farm <- read.table("Farms.txt",header=TRUE)
#farm$acrePrice <- log(farm$acrePrice)
#farm$productivity <- log(farm$productivity)
#farm$tillable <- log(farm$tillable)
head(farm)

#### Exploratory Data Analysis ####
pairs(farm[,c(1:3,5,6)])
# with(farm,{plot(acrePrice~improvements)})
# with(farm,{plot(acrePrice~crpPct)})
# with(farm,{plot(acrePrice~tillable)})
# with(farm,{plot(acrePrice~productivity)})

par(mfrow = c(2,3))
with(farm,{boxplot(acrePrice~financing,main="Financing")})
with(farm,{boxplot(acrePrice~NW,main="Northwest")})
with(farm,{boxplot(acrePrice~SC,main="South Central")})
with(farm,{boxplot(acrePrice~SE,main="Southeast")})
with(farm,{boxplot(acrePrice~SW,main="Southwest")})
with(farm,{boxplot(acrePrice~WC,main="West Coast")})


#### Model Selection ####
mlr <- lm(acrePrice~.,data=farm)
summary(mlr)

farm <- farm[,c(2:length(farm),1)]
library(bestglm)
best.sub <- bestglm(farm,IC="BIC",method="exhaustive")
plot(best.sub$Subsets$BIC,type="b",pch=19,
     xlab="# of Variables",ylab="BIC")
lm.farm <- best.sub$BestModel
summary(lm.farm)

#### Add an Interaction ####
lm.app <- with(farm,{lm(log(acrePrice)~improvements+tillable+
                        crpPct+log(productivity)+NW+WC+NW:productivity)})
summary(lm.app)

# Test the Interaction #
lm.red <- with(farm,{lm(log(acrePrice)~improvements+tillable+
                          crpPct+log(productivity)+NW+WC)})
anova(lm.app,lm.red)
      # interaction does seem to be significant

#### Assumptions ####

par(mfrow=c(1,1))
plot(lm.app$fitted.values,lm.app$residuals,ylab="Residuals",
     main="Residuals vs. Fitted Values",xlab="Fitted Values")
abline(h=0,col='red')
hist(lm.app$residuals,main="Histogram of Residuals",xlab="Residuals")

library(lmtest)
bptest(lm.farm) 

library(car)
avPlots(lm.app)


### Confidence Intervals ####
library(sjPlot)
sjt.lm(lm.app)
confint(lm.app, level=.95)


#### Cross Validation ####
pred.width <- numeric(0)
coverage <- numeric(0)
bias <- numeric(0)
rpmse <- numeric(0)
for (i in 1:1000){
  n <- 40
  sampl <- sample(1:length(farm$acrePrice),n)
  
  train <- farm[-sampl,]
  test <- farm[sampl,]
  train.lm <- with(farm,{lm(log(acrePrice)~log(productivity)+tillable+
                             improvements+crpPct+NW+WC+NW:productivity)})
  pred.farm <- train
  pred.acre <- predict.lm(train.lm,pred.farm)
  pred.farm$predacrePrice <- pred.acre
  pred.int <- predict.lm(train.lm,test,
                         interval="prediction",level=.95)
  covers <- mean(exp(pred.int[,2]) < test$acrePrice & 
                   test$acrePrice < exp(pred.int[,3]))
  bias[i] <- mean(exp(pred.farm$predacrePrice) - pred.farm$acrePrice)
  rpmse[i] <- sqrt(mean((exp(pred.farm$predacrePrice) - pred.farm$acrePrice)^2))
  coverage[i] <- covers
  int.width <- mean(exp(pred.int[,3]) - exp(pred.int[,2]))
  pred.width[i] <- int.width
  
}
mean(bias)
mean(rpmse)
mean(coverage)
mean(pred.width)



# pred.width <- numeric(0)
# coverage <- numeric(0)
# bias <- numeric(0)
# rpmse <- numeric(0)
# for (i in 1:1000){
#   n <- 40
#   sampl <- sample(1:length(farm$acrePrice),n)
#   
#   train <- farm[-sampl,]
#   test <- farm[sampl,]
#   train.lm <- with(farm,{lm(log(acrePrice)~log(productivity)+tillable+
#                               improvements+crpPct+NW+WC )})
#   pred.farm <- train
#   pred.acre <- predict.lm(train.lm,pred.farm)
#   pred.farm$predacrePrice <- exp(pred.acre)
#   pred.int <- exp(predict.lm(train.lm,test,
#                          interval="prediction",level=.95))
#   covers <- mean(pred.int[,2] < test$acrePrice & 
#                    test$acrePrice < pred.int[,3])
#   bias[i] <- mean(pred.farm$predacrePrice - pred.farm$acrePrice)
#   rpmse[i] <- sqrt(mean((pred.farm$predacrePrice - pred.farm$acrePrice)^2))
#   coverage[i] <- covers
#   int.width <- mean(pred.int[,3] - pred.int[,2])
#   pred.width[i] <- int.width
#   
# }
# mean(bias)
# mean(rpmse)
# mean(coverage)
# mean(pred.width)
#### Prediction ####
newdat <- data.frame(financing="title_transfer",NW="Yes",WC="No",improvements=0,tillable=94,crpPct=0,
                     productivity=96)
ans <- predict.lm(lm.app,newdat,interval="prediction",level=.95)
exp(ans)
    # how do I want to report this with the transformation?
